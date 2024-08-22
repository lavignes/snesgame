#![allow(incomplete_features)]
#![feature(generic_const_exprs)]

use std::{
    collections::HashMap,
    error::Error,
    fs::{self, File},
    io::{self, BufWriter, ErrorKind, Read, Write},
    mem,
    path::PathBuf,
    process::ExitCode,
};

use clap::Parser;
use indexmap::IndexMap;
use nyasm::{
    Expr, ExprNode, Label, Op, Pos, Reloc, RelocFlags, RelocVal, Section, SliceInterner,
    StrInterner, Sym, SymFlags, Tok,
};
use serde::{de, Deserialize, Deserializer};
use serde_derive::{Deserialize, Serialize};
use tracing::Level;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Object files
    objects: Vec<PathBuf>,

    /// Config file
    #[arg(short, long)]
    config: PathBuf,

    /// Output file (default: stdout)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Output file for `MSL` debug symbol file
    #[arg(short = 'g', long)]
    debug: Option<PathBuf>,

    /// Output file for VIM tags file
    #[arg(long)]
    tags: Option<PathBuf>,

    /// Pre-defined symbols (repeatable)
    #[arg(short = 'D', long, value_name="KEY1=val", value_parser = nyasm::parse_defines::<String, i32>)]
    define: Vec<(String, i32)>,

    /// One of `TRACE`, `DEBUG`, `INFO`, `WARN`, or `ERROR`
    #[arg(short, long, default_value_t = Level::INFO)]
    log_level: Level,
}

fn main() -> ExitCode {
    let args = Args::parse();
    tracing_subscriber::fmt()
        .with_max_level(args.log_level)
        .with_writer(io::stderr)
        .init();

    if let Err(e) = main_real(args) {
        tracing::error!("{e}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn main_real(args: Args) -> Result<(), Box<dyn Error>> {
    let mut config = File::open(args.config).map_err(|e| format!("cant open file: {e}"))?;
    let mut config_text = String::new();
    config.read_to_string(&mut config_text)?;
    let config: Script = toml::from_str(&config_text)?;

    let mut ld = Ld::new();

    let def_file_section = ld.str_int.intern("__DEFINES__");
    let def_unit = ld.str_int.intern("__EXPORT__");
    for (name, val) in &args.define {
        let string = ld.str_int.intern(name);
        ld.syms.push(Sym {
            label: Label::new(None, string),
            value: Expr::Const(*val),
            unit: def_unit,
            section: def_file_section,
            pos: Pos {
                file: def_file_section,
                line: 1,
                column: 1,
            },
            flags: SymFlags::EQU,
        });
    }

    for (name, mem) in &config.memories {
        let name = ld.str_int.intern(&name);
        ld.memories.push(Memory::new(name, mem.start, mem.size));
    }

    for (name, section) in &config.sections {
        let name = ld.str_int.intern(&name);
        if section.align == 0 {
            Err(ld.err(&format!("section \"{name}\" has an invalid alignment of 0",)))?;
        }
        if let Some(memory) = config.memories.get(&section.load) {
            ld.sections.push(Section::new(name));
            match memory.ty {
                MemoryType::RO => match &section.ty {
                    SectionType::RO => {}
                    _ => {
                        Err(ld.err(&format!(
                            "memory \"{}\" is not type-compatible with section \"{name}\"",
                            section.load
                        )))?;
                    }
                },
                MemoryType::RW => match &section.ty {
                    SectionType::RW | SectionType::BSS => {}
                    _ => {
                        Err(ld.err(&format!(
                            "memory \"{}\" is not type-compatible with section \"{name}\"",
                            section.load
                        )))?;
                    }
                },
            }
        } else {
            Err(ld.err(&format!(
                "memory \"{}\" is not defined in config",
                section.load
            )))?;
        }
    }

    tracing::trace!("loading objects");
    for object in args.objects {
        let path = fs::canonicalize(object)?;
        let path = path.to_str().unwrap();
        let file = File::open(path)?;
        ld.load(path, file)?;
    }

    tracing::trace!("relocating sections");
    for (name, section) in &config.sections {
        let Ld {
            ref mut sections,
            ref mut memories,
            ref mut syms,
            ..
        } = ld;
        let memory = memories
            .iter_mut()
            .find(|memory| memory.name == section.load)
            .unwrap();
        let define = section.define;
        let aligned = ((memory.pc + section.align - 1) / section.align) * section.align;
        if memory.pc != aligned {
            tracing::trace!(
                "aligning section \"{name}\" from ${:08X} to ${aligned:08X}",
                memory.pc
            );
        }
        let files = &section.files;
        let section = sections
            .iter_mut()
            .find(|section| section.name == name)
            .unwrap();
        // we update the section pc to be its absolute start address in memory
        section.pc = aligned;
        tracing::trace!("placing section \"{name}\" at ${aligned:08X}");
        // add any files to the section
        if let Some(files) = files {
            for path in files {
                let mut file = File::open(path)
                    .map_err(|e| format!("Could not open {}: {e}", path.display()))?;
                file.read_to_end(&mut section.data)?;
            }
        }
        // check if section is big or spans multiple banks
        let section_size = section.data.len() as u32;
        if section_size > 0x10000 {
            tracing::warn!("section \"{}\" is larger than 65536 bytes", section.name);
        }
        let start_bank = (section.pc & 0xFF0000) >> 16;
        let end_address = section.pc + section_size;
        let end_bank = (end_address & 0xFF0000) >> 16;
        if ((end_address & 0xFFFF) != 0) && (start_bank != end_bank) {
            tracing::warn!(
                "section \"{}\" starts in bank ${start_bank:02X} and ends in bank ${end_bank:02X}",
                section.name
            );
        }
        memory.pc = aligned + section_size;
        // the "end" is actually 1 past the last address in the memory
        if memory.pc > memory.end {
            Err(io::Error::new(
                ErrorKind::InvalidData,
                format!(
                    "no room left in memory \"{}\" for section \"{name}\"",
                    memory.name
                ),
            ))?;
        }
        // define start and end in symbol table if wanted
        if define {
            let def_file_section = ld.str_int.intern("__DEFINES__");
            let def_unit = ld.str_int.intern("__EXPORT__");
            let start = ld.str_int.intern(&format!("__{name}_START__"));
            syms.push(Sym {
                label: Label::new(None, start),
                value: Expr::Const(section.pc as i32),
                unit: def_unit,
                section: def_file_section,
                pos: Pos {
                    file: def_file_section,
                    line: 1,
                    column: 1,
                },
                flags: SymFlags::EQU,
            });
            let size = ld.str_int.intern(&format!("__{name}_SIZE__"));
            syms.push(Sym {
                label: Label::new(None, size),
                value: Expr::Const(section_size as i32),
                unit: def_unit,
                section: def_file_section,
                pos: Pos {
                    file: def_file_section,
                    line: 1,
                    column: 1,
                },
                flags: SymFlags::EQU,
            });
        }
    }

    for pass in 1..=2 {
        tracing::trace!("symbol table pass {pass}");
        for i in 0..ld.syms.len() {
            let value = match ld.syms[i].value {
                Expr::Const(value) => Expr::Const(value),
                Expr::Addr(section, pc) => {
                    let section = ld.sections.iter().find(|sec| sec.name == section).unwrap();
                    let value = pc + section.pc;
                    Expr::Const(value as i32)
                }
                Expr::List(expr) => {
                    if let Some(value) = ld.expr_eval(ld.syms[i].unit, expr, &config.sections) {
                        Expr::Const(value)
                    } else {
                        Expr::List(expr)
                    }
                }
            };
            ld.syms[i].value = value;
        }
    }

    tracing::trace!("symbol table validation");
    for sym in &ld.syms {
        if let Expr::Const(_) = sym.value {
            continue;
        }
        Err(ld.err_in(
            sym.unit,
            &format!(
                "undefined symbol \"{}\"\n\tdeclared at {}:{}:{}",
                sym.label, sym.pos.file, sym.pos.line, sym.pos.column
            ),
        ))?;
    }

    tracing::trace!("linking");
    for i in 0..ld.sections.len() {
        for j in 0..ld.sections[i].relocs.len() {
            let reloc = ld.sections[i].relocs[j];
            let value = match reloc.value {
                RelocVal::Addr(section, pc) => {
                    let section = ld.sections.iter().find(|sec| sec.name == section).unwrap();
                    (pc + section.pc) as i32
                }
                RelocVal::List(expr) => {
                    if let Some(value) = ld.expr_eval(reloc.unit, expr, &config.sections) {
                        value
                    } else {
                        Err(ld.err_in(
                            reloc.unit,
                            &format!(
                                "expression cannot be solved\n\tdefined at {}:{}:{}",
                                reloc.pos.file, reloc.pos.line, reloc.pos.column
                            ),
                        ))?
                    }
                }
            };
            match reloc.width {
                1 => {
                    if (value as u32) > (u8::MAX as u32) {
                        Err(ld.err_in(
                            reloc.unit,
                            &format!(
                                "expression >1 byte\n\tdefined at {}:{}:{}",
                                reloc.pos.file, reloc.pos.line, reloc.pos.column
                            ),
                        ))?;
                    }
                    ld.sections[i].data[reloc.offset] = value as u8;
                }
                2 => {
                    if (value as u32) > (u16::MAX as u32) {
                        // ABS JMP-type relocations are OK if they are the same bank
                        if (reloc.flags & RelocFlags::ABS_JMP) != 0 {
                            let bank = ld.sections[i].pc >> 16;
                            if ((value as u32) >> 16) != bank {
                                Err(ld.err_in(
                                    reloc.unit,
                                    &format!(
                                        "expression >2 bytes\n\tdefined at {}:{}:{}",
                                        reloc.pos.file, reloc.pos.line, reloc.pos.column
                                    ),
                                ))?;
                            }
                        } else {
                            Err(ld.err_in(
                                reloc.unit,
                                &format!(
                                    "expression >2 bytes\n\tdefined at {}:{}:{}",
                                    reloc.pos.file, reloc.pos.line, reloc.pos.column
                                ),
                            ))?;
                        }
                    }
                    ld.sections[i].data[reloc.offset] = ((value as u32) >> 0) as u8;
                    ld.sections[i].data[reloc.offset + 1] = ((value as u32) >> 8) as u8;
                }
                3 => {
                    if (value as u32) > 0x00FFFFFFu32 {
                        Err(ld.err_in(
                            reloc.unit,
                            &format!(
                                "expression >3 bytes\n\tdefined at {}:{}:{}",
                                reloc.pos.file, reloc.pos.line, reloc.pos.column
                            ),
                        ))?;
                    }
                    if (reloc.flags & RelocFlags::L_JMP) != 0 {
                        let bank = ld.sections[i].pc >> 16;
                        if ((value as u32) >> 16) == bank {
                            tracing::warn!(
                                "long jump within same bank\n\tdefined at {}:{}:{}",
                                reloc.pos.file,
                                reloc.pos.line,
                                reloc.pos.column
                            );
                        }
                    }
                    ld.sections[i].data[reloc.offset] = ((value as u32) >> 0) as u8;
                    ld.sections[i].data[reloc.offset + 1] = ((value as u32) >> 8) as u8;
                    ld.sections[i].data[reloc.offset + 2] = ((value as u32) >> 16) as u8;
                }
                _ => unreachable!(),
            }
        }
    }

    let mut output: Box<dyn Write> = match args.output {
        Some(path) => Box::new(BufWriter::new(
            File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open(path)
                .map_err(|e| format!("cant open file: {e}"))?,
        )),
        None => Box::new(io::stdout()),
    };

    tracing::trace!("writing");
    for (mem_name, memory) in &config.memories {
        for (sec_name, section) in &config.sections {
            if &section.load != mem_name {
                continue;
            }
            let section = &ld.sections.iter().find(|sec| sec.name == sec_name).unwrap();
            tracing::trace!(
                "writing {} bytes of section \"{sec_name}\" in memory \"{mem_name}\"",
                section.data.len()
            );
            output.write_all(&section.data)?;
        }
        if let Some(value) = memory.fill {
            let mem = &ld.memories.iter().find(|mem| mem.name == mem_name).unwrap();
            tracing::trace!(
                "filling {} bytes of memory \"{mem_name}\" with ${value:02X}",
                mem.end - mem.pc,
            );
            for _ in mem.pc..mem.end {
                output.write(&[value])?;
            }
        }
    }

    if let Some(path) = args.debug {
        tracing::trace!("writing debug symbols");
        let mut file = File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)
            .map_err(|e| format!("cant open file: {e}"))?;
        for sym in &ld.syms {
            if (sym.flags & SymFlags::EQU) != 0 {
                continue;
            }
            if let Expr::Const(value) = sym.value {
                writeln!(file, "{value:06X} {}", sym.label.to_string())?;
            }
        }
    }

    if let Some(path) = args.tags {
        tracing::trace!("writing tags file");
        let mut file = File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)
            .map_err(|e| format!("cant open file: {e}"))?;
        ld.syms.sort_by_key(|sym| sym.label.to_string());
        ld.syms.dedup_by_key(|sym| sym.label.to_string());
        for sym in &ld.syms {
            if sym.pos.file == "__DEFINES__" {
                continue;
            }
            writeln!(file, "{}\t{}\t{}", sym.label, sym.pos.file, sym.pos.line)?;
        }
    }

    Ok(())
}

struct Ld<'a> {
    str_int: StrInterner<'a>,
    expr_int: SliceInterner<ExprNode<'a>>,
    sections: Vec<Section<'a>>,
    memories: Vec<Memory<'a>>,
    syms: Vec<Sym<'a>>,
}

impl<'a> Ld<'a> {
    fn new() -> Self {
        Self {
            str_int: StrInterner::new(),
            expr_int: SliceInterner::new(),
            sections: Vec::new(),
            memories: Vec::new(),
            syms: Vec::new(),
        }
    }

    fn err(&self, msg: &str) -> io::Error {
        io::Error::new(ErrorKind::InvalidData, msg)
    }

    fn err_in(&self, file: &str, msg: &str) -> io::Error {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("in file: {file}: {msg}"),
        )
    }

    fn read_int<R: Read, T: FromLeBytes<Buf = [u8; mem::size_of::<T>()]>>(
        &self,
        reader: &mut R,
    ) -> io::Result<T>
    where
        T::Buf: Default,
    {
        let mut buf = T::Buf::default();
        reader.read_exact(&mut buf)?;
        Ok(T::from_le_bytes(buf.into()))
    }

    fn load<R: Read>(&mut self, file: &str, mut reader: R) -> io::Result<()> {
        let mut magic = [0u8; 7];
        reader.read_exact(&mut magic)?;
        if &magic != b"nyasm01" {
            return Err(self.err_in(file, "bad magic"));
        }
        // fill up a temporary string table
        let mut str_int: StrInterner<'_> = StrInterner::new();
        {
            let str_len: u32 = self.read_int(&mut reader)?;
            let mut storage = String::new();
            storage.extend((0..str_len).map(|_| ' '));
            reader.read_exact(unsafe { storage.as_bytes_mut() })?;
            str_int.storages.push(storage);
        }
        // and a temporary expression table
        let mut expr_int: SliceInterner<ExprNode<'_>> = SliceInterner::new();
        {
            let expr_len: u32 = self.read_int(&mut reader)?;
            let mut storage = Vec::new();
            for _ in 0..expr_len {
                let ty: u8 = self.read_int(&mut reader)?;
                match ty {
                    0 => {
                        let value: i32 = self.read_int(&mut reader)?;
                        storage.push(ExprNode::Const(value));
                    }
                    1 => {
                        let ty: u8 = self.read_int(&mut reader)?;
                        match ty {
                            0 => {
                                let value: u8 = self.read_int(&mut reader)?;
                                storage.push(ExprNode::Op(Op::Binary(Tok(value))));
                            }
                            1 => {
                                let value: u8 = self.read_int(&mut reader)?;
                                storage.push(ExprNode::Op(Op::Unary(Tok(value))));
                            }
                            _ => return Err(self.err_in(file, "malformed expression table")),
                        }
                    }
                    2 => {
                        let ty: u8 = self.read_int(&mut reader)?;
                        match ty {
                            0 => {
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let scope = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let string = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let scope = self.str_int.intern(scope);
                                let string = self.str_int.intern(string);
                                storage.push(ExprNode::Label(Label::new(Some(scope), string)));
                            }
                            1 => {
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let string = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let string = self.str_int.intern(string);
                                storage.push(ExprNode::Label(Label::new(None, string)));
                            }
                            _ => return Err(self.err_in(file, "malformed expression table")),
                        }
                    }
                    3 => {
                        let ty: u8 = self.read_int(&mut reader)?;
                        match ty {
                            0 => {
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let scope = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let string = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let tag = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let scope = self.str_int.intern(scope);
                                let string = self.str_int.intern(string);
                                let tag = self.str_int.intern(tag);
                                storage.push(ExprNode::Tag(Label::new(Some(scope), string), tag));
                            }
                            1 => {
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let string = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let tag = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let string = self.str_int.intern(string);
                                let tag = self.str_int.intern(tag);
                                storage.push(ExprNode::Tag(Label::new(None, string), tag));
                            }
                            _ => return Err(self.err_in(file, "malformed expression table")),
                        }
                    }
                    _ => return Err(self.err_in(file, "malformed expression table")),
                }
            }
            expr_int.storages.push(storage);
        }
        // time to start filling the global symbol table
        let syms_len: u32 = self.read_int(&mut reader)?;
        for _ in 0..syms_len {
            let ty: u8 = self.read_int(&mut reader)?;
            let label = match ty {
                0 => {
                    let index: u32 = self.read_int(&mut reader)?;
                    let len: u32 = self.read_int(&mut reader)?;
                    let scope = str_int
                        .slice((index as usize)..((index as usize) + (len as usize)))
                        .unwrap();
                    let index: u32 = self.read_int(&mut reader)?;
                    let len: u32 = self.read_int(&mut reader)?;
                    let string = str_int
                        .slice((index as usize)..((index as usize) + (len as usize)))
                        .unwrap();
                    let scope = self.str_int.intern(scope);
                    let string = self.str_int.intern(string);
                    Label::new(Some(scope), string)
                }
                1 => {
                    let index: u32 = self.read_int(&mut reader)?;
                    let len: u32 = self.read_int(&mut reader)?;
                    let string = str_int
                        .slice((index as usize)..((index as usize) + (len as usize)))
                        .unwrap();
                    let string = self.str_int.intern(string);
                    Label::new(None, string)
                }
                _ => return Err(self.err_in(file, "malformed symbol table")),
            };
            let ty: u8 = self.read_int(&mut reader)?;
            let value = match ty {
                0 => {
                    let value: i32 = self.read_int(&mut reader)?;
                    Expr::Const(value)
                }
                1 => {
                    let index: u32 = self.read_int(&mut reader)?;
                    let len: u32 = self.read_int(&mut reader)?;
                    let expr_section = str_int
                        .slice((index as usize)..((index as usize) + (len as usize)))
                        .unwrap();
                    let pc: u32 = self.read_int(&mut reader)?;
                    let expr_section = self.str_int.intern(expr_section);
                    // place the address relative to the start of the section
                    let pc = if let Some(expr_section) =
                        self.sections.iter().find(|sec| sec.name == expr_section)
                    {
                        pc + expr_section.pc
                    } else {
                        return Err(self.err_in(
                            file,
                            &format!("section \"{expr_section}\" is not defined in config"),
                        ));
                    };
                    Expr::Addr(expr_section, pc)
                }
                2 => {
                    let index: u32 = self.read_int(&mut reader)?;
                    let len: u32 = self.read_int(&mut reader)?;
                    let expr = expr_int
                        .slice((index as usize)..((index as usize) + (len as usize)))
                        .unwrap();
                    let expr = self.expr_int.intern(expr);
                    Expr::List(expr)
                }
                _ => return Err(self.err_in(file, "malformed symbol table")),
            };
            let index: u32 = self.read_int(&mut reader)?;
            let len: u32 = self.read_int(&mut reader)?;
            let unit = str_int
                .slice((index as usize)..((index as usize) + (len as usize)))
                .unwrap();
            // hide static symbols under the object file name
            let unit = if unit == "__STATIC__" {
                self.str_int.intern(file)
            } else {
                self.str_int.intern(unit)
            };
            let index: u32 = self.read_int(&mut reader)?;
            let len: u32 = self.read_int(&mut reader)?;
            let sym_section = str_int
                .slice((index as usize)..((index as usize) + (len as usize)))
                .unwrap();
            let sym_section = self.str_int.intern(sym_section);
            let index: u32 = self.read_int(&mut reader)?;
            let len: u32 = self.read_int(&mut reader)?;
            let sym_file = str_int
                .slice((index as usize)..((index as usize) + (len as usize)))
                .unwrap();
            let sym_file = self.str_int.intern(sym_file);
            let line: u32 = self.read_int(&mut reader)?;
            let column: u32 = self.read_int(&mut reader)?;
            let pos = Pos {
                file: sym_file,
                line: line as usize,
                column: column as usize,
            };
            let flags: u8 = self.read_int(&mut reader)?;
            // duplicate exported symbol?
            if let Some(other) = self
                .syms
                .iter()
                .find(|sym| (sym.label == label) && (sym.unit == unit))
            {
                return Err(self.err_in(file, &format!("duplicate exported symbol \"{label}\" found\n\tdefined at {}:{}:{}\n\tagain at {sym_file}:{line}:{column}", other.pos.file, other.pos.line, other.pos.column)));
            }
            self.syms.push(Sym {
                label,
                value,
                unit,
                section: sym_section,
                pos,
                flags,
            });
        }
        // add to sections
        let sections_len: u32 = self.read_int(&mut reader)?;
        for _ in 0..sections_len {
            let index: u32 = self.read_int(&mut reader)?;
            let len: u32 = self.read_int(&mut reader)?;
            let name = str_int
                .slice((index as usize)..((index as usize) + (len as usize)))
                .unwrap();
            let name = self.str_int.intern(name);
            tracing::trace!("loading section \"{name}\"");
            let section = if let Some(section) = self.sections.iter().find(|sec| sec.name == name) {
                section
            } else {
                return Err(self.err_in(
                    file,
                    &format!("section \"{name}\" is not defined in config"),
                ));
            };
            let data_len: u32 = self.read_int(&mut reader)?;
            let mut data = Vec::new();
            data.extend((0..data_len).map(|_| 0));
            reader.read_exact(&mut data)?;
            // TODO this seems messy,
            let mut relocs = Vec::new();
            let relocs_len: u32 = self.read_int(&mut reader)?;
            for _ in 0..relocs_len {
                let offset: u32 = self.read_int(&mut reader)?;
                // place the offset relative to the start of the section
                let offset = (offset as usize) + (section.pc as usize);
                let width: u8 = self.read_int(&mut reader)?;
                let ty: u8 = self.read_int(&mut reader)?;
                let value = match ty {
                    0 => {
                        let index: u32 = self.read_int(&mut reader)?;
                        let len: u32 = self.read_int(&mut reader)?;
                        dbg!(index, len);
                        let reloc_section = str_int
                            .slice((index as usize)..((index as usize) + (len as usize)))
                            .unwrap();
                        let pc: u32 = self.read_int(&mut reader)?;
                        let reloc_section = self.str_int.intern(reloc_section);
                        // place the address relative to the start of the section
                        let pc = if let Some(reloc_section) =
                            self.sections.iter().find(|sec| sec.name == reloc_section)
                        {
                            pc + reloc_section.pc
                        } else {
                            return Err(self.err_in(
                                file,
                                &format!("section \"{reloc_section}\" is not defined in config"),
                            ));
                        };
                        RelocVal::Addr(reloc_section, pc)
                    }
                    1 => {
                        let index: u32 = self.read_int(&mut reader)?;
                        let len: u32 = self.read_int(&mut reader)?;
                        let expr = expr_int
                            .slice((index as usize)..((index as usize) + (len as usize)))
                            .unwrap();
                        let expr = self.expr_int.intern(expr);
                        RelocVal::List(expr)
                    }
                    _ => return Err(self.err_in(file, "malformed relocation table")),
                };
                let index: u32 = self.read_int(&mut reader)?;
                let len: u32 = self.read_int(&mut reader)?;
                let unit = str_int
                    .slice((index as usize)..((index as usize) + (len as usize)))
                    .unwrap();
                let index: u32 = self.read_int(&mut reader)?;
                let len: u32 = self.read_int(&mut reader)?;
                let reloc_file = str_int
                    .slice((index as usize)..((index as usize) + (len as usize)))
                    .unwrap();
                let unit = if unit == "__STATIC__" {
                    self.str_int.intern(file)
                } else {
                    self.str_int.intern("__EXPORT__")
                };
                let reloc_file = self.str_int.intern(reloc_file);
                let line: u32 = self.read_int(&mut reader)?;
                let column: u32 = self.read_int(&mut reader)?;
                let pos = Pos {
                    file: reloc_file,
                    line: line as usize,
                    column: column as usize,
                };
                let flags: u8 = self.read_int(&mut reader)?;
                relocs.push(Reloc {
                    offset,
                    width,
                    value,
                    unit,
                    pos,
                    flags,
                });
            }
            // extend section
            if let Some(section) = self.sections.iter_mut().find(|sec| sec.name == name) {
                tracing::trace!("extending section \"{name}\" by {} bytes", data.len());
                section.data.extend(&data);
                section.relocs.extend(relocs);
                section.pc += data.len() as u32;
            } else {
                return Err(self.err_in(
                    file,
                    &format!("section \"{name}\" is not defined in config"),
                ));
            };
        }

        Ok(())
    }

    fn expr_eval(
        &self,
        unit: &'_ str,
        expr: &[ExprNode<'_>],
        sections: &IndexMap<String, ConfigSection>,
    ) -> Option<i32> {
        let mut scratch = Vec::new();
        for node in expr.iter() {
            match *node {
                ExprNode::Const(value) => scratch.push(value),
                ExprNode::Label(label) => {
                    let sym = self.syms.iter().find(|sym| {
                        (sym.label == label) && ((sym.unit == unit) || (sym.unit == "__EXPORT__"))
                    })?;
                    match sym.value {
                        Expr::Const(value) => scratch.push(value),
                        Expr::Addr(section, pc) => {
                            let section = self
                                .sections
                                .iter()
                                .find(|sec| sec.name == section)
                                .unwrap();
                            scratch.push((pc + section.pc) as i32);
                        }
                        // expand the sub-expression recursively
                        Expr::List(expr) => {
                            scratch.push(self.expr_eval(unit, expr, sections)?);
                        }
                    }
                }
                ExprNode::Tag(label, tag) => {
                    let sym = self.syms.iter().find(|sym| {
                        (sym.label == label) && ((sym.unit == unit) || (sym.unit == "__EXPORT__"))
                    })?;
                    let tags = sections[sym.section].tags.as_ref();
                    let value = tags?.get(tag)?;
                    scratch.push(*value);
                }
                ExprNode::Op(op) => {
                    let rhs = scratch.pop().unwrap();
                    match op {
                        Op::Unary(Tok::PLUS) => scratch.push(rhs),
                        Op::Unary(Tok::MINUS) => scratch.push(-rhs),
                        Op::Unary(Tok::TILDE) => scratch.push(!rhs),
                        Op::Unary(Tok::BANG) => scratch.push((rhs == 0) as i32),
                        Op::Unary(Tok::LT) => scratch.push(((rhs as u32) & 0xFF) as i32),
                        Op::Unary(Tok::GT) => scratch.push((((rhs as u32) & 0xFF00) >> 8) as i32),
                        Op::Unary(Tok::CARET) => {
                            scratch.push((((rhs as u32) & 0xFF0000) >> 16) as i32)
                        }
                        Op::Binary(tok) => {
                            let lhs = scratch.pop().unwrap();
                            match tok {
                                Tok::PLUS => scratch.push(lhs.wrapping_add(rhs)),
                                Tok::MINUS => scratch.push(lhs.wrapping_sub(rhs)),
                                Tok::STAR => scratch.push(lhs.wrapping_mul(rhs)),
                                Tok::SOLIDUS => scratch.push(lhs.wrapping_div(rhs)),
                                Tok::MODULUS => scratch.push(lhs.wrapping_rem(rhs)),
                                Tok::ASL => scratch.push(lhs.wrapping_shl(rhs as u32)),
                                Tok::ASR => scratch.push(lhs.wrapping_shr(rhs as u32)),
                                Tok::LSR => {
                                    scratch.push((lhs as u32).wrapping_shl(rhs as u32) as i32)
                                }
                                Tok::LT => scratch.push((lhs < rhs) as i32),
                                Tok::LTE => scratch.push((lhs <= rhs) as i32),
                                Tok::GT => scratch.push((lhs > rhs) as i32),
                                Tok::GTE => scratch.push((lhs >= rhs) as i32),
                                Tok::LEQ => scratch.push((lhs == rhs) as i32),
                                Tok::NEQ => scratch.push((lhs != rhs) as i32),
                                Tok::AMP => scratch.push(lhs & rhs),
                                Tok::PIPE => scratch.push(lhs | rhs),
                                Tok::CARET => scratch.push(lhs ^ rhs),
                                Tok::AND => scratch.push(((lhs != 0) && (rhs != 0)) as i32),
                                Tok::LOR => scratch.push(((lhs != 0) || (rhs != 0)) as i32),
                                _ => unreachable!(),
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
        scratch.last().copied()
    }
}

struct Memory<'a> {
    name: &'a str,
    pc: u32,
    end: u32,
}

impl<'a> Memory<'a> {
    fn new(name: &'a str, pc: u32, len: u32) -> Self {
        Self {
            name,
            pc,
            end: pc + len,
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum MemoryType {
    RO,
    RW,
}

#[derive(Serialize, Deserialize)]
struct ConfigMemory {
    #[serde(deserialize_with = "deserialize_bases_u32")]
    start: u32,

    #[serde(deserialize_with = "deserialize_bases_u32")]
    size: u32,

    #[serde(rename = "type")]
    ty: MemoryType,

    #[serde(default, deserialize_with = "deserialize_bases_u8")]
    fill: Option<u8>,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum SectionType {
    RO,
    RW,
    BSS,
}

fn one() -> u32 {
    1
}

#[derive(Serialize, Deserialize)]
struct ConfigSection {
    load: String,

    #[serde(rename = "type")]
    ty: SectionType,

    #[serde(default, deserialize_with = "deserialize_tags")]
    tags: Option<HashMap<String, i32>>,

    #[serde(default)]
    define: bool,

    #[serde(default = "one", deserialize_with = "deserialize_bases_u32")]
    align: u32,

    #[serde(default)]
    files: Option<Vec<PathBuf>>,
}

#[derive(Serialize, Deserialize)]
struct Script {
    #[serde(rename = "MEMORY")]
    memories: IndexMap<String, ConfigMemory>,

    #[serde(rename = "SECTIONS")]
    sections: IndexMap<String, ConfigSection>,
}

fn deserialize_tags<'de, D>(deserializer: D) -> Result<Option<HashMap<String, i32>>, D::Error>
where
    D: Deserializer<'de>,
{
    let string_map = Option::<HashMap<String, String>>::deserialize(deserializer)?;
    if let Some(string_map) = string_map {
        let mut map = HashMap::new();
        for (name, buf) in string_map {
            let value = if buf.starts_with('$') {
                i32::from_str_radix(&buf[1..], 16).map_err(|e| {
                    de::Error::custom(format!("{buf} is not a valid base 16 value: {e}"))
                })?
            } else if buf.starts_with('%') {
                i32::from_str_radix(&buf[1..], 2).map_err(|e| {
                    de::Error::custom(format!("{buf} is not a valid base 2 value: {e}"))
                })?
            } else {
                i32::from_str_radix(&buf, 10).map_err(|e| {
                    de::Error::custom(format!("{buf} is not a valid base 10 value: {e}"))
                })?
            };
            map.insert(name, value);
        }
        return Ok(Some(map));
    }
    Ok(None)
}

fn deserialize_bases_u32<'de, D>(deserializer: D) -> Result<u32, D::Error>
where
    D: Deserializer<'de>,
{
    let buf = String::deserialize(deserializer)?;
    if buf.starts_with('$') {
        u32::from_str_radix(&buf[1..], 16)
            .map_err(|e| de::Error::custom(format!("{buf} is not a valid base 16 address: {e}")))
    } else if buf.starts_with('%') {
        u32::from_str_radix(&buf[1..], 2)
            .map_err(|e| de::Error::custom(format!("{buf} is not a valid base 2 address: {e}")))
    } else {
        u32::from_str_radix(&buf, 10)
            .map_err(|e| de::Error::custom(format!("{buf} is not a valid base 10 address: {e}")))
    }
}

fn deserialize_bases_u8<'de, D>(deserializer: D) -> Result<Option<u8>, D::Error>
where
    D: Deserializer<'de>,
{
    Option::<String>::deserialize(deserializer)?
        .map(|buf| {
            if buf.starts_with('$') {
                u8::from_str_radix(&buf[1..], 16).map_err(|e| {
                    de::Error::custom(format!("{buf} is not a valid base 16 value: {e}"))
                })
            } else if buf.starts_with('%') {
                u8::from_str_radix(&buf[1..], 2).map_err(|e| {
                    de::Error::custom(format!("{buf} is not a valid base 2 value: {e}"))
                })
            } else {
                u8::from_str_radix(&buf, 10).map_err(|e| {
                    de::Error::custom(format!("{buf} is not a valid base 10 value: {e}"))
                })
            }
        })
        .transpose()
}

trait FromLeBytes {
    type Buf;

    fn from_le_bytes(buf: Self::Buf) -> Self;
}

macro_rules! impl_le_bytes (( $($int:ident),* ) => {
    $(
        impl FromLeBytes for $int {
            type Buf = [u8; mem::size_of::<Self>()];

            fn from_le_bytes(buf: Self::Buf) -> Self {
                Self::from_le_bytes(buf)
            }
        }
    )*
});

impl_le_bytes!(u8, u32, i32, usize);
