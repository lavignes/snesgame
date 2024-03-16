#![feature(generic_const_exprs)]

use std::{
    collections::{BTreeMap, HashMap},
    error::Error,
    fs::{self, File},
    io::{self, ErrorKind, Read, Write},
    mem,
    path::PathBuf,
    process::ExitCode,
    str::FromStr,
};

use asm::{
    Expr, ExprNode, Label, Op, Pos, Reloc, RelocVal, Section, SliceInterner, StrInterner, Sym, Tok,
};
use clap::Parser;
use serde::{de, Deserialize, Deserializer};
use serde_derive::{Deserialize, Serialize};

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

    /// Output file for debug symbols
    #[arg(short = 'g', long)]
    debug_file: Option<PathBuf>,

    /// Pre-defined symbols (repeatable)
    #[arg(short = 'D', long, value_name="KEY1=val", value_parser = parse_defines::<String, i32>)]
    define: Vec<(String, i32)>,
}

fn parse_defines<T, U>(s: &str) -> Result<(T, U), Box<dyn Error + Send + Sync + 'static>>
where
    T: FromStr,
    T::Err: Error + Send + Sync + 'static,
    U: FromStr,
    U::Err: Error + Send + Sync + 'static,
{
    let pos = s
        .find('=')
        .ok_or_else(|| format!("invalid SYMBOL=value: no `=` found in `{s}`"))?;
    Ok((s[..pos].parse()?, s[pos + 1..].parse()?))
}

fn main() -> ExitCode {
    if let Err(e) = main_real() {
        eprintln!("{e}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn main_real() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let mut config = File::open(args.config).map_err(|e| format!("cant open file: {e}"))?;
    let mut output: Box<dyn Write> = match args.output {
        Some(path) => Box::new(
            File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open(path)
                .map_err(|e| format!("cant open file: {e}"))?,
        ),
        None => Box::new(io::stdout()),
    };

    let mut config_text = String::new();
    config.read_to_string(&mut config_text)?;
    let config: Script = toml::from_str(&config_text)?;

    let mut ld = Ld::new();

    let def_file = ld.str_int.intern("__DEFINES__");
    let def_unit = ld.str_int.intern("__STATIC__");
    for (name, val) in &args.define {
        let string = ld.str_int.intern(name);
        ld.syms.push(Sym::new(
            Label::new(None, string),
            Expr::Const(*val),
            def_unit,
            def_file,
            Pos(0, 0),
        ));
    }

    for (name, mem) in &config.memories {
        let name = ld.str_int.intern(&name);
        ld.memories.push(Memory::new(name, mem.start, mem.size));
    }

    for (name, section) in &config.sections {
        let name = ld.str_int.intern(&name);
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

    eprint!("loading: ");
    for object in args.objects {
        let path = fs::canonicalize(object)?;
        let path = path.to_str().unwrap();
        let file = File::open(path)?;
        ld.load(path, file)?;
        eprint!(".");
    }
    eprintln!("ok");

    eprint!("relocating: ");
    for (name, section) in &config.sections {
        let Ld {
            ref mut sections,
            ref mut memories,
            ..
        } = ld;
        let memory = memories
            .iter_mut()
            .find(|memory| memory.name == section.load)
            .unwrap();
        // TODO section alignment?
        let section = sections
            .iter_mut()
            .find(|section| section.name == name)
            .unwrap();
        // we update the section pc to be its absolute start address in memory
        section.pc = memory.pc;
        memory.pc += section.data.len() as u32;
        if memory.pc >= memory.end {
            Err(io::Error::new(
                ErrorKind::InvalidData,
                format!(
                    "no room left in memory \"{}\" for section \"{name}\"",
                    memory.name
                ),
            ))?;
        }
        eprint!(".");
    }
    eprintln!("ok");

    for pass in 1..=2 {
        eprint!("symbol pass{pass}: ");
        for i in 0..ld.syms.len() {
            let value = match ld.syms[i].value {
                Expr::Const(value) => Expr::Const(value),
                Expr::Addr(section, pc) => {
                    let section = ld.sections.iter().find(|sec| sec.name == section).unwrap();
                    Expr::Const((pc + section.pc) as i32)
                }
                Expr::List(expr) => {
                    if let Some(value) = ld.expr_eval(expr) {
                        Expr::Const(value)
                    } else {
                        Expr::List(expr)
                    }
                }
            };
            ld.syms[i].value = value;
        }
        eprintln!("ok");
    }

    eprint!("symbol pass3: ");
    for sym in &ld.syms {
        if let Expr::Const(_) = sym.value {
            continue;
        }
        Err(ld.err_in(
            sym.unit,
            &format!(
                "undefined symbol \"{}\"\n\tdeclared at {}:{}:{}",
                sym.label, sym.file, sym.pos.0, sym.pos.1
            ),
        ))?;
    }
    eprintln!("ok");

    eprint!("linking: ");
    for i in 0..ld.sections.len() {
        for j in 0..ld.sections[i].relocs.len() {
            let reloc = ld.sections[i].relocs[j];
            let value = match reloc.value {
                RelocVal::Addr(section, pc) => {
                    let section = ld.sections.iter().find(|sec| sec.name == section).unwrap();
                    (pc + section.pc) as i32
                }
                RelocVal::List(expr) => {
                    if let Some(value) = ld.expr_eval(expr) {
                        value
                    } else {
                        Err(ld.err_in(
                            reloc.unit,
                            &format!(
                                "expression cannot be solved\n\tdefined at {}:{}:{}",
                                reloc.file, reloc.pos.0, reloc.pos.1
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
                                reloc.file, reloc.pos.0, reloc.pos.1
                            ),
                        ))?;
                    }
                    ld.sections[i].data[reloc.offset] = value as u8;
                }
                2 => {
                    if (value as u32) > (u16::MAX as u32) {
                        Err(ld.err_in(
                            reloc.unit,
                            &format!(
                                "expression >2 bytes\n\tdefined at {}:{}:{}",
                                reloc.file, reloc.pos.0, reloc.pos.1
                            ),
                        ))?;
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
                                reloc.file, reloc.pos.0, reloc.pos.1
                            ),
                        ))?;
                    }
                    ld.sections[i].data[reloc.offset] = ((value as u32) >> 0) as u8;
                    ld.sections[i].data[reloc.offset + 1] = ((value as u32) >> 8) as u8;
                    ld.sections[i].data[reloc.offset + 2] = ((value as u32) >> 16) as u8;
                }
                _ => unreachable!(),
            }
        }
        eprint!(".");
    }
    eprintln!("ok");

    eprint!("writing: ");
    for section in ld.sections {
        output.write_all(&section.data)?;
    }
    eprintln!("ok");

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
        let mut magic = [0u8; 6];
        reader.read_exact(&mut magic)?;
        if &magic != b"pasm01" {
            return Err(self.err_in(file, "mad magic"));
        }
        // fill up a temporary string table
        let mut str_int: StrInterner<'_> = StrInterner::new();
        {
            let str_len: usize = self.read_int(&mut reader)?;
            let mut storage = String::new();
            storage.extend((0..str_len).map(|_| ' '));
            reader.read_exact(unsafe { storage.as_bytes_mut() })?;
            str_int.storages.push(storage);
        }
        // and a temporary expression table
        let mut expr_int: SliceInterner<ExprNode<'_>> = SliceInterner::new();
        {
            let expr_len: usize = self.read_int(&mut reader)?;
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
                                let index: usize = self.read_int(&mut reader)?;
                                let len: usize = self.read_int(&mut reader)?;
                                let scope = str_int.slice(index..(index + len)).unwrap();
                                let index: usize = self.read_int(&mut reader)?;
                                let len: usize = self.read_int(&mut reader)?;
                                let string = str_int.slice(index..(index + len)).unwrap();
                                let scope = self.str_int.intern(scope);
                                let string = self.str_int.intern(string);
                                storage.push(ExprNode::Label(Label::new(Some(scope), string)));
                            }
                            1 => {
                                let index: usize = self.read_int(&mut reader)?;
                                let len: usize = self.read_int(&mut reader)?;
                                let string = str_int.slice(index..(index + len)).unwrap();
                                let string = self.str_int.intern(string);
                                storage.push(ExprNode::Label(Label::new(None, string)));
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
        let syms_len: usize = self.read_int(&mut reader)?;
        for _ in 0..syms_len {
            let ty: u8 = self.read_int(&mut reader)?;
            let label = match ty {
                0 => {
                    let index: usize = self.read_int(&mut reader)?;
                    let len: usize = self.read_int(&mut reader)?;
                    let scope = str_int.slice(index..(index + len)).unwrap();
                    let index: usize = self.read_int(&mut reader)?;
                    let len: usize = self.read_int(&mut reader)?;
                    let string = str_int.slice(index..(index + len)).unwrap();
                    let scope = self.str_int.intern(scope);
                    let string = self.str_int.intern(string);
                    Label::new(Some(scope), string)
                }
                1 => {
                    let index: usize = self.read_int(&mut reader)?;
                    let len: usize = self.read_int(&mut reader)?;
                    let string = str_int.slice(index..(index + len)).unwrap();
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
                    let index: usize = self.read_int(&mut reader)?;
                    let len: usize = self.read_int(&mut reader)?;
                    let section = str_int.slice(index..(index + len)).unwrap();
                    let pc: u32 = self.read_int(&mut reader)?;
                    let section = self.str_int.intern(section);
                    // place the address relative to the start of the section
                    let pc = if let Some(section) =
                        self.sections.iter().find(|sec| sec.name == section)
                    {
                        pc + section.pc
                    } else {
                        return Err(self.err_in(
                            file,
                            &format!("section \"{section}\" is not defined in config"),
                        ));
                    };
                    Expr::Addr(section, pc)
                }
                2 => {
                    let index: usize = self.read_int(&mut reader)?;
                    let len: usize = self.read_int(&mut reader)?;
                    let expr = expr_int.slice(index..(index + len)).unwrap();
                    let expr = self.expr_int.intern(expr);
                    Expr::List(expr)
                }
                _ => return Err(self.err_in(file, "malformed symbol table")),
            };
            let index: usize = self.read_int(&mut reader)?;
            let len: usize = self.read_int(&mut reader)?;
            let unit = str_int.slice(index..(index + len)).unwrap();
            // hide static symbols under the object file name
            let unit = if unit == "__STATIC__" {
                self.str_int.intern(file)
            } else {
                self.str_int.intern(unit)
            };
            let index: usize = self.read_int(&mut reader)?;
            let len: usize = self.read_int(&mut reader)?;
            let sym_file = str_int.slice(index..(index + len)).unwrap();
            let sym_file = self.str_int.intern(sym_file);
            let line: usize = self.read_int(&mut reader)?;
            let column: usize = self.read_int(&mut reader)?;
            let pos = Pos(line, column);
            // duplicate exported symbol?
            if let Some(other) = self
                .syms
                .iter()
                .find(|sym| (sym.label == label) && (sym.unit == unit))
            {
                return Err(self.err_in(file, &format!("duplicate exported symbol \"{label}\" found\n\tdefined at {}:{}:{}\n\tagain at {sym_file}:{line}:{column}", other.file, other.pos.0, other.pos.1)));
            }
            self.syms.push(Sym::new(label, value, unit, sym_file, pos));
        }
        // add to sections
        let sections_len: usize = self.read_int(&mut reader)?;
        for _ in 0..sections_len {
            let index: usize = self.read_int(&mut reader)?;
            let len: usize = self.read_int(&mut reader)?;
            let name = str_int.slice(index..(index + len)).unwrap();
            let name = self.str_int.intern(name);
            let section = if let Some(section) = self.sections.iter().find(|sec| sec.name == name) {
                section
            } else {
                return Err(self.err_in(
                    file,
                    &format!("section \"{name}\" is not defined in config"),
                ));
            };
            let data_len: usize = self.read_int(&mut reader)?;
            let mut data = Vec::new();
            data.extend((0..data_len).map(|_| 0));
            reader.read_exact(&mut data)?;
            // TODO this seems messy,
            let mut relocs = Vec::new();
            let relocs_len: usize = self.read_int(&mut reader)?;
            for _ in 0..relocs_len {
                let offset: usize = self.read_int(&mut reader)?;
                // place the offset relative to the start of the section
                let offset = offset + (section.pc as usize);
                let width: u8 = self.read_int(&mut reader)?;
                let ty: u8 = self.read_int(&mut reader)?;
                let value = match ty {
                    0 => {
                        let index: usize = self.read_int(&mut reader)?;
                        let len: usize = self.read_int(&mut reader)?;
                        let section = str_int.slice(index..(index + len)).unwrap();
                        let pc: u32 = self.read_int(&mut reader)?;
                        let section = self.str_int.intern(section);
                        // place the address relative to the start of the section
                        let pc = if let Some(section) =
                            self.sections.iter().find(|sec| sec.name == section)
                        {
                            pc + section.pc
                        } else {
                            return Err(self.err_in(
                                file,
                                &format!("section \"{section}\" is not defined in config"),
                            ));
                        };
                        RelocVal::Addr(section, pc)
                    }
                    1 => {
                        let index: usize = self.read_int(&mut reader)?;
                        let len: usize = self.read_int(&mut reader)?;
                        let expr = expr_int.slice(index..(index + len)).unwrap();
                        let expr = self.expr_int.intern(expr);
                        RelocVal::List(expr)
                    }
                    _ => return Err(self.err_in(file, "malformed relocation table")),
                };
                let index: usize = self.read_int(&mut reader)?;
                let len: usize = self.read_int(&mut reader)?;
                let unit = str_int.slice(index..(index + len)).unwrap();
                let index: usize = self.read_int(&mut reader)?;
                let len: usize = self.read_int(&mut reader)?;
                let reloc_file = str_int.slice(index..(index + len)).unwrap();
                let unit = if unit == "__STATIC__" {
                    self.str_int.intern(file)
                } else {
                    self.str_int.intern(unit)
                };
                let reloc_file = self.str_int.intern(reloc_file);
                let line: usize = self.read_int(&mut reader)?;
                let column: usize = self.read_int(&mut reader)?;
                let pos = Pos(line, column);
                relocs.push(Reloc {
                    offset,
                    width,
                    value,
                    unit,
                    file: reloc_file,
                    pos,
                });
            }
            // extend section
            if let Some(section) = self.sections.iter_mut().find(|sec| sec.name == name) {
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

    fn expr_eval(&self, expr: &[ExprNode<'_>]) -> Option<i32> {
        let mut scratch = Vec::new();
        for node in expr.iter() {
            match *node {
                ExprNode::Const(value) => scratch.push(value),
                ExprNode::Label(label) => {
                    let sym = self.syms.iter().find(|sym| sym.label == label)?;
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
                            scratch.push(self.expr_eval(expr)?);
                        }
                    }
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
                                Tok::EQ => scratch.push((lhs == rhs) as i32),
                                Tok::NEQ => scratch.push((lhs != rhs) as i32),
                                Tok::AMP => scratch.push(lhs & rhs),
                                Tok::PIPE => scratch.push(lhs | rhs),
                                Tok::CARET => scratch.push(lhs ^ rhs),
                                Tok::AND => scratch.push(((lhs != 0) && (rhs != 0)) as i32),
                                Tok::OR => scratch.push(((lhs != 0) || (rhs != 0)) as i32),
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
struct ScriptMemory {
    #[serde(deserialize_with = "deserialize_bases")]
    start: u32,

    #[serde(deserialize_with = "deserialize_bases")]
    size: u32,

    #[serde(rename = "type")]
    ty: MemoryType,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum SectionType {
    RO,
    RW,
    BSS,
}

#[derive(Serialize, Deserialize)]
struct ScriptSection {
    load: String,

    #[serde(rename = "type")]
    ty: SectionType,
}

#[derive(Serialize, Deserialize)]
struct Script {
    #[serde(rename = "MEMORY")]
    memories: HashMap<String, ScriptMemory>,

    #[serde(rename = "SECTIONS")]
    sections: BTreeMap<String, ScriptSection>,
}

fn deserialize_bases<'de, D>(deserializer: D) -> Result<u32, D::Error>
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
