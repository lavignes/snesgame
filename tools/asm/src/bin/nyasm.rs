#![feature(let_chains)]
#![feature(stmt_expr_attributes)]

use std::{
    collections::{HashSet, VecDeque},
    error::Error,
    fmt::Write as FmtWrite,
    fs::{self, File},
    io::{self, BufWriter, ErrorKind, Read, Seek, Write},
    mem,
    path::PathBuf,
    process::ExitCode,
    str::FromStr,
};

use clap::Parser;
use nyasm::{
    Expr, ExprNode, Label, Op, Pos, Reloc, RelocFlags, RelocVal, Section, SliceInterner,
    StrInterner, Sym, SymFlags, Tok,
};
use tracing::Level;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Assembly source file
    source: PathBuf,

    /// Output file (default: stdout)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Pre-defined symbols (repeatable)
    #[arg(short = 'D', long, value_name="KEY1=val", value_parser = nyasm::parse_defines::<String, i32>)]
    define: Vec<(String, i32)>,

    /// Search directories for included files
    #[arg(short = 'I', long)]
    include: Vec<PathBuf>,

    /// Output makefile dependencies lines instead of object file
    #[arg(short = 'M')]
    make_depend: bool,

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
    let input = fs::canonicalize(args.source.clone())?;
    let input = input.to_str().unwrap();
    let file = File::open(input).map_err(|e| format!("cant open file: {e}"))?;
    let lexer = Lexer::new(file, input);

    let mut asm = Asm::new(lexer, args.include);
    asm.str_int.intern(input); // dont forget to intern the input name
    let def_file_section = asm.str_int.intern("__DEFINES__");
    let def_unit = asm.str_int.intern("__STATIC__");
    for (name, val) in &args.define {
        let string = asm.str_int.intern(name);
        asm.syms.push(Sym {
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

    tracing::trace!("starting pass 1");
    asm.pass()?;

    tracing::trace!("starting pass 2");
    asm.rewind()?;
    asm.pass()?;

    let mut output: Box<dyn Write> = match args.output.clone() {
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

    if !args.make_depend {
        output.write_all("nyasm01".as_bytes())?;
        let len = asm
            .str_int
            .storages
            .iter()
            .fold(0, |accum, storage| accum + storage.len());
        output.write_all(&(len as u32).to_le_bytes())?;
        for storage in &asm.str_int.storages {
            output.write_all(storage.as_bytes())?;
        }
        let len = asm
            .expr_int
            .storages
            .iter()
            .fold(0, |accum, storage| accum + storage.len());
        output.write_all(&(len as u32).to_le_bytes())?;
        for storage in &asm.expr_int.storages {
            for expr in storage {
                match expr {
                    ExprNode::Const(value) => {
                        output.write_all(&[0])?;
                        output.write_all(&value.to_le_bytes())?;
                    }
                    ExprNode::Op(op) => {
                        output.write_all(&[1])?;
                        match op {
                            Op::Binary(op) => {
                                output.write_all(&[0])?;
                                output.write_all(&[op.0])?;
                            }
                            Op::Unary(op) => {
                                output.write_all(&[1])?;
                                output.write_all(&[op.0])?;
                            }
                        }
                    }
                    ExprNode::Label(label) => {
                        output.write_all(&[2])?;
                        if let Some(scope) = label.scope {
                            output.write_all(&[0])?;
                            let index = asm.str_int.offset(scope).unwrap();
                            output.write_all(&(index as u32).to_le_bytes())?;
                            output.write_all(&(scope.len() as u32).to_le_bytes())?;
                        } else {
                            output.write_all(&[1])?;
                        }
                        let index = asm.str_int.offset(label.string).unwrap();
                        output.write_all(&(index as u32).to_le_bytes())?;
                        output.write_all(&(label.string.len() as u32).to_le_bytes())?;
                    }
                    ExprNode::Tag(label, tag) => {
                        output.write_all(&[3])?;
                        if let Some(scope) = label.scope {
                            output.write_all(&[0])?;
                            let index = asm.str_int.offset(scope).unwrap();
                            output.write_all(&(index as u32).to_le_bytes())?;
                            output.write_all(&(scope.len() as u32).to_le_bytes())?;
                        } else {
                            output.write_all(&[1])?;
                        }
                        let index = asm.str_int.offset(label.string).unwrap();
                        output.write_all(&(index as u32).to_le_bytes())?;
                        output.write_all(&(label.string.len() as u32).to_le_bytes())?;
                        let index = asm.str_int.offset(tag).unwrap();
                        output.write_all(&(index as u32).to_le_bytes())?;
                        output.write_all(&(tag.len() as u32).to_le_bytes())?;
                    }
                }
            }
        }
        output.write_all(&(asm.syms.len() as u32).to_le_bytes())?;
        for sym in &asm.syms {
            if let Some(scope) = sym.label.scope {
                output.write_all(&[0])?;
                let index = asm.str_int.offset(scope).unwrap();
                output.write_all(&(index as u32).to_le_bytes())?;
                output.write_all(&(scope.len() as u32).to_le_bytes())?;
            } else {
                output.write_all(&[1])?;
            }
            let index = asm.str_int.offset(sym.label.string).unwrap();
            output.write_all(&(index as u32).to_le_bytes())?;
            output.write_all(&(sym.label.string.len() as u32).to_le_bytes())?;
            match sym.value {
                Expr::Const(value) => {
                    output.write_all(&[0])?;
                    output.write_all(&value.to_le_bytes())?;
                }
                Expr::Addr(section, pc) => {
                    output.write_all(&[1])?;
                    let index = asm.str_int.offset(section).unwrap();
                    output.write_all(&(index as u32).to_le_bytes())?;
                    output.write_all(&(section.len() as u32).to_le_bytes())?;
                    output.write_all(&pc.to_le_bytes())?;
                }
                Expr::List(expr) => {
                    output.write_all(&[2])?;
                    let index = asm.expr_int.offset(expr).unwrap();
                    output.write_all(&(index as u32).to_le_bytes())?;
                    output.write_all(&(expr.len() as u32).to_le_bytes())?;
                }
            }
            let index = asm.str_int.offset(sym.unit).unwrap();
            output.write_all(&(index as u32).to_le_bytes())?;
            output.write_all(&(sym.unit.len() as u32).to_le_bytes())?;
            let index = asm.str_int.offset(sym.section).unwrap();
            output.write_all(&(index as u32).to_le_bytes())?;
            output.write_all(&(sym.section.len() as u32).to_le_bytes())?;
            let index = asm.str_int.offset(sym.pos.file).unwrap();
            output.write_all(&(index as u32).to_le_bytes())?;
            output.write_all(&(sym.pos.file.len() as u32).to_le_bytes())?;
            output.write_all(&(sym.pos.line as u32).to_le_bytes())?;
            output.write_all(&(sym.pos.column as u32).to_le_bytes())?;
            output.write_all(&[sym.flags])?;
        }
        // filter out empty sections
        let count = asm
            .sections
            .iter()
            .filter(|section| !section.data.is_empty())
            .count();
        tracing::trace!("writing {count} sections");
        output.write_all(&(count as u32).to_le_bytes())?;
        for section in asm.sections {
            if section.data.is_empty() {
                continue;
            }
            tracing::trace!(
                "writing {} bytes of section \"{}\"",
                section.data.len(),
                section.name,
            );
            let index = asm.str_int.offset(section.name).unwrap();
            output.write_all(&(index as u32).to_le_bytes())?;
            output.write_all(&(section.name.len() as u32).to_le_bytes())?;
            output.write_all(&(section.data.len() as u32).to_le_bytes())?;
            output.write_all(&section.data)?;
            output.write_all(&(section.relocs.len() as u32).to_le_bytes())?;
            for reloc in section.relocs {
                output.write_all(&(reloc.offset as u32).to_le_bytes())?;
                output.write_all(&reloc.width.to_le_bytes())?;
                match reloc.value {
                    RelocVal::Addr(section, pc) => {
                        output.write_all(&[0])?;
                        let index = asm.str_int.offset(section).unwrap();
                        output.write_all(&(index as u32).to_le_bytes())?;
                        output.write_all(&(section.len() as u32).to_le_bytes())?;
                        output.write_all(&pc.to_le_bytes())?;
                    }
                    RelocVal::List(expr) => {
                        output.write_all(&[1])?;
                        let index = asm.expr_int.offset(expr).unwrap();
                        output.write_all(&(index as u32).to_le_bytes())?;
                        output.write_all(&(expr.len() as u32).to_le_bytes())?;
                    }
                }
                let index = asm.str_int.offset(reloc.unit).unwrap();
                output.write_all(&(index as u32).to_le_bytes())?;
                output.write_all(&(reloc.unit.len() as u32).to_le_bytes())?;
                let index = asm.str_int.offset(reloc.pos.file).unwrap();
                output.write_all(&(index as u32).to_le_bytes())?;
                output.write_all(&(reloc.pos.file.len() as u32).to_le_bytes())?;
                output.write_all(&(reloc.pos.line as u32).to_le_bytes())?;
                output.write_all(&(reloc.pos.column as u32).to_le_bytes())?;
                output.write_all(&[reloc.flags])?;
            }
        }
    }

    if args.make_depend {
        let mut obj = args.source.clone();
        obj.set_extension("o");
        for include in asm.included {
            writeln!(output, "{}: {}", obj.display(), include.display())?;
        }
    }

    tracing::debug!("symbols: {}", asm.syms.len());
    tracing::debug!(
        "string heap: {}/{} bytes",
        asm.str_int
            .storages
            .iter()
            .fold(0, |accum, storage| accum + storage.len()),
        asm.str_int
            .storages
            .iter()
            .fold(0, |accum, storage| accum + storage.capacity())
    );
    tracing::debug!(
        "macro heap: {}/{} bytes",
        asm.tok_int.storages.iter().fold(0, |accum, storage| accum
            + (storage.len() * mem::size_of::<MacroTok>())),
        asm.tok_int.storages.iter().fold(0, |accum, storage| accum
            + (storage.capacity() * mem::size_of::<MacroTok>()))
    );
    tracing::debug!(
        "expr heap: {}/{} bytes",
        asm.expr_int.storages.iter().fold(0, |accum, storage| accum
            + (storage.len() * mem::size_of::<ExprNode>())),
        asm.expr_int.storages.iter().fold(0, |accum, storage| accum
            + (storage.capacity() * mem::size_of::<ExprNode>()))
    );

    Ok(())
}

struct Asm<'a> {
    toks: Vec<Box<dyn TokStream<'a> + 'a>>,
    str_int: StrInterner<'a>,
    tok_int: SliceInterner<(Pos<'a>, MacroTok<'a>)>,
    loop_int: SliceInterner<(Pos<'a>, LoopTok<'a>)>,
    expr_int: SliceInterner<ExprNode<'a>>,
    sections: Vec<Section<'a>>,
    section: usize,
    syms: Vec<Sym<'a>>,
    scope: Option<&'a str>,
    emit: bool,
    index8: bool,
    accum8: bool,
    emulation: bool,
    if_level: usize,
    includes: Vec<PathBuf>,     // from args
    included: HashSet<PathBuf>, // for tracking usage with -M flag

    macros: Vec<Macro<'a>>,
    unique: usize, // unique id generator for macro invocations

    // expr parsing
    expr_buffer: Vec<ExprNode<'a>>,
    operator_buffer: Vec<Op>,
}

impl<'a> Asm<'a> {
    fn new<R: Read + Seek + 'static>(lexer: Lexer<'a, R>, includes: Vec<PathBuf>) -> Self {
        let mut str_int = StrInterner::new();
        let code = str_int.intern("__CODE__");
        Self {
            toks: vec![Box::new(lexer)],
            str_int,
            tok_int: SliceInterner::new(),
            loop_int: SliceInterner::new(),
            expr_int: SliceInterner::new(),
            sections: vec![Section::new(code)],
            section: 0,
            syms: Vec::new(),
            scope: None,
            emit: false,
            index8: true,
            accum8: true,
            emulation: true,
            if_level: 0,
            includes,
            included: HashSet::new(),

            macros: Vec::new(),
            unique: 0,

            expr_buffer: Vec::new(),
            operator_buffer: Vec::new(),
        }
    }

    fn rewind(&mut self) -> io::Result<()> {
        self.toks.last_mut().unwrap().rewind()?;
        self.sections = vec![Section::new(self.str_int.intern("__CODE__"))];
        self.section = 0;
        self.scope = None;
        self.emit = true;
        self.index8 = true;
        self.accum8 = true;
        self.emulation = true;
        self.if_level = 0;
        self.unique = 0;
        Ok(())
    }

    fn pass(&mut self) -> io::Result<()> {
        while self.peek()? != Tok::EOF {
            // skip newlines
            if self.peek()? == Tok::NEWLINE {
                self.eat();
                continue;
            }
            // special case, setting the PC
            if self.peek()? == Tok::STAR {
                self.eat();
                if self.peek()? != Tok::EQU {
                    return Err(self.err("expected ="));
                }
                self.eat();
                let expr = self.expr()?;
                let expr = self.const_expr(expr)?;
                self.set_pc(self.range_24(expr)?);
                self.eol()?;
                continue;
            }
            if self.peek()? == Tok::ID {
                let mne = MNEMONICS.iter().find(|mne| self.str_like(&mne.0 .0));
                // is this a label?
                if mne.is_none() {
                    // is this a macro?
                    if let Some(mac) = self
                        .macros
                        .iter()
                        .find(|mac| self.str() == mac.name)
                        .copied()
                    {
                        self.macroinvoke(mac)?;
                        continue;
                    }
                    let pos = self.tok().pos();
                    let string = self.str_intern();
                    let label = if let Some(index) = string.find('.') {
                        let (scope, string) = string.split_at(index);
                        if scope.is_empty() {
                            Label::new(self.scope, string)
                        } else {
                            Label::new(Some(scope), string)
                        }
                    } else {
                        Label::new(None, string)
                    };
                    self.eat();

                    let index = if let Some(item) = self
                        .syms
                        .iter()
                        .enumerate()
                        .find(|item| &item.1.label == &label)
                    {
                        // allowed to redef during second pass
                        // TODO should test if label value didnt change
                        // TODO allow variable kinds that are redefinable
                        if !self.emit {
                            return Err(self.err("symbol already defined"));
                        }
                        item.0
                    } else {
                        // save in the symbol table with temporary value
                        let index = self.syms.len();
                        let unit = self.str_int.intern("__STATIC__");
                        let section = self.sections[self.section].name;
                        self.syms.push(Sym {
                            label,
                            value: Expr::Const(0),
                            unit,
                            section,
                            pos,
                            flags: 0,
                        });
                        index
                    };

                    match self.peek()? {
                        // optional colon
                        Tok::COLON => {
                            self.eat();
                        }
                        // export colons
                        Tok::DUB => {
                            self.eat();
                            if self.emit {
                                let unit = self.str_int.intern("__EXPORT__");
                                if let Some(sym) =
                                    self.syms.iter_mut().find(|sym| sym.label == label)
                                {
                                    if sym.unit == unit {
                                        return Err(self.err("symbol is already exported"));
                                    }
                                    sym.unit = unit;
                                }
                            }
                        }
                        Tok::EQU => {
                            self.eat();
                            let expr = self.expr()?;
                            // equ's must always be const, either on the first or second pass
                            if self.emit {
                                self.syms[index].value = Expr::Const(self.const_expr(expr)?);
                                self.syms[index].flags = SymFlags::EQU;
                            } else if let Expr::Const(expr) = expr {
                                self.syms[index].value = Expr::Const(expr);
                                self.syms[index].flags = SymFlags::EQU;
                            } else {
                                // we couldn't evaluate this yet, so remove it
                                self.syms.pop();
                            }
                            self.eol()?;
                            continue;
                        }
                        _ => {}
                    }
                    // set the scope
                    if !string.starts_with(".") {
                        self.scope.replace(string);
                    }

                    // otherwise it is a pointer to the current PC
                    let section = self.sections[self.section].name;
                    self.syms[index].value = Expr::Addr(section, self.pc());
                    continue;
                }
                if mne.is_none() {
                    return Err(self.err("unrecognized instruction"));
                }
                self.eat();
                self.operand(*mne.unwrap())?;
                self.eol()?;
                continue;
            }
            self.directive()?;
        }
        Ok(())
    }

    fn pc(&self) -> u32 {
        self.sections[self.section].pc
    }

    fn set_pc(&mut self, val: u32) {
        self.sections[self.section].pc = val;
    }

    fn add_pc(&mut self, amt: u32) -> io::Result<()> {
        let val = self.pc().wrapping_add(amt);
        if (val < self.pc()) || (val > 0x01000000) {
            return Err(self.err("pc overflow"));
        }
        self.set_pc(val);
        Ok(())
    }

    fn err(&self, msg: &str) -> io::Error {
        self.tok().err(msg)
    }

    fn str(&self) -> &str {
        self.tok().str()
    }

    fn str_like(&self, string: &str) -> bool {
        self.tok().str().eq_ignore_ascii_case(string)
    }

    fn str_intern(&mut self) -> &'a str {
        let Self {
            ref mut str_int,
            toks,
            ..
        } = self;
        let string = toks.last().unwrap().str();
        str_int.intern(string)
    }

    fn peek(&mut self) -> io::Result<Tok> {
        match self.tok_mut().peek() {
            Ok(Tok::EOF) if self.toks.len() > 1 => {
                // TODO: yuck recursion
                self.toks.pop();
                self.eat();
                self.peek()
            }
            tok => tok,
        }
    }

    fn eat(&mut self) {
        self.tok_mut().eat();
    }

    fn write(&mut self, buf: &[u8]) {
        self.sections[self.section].data.extend_from_slice(buf);
    }

    fn write_str(&mut self) {
        let Self {
            ref mut sections,
            section,
            toks,
            ..
        } = self;
        sections[*section]
            .data
            .extend_from_slice(toks.last().unwrap().str().as_bytes());
    }

    fn tok(&self) -> &dyn TokStream<'a> {
        self.toks.last().unwrap().as_ref()
    }

    fn tok_mut(&mut self) -> &mut dyn TokStream<'a> {
        self.toks.last_mut().unwrap().as_mut()
    }

    fn eol(&mut self) -> io::Result<()> {
        match self.peek()? {
            Tok::NEWLINE | Tok::EOF => {
                self.eat();
                Ok(())
            }
            _ => Err(self.err("expected end of line")),
        }
    }

    fn const_expr(&self, expr: Expr<'_>) -> io::Result<i32> {
        match expr {
            Expr::Const(value) => Ok(value),
            Expr::List(expr) => {
                if let Some(value) = self.expr_eval(expr) {
                    Ok(value)
                } else {
                    Err(self.err("expression must be constant"))
                }
            }
            _ => Err(self.err("expression must be constant")),
        }
    }

    fn const_branch_expr(&self, expr: Expr<'_>) -> io::Result<i32> {
        match expr {
            Expr::Const(value) => Ok(value),
            Expr::Addr(section, pc) => {
                if self.sections[self.section].name == section {
                    Ok(pc as i32)
                } else {
                    Err(self.err("branch expression must be constant"))
                }
            }
            Expr::List(expr) => {
                if let Some(value) = self.expr_branch_eval(expr, true) {
                    Ok(value)
                } else {
                    Err(self.err("branch expression must be constant"))
                }
            }
        }
    }

    fn range_24(&self, value: i32) -> io::Result<u32> {
        if (value as u32) > 0x00FFFFFFu32 {
            return Err(self.err("expression >3 bytes"));
        }
        Ok(value as u32)
    }

    fn range_16(&self, value: i32) -> io::Result<u16> {
        if (value as u32) > (u16::MAX as u32) {
            return Err(self.err("expression >2 bytes"));
        }
        Ok(value as u16)
    }

    fn range_8(&self, value: i32) -> io::Result<u8> {
        if (value as u32) > (u8::MAX as u32) {
            return Err(self.err("expression >1 byte"));
        }
        Ok(value as u8)
    }

    fn expr_precedence(&self, op: Op) -> u8 {
        match op {
            Op::Unary(Tok::LPAREN) => 0xFF, // lparen is lowest precedence
            Op::Unary(_) => 0,              // other unary is highest precedence
            Op::Binary(Tok::SOLIDUS | Tok::MODULUS | Tok::STAR) => 1,
            Op::Binary(Tok::PLUS | Tok::MINUS) => 2,
            Op::Binary(Tok::ASL | Tok::ASR | Tok::LSR) => 3,
            Op::Binary(Tok::LT | Tok::LTE | Tok::GT | Tok::GTE) => 4,
            Op::Binary(Tok::LEQ | Tok::NEQ) => 5,
            Op::Binary(Tok::AMP) => 6,
            Op::Binary(Tok::CARET) => 7,
            Op::Binary(Tok::PIPE) => 8,
            Op::Binary(Tok::AND) => 9,
            Op::Binary(Tok::LOR) => 10,
            _ => unreachable!(),
        }
    }

    fn expr_push_apply(&mut self, op: Op) {
        while let Some(top) = self.operator_buffer.last().copied() {
            if self.expr_precedence(top) > self.expr_precedence(op) {
                break;
            }
            self.operator_buffer.pop();
            self.expr_buffer.push(ExprNode::Op(top));
        }
        self.operator_buffer.push(op);
    }

    fn expr(&mut self) -> io::Result<Expr<'a>> {
        self.expr_buffer.clear();
        self.operator_buffer.clear();
        let mut seen_val = false;
        let mut paren_depth = 0;
        // sort of a pratt/shunting-yard algorithm combo
        loop {
            match self.peek()? {
                // star is multiply or the PC (as a const, intentionally)
                Tok::STAR => {
                    if !seen_val {
                        self.expr_buffer.push(ExprNode::Const(self.pc() as i32));
                        seen_val = true;
                        self.eat();
                        continue;
                    }
                    self.expr_push_apply(Op::Binary(Tok::STAR));
                    seen_val = false;
                    self.eat();
                    continue;
                }
                // these are optionally unary
                tok @ (Tok::PLUS | Tok::MINUS | Tok::CARET | Tok::LT | Tok::GT) => {
                    if seen_val {
                        self.expr_push_apply(Op::Binary(tok));
                    } else {
                        self.expr_push_apply(Op::Unary(tok));
                    }
                    seen_val = false;
                    self.eat();
                    continue;
                }
                // always unary
                tok @ (Tok::BANG | Tok::TILDE) => {
                    self.expr_push_apply(Op::Unary(tok));
                    seen_val = false;
                    self.eat();
                    continue;
                }
                #[rustfmt::skip]
                tok @ (Tok::AMP | Tok::PIPE | Tok::AND | Tok::LOR | Tok::SOLIDUS | Tok::MODULUS
                       | Tok::ASL | Tok::ASR | Tok::LSR | Tok::LTE | Tok::GTE | Tok::LEQ | Tok::NEQ
                      ) => {
                    if !seen_val {
                        return Err(self.err("expected value"));
                    }
                    self.expr_push_apply(Op::Binary(tok));
                    seen_val = false;
                    self.eat();
                    continue;
                }
                Tok::NUM => {
                    if seen_val {
                        return Err(self.err("expected operator"));
                    }
                    self.expr_buffer.push(ExprNode::Const(self.tok().num()));
                    seen_val = true;
                    self.eat();
                    continue;
                }
                Tok::LPAREN => {
                    if seen_val {
                        return Err(self.err("expected operator"));
                    }
                    paren_depth += 1;
                    self.operator_buffer.push(Op::Unary(Tok::LPAREN));
                    seen_val = false;
                    self.eat();
                    continue;
                }
                Tok::RPAREN => {
                    // this rparen is probably part of an indirect address
                    if paren_depth == 0 {
                        break;
                    }
                    paren_depth -= 1;
                    if !seen_val {
                        return Err(self.err("expected value"));
                    }
                    loop {
                        if let Some(op) = self.operator_buffer.pop() {
                            // we apply ops until we see the start of this grouping
                            match op {
                                Op::Binary(tok) | Op::Unary(tok) if tok == Tok::LPAREN => {
                                    break;
                                }
                                _ => {}
                            }
                            self.expr_buffer.push(ExprNode::Op(op));
                        } else {
                            return Err(self.err("unbalanced parens"));
                        }
                    }
                    self.eat();
                    continue;
                }
                Tok::ID => {
                    // is this a macro?
                    if let Some(mac) = self
                        .macros
                        .iter()
                        .find(|mac| self.str() == mac.name)
                        .copied()
                    {
                        self.macroinvoke(mac)?;
                        continue;
                    }
                    if seen_val {
                        return Err(self.err("expected operator"));
                    }
                    let string = self.str_intern();
                    let label = if let Some(index) = string.find('.') {
                        let (scope, string) = string.split_at(index);
                        if scope.is_empty() {
                            Label::new(self.scope, string)
                        } else {
                            Label::new(Some(scope), string)
                        }
                    } else {
                        Label::new(None, string)
                    };
                    self.expr_buffer.push(ExprNode::Label(label));
                    seen_val = true;
                    self.eat();
                    continue;
                }
                Tok::TAG => {
                    self.eat();
                    if self.peek()? != Tok::ID {
                        return Err(self.err("expected label"));
                    }
                    let string = self.str_intern();
                    let label = if let Some(index) = string.find('.') {
                        let (scope, string) = string.split_at(index);
                        if scope.is_empty() {
                            Label::new(self.scope, string)
                        } else {
                            Label::new(Some(scope), string)
                        }
                    } else {
                        Label::new(None, string)
                    };
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    if self.peek()? != Tok::STR {
                        return Err(self.err("expected tag name"));
                    }
                    let tag = self.str_intern();
                    self.expr_buffer.push(ExprNode::Tag(label, tag));
                    seen_val = true;
                    self.eat();
                    continue;
                }
                Tok::LEN => {
                    self.eat();
                    if self.peek()? != Tok::STR {
                        return Err(self.err("expected string"));
                    }
                    self.expr_buffer
                        .push(ExprNode::Const(self.str().len() as i32));
                    seen_val = true;
                    self.eat();
                    continue;
                }
                _ => {
                    if !seen_val {
                        return Err(self.err("expected value"));
                    }
                    if paren_depth != 0 {
                        return Err(self.err("unbalanced parens"));
                    }
                    break;
                }
            }
        }
        while let Some(top) = self.operator_buffer.pop() {
            self.expr_buffer.push(ExprNode::Op(top));
        }
        if let Some(value) = self.expr_eval(&self.expr_buffer) {
            Ok(Expr::Const(value))
        } else {
            Ok(Expr::List(self.expr_int.intern(&self.expr_buffer)))
        }
    }

    fn expr_eval(&self, expr: &[ExprNode<'_>]) -> Option<i32> {
        self.expr_branch_eval(expr, false)
    }

    fn expr_branch_eval(&self, expr: &[ExprNode<'_>], branch: bool) -> Option<i32> {
        let mut scratch = Vec::new();
        for node in expr.iter() {
            match *node {
                ExprNode::Const(value) => scratch.push(value),
                ExprNode::Label(label) => {
                    if let Some(sym) = self.syms.iter().find(|sym| &sym.label == &label) {
                        match sym.value {
                            Expr::Const(value) => scratch.push(value),
                            Expr::Addr(section, pc) => {
                                if branch && (self.sections[self.section].name == section) {
                                    scratch.push(pc as i32)
                                } else {
                                    // the linker has to handle this
                                    return None;
                                }
                            }
                            // expand the sub-expression recursively
                            Expr::List(expr) => {
                                scratch.push(self.expr_branch_eval(expr, branch)?);
                            }
                        }
                    } else {
                        return None; // needs to be solved later
                    }
                }
                ExprNode::Tag(_, _) => {
                    return None; // tags can only be solved at link-time
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

    fn expect(&mut self, tok: Tok) -> io::Result<()> {
        if self.peek()? != tok {
            return Err(self.err("unexpected garbage"));
        }
        self.eat();
        Ok(())
    }

    fn reloc(&mut self, offset: usize, width: u8, expr: Expr<'a>, pos: Pos<'a>, flags: u8) {
        let pc = self.pc() as usize;
        let offset = pc + offset;
        let value = match expr {
            Expr::Const(_) => unreachable!(),
            Expr::Addr(section, pc) => RelocVal::Addr(section, pc),
            Expr::List(expr) => RelocVal::List(expr),
        };
        let unit = self.str_int.intern("__STATIC__");
        self.sections[self.section].relocs.push(Reloc {
            offset,
            width,
            value,
            unit,
            pos,
            flags,
        });
    }

    fn find_opcode(&self, addrs: &[u8; 23], addr: Addr) -> Option<u8> {
        let op = addrs[addr.0 as usize];
        if op == ____ {
            return None;
        }
        Some(op)
    }

    fn check_opcode(&self, addrs: &[u8; 23], addr: Addr) -> io::Result<u8> {
        let op = self
            .find_opcode(addrs, addr)
            .ok_or_else(|| self.err("illegal address mode"))?;
        if self.emulation && NATIVE_OPCODES.contains(&op) {
            Err(self.err("instruction/address mode requires native mode"))
        } else {
            Ok(op)
        }
    }

    fn operand(&mut self, mne: (Mne, &[u8; 23])) -> io::Result<()> {
        match self.peek()? {
            Tok::NEWLINE => {
                let op = self.check_opcode(mne.1, Addr::IMP)?;
                if self.emit {
                    self.write(&[op]);
                }
                return self.add_pc(1);
            }
            Tok::HASH => {
                #[rustfmt::skip]
                let width = match mne.0 {
                    Mne::ADC | Mne::AND | Mne::BIT | Mne::CMP | Mne::EOR | Mne::ORA | Mne::LDA | Mne::SBC
                        => if self.accum8 { 1 } else { 2 },
                    Mne::CPX | Mne::CPY | Mne::LDX | Mne::LDY
                        => if self.index8 { 1 } else { 2 },
                    _ => 1,
                };
                let op = self.check_opcode(mne.1, Addr::IMM)?;
                self.eat();
                let pos = self.tok().pos();
                let expr = self.expr()?;
                if self.emit {
                    self.write(&[op]);
                    if width == 1 {
                        if let Ok(value) = self.const_expr(expr) {
                            self.write(&self.range_8(value)?.to_le_bytes());
                        } else {
                            self.write(&[0xFD]);
                            self.reloc(1, 1, expr, pos, 0);
                        }
                    } else {
                        if let Ok(value) = self.const_expr(expr) {
                            self.write(&self.range_16(value)?.to_le_bytes());
                        } else {
                            self.write(&[0xFD, 0xFD]);
                            self.reloc(1, 2, expr, pos, 0);
                        }
                    }
                }
                return self.add_pc(1 + width);
            }
            Tok::LPAREN => {
                self.eat();
                let abs = self.peek()? == Tok::PIPE;
                if abs {
                    self.eat();
                }
                let pos = self.tok().pos();
                let expr = self.expr()?;
                match self.peek()? {
                    // IDP, IDY, or IAB?
                    Tok::RPAREN => {
                        self.eat();
                        // IDY
                        if !abs {
                            if self.peek()? == Tok::COMMA {
                                let op = self.check_opcode(mne.1, Addr::IDY)?;
                                self.eat();
                                self.expect(Tok::Y)?;
                                if self.emit {
                                    self.write(&[op]);
                                    let value = self.const_expr(expr)?;
                                    self.write(&self.range_8(value)?.to_le_bytes());
                                }
                                return self.add_pc(2);
                            }
                            // IDP?
                            if let Ok(value) = self.const_expr(expr) {
                                if let Ok(byte) = self.range_8(value) {
                                    if let Ok(op) = self.check_opcode(mne.1, Addr::IDP) {
                                        if self.emit {
                                            self.write(&[op]);
                                            self.write(&byte.to_le_bytes());
                                        }
                                        return self.add_pc(2);
                                    }
                                }
                            }
                        }
                        // IAB
                        let op = self.check_opcode(mne.1, Addr::IAB)?;
                        if self.emit {
                            self.write(&[op]);
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_16(value)?.to_le_bytes());
                            } else {
                                self.write(&[0xFD, 0xFD]);
                                self.reloc(1, 2, expr, pos, 0);
                            }
                        }
                        return self.add_pc(3);
                    }
                    // IDX, ISY, or IAX?
                    _ => {
                        self.eat();
                        self.expect(Tok::COMMA)?;
                        if !abs {
                            // ISY?
                            if self.peek()? == Tok::S {
                                let op = self.check_opcode(mne.1, Addr::ISY)?;
                                self.eat();
                                self.expect(Tok::RPAREN)?;
                                self.expect(Tok::COMMA)?;
                                self.expect(Tok::Y)?;
                                if self.emit {
                                    self.write(&[op]);
                                    let value = self.const_expr(expr)?;
                                    self.write(&self.range_8(value)?.to_le_bytes());
                                }
                                return self.add_pc(2);
                            }
                            // IDX?
                            if let Ok(value) = self.const_expr(expr) {
                                if let Ok(byte) = self.range_8(value) {
                                    if let Ok(op) = self.check_opcode(mne.1, Addr::IDX) {
                                        if self.emit {
                                            self.write(&[op]);
                                            self.write(&byte.to_le_bytes());
                                        }
                                        return self.add_pc(2);
                                    }
                                }
                            }
                        }
                        // IAX
                        let op = self.check_opcode(mne.1, Addr::IAX)?;
                        if self.emit {
                            self.write(&[op]);
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_16(value)?.to_le_bytes());
                            } else {
                                self.write(&[0xFD, 0xFD]);
                                self.reloc(1, 2, expr, pos, 0);
                            }
                        }
                        return self.add_pc(3);
                    }
                }
            }
            // IDL, ILY, or IAL?
            Tok::LBRACKET => {
                self.eat();
                let abs = self.peek()? == Tok::PIPE;
                if abs {
                    self.eat();
                }
                let pos = self.tok().pos();
                let expr = self.expr()?;
                self.expect(Tok::RBRACKET)?;
                if !abs {
                    // ILY?
                    if self.peek()? == Tok::COMMA {
                        let op = self.check_opcode(mne.1, Addr::ILY)?;
                        self.eat();
                        self.expect(Tok::Y)?;
                        if self.emit {
                            self.write(&[op]);
                            let value = self.const_expr(expr)?;
                            self.write(&self.range_8(value)?.to_le_bytes());
                        }
                        return self.add_pc(2);
                    }
                    // IDL?
                    if let Ok(value) = self.const_expr(expr) {
                        if let Ok(byte) = self.range_8(value) {
                            if let Ok(op) = self.check_opcode(mne.1, Addr::IDL) {
                                if self.emit {
                                    self.write(&[op]);
                                    self.write(&byte.to_le_bytes());
                                }
                                return self.add_pc(2);
                            }
                        }
                    }
                }
                // IAL
                let op = self.check_opcode(mne.1, Addr::IAL)?;
                if self.emit {
                    self.write(&[op]);
                    if let Ok(value) = self.const_expr(expr) {
                        self.write(&self.range_16(value)?.to_le_bytes());
                    } else {
                        self.write(&[0xFD, 0xFD]);
                        self.reloc(1, 2, expr, pos, 0);
                    }
                }
                return self.add_pc(3);
            }
            _ => {
                // BM
                if matches!(mne.0, Mne::MVN | Mne::MVP) {
                    let op = self.check_opcode(mne.1, Addr::BM)?;
                    let src_pos = self.tok().pos();
                    let src = self.expr()?;
                    let dst_pos = self.tok().pos();
                    let dst = self.expr()?;
                    if self.emit {
                        self.write(&[op]);
                        if let Ok(value) = self.const_expr(src) {
                            self.write(&self.range_8(value)?.to_le_bytes());
                        } else {
                            self.write(&[0xFD]);
                            self.reloc(1, 1, src, src_pos, 0);
                        }
                        if let Ok(value) = self.const_expr(dst) {
                            self.write(&self.range_8(value)?.to_le_bytes());
                        } else {
                            self.write(&[0xFD]);
                            self.reloc(2, 1, dst, dst_pos, 0);
                        }
                    }
                    return self.add_pc(3);
                }
                // RLL
                if matches!(mne.0, Mne::BRL | Mne::PER) {
                    let expr = self.expr()?;
                    let op = self.check_opcode(mne.1, Addr::RLL)?;
                    if self.emit {
                        self.write(&[op]);
                        let value = self.const_branch_expr(expr)?;
                        self.write(&self.range_16(value)?.to_le_bytes());
                    }
                    return self.add_pc(3);
                }
                // REL
                #[rustfmt::skip]
                if matches!(mne.0, Mne::BCC | Mne::BCS | Mne::BNE | Mne::BEQ | Mne::BPL | Mne::BMI | Mne::BRA | Mne::BVC | Mne::BVS) {
                    let expr = self.expr()?;
                    let op = self.check_opcode(mne.1, Addr::REL)?;
                    if self.emit {
                        self.write(&[op]);
                        let value = self.const_branch_expr(expr)?;
                        self.write(&self.range_8(value)?.to_le_bytes());
                    }
                    return self.add_pc(2);
                }
                let length = match self.peek()? {
                    Tok::PIPE => {
                        self.eat();
                        Length::ABS
                    }
                    Tok::ASP => {
                        self.eat();
                        Length::ABL
                    }
                    _ => Length::None,
                };
                let pos = self.tok().pos();
                let expr = self.expr()?;
                match length {
                    Length::None => {
                        if let Ok(value) = self.const_expr(expr) {
                            // SR, DP, DPX, or DPY?
                            if let Ok(byte) = self.range_8(value) {
                                // SR, DPX, DPY?
                                if self.peek()? == Tok::COMMA {
                                    self.eat();
                                    match self.peek()? {
                                        // SR
                                        Tok::S => {
                                            self.eat();
                                            let op = self.check_opcode(mne.1, Addr::SR)?;
                                            if self.emit {
                                                self.write(&[op]);
                                                self.write(&byte.to_le_bytes());
                                            }
                                            return self.add_pc(2);
                                        }
                                        Tok::X => {
                                            self.eat();
                                            // DPX
                                            if let Ok(op) = self.check_opcode(mne.1, Addr::DPX) {
                                                if self.emit {
                                                    self.write(&[op]);
                                                    self.write(&byte.to_le_bytes());
                                                }
                                                return self.add_pc(2);
                                            }
                                            // promote: ABX
                                            if let Ok(op) = self.check_opcode(mne.1, Addr::ABX) {
                                                if self.emit {
                                                    self.write(&[op]);
                                                    self.write(&[byte, 0x00]);
                                                }
                                                return self.add_pc(3);
                                            }
                                            // promote: ALX
                                            let op = self.check_opcode(mne.1, Addr::ALX)?;
                                            if self.emit {
                                                self.write(&[op]);
                                                self.write(&[byte, 0x00, 0x00]);
                                            }
                                            return self.add_pc(4);
                                        }
                                        _ => {
                                            self.expect(Tok::Y)?;
                                            // DPY
                                            if let Ok(op) = self.check_opcode(mne.1, Addr::DPY) {
                                                if self.emit {
                                                    self.write(&[op]);
                                                    self.write(&byte.to_le_bytes());
                                                }
                                                return self.add_pc(2);
                                            }
                                            // promote: ABY
                                            let op = self.check_opcode(mne.1, Addr::ABY)?;
                                            if self.emit {
                                                self.write(&[op]);
                                                self.write(&[byte, 0x00]);
                                            }
                                            return self.add_pc(3);
                                        }
                                    }
                                } else {
                                    // DP
                                    if let Ok(op) = self.check_opcode(mne.1, Addr::DP) {
                                        if self.emit {
                                            self.write(&[op]);
                                            self.write(&byte.to_le_bytes());
                                        }
                                        return self.add_pc(2);
                                    }
                                    // promote: ABS
                                    if let Ok(op) = self.check_opcode(mne.1, Addr::ABS) {
                                        if self.emit {
                                            self.write(&[op]);
                                            self.write(&[byte, 0x00]);
                                        }
                                        return self.add_pc(3);
                                    }
                                    // promote: ABL
                                    let op = self.check_opcode(mne.1, Addr::ABL)?;
                                    if self.emit {
                                        self.write(&[op]);
                                        self.write(&[byte, 0x00, 0x00]);
                                    }
                                    return self.add_pc(4);
                                }
                            }
                            // ABS, ABX, or ABY?
                            if let Ok(value) = self.range_16(value) {
                                // ABX or ABY?
                                if self.peek()? == Tok::COMMA {
                                    self.eat();
                                    if self.peek()? == Tok::X {
                                        self.eat();
                                        // ABX
                                        if let Ok(op) = self.check_opcode(mne.1, Addr::ABX) {
                                            if self.emit {
                                                self.write(&[op]);
                                                self.write(&value.to_le_bytes());
                                            }
                                            return self.add_pc(3);
                                        }
                                        // promote: ALX
                                        let op = self.check_opcode(mne.1, Addr::ALX)?;
                                        if self.emit {
                                            self.write(&[op]);
                                            self.write(&value.to_le_bytes());
                                            self.write(&[0x00]);
                                        }
                                        return self.add_pc(4);
                                    }
                                    // ABY
                                    self.expect(Tok::Y)?;
                                    let op = self.check_opcode(mne.1, Addr::ABY)?;
                                    if self.emit {
                                        self.write(&[op]);
                                        self.write(&value.to_le_bytes());
                                    }
                                    return self.add_pc(3);
                                }
                                // ABS
                                if let Ok(op) = self.check_opcode(mne.1, Addr::ABS) {
                                    if self.emit {
                                        self.write(&[op]);
                                        self.write(&value.to_le_bytes());
                                    }
                                    return self.add_pc(3);
                                }
                                // promote: ABL
                                let op = self.check_opcode(mne.1, Addr::ABL)?;
                                if self.emit {
                                    self.write(&[op]);
                                    self.write(&value.to_le_bytes());
                                    self.write(&[0x00]);
                                }
                                return self.add_pc(4);
                            }
                            let value = self.range_24(value)?;
                            // ALX
                            if self.peek()? == Tok::COMMA {
                                self.eat();
                                self.expect(Tok::X)?;
                                let op = self.check_opcode(mne.1, Addr::ALX)?;
                                if self.emit {
                                    self.write(&[op]);
                                    self.write(&value.to_le_bytes());
                                    self.write(&[0x00]);
                                }
                                return self.add_pc(4);
                            }
                            // ABL
                            let op = self.check_opcode(mne.1, Addr::ABL)?;
                            if self.emit {
                                self.write(&[op]);
                                self.write(&value.to_le_bytes());
                                self.write(&[0x00]);
                            }
                            return self.add_pc(4);
                        }
                        // ABX or ABY
                        if self.peek()? == Tok::COMMA {
                            self.eat();
                            // ABX
                            if self.peek()? == Tok::X {
                                self.eat();
                                let op = self.check_opcode(mne.1, Addr::ABX)?;
                                if self.emit {
                                    self.write(&[op]);
                                    self.write(&[0xFD, 0xFD]);
                                    self.reloc(1, 2, expr, pos, 0);
                                }
                                return self.add_pc(3);
                            }
                            // ABY
                            let op = self.check_opcode(mne.1, Addr::ABY)?;
                            if self.emit {
                                self.write(&[op]);
                                self.write(&[0xFD, 0xFD]);
                                self.reloc(1, 2, expr, pos, 0);
                            }
                            return self.add_pc(3);
                        }
                        // ABS
                        let op = self.check_opcode(mne.1, Addr::ABS)?;
                        if self.emit {
                            // At link time, we want to allow JMPs within the
                            // same program bank.
                            let flags = if matches!(mne.0, Mne::JMP | Mne::JSR) {
                                RelocFlags::ABS_JMP
                            } else {
                                0
                            };
                            self.write(&[op]);
                            self.write(&[0xFD, 0xFD]);
                            self.reloc(1, 2, expr, pos, flags);
                        }
                        return self.add_pc(3);
                    }
                    Length::ABS => {
                        // ABX or ABY
                        if self.peek()? == Tok::COMMA {
                            self.eat();
                            // ABX
                            if self.peek()? == Tok::X {
                                self.eat();
                                let op = self.check_opcode(mne.1, Addr::ABX)?;
                                if self.emit {
                                    self.write(&[op]);
                                    if let Ok(value) = self.const_expr(expr) {
                                        self.write(&self.range_16(value)?.to_le_bytes());
                                    } else {
                                        self.write(&[0xFD, 0xFD]);
                                        self.reloc(1, 2, expr, pos, 0);
                                    }
                                }
                                return self.add_pc(3);
                            }
                            // ABY
                            let op = self.check_opcode(mne.1, Addr::ABY)?;
                            if self.emit {
                                self.write(&[op]);
                                if let Ok(value) = self.const_expr(expr) {
                                    self.write(&self.range_16(value)?.to_le_bytes());
                                } else {
                                    self.write(&[0xFD, 0xFD]);
                                    self.reloc(1, 2, expr, pos, 0);
                                }
                            }
                            return self.add_pc(3);
                        }
                        // ABS
                        let op = self.check_opcode(mne.1, Addr::ABS)?;
                        if self.emit {
                            self.write(&[op]);
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_16(value)?.to_le_bytes());
                            } else {
                                // At link time, we want to allow JMPs within the
                                // same program bank.
                                let flags = if matches!(mne.0, Mne::JMP | Mne::JSR) {
                                    RelocFlags::ABS_JMP
                                } else {
                                    0
                                };
                                self.write(&[0xFD, 0xFD]);
                                self.reloc(1, 2, expr, pos, flags);
                            }
                        }
                        return self.add_pc(3);
                    }
                    Length::ABL => {
                        // ALX
                        if self.peek()? == Tok::COMMA {
                            self.eat();
                            self.expect(Tok::X)?;
                            let op = self.check_opcode(mne.1, Addr::ALX)?;
                            if self.emit {
                                self.write(&[op]);
                                if let Ok(value) = self.const_expr(expr) {
                                    self.write(&self.range_24(value)?.to_le_bytes());
                                } else {
                                    self.write(&[0xFD, 0xFD, 0xFD]);
                                    self.reloc(1, 3, expr, pos, 0);
                                }
                            }
                            return self.add_pc(4);
                        }
                        // ABL
                        let op = self.check_opcode(mne.1, Addr::ABL)?;
                        if self.emit {
                            self.write(&[op]);
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_24(value)?.to_le_bytes());
                            } else {
                                self.write(&[0xFD, 0xFD, 0xFd]);
                                self.reloc(1, 3, expr, pos, 0);
                            }
                        }
                        return self.add_pc(4);
                    }
                }
            }
        }
    }

    fn directive(&mut self) -> io::Result<()> {
        match self.peek()? {
            Tok::BYTE => {
                self.eat();
                loop {
                    if self.peek()? == Tok::STR {
                        if self.emit {
                            self.write_str();
                        }
                        self.add_pc(self.str().len() as u32)?;
                        self.eat();
                    } else {
                        let pos = self.tok().pos();
                        let expr = self.expr()?;
                        if self.emit {
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_8(value)?.to_le_bytes());
                            } else {
                                self.write(&[0x0FD]);
                                self.reloc(0, 1, expr, pos, 0);
                            }
                        }
                        self.add_pc(1)?;
                    }
                    if self.peek()? != Tok::COMMA {
                        break;
                    }
                    self.eat();
                }
                self.eol()?;
            }
            Tok::WORD => {
                self.eat();
                loop {
                    let pos = self.tok().pos();
                    let expr = self.expr()?;
                    if self.emit {
                        if let Ok(value) = self.const_expr(expr) {
                            self.write(&self.range_16(value)?.to_le_bytes());
                        } else {
                            self.write(&[0xFD, 0xFD]);
                            self.reloc(0, 2, expr, pos, 0);
                        }
                    }
                    self.add_pc(2)?;
                    if self.peek()? != Tok::COMMA {
                        break;
                    }
                    self.eat();
                }
                self.eol()?;
            }
            Tok::LONG => {
                self.eat();
                loop {
                    let pos = self.tok().pos();
                    let expr = self.expr()?;
                    if self.emit {
                        if let Ok(value) = self.const_expr(expr) {
                            self.write(&self.range_24(value)?.to_le_bytes());
                        } else {
                            self.write(&[0xFD, 0xFD, 0xFD]);
                            self.reloc(0, 3, expr, pos, 0);
                        }
                    }
                    self.add_pc(3)?;
                    if self.peek()? != Tok::COMMA {
                        break;
                    }
                    self.eat();
                }
                self.eol()?;
            }
            Tok::SECTION => {
                self.eat();
                if self.peek()? != Tok::STR {
                    return Err(self.err("expected section name"));
                }
                let name = self.str_intern();
                self.eat();
                let index = if let Some(sec) = self
                    .sections
                    .iter()
                    .enumerate()
                    .find(|sec| &sec.1.name == &name)
                {
                    sec.0
                } else {
                    // save in the section table with default values
                    let index = self.sections.len();
                    self.sections.push(Section::new(name));
                    index
                };
                self.section = index;
                self.eol()?;
            }
            Tok::INCLUDE => {
                self.eat();
                if self.peek()? != Tok::STR {
                    return Err(self.err("expected file name"));
                }
                let name = self.str();
                let name = if let Ok(name) = fs::canonicalize(name) {
                    name
                } else {
                    let mut base = PathBuf::from_str(self.tok().pos().file).unwrap();
                    base.pop();
                    base.push(name);
                    // try current directory
                    if let Ok(name) = fs::canonicalize(name) {
                        name
                    } else if let Some(name) = self.includes.iter().find_map(|path| {
                        // try every include directory
                        let mut base = path.clone();
                        base.push(name);
                        fs::canonicalize(base).ok()
                    }) {
                        name
                    } else {
                        return Err(self.err("file not found"));
                    }
                };
                self.included.insert(name.clone());
                let name = self.str_int.intern(name.to_str().unwrap());
                let file = File::open(name)?;
                self.eat();
                let lexer = Lexer::new(file, name);
                self.toks.push(Box::new(lexer));
            }
            tok @ (Tok::IF | Tok::IFDEF | Tok::IFNDEF) => {
                self.ifdirective(tok)?;
            }
            Tok::END => {
                if self.if_level == 0 {
                    return Err(self.err("unexpected end"));
                }
                self.eat();
                self.if_level -= 1;
            }
            Tok::RES => {
                self.eat();
                let expr = self.expr()?;
                let expr = self.const_expr(expr)?;
                let res = self.range_24(expr)?;
                // reserve space by allocating literal bytes
                // if we dont do this, our current linker will
                // overlap the same section across objects as pc
                // does not allocate space on its own
                if self.emit {
                    for _ in 0..res {
                        self.write(&[0x00]);
                    }
                }
                self.add_pc(res)?;
                self.eol()?;
            }
            Tok::MACRO => {
                self.macrodef()?;
            }
            Tok::LOOP => {
                self.forloop()?;
            }
            Tok::STRUCT => {
                self.structdef()?;
            }
            Tok::FAIL => {
                self.eat();
                if self.peek()? != Tok::STR {
                    return Err(self.err("expected message"));
                }
                return Err(self.err(self.str()));
            }
            Tok::INDEX8 => {
                self.eat();
                self.index8 = true;
            }
            Tok::INDEX16 => {
                self.eat();
                self.index8 = false;
            }
            Tok::ACCUM8 => {
                self.eat();
                self.accum8 = true;
            }
            Tok::ACCUM16 => {
                self.eat();
                self.accum8 = false;
            }
            Tok::EMULATION => {
                self.eat();
                self.emulation = true;
            }
            Tok::NATIVE => {
                self.eat();
                self.emulation = false;
            }
            _ => return Err(self.err("expected directive")),
        }
        Ok(())
    }

    fn macroinvoke(&mut self, mac: Macro<'a>) -> io::Result<()> {
        let pos = self.tok().pos();
        self.eat();
        let mut args = VecDeque::new();
        let mut arg = Vec::new();
        loop {
            // TODO: pos per tok
            let pos = self.tok().pos();
            match self.peek()? {
                tok @ (Tok::BREAK | Tok::NEWLINE | Tok::EOF) => {
                    if !arg.is_empty() {
                        let arg = self.tok_int.intern(&arg);
                        args.push_back(arg);
                    }
                    if tok == Tok::BREAK {
                        self.eat();
                    }
                    break;
                }
                Tok::ID => arg.push((pos, MacroTok::Id(self.str_intern()))),
                Tok::STR => arg.push((pos, MacroTok::Str(self.str_intern()))),
                Tok::NUM => arg.push((pos, MacroTok::Num(self.tok().num()))),
                tok => arg.push((pos, MacroTok::Tok(tok))),
            }
            self.eat();
            if self.peek()? == Tok::COMMA {
                self.eat();
                let iarg = self.tok_int.intern(&arg);
                args.push_back(iarg);
                arg.clear();
            }
        }
        self.unique += 1;
        self.toks.push(Box::new(MacroInvocation {
            inner: mac,
            unique: self.unique,
            index: 0,
            join_buf: String::new(),
            args,
            arg_index: 0,
            pos,
        }));
        Ok(())
    }

    fn ifdirective(&mut self, tok: Tok) -> io::Result<()> {
        self.eat();
        let expr = self.expr()?;
        let skip = match tok {
            Tok::IF => self.const_expr(expr)? == 0,
            Tok::IFDEF => !matches!(expr, Expr::Const(_)),
            Tok::IFNDEF => matches!(expr, Expr::Const(_)),
            _ => unreachable!(),
        };
        if skip {
            let mut if_level = 0;
            loop {
                match self.peek()? {
                    Tok::IF | Tok::IFDEF | Tok::STRUCT | Tok::MACRO | Tok::LOOP => if_level += 1,
                    Tok::END => {
                        if if_level == 0 {
                            self.eat();
                            break;
                        }
                        if_level -= 1;
                    }
                    _ => {}
                }
                self.eat();
            }
        }
        self.if_level += 1;
        Ok(())
    }

    fn structdef(&mut self) -> io::Result<()> {
        self.eat();
        if self.peek()? != Tok::ID {
            return Err(self.err("expected struct name"));
        }
        let string = self.str_intern();
        if string.starts_with(".") {
            return Err(self.err("struct must be global"));
        }
        let pos = self.tok().pos();
        self.eat();
        let label = Label::new(Some(string), self.str_int.intern(".SIZE"));
        // TODO: check if struct already defined (similar to macro)
        let mut size = 0;
        let unit = self.str_int.intern("__STATIC__");
        let section = self.sections[self.section].name;
        loop {
            match self.peek()? {
                Tok::NEWLINE => {
                    self.eat();
                    continue;
                }
                Tok::END => {
                    self.eat();
                    break;
                }
                tok @ (Tok::IF | Tok::IFDEF | Tok::IFNDEF) => {
                    self.ifdirective(tok)?;
                    continue;
                }
                _ => {}
            }
            if self.peek()? != Tok::ID {
                return Err(self.err("expected field name"));
            }
            {
                if self.str_like(".SIZE") {
                    return Err(self.err(".SIZE is a reserved field name"));
                }
                // is this a macro?
                if let Some(mac) = self
                    .macros
                    .iter()
                    .find(|mac| self.str() == mac.name)
                    .copied()
                {
                    self.macroinvoke(mac)?;
                    continue;
                }
                let field = self.str_intern();
                if !field.starts_with(".") {
                    return Err(self.err("field must have a local label"));
                }
                let pos = self.tok().pos();
                self.eat();
                let label = Label::new(Some(string), field);
                let expr = self.expr()?;
                let expr = self.const_expr(expr)?;
                if !self.emit {
                    self.syms.push(Sym {
                        label,
                        value: Expr::Const(size),
                        unit,
                        section,
                        pos,
                        flags: 0,
                    });
                }
                size += expr;
            }
            self.eol()?;
        }
        if !self.emit {
            self.syms.push(Sym {
                label,
                value: Expr::Const(size),
                unit,
                section,
                pos,
                flags: 0,
            });
        }
        Ok(())
    }

    fn macrodef(&mut self) -> io::Result<()> {
        self.eat();
        if self.peek()? != Tok::ID {
            return Err(self.err("expected macro name"));
        }
        let string = self.str_intern();
        if string.starts_with(".") {
            return Err(self.err("macro must be global"));
        }
        self.eat();
        let label = Label::new(None, string);
        // TODO: check if macro is already defined
        // if we are in the emit pass then its safe to skip
        self.eol()?;
        let mut toks = Vec::new();
        let mut if_level = 0;
        loop {
            match self.peek()? {
                Tok::IF | Tok::IFDEF | Tok::STRUCT | Tok::MACRO | Tok::LOOP => if_level += 1,
                Tok::END => {
                    if if_level == 0 {
                        self.eat();
                        break;
                    }
                    if_level -= 1;
                }
                _ => {}
            }
            // TODO: pos per tok
            let pos = self.tok().pos();
            match self.peek()? {
                Tok::EOF => return Err(self.err("unexpected end of file")),
                Tok::ID => toks.push((pos, MacroTok::Id(self.str_intern()))),
                Tok::STR => toks.push((pos, MacroTok::Str(self.str_intern()))),
                Tok::NUM => toks.push((pos, MacroTok::Num(self.tok().num()))),
                Tok::ARG => toks.push((pos, MacroTok::Arg((self.tok().num() as usize) - 1))),
                Tok::NARG => toks.push((pos, MacroTok::Narg)),
                Tok::UNIQ => toks.push((pos, MacroTok::Uniq)),
                Tok::SHIFT => toks.push((pos, MacroTok::Shift)),
                Tok::JOIN => {
                    self.eat();
                    let mut jtoks = Vec::new();
                    loop {
                        let pos = self.tok().pos();
                        match self.peek()? {
                            Tok::ID => jtoks.push((pos, MacroTok::Id(self.str_intern()))),
                            Tok::STR => jtoks.push((pos, MacroTok::Str(self.str_intern()))),
                            Tok::NUM => jtoks.push((pos, MacroTok::Num(self.tok().num()))),
                            Tok::ARG => {
                                jtoks.push((pos, MacroTok::Arg((self.tok().num() as usize) - 1)))
                            }
                            Tok::UNIQ => jtoks.push((pos, MacroTok::Uniq)),
                            _ => return Err(self.err("invalid \\JOIN input")),
                        }
                        self.eat();
                        if self.peek()? == Tok::BREAK {
                            self.eat();
                            break;
                        }
                        if self.peek()? != Tok::COMMA {
                            break;
                        }
                        self.eat();
                    }
                    let jtoks = self.tok_int.intern(&jtoks);
                    if jtoks.is_empty() {
                        return Err(self.err("\\JOIN needs inputs"));
                    }
                    toks.push((pos, MacroTok::Join(jtoks)));
                    continue;
                }
                tok => toks.push((pos, MacroTok::Tok(tok))),
            }
            self.eat();
        }
        let toks = self.tok_int.intern(&toks);
        self.macros.push(Macro {
            name: label.string,
            toks,
        });
        Ok(())
    }

    fn forloop(&mut self) -> io::Result<()> {
        self.eat();
        if self.peek()? != Tok::ID {
            return Err(self.err("expected variable name"));
        }
        let string = self.str_intern();
        if string.starts_with(".") {
            return Err(self.err("variable must be global"));
        }
        self.eat();
        self.expect(Tok::COMMA)?;
        let end = self.expr()?;
        let end = self.const_expr(end)?;
        let mut toks = Vec::new();
        let mut if_level = 0;
        loop {
            match self.peek()? {
                Tok::IF | Tok::IFDEF | Tok::STRUCT | Tok::MACRO | Tok::LOOP => if_level += 1,
                Tok::END => {
                    if if_level == 0 {
                        self.eat();
                        break;
                    }
                    if_level -= 1;
                }
                _ => {}
            }
            // TODO: a lot of these position calcs are probs wrong because we need to check pos
            //   *after* skipping whitespace, meaning we need to peek first.
            let pos = self.tok().pos();
            match self.peek()? {
                Tok::EOF => return Err(self.err("unexpected end of file")),
                Tok::ID if self.str_like(&string) => toks.push((pos, LoopTok::Iter)),
                Tok::ID => toks.push((pos, LoopTok::Id(self.str_intern()))),
                Tok::STR => toks.push((pos, LoopTok::Str(self.str_intern()))),
                Tok::NUM => toks.push((pos, LoopTok::Num(self.tok().num()))),
                Tok::JOIN => {
                    self.eat();
                    let mut jtoks = Vec::new();
                    loop {
                        let pos = self.tok().pos();
                        match self.peek()? {
                            Tok::ID if self.str_like(&string) => jtoks.push((pos, LoopTok::Iter)),
                            Tok::ID => jtoks.push((pos, LoopTok::Id(self.str_intern()))),
                            Tok::STR => jtoks.push((pos, LoopTok::Str(self.str_intern()))),
                            Tok::NUM => jtoks.push((pos, LoopTok::Num(self.tok().num()))),
                            _ => return Err(self.err("invalid \\JOIN input")),
                        }
                        self.eat();
                        if self.peek()? == Tok::BREAK {
                            self.eat();
                            break;
                        }
                        if self.peek()? != Tok::COMMA {
                            break;
                        }
                        self.eat();
                    }
                    let jtoks = self.loop_int.intern(&jtoks);
                    if jtoks.is_empty() {
                        return Err(self.err("\\JOIN needs inputs"));
                    }
                    toks.push((pos, LoopTok::Join(jtoks)));
                    continue;
                }
                tok => toks.push((pos, LoopTok::Tok(tok))),
            }
            self.eat();
        }
        let toks = self.loop_int.intern(&toks);
        self.toks.push(Box::new(Loop {
            toks,
            index: 0,
            join_buf: String::new(),
            iter: 0,
            end: end as usize,
        }));
        Ok(())
    }
}

#[derive(PartialEq, Eq)]
enum Length {
    None,
    ABS,
    ABL,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Addr(u8);

#[rustfmt::skip]
impl Addr {
    const IMP: Self = Self(0);  //
    const IMM: Self = Self(1);  // #$00
    const SR: Self = Self(2);   // $00,S
    const DP: Self = Self(3);   // $00
    const DPX: Self = Self(4);  // $00,X
    const DPY: Self = Self(5);  // $00,Y
    const IDP: Self = Self(6);  // ($00)
    const IDX: Self = Self(7);  // ($00,X)
    const IDY: Self = Self(8);  // ($00),Y
    const IDL: Self = Self(9);  // [$00]
    const ILY: Self = Self(10); // [$00],Y
    const ISY: Self = Self(11); // ($00,S),Y
    const ABS: Self = Self(12); // |$0000
    const ABX: Self = Self(13); // |$0000,X
    const ABY: Self = Self(14); // |$0000,Y
    const ABL: Self = Self(15); // @$000000
    const ALX: Self = Self(16); // @$000000,X
    const IAB: Self = Self(17); // (|$0000)
    const IAX: Self = Self(18); // (|$0000,X)
    const IAL: Self = Self(19); // [|$0000]
    const REL: Self = Self(20); // ±$00
    const RLL: Self = Self(21); // ±$0000
    const BM: Self = Self(22);  // $00,$00
}

const ____: u8 = 0x42; // $42 (WDC) is reserved so we special-case it as a blank

#[rustfmt::skip]
const MNEMONICS: &[(Mne, &[u8; 23])] = &[
    //           imp   imm   sr    dp    dpx   dpy   idp   idx   idy   idl   ily   isy   abs   abx   aby   abl   alx   iab   iax   ial   rel   rll   bm
    (Mne::ADC, &[____, 0x69, 0x63, 0x65, 0x75, ____, 0x72, 0x61, 0x71, 0x67, 0x77, 0x73, 0x6D, 0x7D, 0x79, 0x6F, 0x7F, ____, ____, ____, ____, ____, ____]),
    (Mne::AND, &[____, 0x29, 0x23, 0x25, 0x35, ____, 0x32, 0x21, 0x31, 0x27, 0x37, 0x33, 0x2D, 0x3D, 0x39, 0x2F, 0x3F, ____, ____, ____, ____, ____, ____]),
    (Mne::ASL, &[0x0A, ____, ____, 0x06, 0x16, ____, ____, ____, ____, ____, ____, ____, 0x0E, 0x1E, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::BCC, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x90, ____, ____]),
    (Mne::BCS, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0xB0, ____, ____]),
    (Mne::BNE, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0xD0, ____, ____]),
    (Mne::BEQ, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0xF0, ____, ____]),
    (Mne::BIT, &[____, 0x89, ____, 0x24, 0x34, ____, ____, ____, ____, ____, ____, ____, 0x2C, 0x3C, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::BPL, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x10, ____, ____]),
    (Mne::BMI, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x30, ____, ____]),
    (Mne::BRA, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x80, ____, ____]),
    (Mne::BRK, &[____, 0x00, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::BRL, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x82, ____]),
    (Mne::BVC, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x50, ____, ____]),
    (Mne::BVS, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x70, ____, ____]),
    (Mne::CLC, &[0x18, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::CLD, &[0xD8, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::CLI, &[0x58, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::CLV, &[0xB8, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::CMP, &[____, 0xC9, 0xC3, 0xC5, 0xD5, ____, 0xD2, 0xC1, 0xD1, 0xC7, 0xD7, 0xD3, 0xCD, 0xDD, 0xD9, 0xCF, 0xDF, ____, ____, ____, ____, ____, ____]),
    (Mne::COP, &[____, 0x02, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::CPX, &[____, 0xE0, ____, 0xE4, ____, ____, ____, ____, ____, ____, ____, ____, 0xEC, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::CPY, &[____, 0xC0, ____, 0xC4, ____, ____, ____, ____, ____, ____, ____, ____, 0xCC, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    //           imp   imm   sr    dp    dpx   dpy   idp   idx   idy   idl   ily   isy   abs   abx   aby   abl   alx   iab   iax   ial   rel   rll   bm
    (Mne::DEC, &[0x3A, ____, ____, 0xC6, 0xD6, ____, ____, ____, ____, ____, ____, ____, 0xCE, 0xDE, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::DEX, &[0xCA, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::DEY, &[0x88, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::EOR, &[____, 0x49, 0x43, 0x45, 0x55, ____, 0x52, 0x41, 0x51, 0x47, 0x57, 0x53, 0x4D, 0x5D, 0x59, 0x4F, 0x5F, ____, ____, ____, ____, ____, ____]),
    (Mne::INC, &[0x1A, ____, ____, 0xE6, 0xF6, ____, ____, ____, ____, ____, ____, ____, 0xEE, 0xFE, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::INX, &[0xE8, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::INY, &[0xC8, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::JML, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x5C, ____, ____, ____, 0xDC, ____, ____, ____]),
    (Mne::JMP, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x4C, ____, ____, ____, ____, 0x6C, 0x7C, ____, ____, ____, ____]),
    (Mne::JSL, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x22, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::JSR, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x20, ____, ____, ____, ____, ____, 0xFC, ____, ____, ____, ____]),
    (Mne::LDA, &[____, 0xA9, 0xA3, 0xA5, 0xB5, ____, 0xB2, 0xA1, 0xB1, 0xA7, 0xB7, 0xB3, 0xAD, 0xBD, 0xB9, 0xAF, 0xBF, ____, ____, ____, ____, ____, ____]),
    (Mne::LDX, &[____, 0xA2, ____, 0xA6, ____, 0xB6, ____, ____, ____, ____, ____, ____, 0xAE, ____, 0xBE, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::LDY, &[____, 0xA0, ____, 0xA4, 0xB4, ____, ____, ____, ____, ____, ____, ____, 0xAC, 0xBC, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::LSR, &[0x4A, ____, ____, 0x46, 0x56, ____, ____, ____, ____, ____, ____, ____, 0x4E, 0x5E, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::MVN, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x54]),
    (Mne::MVP, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x44]),
    (Mne::NOP, &[0xEA, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::ORA, &[____, 0x09, 0x03, 0x05, 0x15, ____, 0x12, 0x01, 0x11, 0x07, 0x17, 0x13, 0x0D, 0x1D, 0x19, 0x0F, 0x1F, ____, ____, ____, ____, ____, ____]),
    (Mne::PEA, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0xF4, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PEI, &[____, ____, ____, 0xD4, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PER, &[____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, 0x62, ____]),
    //           imp   imm   sr    dp    dpx   dpy   idp   idx   idy   idl   ily   isy   abs   abx   aby   abl   alx   iab   iax   ial   rel   rll   bm
    (Mne::PHA, &[0x48, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PHB, &[0x8B, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PHD, &[0x0B, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PHK, &[0x4B, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PHP, &[0x08, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PHX, &[0xDA, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PHY, &[0x5A, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PLA, &[0x68, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PLB, &[0xAB, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PLD, &[0x2B, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PLP, &[0x28, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PLX, &[0xFA, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::PLY, &[0x7A, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::REP, &[____, 0xC2, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::ROL, &[0x2A, ____, ____, 0x26, 0x36, ____, ____, ____, ____, ____, ____, ____, 0x2E, 0x3E, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::ROR, &[0x6A, ____, ____, 0x66, 0x76, ____, ____, ____, ____, ____, ____, ____, 0x6E, 0x7E, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::RTI, &[0x40, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::RTL, &[0x6B, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::RTS, &[0x60, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::SBC, &[____, 0xE9, 0xE3, 0xE5, 0xF5, ____, 0xF2, 0xE1, 0xF1, 0xE7, 0xF7, 0xF3, 0xED, 0xFD, 0xF9, 0xEF, 0xFF, ____, ____, ____, ____, ____, ____]),
    (Mne::SEC, &[0x38, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::SED, &[0xF8, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::SEI, &[0x78, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    //           imp   imm   sr    dp    dpx   dpy   idp   idx   idy   idl   ily   isy   abs   abx   aby   abl   alx   iab   iax   ial   rel   rll   bm
    (Mne::SEP, &[____, 0xE2, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::STA, &[____, ____, 0x83, 0x85, 0x95, ____, 0x92, 0x81, 0x91, 0x87, 0x97, 0x93, 0x8D, 0x9D, 0x99, 0x8F, 0xBF, ____, ____, ____, ____, ____, ____]),
    (Mne::STP, &[0xDB, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::STX, &[____, ____, ____, 0x86, ____, 0x96, ____, ____, ____, ____, ____, ____, 0x8E, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::STY, &[____, ____, ____, 0x84, 0x94, ____, ____, ____, ____, ____, ____, ____, 0x8C, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::STZ, &[____, ____, ____, 0x64, 0x74, ____, ____, ____, ____, ____, ____, ____, 0x9C, 0x9E, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TAX, &[0xAA, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TAY, &[0xA8, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TCD, &[0x5B, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TCS, &[0x1B, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TDC, &[0x7B, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TRB, &[____, ____, ____, 0x14, ____, ____, ____, ____, ____, ____, ____, ____, 0x1C, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TSB, &[____, ____, ____, 0x04, ____, ____, ____, ____, ____, ____, ____, ____, 0x0C, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TSC, &[0x3B, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TSX, &[0xBA, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TXA, &[0x8A, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TXS, &[0x9A, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TXY, &[0x9B, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TYA, &[0x98, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::TYX, &[0xBB, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::WAI, &[0xCB, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::WDC, &[____, 0x42, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::XBA, &[0xEB, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
    (Mne::XCE, &[0xFB, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____, ____]),
];

#[rustfmt::skip]
const NATIVE_OPCODES: &[u8] = &[
    0x02, 0x03, 0x07, 0x0B, 0x0F, 0x13, 0x17, 0x1B, 0x1F, 0x22, 0x23, 0x27, 0x2B, 0x2F,
    0x33, 0x37, 0x3B, 0x3F, 0x24, 0x43, 0x44, 0x47, 0x4B, 0x4F, 0x53, 0x54, 0x57, 0x5B, 0x5C, 0x5F,
    0x62, 0x63, 0x67, 0x6B, 0x6F, 0x73, 0x77, 0x7B, 0x7F, 0x82, 0x83, 0x87, 0x8B, 0x8F,
    0x93, 0x97, 0x9B, 0x9F, 0xA3, 0xA7, 0xAB, 0xAF, 0xB3, 0xB7, 0xBB, 0xBF,
    0xC2, 0xC3, 0xC7, 0xCF, 0xD3, 0xD4, 0xD7, 0xDC, 0xDF,
    0xE2, 0xE3, 0xE7, 0xEB, 0xEF, 0xF3, 0xF4, 0xF7, 0xFB, 0xFC, 0xFF
];

#[derive(Clone, Copy, PartialEq, Eq)]
struct Mne(&'static str);

impl Mne {
    const ADC: Self = Self("ADC");
    const AND: Self = Self("AND");
    const ASL: Self = Self("ASL");
    const BCC: Self = Self("BCC");
    const BCS: Self = Self("BCS");
    const BEQ: Self = Self("BEQ");
    const BIT: Self = Self("BIT");
    const BMI: Self = Self("BMI");
    const BNE: Self = Self("BNE");
    const BPL: Self = Self("BPL");
    const BRA: Self = Self("BRA");
    const BRK: Self = Self("BRK");
    const BRL: Self = Self("BRL");
    const BVC: Self = Self("BVC");
    const BVS: Self = Self("BVS");
    const CLC: Self = Self("CLC");
    const CLD: Self = Self("CLD");
    const CLI: Self = Self("CLI");
    const CLV: Self = Self("CLV");
    const CMP: Self = Self("CMP");
    const CPX: Self = Self("CPX");
    const CPY: Self = Self("CPY");
    const COP: Self = Self("COP");
    const DEC: Self = Self("DEC");
    const DEX: Self = Self("DEX");
    const DEY: Self = Self("DEY");
    const EOR: Self = Self("EOR");
    const INC: Self = Self("INC");
    const INX: Self = Self("INX");
    const INY: Self = Self("INY");
    const JMP: Self = Self("JMP");
    const JML: Self = Self("JML");
    const JSR: Self = Self("JSR");
    const JSL: Self = Self("JSL");
    const LDA: Self = Self("LDA");
    const LDX: Self = Self("LDX");
    const LDY: Self = Self("LDY");
    const LSR: Self = Self("LSR");
    const MVN: Self = Self("MVN");
    const MVP: Self = Self("MVP");
    const NOP: Self = Self("NOP");
    const ORA: Self = Self("ORA");
    const PEA: Self = Self("PEA");
    const PEI: Self = Self("PEI");
    const PER: Self = Self("PER");
    const PHA: Self = Self("PHA");
    const PHB: Self = Self("PHB");
    const PHD: Self = Self("PHD");
    const PHK: Self = Self("PHK");
    const PHP: Self = Self("PHP");
    const PHX: Self = Self("PHX");
    const PHY: Self = Self("PHY");
    const PLA: Self = Self("PLA");
    const PLB: Self = Self("PLB");
    const PLD: Self = Self("PLD");
    const PLP: Self = Self("PLP");
    const PLX: Self = Self("PLX");
    const PLY: Self = Self("PLY");
    const REP: Self = Self("REP");
    const ROL: Self = Self("ROL");
    const ROR: Self = Self("ROR");
    const RTI: Self = Self("RTI");
    const RTS: Self = Self("RTS");
    const RTL: Self = Self("RTL");
    const SBC: Self = Self("SBC");
    const SEC: Self = Self("SEC");
    const SED: Self = Self("SED");
    const SEI: Self = Self("SEI");
    const SEP: Self = Self("SEP");
    const STA: Self = Self("STA");
    const STX: Self = Self("STX");
    const STY: Self = Self("STY");
    const STP: Self = Self("STP");
    const STZ: Self = Self("STZ");
    const TAX: Self = Self("TAX");
    const TAY: Self = Self("TAY");
    const TCD: Self = Self("TCD");
    const TCS: Self = Self("TCS");
    const TDC: Self = Self("TDC");
    const TSC: Self = Self("TSC");
    const TSX: Self = Self("TSX");
    const TXA: Self = Self("TXA");
    const TXS: Self = Self("TXS");
    const TXY: Self = Self("TXY");
    const TYA: Self = Self("TYA");
    const TYX: Self = Self("TYX");
    const TRB: Self = Self("TRB");
    const TSB: Self = Self("TSB");
    const WAI: Self = Self("WAI");
    const WDC: Self = Self("WDC");
    const XCE: Self = Self("XCE");
    const XBA: Self = Self("XBA");
}

const DIRECTIVES: &[(&'static str, Tok)] = &[
    ("BYTE", Tok::BYTE),
    ("WORD", Tok::WORD),
    ("LONG", Tok::LONG),
    ("SECTION", Tok::SECTION),
    ("INCLUDE", Tok::INCLUDE),
    ("IF", Tok::IF),
    ("IFDEF", Tok::IFDEF),
    ("IFNDEF", Tok::IFNDEF),
    ("END", Tok::END),
    ("RES", Tok::RES),
    ("MACRO", Tok::MACRO),
    ("LOOP", Tok::LOOP),
    ("FAIL", Tok::FAIL),
    ("STRUCT", Tok::STRUCT),
    ("TAG", Tok::TAG),
    ("LEN", Tok::LEN),
    ("INDEX8", Tok::INDEX8),
    ("INDEX16", Tok::INDEX16),
    ("ACCUM8", Tok::ACCUM8),
    ("ACCUM16", Tok::ACCUM16),
    ("EMULATION", Tok::EMULATION),
    ("NATIVE", Tok::NATIVE),
    ("NARG", Tok::NARG),
    ("SHIFT", Tok::SHIFT),
    ("UNIQ", Tok::UNIQ),
    ("JOIN", Tok::JOIN),
    ("BREAK", Tok::BREAK),
];

const DIGRAPHS: &[(&[u8; 2], Tok)] = &[
    (b"<<", Tok::ASL),
    (b">>", Tok::ASR),
    (b"~>", Tok::LSR),
    (b"<=", Tok::LTE),
    (b">=", Tok::GTE),
    (b"==", Tok::LEQ),
    (b"!=", Tok::NEQ),
    (b"&&", Tok::AND),
    (b"||", Tok::LOR),
    (b"::", Tok::DUB),
];

trait TokStream<'a> {
    fn err(&self, msg: &str) -> io::Error;

    fn peek(&mut self) -> io::Result<Tok>;

    fn eat(&mut self);

    fn rewind(&mut self) -> io::Result<()>;

    fn str(&self) -> &str;

    fn num(&self) -> i32;

    fn pos(&self) -> Pos<'a>;
}

struct Lexer<'a, R> {
    reader: PeekReader<R>,
    string: String,
    number: i32,
    stash: Option<Tok>,
    pos: Pos<'a>,
}

impl<'a, R: Read + Seek> Lexer<'a, R> {
    fn new(reader: R, file: &'a str) -> Self {
        Self {
            reader: PeekReader::new(reader),
            string: String::new(),
            number: 0,
            stash: None,
            pos: Pos {
                file,
                line: 1,
                column: 1,
            },
        }
    }
}

impl<'a, R: Read + Seek> TokStream<'a> for Lexer<'a, R> {
    fn err(&self, msg: &str) -> io::Error {
        io::Error::new(
            ErrorKind::InvalidData,
            format!(
                "{}:{}:{}: {msg}",
                self.pos.file, self.pos.line, self.pos.column
            ),
        )
    }

    fn peek(&mut self) -> io::Result<Tok> {
        if let Some(tok) = self.stash {
            return Ok(tok);
        }
        // skip whitespace
        while let Some(c) = self.reader.peek()? {
            if !b" \t\r".contains(&c) {
                break;
            }
            self.reader.eat();
        }
        // skip comment
        if let Some(b';') = self.reader.peek()? {
            while !matches!(self.reader.peek()?, Some(b'\n')) {
                self.reader.eat();
            }
        }
        self.pos.line = self.reader.line;
        self.pos.column = self.reader.column;
        match self.reader.peek()? {
            None => {
                self.reader.eat();
                self.stash = Some(Tok::EOF);
                Ok(Tok::EOF)
            }
            // macro argument, directive (\foo), or maybe a skipped newline
            Some(b'\\') => {
                self.reader.eat();
                if let Some(b'\n') = self.reader.peek()? {
                    self.reader.eat();
                    return self.peek(); // TODO shouldn't recurse
                }
                // macro argument
                if let Some(c) = self.reader.peek()?
                    && c.is_ascii_digit()
                {
                    while let Some(c) = self.reader.peek()? {
                        if !c.is_ascii_digit() {
                            break;
                        }
                        self.string.push(c as char);
                        self.reader.eat();
                    }
                    self.number = i32::from_str_radix(&self.string, 10)
                        .map_err(|e| self.err(&e.to_string()))?;
                    if self.number < 1 {
                        return Err(self.err("macro argument must be positive"));
                    }
                    self.stash = Some(Tok::ARG);
                    return Ok(Tok::ARG);
                }
                // some other directive
                while let Some(c) = self.reader.peek()? {
                    if !c.is_ascii_alphanumeric() {
                        break;
                    }
                    self.reader.eat();
                    self.string.push(c.to_ascii_uppercase() as char);
                }
                if let Some(tok) = DIRECTIVES
                    .iter()
                    .find_map(|(s, tok)| (*s == self.string).then_some(tok))
                    .copied()
                {
                    self.stash = Some(tok);
                    return Ok(tok);
                }
                Err(self.err("unrecognized directive"))
            }
            // number
            Some(c) if c.is_ascii_digit() || c == b'$' || c == b'%' => {
                let radix = match c {
                    b'$' => {
                        self.reader.eat();
                        16
                    }
                    b'%' => {
                        self.reader.eat();
                        2
                    }
                    _ => 10,
                };
                // edge case: modulus
                if (c == b'%') && self.reader.peek()?.is_some_and(|nc| !b"01".contains(&nc)) {
                    self.stash = Some(Tok::MODULUS);
                    return Ok(Tok::MODULUS);
                }
                // parse number
                while let Some(c) = self.reader.peek()? {
                    if c == b'_' {
                        self.reader.eat();
                        continue; // allow '_' separators in numbers
                    }
                    if !c.is_ascii_alphanumeric() {
                        break;
                    }
                    self.string.push(c as char);
                    self.reader.eat();
                }
                self.number = i32::from_str_radix(&self.string, radix)
                    .map_err(|e| self.err(&e.to_string()))?;
                self.stash = Some(Tok::NUM);
                Ok(Tok::NUM)
            }
            // string
            Some(b'"') => {
                self.reader.eat();
                while let Some(c) = self.reader.peek()? {
                    if c == b'"' {
                        self.reader.eat();
                        break;
                    }
                    if c == b'\\' {
                        self.reader.eat();
                        self.string.push(match self.reader.peek()? {
                            Some(b'n') => b'\n',
                            Some(b'r') => b'\r',
                            Some(b't') => b'\t',
                            Some(b'\\') => b'\\',
                            Some(b'"') => b'"',
                            Some(b'0') => b'\0',
                            _ => return Err(self.err("invalid escape")),
                        } as char);
                        self.reader.eat();
                        continue;
                    }
                    self.string.push(c as char);
                    self.reader.eat();
                }
                self.stash = Some(Tok::STR);
                Ok(Tok::STR)
            }
            // char
            Some(b'\'') => {
                self.reader.eat();
                if let Some(c) = self.reader.peek()? {
                    if c == b'\\' {
                        self.reader.eat();
                        self.number = match self.reader.peek()? {
                            Some(b'n') => b'\n',
                            Some(b'r') => b'\r',
                            Some(b't') => b'\t',
                            Some(b'\\') => b'\\',
                            Some(b'\'') => b'\'',
                            Some(b'0') => b'\0',
                            _ => return Err(self.err("invalid escape")),
                        } as i32;
                    } else {
                        self.number = c as i32;
                    }
                    self.reader.eat();
                }
                if self.reader.peek()? != Some(b'\'') {
                    return Err(self.err("invalid character"));
                }
                self.reader.eat();
                self.stash = Some(Tok::NUM);
                return Ok(Tok::NUM);
            }
            // idents, and single chars
            Some(c) => {
                while let Some(c) = self.reader.peek()? {
                    if !c.is_ascii_alphanumeric() && !b"_.".contains(&c) {
                        break;
                    }
                    self.reader.eat();
                    self.string.push(c as char);
                }
                // c wasn't an ident, so wasnt eaten. digraph?
                if self.string.len() == 0 {
                    self.reader.eat();
                    if let Some(nc) = self.reader.peek()? {
                        let s = &[c.to_ascii_uppercase(), nc.to_ascii_uppercase()];
                        if let Some(tok) = DIGRAPHS
                            .iter()
                            .find_map(|(bs, tok)| (*bs == s).then_some(tok))
                            .copied()
                        {
                            self.reader.eat();
                            self.stash = Some(tok);
                            return Ok(tok);
                        }
                    }
                }
                // we already ate both chars. digraph?
                if self.string.len() == 2 {
                    let bytes = self.string.as_bytes();
                    let s = &[bytes[0].to_ascii_uppercase(), bytes[1].to_ascii_uppercase()];
                    if let Some(tok) = DIGRAPHS
                        .iter()
                        .find_map(|(bs, tok)| (*bs == s).then_some(tok))
                        .copied()
                    {
                        self.stash = Some(tok);
                        return Ok(tok);
                    }
                }
                if self.string.len() == 1 {
                    let bytes = self.string.as_bytes();
                    let tok = match bytes[0].to_ascii_uppercase() {
                        b'S' => Tok::S,
                        b'X' => Tok::X,
                        b'Y' => Tok::Y,
                        _ => Tok::ID,
                    };
                    self.stash = Some(tok);
                    return Ok(tok);
                }
                // ident?
                if !self.string.is_empty() {
                    self.stash = Some(Tok::ID);
                    return Ok(Tok::ID);
                }
                // else return an uppercase of whatever this char is
                self.stash = Some(Tok(c.to_ascii_uppercase()));
                Ok(Tok(c.to_ascii_uppercase()))
            }
        }
    }

    fn eat(&mut self) {
        self.string.clear();
        self.stash.take();
    }

    fn rewind(&mut self) -> io::Result<()> {
        self.string.clear();
        self.stash = None;
        self.pos.line = 1;
        self.pos.column = 1;
        self.reader.rewind()
    }

    fn str(&self) -> &str {
        &self.string
    }

    fn num(&self) -> i32 {
        self.number
    }

    fn pos(&self) -> Pos<'a> {
        self.pos
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MacroTok<'a> {
    Tok(Tok),
    Str(&'a str),
    Id(&'a str),
    Num(i32),
    Arg(usize),
    Narg,
    Shift,
    Uniq,
    Join(&'a [(Pos<'a>, MacroTok<'a>)]),
}

#[derive(Clone, Copy)]
struct Macro<'a> {
    name: &'a str,
    toks: &'a [(Pos<'a>, MacroTok<'a>)],
}

struct MacroInvocation<'a> {
    inner: Macro<'a>,
    unique: usize,
    index: usize,
    join_buf: String,
    args: VecDeque<&'a [(Pos<'a>, MacroTok<'a>)]>,
    arg_index: usize,
    pos: Pos<'a>,
}

impl<'a> TokStream<'a> for MacroInvocation<'a> {
    fn err(&self, msg: &str) -> io::Error {
        let pos = self.pos();
        io::Error::new(
            ErrorKind::InvalidData,
            format!(
                "{}:{}:{}: in macro \"{}\"\n\t{}:{}:{}: {msg}",
                self.pos.file,
                self.pos.line,
                self.pos.column,
                self.inner.name,
                pos.file,
                pos.line,
                pos.column
            ),
        )
    }

    fn peek(&mut self) -> io::Result<Tok> {
        if self.index == self.inner.toks.len() {
            return Ok(Tok::EOF);
        }
        match self.inner.toks[self.index] {
            (_, MacroTok::Tok(tok)) => Ok(tok),
            (_, MacroTok::Str(_)) => Ok(Tok::STR),
            (_, MacroTok::Id(_)) => Ok(Tok::ID),
            (_, MacroTok::Num(_)) => Ok(Tok::NUM),
            (_, MacroTok::Arg(index)) => {
                if index >= self.args.len() {
                    return Err(self.err("argument is undefined"));
                }
                match self.args[index][self.arg_index] {
                    (_, MacroTok::Tok(tok)) => Ok(tok),
                    (_, MacroTok::Str(_)) => Ok(Tok::STR),
                    (_, MacroTok::Id(_)) => Ok(Tok::ID),
                    (_, MacroTok::Num(_)) => Ok(Tok::NUM),
                    _ => unreachable!(),
                }
            }
            (_, MacroTok::Narg) => Ok(Tok::NUM),
            (_, MacroTok::Shift) => {
                if self.args.is_empty() {
                    return Err(self.err("no arguments to shift"));
                }
                Ok(Tok::NEWLINE)
            }
            (_, MacroTok::Uniq) => {
                self.join_buf.clear();
                write!(&mut self.join_buf, "_{}", self.unique).unwrap();
                Ok(Tok::ID)
            }
            (_, MacroTok::Join(toks)) => {
                self.join_buf.clear();
                for (_, tok) in toks {
                    match tok {
                        MacroTok::Id(string) => self.join_buf.push_str(string),
                        MacroTok::Str(string) => self.join_buf.push_str(string),
                        MacroTok::Num(val) => write!(&mut self.join_buf, "{val:X}").unwrap(),
                        MacroTok::Arg(index) => match self.args[*index][self.arg_index] {
                            (_, MacroTok::Str(string)) => self.join_buf.push_str(string),
                            (_, MacroTok::Id(string)) => self.join_buf.push_str(string),
                            _ => unreachable!(),
                        },
                        MacroTok::Uniq => write!(&mut self.join_buf, "_{}", self.unique).unwrap(),
                        _ => unreachable!(),
                    }
                }
                match toks.first().unwrap() {
                    (_, MacroTok::Id(_)) => Ok(Tok::ID),
                    (_, MacroTok::Str(_)) => Ok(Tok::STR),
                    (_, MacroTok::Num(_)) => Ok(Tok::STR),
                    (_, MacroTok::Arg(index)) => match self.args[*index][self.arg_index] {
                        (_, MacroTok::Str(_)) => Ok(Tok::STR),
                        (_, MacroTok::Id(_)) => Ok(Tok::ID),
                        _ => unreachable!(),
                    },
                    (_, MacroTok::Uniq) => Ok(Tok::ID),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn eat(&mut self) {
        match self.inner.toks[self.index] {
            (_, MacroTok::Shift) => {
                self.args.pop_front();
            }
            (_, MacroTok::Arg(index)) => {
                self.arg_index += 1;
                if self.arg_index < self.args[index].len() {
                    return;
                }
                self.arg_index = 0;
            }
            _ => {}
        }
        self.index += 1;
    }

    fn rewind(&mut self) -> io::Result<()> {
        self.index = 0;
        Ok(())
    }

    fn str(&self) -> &str {
        match self.inner.toks[self.index] {
            (_, MacroTok::Str(string)) => string,
            (_, MacroTok::Id(string)) => string,
            (_, MacroTok::Arg(index)) => match self.args[index][self.arg_index] {
                (_, MacroTok::Str(string)) => string,
                (_, MacroTok::Id(string)) => string,
                _ => unreachable!(),
            },
            (_, MacroTok::Join(_)) => &self.join_buf,
            (_, MacroTok::Uniq) => &self.join_buf,
            _ => unreachable!(),
        }
    }

    fn num(&self) -> i32 {
        match self.inner.toks[self.index] {
            (_, MacroTok::Num(val)) => val,
            (_, MacroTok::Arg(index)) => match self.args[index][self.arg_index] {
                (_, MacroTok::Num(val)) => val,
                _ => unreachable!(),
            },
            (_, MacroTok::Narg) => self.args.len() as i32,
            _ => unreachable!(),
        }
    }

    fn pos(&self) -> Pos<'a> {
        self.inner.toks[self.index].0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LoopTok<'a> {
    Tok(Tok),
    Str(&'a str),
    Id(&'a str),
    Num(i32),
    Iter,
    Join(&'a [(Pos<'a>, LoopTok<'a>)]),
}

struct Loop<'a> {
    toks: &'a [(Pos<'a>, LoopTok<'a>)],
    index: usize,
    join_buf: String,
    iter: usize,
    end: usize,
}

impl<'a> TokStream<'a> for Loop<'a> {
    fn err(&self, msg: &str) -> io::Error {
        let pos = self.pos();
        io::Error::new(
            ErrorKind::InvalidData,
            format!(
                "{}:{}:{}: in loop index {}: {msg}",
                pos.file, pos.line, pos.column, self.iter,
            ),
        )
    }

    fn peek(&mut self) -> io::Result<Tok> {
        if self.iter >= self.end {
            return Ok(Tok::EOF);
        }
        match self.toks[self.index] {
            (_, LoopTok::Tok(tok)) => Ok(tok),
            (_, LoopTok::Str(_)) => Ok(Tok::STR),
            (_, LoopTok::Id(_)) => Ok(Tok::ID),
            (_, LoopTok::Num(_)) => Ok(Tok::NUM),
            (_, LoopTok::Iter) => Ok(Tok::NUM),
            (_, LoopTok::Join(toks)) => {
                self.join_buf.clear();
                for (_, tok) in toks {
                    match tok {
                        LoopTok::Id(string) => self.join_buf.push_str(string),
                        LoopTok::Str(string) => self.join_buf.push_str(string),
                        LoopTok::Num(val) => write!(&mut self.join_buf, "{val:X}").unwrap(),
                        LoopTok::Iter => write!(&mut self.join_buf, "{}", self.iter).unwrap(),
                        _ => unreachable!(),
                    }
                }
                match toks.first().unwrap() {
                    (_, LoopTok::Id(_)) => Ok(Tok::ID),
                    (_, LoopTok::Str(_)) => Ok(Tok::STR),
                    (_, LoopTok::Num(_)) => Ok(Tok::STR),
                    (_, LoopTok::Iter) => Ok(Tok::STR),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn eat(&mut self) {
        self.index += 1;
        if self.index == self.toks.len() {
            self.iter += 1;
            self.index = 0;
        }
    }

    fn rewind(&mut self) -> io::Result<()> {
        unreachable!()
    }

    fn str(&self) -> &str {
        match self.toks[self.index] {
            (_, LoopTok::Str(string)) => string,
            (_, LoopTok::Id(string)) => string,
            (_, LoopTok::Join(_)) => &self.join_buf,
            _ => unreachable!(),
        }
    }

    fn num(&self) -> i32 {
        match self.toks[self.index] {
            (_, LoopTok::Num(val)) => val,
            (_, LoopTok::Iter) => self.iter as i32,
            _ => unreachable!(),
        }
    }

    fn pos(&self) -> Pos<'a> {
        self.toks[self.index].0
    }
}

struct PeekReader<R> {
    inner: R,
    line: usize,
    column: usize,
    stash: Option<u8>,
}

impl<R: Read + Seek> PeekReader<R> {
    fn new(reader: R) -> Self {
        Self {
            inner: reader,
            line: 1,
            column: 1,
            stash: None,
        }
    }

    fn peek(&mut self) -> io::Result<Option<u8>> {
        if self.stash.is_none() {
            let mut buf = [0];
            self.stash = self
                .inner
                .read(&mut buf)
                .map(|n| if n == 0 { None } else { Some(buf[0]) })?;
        }
        Ok(self.stash)
    }

    fn eat(&mut self) {
        self.column += 1;
        if let Some(b'\n') = self.stash.take() {
            self.line += 1;
            self.column = 1;
        }
    }

    fn rewind(&mut self) -> io::Result<()> {
        self.stash = None;
        self.line = 1;
        self.column = 1;
        self.inner.rewind()
    }
}
