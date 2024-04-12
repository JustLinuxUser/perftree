use core::panic;
use cozy_chess::{BitBoard, Move, *};
use std::collections::BTreeMap;
use std::fmt::Display;
use std::io::{self, BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};

const INITIAL_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

pub struct State {
    stockfish: Stockfish,
    script: Script,
    cozy: Cozy,
    fen: String,
    moves: Vec<String>,
    depth: usize,
}

impl State {
    pub fn new<S>(cmd: S) -> io::Result<State>
    where
        S: Into<String>,
    {
        Ok(State {
            stockfish: Stockfish::new()?,
            script: Script::new(cmd),
            fen: INITIAL_FEN.to_string(),
            moves: Vec::new(),
            depth: 1,
            cozy: Cozy {},
        })
    }

    pub fn fen(&self) -> &str {
        &self.fen
    }

    pub fn set_fen<S>(&mut self, fen: S)
    where
        S: Into<String>,
    {
        self.fen = fen.into();
        self.moves.clear();
    }

    pub fn moves(&self) -> &[String] {
        &self.moves
    }

    pub fn set_moves<V>(&mut self, moves: V)
    where
        V: Into<Vec<String>>,
    {
        self.moves = moves.into();
    }

    pub fn depth(&self) -> usize {
        self.depth
    }

    pub fn set_depth(&mut self, depth: usize) {
        self.depth = depth;
    }

    pub fn goto_root(&mut self) {
        self.moves.clear();
    }

    pub fn goto_parent(&mut self) {
        self.moves.pop();
    }

    pub fn goto_child<S>(&mut self, move_: S)
    where
        S: Into<String>,
    {
        self.moves.push(move_.into());
    }

    pub fn diff(&mut self) -> io::Result<Diff> {
        Ok(Diff::new(
            &self
                .script
                .perft(&self.fen, &self.moves, self.depth - self.moves.len())?,
            &self
                .cozy
                .perft(&self.fen, &self.moves, self.depth - self.moves.len())?,
        ))
    }
}

#[derive(Debug, Clone)]
pub struct Diff {
    total_count: (u128, u128),
    child_count: BTreeMap<String, (Option<u128>, Option<u128>)>,
}

impl Diff {
    pub fn new(lhs: &Perft, rhs: &Perft) -> Diff {
        let mut child_count = BTreeMap::new();
        for (move_, &count) in &lhs.child_count {
            child_count.entry(move_.clone()).or_insert((None, None)).0 = Some(count);
        }
        for (move_, &count) in &rhs.child_count {
            child_count.entry(move_.clone()).or_insert((None, None)).1 = Some(count);
        }
        Diff {
            total_count: (lhs.total_count, rhs.total_count),
            child_count,
        }
    }

    pub fn total_count(&self) -> (u128, u128) {
        self.total_count
    }

    pub fn child_count(&self) -> &BTreeMap<String, (Option<u128>, Option<u128>)> {
        &self.child_count
    }
}

pub trait Engine {
    fn perft(&mut self, fen: &str, moves: &[String], depth: usize) -> io::Result<Perft>;
}

pub struct Perft {
    total_count: u128,
    child_count: BTreeMap<String, u128>,
}

impl Perft {
    pub fn new(total_count: u128, child_count: BTreeMap<String, u128>) -> Perft {
        Perft {
            total_count,
            child_count,
        }
    }

    pub fn total_count(&self) -> u128 {
        self.total_count
    }

    pub fn child_count(&self) -> &BTreeMap<String, u128> {
        &self.child_count
    }
}

pub struct Script {
    cmd: String,
}

impl Script {
    pub fn new<S>(cmd: S) -> Script
    where
        S: Into<String>,
    {
        Script { cmd: cmd.into() }
    }
}

impl Engine for Script {
    fn perft(&mut self, fen: &str, moves: &[String], depth: usize) -> io::Result<Perft> {
        let mut command = Command::new(&self.cmd);
        command.arg(depth.to_string());
        command.arg(fen);
        if !moves.is_empty() {
            command.arg(moves.join(" "));
        }

        let output = command.output()?;
        //re-raise output from stderr
        io::stderr().write_all(&output.stderr)?;
        let mut lines = output.stdout.lines();

        let mut child_count = BTreeMap::new();
        loop {
            let line = lines
                .next()
                .ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "unexpected eof while parsing script output",
                    )
                })
                .and_then(|result| result)?;

            if line.is_empty() {
                break;
            }
            let mut parts = line.split_whitespace();
            let move_ = parts
                .next()
                .ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "unexpected end of line; expected move and count separated by spaces",
                    )
                })?
                .to_string();
            let count = parts
                .next()
                .ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "unexpected end of line; expected move and count separated by spaces",
                    )
                })?
                .parse()
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;
            child_count.insert(move_, count);
        }

        let total_count = lines
            .next()
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "unexpected eof while parsing script output",
                )
            })
            .and_then(|result| result)?
            .parse()
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;

        Ok(Perft {
            child_count,
            total_count,
        })
    }
}

pub struct Stockfish {
    child: Child,
    inp: BufReader<ChildStdout>,
    out: ChildStdin,
}

pub struct Cozy {}
impl Engine for Cozy {
    fn perft(&mut self, fen: &str, moves: &[String], depth: usize) -> io::Result<Perft> {
        fn recursive_perft(b: &Board, m: Move, depth: usize) -> u128 {
            if depth == 0 {
                return 1;
            }
            let mut new_board = b.clone();
            new_board.play(m);

            let mut total_count = 0u128;
            new_board.generate_moves(|m| {
                for m in m {
                    total_count += recursive_perft(&new_board, m, depth - 1);
                }
                false
            });
            total_count
        }
        let mut child_count = BTreeMap::<String, u128>::new();
        let mut total_count = 0u128;
        let mut is_shredder = false;
        let res = Board::from_fen(fen, false);
        let mut b = match res {
            Ok(b) => b,
            Err(e) => match e {
                FenParseError::InvalidCastlingRights => {
                    is_shredder = true;
                    Board::from_fen(fen, true).expect("successfully able to parse the shredder fen")
                }
                _ => panic!("Failed to parse fen"),
            },
        };
        if is_shredder {
            println!("Dealing with a shredder fen");
        }
        for m in moves {
            b.play(m.parse().unwrap())
        }
        b.generate_moves(|m| {
            for m in m {
                let from = format!("{}", m.from);
                let mut to_sq = format!("{}", m.to);
                if !is_shredder {
                    if let Some(Piece::King) = b.piece_on(m.from) {
                        match (m.from, m.to) {
                            (Square::E1, Square::A1) => to_sq = Square::C1.to_string(),
                            (Square::E1, Square::H1) => to_sq = Square::G1.to_string(),
                            (Square::E8, Square::A8) => to_sq = Square::C8.to_string(),
                            (Square::E8, Square::H8) => to_sq = Square::G8.to_string(),
                            _ => (),
                        }
                    }
                }
                let promo = match m.promotion {
                    Some(Piece::Queen) => "q",
                    Some(Piece::Rook) => "r",
                    Some(Piece::Bishop) => "b",
                    Some(Piece::Knight) => "k",
                    _ => "",
                };
                let move_str = from.clone() + &to_sq + promo;
                let curr_count = child_count.get(&move_str).unwrap_or(&0);
                let count = recursive_perft(&b, m, depth - 1);
                child_count.insert(move_str, curr_count + count);
                total_count += count;
            }
            false
        });
        Ok(Perft {
            total_count,
            child_count,
        })
    }
}

impl Stockfish {
    pub fn new() -> io::Result<Stockfish> {
        let mut child = Command::new("stockfish")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;

        let mut inp = BufReader::new(child.stdout.take().expect("stdout not captured"));
        // consume/skip header
        let mut buf = String::new();
        inp.read_line(&mut buf)?;

        let out = child.stdin.take().expect("stdin not captured");

        Ok(Stockfish { child, inp, out })
    }
}

impl Engine for Stockfish {
    fn perft(&mut self, fen: &str, moves: &[String], depth: usize) -> io::Result<Perft> {
        // send command to stockfish
        let mut buf = String::new();

        write!(self.out, "position fen {}", fen)?;
        if !moves.is_empty() {
            write!(self.out, " moves {}", moves.join(" "))?;
        }
        write!(self.out, "\ngo perft {}\n", depth)?;

        // parse child counts
        let mut child_count = BTreeMap::new();
        loop {
            buf.clear();
            self.inp.read_line(&mut buf)?;
            if buf.trim().is_empty() {
                break;
            }
            if buf.trim().contains("Stockfish") || buf.trim().contains("info") {
                continue;
            }
            let mut parts = buf.trim().split(": ");
            let move_ = parts
                .next()
                .ok_or_else(|| {
                    println!("got: {buf}");
                    io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "unexpected end of line, Stockfish",
                    )
                })?
                .to_string();
            let count = parts
                .next()
                .ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "unexpected end of line, Stockfish",
                    )
                })?
                .parse()
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;
            child_count.insert(move_, count);
        }

        // parse total count
        loop {
            buf.clear();
            self.inp.read_line(&mut buf)?;
            if !buf.contains("Stockfish") && !buf.contains("info") && !buf.trim().is_empty() {
                break;
            }
        }
        let mut parts = buf.trim().split(": ");
        let total_count = parts
            .nth(1)
            .ok_or_else(|| {
                println!("got: {buf}");
                io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "unexpected end of line, Stockfish total count",
                )
            })?
            .parse()
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;

        // throw away empty line
        buf.clear();
        self.inp.read_line(&mut buf)?;

        Ok(Perft {
            child_count,
            total_count,
        })
    }
}

impl Drop for Stockfish {
    fn drop(&mut self) {
        let _ = self.child.kill();
    }
}
