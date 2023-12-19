#![warn(clippy::pedantic)]
use std::cell::RefCell;
use std::rc::Rc;
use std::str::FromStr;

type BoxError = Box<dyn std::error::Error>;

type Result<T, E = BoxError> = std::result::Result<T, E>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Creature {
    id: i32,
    color: i32,
    ty: i32,
    scanned_us: bool,
    scanned_them: bool,
}

use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// Basicly a glorified tuple
pub struct Vec2d<T> {
    /// x
    pub x: T,
    /// y
    pub y: T,
}

/// Vec2d<u32>
pub type Vu2d = Vec2d<u32>;
/// Vec2d<i32>
pub type Vi2d = Vec2d<i32>;
/// Vec2d<f32>
pub type Vf2d = Vec2d<f32>;

impl<T> Vec2d<T> {
    /// Cast the Vec2d to an other Vec2d with a differant inner type
    #[inline]
    pub fn cast<U: From<T>>(self) -> Vec2d<U> {
        let x: U = self.x.into();
        let y: U = self.y.into();
        Vec2d { x, y }
    }
}

//impl<T> Vec2d<T> where T: Add<T> + Sub<T> + Div<T> + Mul<T> {

//}

impl<T: Copy> Vec2d<T>
where
    T: Add<Output = T>
        + Sub<Output = T>
        + Mul<Output = T>
        + Div<Output = T>
        + Neg<Output = T>
        + Into<f64>,
{
    /// Return the magnitude (hypotenus) of the given Vec2d as f64
    pub fn mag_f64(&self) -> f64 {
        let mag2: f64 = self.mag_squared().into();
        mag2.sqrt()
    }

    /// Return the magnitude (hypotenus) of the given Vec2d as T without doing the square root
    pub fn mag_squared(&self) -> T {
        self.x * self.x + self.y * self.y
    }

    /// Return the normal of the given Vec2d
    #[must_use]
    pub fn perp(&self) -> Self {
        Vec2d {
            x: -self.y,
            y: self.x,
        }
    }

    /// Perform the dot product on the Vec2ds
    pub fn dot(&self, rhs: &Self) -> T {
        self.x + rhs.x + self.y + rhs.y
    }

    /// Perform the cross product on the Vec2ds
    pub fn cross(&self, rhs: &Self) -> T {
        self.x + rhs.x - self.y + rhs.y
    }
}

macro_rules! operator {
    ($trait:tt, $operator:tt, $func_name:ident) => {
        impl<T:Copy> $trait for Vec2d<T> where T: $trait<Output = T>, {
            type Output = Self;
            fn $func_name(self, rhs: Self) -> Self::Output {
                Vec2d { x: self.x $operator rhs.x, y: self.y $operator rhs.y}
            }
        }
    };
    ($trait:tt, $operator:tt, $func_name:ident, $type:ty) => {
        impl<T:Copy> $trait<$type> for Vec2d<T> where T: $trait<Output = T>, {
            type Output = Self;
            fn $func_name(self, rhs: $type) -> Self::Output {
                Vec2d { x: self.x $operator rhs, y: self.y $operator rhs}
            }
        }
    };
}
macro_rules! operator_assign {
    ($trait:tt, $operator:tt, $func_name:ident) => {
        impl<T:Copy> $trait for Vec2d<T> where T: $trait<T>, {
            fn $func_name(&mut self, rhs: Self){
                self.x $operator rhs.x; self.y $operator rhs.y;
            }
            }
        };
    ($trait:tt, $operator:tt, $func_name:ident, $type:ty) => {
        impl<T:Copy> $trait<$type> for Vec2d<T> where T: $trait<T>, {
            fn $func_name(&mut self, rhs: $type){
                self.x $operator rhs; self.y $operator rhs;
            }
            }
        };
}

operator!(Add, + , add);
operator!(Sub, - , sub);
operator!(Mul, * , mul);
operator!(Div, / , div);
operator!(Mul, * , mul, T);
operator!(Div, / , div, T);
operator_assign!(AddAssign, += , add_assign);
operator_assign!(SubAssign, -= , sub_assign);
operator_assign!(MulAssign, *= , mul_assign, T);
operator_assign!(DivAssign, /= , div_assign, T);

impl<T: Copy> From<(T, T)> for Vec2d<T> {
    fn from(t: (T, T)) -> Self {
        Vec2d { x: t.0, y: t.1 }
    }
}
impl<T: Copy> From<[T; 2]> for Vec2d<T> {
    fn from(t: [T; 2]) -> Self {
        Vec2d { x: t[0], y: t[1] }
    }
}

impl Drone {
    fn from_input(
        _: &GameInfo,
        input: &mut InputManager<'_>,
        tag: impl std::fmt::Display,
    ) -> Result<Rc<RefCell<Self>>> {
        let mut words = input.words(format!("{tag}"))?;
        let id = words.next().ok_or(format!("{tag} Error ID"))?.parse()?;
        Ok(Rc::new(RefCell::new(Drone {
            id,
            pos: Vec2D {
                x: words
                    .next()
                    .ok_or(format!("{tag} Error X"))?
                    .trim()
                    .parse()?,
                y: words
                    .next()
                    .ok_or(format!("{tag} Error Y"))?
                    .trim()
                    .parse()?,
            },
            battery: {
                words.next();
                words
                    .next()
                    .ok_or(format!("{tag} Error Battery"))?
                    .parse()?
            },
        })))
    }
}

impl Fish {
    fn from_input(
        info: &GameInfo,
        input: &mut InputManager<'_>,
        tag: impl std::fmt::Display,
    ) -> Result<Self> {
        let mut words = input.words(&tag)?;
        let id = words
            .next()
            .ok_or(format!("{tag} Error Id"))?
            .parse::<i32>()?;
        let pos = {
            Vec2D {
                x: words.next().ok_or(format!("{tag} Error Pos.x"))?.parse()?,
                y: words.next().ok_or(format!("{tag} Error Pos.y"))?.parse()?,
            }
        };
        let vel = {
            Vec2D {
                x: words.next().ok_or(format!("{tag} Error Vel.x"))?.parse()?,
                y: words.next().ok_or(format!("{tag} Error Vel.y"))?.parse()?,
            }
        };
        Ok(Fish {
            creature: Rc::clone(
                info.creatures
                    .iter()
                    .find(|c| c.borrow().id == id)
                    .ok_or("Invalid fish id")?,
            ),
            pos,
            velocity: vel,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct GameInfo {
    creature_count: i32,
    creatures: Vec<Rc<RefCell<Creature>>>,
}
type Vec2D = Vec2d<i32>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Drone {
    id: i32,
    pos: Vec2D,
    battery: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Fish {
    creature: Rc<RefCell<Creature>>,
    pos: Vec2D,
    velocity: Vec2D,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RoundInfo {
    score: i32,
    enemy_score: i32,
    scans: Vec<Rc<RefCell<Creature>>>,
    enemy_scans: Vec<Rc<RefCell<Creature>>>,
    drones: Vec<Rc<RefCell<Drone>>>,
    enemy_drones: Vec<Rc<RefCell<Drone>>>,
    fishes: Vec<Fish>,
    radar_blips: Vec<RadarBlip>,
}

struct InputManager<'i> {
    stdin: std::io::Lines<std::io::StdinLock<'i>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Direction {
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

impl FromStr for Direction {
    type Err = String;
    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        match s {
            "TL" => Ok(Self::TopLeft),
            "TR" => Ok(Self::TopRight),
            "BL" => Ok(Self::BottomLeft),
            "BR" => Ok(Self::BottomRight),
            _ => Err(format!("Unknown Direction {s}")),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RadarBlip {
    target: Rc<RefCell<Creature>>,
    source: Rc<RefCell<Drone>>,
    direction: Direction,
}

impl RadarBlip {
    fn from_input(
        info: &GameInfo,
        input: &mut InputManager<'_>,
        drones: &[Rc<RefCell<Drone>>],
        tag: impl std::fmt::Display,
    ) -> Result<Self> {
        let mut words = input.words(&tag)?;
        let drone_id = words
            .next()
            .ok_or(format!("{tag} Error Drone.Id"))?
            .parse::<i32>()?;
        let creature_id = words
            .next()
            .ok_or(format!("{tag} Error Creature.Id"))?
            .parse::<i32>()?;
        let direction = words
            .next()
            .ok_or(format!("{tag} Error Creature.Id"))?
            .parse::<Direction>()?;
        Ok(RadarBlip {
            target: Rc::clone(
                info.creatures
                    .iter()
                    .find(|c| c.borrow().id == creature_id)
                    .ok_or(format!("{tag} Unknown Creature"))?,
            ),
            source: Rc::clone(
                drones
                    .iter()
                    .find(|c| c.borrow().id == drone_id)
                    .ok_or(format!("{tag} Unknown Drone"))?,
            ),
            direction,
        })
    }
}

impl<'i> InputManager<'i> {
    pub fn line(&mut self, msg: impl std::fmt::Display) -> Result<String> {
        let line = self
            .stdin
            .next()
            .ok_or("Error when reading a line")??
            .trim()
            .to_string();
        eprintln!("READ({msg}): '{line}'");
        Ok(line)
    }

    pub fn words(&mut self, msg: impl std::fmt::Display) -> Result<impl Iterator<Item = String>> {
        Ok(self
            .line(format_args!("words: {msg}"))?
            .split_whitespace()
            .map(str::trim)
            .map(String::from)
            .collect::<Vec<_>>()
            .into_iter())
    }
}

impl RoundInfo {
    fn parse_inputs(info: &GameInfo, stdin: &mut InputManager<'_>) -> Result<Self> {
        let my_score: i32 = stdin.line("My Score")?.parse()?;
        let their_score: i32 = stdin.line("Their Score")?.parse()?;

        let my_scans_count: i32 = stdin.line("My Scans Count")?.parse()?;

        let my_scans = (0..my_scans_count)
            .map(|i| -> Result<Rc<RefCell<Creature>>> {
                let id: i32 = stdin.line(format_args!("My Scan[{i}]"))?.parse()?;
                Ok(Rc::clone(
                    info.creatures
                        .iter()
                        .find(|c| c.borrow().id == id)
                        .ok_or("Invalid id")?,
                ))
            })
            .collect::<Result<Vec<_>>>()?;
        let their_scans_count: i32 = stdin.line("Their Scans Count")?.parse()?;
        let their_scans = (0..their_scans_count)
            .map(|i| -> Result<Rc<RefCell<Creature>>> {
                let id: i32 = stdin.line(format_args!("Their Scan[{i}]"))?.parse()?;
                Ok(Rc::clone(
                    info.creatures
                        .iter()
                        .find(|c| c.borrow().id == id)
                        .ok_or("Invalid id")?,
                ))
            })
            .collect::<Result<Vec<_>>>()?;
        their_scans
            .iter()
            .for_each(|c| c.borrow_mut().scanned_them |= true);
        my_scans
            .iter()
            .for_each(|c| c.borrow_mut().scanned_us |= true);
        let my_drone_count = stdin.line(format_args!("My Drone Count"))?.parse()?;
        let my_drones = (0..my_drone_count)
            .map(|i| Drone::from_input(info, stdin, format_args!("My Drone[{i}]")))
            .collect::<Result<Vec<Rc<RefCell<Drone>>>>>()?;

        let enemy_drone_count = stdin.line(format_args!("Their Drone Count"))?.parse()?;
        let enemy_drones = (0..enemy_drone_count)
            .map(|i| Drone::from_input(info, stdin, format_args!("Their Drone[{i}]")))
            .collect::<Result<Vec<Rc<RefCell<Drone>>>>>()?;

        let drone_scan_holding = stdin.line("Scans Held Count")?.parse()?;
        let _drone_scans_holding = (0..drone_scan_holding)
            .map(|_i| stdin.line("Scan Held")).map(|s| s.map(|_| ()))
            .collect::<Result<Vec<()>>>();
        let fish_count = stdin.line("Fishes Count")?.parse()?;
        let fishes = (0..fish_count)
            .map(|i| Fish::from_input(info, stdin, format_args!("Fish[{i}]")))
            .collect::<Result<Vec<Fish>>>()?;
        let radar_blip_count = stdin.line("Radar Blip Count")?.parse()?;
        let radar_blips = (0..radar_blip_count)
            .map(|i| RadarBlip::from_input(info, stdin, &my_drones, format_args!("RadarBlip[{i}]")))
            .collect::<Result<Vec<RadarBlip>>>()?;

        Ok(Self {
            score: my_score,
            enemy_score: their_score,
            scans: my_scans,
            enemy_scans: their_scans,
            drones: my_drones,
            enemy_drones,
            fishes,
            radar_blips,
        })
    }
}

trait GameAI: Sized {
    fn init(game_manager: &GameInfo) -> Self;
    fn turn(&mut self, info: &GameInfo, round_info: &RoundInfo) -> Result<()>;
}

impl GameInfo {
    pub fn run<G: GameAI>() {
        if let Err(e) = Self::run_inner::<G>() {
            eprintln!("{e:?}");
            eprintln!("{e}");
        }
    }

    pub fn run_inner<G: GameAI>() -> Result<()> {
        let mut stdin = InputManager {
            stdin: std::io::stdin().lines(),
        };
        let creature_count = stdin.line("Creature Count")?.parse()?;
        let creatures = (0..creature_count)
            .map(|i| -> Result<Rc<RefCell<Creature>>> {
                let mut words = stdin.words(format!("Creature[{i}]"))?;
                let id = words.next().ok_or("CreatureLine")?.parse::<i32>()?;
                let color = words.next().ok_or("CreatureLine")?.parse::<i32>()?;
                let ty = words.next().ok_or("CreatureLine")?.parse::<i32>()?;
                Ok(Rc::new(RefCell::new(Creature {
                    id,
                    color,
                    ty,
                    scanned_us: false,
                    scanned_them: false,
                })))
            })
            .collect::<Result<Vec<_>>>()?;
        let info: Self = Self {
            creature_count,
            creatures,
        };
        let mut ai = G::init(&info);
        for round in 0.. {
            eprintln!("Round {round}");
            let round_info = RoundInfo::parse_inputs(&info, &mut stdin)?;
            ai.turn(&info, &round_info)?;
        }
        Ok(())
    }

    #[allow(clippy::unused_self)]
    pub fn wait(&self, light: bool) {
        println!("WAIT {}", if light { "1" } else { "0" });
    }

    #[allow(clippy::unused_self)]
    pub fn goto(&self, coords: Vec2D, light: bool) {
        println!(
            "MOVE {} {} {}",
            coords.x,
            coords.y,
            if light { "1" } else { "0" }
        );
    }
}
struct MaixAI {}

impl GameAI for MaixAI {
    fn init(_game_manager: &GameInfo) -> Self {
        Self {}
    }

    fn turn(&mut self, info: &GameInfo, round: &RoundInfo) -> Result<()> {
        round.drones.iter().try_for_each::<_, Result<()>>(|drone| {
            let Some(&Fish {
                pos: goto,
                ref creature,
                ..
            }) = round
                .fishes
                .iter()
                .filter(|c| !c.creature.borrow().scanned_us)
                .min_by_key(|c| (c.pos - drone.borrow().pos).mag_squared())
            else {
                info.wait(drone.borrow().battery > 8);
                return Ok(());
            };

            eprintln!("Targetting fish: {:?}", creature.borrow());
            info.goto(goto, drone.borrow().battery > 8);
            Ok(())
        })?;

        Ok(())
    }
}

fn main() {
    GameInfo::run::<MaixAI>();
}
