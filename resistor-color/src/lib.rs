use enum_iterator;
use int_enum::IntEnum;

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, enum_iterator::Sequence, IntEnum)]
pub enum ResistorColor {
    Black = 0,
    Blue = 6,
    Brown = 1,
    Green = 5,
    Grey = 8,
    Orange = 3,
    Red = 2,
    Violet = 7,
    White = 9,
    Yellow = 4,
}

pub fn color_to_value(color: ResistorColor) -> u32 {
    color.int_value().into()
}

pub fn value_to_color_string(value: u32) -> String {
    String::from(match value {
        0 => "Black",
        6 => "Blue",
        1 => "Brown",
        5 => "Green",
        8 => "Grey",
        3 => "Orange",
        2 => "Red",
        7 => "Violet",
        9 => "White",
        4 => "Yellow",
        _ => "value out of range",
    })
}

pub fn colors() -> Vec<ResistorColor> {
    let mut all_colors: Vec<ResistorColor> = enum_iterator::all().collect();

    all_colors.sort_by(|x, y| x.int_value().cmp(&y.int_value()));

    all_colors
}
