// This stub file contains items that aren't used yet; feel free to remove this module attribute
// to enable stricter warnings.
#![allow(unused)]

pub fn production_rate_per_hour(speed: u8) -> f64 {
    if speed == 0 {
        0.
    } else if 1 <= speed && speed <= 4 {
        1. * optimistic_production_rate(speed)
    } else if 5 <= speed && speed <= 8 {
        0.9 * optimistic_production_rate(speed)
    } else if 9 <= speed && speed <= 10 {
        0.77 * optimistic_production_rate(speed)
    } else {
        panic!("Production speed can only be from 0 to 10");
    }
}

fn optimistic_production_rate(speed: u8) -> f64 {
    (speed as f64) * 221.
}

pub fn working_items_per_minute(speed: u8) -> u32 {
    (production_rate_per_hour(speed) / 60.) as u32
}
