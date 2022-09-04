use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub struct Clock {
    minutes: i32,
}

struct HourClock {
    hours: i32,
    minutes: i32,
}

const MINUTES_IN_AN_HOUR: i32 = 60;
const HOURS_IN_A_DAY: i32 = 24;

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        Clock {
            minutes: (hours * MINUTES_IN_AN_HOUR + minutes)
                .rem_euclid(HOURS_IN_A_DAY * MINUTES_IN_AN_HOUR),
        }
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Clock {
            minutes: self.minutes + minutes,
        }
    }

    fn to_hour_clock(&self) -> HourClock {
        HourClock {
            hours: self
                .minutes
                .div_euclid(MINUTES_IN_AN_HOUR)
                .rem_euclid(HOURS_IN_A_DAY),
            minutes: self.minutes.rem_euclid(MINUTES_IN_AN_HOUR),
        }
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let hour_clock = self.to_hour_clock();
        write!(f, "{:02}:{:02}", hour_clock.hours, hour_clock.minutes)
    }
}
