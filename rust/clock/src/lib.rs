use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub struct Clock {
    hours: i32,
    minutes: i32,
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let hours_str = if self.hours.abs() < 10 {
            format!("0{}", self.hours)
        } else {
            self.hours.to_string()
        };

        let minutes_str = if self.minutes.abs() < 10 {
            format!("0{}", self.minutes)
        } else {
            self.minutes.to_string()
        };

        write!(f, "{}:{}", hours_str, minutes_str)
    }
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let div24err = "Impossible happened: dividing by 24 failed";
        let div60err = "Impossible happened: dividing by 60 failed";

        let (hours, minutes) = if minutes < 0 {
            (
                hours + minutes.checked_div(60).expect(div60err) - 1,
                60 + minutes.checked_rem(60).expect(div60err),
            )
        } else {
            (hours, minutes)
        };

        let hours = if hours < 0 {
            24 + hours.checked_rem(24).expect(div24err)
        } else {
            hours
        };

        Clock {
            hours: (hours + minutes.checked_div(60).expect(div60err))
                .checked_rem(24)
                .expect(div24err),
            minutes: minutes.checked_rem(60).expect(div60err),
        }
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Clock::new(self.hours, self.minutes + minutes)
    }
}
