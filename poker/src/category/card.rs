use int_enum::IntEnum;
use std::cmp;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Card {
    pub suit: Suit,
    pub rank: Rank,
}

impl Card {
    pub fn new(rank: Rank, suit: Suit) -> Card {
        Card { rank, suit }
    }

    fn compare(&self, other: &Self) -> cmp::Ordering {
        if self.rank > other.rank {
            cmp::Ordering::Greater
        } else if self.rank < other.rank {
            cmp::Ordering::Less
        } else {
            cmp::Ordering::Equal
        }
    }
}

impl cmp::PartialOrd for Card {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.compare(other))
    }
}

impl cmp::Ord for Card {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.compare(other)
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, IntEnum, PartialEq, Eq)]
pub enum Suit {
    Clubs = 1,
    Spades = 2,
    Diamonds = 3,
    Hearts = 4,
}

impl cmp::PartialOrd for Suit {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.int_value().partial_cmp(&other.int_value())
    }
}

impl cmp::Ord for Suit {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.int_value().cmp(&other.int_value())
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, IntEnum)]
pub enum Rank {
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7,
    Eight = 8,
    Nine = 9,
    Ten = 10,
    Jack = 11,
    Queen = 12,
    King = 13,
    Ace = 14,
}

impl cmp::PartialOrd for Rank {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.int_value().partial_cmp(&other.int_value())
    }
}

impl cmp::Ord for Rank {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.int_value().cmp(&other.int_value())
    }
}
