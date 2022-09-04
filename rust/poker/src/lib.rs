use int_enum::IntEnum;
use std::cmp;
use std::collections;

/// Given a list of poker hands, return a list of those hands which win.
///
/// Note the type signature: this function should return _the same_ reference to
/// the winning hand(s) as were passed in, not reconstructed strings which happen to be equal.
pub fn winning_hands<'a>(_hands: &[&'a str]) -> Vec<&'a str> {
    unimplemented!("winning_hands")
    // let mut parsed_hands = hands
    //     .into_iter()
    //     .map(|hand| Hand::parse_hand(hand).map(|cards| cards_to_category(cards)))
    //     .collect::<Vec<Hand<Category>>>();

    // parsed_hands.sort_by(|a, b| a.partial_cmp(b).unwrap_or(cmp::Ordering::Greater));

    // parsed_hands
    //     .into_iter()
    //     .map(|hand| hand.reference)
    //     .collect()
}

#[derive(PartialEq)]
struct Hand<'a, T> {
    reference: &'a str,
    hand: T,
}

#[derive(Debug, PartialEq, Eq)]
enum Category {
    FiveOfAKind,
    StraightFlush,
    FourOfAKind,
    FullHouse,
    Flush,
    Straight,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
}

impl Category {
    fn new(mut hand: Vec<Card>) -> Category {
        let mut ranks = collections::HashMap::new();

        for card in &hand {
            ranks
                .entry(Card::joker_map(None, *card, |card| Some(card.rank)))
                .and_modify(|count| *count += 1)
                .or_insert(1);
        }

        let n_of_a_kind = ranks
            .keys()
            .flatten()
            .map(|key| ranks[&Some(*key)])
            .max()
            .unwrap_or(0);

        if n_of_a_kind + ranks.get(&None).unwrap_or(&0) >= 5 {
            return Category::FiveOfAKind;
        }

        if n_of_a_kind + ranks.get(&None).unwrap_or(&0) == 4 {
            return Category::FourOfAKind;
        }

        let is_full_house_without_jokers =
            n_of_a_kind == 3 && ranks.iter().any(|(_, &count)| count == 2);

        let is_full_house_with_joker =
            ranks.iter().filter(|count| *count.1 == 2).count() >= 2 && ranks[&None] >= 1;

        if is_full_house_without_jokers || is_full_house_with_joker {
            return Category::FullHouse;
        }

        let mut suit = None;

        let is_flush = hand.iter().all(|card| match card {
            Card::Joker => true,
            Card::Regular(card) => match suit {
                None => {
                    suit = Some(card.suit);
                    true
                }
                Some(suit) => suit == card.suit,
            },
        });

        hand.sort();

        let is_straight_flush =
            hand.iter()
                .enumerate()
                .filter(|(i, _)| *i != 0)
                .all(|(i, card)| {
                    Card::joker_map(true, *card, &|card: RegularCard| {
                        Card::joker_map(true, hand[i - 1], &|prev_card: RegularCard| {
                            Ok(card.rank) == Rank::from_int(prev_card.rank.int_value() + 1)
                        })
                    })
                });

        if is_straight_flush {
            return Category::StraightFlush;
        }

        if is_flush {
            return Category::Flush;
        }

        unimplemented!("Category::new")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identifies_flush() {
        let make_club = |rank| Card::new(rank, Suit::Clubs);
        assert_eq!(
            Category::new(vec![
                make_club(Rank::Jack),
                make_club(Rank::Eight),
                make_club(Rank::Nine),
                make_club(Rank::Four),
                make_club(Rank::Three)
            ]),
            Category::Flush
        );
    }

    #[test]
    fn identifies_full_house() {
        let make_ten = |suit| Card::new(Rank::Ten, suit);
        let make_nine = |suit| Card::new(Rank::Nine, suit);

        assert_eq!(
            Category::new(vec![
                make_ten(Suit::Spades),
                make_ten(Suit::Hearts),
                make_ten(Suit::Clubs),
                make_nine(Suit::Clubs),
                make_nine(Suit::Hearts),
            ]),
            Category::FullHouse
        );

        assert_eq!(
            Category::new(vec![
                make_ten(Suit::Spades),
                make_ten(Suit::Hearts),
                Card::Joker,
                make_nine(Suit::Clubs),
                make_nine(Suit::Hearts),
            ]),
            Category::FullHouse
        );
    }

    #[test]
    fn identifies_four_of_a_kind() {
        let make_ten = |suit| Card::new(Rank::Ten, suit);
        assert_eq!(
            Category::new(vec![
                make_ten(Suit::Spades),
                make_ten(Suit::Hearts),
                Card::new(Rank::Jack, Suit::Spades),
                Card::Joker,
                make_ten(Suit::Clubs),
            ]),
            Category::FourOfAKind
        );
    }

    #[test]
    fn identifies_straight_flush() {
        let make_club = |rank| Card::new(rank, Suit::Clubs);
        assert_eq!(
            Category::new(vec![
                make_club(Rank::Jack),
                make_club(Rank::Ten),
                make_club(Rank::Nine),
                make_club(Rank::Eight),
                make_club(Rank::Seven)
            ]),
            Category::StraightFlush
        );

        assert_eq!(
            Category::new(vec![
                make_club(Rank::Nine),
                make_club(Rank::Ten),
                make_club(Rank::Seven),
                make_club(Rank::Eight),
                make_club(Rank::Jack),
            ]),
            Category::StraightFlush
        );
    }

    #[test]
    fn identifies_five_of_a_kind() {
        let make_ace = |suit| Card::new(Rank::Ace, suit);
        assert_eq!(
            Category::new(vec![
                Card::Joker,
                make_ace(Suit::Spades),
                make_ace(Suit::Hearts),
                make_ace(Suit::Diamonds),
                make_ace(Suit::Clubs),
            ]),
            Category::FiveOfAKind
        );

        let make_jack = |suit| Card::new(Rank::Jack, suit);
        assert_eq!(
            Category::new(vec![
                make_jack(Suit::Spades),
                make_jack(Suit::Hearts),
                make_jack(Suit::Diamonds),
                Card::Joker,
                make_jack(Suit::Clubs),
            ]),
            Category::FiveOfAKind
        );
    }
}

impl Hand<'_, Vec<Card>> {
    fn parse_hand(input: &str) -> Hand<Vec<Card>> {
        let mut cards = input
            .split_whitespace()
            .into_iter()
            .map(|card| {
                let rank = match card.chars().nth(0).expect("Invalid rank") {
                    '2' => Rank::Two,
                    '3' => Rank::Three,
                    '4' => Rank::Four,
                    '5' => Rank::Five,
                    '6' => Rank::Six,
                    '7' => Rank::Seven,
                    '8' => Rank::Eight,
                    '9' => Rank::Nine,
                    '1' => Rank::Ten,
                    'J' => Rank::Jack,
                    'Q' => Rank::Queen,
                    'K' => Rank::King,
                    'A' => Rank::Ace,
                    _ => panic!("Invalid rank"),
                };

                let suit = match card.chars().nth(1).expect("Invalid suit") {
                    'S' => Suit::Spades,
                    'H' => Suit::Hearts,
                    'D' => Suit::Diamonds,
                    'C' => Suit::Clubs,
                    _ => panic!("Invalid sui"),
                };

                Card::Regular(RegularCard { rank, suit })
            })
            .collect::<Vec<Card>>();

        cards.sort();

        Hand {
            reference: input,
            hand: cards,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Card {
    Regular(RegularCard),
    Joker,
}

impl Card {
    fn new(rank: Rank, suit: Suit) -> Card {
        Card::Regular(RegularCard { rank, suit })
    }

    fn joker_map<F, T>(joker_val: T, card: Card, mut f: F) -> T
    where
        F: FnMut(RegularCard) -> T,
    {
        match card {
            Card::Joker => joker_val,
            Card::Regular(card) => f(card),
        }
    }

    fn compare(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (Card::Joker, Card::Joker) => cmp::Ordering::Equal,
            (Card::Joker, Card::Regular(_)) => cmp::Ordering::Greater,
            (Card::Regular(_), Card::Joker) => cmp::Ordering::Less,
            (Card::Regular(lhs), Card::Regular(rhs)) => lhs.cmp(rhs),
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct RegularCard {
    suit: Suit,
    rank: Rank,
}

impl RegularCard {
    fn compare(&self, other: &Self) -> cmp::Ordering {
        if self.rank > other.rank {
            cmp::Ordering::Greater
        } else if self.rank < other.rank {
            cmp::Ordering::Less
        } else {
            self.suit.cmp(&other.suit)
        }
    }
}

impl cmp::PartialOrd for RegularCard {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.compare(other))
    }
}

impl cmp::Ord for RegularCard {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.compare(other)
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, IntEnum, PartialEq, Eq)]
enum Suit {
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
