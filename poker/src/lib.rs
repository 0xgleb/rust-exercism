use int_enum::IntEnum;
use std::cmp;
use std::cmp::Ordering;
use std::collections;

const DEBUG: bool = false;

/// Given a list of poker hands, return a list of those hands which win.
///
/// Note the type signature: this function should return _the same_ reference to
/// the winning hand(s) as were passed in, not reconstructed strings which happen to be equal.
pub fn winning_hands<'a>(hands: &[&'a str]) -> Vec<&'a str> {
    if DEBUG {
        println!("\n\n\n");

        for hand in hands {
            println!("{}", hand)
        }
    }

    let mut parsed_hands = hands
        .into_iter()
        .map(|hand| {
            let card_hand = Hand::parse_hand(hand);

            Hand {
                reference: card_hand.reference,
                hand: Category::new(card_hand.hand),
            }
        })
        .collect::<Vec<Hand<Category>>>();

    if DEBUG {
        println!("\n\n\n");

        for hand in &parsed_hands {
            println!("{:?}", hand)
        }
    }

    parsed_hands.sort_by(|a, b| b.partial_cmp(a).unwrap());

    if DEBUG {
        println!("\n\n\n");

        for hand in &parsed_hands {
            println!("{:?}", hand)
        }
    }

    let result = parsed_hands
        .iter()
        .take_while(|hand| hand.partial_cmp(&&&parsed_hands[0]) == Some(cmp::Ordering::Equal))
        .map(|hand| hand.reference)
        .collect();

    if DEBUG {
        println!("\n\n\n");

        for hand in &result {
            println!("{:?}", hand)
        }

        println!("\n\n\n");
    }

    result
}

#[derive(Debug, PartialEq)]
struct Hand<'a, T> {
    reference: &'a str,
    hand: T,
}

impl<T: PartialOrd> cmp::PartialOrd for Hand<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.hand.partial_cmp(&other.hand)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Category {
    FiveOfAKind,
    StraightFlush(Rank),
    FourOfAKind(Rank, Rank),
    FullHouse(Rank, Rank),
    Flush(Vec<Rank>),
    Straight(Rank),
    ThreeOfAKind(Rank, Rank),
    TwoPair(Rank, Rank, Rank),
    OnePair(Rank),
    HighCard(Vec<Card>),
}

impl cmp::PartialOrd for Category {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(match (self, other) {
            (Category::FiveOfAKind, Category::FiveOfAKind) => Ordering::Equal,
            (Category::StraightFlush(lrank), Category::StraightFlush(rrank)) => lrank.cmp(rrank),
            (
                Category::FourOfAKind(lfour_rank, lkicker_rank),
                Category::FourOfAKind(rfour_rank, rkicker_rank),
            ) => {
                let cmp = lfour_rank.cmp(rfour_rank);
                if cmp == Ordering::Equal {
                    lkicker_rank.cmp(rkicker_rank)
                } else {
                    cmp
                }
            }
            (
                Category::FullHouse(ltriplet_rank, lpair_rank),
                Category::FullHouse(rtriplet_rank, rpair_rank),
            ) => {
                let cmp = ltriplet_rank.cmp(rtriplet_rank);
                if cmp == Ordering::Equal {
                    lpair_rank.cmp(rpair_rank)
                } else {
                    cmp
                }
            }
            (Category::Flush(lranks), Category::Flush(rranks)) => lranks.cmp(rranks),
            (Category::Straight(lrank), Category::Straight(rrank)) => lrank.cmp(rrank),
            (Category::ThreeOfAKind(lrank, lhighest), Category::ThreeOfAKind(rrank, rhighest)) => {
                let cmp = lrank.cmp(rrank);
                if cmp == Ordering::Equal {
                    lhighest.cmp(rhighest)
                } else {
                    cmp
                }
            }
            (
                Category::TwoPair(lpair1, lpair2, lother_card),
                Category::TwoPair(rpair1, rpair2, rother_card),
            ) => {
                let lhs = lpair1.max(lpair2);
                let rhs = rpair1.max(rpair2);

                let mut cmp = lhs.cmp(rhs);

                if cmp == Ordering::Equal {
                    let lhs = lpair1.min(lpair2);
                    let rhs = rpair1.min(rpair2);

                    cmp = lhs.cmp(rhs);
                }

                if cmp == Ordering::Equal {
                    cmp = lother_card.cmp(rother_card);
                }

                cmp
            }
            (Category::OnePair(lhs), Category::OnePair(rhs)) => lhs.cmp(rhs),
            (Category::HighCard(lhs), Category::HighCard(rhs)) => lhs.cmp(rhs),
            (lhs, rhs) => {
                let to_rank = |category: &Category| match category {
                    Category::FiveOfAKind => 10,
                    Category::StraightFlush(_) => 9,
                    Category::FourOfAKind(_, _) => 8,
                    Category::FullHouse(_, _) => 7,
                    Category::Flush(_) => 6,
                    Category::Straight(_) => 5,
                    Category::ThreeOfAKind(_, _) => 4,
                    Category::TwoPair(_, _, _) => 3,
                    Category::OnePair(_) => 2,
                    Category::HighCard(_) => 1,
                };

                to_rank(lhs).cmp(&to_rank(rhs))
            }
        })
    }
}

impl Category {
    fn new(mut hand: Vec<Card>) -> Category {
        let mut ranks = collections::HashMap::new();

        for card in &hand {
            ranks
                .entry(card.rank)
                .and_modify(|count| *count += 1)
                .or_insert(1);
        }

        let n_of_a_kind = ranks.keys().map(|key| ranks[key]).max().unwrap_or(0);

        let find_rank = |n| *ranks.iter().find(|count| *count.1 == n).unwrap().0;

        if n_of_a_kind >= 5 {
            return Category::FiveOfAKind;
        }

        if n_of_a_kind == 4 {
            return Category::FourOfAKind(find_rank(4), find_rank(1));
        }

        let is_full_house = n_of_a_kind == 3 && ranks.iter().any(|(_, &count)| count == 2);

        if is_full_house {
            return Category::FullHouse(find_rank(3), find_rank(2));
        }

        let mut suit = None;

        let is_flush = hand.iter().all(|card| match suit {
            None => {
                suit = Some(card.suit);
                true
            }
            Some(suit) => suit == card.suit,
        });

        hand.sort();

        let is_straight =
            hand.iter()
                .enumerate()
                .filter(|(i, _)| *i != 0)
                .all(|(i, current_card)| {
                    let prev_card = hand[i - 1];
                    Ok(current_card.rank) == Rank::from_int(prev_card.rank.int_value() + 1)
                        || (prev_card.rank == Rank::Two
                            && current_card.rank == Rank::Three
                            && i == 1
                            && hand[hand.len() - 1].rank == Rank::Ace)
                        || (current_card.rank == Rank::Ace
                            && i == hand.len() - 1
                            && hand[0].rank == Rank::Two)
                });

        let get_straight_rank = || {
            if hand[0].rank == Rank::Two && hand[hand.len() - 1].rank == Rank::Ace {
                hand[hand.len() - 2].rank
            } else {
                *ranks.iter().max_by(|x, y| x.0.cmp(y.0)).unwrap().0
            }
        };

        if is_straight && is_flush {
            return Category::StraightFlush(get_straight_rank());
        }

        if is_flush {
            return Category::Flush(hand.iter().rev().map(|hand| hand.rank).collect());
        }

        if is_straight {
            return Category::Straight(get_straight_rank());
        }

        if n_of_a_kind == 3 {
            return Category::ThreeOfAKind(
                *ranks.iter().find(|count| *count.1 == 3).unwrap().0,
                *ranks.iter().filter(|count| *count.1 != 3).max().unwrap().0,
            );
        }

        let pairs: Vec<_> = ranks.iter().filter(|count| *count.1 == 2).collect();

        if pairs.len() == 2 {
            return Category::TwoPair(
                *pairs[0].0,
                *pairs[1].0,
                *ranks.iter().find(|count| *count.1 == 1).unwrap().0,
            );
        }

        hand.reverse();

        if let Some((rank, _)) = ranks.iter().find(|count| *count.1 == 2) {
            return Category::OnePair(*rank);
        }

        Category::HighCard(hand)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn two_pair_better_than_one() {
        let two_pair = Category::TwoPair(Rank::Five, Rank::Four, Rank::King);
        let one_pair = Category::OnePair(Rank::Eight);
        let lower_one_pair = Category::OnePair(Rank::Two);

        assert_eq!(two_pair.partial_cmp(&one_pair), Some(Ordering::Greater));

        assert_eq!(
            two_pair.partial_cmp(&lower_one_pair),
            Some(Ordering::Greater)
        );

        assert_eq!(lower_one_pair.partial_cmp(&two_pair), Some(Ordering::Less));

        assert_eq!(
            one_pair.partial_cmp(&lower_one_pair),
            Some(Ordering::Greater)
        );

        let mut vec1 = vec![lower_one_pair.clone(), two_pair.clone(), one_pair.clone()];
        let vec2 = vec![two_pair.clone(), one_pair.clone(), lower_one_pair.clone()];

        vec1.sort_by(|a, b| b.partial_cmp(a).unwrap());

        assert_eq!(vec1, vec2);
    }

    #[test]
    fn highest_card() {
        let original = vec![
            Card::new(Rank::Three, Suit::Spades),
            Card::new(Rank::Four, Suit::Spades),
            Card::new(Rank::Five, Suit::Diamonds),
            Card::new(Rank::Six, Suit::Hearts),
            Card::new(Rank::Jack, Suit::Hearts),
        ];

        let sorted = vec![
            Card::new(Rank::Jack, Suit::Hearts),
            Card::new(Rank::Six, Suit::Hearts),
            Card::new(Rank::Five, Suit::Diamonds),
            Card::new(Rank::Four, Suit::Spades),
            Card::new(Rank::Three, Suit::Spades),
        ];

        assert_eq!(Category::new(original), Category::HighCard(sorted));
    }

    #[test]
    fn identifies_one_pair() {
        assert_eq!(
            Category::new(vec![
                Card::new(Rank::Queen, Suit::Diamonds),
                Card::new(Rank::Queen, Suit::Hearts),
                Card::new(Rank::Seven, Suit::Spades),
                Card::new(Rank::Nine, Suit::Diamonds),
                Card::new(Rank::Six, Suit::Clubs),
            ]),
            Category::OnePair(Rank::Queen)
        );
    }

    #[test]
    fn identifies_three_of_a_kind() {
        assert_eq!(
            Category::new(vec![
                Card::new(Rank::Queen, Suit::Diamonds),
                Card::new(Rank::Queen, Suit::Hearts),
                Card::new(Rank::Queen, Suit::Spades),
                Card::new(Rank::Seven, Suit::Diamonds),
                Card::new(Rank::Six, Suit::Clubs),
            ]),
            Category::ThreeOfAKind(Rank::Queen, Rank::Seven)
        );
    }

    #[test]
    fn identifies_straight() {
        assert_eq!(
            Category::new(vec![
                Card::new(Rank::Ten, Suit::Diamonds),
                Card::new(Rank::Eight, Suit::Hearts),
                Card::new(Rank::Nine, Suit::Spades),
                Card::new(Rank::Seven, Suit::Diamonds),
                Card::new(Rank::Six, Suit::Clubs),
            ]),
            Category::Straight(Rank::Ten)
        );
    }

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
            Category::Flush(vec![
                Rank::Jack,
                Rank::Nine,
                Rank::Eight,
                Rank::Four,
                Rank::Three
            ])
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
            Category::FullHouse(Rank::Ten, Rank::Nine)
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
                make_ten(Suit::Diamonds),
                make_ten(Suit::Clubs),
            ]),
            Category::FourOfAKind(Rank::Ten, Rank::Jack)
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
            Category::StraightFlush(Rank::Jack)
        );

        assert_eq!(
            Category::new(vec![
                make_club(Rank::Nine),
                make_club(Rank::Ten),
                make_club(Rank::Seven),
                make_club(Rank::Eight),
                make_club(Rank::Jack),
            ]),
            Category::StraightFlush(Rank::Jack)
        );
    }

    #[test]
    fn identifies_five_of_a_kind() {
        let make_ace = |suit| Card::new(Rank::Ace, suit);
        assert_eq!(
            Category::new(vec![
                make_ace(Suit::Hearts),
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
                make_jack(Suit::Diamonds),
                make_jack(Suit::Clubs),
            ]),
            Category::FiveOfAKind
        );
    }
}

impl Hand<'_, Vec<Card>> {
    fn parse_hand(input: &str) -> Hand<Vec<Card>> {
        let char_to_suit = |char| match char {
            'S' => Suit::Spades,
            'H' => Suit::Hearts,
            'D' => Suit::Diamonds,
            'C' => Suit::Clubs,
            _ => panic!("Invalid suit"),
        };

        let mut cards = input
            .split_whitespace()
            .into_iter()
            .map(|card| {
                if card.len() == 2 {
                    let rank = match card.chars().nth(0).expect("Invalid rank") {
                        '2' => Rank::Two,
                        '3' => Rank::Three,
                        '4' => Rank::Four,
                        '5' => Rank::Five,
                        '6' => Rank::Six,
                        '7' => Rank::Seven,
                        '8' => Rank::Eight,
                        '9' => Rank::Nine,
                        'J' => Rank::Jack,
                        'Q' => Rank::Queen,
                        'K' => Rank::King,
                        'A' => Rank::Ace,
                        _ => panic!("Invalid rank"),
                    };

                    let suit = char_to_suit(card.chars().nth(1).expect("Invalid suit"));

                    Card { rank, suit }
                } else {
                    Card {
                        rank: Rank::Ten,
                        suit: char_to_suit(card.chars().nth(2).expect("Invalid card")),
                    }
                }
            })
            .collect::<Vec<Card>>();

        cards.sort();

        Hand {
            reference: input,
            hand: cards,
        }
    }
}

// impl Card {
//     fn new(rank: Rank, suit: Suit) -> Card {
//         Card::Regular(RegularCard { rank, suit })
//     }

//     fn joker_map<F, T>(joker_val: T, card: Card, mut f: F) -> T
//     where
//         F: FnMut(RegularCard) -> T,
//     {
//         match card {
//             Card::Joker => joker_val,
//             Card::Regular(card) => f(card),
//         }
//     }

//     fn compare(&self, other: &Self) -> cmp::Ordering {
//         match (self, other) {
//             (Card::Joker, Card::Joker) => cmp::Ordering::Equal,
//             (Card::Joker, Card::Regular(_)) => cmp::Ordering::Greater,
//             (Card::Regular(_), Card::Joker) => cmp::Ordering::Less,
//             (Card::Regular(lhs), Card::Regular(rhs)) => lhs.cmp(rhs),
//         }
//     }
// }

// impl cmp::PartialOrd for Card {
//     fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
//         Some(self.compare(other))
//     }
// }

// impl cmp::Ord for Card {
//     fn cmp(&self, other: &Self) -> cmp::Ordering {
//         self.compare(other)
//     }
// }

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Card {
    suit: Suit,
    rank: Rank,
}

impl Card {
    fn new(rank: Rank, suit: Suit) -> Card {
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
