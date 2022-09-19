use int_enum::IntEnum;
use std::cmp;
use std::cmp::Ordering;
use std::collections;

pub use card::*;

mod card;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Category {
    FiveOfAKind,
    StraightFlush(Rank),
    FourOfAKind {
        four_rank: Rank,
        kicker_rank: Rank,
    },
    FullHouse {
        triplet_rank: Rank,
        pair_rank: Rank,
    },
    Flush(Vec<Rank>),
    Straight(Rank),
    ThreeOfAKind {
        triplet_rank: Rank,
        kicker_rank: Rank,
    },
    TwoPair {
        pair1_rank: Rank,
        pair2_rank: Rank,
        kicker_rank: Rank,
    },
    OnePair(Rank),
    HighCard(Vec<Card>),
}

impl cmp::PartialOrd for Category {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(match (self, other) {
            (Category::FiveOfAKind, Category::FiveOfAKind) => Ordering::Equal,
            (Category::StraightFlush(lrank), Category::StraightFlush(rrank)) => lrank.cmp(rrank),
            (
                Category::FourOfAKind {
                    four_rank: lfour_rank,
                    kicker_rank: lkicker_rank,
                },
                Category::FourOfAKind {
                    four_rank: rfour_rank,
                    kicker_rank: rkicker_rank,
                },
            ) => {
                let cmp = lfour_rank.cmp(rfour_rank);
                if cmp == Ordering::Equal {
                    lkicker_rank.cmp(rkicker_rank)
                } else {
                    cmp
                }
            }
            (
                Category::FullHouse {
                    triplet_rank: ltriplet_rank,
                    pair_rank: lpair_rank,
                },
                Category::FullHouse {
                    triplet_rank: rtriplet_rank,
                    pair_rank: rpair_rank,
                },
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
            (
                Category::ThreeOfAKind {
                    triplet_rank: lrank,
                    kicker_rank: lhighest,
                },
                Category::ThreeOfAKind {
                    triplet_rank: rrank,
                    kicker_rank: rhighest,
                },
            ) => {
                let cmp = lrank.cmp(rrank);
                if cmp == Ordering::Equal {
                    lhighest.cmp(rhighest)
                } else {
                    cmp
                }
            }
            (
                Category::TwoPair {
                    pair1_rank: lpair1,
                    pair2_rank: lpair2,
                    kicker_rank: lother_card,
                },
                Category::TwoPair {
                    pair1_rank: rpair1,
                    pair2_rank: rpair2,
                    kicker_rank: rother_card,
                },
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
                    Category::FourOfAKind { .. } => 8,
                    Category::FullHouse { .. } => 7,
                    Category::Flush(_) => 6,
                    Category::Straight(_) => 5,
                    Category::ThreeOfAKind { .. } => 4,
                    Category::TwoPair { .. } => 3,
                    Category::OnePair(_) => 2,
                    Category::HighCard(_) => 1,
                };

                to_rank(lhs).cmp(&to_rank(rhs))
            }
        })
    }
}

impl Category {
    pub fn new(mut hand: Vec<Card>) -> Category {
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
            return Category::FourOfAKind {
                four_rank: find_rank(4),
                kicker_rank: find_rank(1),
            };
        }

        let is_full_house = n_of_a_kind == 3 && ranks.iter().any(|(_, &count)| count == 2);

        if is_full_house {
            return Category::FullHouse {
                triplet_rank: find_rank(3),
                pair_rank: find_rank(2),
            };
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
            return Category::ThreeOfAKind {
                triplet_rank: *ranks.iter().find(|count| *count.1 == 3).unwrap().0,
                kicker_rank: *ranks.iter().filter(|count| *count.1 != 3).max().unwrap().0,
            };
        }

        let pairs: Vec<_> = ranks.iter().filter(|count| *count.1 == 2).collect();

        if pairs.len() == 2 {
            return Category::TwoPair {
                pair1_rank: *pairs[0].0,
                pair2_rank: *pairs[1].0,
                kicker_rank: *ranks.iter().find(|count| *count.1 == 1).unwrap().0,
            };
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
        let two_pair = Category::TwoPair {
            pair1_rank: Rank::Five,
            pair2_rank: Rank::Four,
            kicker_rank: Rank::King,
        };
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
            Category::ThreeOfAKind {
                triplet_rank: Rank::Queen,
                kicker_rank: Rank::Seven
            }
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
            Category::FullHouse {
                triplet_rank: Rank::Ten,
                pair_rank: Rank::Nine
            }
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
            Category::FourOfAKind {
                four_rank: Rank::Ten,
                kicker_rank: Rank::Jack
            }
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
