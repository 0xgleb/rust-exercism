pub use category::*;
use std::cmp;
use std::cmp::Ordering;

mod category;

/// Given a list of poker hands, return a list of those hands which win.
///
/// Note the type signature: this function should return _the same_ reference to
/// the winning hand(s) as were passed in, not reconstructed strings which happen to be equal.
pub fn winning_hands<'a>(hands: &[&'a str]) -> Vec<&'a str> {
    let mut parsed_hands = hands
        .into_iter()
        .map(|hand| {
            let card_hand = Hand::parse_hand(hand);

            Hand {
                reference: card_hand.reference,
                hand: category::Category::new(card_hand.hand),
            }
        })
        .collect::<Vec<Hand<category::Category>>>();

    parsed_hands.sort_by(|a, b| b.partial_cmp(a).unwrap());

    let result = parsed_hands
        .iter()
        .take_while(|hand| hand.partial_cmp(&&&parsed_hands[0]) == Some(cmp::Ordering::Equal))
        .map(|hand| hand.reference)
        .collect();

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

impl Hand<'_, Vec<category::Card>> {
    fn parse_hand(input: &str) -> Hand<Vec<category::Card>> {
        let char_to_suit = |char| match char {
            'S' => category::Suit::Spades,
            'H' => category::Suit::Hearts,
            'D' => category::Suit::Diamonds,
            'C' => category::Suit::Clubs,
            _ => panic!("Invalid suit"),
        };

        let mut cards = input
            .split_whitespace()
            .into_iter()
            .map(|card| {
                if card.len() == 2 {
                    let rank = match card.chars().nth(0).expect("Invalid rank") {
                        '2' => category::Rank::Two,
                        '3' => category::Rank::Three,
                        '4' => category::Rank::Four,
                        '5' => category::Rank::Five,
                        '6' => category::Rank::Six,
                        '7' => category::Rank::Seven,
                        '8' => category::Rank::Eight,
                        '9' => category::Rank::Nine,
                        'J' => category::Rank::Jack,
                        'Q' => category::Rank::Queen,
                        'K' => category::Rank::King,
                        'A' => category::Rank::Ace,
                        _ => panic!("Invalid rank"),
                    };

                    let suit = char_to_suit(card.chars().nth(1).expect("Invalid suit"));

                    category::Card { rank, suit }
                } else {
                    category::Card {
                        rank: category::Rank::Ten,
                        suit: char_to_suit(card.chars().nth(2).expect("Invalid card")),
                    }
                }
            })
            .collect::<Vec<category::Card>>();

        cards.sort();

        Hand {
            reference: input,
            hand: cards,
        }
    }
}
