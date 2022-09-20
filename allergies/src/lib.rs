pub struct Allergies {
    allergies: Vec<Allergen>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Allergen {
    Eggs,
    Peanuts,
    Shellfish,
    Strawberries,
    Tomatoes,
    Chocolate,
    Pollen,
    Cats,
}

impl Allergies {
    pub fn new(score: u32) -> Self {
        let mut allergies = vec![];
        let mut mut_score = score;

        while mut_score >= 256 {
            mut_score -= 128;
        }

        if mut_score >= 128 {
            allergies.push(Allergen::Cats);
            mut_score -= 128;
        }

        if mut_score >= 64 {
            allergies.push(Allergen::Pollen);
            mut_score -= 64;
        }

        if mut_score >= 32 {
            allergies.push(Allergen::Chocolate);
            mut_score -= 32;
        }

        if mut_score >= 16 {
            allergies.push(Allergen::Tomatoes);
            mut_score -= 16;
        }

        if mut_score >= 8 {
            allergies.push(Allergen::Strawberries);
            mut_score -= 8;
        }

        if mut_score >= 4 {
            allergies.push(Allergen::Shellfish);
            mut_score -= 4;
        }

        if mut_score >= 2 {
            allergies.push(Allergen::Peanuts);
            mut_score -= 2;
        }

        if mut_score >= 1 {
            allergies.push(Allergen::Eggs);
        }

        Allergies { allergies }
    }

    pub fn is_allergic_to(&self, allergen: &Allergen) -> bool {
        self.allergies.iter().any(|allergic| allergic == allergen)
    }

    pub fn allergies(&self) -> Vec<Allergen> {
        self.allergies.to_vec()
    }
}
