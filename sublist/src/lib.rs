#[derive(Debug, PartialEq, Eq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

pub fn sublist<T: PartialEq>(first_list: &[T], second_list: &[T]) -> Comparison {
    let sublist = check_if_sublist(first_list, second_list);
    let superlist = check_if_sublist(second_list, first_list);

    if sublist && superlist {
        Comparison::Equal
    } else if sublist {
        Comparison::Sublist
    } else if superlist {
        Comparison::Superlist
    } else {
        Comparison::Unequal
    }
}

fn check_if_sublist<T: PartialEq>(inner_list: &[T], outer_list: &[T]) -> bool {
    if inner_list.len() == 0 {
        return true;
    }

    if outer_list.len() < inner_list.len() {
        return false;
    }

    let mut is_equal = true;

    for i in 0..(outer_list.len() - inner_list.len() + 1) {
        is_equal = true;
        for j in 0..inner_list.len() {
            if inner_list[j] != outer_list[i + j] {
                is_equal = false;
                break;
            }
        }

        if is_equal {
            return true;
        }
    }

    is_equal
}
