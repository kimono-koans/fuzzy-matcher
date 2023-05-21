use crate::{FuzzyMatcher, IndexType, ScoreType};

pub fn cheap_matches(
    choice: &[char],
    pattern: &[char],
    case_sensitive: bool,
) -> Option<Vec<usize>> {
    let first_match_indices: Vec<usize> = choice
        .iter()
        .enumerate()
        .zip(pattern.iter())
        .map_while(|((idx, c), p)| {
            if char_equal(*c, *p, case_sensitive) {
                return Some(idx + 1usize)
            }
            
            None
        }).collect();

    if !first_match_indices.is_empty() {
        return Some(first_match_indices)
    }
    
    None    
}

/// Given 2 character, check if they are equal (considering ascii case)
/// e.g. ('a', 'A', true) => false
/// e.g. ('a', 'A', false) => true
#[inline]
pub fn char_equal(a: char, b: char, case_sensitive: bool) -> bool {
    if case_sensitive {
        a == b
    } else {
        a.eq_ignore_ascii_case(&b)
    }
}

#[derive(Debug, PartialEq)]
pub enum CharType {
    NonWord,
    Lower,
    Upper,
    Number,
}

#[inline]
pub fn char_type_of(ch: char) -> CharType {
    if ch.is_lowercase() {
        CharType::Lower
    } else if ch.is_uppercase() {
        CharType::Upper
    } else if ch.is_numeric() {
        CharType::Number
    } else {
        CharType::NonWord
    }
}

#[derive(Debug, PartialEq)]
pub enum CharRole {
    Tail,
    Head,
}

// checkout https://github.com/llvm-mirror/clang-tools-extra/blob/master/clangd/FuzzyMatch.cpp
// The Role can be determined from the Type of a character and its neighbors:
//
//   Example  | Chars | Type | Role
//   ---------+--------------+-----
//   F(o)oBar | Foo   | Ull  | Tail
//   Foo(B)ar | oBa   | lUl  | Head
//   (f)oo    | ^fo   | Ell  | Head
//   H(T)TP   | HTT   | UUU  | Tail
//
//      Curr= Empty Lower Upper Separ
// Prev=Empty 0x00, 0xaa, 0xaa, 0xff, // At start, Lower|Upper->Head
// Prev=Lower 0x00, 0x55, 0xaa, 0xff, // In word, Upper->Head;Lower->Tail
// Prev=Upper 0x00, 0x55, 0x59, 0xff, // Ditto, but U(U)U->Tail
// Prev=Separ 0x00, 0xaa, 0xaa, 0xff, // After separator, like at start
pub fn char_role(prev: char, cur: char) -> CharRole {
    use self::CharRole::*;
    use self::CharType::*;
    match (char_type_of(prev), char_type_of(cur)) {
        (Lower, Upper) | (NonWord, Lower) | (NonWord, Upper) => Head,
        _ => Tail,
    }
}

#[allow(dead_code)]
pub fn assert_order<'a>(matcher: &dyn FuzzyMatcher, pattern: &str, choices: &[&'a str]) {
    let result = filter_and_sort(matcher, pattern, choices);

    assert_eq!(result, choices);

    // debug print
    println!("pattern: {}", pattern);
    choices.iter().for_each(|choice| {
        if let Some((score, indices)) = matcher.fuzzy_indices(choice, pattern) {
            println!("{}: {:?}", score, wrap_matches(choice, &indices));
        } else {
            println!("NO MATCH for {}", choice);
        }
    });
}

#[allow(dead_code)]
pub fn filter_and_sort<'a>(
    matcher: &dyn FuzzyMatcher,
    pattern: &str,
    lines: &[&'a str],
) -> Vec<&'a str> {
    let mut lines_with_score: Vec<(ScoreType, &'a str)> = lines
        .iter()
        .filter_map(|&s| matcher.fuzzy_match(s, pattern).map(|score| (score, s)))
        .collect();
    lines_with_score.sort_by_key(|(score, _)| -score);
    lines_with_score
        .into_iter()
        .map(|(_, string)| string)
        .collect()
}

#[allow(dead_code)]
pub fn wrap_matches(line: &str, indices: &[IndexType]) -> String {    
    line
        .chars()
        .enumerate()
        .zip(indices.iter())
        .fold(String::new(),|mut ret, ((idx, ch), idx_type)| {
            let next_id = idx_type;
            
            if *next_id as usize == idx {
                ret.push_str(format!("[{}]", ch).as_str())
            } else {
                ret.push(ch)
            }

            ret
        })
}
