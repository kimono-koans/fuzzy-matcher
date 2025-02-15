use std::cell::RefCell;

use crate::FuzzyMatcher;
use crate::IndexType;
use crate::ScoreType;

const BASELINE: i64 = 0i64;

thread_local! {
    static FORWARD: RefCell<Vec<usize>> = RefCell::new(Vec::new());
    static REVERSE: RefCell<Vec<usize>> = RefCell::new(Vec::new());
}

impl FuzzyMatcher for SimpleMatcher {
    fn fuzzy_indices(&self, choice: &str, pattern: &str) -> Option<(ScoreType, Vec<IndexType>)> {
        self.fuzzy(choice, pattern)
    }

    fn fuzzy_match(&self, choice: &str, pattern: &str) -> Option<ScoreType> {
        self.fuzzy(choice, pattern).map(|(score, _)| score)
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
enum CaseMatching {
    Respect,
    Ignore,
    Smart,
}

pub struct SimpleMatcher {
    case: CaseMatching,
}

impl Default for SimpleMatcher {
    fn default() -> Self {
        SimpleMatcher {
            case: CaseMatching::Smart,
        }
    }
}

impl SimpleMatcher {
    fn fuzzy(&self, choice: &str, pattern: &str) -> Option<(ScoreType, Vec<IndexType>)> {
        let new_match = SimpleMatch::new(choice, pattern, self);
        new_match.fuzzy()
    }

    pub fn ignore_case(mut self) -> Self {
        self.case = CaseMatching::Ignore;
        self
    }

    pub fn smart_case(mut self) -> Self {
        self.case = CaseMatching::Smart;
        self
    }

    pub fn respect_case(mut self) -> Self {
        self.case = CaseMatching::Respect;
        self
    }

    fn contains_upper(&self, string: &str) -> bool {
        string.bytes().any(|b| b.is_ascii_uppercase())
    }

    fn is_case_sensitive(&self, pattern: &str) -> bool {
        match self.case {
            CaseMatching::Respect => true,
            CaseMatching::Ignore => false,
            CaseMatching::Smart => self.contains_upper(pattern),
        }
    }
}

struct SimpleMatch<'a> {
    choice: &'a str,
    pattern: &'a str,
    choice_len: usize,
    pattern_len: usize,
    case_sensitive: bool,
    is_ascii: bool,
}

impl<'a> SimpleMatch<'a> {
    fn new(choice: &'a str, pattern: &'a str, matcher: &'a SimpleMatcher) -> Self {
        let case_sensitive = matcher.is_case_sensitive(pattern);
        let mut choice_len = choice.len();
        let mut pattern_len = pattern.len();

        let is_ascii = choice.is_ascii() && pattern.is_ascii();
        if !is_ascii {
            choice_len = choice.chars().count();
            pattern_len = pattern.chars().count();
        }

        Self {
            choice,
            pattern,
            choice_len,
            pattern_len,
            case_sensitive,
            is_ascii,
        }
    }

    fn fuzzy(&self) -> Option<(ScoreType, Vec<IndexType>)> {
        if self.pattern_len == 0 {
            return Some((0, Vec::new()));
        }

        if self.choice_len == 0 || self.pattern_len > self.choice_len {
            return None;
        }

        FORWARD.with_borrow_mut(|mut pattern_indices| {
            pattern_indices.clear();

            self.forward_matches(pattern_indices)?;

            let forward_closeness = self.closeness(&pattern_indices);

            if forward_closeness != 0 {
                self.reverse_matches(&mut pattern_indices, forward_closeness)
            }

            if self.pattern_len > 3 && Self::none_consecutive(&pattern_indices) {
                return None;
            }

            let score = self.score(&pattern_indices);

            if score >= BASELINE {
                return Some((score, FORWARD.replace(Vec::with_capacity(self.pattern_len))));
            }

            None
        })
    }

    fn closeness(&self, matches: &[usize]) -> usize {
        let matches_len = matches.len();

        let start_idx = *matches.first().unwrap_or(&0);
        let end_idx = *matches.last().unwrap_or(&0);

        self.pattern_len.abs_diff(matches_len)
            + self.pattern_len.abs_diff(end_idx.abs_diff(start_idx) + 1)
    }

    fn none_consecutive(matches: &[usize]) -> bool {
        matches.iter().enumerate().all(|(idx, val)| {
            let next_proposed = Some(val + &1);
            let next_actual = matches.get(idx + 1);

            next_actual != next_proposed.as_ref()
        })
    }

    fn score(&self, matches: &[usize]) -> i64 {
        let start_idx = matches.first().unwrap_or(&0);

        let closeness_score = 1_048_576 - (self.closeness(matches) * 32_768);

        let start_idx_bonus = if let Some((first_alpha_idx, _)) = self
            .choice
            .bytes()
            .enumerate()
            .filter(|(_idx, c_char)| !c_char.is_ascii_alphabetic())
            .next()
        {
            if &first_alpha_idx == start_idx {
                32_768
            } else {
                0
            }
        } else {
            0
        };

        let first_letter_case_bonus = if self.first_letter_uppercase(start_idx) {
            16_384
        } else {
            0
        };

        let word_boundary_bonus = self.word_boundary(matches) * 16_384;

        let follows_special_char_bonus = self.follows_special_char(matches) * 4_096;

        let len_neg = self.choice_len * 16;

        (closeness_score
            + start_idx_bonus
            + follows_special_char_bonus
            + word_boundary_bonus
            + first_letter_case_bonus
            - len_neg
            - 131_072) as i64
    }

    fn forward_matches(&self, pattern_indices: &mut Vec<usize>) -> Option<()> {
        self.forward(pattern_indices);

        if pattern_indices.is_empty() {
            return None;
        }

        if pattern_indices.len() + 2 <= self.pattern_len {
            return None;
        }

        Some(())
    }

    fn reverse_matches(&self, matches: &mut Vec<usize>, forward_closeness: usize) {
        REVERSE.with_borrow_mut(|mut pattern_indices| {
            pattern_indices.shrink_to(self.pattern_len);
            pattern_indices.clear();

            self.reverse(&mut pattern_indices);

            let reverse_closeness = self.closeness(&pattern_indices);

            if reverse_closeness < forward_closeness {
                *matches = REVERSE.replace(Vec::with_capacity(self.pattern_len));
            }
        })
    }

    #[inline]
    fn word_boundary(&self, matches: &[usize]) -> usize {
        matches
            .iter()
            .filter(|idx| {
                if idx == &&0 {
                    return true;
                }

                let previous = *idx - 1;

                self.choice
                    .bytes()
                    .enumerate()
                    .nth(previous)
                    .map(|(idx, b)| self.choice.is_char_boundary(idx) && b == b'\t' || b == b' ')
                    .unwrap_or(false)
            })
            .count()
    }

    #[inline]
    fn follows_special_char(&self, matches: &[usize]) -> usize {
        matches
            .iter()
            .map(|idx| {
                let previous = idx - 1;

                if previous <= 0 {
                    return None;
                }

                self.choice
                    .bytes()
                    .enumerate()
                    .nth(previous)
                    .map(|(idx, b)| {
                        self.choice.is_char_boundary(idx) && b == b'\t'
                            || b == b'/'
                            || b == b':'
                            || b == b'-'
                            || b == b'_'
                            || b == b' '
                    })
            })
            .count()
    }

    #[inline]
    fn first_letter_uppercase(&self, start_idx: &usize) -> bool {
        self.pattern.bytes().nth(0).unwrap().is_ascii_uppercase()
            && self
                .choice
                .bytes()
                .nth(*start_idx)
                .unwrap()
                .is_ascii_uppercase()
    }
}

pub trait Matching {
    fn forward(&self, pattern_indices: &mut Vec<usize>);
    fn reverse(&self, pattern_indices: &mut Vec<usize>);
    fn char_equal(&self, a: &char, b: &char) -> bool;
    fn byte_equal(&self, a: &u8, b: &u8) -> bool;
}

impl<'a> Matching for SimpleMatch<'a> {
    fn forward(&self, pattern_indices: &mut Vec<usize>) {
        if self.is_ascii {
            let mut choice_iter = self.choice.bytes().enumerate();

            for p_char in self.pattern.bytes() {
                match choice_iter.find_map(|(idx, c_char)| {
                    if self.byte_equal(&p_char, &c_char) {
                        return Some(idx);
                    }

                    None
                }) {
                    Some(char_idx) => pattern_indices.push(char_idx),
                    None => continue,
                }
            }
        } else {
            let mut choice_iter = self.choice.char_indices();

            for p_char in self.pattern.chars() {
                match choice_iter.find_map(|(idx, c_char)| {
                    if self.char_equal(&p_char, &c_char) {
                        return Some(idx);
                    }

                    None
                }) {
                    Some(char_idx) => pattern_indices.push(char_idx),
                    None => continue,
                }
            }
        }
    }

    fn reverse(&self, pattern_indices: &mut Vec<usize>) {
        if self.is_ascii {
            let mut choice_iter = self.choice.bytes().enumerate().rev();

            for p_char in self.pattern.bytes().rev() {
                match choice_iter.find_map(|(idx, c_char)| {
                    if self.byte_equal(&p_char, &c_char) {
                        return Some(idx);
                    }

                    None
                }) {
                    Some(char_idx) => pattern_indices.push(char_idx),
                    None => continue,
                }
            }
        } else {
            let mut choice_iter = self.choice.char_indices().rev();

            for p_char in self.pattern.chars().rev() {
                match choice_iter.find_map(|(idx, c_char)| {
                    if self.char_equal(&p_char, &c_char) {
                        return Some(idx);
                    }

                    None
                }) {
                    Some(char_idx) => pattern_indices.push(char_idx),
                    None => continue,
                }
            }
        }

        pattern_indices.reverse();
    }

    #[inline]
    fn char_equal(&self, a: &char, b: &char) -> bool {
        if !self.case_sensitive {
            return a.to_lowercase().eq(b.to_lowercase());
        }

        a == b
    }

    #[inline]
    fn byte_equal(&self, a: &u8, b: &u8) -> bool {
        if !self.case_sensitive {
            return a.eq_ignore_ascii_case(&b);
        }

        a == b
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_reverse() {
        let matcher = SimpleMatcher::default();
        assert_eq!(
            Some(vec![7, 8, 9, 10]),
            matcher
                .fuzzy_indices("bullsh shit\n", "shit")
                .map(|inner| inner.1)
        );
    }

    #[test]
    fn test_simple_double_reverse() {
        let matcher = SimpleMatcher::default();
        assert_eq!(
            Some(vec![10, 11, 12, 13]),
            matcher
                .fuzzy_indices("bullsh it shit\n", "shit")
                .map(|inner| inner.1)
        );
    }

    #[test]
    fn test_simple_non_consecutive() {
        let matcher = SimpleMatcher::default();
        assert_eq!(None, matcher.fuzzy_indices("bsuhlilt\n", "shit"));
    }
}
