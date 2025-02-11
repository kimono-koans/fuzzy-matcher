use crate::FuzzyMatcher;
use crate::IndexType;
use crate::ScoreType;

const BASELINE: i64 = 0i64;

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
        if string.is_ascii() {
            return string.bytes().any(|b| b.is_ascii_uppercase());
        }

        string.chars().any(|b| b.is_uppercase())
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
    match_type: MatchType,
    case_sensitive: bool,
}

enum MatchType {
    Bytes(ChoicePatternLen),
    Chars(ChoicePatternLen),
}

impl MatchType {
    fn pattern_len(&self) -> usize {
        match self {
            MatchType::Bytes(inner) => inner.pattern_len,
            MatchType::Chars(inner) => inner.pattern_len,
        }
    }

    fn choice_len(&self) -> usize {
        match self {
            MatchType::Bytes(inner) => inner.choice_len,
            MatchType::Chars(inner) => inner.choice_len,
        }
    }
}

struct ChoicePatternLen {
    pub choice_len: usize,
    pub pattern_len: usize,
}

impl<'a> SimpleMatch<'a> {
    fn new(choice: &'a str, pattern: &'a str, matcher: &'a SimpleMatcher) -> Self {
        let case_sensitive = matcher.is_case_sensitive(pattern);

        let match_type = if choice.is_ascii() && pattern.is_ascii() {
            let choice_len = choice.chars().count();
            let pattern_len = pattern.chars().count();

            MatchType::Bytes(ChoicePatternLen {
                choice_len,
                pattern_len,
            })
        } else {
            let choice_len = choice.len();
            let pattern_len = pattern.len();

            MatchType::Chars(ChoicePatternLen {
                choice_len,
                pattern_len,
            })
        };

        Self {
            choice,
            pattern,
            match_type,
            case_sensitive,
        }
    }

    #[inline(always)]
    fn fuzzy(&self) -> Option<(ScoreType, Vec<IndexType>)> {
        if self.match_type.pattern_len() == 0 {
            return Some((0, Vec::new()));
        }

        if self.match_type.choice_len() == 0
            || self.match_type.pattern_len() > self.match_type.choice_len()
        {
            return None;
        }

        let mut matches = self.forward_matches()?;

        let closeness = self.closeness(&matches);

        if closeness != 0 {
            self.reverse_matches(&mut matches);
        }

        let score = self.score(&matches);

        if score >= BASELINE {
            return Some((score, matches));
        }

        None
    }

    #[inline(always)]
    fn closeness(&self, matches: &[usize]) -> usize {
        let start_idx = *matches.first().unwrap_or(&0);
        let end_idx = *matches.last().unwrap_or(&0);

        self.match_type
            .pattern_len()
            .abs_diff(end_idx.abs_diff(start_idx) + 1)
    }

    #[allow(dead_code)]
    fn none_consecutive(matches: &[usize]) -> bool {
        matches.iter().enumerate().all(|(idx, val)| {
            let next_proposed = Some(val + &1);
            let next_actual = matches.get(idx + 1);

            next_actual != next_proposed.as_ref()
        })
    }

    fn first_alpha_char(&self, start_idx: usize) -> usize {
        let pat_contains_non_alpha = self.pattern.chars().any(|c_char| !c_char.is_alphanumeric());

        let first_alpha_char = if pat_contains_non_alpha {
            match self.match_type {
                MatchType::Bytes(_) => self
                    .choice
                    .bytes()
                    .enumerate()
                    .find_map(|(idx, c_char)| {
                        if c_char.is_ascii_alphanumeric() {
                            return Some(idx);
                        }

                        None
                    })
                    .unwrap_or(start_idx),
                MatchType::Chars(_) => self
                    .choice
                    .char_indices()
                    .find_map(|(idx, c_char)| {
                        if c_char.is_alphanumeric() {
                            return Some(idx);
                        }

                        None
                    })
                    .unwrap_or(start_idx),
            }
        } else {
            start_idx
        };

        first_alpha_char
    }

    #[inline(always)]
    fn score(&self, matches: &[usize]) -> i64 {
        let start_idx = *matches.first().unwrap_or(&0);

        let closeness = self.closeness(matches);

        let closeness_score: i64 = (524_288 - (closeness * 32_768)) as i64;

        let first_alpha_char = self.first_alpha_char(start_idx);

        let start_idx_bonus: i64 = (32_768 - (first_alpha_char * 4_096)) as i64;

        let first_letter_case_bonus: i64 = if self.first_letter_uppercase(start_idx) {
            16_384
        } else {
            0
        };

        let word_boundary_bonus = (self.word_boundary(matches) * 16_384) as i64;

        let follows_special_char_bonus = (self.follows_special_char(matches) * 4_096) as i64;

        let len_neg: i64 = (self.match_type.choice_len() * 8) as i64;

        closeness_score
            + start_idx_bonus
            + first_letter_case_bonus
            + follows_special_char_bonus
            + word_boundary_bonus
            - len_neg
            - 65_536i64
    }

    #[inline(always)]
    fn forward_matches(&self) -> Option<Vec<usize>> {
        self.forward()
    }

    #[inline(always)]
    fn reverse_matches(&self, matches: &mut Vec<usize>) {
        self.reverse(matches)
    }

    #[inline(always)]
    fn word_boundary(&self, matches: &[usize]) -> usize {
        matches
            .iter()
            .filter(|idx| {
                if idx == &&0 {
                    return true;
                }

                let previous = *idx - 1;

                match self.match_type {
                    MatchType::Bytes(_) => self
                        .choice
                        .bytes()
                        .enumerate()
                        .nth(previous)
                        .map(|(idx, b)| {
                            self.choice.is_char_boundary(idx) && b == b'\t' || b == b' '
                        })
                        .unwrap_or(false),
                    MatchType::Chars(_) => self
                        .choice
                        .char_indices()
                        .nth(previous)
                        .map(|(idx, b)| self.choice.is_char_boundary(idx) && b == '\t' || b == ' ')
                        .unwrap_or(false),
                }
            })
            .count()
    }

    #[inline(always)]
    fn follows_special_char(&self, matches: &[usize]) -> usize {
        matches
            .iter()
            .map(|idx| {
                let previous = idx - 1;

                if previous <= 0 {
                    return None;
                }

                match self.match_type {
                    MatchType::Bytes(_) => {
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
                    }
                    MatchType::Chars(_) => {
                        self.choice.char_indices().nth(previous).map(|(idx, b)| {
                            self.choice.is_char_boundary(idx) && b == '\t'
                                || b == '/'
                                || b == ':'
                                || b == '-'
                                || b == '_'
                                || b == ' '
                        })
                    }
                }
            })
            .count()
    }

    #[inline]
    fn first_letter_uppercase(&self, start_idx: usize) -> bool {
        match self.match_type {
            MatchType::Bytes(_) => {
                self.pattern.bytes().nth(0).unwrap().is_ascii_uppercase()
                    && self
                        .choice
                        .bytes()
                        .nth(start_idx)
                        .unwrap()
                        .is_ascii_uppercase()
            }
            MatchType::Chars(_) => {
                self.pattern.chars().nth(0).unwrap().is_uppercase()
                    && self.choice.chars().nth(start_idx).unwrap().is_uppercase()
            }
        }
    }
}

pub trait Matching {
    fn forward(&self) -> Option<Vec<usize>>;
    fn reverse(&self, pattern_indices: &mut Vec<usize>);
    fn char_equal(&self, a: &char, b: &char) -> bool;
    fn byte_equal(&self, a: &u8, b: &u8) -> bool;
}

impl<'a> Matching for SimpleMatch<'a> {
    #[inline(always)]
    fn forward(&self) -> Option<Vec<usize>> {
        match self.match_type {
            MatchType::Bytes(_) => {
                let mut choice_iter = self.choice.bytes().enumerate();

                let mut iter = self.pattern.bytes().filter_map(move |p_char| {
                    choice_iter.find_map(|(idx, c_char)| {
                        if self.byte_equal(&p_char, &c_char) {
                            return Some(idx);
                        }

                        None
                    })
                });

                let count = iter.by_ref().count();

                if count == 0 {
                    return None;
                }

                // give a little flex, 2 chars, to when we bump a pattern for being off
                if count + 2 <= self.match_type.pattern_len() {
                    return None;
                }

                Some(iter.collect())
            }
            MatchType::Chars(_) => {
                let mut choice_iter = self.choice.char_indices();

                let mut iter = self.pattern.chars().filter_map(move |p_char| {
                    choice_iter.find_map(|(idx, c_char)| {
                        if self.char_equal(&p_char, &c_char) {
                            return Some(idx);
                        }

                        None
                    })
                });

                let count = iter.by_ref().count();

                if count == 0 {
                    return None;
                }

                // give a little flex, 2 chars, to when we bump a pattern for being off
                if count + 2 <= self.match_type.pattern_len() {
                    return None;
                }

                Some(iter.collect())
            }
        }
    }

    #[inline(always)]
    fn reverse(&self, pattern_indices: &mut Vec<usize>) {
        let start_idx = *pattern_indices.first().unwrap_or(&0);
        let end_idx = *pattern_indices.last().unwrap_or(&0);

        let diff = end_idx - start_idx + 1;

        if diff == 0 {
            return;
        }

        match self.match_type {
            MatchType::Bytes(_) => {
                let mut choice_iter = self.choice.bytes().enumerate().rev();

                let mut iter = self
                    .pattern
                    .bytes()
                    .rev()
                    .filter_map(move |p_char| {
                        choice_iter.find_map(|(idx, c_char)| {
                            if self.byte_equal(&p_char, &c_char) {
                                return Some(idx);
                            }

                            None
                        })
                    })
                    .rev();

                let first = iter.by_ref().next();
                let last = iter.by_ref().last();

                let reverse_start_idx = first.unwrap_or(0);
                let reverse_end_idx = last.unwrap_or(0);
                let reverse_diff = reverse_end_idx - reverse_start_idx + 1;

                if reverse_diff < diff {
                    *pattern_indices = iter.collect()
                }
            }
            MatchType::Chars(_) => {
                let mut choice_iter = self.choice.char_indices().rev();

                let mut iter = self
                    .pattern
                    .chars()
                    .rev()
                    .filter_map(move |p_char| {
                        choice_iter.find_map(|(idx, c_char)| {
                            if self.char_equal(&p_char, &c_char) {
                                return Some(idx);
                            }

                            None
                        })
                    })
                    .rev();

                let first = iter.by_ref().next();
                let last = iter.by_ref().last();

                let reverse_start_idx = first.unwrap_or(0);
                let reverse_end_idx = last.unwrap_or(0);
                let reverse_diff = reverse_end_idx - reverse_start_idx + 1;

                if reverse_diff < diff {
                    *pattern_indices = iter.collect()
                }
            }
        }
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
}

// fn reverse(&self, pattern_indices: &mut Vec<usize>) {
//     let mut choice_iter = self.inner.choice.char_indices().rev();

//     for p_char in self.inner.pattern.chars().rev() {
//         match choice_iter.find_map(|(idx, c_char)| {3
//             if self.char_equal(p_char, c_char) {
//                 return Some(idx);
//             }

//             None
//         }) {
//             Some(char_idx) => pattern_indices.push(char_idx),
//             None => return,
//         }
//     }
//     pattern_indices.reverse()
// }

// fn reverse(&self, pattern_indices: &mut Vec<usize>) {
//     let mut choice_iter = self.inner.choice.as_bytes().iter().enumerate().rev();

//     for p_char in self.inner.pattern.as_bytes().iter().rev() {
//         match choice_iter.find_map(|(idx, c_char)| {
//             if self.byte_equal(p_char, c_char) && self.inner.choice.is_char_boundary(idx) {
//                 return Some(idx);
//             }

//             None
//         }) {
//             Some(char_idx) => pattern_indices.push(char_idx),
//             None => return,
//         }
//     }
//     pattern_indices.reverse()
// }
