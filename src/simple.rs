use crate::FuzzyMatcher;
use crate::IndexType;
use crate::ScoreType;
use std::cmp::Ordering;

const BASELINE: i64 = 200_000;

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
        string.bytes().any(|ch| ch.is_ascii_uppercase())
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

        let mut matches = self.forward_matches()?;

        self.reverse_matches(&mut matches);

        let score = self.score(&matches);

        if score >= BASELINE {
            return Some((score, matches));
        }

        None
    }

    fn score(&self, matches: &[usize]) -> i64 {
        let start_idx = *matches.first().unwrap_or(&0);
        let end_idx = *matches.last().unwrap_or(&0);

        let closeness = end_idx - start_idx;

        let closeness_score = if closeness == 0 {
            10_000_000
        } else if closeness >= 4 {
            0
        } else {
            1_000_000 / closeness
        };

        let pat_contains_non_alpha = self.pattern.contains(|c: char| !c.is_alphabetic());

        let first_alpha_char = if !pat_contains_non_alpha {
            self.choice
                .find(|c: char| c.is_alphabetic())
                .unwrap_or(start_idx)
        } else {
            start_idx
        };

        let start_idx_bonus = if first_alpha_char == 0 {
            200_000
        } else {
            200_000 / start_idx
        };

        let first_letter_case_bonus = if self.first_letter_uppercase(start_idx) {
            20_000
        } else {
            0
        };

        let follows_special_char_bonus = self.follows_special_char(matches) * 10_000;

        (closeness_score + start_idx_bonus + first_letter_case_bonus + follows_special_char_bonus)
            as i64
    }

    fn forward_matches(&self) -> Option<Vec<usize>> {
        let mut pattern_indices: Vec<usize> = Vec::with_capacity(self.pattern_len);

        if self.is_ascii {
            ByteMatching::from(self).forward(&mut pattern_indices)
        } else {
            CharMatching::from(self).forward(&mut pattern_indices)
        }

        if pattern_indices.len() != self.pattern_len {
            return None;
        }

        Some(pattern_indices)
    }

    fn reverse_matches(&self, matches: &mut Vec<usize>) {
        let start_idx = *matches.first().unwrap_or(&0);
        let end_idx = *matches.last().unwrap_or(&0);

        let idx_abs_diff = end_idx - start_idx;

        if idx_abs_diff == 0 {
            return;
        }

        let mut pattern_indices: Vec<usize> = Vec::with_capacity(self.pattern_len);

        let new_diff = if self.is_ascii {
            ByteMatching::from(self).reverse(&mut pattern_indices, idx_abs_diff)
        } else {
            CharMatching::from(self).reverse(&mut pattern_indices, idx_abs_diff)
        };

        if idx_abs_diff > new_diff {
            pattern_indices.reverse();
            *matches = pattern_indices;
        }
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
                    .nth(previous)
                    .map(|b| b == b'/' || b == b':' || b == b'-' || b == b'_' || b == b' ')
            })
            .count()
    }

    #[inline]
    fn first_letter_uppercase(&self, start_idx: usize) -> bool {
        self.pattern.bytes().nth(0).unwrap().is_ascii_uppercase()
            && self
                .choice
                .bytes()
                .nth(start_idx)
                .unwrap()
                .is_ascii_uppercase()
    }
}

struct CharMatching<'a> {
    inner: &'a SimpleMatch<'a>,
}

impl<'a> From<&'a SimpleMatch<'a>> for CharMatching<'a> {
    fn from(value: &'a SimpleMatch) -> Self {
        Self { inner: value }
    }
}

impl<'a> CharMatching<'a> {
    fn forward(&self, pattern_indices: &mut Vec<usize>) {
        let mut skip = 0usize;

        for p_char in self.inner.pattern.chars() {
            match self
                .inner
                .choice
                .char_indices()
                .skip(skip)
                .find_map(|(idx, c_char)| {
                    if self.char_equal(p_char, c_char) {
                        skip = idx;
                        return Some(idx);
                    }

                    None
                }) {
                Some(char_idx) => pattern_indices.push(char_idx),
                None => return,
            }
        }
    }
    fn reverse(&self, pattern_indices: &mut Vec<usize>, idx_abs_diff: usize) -> usize {
        let mut skip = 0usize;
        let mut new_diff = 0usize;

        for p_char in self.inner.pattern.chars().rev() {
            new_diff = pattern_indices.first().unwrap_or(&0usize)
                - pattern_indices.last().unwrap_or(&0usize);

            if new_diff > idx_abs_diff {
                return new_diff;
            }

            match self
                .inner
                .choice
                .char_indices()
                .rev()
                .skip(skip)
                .find_map(|(idx, c_char)| {
                    if self.char_equal(p_char, c_char) {
                        skip = idx;
                        return Some(idx);
                    }

                    None
                }) {
                Some(char_idx) => pattern_indices.push(char_idx),
                None => return new_diff,
            }
        }

        new_diff
    }

    #[inline]
    fn char_equal(&self, a: char, b: char) -> bool {
        if !self.inner.case_sensitive {
            if !a.is_ascii() && !b.is_ascii() {
                return a.to_lowercase().cmp(b.to_lowercase()) == Ordering::Equal;
            }
            return a.eq_ignore_ascii_case(&b);
        }

        a == b
    }
}

struct ByteMatching<'a> {
    inner: &'a SimpleMatch<'a>,
}

impl<'a> From<&'a SimpleMatch<'a>> for ByteMatching<'a> {
    fn from(value: &'a SimpleMatch) -> Self {
        Self { inner: value }
    }
}

impl<'a> ByteMatching<'a> {
    fn forward(&self, pattern_indices: &mut Vec<usize>) {
        let mut skip = 0usize;

        for p_char in self.inner.pattern.bytes() {
            match self
                .inner
                .choice
                .bytes()
                .enumerate()
                .skip(skip)
                .find_map(|(idx, c_char)| {
                    if self.byte_equal(p_char, c_char) {
                        skip = idx;
                        return Some(idx);
                    }

                    None
                }) {
                Some(char_idx) => pattern_indices.push(char_idx),
                None => return,
            }
        }
    }
    fn reverse(&self, pattern_indices: &mut Vec<usize>, idx_abs_diff: usize) -> usize {
        let mut skip = 0usize;

        let mut new_diff = 0usize;

        for p_char in self.inner.pattern.bytes().rev() {
            new_diff = pattern_indices.last().unwrap_or(&0usize)
                - pattern_indices.first().unwrap_or(&0usize);

            if new_diff > idx_abs_diff {
                return new_diff;
            }

            match self
                .inner
                .choice
                .bytes()
                .enumerate()
                .rev()
                .skip(skip)
                .find_map(|(idx, c_char)| {
                    if self.byte_equal(p_char, c_char) {
                        skip = idx;
                        return Some(idx);
                    }

                    None
                }) {
                Some(char_idx) => pattern_indices.push(char_idx),
                None => return new_diff,
            }
        }
        new_diff
    }

    #[inline]
    fn byte_equal(&self, a: u8, b: u8) -> bool {
        if !self.inner.case_sensitive {
            return a.eq_ignore_ascii_case(&b);
        }

        a == b
    }
}
