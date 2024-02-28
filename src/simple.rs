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
            case: CaseMatching::Respect,
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
        string.chars().any(|ch| ch.is_ascii_uppercase())
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
    case_sensitive: bool,
    is_ascii: bool,
}

impl<'a> SimpleMatch<'a> {
    fn new(choice: &'a str, pattern: &'a str, matcher: &'a SimpleMatcher) -> Self {
        let case_sensitive = matcher.is_case_sensitive(pattern);
        let is_ascii = pattern.is_ascii() && choice.is_ascii();

        Self {
            choice,
            pattern,
            case_sensitive,
            is_ascii,
        }
    }

    fn fuzzy(&self) -> Option<(ScoreType, Vec<IndexType>)> {
        let choice_len = self.choice.chars().count();
        let pattern_len = self.pattern.chars().count();

        if pattern_len == 0 {
            return Some((0, Vec::new()));
        }

        if choice_len == 0 {
            return None;
        }

        let mut matches = self.forward_matches(pattern_len)?;

        let mut start_idx = *matches.first()?;
        let end_idx = *matches.last()?;

        self.reverse_matches(pattern_len, &mut start_idx, end_idx, &mut matches);

        let score = Self::score(start_idx, end_idx, pattern_len, choice_len);

        if score >= BASELINE {
            return Some((score, matches));
        }

        None
    }

    fn score(start_idx: usize, end_idx: usize, pattern_len: usize, choice_len: usize) -> i64 {
        // imagine pattern.len() = 1, but abs_diff is zero
        let closeness = start_idx.abs_diff(end_idx) - pattern_len + 1;

        let closeness_score = if closeness == 0 {
            10_000_000
        } else if closeness >= 4 {
            0
        } else {
            2_000_000 / closeness
        };

        let first_letter_bonus = if start_idx == 0 {
            200_000
        } else if start_idx <= 4 {
            100_000 / start_idx
        } else {
            0
        };

        let choice_len_neg_bonus = 1_000 * choice_len;

        (closeness_score + first_letter_bonus - choice_len_neg_bonus) as i64
    }

    fn forward_matches(&self, pattern_len: usize) -> Option<Vec<usize>> {
        let mut skip = 0usize;

        let mut pattern_indices: Vec<usize> = Vec::with_capacity(pattern_len);

        for p_char in self.pattern.chars() {
            match self
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
                None => return None,
            }
        }

        assert!(pattern_indices.len() == pattern_len);

        Some(pattern_indices)
    }

    fn reverse_matches(
        &self,
        pattern_len: usize,
        start_idx: &mut usize,
        end_idx: usize,
        matches: &mut Vec<usize>,
    ) {
        let idx_abs_diff = start_idx.abs_diff(end_idx);

        if idx_abs_diff == 0 {
            return;
        }

        let mut skip = 0usize;

        let mut pattern_indices: Vec<usize> = Vec::with_capacity(pattern_len);

        for p_char in self.pattern.chars().rev() {
            match self
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
                None => return,
            }
        }

        assert!(pattern_indices.len() == pattern_len);
        assert!(pattern_indices.len() >= 1);

        let new_diff = pattern_indices.first().unwrap() - pattern_indices.last().unwrap();

        if idx_abs_diff > new_diff {
            pattern_indices.reverse();

            let first = pattern_indices.first().unwrap();

            *start_idx = *first;
            *matches = pattern_indices;
        }
    }

    #[inline]
    pub fn char_equal(&self, a: char, b: char) -> bool {
        if !self.case_sensitive {
            if !self.is_ascii {
                return a.to_lowercase().cmp(b.to_lowercase()) == Ordering::Equal;
            }
            return a.eq_ignore_ascii_case(&b);
        }

        a == b
    }
}
