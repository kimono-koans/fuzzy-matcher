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
    choice_len: usize,
    pattern_len: usize,
    case_sensitive: bool,
    is_ascii: bool,
}

impl<'a> SimpleMatch<'a> {
    fn new(choice: &'a str, pattern: &'a str, matcher: &'a SimpleMatcher) -> Self {
        let case_sensitive = matcher.is_case_sensitive(pattern);
        let choice_len = choice.chars().count();
        let pattern_len = pattern.chars().count();
        let is_ascii = choice_len == choice.len() && pattern_len == pattern.len();

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

        let matches = if self.is_ascii {
            ByteMatching::fuzzy(self)?
        } else {
            CharMatching::fuzzy(self)?
        };

        let start_idx = *matches.first()?;
        let end_idx = *matches.last()?;

        let score = self.score(start_idx, end_idx);

        if score >= BASELINE {
            return Some((score, matches));
        }

        None
    }

    fn score(&self, start_idx: usize, end_idx: usize) -> i64 {
        // imagine pattern.len() = 1, but abs_diff is zero
        let closeness = start_idx.abs_diff(end_idx) - self.pattern_len + 1;

        let closeness_score = if closeness == 0 {
            10_000_000
        } else if closeness >= 4 {
            0
        } else {
            2_000_000 / closeness
        };

        let start_idx_bonus = if start_idx == 0 {
            200_000
        } else if start_idx <= 4 {
            100_000 / start_idx
        } else {
            0
        };

        let first_letter_case_bonus = if self.first_letter_uppercase(start_idx) {
            200_000
        } else {
            0
        };

        let choice_len_neg_bonus = 1_000 * self.choice_len;

        (closeness_score + start_idx_bonus - choice_len_neg_bonus + first_letter_case_bonus) as i64
    }

    #[inline]
    fn first_letter_uppercase(&self, start_idx: usize) -> bool {
        let pattern_first_letter = self.pattern.chars().nth(0).unwrap();
        let choice_first_letter = self.choice.chars().nth(start_idx).unwrap();

        if !pattern_first_letter.is_ascii() {
            return pattern_first_letter.is_uppercase() && choice_first_letter.is_uppercase();
        }

        pattern_first_letter.is_ascii_uppercase() && choice_first_letter.is_ascii_uppercase()
    }
}

struct CharMatching;

impl CharMatching {
    fn fuzzy(request: &SimpleMatch) -> Option<Vec<usize>> {
        let mut matches = Self::forward_matches(request)?;

        let mut start_idx = *matches.first()?;
        let end_idx = *matches.last()?;

        Self::reverse_matches(request, &mut start_idx, end_idx, &mut matches);
        Some(matches)
    }

    fn forward_matches(request: &SimpleMatch) -> Option<Vec<usize>> {
        let mut skip = 0usize;

        let mut pattern_indices: Vec<usize> = Vec::with_capacity(request.pattern_len);

        for p_char in request.pattern.chars() {
            match request
                .choice
                .char_indices()
                .skip(skip)
                .find_map(|(idx, c_char)| {
                    if Self::char_equal(request, p_char, c_char) {
                        skip = idx;
                        return Some(idx);
                    }

                    None
                }) {
                Some(char_idx) => pattern_indices.push(char_idx),
                None => return None,
            }
        }

        assert!(pattern_indices.len() == request.pattern_len);

        Some(pattern_indices)
    }

    fn reverse_matches(
        request: &SimpleMatch,
        start_idx: &mut usize,
        end_idx: usize,
        matches: &mut Vec<usize>,
    ) {
        let idx_abs_diff = start_idx.abs_diff(end_idx);

        if idx_abs_diff == 0 {
            return;
        }

        let mut skip = 0usize;

        let mut pattern_indices: Vec<usize> = Vec::with_capacity(request.pattern_len);

        for p_char in request.pattern.chars().rev() {
            match request
                .choice
                .char_indices()
                .rev()
                .skip(skip)
                .find_map(|(idx, c_char)| {
                    if Self::char_equal(request, p_char, c_char) {
                        skip = idx;
                        return Some(idx);
                    }

                    None
                }) {
                Some(char_idx) => pattern_indices.push(char_idx),
                None => return,
            }
        }

        assert!(pattern_indices.len() == request.pattern_len);
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
    fn char_equal(request: &SimpleMatch, a: char, b: char) -> bool {
        if !request.case_sensitive {
            if !a.is_ascii() && !b.is_ascii() {
                return a.to_lowercase().cmp(b.to_lowercase()) == Ordering::Equal;
            }
            return a.eq_ignore_ascii_case(&b);
        }

        a == b
    }
}

struct ByteMatching;

impl ByteMatching {
    fn fuzzy(request: &SimpleMatch) -> Option<Vec<usize>> {
        let mut matches = Self::forward_matches(request)?;

        let mut start_idx = *matches.first()?;
        let end_idx = *matches.last()?;

        Self::reverse_matches(request, &mut start_idx, end_idx, &mut matches);
        Some(matches)
    }

    fn forward_matches(request: &SimpleMatch) -> Option<Vec<usize>> {
        let mut skip = 0usize;

        let mut pattern_indices: Vec<usize> = Vec::with_capacity(request.pattern_len);

        for p_char in request.pattern.bytes() {
            match request
                .choice
                .bytes()
                .enumerate()
                .skip(skip)
                .find_map(|(idx, c_char)| {
                    if Self::byte_equal(request, p_char, c_char) {
                        skip = idx;
                        return Some(idx);
                    }

                    None
                }) {
                Some(char_idx) => pattern_indices.push(char_idx),
                None => return None,
            }
        }

        assert!(pattern_indices.len() == request.pattern_len);

        Some(pattern_indices)
    }

    fn reverse_matches(
        request: &SimpleMatch,
        start_idx: &mut usize,
        end_idx: usize,
        matches: &mut Vec<usize>,
    ) {
        let idx_abs_diff = start_idx.abs_diff(end_idx);

        if idx_abs_diff == 0 {
            return;
        }

        let mut skip = 0usize;

        let mut pattern_indices: Vec<usize> = Vec::with_capacity(request.pattern_len);

        for p_char in request.pattern.bytes().rev() {
            match request
                .choice
                .bytes()
                .enumerate()
                .rev()
                .skip(skip)
                .find_map(|(idx, c_char)| {
                    if Self::byte_equal(request, p_char, c_char) {
                        skip = idx;
                        return Some(idx);
                    }

                    None
                }) {
                Some(char_idx) => pattern_indices.push(char_idx),
                None => return,
            }
        }

        assert!(pattern_indices.len() == request.pattern_len);
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
    fn byte_equal(request: &SimpleMatch, a: u8, b: u8) -> bool {
        if !request.case_sensitive {
            return a.eq_ignore_ascii_case(&b);
        }

        a == b
    }
}
