use crate::{FuzzyMatcher, IndexType, ScoreType};

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
enum CaseMatching {
    Respect,
    Ignore,
    Smart,
}

pub struct BitapMatcher {
    case: CaseMatching,
}

impl Default for BitapMatcher {
    fn default() -> Self {
        Self {
            case: CaseMatching::Ignore,
        }
    }
}

impl BitapMatcher {
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
        for ch in string.chars() {
            if ch.is_ascii_uppercase() {
                return true;
            }
        }

        false
    }

    fn is_case_sensitive(&self, pattern: &str) -> bool {
        match self.case {
            CaseMatching::Respect => true,
            CaseMatching::Ignore => false,
            CaseMatching::Smart => self.contains_upper(pattern),
        }
    }
}

impl FuzzyMatcher for BitapMatcher {
    fn fuzzy_indices(&self, choice: &str, pattern: &str) -> Option<(ScoreType, Vec<IndexType>)> {
        use bitap::Pattern;

        // Compile the pattern you're searching for.
        let bitap_pattern = Pattern::new(pattern).unwrap();

        // Run one of the search functions:
        // - pattern.lev for levenshtein distance matching
        // - pattern.osa for optimal string alignment distance matching
        // The second parameter determines the maximum edit distance
        // that you want to return matches for.
        let max_distance = 4;
        let matches = bitap_pattern.lev(choice, max_distance);

        let res = matches.fold((0i64, Vec::new()), |(mut score, mut loc), m| {
            score += max_distance as i64 - m.distance as i64;
            loc.push(max_distance - m.end);

            (score, loc)
        });

        if res == (0i64, Vec::new()) {
            return None;
        }

        Some(res)
    }

    fn fuzzy_match(&self, choice: &str, pattern: &str) -> Option<ScoreType> {
        Self::fuzzy_indices(self, choice, pattern).map(|i| i.0)
    }
}

/// fuzzy match `line` with `pattern`, returning the score and indices of matches
pub fn fuzzy_indices(line: &str, pattern: &str) -> Option<(ScoreType, Vec<IndexType>)> {
    BitapMatcher::default()
        .ignore_case()
        .fuzzy_indices(line, pattern)
}

/// fuzzy match `line` with `pattern`, returning the score(the larger the better) on match
pub fn fuzzy_match(line: &str, pattern: &str) -> Option<ScoreType> {
    BitapMatcher::default()
        .ignore_case()
        .fuzzy_match(line, pattern)
}
