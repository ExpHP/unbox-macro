use ::std::fmt;
use ::quote::ToTokens;

// Display adapter for ToTokens, since very few syn types impl Display
pub(crate) struct ShowToks<T>(pub(crate) T);

impl<T: ToTokens> fmt::Display for ShowToks<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let mut toks = ::quote::Tokens::new();
        self.0.to_tokens(&mut toks);
        write!(f, "{}", toks)
    }
}

