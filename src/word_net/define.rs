use failure::{err_msg, Error};
use std::convert::TryFrom;

#[derive(PartialEq, Debug)]
pub enum PointerSymbol {
    Antonym,
    Hypernym,
    InstanceHypernym,
    Hyponym,
    MemberHolonym,
    SubstanceHolonym,
    PartHolonym,
    MemberMeronym,
    SubstanceMeronym,
    PartMeronym,
    Attribute,
    DerivationallyRelated,
    DomainOfTopic,
    MemberOfTopic,
    DomainOfRegion,
    MemberOfRegion,
    DomainOfUsage,
    MemberOfUsage,
}

//TODO elegant way to build enum
impl<'a> TryFrom<&'a str> for PointerSymbol {
    type Error = Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s {
            "!" => Ok(PointerSymbol::Antonym),
            "@" => Ok(PointerSymbol::Hypernym),
            "@i" => Ok(PointerSymbol::InstanceHypernym),
            "~" => Ok(PointerSymbol::Hyponym),
            "~i" => Ok(PointerSymbol::InstanceHypernym),
            "#m" => Ok(PointerSymbol::MemberHolonym),
            "#s" => Ok(PointerSymbol::SubstanceHolonym),
            "#p" => Ok(PointerSymbol::PartHolonym),
            "%m" => Ok(PointerSymbol::MemberMeronym),
            "%s" => Ok(PointerSymbol::SubstanceMeronym),
            "%p" => Ok(PointerSymbol::PartMeronym),
            "=" => Ok(PointerSymbol::Attribute),
            "+" => Ok(PointerSymbol::DerivationallyRelated),
            ";c" => Ok(PointerSymbol::DomainOfTopic),
            "-c" => Ok(PointerSymbol::MemberOfTopic),
            ";r" => Ok(PointerSymbol::DomainOfRegion),
            "-r" => Ok(PointerSymbol::MemberOfRegion),
            ";u" => Ok(PointerSymbol::DomainOfUsage),
            "-u" => Ok(PointerSymbol::MemberOfUsage),
            _ => Err(err_msg("not a valid value")),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Gloss {
    pub definition: Vec<String>,
    pub example: Vec<String>,
}

impl<'a> From<&'a str> for Gloss {
    fn from(gloss: &'a str) -> Self {
        let gloss = gloss.trim();
        let mut definition = Vec::new();
        let mut example = Vec::new();
        gloss.split(";").for_each(|item: &str| {
            let item = item.trim();
            if item.starts_with('"') {
                example.push(item.trim_matches('"').to_owned());
            } else {
                definition.push(item.to_owned());
            }
        });
        Gloss {
            definition,
            example,
        }
    }
}

//Part of speech
#[derive(Debug, Eq, PartialEq, EnumString, EnumIter, Hash, Clone, Copy)]
pub enum POS {
    #[strum(serialize = "noun", serialize = "n")]
    Noun,
    #[strum(serialize = "verb", serialize = "v")]
    Verb,
    #[strum(serialize = "adj", serialize = "a")]
    Adj,
    #[strum(serialize = "adjs", serialize = "s")]
    Adjs,
    #[strum(serialize = "adv", serialize = "r")]
    Adv,
}

#[derive(PartialEq, Debug, Clone)]
pub struct SynsetPointer {
    pub pointer_symbol: String,
    pub synset_offset: usize,
    pub pos: POS,
}

#[derive(Debug, Eq, PartialEq, EnumString, EnumIter, Hash, Clone, Copy)]
pub enum FileType {
    #[strum(serialize = "data")]
    Data,
    #[strum(serialize = "index")]
    Index,
}
