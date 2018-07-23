use failure::{err_msg, Error};
use rayon::iter::repeatn;
use rayon::prelude::*;
use std;
use std::collections::{HashMap, VecDeque};
use std::convert::*;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader};
use std::iter;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::time::Instant;

#[derive(PartialEq, Debug)]
pub enum SynsetType {
    Noun,
    Verb,
    Adjective,
    AdjectiveSatellite,
    Adverb,
}

impl<'a> TryFrom<&'a str> for SynsetType {
    type Error = Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s {
            "n" => Ok(SynsetType::Noun),
            "v" => Ok(SynsetType::Verb),
            "a" => Ok(SynsetType::Adjective),
            "s" => Ok(SynsetType::AdjectiveSatellite),
            "r" => Ok(SynsetType::Adverb),
            _ => Err(err_msg("not a valid value")),
        }
    }
}

#[derive(PartialEq, Debug)]
enum NounSymbol {
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

impl<'a> TryFrom<&'a str> for NounSymbol {
    type Error = Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s {
            "!" => Ok(NounSymbol::Antonym),
            "@" => Ok(NounSymbol::Hypernym),
            "@i" => Ok(NounSymbol::InstanceHypernym),
            "~" => Ok(NounSymbol::Hyponym),
            "~i" => Ok(NounSymbol::InstanceHypernym),
            "#m" => Ok(NounSymbol::MemberHolonym),
            "#s" => Ok(NounSymbol::SubstanceHolonym),
            "#p" => Ok(NounSymbol::PartHolonym),
            "%m" => Ok(NounSymbol::MemberMeronym),
            "%s" => Ok(NounSymbol::SubstanceMeronym),
            "%p" => Ok(NounSymbol::PartMeronym),
            "=" => Ok(NounSymbol::Attribute),
            "+" => Ok(NounSymbol::DerivationallyRelated),
            ";c" => Ok(NounSymbol::DomainOfTopic),
            "-c" => Ok(NounSymbol::MemberOfTopic),
            ";r" => Ok(NounSymbol::DomainOfRegion),
            "-r" => Ok(NounSymbol::MemberOfRegion),
            ";u" => Ok(NounSymbol::DomainOfUsage),
            "-u" => Ok(NounSymbol::MemberOfUsage),
            _ => Err(err_msg("not a valid value")),
        }
    }
}

#[derive(PartialEq, Debug)]
struct SynsetPointer {
    pointer_symbol: NounSymbol,
    synset_offset: usize,
    pos: SynsetType,
    source_target: usize,
}

#[derive(PartialEq, Debug)]
struct Word {
    word: String,
    lex_id: usize,
}

#[derive(PartialEq, Debug)]
struct Synset {
    synset_offset: usize,
    lex_filenum: usize,
    ss_type: SynsetType,
    w_cnt: usize,
    word: Vec<Word>,
    p_cnt: usize,
    ptr: Vec<SynsetPointer>,
    gloss: String,
}

impl Synset {
    fn split_gloss(data: String) -> Result<(String, String), Error> {
        let res: Vec<&str> = data.rsplit("|").collect();
        let gloss = res.get(0).ok_or(err_msg("could not find gloss"))?;
        let other = res.get(1).ok_or(err_msg("could not find synset_str"))?;
        return Ok((other.trim().to_owned(), gloss.trim().to_owned()));
    }
    fn get_offset(&self) -> usize {
        return self.synset_offset;
    }
    fn parse_words_from_token_list(
        count: usize,
        token_list: &mut VecDeque<&str>,
    ) -> Result<Vec<Word>, Error> {
        let mut res = vec![];
        for _ in 0..count {
            let word = token_list
                .pop_front()
                .ok_or(err_msg("could not find word"))?;
            let lex_id = token_list
                .pop_front()
                .ok_or(err_msg("could not find lex_id"))?
                .parse::<usize>()?;
            res.push(Word {
                word: word.to_owned(),
                lex_id,
            });
        }
        Ok(res)
    }

    fn parse_ptrs_from_token_list(
        count: usize,
        token_list: &mut VecDeque<&str>,
    ) -> Result<Vec<SynsetPointer>, Error> {
        let mut res = vec![];
        
        for _ in 0..count {
            let pointer_symbol: NounSymbol = token_list
                .pop_front()
                .ok_or(err_msg("could not find pointer_symbol"))?
                .try_into()?;
            let synset_offset = token_list
                .pop_front()
                .ok_or(err_msg("could not find synset_offset"))?
                .parse::<usize>()?;
            let pos: SynsetType = token_list
                .pop_front()
                .ok_or(err_msg("could not find ss_type"))?
                .try_into()?;
            let source_target = token_list
                .pop_front()
                .ok_or(err_msg("could not find synset_offset"))?
                .parse::<usize>()?;
            res.push(SynsetPointer {
                pointer_symbol,
                synset_offset,
                pos,
                source_target,
            });
        }
        Ok(res)
    }

    pub fn parse_sysset_from_line(data: String) -> Result<Synset, Error> {
        let (synset_str, gloss) = Self::split_gloss(data)?;
        let mut synset_token_list: VecDeque<&str> = synset_str.split_whitespace().collect();

        let synset_offset = synset_token_list
            .pop_front()
            .ok_or(err_msg("could not find synset_offset"))?
            .parse::<usize>()?;
        let lex_filenum = synset_token_list
            .pop_front()
            .ok_or(err_msg("could not find lex_filenum"))?
            .parse::<usize>()?;
        let ss_type: SynsetType = synset_token_list
            .pop_front()
            .ok_or(err_msg("could not find ss_type"))?
            .try_into()?;

        let w_cnt = synset_token_list
            .pop_front()
            .ok_or(err_msg("could not find w_cnt"))?
            .parse::<usize>()?;
        let word = Self::parse_words_from_token_list(w_cnt, &mut synset_token_list)?;
        let p_cnt = synset_token_list
            .pop_front()
            .ok_or(err_msg("could not find p_cnt"))?
            .parse::<usize>()?;
        let start = Instant::now();
        let ptr = Self::parse_ptrs_from_token_list(p_cnt, &mut synset_token_list)?;
        let end = Instant::now();
        println!("peer {:?}", end - start);
        Ok(Synset {
            synset_offset,
            lex_filenum,
            ss_type,
            w_cnt,
            word,
            p_cnt,
            ptr,
            gloss,
        })
    }
}

struct WordNet {
    synsets: HashMap<usize, Synset>,
}
impl WordNet {
    pub fn new() -> Self {
        Self {
            synsets: HashMap::default(),
        }
    }
    fn from_data_file<P: AsRef<Path>>(path: P) -> Result<Self, Error> {
        let f = File::open(path)?;
        let f = BufReader::new(f);
        let mut data = HashMap::default();
        let start = Instant::now();
        let lines: Vec<String> = f
            .lines()
            .filter_map(|l| l.ok())
            .filter(|l| !l.starts_with("  "))
            .collect();

        let read = Instant::now();
        println!("read {:?} read spend {:?} ", read, read - start);
        let synsets: Vec<Synset> = lines
            .par_iter()
            .filter_map(|line| Synset::parse_sysset_from_line(line.to_string()).ok())
            .collect();
        let parse = Instant::now();

        println!("parse {:?}", parse);
        println!("parse spend {:?}", parse - read);

        synsets.into_iter().for_each(|synset| {
            // println!("offset {} {:?}", synset.get_offset(), synset.word);
            data.insert(synset.get_offset(), synset);
        });
        return Ok(Self { synsets: data });
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_split_gloss() {
        let noun_synset_raw=r#"00006269 03 n 01 life 0 002 @ 00004258 n 0000 ~ 08010218 n 0000 | living things collectively; "the oceans are teeming with life""#;
        assert_eq!(
            Synset::split_gloss(noun_synset_raw.to_owned()).unwrap(),
            (
                "00006269 03 n 01 life 0 002 @ 00004258 n 0000 ~ 08010218 n 0000".to_owned(),
                r#"living things collectively; "the oceans are teeming with life""#.to_owned()
            )
        );
    }

    #[test]
    fn test_parse_synsets() {
        let noun_synset_raw=r#"00006269 03 n 01 life 0 002 @ 00004258 n 0000 ~ 08010218 n 0000 | living things collectively; "the oceans are teeming with life""#;
        let except = Synset {
            synset_offset: 00006269,
            lex_filenum: 03,
            ss_type: SynsetType::Noun,
            w_cnt: 01,

            word: vec![Word {
                word: "life".to_owned(),
                lex_id: 0,
            }],
            p_cnt: 002,
            ptr: vec![
                SynsetPointer {
                    pointer_symbol: NounSymbol::Hypernym,
                    synset_offset: 00004258,
                    pos: SynsetType::Noun,
                    source_target: 0000,
                },
                SynsetPointer {
                    pointer_symbol: NounSymbol::Hyponym,
                    synset_offset: 08010218,
                    pos: SynsetType::Noun,
                    source_target: 0000,
                },
            ],
            gloss: r#"living things collectively; "the oceans are teeming with life""#.to_owned(),
        };
        let prepared = Synset::parse_sysset_from_line(noun_synset_raw.to_owned()).unwrap();
        assert_eq!(prepared, except);
    }

    #[test]
    fn test_parse_file() {
        let start = Instant::now();
        WordNet::from_data_file("/home/oaa/Downloads/dict/data.noun");
        let end = Instant::now();
        println!("{:?}", end - start);
    }
}
