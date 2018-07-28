use failure::{err_msg, Error};
use rayon::prelude::*;
use std::collections::{HashMap, VecDeque};
use std::convert::*;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::fs;
use std::ffi::OsStr;

#[derive(PartialEq, Debug)]
enum PointerSymbol {
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
struct SynsetPointer {
    pointer_symbol: String,
    synset_offset: usize,
    pos: SynsetType,
    source_target: usize,
}

#[derive(PartialEq, Debug, Clone)]
struct Word {
    word: String,
    lex_id: usize,
}

#[derive(PartialEq, Debug, Clone)]
struct Synset {
    synset_offset: usize,
    lex_filenum: usize,
    ss_type: SynsetType,
    w_cnt: usize,
    word: Vec<Word>,
    p_cnt: usize,
    ptr: Vec<SynsetPointer>,
    gloss: Gloss,
}

#[derive(PartialEq, Debug, Clone)]
struct SynsetIndex {
    name: String,
    lemma: String,
    pos: POS,
    synset_cnt: usize,
    p_cnt: usize,
    ptr_symbol: Vec<String>,
    sense_cnt: usize,
    tagsense_cnt: usize,
    synset_offset: Vec<usize>,
}

#[derive(PartialEq, Debug, Clone)]
struct Gloss {
    definition: Vec<String>,
    example: Vec<String>,
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
        Gloss { definition, example }
    }
}

#[derive(PartialEq, Debug, Clone)]
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

impl Synset {
    fn split_gloss(data: String) -> Result<(String, String), Error> {
        let res: Vec<&str> = data.rsplit("|").collect();
        let gloss = res.get(0).ok_or(err_msg("could not find gloss"))?;
        let other = res.get(1).ok_or(err_msg("could not find synset_str"))?;
        return Ok((other.trim().to_owned(), gloss.trim().to_owned()));
    }

    fn parse_words_from_token_list(
        count: usize,
        token_list: &mut VecDeque<&str>,
    ) -> Result<Vec<Word>, Error> {
        let chunk_size = 2;
        let data: Vec<&str> = token_list.drain(0..count * chunk_size).collect();

        let items: Vec<&[&str]> = data.chunks(chunk_size).collect();
        let res: Vec<Word> = items.into_par_iter()
            .map(|items: &[&str]| {
                let word = items[0];
                let lex_id = usize::from_str_radix(items[1], 16)?;
                Ok(Word {
                    word: word.to_string(),
                    lex_id: lex_id,
                })
            })
            .filter_map(|r: Result<Word, Error>| r.ok())
            .collect();
        Ok(res)
    }

    fn parse_ptrs_from_token_list(
        count: usize,
        token_list: &mut VecDeque<&str>,
    ) -> Result<Vec<SynsetPointer>, Error> {
        let data: Vec<&str> = token_list.drain(0..count * 4).collect();


        let items: Vec<&[&str]> = data.chunks(4).collect();

        let res: Vec<SynsetPointer> = items.into_par_iter().map(|items: &[&str]| {
            let pointer_symbol = items[0].to_string();
            let synset_offset = items[1].parse::<usize>()?;
            let pos: SynsetType = items[2].try_into()?;
            let source_target = items[3].parse::<usize>()?;
            let pointer = SynsetPointer {
                pointer_symbol,
                synset_offset,
                pos,
                source_target,
            };
            Ok(pointer)
        }).filter_map(|r: Result<SynsetPointer, Error>| r.ok())
            .collect();
        Ok(res)
    }

    pub fn parse_index_from_line(data: String) -> Result<SynsetIndex, Error> {
        let mut index_token_list: VecDeque<&str> = data.split_whitespace().collect();
        let lemma = index_token_list.pop_front().ok_or(err_msg("could not find lemma"))?.to_owned();
        let pos_str = index_token_list.pop_front().ok_or(err_msg("could not find pos"))?;
        let pos = POS::from_str(pos_str)?;
        let synset_cnt: usize = index_token_list.pop_front().ok_or(err_msg("could not find synset_cnt"))?.to_owned().parse()?;
        let p_cnt: usize = index_token_list.pop_front().ok_or(err_msg("could not find synset_cnt"))?.to_owned().parse()?;
        let ptr_symbol: Vec<String> = index_token_list.drain(0..p_cnt).map(String::from).collect();
        let sense_cnt: usize = index_token_list.pop_front().ok_or(err_msg("could not find sense_cnt"))?.to_owned().parse()?;
        let tagsense_cnt: usize = index_token_list.pop_front().ok_or(err_msg("could not find tagsense_cnt"))?.to_owned().parse()?;
        let synset_offset = index_token_list
            .into_iter()
            .map(|offset| offset.parse::<usize>())
            .filter_map(|r| r.ok())
            .collect();
        let name = format!("{}.{}", lemma, pos_str);
        Ok(SynsetIndex {
            name,
            lemma,
            pos,
            synset_cnt,
            p_cnt,
            ptr_symbol,
            sense_cnt,
            tagsense_cnt,
            synset_offset,
        })
    }

    pub fn parse_synset_from_line(data: String) -> Result<Synset, Error> {
        let (synset_str, gloss) = Self::split_gloss(data.clone())?;
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
        let ptr = Self::parse_ptrs_from_token_list(p_cnt, &mut synset_token_list)?;

        Ok(Synset {
            synset_offset,
            lex_filenum,
            ss_type,
            w_cnt,
            word,
            p_cnt,
            ptr,
            gloss: Gloss::from(gloss.as_str()),
        })
    }
}

type PosSynsets = HashMap<POS, HashMap<usize, Box<Synset>>>;
type WordNetIndex = Vec<SynsetIndex>;
type AllSynsets = Vec<Box<Synset>>;

//stupid rust could not self reference in struct
pub struct WordNet {
    #[allow(dead_code)]
    pos_synsets: PosSynsets,
    all_synsets: AllSynsets,
    #[allow(dead_code)]
    index: WordNetIndex,
}

//Part of speech
#[derive(Debug, Eq, PartialEq, EnumString, EnumIter, Hash, Clone, Copy)]
enum POS {
    #[strum(serialize = "noun", serialize = "n")]
    Noun,
    #[strum(serialize = "verb", serialize = "v")]
    Verb,
    #[strum(serialize = "adj", serialize = "a")]
    Adj,
    #[strum(serialize = "adv", serialize = "r")]
    Adv,
}

#[derive(Debug, Eq, PartialEq, EnumString, EnumIter, Hash, Clone, Copy)]
enum FileType {
    #[strum(serialize = "data")]
    Data,
    #[strum(serialize = "index")]
    Index,
}

use std::env;

impl WordNet {
    pub fn new<P: AsRef<OsStr>>(wn_path: Option<P>) -> Result<Self, Error> {
        let wn_path = wn_path.map(|p| PathBuf::from(p.as_ref()))
            .or(env::var_os("WN_PATH").map(PathBuf::from))
            .ok_or(err_msg("could not find the location of wordnet"))
            .and_then(|p| {
                if p.exists() && p.is_dir() {
                    Ok(p)
                } else {
                    Err(err_msg(format!("{:?} not exists or is not a dir", p)))
                }
            })?;

        let (pos_synsets, all_synsets) = Self::build_data(&wn_path)?;
        let index = Self::build_index(&wn_path)?;
        Ok(Self { pos_synsets, all_synsets, index })
    }

    fn tag_pos_to_file(p: PathBuf, expect_file_type: FileType) -> Result<(POS, PathBuf), Error> {
        let file_stem = p
            .file_stem().ok_or(err_msg("could not find file stem"))?
            .to_str().ok_or(err_msg("could not trans stem to string"))?
            .to_owned();
        let extension = p
            .extension().ok_or(err_msg("could not find file extension"))?
            .to_str().ok_or(err_msg("could not trans extension to string"))?
            .to_owned();

        let pos = POS::from_str(&extension);
        let file_type = FileType::from_str(&file_stem);
        match (pos, file_type) {
            (Ok(pos), Ok(file_type)) => {
                if file_type == expect_file_type {
                    return Ok((pos, p));
                } else {
                    return Err(err_msg("not this type"));
                }
            }
            _ => {
                return Err(err_msg(format!("{:?} {:?} {:?} {:?}", pos, p, expect_file_type, file_type)));
            }
        }
    }

    fn build_index<P: AsRef<Path>>(path: P) -> Result<WordNetIndex, Error> {
        let path = PathBuf::from(path.as_ref());
        if !path.exists() {
            return Err(err_msg(format!("could not find path {:?}", path)));
        };
        let pos_synset: Vec<(POS, PathBuf)> = if path.is_dir() {
            fs::read_dir(path)?
                .filter_map(|entry| entry.ok())
                .map(|entry| entry.path())
                .filter_map(|p| {
                    Self::tag_pos_to_file(p, FileType::Index).ok()
                })
                .collect()
        } else {
            if let Ok(x) = Self::tag_pos_to_file(path, FileType::Index) {
                vec![x]
            } else {
                vec![]
            }
        };
        let data: Vec<Vec<SynsetIndex>> = pos_synset
            .into_par_iter()
            .filter_map(|(_pos, p)| {
                Self::from_index_file(&p).ok()
            })
            .collect();
        let mut data: WordNetIndex = data.into_iter()
            .fold(Default::default(), |mut acc, next| {
                acc.extend(next);
                acc
            });
        data.sort_by(|l, r| l.name.cmp(&r.name));
        return Ok(data);
    }

    fn build_data<P: AsRef<Path>>(path: P) -> Result<(PosSynsets, AllSynsets), Error> {
        let path = PathBuf::from(path.as_ref());
        if !path.exists() {
            return Err(err_msg(format!("could not find path {:?}", path)));
        };
        let pos_synset: Vec<(POS, PathBuf)> = if path.is_dir() {
            fs::read_dir(path)?
                .filter_map(|entry| entry.ok())
                .map(|entry| entry.path())
                .filter_map(|p| {
                    Self::tag_pos_to_file(p, FileType::Data).ok()
                })
                .collect()
        } else {
            if let Ok(x) = Self::tag_pos_to_file(path, FileType::Data) {
                vec![x]
            } else {
                vec![]
            }
        };

        let data: Vec<(POS, Vec<Box<Synset>>)> = pos_synset
            .into_par_iter()
            .filter_map(|(pos, p)| {
                Self::from_data_file(&p)
                    .map(|synsets| (pos, synsets))
                    .ok()
            })
            .collect();

        let mut pos_synsets: HashMap<POS, HashMap<usize, Box<Synset>>> = HashMap::new();

        for (p, v) in data.clone() {
            let v_clone = v.clone();
            let mut map = HashMap::new();
            for s in v_clone {
                map.insert(s.synset_offset, s);
            }
            pos_synsets.entry(p).and_modify(|saved_synsets| {
                saved_synsets.extend(map);
            });
        }

        let mut all_synsets = vec![];
        for (_p, v) in data.clone() {
            let v_clone = v.clone();
            all_synsets.extend(v_clone);
        }
        Ok((pos_synsets, all_synsets))
    }

    fn from_index_file<P: AsRef<Path>>(path: P) -> Result<Vec<SynsetIndex>, Error> {
        let f = File::open(path)?;
        let f = BufReader::new(f);
        let lines: Vec<String> = f
            .lines()
            .filter_map(|l| l.ok())
            .filter(|l| !l.starts_with("  "))
            .collect();
        let index = lines
            .par_iter()
            .filter_map(|line| Synset::parse_index_from_line(line.to_string()).ok())
            .collect();
        Ok(index)
    }
    fn from_data_file<P: AsRef<Path>>(path: P) -> Result<Vec<Box<Synset>>, Error> {
        let f = File::open(path)?;
        let f = BufReader::new(f);
        let lines: Vec<String> = f
            .lines()
            .filter_map(|l| l.ok())
            .filter(|l| !l.starts_with("  "))
            .collect();
        let synsets = lines
            .par_iter()
            .filter_map(|line| Synset::parse_synset_from_line(line.to_string()).ok())
            .map(|s| Box::new(s))
            .collect();
        Ok(synsets)
    }
}


impl WordNet {
    pub fn len(&self) -> usize {
        self.all_synsets.len()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::time::Instant;

    #[test]
    fn test_split_gloss() {
        let noun_synset_raw = r#"00006269 03 n 01 life 0 002 @ 00004258 n 0000 ~ 08010218 n 0000 | living things collectively; "the oceans are teeming with life""#;
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
        let noun_synset_raw = r#"00006269 03 n 01 life 0 002 @ 00004258 n 0000 ~ 08010218 n 0000 | living things collectively; "the oceans are teeming with life""#;
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
                    pointer_symbol: "@".to_string(),
                    synset_offset: 00004258,
                    pos: SynsetType::Noun,
                    source_target: 0000,
                },
                SynsetPointer {
                    pointer_symbol: "~".to_string(),
                    synset_offset: 08010218,
                    pos: SynsetType::Noun,
                    source_target: 0000,
                },
            ],
            gloss: Gloss::from(r#"living things collectively; "the oceans are teeming with life""#),
        };
        let prepared = Synset::parse_synset_from_line(noun_synset_raw.to_owned()).unwrap();
        assert_eq!(prepared, except);

        assert_eq!(Synset::parse_synset_from_line(r#"00015078 02 r 01 well c 0 |well"#.to_string()).unwrap(),
                   Synset {
                       synset_offset: 00015078,
                       lex_filenum: 02,
                       ss_type: SynsetType::Adverb,
                       w_cnt: 1,
                       word: vec![Word { word: "well".to_string(), lex_id: 12 }],
                       p_cnt: 0,
                       ptr: Vec::new(),
                       gloss: Gloss {
                           definition: vec!["well".to_string()],
                           example: Vec::new(),
                       },
                   })
    }

    #[test]
    fn test_parse_gloss() {
        let raw = "facing or on the side toward the apex";
        assert_eq!(Gloss { definition: vec![raw.to_string()], example: Vec::new() }, Gloss::from(raw));
        let raw = r#"nearest to or facing toward the axis of an organ or organism; "the upper side of a leaf is known as the adaxial surface" "#;
        assert_eq!(Gloss { definition: vec!["nearest to or facing toward the axis of an organ or organism".to_string()], example: vec!["the upper side of a leaf is known as the adaxial surface".to_string()] }, Gloss::from(raw))
    }

    #[test]
    fn test_parse_data_line() {
        let line = r#"00624823 31 v 03 interpret 0 construe 0 see d 018 @ 00590283 v 0000 + 06755325 n 0201 ^ 02666897 v 0202 + 07185404 n 0101 + 05937299 n 0101 + 05774244 n 0101 ~ 00621541 v 0000 ~ 00623176 v 0000 ~ 00625328 v 0000 ~ 00625484 v 0000 ~ 00625619 v 0000 ~ 00625812 v 0000 ~ 00625935 v 0000 ~ 00626148 v 0000 ~ 00626756 v 0000 ~ 00629157 v 0000 $ 00692380 v 0000 ~ 01634074 v 0000 01 + 08 00 | make sense of; assign a meaning to; "What message do you see in this letter?"; "How do you interpret his behavior?""#;
        let synset = Synset::parse_synset_from_line(line.to_string());
        println!("{:?}", synset);
    }

    #[test]
    fn test_parse_index_line() {
        let raw = r#"grammatical a 2 3 ! \ + 2 1 02891626 01149515"#;
        let expect = SynsetIndex {
            name: "grammatical.a".to_string(),
            lemma: String::from("grammatical"),
            pos: POS::Adj,
            synset_cnt: 2,
            p_cnt: 3,
            ptr_symbol: vec!["!".to_string(), "\\".to_string(), "+".to_string()],
            sense_cnt: 2,
            tagsense_cnt: 1,
            synset_offset: vec![02891626, 01149515],
        };
        assert_eq!(Synset::parse_index_from_line(raw.to_string()).unwrap(), expect);
    }

    #[ignore]
    #[test]
    fn test_parse_index_file() {
        let index = WordNet::build_index("/home/oaa/Downloads/dict").unwrap();
        println!("index {}", index.len());
    }

    #[ignore]
    #[test]
    fn test_parse_data_file() {
        let (_pos, all) = WordNet::build_data("/home/oaa/Downloads/dict").unwrap();
        println!("len {}", all.len());
        let res = all.into_iter().find(|s| s.w_cnt > 1);
        println!("res {:?}", res);
    }

    #[ignore]
    #[test]
    fn test_word_net() {
        let start = Instant::now();
        let wn = WordNet::new(Some("/home/oaa/Downloads/dict")).unwrap();
        println!("len {}", wn.len());
        let end = Instant::now();

        println!("{:?}", end - start);
    }
}
