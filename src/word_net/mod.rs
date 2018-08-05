use difflib;
use failure::{err_msg, Error};
use itertools;
use rayon::prelude::*;
use std::collections::{HashMap, VecDeque};
use std::convert::*;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::time::Instant;

mod define;

use self::define::{FileType, Gloss, SynsetPointer, POS};

lazy_static! {
    static ref WORDNET_INDEX: WordNetIndex = WordNetIndex::try_new().unwrap();
    static ref POS_SYNSETS: PosSynsets = PosSynsets::try_new().unwrap();
}

#[derive(PartialEq, Debug, Clone)]
struct SynsetPtr {
    pos: POS,
    offset: usize,
}

#[derive(PartialEq, Debug, Clone)]
struct Lemma {
    synset_ptr: SynsetPtr,
    name: String,
    lex_id: usize,
}

#[derive(PartialEq, Debug, Clone)]
struct LemmaPointer {
    ptr: SynsetPtr,
    target_index: usize,
}

impl Lemma {
    fn name(&self) -> String {
        return self.name.to_owned();
    }
    fn pertainyms(&self) -> Vec<&Lemma> {
        return self.related("\\".to_owned());
    }
    fn related(&self, symbol: String) -> Vec<&Lemma> {
        let parent_synset = POS_SYNSETS.get(&self.synset_ptr);
        parent_synset
            .lemma_pointers
            .get(&self.name)
            .and_then(|map| map.get(&symbol))
            .map(|lis: &Vec<LemmaPointer>| {
                lis.par_iter()
                    .filter_map(|lp: &LemmaPointer| {
                        POS_SYNSETS.get(&lp.ptr).lemmas().get(lp.target_index)
                    })
                    .collect()
            })
            .unwrap_or_default()
    }
}

type LemmaPointers = HashMap<String, HashMap<String, Vec<LemmaPointer>>>;

#[derive(PartialEq, Debug, Clone)]
pub struct Synset {
    offset: usize,
    //TODO build nest enum from string
    lexname: usize,
    pos: POS,
    n_lemmas: usize,
    lemmas: Vec<Lemma>,
    n_pointers: usize,
    pointers: Vec<SynsetPointer>,
    lemma_pointers: LemmaPointers,
    definition: Vec<String>,
    example: Vec<String>,
    name: String,
}

type PointerRes = (Vec<SynsetPointer>, LemmaPointers);

impl Synset {
    fn split_gloss(data: String) -> Result<(String, String), Error> {
        let res: Vec<&str> = data.rsplit("|").collect();
        let gloss = res.get(0).ok_or(err_msg("could not find gloss"))?;
        let other = res.get(1).ok_or(err_msg("could not find synset_str"))?;
        return Ok((other.trim().to_owned(), gloss.trim().to_owned()));
    }

    fn parse_words_from_token_list(
        pos: POS,
        offset: usize,
        count: usize,
        token_list: &mut VecDeque<&str>,
    ) -> Result<Vec<Lemma>, Error> {
        let chunk_size = 2;
        let data: Vec<&str> = token_list.drain(0..count * chunk_size).collect();

        let items: Vec<&[&str]> = data.chunks(chunk_size).collect();
        let res: Vec<Lemma> = items
            .into_par_iter()
            .map(|items: &[&str]| {
                let word = items[0];
                let lex_id = usize::from_str_radix(items[1], 16)?;
                Ok(Lemma {
                    synset_ptr: SynsetPtr { pos, offset },
                    name: word.to_string(),
                    lex_id,
                })
            })
            .filter_map(|r: Result<Lemma, Error>| r.ok())
            .collect();
        Ok(res)
    }
    fn parse_ptrs_from_token_list(
        word: &Vec<Lemma>,
        count: usize,
        token_list: &mut VecDeque<&str>,
    ) -> Result<PointerRes, Error> {
        let data: Vec<&str> = token_list.drain(0..count * 4).collect();

        let chunks: Vec<&[&str]> = data.chunks(4).collect();
        let data: Vec<(String, usize, POS, usize, usize)> = chunks
            .par_iter()
            .map(|items: &&[&str]| {
                let pointer_symbol = items[0].to_string();
                let synset_offset = items[1].parse::<usize>()?;
                let pos = POS::from_str(items[2])?;
                let source_target = items[3];
                let source_index = usize::from_str_radix(&source_target[0..2], 16)?;
                let target_index = usize::from_str_radix(&source_target[2..4], 16)?;
                return Ok((
                    pointer_symbol,
                    synset_offset,
                    pos,
                    source_index,
                    target_index,
                ));
            })
            .filter_map(|r: Result<(String, usize, POS, usize, usize), Error>| r.ok())
            .collect();
        let mut pointers: Vec<SynsetPointer> = vec![];
        let mut lemma_pointers: LemmaPointers = Default::default();

        for (pointer_symbol, synset_offset, pos, source_index, target_index) in data {
            if source_index == 0 && target_index == 0 {
                pointers.push(SynsetPointer {
                    pos,
                    pointer_symbol,
                    synset_offset,
                });
            } else {
                let source_lemma_name = word[source_index - 1].name();
                let mut v = lemma_pointers.entry(source_lemma_name).or_default();
                let mut lis = v.entry(pointer_symbol).or_default();
                lis.push(LemmaPointer {
                    ptr: SynsetPtr {
                        pos,
                        offset: synset_offset,
                    },
                    target_index: target_index - 1,
                })
            }
        }
        Ok((pointers, lemma_pointers))
    }

    pub fn parse_synset_from_line(data: String) -> Result<Synset, Error> {
        let (synset_str, gloss) = Self::split_gloss(data.clone())?;
        let mut synset_token_list: VecDeque<&str> = synset_str.split_whitespace().collect();
        let offset = synset_token_list
            .pop_front()
            .ok_or(err_msg("could not find synset_offset"))?
            .parse::<usize>()?;
        let lex_filenum = synset_token_list
            .pop_front()
            .ok_or(err_msg("could not find lex_filenum"))?;
        let lex_filenum = usize::from_str_radix(lex_filenum, 10)?;
        let pos_str = synset_token_list
            .pop_front()
            .ok_or(err_msg("could not find pos"))?;
        let pos = POS::from_str(pos_str)?;

        let n_lemmas = synset_token_list
            .pop_front()
            .ok_or(err_msg("could not find w_cnt"))?;
        let n_lemmas = usize::from_str_radix(n_lemmas, 16)?;
        let word = Self::parse_words_from_token_list(pos, offset, n_lemmas, &mut synset_token_list)
            .map_err(|e| {
                println!("parse_words_from_token_list err {:?}", e);
                e
            })?;
        let p_cnt = synset_token_list
            .pop_front()
            .ok_or(err_msg("could not find p_cnt"))?
            .parse::<usize>()?;

        let (ptr, lemma_pointers) =
            Self::parse_ptrs_from_token_list(&word, p_cnt, &mut synset_token_list)?;
        let gloss = Gloss::from(gloss.as_str());
        let lemma_name = word[0].name().to_lowercase();
        let sense_index = WORDNET_INDEX.get_sense_index(&lemma_name, &pos, offset);

        let name = format!("{}.{}.{}", lemma_name, pos_str, sense_index + 1);
        let synset = Synset {
            name,
            offset,
            lexname: lex_filenum,
            pos,
            n_lemmas,
            lemmas: word,
            n_pointers: p_cnt,
            pointers: ptr,
            lemma_pointers,
            example: gloss.example,
            definition: gloss.definition,
        };
        Ok(synset)
    }
}

impl Synset {
    fn lemmas(&self) -> &Vec<Lemma> {
        return &self.lemmas;
    }
}

type BSynset = Box<Synset>;

struct PosSynsets(HashMap<POS, HashMap<usize, BSynset>>);

use std::ops::Deref;

impl Deref for PosSynsets {
    type Target = HashMap<POS, HashMap<usize, BSynset>>;
    fn deref(&self) -> &HashMap<POS, HashMap<usize, BSynset>> {
        let PosSynsets(ref inner) = *self;
        inner
    }
}

impl PosSynsets {
    fn get(&self, ptr: &SynsetPtr) -> &BSynset {
        let res = self.0.get(&ptr.pos).and_then(|m| m.get(&ptr.offset));
        if res.is_none() {
            println!("PosSynsets err get {:?}", ptr);
        }
        res.unwrap()
    }
}

impl PosSynsets {
    pub fn try_new() -> Result<Self, Error> {
        let start = Instant::now();
        let path: PathBuf = get_wn_path(None::<PathBuf>)?;
        let res = Self::build_data(path);
        let end = Instant::now();
        println!("build_data {:?}", end - start);
        res
    }

    fn build_data<P: AsRef<Path>>(path: P) -> Result<PosSynsets, Error> {
        let path = PathBuf::from(path.as_ref());
        if !path.exists() {
            return Err(err_msg(format!("could not find path {:?}", path)));
        };
        let pos_synset: Vec<(POS, PathBuf)> = if path.is_dir() {
            fs::read_dir(path)?
                .filter_map(|entry| entry.ok())
                .map(|entry| entry.path())
                .filter_map(|p| tag_pos_to_file(p, FileType::Data).ok())
                .collect()
        } else {
            if let Ok(x) = tag_pos_to_file(path, FileType::Data) {
                vec![x]
            } else {
                vec![]
            }
        };

        let data: Vec<(POS, Vec<BSynset>)> = pos_synset
            .into_par_iter()
            .filter_map(|(pos, p)| Self::from_data_file(&p).map(|synsets| (pos, synsets)).ok())
            .collect();

        let mut map: HashMap<POS, HashMap<usize, BSynset>> = HashMap::new();

        for (p, v) in data.clone() {
            let v_clone = v.clone();
            let mut synset_map = HashMap::new();
            for s in v_clone {
                synset_map.insert(s.offset, s);
            }
            map.entry(p).or_default().extend(synset_map);
        }

        Ok(PosSynsets(map))
    }

    fn from_data_file<P: AsRef<Path>>(path: P) -> Result<Vec<BSynset>, Error> {
        let f = File::open(path)?;
        let f = BufReader::new(f);
        let lines: Vec<String> = f
            .lines()
            .filter_map(|l| l.ok())
            .filter(|l| !l.starts_with("  "))
            .collect();
        let synsets = lines
            .par_iter()
            .filter_map(|line| {
                Synset::parse_synset_from_line(line.to_string())
                    .map_err(|e| println!("parse_synset_from_line err {} {:?}", line, e))
                    .ok()
            })
            .map(|s| Box::new(s))
            .collect();
        Ok(synsets)
    }
}

fn tag_pos_to_file(p: PathBuf, expect_file_type: FileType) -> Result<(POS, PathBuf), Error> {
    let file_stem = p
        .file_stem()
        .ok_or(err_msg("could not find file stem"))?
        .to_str()
        .ok_or(err_msg("could not trans stem to string"))?
        .to_owned();
    let extension = p
        .extension()
        .ok_or(err_msg("could not find file extension"))?
        .to_str()
        .ok_or(err_msg("could not trans extension to string"))?
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
            return Err(err_msg(format!(
                "{:?} {:?} {:?} {:?}",
                pos, p, expect_file_type, file_type
            )));
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
struct SynsetIndex {
    lemma: String,
    pos: POS,
    synset_cnt: usize,
    p_cnt: usize,
    ptr_symbol: Vec<String>,
    sense_cnt: usize,
    tagsense_cnt: usize,
    synset_offset: Vec<usize>,
}

struct WordNetIndex {
    index: HashMap<POS, Vec<SynsetIndex>>,
}

fn get_wn_path(wn_path: Option<PathBuf>) -> Result<PathBuf, Error> {
    let wn_path = wn_path
        .or(env::var_os("WN_PATH").map(PathBuf::from))
        .ok_or(err_msg("could not find the location of wordnet"))
        .and_then(|p| {
            if p.exists() & &p.is_dir() {
                Ok(p)
            } else {
                Err(err_msg(format!("{:?} not exists or is not a dir", p)))
            }
        })?;
    Ok(wn_path)
}

impl WordNetIndex {
    //TODO need more elegant way to mock this
    pub fn try_new() -> Result<Self, Error> {
        let start = Instant::now();
        let path: PathBuf = get_wn_path(None::<PathBuf>)?;
        let res = Self::build_index(path);
        let end = Instant::now();
        println!("build index {:?} ", end - start);
        res
    }

    #[cfg(test)]
    pub fn get_sense_index(&self, name: &String, p: &POS, offset: usize) -> usize {
        0
    }

    pub fn get_offset(&self, name: &String, p: &POS) -> Vec<usize> {
        let lis = &self.index[p];
        let res = lis
            .binary_search_by_key(name, |i| i.lemma.to_owned().clone())
            .map(|index| lis[index].synset_offset.clone());
        res.unwrap_or_default()
    }

    #[cfg(not(test))]
    pub fn get_sense_index(&self, name: &String, p: &POS, offset: usize) -> usize {
        self.get_offset(name, p)
            .iter()
            .position(|r| *r == offset)
            .unwrap()
    }

    pub fn parse_index_from_line(data: String) -> Result<SynsetIndex, Error> {
        let mut index_token_list: VecDeque<&str> = data.split_whitespace().collect();
        let lemma = index_token_list
            .pop_front()
            .ok_or(err_msg("could not find lemma"))?
            .to_owned();
        let pos_str = index_token_list
            .pop_front()
            .ok_or(err_msg("could not find pos"))?;
        let pos = POS::from_str(pos_str)?;
        let synset_cnt: usize = index_token_list
            .pop_front()
            .ok_or(err_msg("could not find synset_cnt"))?
            .to_owned()
            .parse()?;
        let p_cnt: usize = index_token_list
            .pop_front()
            .ok_or(err_msg("could not find synset_cnt"))?
            .to_owned()
            .parse()?;
        let ptr_symbol: Vec<String> = index_token_list.drain(0..p_cnt).map(String::from).collect();
        let sense_cnt: usize = index_token_list
            .pop_front()
            .ok_or(err_msg("could not find sense_cnt"))?
            .to_owned()
            .parse()?;
        let tagsense_cnt: usize = index_token_list
            .pop_front()
            .ok_or(err_msg("could not find tagsense_cnt"))?
            .to_owned()
            .parse()?;
        let synset_offset = index_token_list
            .iter()
            .map(|offset| offset.parse::<usize>())
            .filter_map(|r| r.ok())
            .collect();
        let _name = format!("{}.{}", lemma, pos_str);
        Ok(SynsetIndex {
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
    fn from_index_file<P: AsRef<Path>>(path: P) -> Result<Vec<SynsetIndex>, Error> {
        let path = path.as_ref();
        let f = File::open(path)?;
        let f = BufReader::new(f);
        let lines: Vec<String> = f
            .lines()
            .filter_map(|l| l.ok())
            .filter(|l| !l.starts_with("  "))
            .collect();
        let index = lines
            .par_iter()
            .filter_map(|line| Self::parse_index_from_line(line.to_string()).ok())
            .collect();
        Ok(index)
    }

    fn build_index(path: PathBuf) -> Result<WordNetIndex, Error> {
        if !path.exists() {
            return Err(err_msg(format!("could not find path {:?}", path)));
        };
        let pos_synset: Vec<(POS, PathBuf)> = if path.is_dir() {
            fs::read_dir(path)?
                .filter_map(|entry| entry.ok())
                .map(|entry| entry.path())
                .filter_map(|p| tag_pos_to_file(p, FileType::Index).ok())
                .collect()
        } else {
            if let Ok(x) = tag_pos_to_file(path, FileType::Index) {
                vec![x]
            } else {
                vec![]
            }
        };
        let data: Vec<(POS, Vec<SynsetIndex>)> = pos_synset
            .into_par_iter()
            .filter_map(|(_pos, p)| Self::from_index_file(&p).map(|d| (_pos, d)).ok())
            .collect();

        let mut index: HashMap<POS, Vec<SynsetIndex>> = HashMap::default();

        for (pos, mut data) in data {
            let saved_index = index.entry(pos).or_default();
            saved_index.append(&mut data);
        }

        for val in index.values_mut() {
            val.sort_by(|l, r| l.lemma.cmp(&r.lemma));
        }
        return Ok(WordNetIndex { index });
    }
}

//stupid rust could not self reference in struct
pub struct WordNet {
    synsets: &'static PosSynsets,
    index: &'static WordNetIndex,
}

use std::env;

impl WordNet {
    pub fn new() -> Result<Self, Error> {
        let synsets = &*POS_SYNSETS;
        let index: &WordNetIndex = &*WORDNET_INDEX;
        Ok(Self { synsets, index })
    }
}

impl WordNet {
    pub fn synsets(&self, pos: &POS, word: &String) -> Vec<&BSynset> {
        //TODO add logic of _morphy
        let offsets = self.index.get_offset(&word.to_lowercase(), &pos);
        let synset_map = &self.synsets[pos];
        let mut res = vec![];
        for offset in offsets {
            //TODO handle none here
            synset_map.get(&offset).map(|ss| {
                res.push(ss);
                ()
            });
        }
        res
    }

    pub fn all_synsets(&self, pos: Option<POS>) -> Vec<&BSynset> {
        if let Some(pos) = pos {
            self.synsets[&pos].values().collect()
        } else {
            let res: Vec<&BSynset> =
                itertools::flatten(self.synsets.values().map(|map| map.values())).collect();
            res
        }
    }
}

const WN_PATH: &'static str = "/home/oaa/Downloads/dict";

mod word_forms {
    use super::*;
    use inflector::string::singularize::to_singular;
    use itertools;

    lazy_static! {
        static ref WN: WordNet = build_word_net();
    }

    fn build_word_net() -> WordNet {
        ::std::env::set_var("WN_PATH", WN_PATH);
        WordNet::new().unwrap()
    }

    fn get_all_wordnet_words() -> Vec<String> {
        fn get_all_lemma_names(s: &&'static BSynset) -> impl Iterator<Item=String> {
            s.lemmas().iter().map(|l| l.name())
        }

        let res: Vec<String> =
            itertools::flatten(WN.all_synsets(None).iter().map(get_all_lemma_names)).collect();
        res
    }

    fn get_conjugated_verb_list() -> Vec<Vec<String>> {
        let data = include_str!("../../data/en-verbs.txt");

        fn split_with_comma(l: &str) -> Vec<String> {
            l.split(",")
                .filter(|i| !i.is_empty())
                .map(|i| i.to_string())
                .collect()
        }
        data.lines()
            .filter(|l| !l.starts_with(";"))
            .map(split_with_comma)
            .collect()
    }

    fn get_related_lemmas(_word: &String) {}

    pub fn get_word_forms(word: String) {
        let _word = to_singular(&word);
    }

    fn get_adjective_to_adverb() -> HashMap<String, String> {
        let mut map = HashMap::default();
        for synset in WN.all_synsets(Some(POS::Adv)) {
            println!("get_adjective_to_adverb {}", synset.name);
            for lemma in synset.lemmas() {
                let word = lemma.name();
                let this_word_lemmas = c![lemma, for lemma in ss.lemmas(), for ss in WN.synsets( & POS::Adv, & word), if lemma.name() == word];
                let pertainyms = c![pertainym.name(), for pertainym in this_word_lemma.pertainyms(), for this_word_lemma in this_word_lemmas.iter()];
                let matches = difflib::get_close_matches(
                    &word,
                    pertainyms.iter().map(|s| s as &str).collect(),
                    3,
                    0.6,
                );
                if let Some(match_word) = matches.get(0) {
                    map.insert(String::from(*match_word), word);
                }
            }
        }
        map
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
        env::set_var("WN_PATH", WN_PATH);
        let noun_synset_raw = r#"00006269 03 n 01 life 0 002 @ 00004258 n 0000 ~ 08010218 n 0101 | living things collectively; "the oceans are teeming with life""#;
        let mut lemma_pointer = HashMap::default();
        let mut p = HashMap::default();
        p.insert(
            "~".to_string(),
            vec![LemmaPointer {
                ptr: SynsetPtr {
                    pos: POS::Noun,
                    offset: 08010218,
                },
                target_index: 0,
            }],
        );
        lemma_pointer.insert("life".to_owned(), p);
        let except = Synset {
            name: "life.n.1".to_string(),
            offset: 00006269,
            lexname: 03,
            pos: POS::Noun,
            n_lemmas: 01,
            lemmas: vec![Lemma {
                synset_ptr: SynsetPtr {
                    pos: POS::Noun,
                    offset: 00006269,
                },
                name: "life".to_owned(),
                lex_id: 0,
            }],
            n_pointers: 002,
            lemma_pointers: lemma_pointer,
            pointers: vec![SynsetPointer {
                pointer_symbol: "@".to_string(),
                synset_offset: 00004258,
                pos: POS::Noun,
            }],
            example: vec!["the oceans are teeming with life".to_owned()],
            definition: vec!["living things collectively".to_owned()],
        };
        let prepared = Synset::parse_synset_from_line(noun_synset_raw.to_owned()).unwrap();
        assert_eq!(prepared, except);

        assert_eq!(
            Synset::parse_synset_from_line(r#"00015078 02 r 01 well c 0 |well"#.to_string())
                .unwrap(),
            Synset {
                name: "well.r.1".to_string(),
                offset: 00015078,
                lexname: 02,
                pos: POS::Adv,
                n_lemmas: 1,
                lemmas: vec![Lemma {
                    synset_ptr: SynsetPtr {
                        pos: POS::Adv,
                        offset: 00015078,
                    },
                    name: "well".to_string(),
                    lex_id: 12,
                }],
                lemma_pointers: Default::default(),
                n_pointers: 0,
                pointers: Vec::new(),
                definition: vec!["well".to_string()],
                example: Vec::new(),
            }
        );
    }

    #[test]
    fn test_parse_gloss() {
        let raw = "facing or on the side toward the apex";
        assert_eq!(
            Gloss {
                definition: vec![raw.to_string()],
                example: Vec::new(),
            },
            Gloss::from(raw)
        );
        let raw = r#"nearest to or facing toward the axis of an organ or organism; "the upper side of a leaf is known as the adaxial surface" "#;
        assert_eq!(
            Gloss {
                definition: vec![
                    "nearest to or facing toward the axis of an organ or organism".to_string(),
                ],
                example: vec![
                    "the upper side of a leaf is known as the adaxial surface".to_string(),
                ],
            },
            Gloss::from(raw)
        )
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
            lemma: String::from("grammatical"),
            pos: POS::Adj,
            synset_cnt: 2,
            p_cnt: 3,
            ptr_symbol: vec!["!".to_string(), "\\".to_string(), "+".to_string()],
            sense_cnt: 2,
            tagsense_cnt: 1,
            synset_offset: vec![02891626, 01149515],
        };
        assert_eq!(
            WordNetIndex::parse_index_from_line(raw.to_string()).unwrap(),
            expect
        );
    }

    #[ignore]
    #[test]
    fn test_parse_index_file() {
        let index = WordNetIndex::build_index(PathBuf::from("/home/oaa/Downloads/dict")).unwrap();
    }

    #[ignore]
    #[test]
    fn test_parse_data_file() {
        let pos = PosSynsets::build_data("/home/oaa/Downloads/dict").unwrap();
    }

    #[ignore]
    #[test]
    fn test_all_synsets() {
        env::set_var("WN_PATH", WN_PATH);

        let WN = WordNet::new().unwrap();
        let all: Vec<&BSynset> = WN.all_synsets(None);
        println!("len {}", all.len());
    }

    #[ignore]
    #[test]
    fn test_word_net() {
        let start = Instant::now();
        let wn = WordNet::new().unwrap();
        let all = wn.all_synsets(None);
        let end = Instant::now();
        println!("{:?}", end - start);
    }
}
