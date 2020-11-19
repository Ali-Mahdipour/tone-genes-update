######################################################################################
#
# Here we assemble and preprocess linguistic data from several databases
#
# (c) Dan Dediu, 2020
#
######################################################################################

# Assume the current directory is the project directory!


# Libraries:
library(dplyr);


##
## Glottolog 4.1 ####
##

if( !dir.exists("./data/language/input/glottolog4.1") )
{
  # Download the needed data:
  dir.create("./data/language/input/glottolog4.1", showWarnings=FALSE);
  if( download.file("https://cdstar.shh.mpg.de/bitstreams/EAEA0-3DAE-E27B-4692-0/glottolog_languoid.csv.zip", 
                    destfile="./data/language/input/glottolog4.1/glottolog_languoid.csv.zip") != 0 ||
      download.file("https://cdstar.shh.mpg.de/bitstreams/EAEA0-3DAE-E27B-4692-0/languages_and_dialects_geo.csv", 
                    destfile="./data/language/input/glottolog4.1/languages_and_dialects_geo.csv") != 0 ||
      download.file("https://cdstar.shh.mpg.de/bitstreams/EAEA0-3DAE-E27B-4692-0/tree_glottolog_newick.txt", 
                    destfile="./data/language/input/glottolog4.1/tree_glottolog_newick.txt") != 0 )
  {
    stop("Cannot retrive data for Glottolog v4.1 from shh.mpg.de!\n");
  }
  
  # Extract it:
  unzip("./data/language/input/glottolog4.1/glottolog_languoid.csv.zip", exdir="./data/language/input/glottolog4.1/");

  # Remove unneeded files:
  unlink("./data/language/input/glottolog4.1/glottolog_languoid.csv.zip", recursive=FALSE);
}

# Everything should be fine now...
glottolog_data       <- read.table("./data/language/input/glottolog4.1/languoid.csv", header=TRUE, sep=",", quote='"');
glottolog_data_areas <- read.table("./data/language/input/glottolog4.1/languages_and_dialects_geo.csv", header=TRUE, sep=",", quote='"');


##
## PHOIBLE ####
##

if( !dir.exists("./data/language/input/phoible-v2.0.1") )
{
  # Retrieve it from Zenodo:
  if( download.file("https://zenodo.org/record/2677911/files/cldf-datasets/phoible-v2.0.1.zip", 
                    destfile="./data/language/input/phoible-v2.0.1/phoible-v2.0.1.zip") != 0 )
  {
    stop("Cannot retrive the data for PHOIBLE v2.0.1 from Zenodo!\n");
  }
  
  
  # Extract it:
  unzip("./data/language/input/phoible-v2.0.1/phoible-v2.0.1.zip", exdir="./data/language/input/phoible-v2.0.1");
  
  # Remove the bits we won't use:
  unlink("./data/language/input/phoible-v2.0.1/cldf-datasets-phoible-f36deac/create.py", recursive=FALSE);
  unlink("./data/language/input/phoible-v2.0.1/cldf-datasets-phoible-f36deac/test.py", recursive=FALSE);
  unlink("./data/language/input/phoible-v2.0.1/cldf-datasets-phoible-f36deac/cldf/contributions.csv", recursive=FALSE);
  unlink("./data/language/input/phoible-v2.0.1/cldf-datasets-phoible-f36deac/cldf/contributors.csv", recursive=FALSE);
  unlink("./data/language/input/phoible-v2.0.1/cldf-datasets-phoible-f36deac/cldf/languages.csv", recursive=FALSE);
  unlink("./data/language/input/phoible-v2.0.1/cldf-datasets-phoible-f36deac/cldf/sources.bib", recursive=FALSE);
  unlink("./data/language/input/phoible-v2.0.1/cldf-datasets-phoible-f36deac/cldf/StructureDataset-metadata.json", recursive=FALSE);
}

# Everything should be fine now...
d_phoible_parameters <- read.table("./data/language/input/phoible-v2.0.1//cldf-datasets-phoible-f36deac/cldf/parameters.csv", sep=",", header=TRUE);
d_phoible_parameters <- d_phoible_parameters[ !is.na(d_phoible_parameters$SegmentClass) & d_phoible_parameters$SegmentClass == "tone", c("ID", "Name", "Description")]; # keep only the tones
d_phoible_values_full <- read.table("./data/language/input/phoible-v2.0.1/cldf-datasets-phoible-f36deac/cldf/values.csv", sep=",", header=TRUE);
d_phoible_values <- merge(d_phoible_values_full[,c("ID", "Language_ID", "Parameter_ID", "Value", "Marginal", "Allophones")], 
                          d_phoible_parameters, by.x=c("Parameter_ID", "Value"), by.y=c("ID", "Name"), all=FALSE); # keep only languages with tone symbols
d_phoible_values <- d_phoible_values[ d_phoible_values$Marginal != "true", ];

# Which symbols and languages to keep:
tmp <- d_phoible_values %>%
  group_by(Parameter_ID, Value) %>%
  summarise(no_lgs = n()) %>%
  arrange(desc(no_lgs))

# Remove rare or "weird" symbols:
d_phoible_values <- d_phoible_values[ !(d_phoible_values$Parameter_ID %in% c("304720412EBB60BAA60FED702D361604", # ↓˦ (11 languages)
                                                                             "EB150ACC8B614BECD70C57A73E29A158", # ↓ (5 languages)
                                                                             "580FEDB5E211EA949F256264D5019244", # ˦↓˦ (3 languages)
                                                                             "91E25A72DACF30BA06438AFE223E48EB", # ˥̰ (2 languages
                                                                             "DC360FCD2919CADF7D326367357FE34F", # ˩̰ (2 languages)
                                                                             "187321E24435D8B798F59F7B5FB4BF53", # ↓˦˨ (1 language)
                                                                             "3F0BA824C7276C5E286ECB25EC1D01E6", # ↓˦↓˦ (1 language)
                                                                             "4DB5F95E762595DA42CA893FFDF80648", # ˧˨ˤ (1 language)
                                                                             "70E7DABB2E7EB34D9E442619D67496B9", # ˦˥̰ (1 language)
                                                                             "749B61EA65A40AAA80CEFE6F0F337752", # ˧˨̤ (1 language)
                                                                             "8DAB1E1619693CF636242B4934C7AFA1", # ˦ˀ (1 language)
                                                                             "8F253FB1FFBFB0F7EB811CD7DB5F785F", # ˩̤ (1 language)
                                                                             "999DED248AE06E14169BCC91CF4C3BA7", # ˩̤ (1 language)
                                                                             "B5D7570AB6F97E86267F82BF47BEE82B", # ˥˩˩˥ (1 language)
                                                                             "E011FDAB9EA6316743EEA2DB343D2502", # ˧˩̰ (1 language)
                                                                             "ECFE918D274D0F2288C457A59616B4E2", # ˥˧̰ (1 language)
                                                                             "EFF6BD8CD1677385309BCEEF00673451"  # ˦̰ (1 language)
                                                                             )), ]

# Count the number of unique tone symbols per language:
phoible_number_tones <- d_phoible_values %>%
  group_by(Language_ID) %>%
  summarise(number_tone_symbols = length(unique(Parameter_ID))) %>%
  arrange(desc(number_tone_symbols));
# ... and consider that all the otherslanguages don't have tone:
phoible_number_tones <- rbind(phoible_number_tones,
                              data.frame("Language_ID" = unique(setdiff(as.character(d_phoible_values_full$Language_ID), 
                                                                        as.character(phoible_number_tones$Language_ID))),
                                         "number_tone_symbols" = 0 ));
# Add language names:
phoible_number_tones <- merge(phoible_number_tones, glottolog_data[,c("id", "name")], by.x="Language_ID", by.y="id", all=FALSE); names(phoible_number_tones)[3] <- "Name.glottlog";
# Remove (partial) duplicates:
phoible_number_tones <- unique(phoible_number_tones);
indices_to_keep <- unique(vapply(1:nrow(phoible_number_tones), function(i) 
  {
    # Check if partially duplicated:
    s <- which(phoible_number_tones$Language_ID == phoible_number_tones$Language_ID[i] &
                 phoible_number_tones$number_tone_symbols == phoible_number_tones$number_tone_symbols[i]);
    if( length(s) == 1 )
    { 
      return (s); # unique hit, keep it!
    } else if( length(s) == 0 )
    {
      stop(paste0("Error finding language with glottcode ",phoible_number_tones$Language_ID[i],"!\n")); # this shound never happen!
      return (NA);
    } else
    {
      # Ok, so this is (partly) duplicated: keep the one with a non-empty language name:
      s1 <- s[ !is.na(phoible_number_tones$Name.glottlog[s]) & phoible_number_tones$Name.glottlog[s] != "" ];
      if( length(s1) == 1 )
      {
        return (s1);
      } else if( length(s1) == 0 )
      {
        return (min(s)); # there's no name for this one so keep it with an empty name
      } else if( length(unique(phoible_number_tones$Name.glottlog[s1])) > 1 )
      {
        warning(paste0("Multiple different names found for language with glottcode ",phoible_number_tones$Language_ID[i],": ",
                       paste0("'",unique(phoible_number_tones$Name.glottlog[s1]),",", collapse=", "),"!\n"));
        return (min(s1)); # return the first one
      } else
      {
        return (min(s1)); # there's really only one unique name
      }
    }
  }, numeric(1)));
phoible_number_tones <- phoible_number_tones[ indices_to_keep, ];
names(phoible_number_tones) <- c("glottocode", "number_tone_symbols", "glottolog_name");
# Sort by number of tones and language name;
phoible_number_tones <- phoible_number_tones[ order(-phoible_number_tones$number_tone_symbols, phoible_number_tones$glottolog_name), ];


##
## WALS ####
##

if( !dir.exists("./data/language/input/wals-2014") )
{
  # Retrieve the data:
  if( download.file("https://cdstar.shh.mpg.de/bitstreams/EAEA0-7269-77E5-3E10-0/wals_language.csv.zip", 
                    destfile="./data/language/input/wals-2014/wals_language-2014.csv.zip") != 0 )
  {
    stop("Cannot retrive the data for WALS 2014 from ssh.mpg.de!\n");
  }
  
  
  # Extract it:
  unzip("./data/language/input/wals-2014/wals_language-2014.csv.zip", exdir="./data/language/input/wals-2014");
}

# Everything should be fine now...
wals_data <- read.table("./data/language/input/wals-2014/language.csv", sep=",", quote='"', header=TRUE, stringsAsFactors=FALSE);
wals_data <- wals_data[ (!is.na(wals_data$X13A.Tone) & wals_data$X13A.Tone != ""),  # keep the languages where we have data at least for tone
                        c("iso_code", "glottocode", "Name", "latitude", "longitude", "genus", "family", "macroarea", "countrycodes", "X13A.Tone") ];
names(wals_data) <- c("iso_code", "glottocode", "name", "latitude", "longitude", "genus", "family", "macroarea", "countrycodes", "tone");
wals_data$tone[ wals_data$tone == "" ] <- NA; # mark the missing data as such!
wals_data$tone <- ifelse(wals_data$tone == "1 No tones", "None",
                         ifelse(wals_data$tone == "2 Simple tone system", "Simple", 
                                "Complex"));


##
## LAPSyD ####
##

lapsyd_data <- read.table("./data/language/input/lapsyd/LAPSyD-Tone-20200330.csv", sep="\t", quote="", header=TRUE, stringsAsFactors=FALSE);

# Add Glottolog codes:
lapsyd_data <- merge(lapsyd_data, unique(glottolog_data[,c("iso639P3code", "id")]), by.x="ISO", by.y="iso639P3code", all.x=TRUE, all.y=FALSE);
names(lapsyd_data)[ncol(lapsyd_data)] <- "glottocode";

# Manually fix issues:
lapsyd_data$glottocode <- as.character(lapsyd_data$glottocode);
lapsyd_data$ISO <- as.character(lapsyd_data$ISO);
lapsyd_data$glottocode[ lapsyd_data$ISO == "ell" & lapsyd_data$Name == "Grico" ] <- "apul1236"; # this really is Apulia-Calabrian Greek
lapsyd_data$glottocode[ lapsyd_data$ISO == "dwu" ] <- "dhuw1249"; # for some reason, Glottolog still uses the "deprecated" ISO 639-3 duj
lapsyd_data[ lapsyd_data$ISO == "xnu", c("ISO", "glottocode") ] <- c("xnz", "kenu1243"); # this seems to be Kenuzi, a dilect of Kenuzi-Dongola, glottocode kenu1236, ISO 639-3 kzh

lapsyd_data$Tone[ lapsyd_data$Tone == "" ] <- NA; # mark the missing data as such!


##
## Dediu & Ladd (2007) tone judgments ####
##

dl2007_data <- read.table("./data/language/input/dediu-ladd-2007/Dediu-Ladd-2007.tsv", header=TRUE, sep="\t", quote="");

# Remove the "Papuan" data that is too uninformative:
dl2007_data <- dl2007_data[ dl2007_data$Population != "Papuan", ];

# Split multiple language codes into multiple rows:
dl2007_data <- do.call(rbind, lapply(1:nrow(dl2007_data), 
                                     function(i) data.frame(dl2007_data[i,], 
                                                            "ISO"=if(dl2007_data$Languages[i] == "") "" else strsplit(as.character(dl2007_data$Languages[i]),"[[:blank:]]+")[[1]], row.names=NULL)));

# Add Glottolog codes:
dl2007_data <- merge(dl2007_data, unique(glottolog_data[,c("iso639P3code", "name", "id")]), by.x="ISO", by.y="iso639P3code", all.x=TRUE, all.y=FALSE);
names(dl2007_data)[ncol(dl2007_data)] <- "glottocode";


##
## World Phonotactics Database (WPHON) ####
##

# As this database is not available online anymore at the time of this writing (April 2020), we used the WayBackMachine/InternetArchive to access the latest available spanshot from 2014...
if( !file.exists("./data/language/input/wphon/wphon-latest.csv.xz") )
{
  stop("Please run the Python script at ./data/language/input/wphon/download_and_convert_WPD.py (including the manual steps inside)!\n");
}
wphon_data <- read.table(xzfile("./data/language/input/wphon/wphon-latest.csv.xz"), header=TRUE, sep=",", quote='"', stringsAsFactors=FALSE);

# Keep only the entries with the data of interest:
wphon_data <- wphon_data[ !is.na(wphon_data$Maximal.onset) | !is.na(wphon_data$Maximal.coda) | !is.na(wphon_data$Tonal.contrasts), ];

# Deal with missing ISOs and multiple entries for the same main ISO code:
wphon_data$ISO.code <- trimws(wphon_data$ISO.code);
wphon_data <- wphon_data[ !is.na(wphon_data$ISO.code) & wphon_data$ISO.code != "" & !grepl("-", wphon_data$ISO.code, fixed=TRUE), ];

# Check for duplicated ISO codes:
if( FALSE )
{
  wphon_data[ (s <- which(duplicated(wphon_data$ISO.code))), 
              c("ISO.code", "Language", "Latitude", "Longitude", "Language.family", "Language.family.1", "Language.family.2", "World.macro.region")];
  wphon_data[ wphon_data$ISO.code == wphon_data$ISO.code[s[1]], 
              c("ISO.code", "Language", "Latitude", "Longitude", "Language.family", "Language.family.1", "Language.family.2", "World.macro.region")];
}
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "unr" & wphon_data$Language == "Keraq"), ]; # could be "Kera' Mundari" (a dialect of Mundari (unr))?
wphon_data[ wphon_data$ISO.code == "mro" & wphon_data$Language == "Hkongso", c("ISO.code", "Language") ] <- c("anl", "Anu-Hkongso"); # seems to be a rather different language
wphon_data[ wphon_data$ISO.code == "can" & wphon_data$Language == "Achang_Lianghe", c("ISO.code", "Language") ] <- c("acn", "Longchuan Achang"); # this is the closest match
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "sgw" & wphon_data$Language == "Chaha"), ]; # seems to be a dialect of Gurage_West_Chaha (sgw))?
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "apc" & wphon_data$Language == "Arabic_Syrian"), ]; # keep just Arabic_Lebanese (apc) 
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "nrb" & wphon_data$Language == "Nera"), ]; # seems not to exist (but there is Nara (nrb))
wphon_data[ wphon_data$ISO.code == "new" & wphon_data$Language == "Ngwe", c("ISO.code") ] <- c("nwe"); # seems a clearical error
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "shi" & wphon_data$Language == "Shilha"), ]; # seems not to exist (but there is Tashelhiyt (shi))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "tbi" & wphon_data$Language == "Tabi"), ]; # seems not to exist (but there is Gaam (tbi))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "hio" & wphon_data$Language == "G||abake"), ]; # seems to be a dialect of Tsoa (hio))?
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "djd" & wphon_data$Language == "Ngaliwurru"), ]; # same language with Jaminjung (djd)
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "wmi" & wphon_data$Language == "Agwamin"), ]; # seems not to exist (but there is Wamin (wmi))
wphon_data[ wphon_data$ISO.code == "bgb" & wphon_data$Language == "Bagusa", c("ISO.code") ] <- c("bqb"); # seems a clearical error
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "bco" & wphon_data$Language == "Bosavi"), ]; # seems to be whole language group (of which Kaluli (bco) is a member)
wphon_data[ wphon_data$ISO.code == "ivv" & wphon_data$Language == "Ivatan_Southern", c("ISO.code", "Language") ] <- c("ivb", "Ibatan"); # seems to be this language
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "mgl" & wphon_data$Language == "Kilenge"), ]; # seems to be the same thing as Maleu (mgl)
wphon_data[ wphon_data$ISO.code == "kyi" & wphon_data$Language == "Karao", c("ISO.code") ] <- c("kyj"); # seems a clearical error
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "lcm" & wphon_data$Language == "Lavonggai"), ]; # seems not to exist (but there is Tungak (lcm))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "kts" & wphon_data$Language == "Kati_South"), ]; # seems to be the same thing as (South) Muyu (kts))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "nrz" & wphon_data$Language == "Nara"), ]; # seems not to exist (but there is Alaala (nrz))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "llp" & wphon_data$Language == "Nguna"), ]; # seems to be a dialect of Efate_North_Nguna (llp))?
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "enq" & wphon_data$Language == "Sau"), ]; # seems to be a dialect of Enga (enq))?
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "fau" & wphon_data$Language == "Sehudate"), ]; # seems not to exist (but there is Fayu (fau))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "jet" & wphon_data$Language == "Sko-Tiau"), ]; # seems not to exist (but there is Manem (jet))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "tbg" & wphon_data$Language == "Tairora"), ]; # seems to be whole language group (of which Tairora_Northern (tbg) is a member)
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "wah" & wphon_data$Language == "Kesui"), ]; # seems not to exist (but there is Watubela (wah))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "gah" & wphon_data$Language == "Gahuku"), ]; # seems not to exist (but there is Alekano (gah))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "nca" & wphon_data$Language == "Naho"), ]; # seems to be whole language group (of which Iyo (nca) is a member)
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "ale" & wphon_data$Language == "Aleut_Atkan"), ]; # seems not to exist (but there is Aleut (ale))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "amu" & wphon_data$Language == "Amuzgo_Huixtepec"), ]; # seems not to exist (but there is Amuzgo (amu))
wphon_data[ wphon_data$ISO.code == "cpc" & wphon_data$Language == "Axininca", c("ISO.code", "Language") ] <- c("cni", "Asháninka"); # seems to be this language
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "cni" & wphon_data$Language == "Campa"), ]; # seems not to exist (but there is Asháninka (cni))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "nsz" & wphon_data$Language == "Nisenan_Central Hill "), ]; # seems not to exist (but there is Nisenan (nsz))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "jic" & wphon_data$Language == "Jicaque"), ]; # seems to be whole language group (of which Tol (jic) is a member)
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "kzw" & wphon_data$Language == "Kipeá"), ]; # does not seem to have an ISO code (Glottocode kipe1235)
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "ign" & wphon_data$Language == "Moxo"), ]; # seems to be whole language group (of which Ignaciano (ign) is a member)
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "hur" & wphon_data$Language == "Nanaimo"), ]; # seems to be a dialect of Halkomelem (hur))?
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "aio" & wphon_data$Language == "Tai Aiton"), ]; # might be a dialect of Aiton (aio))?
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "ahk" & wphon_data$Language == "Akha_Senchai"), ]; # seems not to exist (but there is Akha (ahk))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "atb" & wphon_data$Language == "Atsi"), ]; # seems not to exist (but there is Zaiwa (atb))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "pwo" & wphon_data$Language == "Karen_Pho_Bassein"), ]; # unclear which os it is besides Karen_Pho (pwo)
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "kjp" & wphon_data$Language == "Phlong"), ]; # unclear which os it is besides Karen_Pho_Moulmein (kjp)
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "kpm" & wphon_data$Language == "Sre"), ]; # seems to be a dialect of Koho (kpm))?
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "blr" & wphon_data$Language == "Kontoi"), ]; # seems not to exist (but there is Blang (blr))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "arw" & wphon_data$Language == "Arawak"), ]; # seems not to exist (but there is Lokono (arw))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "lic" & wphon_data$Language == "Li_Baoding"), ]; # seems not to exist (but there is Hlai (lic))
wphon_data[ wphon_data$ISO.code == "lbo" & wphon_data$Language == "Jru", c("ISO.code", "Language") ] <- c("sqq", "Sou"); # seems to be this language
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "mrh" & wphon_data$Language == "Lakher"), ]; # seems not to exist (but there is Mara (mrh))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "sah" & wphon_data$Language == "Yakut"), ]; # seems not to exist (but there is Sakha (sah))
wphon_data[ wphon_data$ISO.code == "sqq" & wphon_data$Language == "Laven", c("ISO.code", "Language") ] <- c("lbo", "Laven"); # seems to be this language
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "sqq" & wphon_data$Language == "Su"), ]; # seems not to exist (but there is Sou (sqq))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "inb" & wphon_data$Language == "Quechua_Inga_Highland"), ]; # seems not to exist (but there is Quechua_Inga (inb))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "suy" & wphon_data$Language == "Tapayuna"), ]; # seems not to exist (but there is Suyá (suy))
wphon_data[ wphon_data$ISO.code == "way" & wphon_data$Language == "Wyandot", c("ISO.code") ] <- c("wya"); # seems a clearical error
wphon_data[ wphon_data$ISO.code == "esi" & wphon_data$Language == "Iñupiaq_Seward_Qawiaraq", c("ISO.code", "Language") ] <- c("esk", "Seward Alaska Inupiatun"); # seems to be this language
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "ike" & wphon_data$Language == "Inuktitut_Nunavik_north"), ]; # seems to be a dialect of Inuttut (ike))?
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "mww" & wphon_data$Language == "Petchabun"), ]; # seems to be a dialect of White_Hmong (mww))?
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "ajp" & wphon_data$Language == "Arabic_Jordanian"), ]; # seems to be a similar to Arabic_Palestinian (ajp))?
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "lbo" & wphon_data$Language == "Loven"), ]; # seems to be whole language group (of which Laven (lbo) is a member)
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "rbb" & wphon_data$Language == "Deang_Guangka"), ]; # seems not to exist (but there is Rumai (rbb))
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "ast" & wphon_data$Language == "Asturian" & wphon_data$ID == "4166"), ]; # there are two entries for Asturian: keep the 1st
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "neh" & wphon_data$Language == "Phobjip"), ]; # seems to be a dialect of Mangde (neh))?
wphon_data <- wphon_data[ !(wphon_data$ISO.code == "aap" & wphon_data$Language == "Arara do Para"), ]; # seems to be pretty much the same as Arára_Pará (aap)

# Keep only the relevant info:
wphon_data <- wphon_data[!is.na(wphon_data$ISO.code) & wphon_data$ISO.code != "" & !grepl("-", wphon_data$ISO.code, fixed=TRUE) & !is.na(wphon_data$Tonal.contrasts), 
                         c("ISO.code", "Tonal.contrasts")];

# Add Glottolog codes:
wphon_data <- merge(wphon_data, unique(glottolog_data[,c("iso639P3code", "name", "id")]), by.x="ISO.code", by.y="iso639P3code", all.x=TRUE, all.y=FALSE);
names(wphon_data)[ncol(wphon_data)] <- "glottocode";




##
## Combine all the language data ####
##

# Merge the individual datasets:
language_data <- merge(phoible_number_tones, wals_data, by="glottocode", all=TRUE);
language_data <- merge(language_data, lapsyd_data, by="glottocode", all=TRUE);
language_data <- merge(language_data, dl2007_data[,c("Tone", "glottocode")], by="glottocode", all=TRUE);
language_data <- merge(language_data, wphon_data[,c("Tonal.contrasts", "glottocode")], by="glottocode", all=TRUE);
# Add Glottolog info:
language_data <- merge(language_data, glottolog_data, by.x="glottocode", by.y="id", all=TRUE);
language_data <- merge(language_data, glottolog_data_areas, by="glottocode", all=TRUE);

# Remove all retired entries, entries without geographical info, families and sign languages:
language_data <- language_data[ !is.na(language_data$latitude) & !is.na(language_data$longitude) & 
                                    language_data$level.y != "family" & !grepl("sign", tolower(language_data$name), fixed=TRUE) &
                                    language_data$bookkeeping != "True",];


# Rename, remove and rearrange columns:
language_data <- language_data[, c("glottocode", "iso639P3code", "name.y", "level.y", # language info
                                   "family_id", "macroarea.y", "latitude.y", "longitude.y", "country_ids", # family and area
                                   "number_tone_symbols", # PHOIBLE
                                   "tone", # WALS
                                   "Tone.x", "Tone_number", "Tone_comments", # LAPSyD
                                   "Tone.y", # Dediu & Ladd (2007)
                                   "Tonal.contrasts" # WPHON
                                  )];
names(language_data) <- c("glottocode", "iso", "name", "level", # language info
                          "family", "macroarea", "latitude", "longitude", "country", # family and area
                          "ph_n_tones", # PHOIBLE
                          "wa_tone", # WALS
                          "la_tone", "la_n_tones", "la_tone_comments", # LAPSyD
                          "dl_tone", # Dediu & Ladd (2007)
                          "wp_tone" # WPHON
                        );

# Keep rows with at least one non-missing data:
language_data <- language_data[ rowSums(!is.na(language_data[, c("ph_n_tones", 
                                                                 "wa_tone",
                                                                 "la_tone", "la_n_tones", 
                                                                 "dl_tone",
                                                                 "wp_tone") ])) > 0, ];

# Order it:
language_data <- unique(language_data);
language_data <- language_data[ order(language_data$glottocode, language_data$iso, language_data$name), ];

# Recode factors making sure they have the right contrasts and ordering (if the case):
language_data$wa_tone <- ordered(language_data$wa_tone, levels=c("None", "Simple", "Complex"));
language_data$la_tone <- ordered(language_data$la_tone, levels=c("None", "Marginal", "Simple", "Moderately complex", "Complex"));
language_data$dl_tone <- factor(as.character(language_data$dl_tone), levels=c("0", "1"), labels=c("No", "Yes"));

# Check for missing data in important fields:
if( FALSE )
{
  language_data[ is.na(language_data$iso)          | language_data$iso        == "" |
                   is.na(language_data$glottocode) | language_data$glottocode == "" |
                   is.na(language_data$name)       | language_data$name       == "" |
                   is.na(language_data$level)      | language_data$level      == "" |
                   is.na(language_data$family)     | language_data$family     == "" |
                   is.na(language_data$macroarea)  | language_data$macroarea  == "", 1:9];
}
language_data$family    <- as.character(language_data$family);
language_data$macroarea <- as.character(language_data$macroarea);

# Special cases:
language_data[ language_data$iso == "hai", c("level", "family", "macroarea", "latitude", "longitude", "country") ] <- 
  language_data[ language_data$iso == "hdn", c("level", "family", "macroarea", "latitude", "longitude", "country") ]; # For Haida use the data from Northern Haida (hdn)
language_data[ language_data$iso == "din", c("level", "macroarea", "latitude", "longitude", "country") ] <- 
  language_data[ language_data$iso == "dip", c("level", "macroarea", "latitude", "longitude", "country") ]; # For Dinka use the data from Northeastern Dinka (dip)
language_data[ language_data$iso == "nep", c("level", "macroarea") ] <- 
  language_data[ language_data$iso == "npi", c("level", "macroarea") ]; # For Eastern Pahari use the data from Nepali (npi)
language_data[ language_data$iso == "jya", c("level", "macroarea") ] <- 
  list("language", "Eurasia"); # For Northern Gyalrong use the data from Tshobdun (tsho1240)
language_data[ language_data$iso == "mon", c("level", "macroarea", "latitude", "longitude") ] <- 
  language_data[ language_data$iso == "hdn", c("level", "macroarea", "latitude", "longitude") ]; # For Mongolian use the data from Halh Mongolian (khk)
language_data[ language_data$iso == "den", c("level", "macroarea", "latitude", "longitude") ] <- 
  language_data[ language_data$iso == "scs", c("level", "macroarea", "latitude", "longitude") ]; # For Slave use the data from North Slavey (scs)
language_data[ language_data$iso == "kur", c("level", "macroarea", "latitude", "longitude") ] <- 
  language_data[ language_data$iso == "ckb", c("level", "macroarea", "latitude", "longitude") ]; # For Kurdish use the data from Central Kurdish (ckb)

# Language isolates -> the family is the language itself:
s <- (is.na(language_data$family) | language_data$family == ""); language_data$family[s] <- as.character(language_data$glottocode[s]);

# Remove the remaining entries without ISO:
language_data <- language_data[ !is.na(language_data$iso) & language_data$iso != "", ];

# Save it:
save(language_data, file="./data/language/output/language_data.RData", compress="xz", compression_level=9);








