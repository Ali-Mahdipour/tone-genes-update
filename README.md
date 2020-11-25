# README file

This project implements the updated analysis of the relationship between *tone* and the two "derived" alleles of *ASPM* and *Microcephalin* (*MCPH1*), and is structured as follows:

- the `paper` folder contains the summary of the data, methods and results written as a scientific paper;

- the `data` folder contains the data needed for the analysis (and any dedicated pre-processing) and is structured into two sub-folders (`genetics` and `language`), each structured in turn into `input` (the primary data), `code` (contaning an `R` script dedicated for downloading and pre-processing the input data) and `output` (containing the pre-processed data ready for use in the main analysis):

  + the `genetics` sub-folder contains the data and code related to the population frequencies of the two "derived" alleles (or their proxies):
  
    - the `input` sub-folder contains (all files are in `TSV` or `CSV` formats):
    
      + `populations.tsv`: contains information about all the populations and samples with genetic data from all the sources, including the source, code and description, the corresponding *ALFERD* sample ID (if any);
      + `1000genomes`, `ALFRED`, `dbSNP`, `gnomAD`, `Lahn_etal_2005`, `LDLink` and `Wong_etal_2020` contain allele frequency information collected (manually) from these databases
      + `gnomAD` and `HDGP-CEPH` also contain meta-information about some populations
      + `LDLink` also contains information about "proxies" in high LD with the two "derived" alleles.
      
    - the `code` contains the `R` script `00_preprocesses_genetics.R` which pre-processes and combines these various data sources.
    
    - following the pre-processing, the `output` folder will contain the `gene_freqs.tsv` with all the information about the samples (weigthed average allele frequencies and mapped language ISO_639_3 codes).
    
  + the `language` sub-folder contains the data and code related to the tone data:
  
    - the `input` sub-folder contains (all files are in `TSV` or `CSV` formats): `dediu-ladd-2007`, `glottolog4.1`, `lapsyd`, `phoible-v2.0.1`, `wals-2014` and `wphon` contain tone information collected (automatically or manually) from these data sources

    - the `code` contains the `R` script `00_preprocess_language.R` which donwloads, pre-processes and combines these various data sources.
    
    - following the pre-processing, the `output` folder will contain the `XZ`-compressed `R` data file `language_data.RData` with combined information about tone, and meta-information about the languages (family, macroarea and geographic coordinates).
    
- the `code` folder contains the `Rmarkdown` script `tone-genes-vXX.Rmd` and two sub-folders:

  + `figures`, where the publication-quality figures for the paper are placed, and
  
  + `cache-results` where the computationally expensive intermediate results are stored for faster compilation and reproducibility.

Please note that these data, code and results are released under GPL2 (https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html) unless otherwise specificed in the riginal sources.

Dan Dediu [ddediu@gmail.com], November 2020


