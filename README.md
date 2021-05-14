# README file

This project implements the updated analysis of the relationship between *tone* and the two "derived" alleles of *ASPM* and *Microcephalin* (*MCPH1*), and is structured as follows:

- the `data` folder contains the data needed for the analysis (and any dedicated pre-processing) and is structured into two sub-folders (`genetics` and `language`), each structured in turn into `input` (the primary data), `code` (containing an `R` script dedicated for downloading and pre-processing the input data) and `output` (containing the pre-processed data ready for use in the main analysis):

  + the `genetics` sub-folder contains the data and code related to the population frequencies of the two "derived" alleles (or their proxies):
  
    - the `input` sub-folder contains the input files (`TSV` or `CSV` formats).
      
    - the `code` contains the `R` script `00_preprocesses_genetics.R` which pre-processes and combines these various data sources.
    
    - following the pre-processing, the `output` folder will contain the data used in the analyses.
    
  + the `language` sub-folder contains the data and code related to the tone data:
  
    - the `input` sub-folder contains the input files (mostly in `TSV` or `CSV` formats, but also other formats specific to the dataset).

    - the `code` contains the `R` script `00_preprocess_language.R` which downloads, pre-processes and combines these various data sources.
    
    - following the pre-processing, the `output` folder the data used in the analyses.
    
- the `code` folder contains the `Rmarkdown` script and needed auxiliary files, and two sub-folders:

  + `figures`, where the publication-quality figures for the paper are placed, and
  
  + `cache-results` where the computationally expensive intermediate results are stored for faster compilation and reproducibility.

Please note that these data, code and results are released under GPL2 (https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html) unless otherwise specified in the original sources.

Dan Dediu [ddediu@gmail.com], May 2021


