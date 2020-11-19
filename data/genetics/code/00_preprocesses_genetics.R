######################################################################################
#
# Here we assemble and preprocess the allele frequency data from several databases
#
# (c) Dan Dediu, 2020
#
######################################################################################

# Assume the current directory is the project directory!


# Libraries:
library(dplyr);
library(data.table);


##
## ASPM ####
##


##
## Load the individual datasets ####
##

# The original ASPM "derived" (or D allele) frequency data from Mekel-Bobrov et al. (2005):
d_MB2005 <- read.table("./data/genetics/input/Lahn_etal_2005/frequencies-ASPM.csv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));
d_MB2005$N_alleles <- d_MB2005$N_individuals * 2; # add number of alleles
d_MB2005$Database <- "Mekel-Bobrov et al. 2005"; # add the database as well to the data
# Make the Mekel-Bobrov et al. (2005) notations compatible with the modern ones:
d_MB2005$SNP <- "A44871G"; 
d_MB2005$Allele_major_original <- d_MB2005$Allele_major; d_MB2005$Allele_minor_original <- d_MB2005$Allele_minor;
d_MB2005$Allele_major <- ifelse(d_MB2005$Allele_major == "D", "A", "G"); d_MB2005$Allele_minor <- ifelse(d_MB2005$Allele_minor == "D", "A", "G");
d_MB2005$Maps_on_rs41310927_C <- "A";

# The Wong et al (2020) paper for Cantonese speakers:
d_W2020 <- read.table("./data/genetics/input/Wong_etal_2020/frequencies-ASPM.tsv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));
d_W2020$N_alleles <- d_W2020$N_individuals * 2; # add number of alleles
d_W2020$Allele_major <- "T"; # this was interpreted as "TRUE"
d_W2020$Database <- "Wong et al. 2020" # add the database as well to the data
d_W2020$Maps_on_rs41310927_C <- "C";

# The ALFRED data:
d_ALFRED <- read.table("./data/genetics/input/ALFRED/frequencies-ASPM.tsv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));
d_ALFRED$Maps_on_rs41310927_C <- "A";

# The 1000 genomes data:
d_1kG <- read.table("./data/genetics/input/1000genomes/frequencies-ASPM.tsv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));
d_1kG$Allele_major <- "T"; # this was interpreted as "TRUE"
d_1kG$Maps_on_rs41310927_C <- "C";

# The gnomAD data:
d_gnomAD <- read.table("./data/genetics/input/gnomAD/frequencies-ASPM.tsv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));
d_gnomAD$Database <- "gnomAD" # add the database as well to the data
d_gnomAD <- d_gnomAD[ is.na(d_gnomAD$Sex), ]; # keep both sexes only
d_gnomAD <- d_gnomAD[,c("Database", "SNP", "Allele_major", "Allele_minor", "Population", "N_alleles", "MAF", "gnomAD_id", "MA_count", "N_homozygotes")];
d_gnomAD$Maps_on_rs41310927_C[ d_gnomAD$SNP == "rs41310927" ] <- "C";
d_gnomAD$Maps_on_rs41310927_C[ d_gnomAD$SNP == "rs41308365" ] <- "A";
d_gnomAD$Maps_on_rs41310927_C[ d_gnomAD$SNP == "rs3762271" ]  <- "T";

# The LDLink data:
d_LDLink <- read.table("./data/genetics/input/LDLink/frequencies-ASPM.tsv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));
d_LDLink$Database <- "LDLink"; # add the database as well to the data
d_LDLink$N_alleles <- d_LDLink$N_individuals * 2; # add number of alleles
d_LDLink$Maps_on_rs41310927_C[ d_LDLink$SNP == "rs41310927" ]  <- "C";
d_LDLink$Maps_on_rs41310927_C[ d_LDLink$SNP == "rs3762271" ]   <- "T";
d_LDLink$Maps_on_rs41310927_C[ d_LDLink$SNP == "rs41308365" ]  <- "A";
d_LDLink$Maps_on_rs41310927_C[ d_LDLink$SNP == "rs41304071" ]  <- "T";
d_LDLink$Maps_on_rs41310927_C[ d_LDLink$SNP == "rs147068597" ] <- "A";
d_LDLink$Maps_on_rs41310927_C[ d_LDLink$SNP == "rs61819087" ]  <- "G";

# The dbSNP data:
d_dbSNP <- read.table("./data/genetics/input/dbSNP/frequencies-ASPM.tsv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));
d_dbSNP$Maps_on_rs41310927_C[ d_dbSNP$SNP == "rs41310927" ]   <- "C";
d_dbSNP$Maps_on_rs41310927_C[ d_dbSNP$SNP == "rs41308365" ]   <- "A";
d_dbSNP$Maps_on_rs41310927_C[ d_dbSNP$SNP == "rs3762271" ]    <- "T";
d_dbSNP$Maps_on_rs41310927_C[ d_dbSNP$SNP == "rs41304071" ]   <- "T";
d_dbSNP$Maps_on_rs41310927_C[ d_dbSNP$SNP == "rs61819087" ]   <- "G";


##
## Load the population descriptions and cross-links between databases ####
##

# Load the populations:
d_pops <- read.table("./data/genetics/input/populations.tsv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));

# Map all populations to the same unique population ID (Pop_UID): preference is given to an ALFRED sample, if available:
d_pops$Pop_UID <- NA;

# For ALFRED, use the sample UID:
s <- (d_pops$Source == "ALFRED"); d_pops$Pop_UID[s] <- as.character(d_pops$Code[s]);
d_ALFRED$Pop_UID <- d_ALFRED$Sample_UID;
d_ALFRED <- d_ALFRED[,c("Database", "SNP", "Allele_major", "Allele_minor", "Population", "Pop_UID", "N_alleles", "MAF", "Maps_on_rs41310927_C")];

# For 1000 genomes, use the manually mapped ALFRED sample UID for those that have it:
s <- (d_pops$Source == "1000Genomes" & !is.na(d_pops$ALFRED_sample_UID)); d_pops$Pop_UID[s] <- as.character(d_pops$ALFRED_sample_UID[s]);
s <- (d_pops$Source == "1000Genomes" & is.na(d_pops$ALFRED_sample_UID)); d_pops$Pop_UID[s] <- paste0("1KG_",as.character(d_pops$Code[s]));
s <- which(d_pops$Source == "1000Genomes"); for( i in s ) d_1kG$Pop_UID[ as.character(d_1kG$Population) == as.character(d_pops$Code[i]) ] <- as.character(d_pops$Pop_UID[i]);
d_1kG <- d_1kG[,c("Database", "SNP", "Allele_major", "Allele_minor", "Population", "Pop_UID", "N_alleles", "MAF", "Maps_on_rs41310927_C", "Genotypes")];

# For Wong et al (2020), create a new UID:
s <- (d_pops$Source == "Wong et al. 2020"); d_pops$Pop_UID[s] <- as.character(d_pops$Code[s]);
d_W2020$Pop_UID <- as.character(d_pops$Code[s]);
d_W2020 <- d_W2020[,c("Database", "SNP", "Allele_major", "Allele_minor", "Population", "Pop_UID", "N_alleles", "MAF", "Maps_on_rs41310927_C", "Gene", "N_individuals", "Genotypes")];

# For Mekel-Bobrov et al. (2005), some match ALFRED samples, but some don't (for those, create a new UIDs):
s <- which(d_pops$Source == "Mekel-Bobrov et al. 2005" & !is.na(d_pops$ALFRED_sample_UID));
d_pops$Pop_UID[s] <- as.character(d_pops$ALFRED_sample_UID[s]);
s <- which(d_pops$Source == "Mekel-Bobrov et al. 2005" & is.na(d_pops$ALFRED_sample_UID));
d_pops$Pop_UID[s] <- paste0("MB2005_", gsub(" ", "", as.character(d_pops$Code[s]), fixed = TRUE));
s <- which(d_pops$Source == "Mekel-Bobrov et al. 2005"); for( i in s ) d_MB2005$Pop_UID[ as.character(d_MB2005$Population) == as.character(d_pops$Code[i]) ] <- as.character(d_pops$Pop_UID[i]);
d_MB2005 <- d_MB2005[,c("Database", "SNP", "Allele_major", "Allele_minor", "Population", "Pop_UID", "N_alleles", "MAF", "Maps_on_rs41310927_C", "Gene", "N_individuals", "Allele_major_original", "Allele_minor_original")];

# For LDLink, individual populations map to 1000 genomes; for the others, generate new codes:
s <- which(d_pops$Source == "LDLink");
for( i in s )
{
  d_pops$Pop_UID[i] <- as.character(d_pops$Pop_UID[ d_pops$Source == "1000Genomes" & d_pops$Code == d_pops$Code[i]]);
  d_LDLink$Pop_UID[ as.character(d_LDLink$Code) == as.character(d_pops$Code[i]) ] <- as.character(d_pops$Pop_UID[i]);
}
d_LDLink <- d_LDLink[,c("Database", "SNP", "Allele_major", "Allele_minor", "Population", "Pop_UID", "N_alleles", "MAF", "Maps_on_rs41310927_C", "Code", "N_individuals", "Allele.frequencies")];

# For gnomAD, create a new UIDs:
s <- (d_pops$Source == "gnomAD"); d_pops$Pop_UID[s] <- paste0("gnomAD_", gsub(" ", "", as.character(d_pops$Code[s]), fixed = TRUE));
d_gnomAD$Pop_UID <- paste0("gnomAD_", gsub(" ", "", as.character(d_gnomAD$Population), fixed = TRUE));

# For The PAGE study, create a new UIDs:
s <- (d_pops$Source == "The PAGE Study"); d_pops$Pop_UID[s] <- paste0("PAGE_", gsub(" ", "", as.character(d_pops$Description[s]), fixed = TRUE));
s <- (d_dbSNP$Database == "The PAGE Study"); d_dbSNP$Pop_UID[s] <- paste0("PAGE_", gsub(" ", "", as.character(d_dbSNP$Population[s]), fixed = TRUE));

# For Genetic variation in the Estonian population, create a new UID:
s <- (d_pops$Source == "Genetic variation in the Estonian population"); d_pops$Pop_UID[s] <- "ESTONIAN_VAR";
s <- (d_dbSNP$Database == "Genetic variation in the Estonian population"); d_dbSNP$Pop_UID[s] <- "ESTONIAN_VAR";

# For The Avon Longitudinal Study of Parents and Children, use Code:
s <- (d_pops$Source == "The Avon Longitudinal Study of Parents and Children"); d_pops$Pop_UID[s] <- as.character(d_pops$Code[s]); s1 <- s;
s <- (d_dbSNP$Database == "The Avon Longitudinal Study of Parents and Children"); d_dbSNP$Pop_UID[s] <- unique(as.character(d_pops$Pop_UID[s1]));

# For UK 10K study - Twins, use Code:
s <- (d_pops$Source == "UK 10K study - Twins"); d_pops$Pop_UID[s] <- as.character(d_pops$Code[s]); s1 <- s;
s <- (d_dbSNP$Database == "UK 10K study - Twins"); d_dbSNP$Pop_UID[s] <- unique(as.character(d_pops$Pop_UID[s1]));

# For Northern Sweden, use Code:
s <- (d_pops$Source == "Northern Sweden"); d_pops$Pop_UID[s] <- as.character(d_pops$Code[s]); s1 <- s;
s <- (d_dbSNP$Database == "Northern Sweden"); d_dbSNP$Pop_UID[s] <- unique(as.character(d_pops$Pop_UID[s1]));

# For A Vietnamese Genetic Variation Database, create a new UID:
s <- (d_pops$Source == "A Vietnamese Genetic Variation Database"); d_pops$Pop_UID[s] <- "VIETNAM_VAR"; s1 <- s;
s <- (d_dbSNP$Database == "A Vietnamese Genetic Variation Database"); d_dbSNP$Pop_UID[s] <- unique(as.character(d_pops$Pop_UID[s1]));

# For ExAC, create a new UIDs:
s <- (d_pops$Source == "ExAC"); d_pops$Pop_UID[s] <- paste0("ExAC_", gsub(" ", "", as.character(d_pops$Code[s]), fixed = TRUE));
s <- (d_dbSNP$Database == "ExAC"); d_dbSNP$Pop_UID[s] <- paste0("ExAC_", gsub(" ", "", as.character(d_dbSNP$Population[s]), fixed = TRUE));

# For GO Exome Sequencing Project, create a new UIDs:
s <- (d_pops$Source == "GO Exome Sequencing Project"); d_pops$Pop_UID[s] <- paste0("GOEx_", gsub(" ", "", as.character(d_pops$Code[s]), fixed = TRUE));
s <- (d_dbSNP$Database == "GO Exome Sequencing Project"); d_dbSNP$Pop_UID[s] <- paste0("GOEx_", gsub(" ", "", as.character(d_dbSNP$Population[s]), fixed = TRUE));

# For TopMed, create a new UIDs:
s <- (d_pops$Source == "TopMed"); d_pops$Pop_UID[s] <- as.character(d_pops$Source[s]); s1 <- s;
s <- (d_dbSNP$Database == "TopMed"); d_dbSNP$Pop_UID[s] <- unique(as.character(d_pops$Pop_UID[s1]));

# For gnomAD - Exomes, create a new UIDs:
s <- (d_pops$Source == "gnomAD - Exomes"); d_pops$Pop_UID[s] <- paste0("gnomADexomes_", gsub(" ", "", as.character(d_pops$Code[s]), fixed = TRUE));
s <- (d_dbSNP$Database == "gnomAD - Exomes"); d_dbSNP$Pop_UID[s] <- paste0("gnomADexomes_", gsub(" ", "", as.character(d_dbSNP$Population[s]), fixed = TRUE));

# For gnomAD - Genomes, create a new UIDs:
s <- (d_pops$Source == "gnomAD - Genomes"); d_pops$Pop_UID[s] <- paste0("gnomADgenomes_", gsub(" ", "", as.character(d_pops$Code[s]), fixed = TRUE));
s <- (d_dbSNP$Database == "gnomAD - Genomes"); d_dbSNP$Pop_UID[s] <- paste0("gnomADgenomes_", gsub(" ", "", as.character(d_dbSNP$Population[s]), fixed = TRUE));

# For dbSNP, some manual matches for 1000 genomes:
d_dbSNP$Pop_UID[ d_dbSNP$Database == "1000Genomes" & d_dbSNP$Population == "Global" ] <- unique(d_pops$Pop_UID[ d_pops$Source == "1000Genomes" & d_pops$Code == "ALL" ]);
d_dbSNP$Pop_UID[ d_dbSNP$Database == "1000Genomes" & d_dbSNP$Population == "African" ] <- unique(d_pops$Pop_UID[ d_pops$Source == "1000Genomes" & d_pops$Code == "AFR" ]);
d_dbSNP$Pop_UID[ d_dbSNP$Database == "1000Genomes" & d_dbSNP$Population == "East Asian" ] <- unique(d_pops$Pop_UID[ d_pops$Source == "1000Genomes" & d_pops$Code == "EAS" ]);
d_dbSNP$Pop_UID[ d_dbSNP$Database == "1000Genomes" & d_dbSNP$Population == "Europe" ] <- unique(d_pops$Pop_UID[ d_pops$Source == "1000Genomes" & d_pops$Code == "EUR" ]);
d_dbSNP$Pop_UID[ d_dbSNP$Database == "1000Genomes" & d_dbSNP$Population == "South Asian" ] <- unique(d_pops$Pop_UID[ d_pops$Source == "1000Genomes" & d_pops$Code == "SAS" ]);
d_dbSNP$Pop_UID[ d_dbSNP$Database == "1000Genomes" & d_dbSNP$Population == "American" ] <- unique(d_pops$Pop_UID[ d_pops$Source == "1000Genomes" & d_pops$Code == "AMR" ]);

# dbSNP columns:
d_dbSNP <- d_dbSNP[,c("Database", "SNP", "Allele_major", "Allele_minor", "Population", "Pop_UID", "N_alleles", "MAF", "Maps_on_rs41310927_C", "Group")];



##
## Check which data to keep (i.e., is informative and non-redundant) and which data to combine ####
##

# Make sure the SNP and Pop_UID are character!
d_1kG$SNP    <- as.character(d_1kG$SNP);    d_1kG$Pop_UID    <- as.character(d_1kG$Pop_UID);
d_ALFRED$SNP <- as.character(d_ALFRED$SNP); d_ALFRED$Pop_UID <- as.character(d_ALFRED$Pop_UID);
d_dbSNP$SNP  <- as.character(d_dbSNP$SNP);  d_dbSNP$Pop_UID  <- as.character(d_dbSNP$Pop_UID);
d_gnomAD$SNP <- as.character(d_gnomAD$SNP); d_gnomAD$Pop_UID <- as.character(d_gnomAD$Pop_UID);
d_LDLink$SNP <- as.character(d_LDLink$SNP); d_LDLink$Pop_UID <- as.character(d_LDLink$Pop_UID);
d_MB2005$SNP <- as.character(d_MB2005$SNP); d_MB2005$Pop_UID <- as.character(d_MB2005$Pop_UID);
d_W2020$SNP  <- as.character(d_W2020$SNP);  d_W2020$Pop_UID  <- as.character(d_W2020$Pop_UID);

# d_1kG is fully contained in d_LDLink, so we disregard it!
d <- merge(d_1kG, d_LDLink, by=c("SNP", "Pop_UID"), suffixes=c("_1kG", "_LDL"), all=TRUE);
plot(d$MAF_1kG, d$MAF_LDL); cor.test(d$MAF_1kG, d$MAF_LDL); 
rm(d_1kG);

# is d_MB2005 the same as ALFRED? No: while they coincide in 54% of the cases and the overall correlation is very high (r = 0.98), 
# there are small discrepancies (on both the frequency and the number of cases) which mean that we should keep both...
d <- merge(d_MB2005, d_ALFRED, by="Pop_UID", suffixes=c("_MB", "_A"));
plot(d$MAF_MB, d$MAF_A); cor.test(d$MAF_MB, d$MAF_A); 
plot(d$N_alleles_MB, d$N_alleles_A); cor.test(d$N_alleles_MB, d$N_alleles_A); 
d$delta_MAF <- d$MAF_MB - d$MAF_A; d$delta_N <- d$N_alleles_MB - d$N_alleles_A;
d1 <- d[,c("Pop_UID", "Population_A", "MAF_MB", "MAF_A", "N_alleles_MB", "N_alleles_A", "delta_MAF", "delta_N")];
sum(d1$delta_MAF == 0) / nrow(d1);
d1 <- d1[ d1$delta_MAF != 0, ];
d1 <- d1[ order(d1$delta_MAF, d1$delta_N), ];

# LDLink vs ALFRED for rs3762271: LDLink is almost completely included in ALFRED, so do consider here only the part that is not in ALFRED:
d <- merge(d_LDLink, d_ALFRED, by=c("SNP", "Pop_UID"), suffixes=c("_LDL", "_A"));
plot(d$MAF_LDL, d$MAF_A); cor.test(d$MAF_LDL, d$MAF_A); 
plot(d$N_alleles_LDL, d$N_alleles_A); cor.test(d$N_alleles_LDL, d$N_alleles_A); 
d$delta_MAF <- d$MAF_LDL - d$MAF_A; d$delta_N <- d$N_alleles_LDL - d$N_alleles_A;
d1 <- d[,c("Pop_UID", "Population_A", "MAF_LDL", "MAF_A", "N_alleles_LDL", "N_alleles_A", "delta_MAF", "delta_N")];
sum(abs(d1$delta_MAF) < 0.001) / nrow(d1);
sum(d1$delta_N != 0) / nrow(d1); d1[ d1$delta_N != 0, ];
setdiff(d_LDLink$Pop_UID[ d_LDLink$SNP == "rs3762271" ], d_ALFRED$Pop_UID);
d_LDLink_keep <- d_LDLink[ !(d_LDLink$SNP == "rs3762271" & d_LDLink$Pop_UID %in% d_ALFRED$Pop_UID), ];
d_LDLink_keep <- d_LDLink_keep[ !(d_LDLink_keep$Pop_UID %in% c("1KG_ALL", "1KG_AFR", "1KG_AMR", "1KG_EAS", "1KG_EUR", "1KG_SAS")), ]; # don't keep populations that are too vague or too large to be useful

# dbSNP: remove all populations that are too vague or too large to be of any use:
d_dbSNP_keep <- d_dbSNP[ (d_dbSNP$Database == "gnomAD - Exomes" & d_dbSNP$Population == "Ashkenazi Jewish") | 
                      (d_dbSNP$Database == "gnomAD - Genomes" & d_dbSNP$Population == "Ashkenazi Jewish") | 
                      (d_dbSNP$Database == "The PAGE Study" & d_dbSNP$Population %in% c("AfricanAmerican", "Mexican", "PuertoRican", "NativeHawaiian", "Cuban", "Dominican", "CentralAmerican", "SouthAmerican")) |
                      (d_dbSNP$Database == "Genetic variation in the Estonian population") | 
                      (d_dbSNP$Database == "The Avon Longitudinal Study of Parents and Children") | 
                      (d_dbSNP$Database == "UK 10K study - Twins") |
                      (d_dbSNP$Database == "Northern Sweden") | 
                      (d_dbSNP$Database == "A Vietnamese Genetic Variation Database"), ];

# gnomAD: remove all populations that are too vague or too large to be of any use:
d_gnomAD_keep <- d_gnomAD[ (d_gnomAD$Population %in% c("asj", "kor", "jpn", "fin", "bgr", "est", "swe")), ];


##
## Combine the SNPs in a single genetic info ####
##

# Keep only the essential info and go wide-format:
d_MB2005_wide <- d_MB2005[,c("SNP", "Pop_UID", "N_alleles", "MAF")]; d_MB2005_wide <- dcast(setDT(d_MB2005_wide), Pop_UID ~ SNP, value.var=c("MAF", "N_alleles"));
d_W2020_wide <- d_W2020[,c("SNP", "Pop_UID", "N_alleles", "MAF")]; d_W2020_wide <- dcast(setDT(d_W2020_wide), Pop_UID ~ SNP, value.var=c("MAF", "N_alleles"));
d_ALFRED_wide <- d_ALFRED[,c("SNP", "Pop_UID", "N_alleles", "MAF")]; d_ALFRED_wide <- dcast(setDT(d_ALFRED_wide), Pop_UID ~ SNP, value.var=c("MAF", "N_alleles"));
d_LDLink_wide <- d_LDLink_keep[,c("SNP", "Pop_UID", "N_alleles", "MAF")]; d_LDLink_wide <- dcast(setDT(d_LDLink_wide), Pop_UID ~ SNP, value.var=c("MAF", "N_alleles"));
d_dbSNP_wide <- d_dbSNP_keep[,c("SNP", "Pop_UID", "N_alleles", "MAF")]; d_dbSNP_wide <- dcast(setDT(d_dbSNP_wide), Pop_UID ~ SNP, value.var=c("MAF", "N_alleles"));
d_gnomAD_wide <- d_gnomAD_keep[,c("SNP", "Pop_UID", "N_alleles", "MAF")]; d_gnomAD_wide <- dcast(setDT(d_gnomAD_wide), Pop_UID ~ SNP, value.var=c("MAF", "N_alleles"));

d_all_wide <- merge(d_MB2005_wide, d_W2020_wide, by="Pop_UID", suffixes=c("_MB", "_W"), all=TRUE);
d_all_wide <- merge(d_all_wide, d_ALFRED_wide, by="Pop_UID", suffixes=c("", "_AL"), all=TRUE);
d_all_wide <- merge(d_all_wide, d_LDLink_wide, by="Pop_UID", suffixes=c("", "_LD"), all=TRUE);
d_all_wide <- merge(d_all_wide, d_dbSNP_wide, by="Pop_UID", suffixes=c("", "_SN"), all=TRUE);
d_all_wide <- merge(d_all_wide, d_gnomAD_wide, by="Pop_UID", suffixes=c("", "_GN"), all=TRUE);
d_all_wide <- as.data.frame(d_all_wide);

# Correlations:
pairs(d_all_wide[,grep("MAF_",names(d_all_wide),fixed=TRUE)], 
      upper.panel=function(x, y, ...)
        { 
          par(usr = c(0, 1, 0, 1)); 
          r <- NULL; try(r <- cor.test(x, y), silent=TRUE); 
          m <- NULL; try(m <- lm(y ~ x), silent=TRUE);
          if( !is.null(r) ) text(0.5, 0.5, sprintf("r=%.3f\na=%.2f, b=%.2f",r$estimate,coef(m)["(Intercept)"],coef(m)["x"]), cex=1.0); 
        });

# This confirms that they are very highly correlated: so, combine them into a single MAF per population:
snps <- substring(names(d_all_wide)[ grep("MAF_", names(d_all_wide), fixed=TRUE) ], nchar("MAF_")+1); # select the loci:
d_all_wide$N_alleles_total <- rowSums(d_all_wide[,paste0("N_alleles_",snps)], na.rm=TRUE); # total number of alleles
d_all_wide$N_databases <- rowSums(!is.na(d_all_wide[,paste0("MAF_",snps)]), na.rm=TRUE); # number of databases with actual data
d_all_wide$MAF_avg <- rowMeans(d_all_wide[,paste0("MAF_",snps)], na.rm=TRUE); # raw average of the MAFs
d_all_wide$MAF_wavg <- rowSums(d_all_wide[,paste0("MAF_",snps)] * d_all_wide[,paste0("N_alleles_",snps)], na.rm=TRUE) / 
  rowSums(d_all_wide[,paste0("N_alleles_",snps)], na.rm=TRUE); # weigthed average of the MAFs
cor.test(d_all_wide$MAF_avg, d_all_wide$MAF_wavg); # the raw average and weighted average are highly correlated

# Make it clear that this is about ASPM (and replace MAF by the frequency of the "derived" allele, and rearrange columns):
d_all_wide <- d_all_wide[,c("Pop_UID", "N_alleles_total", "MAF_avg", "MAF_wavg", "N_databases")];
names(d_all_wide) <- c("Pop_UID","ASPM_n_alleles", "ASPM_freq_avg", "ASPM_freq_wavg", "ASPM_n_databases");



##
## MCPH1 ####
##

##
## Load the individual datasets ####
##

rm(d_MB2005, d_W2020, d_ALFRED, d_gnomAD, d_LDLink, d_dbSNP);  # reuse the variables names from ASPM  but make sure we don't carry any unwanted garbage over...

# The original MCPH1 "derived" (or D allele) frequency data from Evans et al. (2005):
d_MB2005 <- read.table("./data/genetics/input/Lahn_etal_2005/frequencies-MCPH1.csv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));
d_MB2005$N_alleles <- d_MB2005$N_individuals * 2; # add number of alleles
d_MB2005$Database <- "Mekel-Bobrov et al. 2005"; # add the database as well to the data
# Make the Evans et al. (2005) notations compatible with the modern ones:
d_MB2005$SNP <- "G37995C"; 
d_MB2005$Allele_major_original <- d_MB2005$Allele_major; d_MB2005$Allele_minor_original <- d_MB2005$Allele_minor;
d_MB2005$Allele_major <- ifelse(d_MB2005$Allele_major == "D", "C", "G"); d_MB2005$Allele_minor <- ifelse(d_MB2005$Allele_minor == "D", "C", "G");
d_MB2005$Maps_on_rs930557_C <- "C";

# The Wong et al (2020) paper for Cantonese speakers:
d_W2020 <- read.table("./data/genetics/input/Wong_etal_2020/frequencies-MCPH1.tsv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));
d_W2020$N_alleles <- d_W2020$N_individuals * 2; # add number of alleles
d_W2020$Database <- "Wong et al. 2020" # add the database as well to the data
d_W2020$Maps_on_rs930557_C <- "C";

# The ALFRED data:
d_ALFRED <- read.table("./data/genetics/input/ALFRED/frequencies-MCHP1.tsv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));
d_ALFRED$Maps_on_rs930557_C <- "G";
d_ALFRED$MAF <- (1.0 - d_ALFRED$MAF); # the allele A for rs930557 corresponds to the "derived" allele (thus the major allele) of G37995C

# The 1000 genomes data:
d_1kG <- read.table("./data/genetics/input/1000genomes/frequencies-MCPH1.csv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));
d_1kG$Maps_on_rs930557_C <- "C";

# The gnomAD data:
d_gnomAD <- NULL; # this is already contained in other databases

# The LDLink data:
d_LDLink <- read.table("./data/genetics/input/LDLink/frequencies-MCPH1.tsv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));
d_LDLink$Database <- "LDLink"; # add the database as well to the data
d_LDLink$N_alleles <- d_LDLink$N_individuals * 2; # add number of alleles
d_LDLink$Maps_on_rs930557_C <- "C";

# The dbSNP data:
d_dbSNP <- read.table("./data/genetics/input/dbSNP/frequencies-MCPH1.tsv", header=TRUE, sep="\t", quote="", na.strings=c("NA", ""));
d_dbSNP$Maps_on_rs930557_C <- "C";


##
## Cross-links between databases ####
##

# For ALFRED, use the sample UID:
d_ALFRED$Pop_UID <- d_ALFRED$Sample_UID;
d_ALFRED <- d_ALFRED[,c("Database", "SNP", "Allele_major", "Allele_minor", "Population", "Pop_UID", "N_alleles", "MAF", "Maps_on_rs930557_C")];

# For 1000 genomes, use the manually mapped ALFRED sample UID for those that have it:
s <- which(d_pops$Source == "1000Genomes"); for( i in s ) d_1kG$Pop_UID[ as.character(d_1kG$Population) == as.character(d_pops$Code[i]) ] <- as.character(d_pops$Pop_UID[i]);
d_1kG <- d_1kG[,c("Database", "SNP", "Allele_major", "Allele_minor", "Population", "Pop_UID", "N_alleles", "MAF", "Maps_on_rs930557_C", "Genotypes")];

# For Wong et al (2020), create a new UID:
s <- (d_pops$Source == "Wong et al. 2020"); d_W2020$Pop_UID <- as.character(d_pops$Code[s]);
d_W2020 <- d_W2020[,c("Database", "SNP", "Allele_major", "Allele_minor", "Population", "Pop_UID", "N_alleles", "MAF", "Maps_on_rs930557_C", "Gene", "N_individuals", "Genotypes")];

# For Evans et al. (2005), some match ALFRED samples, but some don't (for those, create a new UIDs):
s <- which(d_pops$Source == "Mekel-Bobrov et al. 2005"); for( i in s ) d_MB2005$Pop_UID[ as.character(d_MB2005$Population) == as.character(d_pops$Code[i]) ] <- as.character(d_pops$Pop_UID[i]);
d_MB2005 <- d_MB2005[,c("Database", "SNP", "Allele_major", "Allele_minor", "Population", "Pop_UID", "N_alleles", "MAF", "Maps_on_rs930557_C", "Gene", "N_individuals", "Allele_major_original", "Allele_minor_original")];

# For LDLink, individual populations map to 1000 genomes; for the others, generate new codes:
s <- which(d_pops$Source == "LDLink");
for( i in s )
{
  d_LDLink$Pop_UID[ as.character(d_LDLink$Code) == as.character(d_pops$Code[i]) ] <- as.character(d_pops$Pop_UID[i]);
}
d_LDLink <- d_LDLink[,c("Database", "SNP", "Allele_major", "Allele_minor", "Population", "Pop_UID", "N_alleles", "MAF", "Maps_on_rs930557_C", "Code", "N_individuals")];

# For The PAGE study, create a new UIDs:
s <- (d_dbSNP$Database == "The PAGE Study"); d_dbSNP$Pop_UID[s] <- paste0("PAGE_", gsub(" ", "", as.character(d_dbSNP$Population[s]), fixed = TRUE));

# For Genetic variation in the Estonian population, create a new UID:
s <- (d_dbSNP$Database == "Genetic variation in the Estonian population"); d_dbSNP$Pop_UID[s] <- "ESTONIAN_VAR";

# For The Avon Longitudinal Study of Parents and Children, use Code:
s <- (d_dbSNP$Database == "The Avon Longitudinal Study of Parents and Children"); d_dbSNP$Pop_UID[s] <- unique(as.character(d_pops$Pop_UID[d_pops$Source == "The Avon Longitudinal Study of Parents and Children"]));

# For UK 10K study - Twins, use Code:
s <- (d_dbSNP$Database == "UK 10K study - Twins"); d_dbSNP$Pop_UID[s] <- unique(as.character(d_pops$Pop_UID[d_pops$Source == "UK 10K study - Twins"]));

# For Northern Sweden, use Code:
s <- (d_dbSNP$Database == "Northern Sweden"); d_dbSNP$Pop_UID[s] <- unique(as.character(d_pops$Pop_UID[d_pops$Source == "Northern Sweden"]));

# For A Vietnamese Genetic Variation Database, create a new UID:
s <- (d_dbSNP$Database == "A Vietnamese Genetic Variation Database"); d_dbSNP$Pop_UID[s] <- unique(as.character(d_pops$Pop_UID[d_pops$Source == "A Vietnamese Genetic Variation Database"]));

# For ExAC, create a new UIDs:
s <- (d_dbSNP$Database == "ExAC"); d_dbSNP$Pop_UID[s] <- paste0("ExAC_", gsub(" ", "", as.character(d_dbSNP$Population[s]), fixed = TRUE));

# For GO Exome Sequencing Project, create a new UIDs:
s <- (d_dbSNP$Database == "GO Exome Sequencing Project"); d_dbSNP$Pop_UID[s] <- paste0("GOEx_", gsub(" ", "", as.character(d_dbSNP$Population[s]), fixed = TRUE));

# For TopMed, create a new UIDs:
s <- (d_dbSNP$Database == "TopMed"); d_dbSNP$Pop_UID[s] <- unique(as.character(d_pops$Pop_UID[d_pops$Source == "TopMed"]));

# For gnomAD - Exomes, create a new UIDs:
s <- (d_dbSNP$Database == "gnomAD - Exomes"); d_dbSNP$Pop_UID[s] <- paste0("gnomADexomes_", gsub(" ", "", as.character(d_dbSNP$Population[s]), fixed = TRUE));

# For gnomAD - Genomes, create a new UIDs:
s <- (d_dbSNP$Database == "gnomAD - Genomes"); d_dbSNP$Pop_UID[s] <- paste0("gnomADgenomes_", gsub(" ", "", as.character(d_dbSNP$Population[s]), fixed = TRUE));

# For FINRISK, create a new UIDs:
s <- (d_pops$Source == "FINRISK"); d_pops$Pop_UID[s] <- "FINRISK";
s <- (d_dbSNP$Database == "FINRISK"); d_dbSNP$Pop_UID[s] <- "FINRISK";

# For Genome of the Netherlands Release 5, create a new UIDs:
s <- (d_pops$Source == "Genome of the Netherlands Release 5"); d_pops$Pop_UID[s] <- "GenNed5";
s <- (d_dbSNP$Database == "Genome of the Netherlands Release 5"); d_dbSNP$Pop_UID[s] <- "GenNed5";

# For KOREAN population from KRGDB, create a new UIDs:
s <- (d_pops$Source == "KOREAN population from KRGDB"); d_pops$Pop_UID[s] <- "KRGDB";
s <- (d_dbSNP$Database == "KOREAN population from KRGDB"); d_dbSNP$Pop_UID[s] <- "KRGDB";

# For Qatari, create a new UIDs:
s <- (d_pops$Source == "Qatari"); d_pops$Pop_UID[s] <- "Qatari";
s <- (d_dbSNP$Database == "Qatari"); d_dbSNP$Pop_UID[s] <- "Qatari";

# For The Danish reference pan genome, create a new UIDs:
s <- (d_pops$Source == "The Danish reference pan genome"); d_pops$Pop_UID[s] <- "GenDan";
s <- (d_dbSNP$Database == "The Danish reference pan genome"); d_dbSNP$Pop_UID[s] <- "GenDan";

# For dbSNP, some manual matches for 1000 genomes:
d_dbSNP$Pop_UID[ d_dbSNP$Database == "1000Genomes" & d_dbSNP$Population == "Global" ] <- unique(d_pops$Pop_UID[ d_pops$Source == "1000Genomes" & d_pops$Code == "ALL" ]);
d_dbSNP$Pop_UID[ d_dbSNP$Database == "1000Genomes" & d_dbSNP$Population == "African" ] <- unique(d_pops$Pop_UID[ d_pops$Source == "1000Genomes" & d_pops$Code == "AFR" ]);
d_dbSNP$Pop_UID[ d_dbSNP$Database == "1000Genomes" & d_dbSNP$Population == "East Asian" ] <- unique(d_pops$Pop_UID[ d_pops$Source == "1000Genomes" & d_pops$Code == "EAS" ]);
d_dbSNP$Pop_UID[ d_dbSNP$Database == "1000Genomes" & d_dbSNP$Population == "Europe" ] <- unique(d_pops$Pop_UID[ d_pops$Source == "1000Genomes" & d_pops$Code == "EUR" ]);
d_dbSNP$Pop_UID[ d_dbSNP$Database == "1000Genomes" & d_dbSNP$Population == "South Asian" ] <- unique(d_pops$Pop_UID[ d_pops$Source == "1000Genomes" & d_pops$Code == "SAS" ]);
d_dbSNP$Pop_UID[ d_dbSNP$Database == "1000Genomes" & d_dbSNP$Population == "American" ] <- unique(d_pops$Pop_UID[ d_pops$Source == "1000Genomes" & d_pops$Code == "AMR" ]);

# dbSNP columns:
d_dbSNP <- d_dbSNP[,c("Database", "SNP", "Allele_major", "Allele_minor", "Population", "Pop_UID", "N_alleles", "MAF", "Maps_on_rs930557_C", "Group")];



##
## Check which data to keep (i.e., is informative and non-redundant) and which data to combine ####
##

# Make sure the SNP and Pop_UID are character!
d_1kG$SNP    <- as.character(d_1kG$SNP);    d_1kG$Pop_UID    <- as.character(d_1kG$Pop_UID);
d_ALFRED$SNP <- as.character(d_ALFRED$SNP); d_ALFRED$Pop_UID <- as.character(d_ALFRED$Pop_UID);
d_dbSNP$SNP  <- as.character(d_dbSNP$SNP);  d_dbSNP$Pop_UID  <- as.character(d_dbSNP$Pop_UID);
d_LDLink$SNP <- as.character(d_LDLink$SNP); d_LDLink$Pop_UID <- as.character(d_LDLink$Pop_UID);
d_MB2005$SNP <- as.character(d_MB2005$SNP); d_MB2005$Pop_UID <- as.character(d_MB2005$Pop_UID);
d_W2020$SNP  <- as.character(d_W2020$SNP);  d_W2020$Pop_UID  <- as.character(d_W2020$Pop_UID);

# d_1kG is fully contained in d_LDLink, so we disregard it!
d <- merge(d_1kG, d_LDLink, by=c("SNP", "Pop_UID"), suffixes=c("_1kG", "_LDL"), all=TRUE);
plot(d$MAF_1kG, d$MAF_LDL); cor.test(d$MAF_1kG, d$MAF_LDL); 
rm(d_1kG);

# is d_MB2005 the same as ALFRED? No: while they coincide in 13% of the cases and the overall correlation is very high (r = 0.98), 
# there are small discrepancies (on both the frequency and the number of cases) which mean that we should keep both...
d <- merge(d_MB2005, d_ALFRED, by="Pop_UID", suffixes=c("_MB", "_A"));
plot(d$MAF_MB, d$MAF_A); cor.test(d$MAF_MB, d$MAF_A); 
plot(d$N_alleles_MB, d$N_alleles_A); cor.test(d$N_alleles_MB, d$N_alleles_A); 
d$delta_MAF <- d$MAF_MB - d$MAF_A; d$delta_N <- d$N_alleles_MB - d$N_alleles_A;
d1 <- d[,c("Pop_UID", "Population_A", "MAF_MB", "MAF_A", "N_alleles_MB", "N_alleles_A", "delta_MAF", "delta_N")];
sum(d1$delta_MAF == 0) / nrow(d1);
d1 <- d1[ d1$delta_MAF != 0, ];
d1 <- d1[ order(d1$delta_MAF, d1$delta_N), ];

# dbSNP: remove all populations that are too vague or too large to be of any use:
d_dbSNP_keep <- d_dbSNP[ (d_dbSNP$Database == "gnomAD - Exomes" & d_dbSNP$Population == "Ashkenazi Jewish") | 
                           (d_dbSNP$Database == "gnomAD - Genomes" & d_dbSNP$Population == "Ashkenazi Jewish") | 
                           (d_dbSNP$Database == "The PAGE Study" & d_dbSNP$Population %in% c("AfricanAmerican", "Mexican", "PuertoRican", "NativeHawaiian", "Cuban", "Dominican", "CentralAmerican", "SouthAmerican")) |
                           (d_dbSNP$Database == "Genetic variation in the Estonian population") | 
                           (d_dbSNP$Database == "The Avon Longitudinal Study of Parents and Children") | 
                           (d_dbSNP$Database == "UK 10K study - Twins") |
                           (d_dbSNP$Database == "Northern Sweden") | 
                           (d_dbSNP$Database == "A Vietnamese Genetic Variation Database") | 
                           (d_dbSNP$Database == "FINRISK") |
                           (d_dbSNP$Database == "Genome of the Netherlands Release 5") |
                           (d_dbSNP$Database == "KOREAN population from KRGDB") | 
                           (d_dbSNP$Database == "Qatari") |
                           (d_dbSNP$Database == "The Danish reference pan genome"), ];


##
## Combine the SNPs in a single genetic info ####
##

# Keep only the essential info and go wide-format:
d_MB2005_wide <- d_MB2005[,c("SNP", "Pop_UID", "N_alleles", "MAF")]; d_MB2005_wide <- dcast(setDT(d_MB2005_wide), Pop_UID ~ SNP, value.var=c("MAF", "N_alleles"));
d_W2020_wide <- d_W2020[,c("SNP", "Pop_UID", "N_alleles", "MAF")]; d_W2020_wide <- dcast(setDT(d_W2020_wide), Pop_UID ~ SNP, value.var=c("MAF", "N_alleles"));
d_ALFRED_wide <- d_ALFRED[,c("SNP", "Pop_UID", "N_alleles", "MAF")]; d_ALFRED_wide <- dcast(setDT(d_ALFRED_wide), Pop_UID ~ SNP, value.var=c("MAF", "N_alleles"));
d_LDLink_wide <- d_LDLink[,c("SNP", "Pop_UID", "N_alleles", "MAF")]; d_LDLink_wide <- dcast(setDT(d_LDLink_wide), Pop_UID ~ SNP, value.var=c("MAF", "N_alleles"));
d_dbSNP_wide <- d_dbSNP_keep[,c("SNP", "Pop_UID", "N_alleles", "MAF")]; d_dbSNP_wide <- dcast(setDT(d_dbSNP_wide), Pop_UID ~ SNP, value.var=c("MAF", "N_alleles"));

d_all_wide2 <- merge(d_MB2005_wide, d_W2020_wide, by="Pop_UID", suffixes=c("_MB", "_W"), all=TRUE);
d_all_wide2 <- merge(d_all_wide2, d_ALFRED_wide, by="Pop_UID", suffixes=c("", "_AL"), all=TRUE);
d_all_wide2 <- merge(d_all_wide2, d_LDLink_wide, by="Pop_UID", suffixes=c("", "_LD"), all=TRUE);
d_all_wide2 <- merge(d_all_wide2, d_dbSNP_wide, by="Pop_UID", suffixes=c("", "_SN"), all=TRUE);
d_all_wide2 <- as.data.frame(d_all_wide2);

# Correlations:
pairs(d_all_wide2[,grep("MAF_",names(d_all_wide2),fixed=TRUE)], 
      upper.panel=function(x, y, ...)
        { 
          par(usr = c(0, 1, 0, 1)); 
          r <- NULL; try(r <- cor.test(x, y), silent=TRUE); 
          m <- NULL; try(m <- lm(y ~ x), silent=TRUE);
          if( !is.null(r) ) text(0.5, 0.5, sprintf("r=%.3f\na=%.2f, b=%.2f",r$estimate,coef(m)["(Intercept)"],coef(m)["x"]), cex=1.0); 
        });

# This confirms that they are very highly correlated: so, combine them into a single MAF per population:
snps <- substring(names(d_all_wide2)[ grep("MAF_", names(d_all_wide2), fixed=TRUE) ], nchar("MAF_")+1); # select the loci:
d_all_wide2$N_alleles_total <- rowSums(d_all_wide2[,paste0("N_alleles_",snps)], na.rm=TRUE); # total number of alleles
d_all_wide2$N_databases <- rowSums(!is.na(d_all_wide2[,paste0("MAF_",snps)]), na.rm=TRUE); # number of databases with actual data
d_all_wide2$MAF_avg <- rowMeans(d_all_wide2[,paste0("MAF_",snps)], na.rm=TRUE); # raw average of the MAFs
d_all_wide2$MAF_wavg <- rowSums(d_all_wide2[,paste0("MAF_",snps)] * d_all_wide2[,paste0("N_alleles_",snps)], na.rm=TRUE) / 
  rowSums(d_all_wide2[,paste0("N_alleles_",snps)], na.rm=TRUE); # weigthed average of the MAFs
cor.test(d_all_wide2$MAF_avg, d_all_wide2$MAF_wavg); # the raw average and weighted average are highly correlated

# Make it clear that this about MCPH1 (and replace MAF by the frequency of the "derived" allele, and rearrange columns):
d_all_wide2 <- d_all_wide2[,c("Pop_UID", "N_alleles_total", "MAF_avg", "MAF_wavg", "N_databases")];
names(d_all_wide2) <- c("Pop_UID", "MCPH1_n_alleles", "MCPH1_freq_avg", "MCPH1_freq_wavg", "MCPH1_n_databases");
d_all_wide2 <- d_all_wide2[ -grep("1KG_",d_all_wide2$Pop_UID,fixed=TRUE), ]; # remove the 1000 genomes generic populations

# We're talking about the "derived" allele, so make the frequencies about it:
d_all_wide2$MCPH1_freq_avg <- (1.0 - d_all_wide2$MCPH1_freq_avg); d_all_wide2$MCPH1_freq_wavg <- (1.0 - d_all_wide2$MCPH1_freq_wavg);

# Add this info the the ASPM data:
d_all_wide <- merge(d_all_wide, d_all_wide2, by="Pop_UID", all=TRUE);



##
## Add language and meta-population info ####
##

# Add language data (ISO 639-3 codes); where multiple codes are possible, split them on separate rows:
d_all_wide_lgs <- do.call(rbind, lapply(1:nrow(d_all_wide), function(i)
{
  # Get the Pop_UID:
  pop_uid <- d_all_wide$Pop_UID[i];
  if( is.na(pop_uid) || pop_uid == "" ) stop(paste0("Undefined Pop_UID for entry ",i," !\n"));
  
  # Find the entry/entries with defined ISO codes:
  s <- (d_pops$Pop_UID == pop_uid & !is.na(d_pops$Language_ISOs) & d_pops$Language_ISOs != "");
  if( sum(s) == 0 )
  {
    warning(paste0("No language info for Pop_UID ",pop_uid,": ignoring it..."));
    return (NULL);
  }
  isos <- trimws(unlist(strsplit(as.character(d_pops$Language_ISOs[s]), ",", fixed=TRUE)));
  isos <- isos[ !is.na(isos) & isos != "" & isos != "?" ]; 
  if( is.null(isos) || length(isos) == 0 )
  {
    warning(paste0("No usable language info for Pop_UID ",pop_uid,": ignoring it..."));
    return (NULL);
  } else
  {
    return (cbind(Pop_UID=d_all_wide[i,1], "ISO_639_3"=isos, d_all_wide[i,2:ncol(d_all_wide)], row.names = NULL));
  }
}));

# Add meta-population info:
d_all_wide_lgs <- merge(d_all_wide_lgs, unique(d_pops[!is.na(d_pops$Metapopulation_ID), c("Pop_UID", "Metapopulation_ID", "Metapopulation_name")]), by="Pop_UID", all.x=TRUE, all.y=FALSE);
d_all_wide_lgs <- d_all_wide_lgs[,c("Pop_UID", "Metapopulation_ID", "Metapopulation_name", 
                                    "ISO_639_3", 
                                    "ASPM_n_alleles", "ASPM_freq_avg", "ASPM_freq_wavg", "ASPM_n_databases", 
                                    "MCPH1_n_alleles", "MCPH1_freq_avg", "MCPH1_freq_wavg", "MCPH1_n_databases")];

  

##
## Check the frequencies for multiple samples within the same metapopulation ####
##

d <- unique(d_all_wide_lgs[, -which("ISO_639_3" == names(d_all_wide_lgs))]); # remove language info
# for ASPM:
tmp <- d %>%
  filter(!is.na(ASPM_freq_wavg)) %>%
  select(Pop_UID, starts_with("Metapopulation_"), starts_with("ASPM_")) %>%
  group_by(Metapopulation_ID) %>%
  filter(n() > 1) %>%
  summarise("Metapopulation_name"=unique(Metapopulation_name), "N_samples"=n(),
            "range_ASPM"=max(ASPM_freq_wavg)-min(ASPM_freq_wavg), "min_ASPM"=min(ASPM_freq_wavg), "max_ASPM"=max(ASPM_freq_wavg), 
            "mean_ASPM"=mean(ASPM_freq_wavg), "Wmean_ASPM"=sum(ASPM_freq_wavg * ASPM_n_alleles) / sum(ASPM_n_alleles)) %>%
  arrange(desc(range_ASPM));
# View(tmp) # -> some are pretty big, so let's keep these separate

# for MCPH1:
tmp <- d %>%
  filter(!is.na(MCPH1_freq_wavg)) %>%
  select(Pop_UID, starts_with("Metapopulation_"), starts_with("MCPH1_")) %>%
  group_by(Metapopulation_ID) %>%
  filter(n() > 1) %>%
  summarise("Metapopulation_name"=unique(Metapopulation_name), "N_samples"=n(),
            "range_MCPH1"=max(MCPH1_freq_wavg)-min(MCPH1_freq_wavg), "min_MCPH1"=min(MCPH1_freq_wavg), "max_MCPH1"=max(MCPH1_freq_wavg), 
            "mean_MCPH1"=mean(MCPH1_freq_wavg), "Wmean_MCPH1"=sum(MCPH1_freq_wavg * MCPH1_n_alleles) / sum(MCPH1_n_alleles)) %>%
  arrange(desc(range_MCPH1));
# View(tmp) # -> some are pretty big, so let's keep these separate



##
## Add ALFRED (proxy) sample info ####
##

# What are the ALFRED sample UID's corresponding for these populations?
d_all_wide_lgs$ALFRED_SUID <- NA;

# For those that already have ALFRED UIDs', it's simple:
s <- grep("SA", d_all_wide_lgs$Pop_UID, fixed=TRUE); d_all_wide_lgs$ALFRED_SUID[s] <- trimws(as.character(d_all_wide_lgs$Pop_UID[s]));

# Now, do it manually for the others, trying to find the best matching ALFRED sample with enough genetic data (+ we prefer to use samples already in our database):
# remember that we do this mapping so that we can compute geentic distances between populations, so a degree of approximation is ok...

# See which populations (still) have no ALFRED equivalents
if( FALSE ) unique(d_all_wide_lgs[ is.na(d_all_wide_lgs$ALFRED_SUID), c("Pop_UID", "Metapopulation_ID", "Metapopulation_name") ]); 

# Wong et al. 2020 can be mapped to "SA004059S" (Han Chinese South, China (CHS), part of the 1000 Genomes) as the samples from Hong Kong are genotyped at very few loci:
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "WONG2020" ] <- "SA004059S";

# Genetic variation in the Estonian population & gnomAD[est] maps to "SA003028N" (Estonian):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID %in% c("ESTONIAN_VAR", "gnomAD_est") ] <- "SA003028N";

# The Avon Longitudinal Study of Parents and Children & UK 10K study - Twins map to "SA004050J"	(British from England and Scotland (GBR)):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID %in% c("AVON", "UK10K") ] <- "SA004050J";

# Genetic variation in the Northern Sweden population & gnomAD[swe] maps to "SA003445Q" (individuals with Swedish names):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID %in% c("ACPOP", "gnomAD_swe") ] <- "SA003445Q";

# A Vietnamese Genetic Variation Database maps to "SA004249T"	(Kinh in Ho Chi Minh City, Vietnam (KHV)):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "VIETNAM_VAR" ] <- "SA004249T";

# Ashkenazi Jewish maps to "SA004372Q" (Ashkenazi):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID %in% c("gnomADexomes_AshkenaziJewish", "gnomADgenomes_AshkenaziJewish", "gnomAD_asj") ] <- "SA004372Q";

# gnomAD[bgr] maps to "SA003452O" (unrelated Bulgarians):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "gnomAD_bgr" ] <- "SA003452O";

# gnomAD[jpn] maps to "SA004060K"	(Japanese in Tokyo, Japan (JPT)):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "gnomAD_jpn" ] <- "SA004060K";

# gnomAD[kor] maps to "SA003027M"	(Koreans):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "gnomAD_kor" ] <- "SA003027M";

# gnomAD[fin] maps to "SA004049R" (Finnish in Finland (FIN)):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "gnomAD_fin" ] <- "SA004049R";

# Mekel-Bobrov et al. 2005's "Turu" arguably maps to "SA001819T" (Bantu (Kenya), HGDP-CEPH):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "MB2005_Turu" ] <- "SA001819T";

# Mekel-Bobrov et al. 2005's "Bamoun" arguably maps to "SA001838U" (healthy, unrelated Bamileke and Bantu individuals from Cameroon):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "MB2005_Bamoun" ] <- "SA001838U";

# Mekel-Bobrov et al. 2005's "Bakola Pygmy" arguably maps to "SA002256P" (Biaka Pygmies, HGDP-CEPH):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "MB2005_BakolaPygmy" ] <- "SA002256P";

# Mekel-Bobrov et al. 2005's "Zime" arguably maps to "SA000405J" (Chadians):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "MB2005_Zime" ] <- "SA000405J";

# The PAGE Study's "AfricanAmerican" maps to "SA004047P" (African Ancestry in SW USA (ASW)):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "PAGE_AfricanAmerican" ] <- "SA004047P";

# The PAGE Study's "Mexican" maps to "SA004110G" (Mexican Ancestry in Los Angeles, CA (MXL)):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "PAGE_Mexican" ] <- "SA004110G";

# The PAGE Study's "PuertoRican" maps to "SA004111H" (Puerto Rican in Puerto Rico (PUR)):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "PAGE_PuertoRican" ] <- "SA004111H";

# The PAGE Study's "NativeHawaiian" arguably maps to "SA004382R" (Micronesians):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "PAGE_NativeHawaiian" ] <- "SA004382R";

# The PAGE Study's "Cuban" arguably maps to "SA001532L" (unrelated randomly selected Mestizos from La Habana, Cuba):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "PAGE_Cuban" ] <- "SA001532L";

# The PAGE Study's "Dominican" arguably maps to "SA004242M" (African Caribbean in Barbados (ACB)):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "PAGE_Dominican" ] <- "SA004242M";

# The PAGE Study's "CentralAmerican" arguably maps to "SA004110G" (Mexican Ancestry in Los Angeles, CA (MXL)):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "PAGE_CentralAmerican" ] <- "SA004110G";

# The PAGE Study's "SouthAmerican" arguably maps to "SA004109O" (Colombian in Medellín, Colombia (CLM)):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "PAGE_SouthAmerican" ] <- "SA004109O";

# The FINRISK's "Finnish" arguably maps to "SA004049R" (Finns):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "FINRISK" ] <- "SA004049R";

# The KRGDB's "Koreans" arguably maps to "SA003027M" (Koreans):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "KRGDB" ] <- "SA003027M";

# The GenDan's "Danes" arguably maps to "SA004392S" (Danes):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "GenDan" ] <- "SA004392S";

# The GenNed5's "Dutch" arguably maps to "SA004045N" (Dutch Europeans):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "GenNed5" ] <- "SA004045N";

# The Qatari's "Dutch" arguably maps to "SA001127L" (Qatari):
d_all_wide_lgs$ALFRED_SUID[ d_all_wide_lgs$Pop_UID == "Qatari" ] <- "SA001127L";


# Save these to file:
d_all_wide_lgs <- unique(d_all_wide_lgs); # remove any duplicates
write.table(d_all_wide_lgs, "./data/genetics/output/gene_freqs.tsv", sep="\t", quote=FALSE, row.names=FALSE);

