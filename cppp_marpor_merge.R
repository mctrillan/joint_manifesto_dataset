##########################################
###                                    ###
###               README               ###
###                                    ###
##########################################

# This is a script for merging the CPPP (pledge) and MARPOR (manifesto project) datasets, with documents as observations
# The MARPOR data used is the latest available, downloaded using the API
# The pledge dataset is from the Thomson et al. 2017 comparative paper in AJPS (data publicly available here https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/YJUIBI)
# This script is annotated in detail throughout, and I recommend going step-by-step
# If you have comments, suggestions or questions please get in touch at fraser.mcmillan@glasgow.ac.uk

# If you just want to generate the dataset with pre-selected variables all you need to do is the following:
# 1. Log in at https://manifesto-project.wzb.eu and generate your api key
# 2. Set working directory and ensure it contains the AJPS pledge dataset, BOTH party name csv files and your MARPOR api key text file, named key.txt
# 3. Run code from line 24 to line 176, creating the "final" data.frame object with 164 obs

# If you would like to choose more variables from the MARPOR or CPPP datasets, you need to specify this manually by editing the code

# Code for export to other formats (Excel, Stata, SPSS) can be found at the bottom of the script

# Italian pledge data is aggregated by bloc, so it cannot be merged with MARPOR data



#load required packages
library(readstata13)
library(manifestoR)
library(dplyr)
library(tidyr)
library(manifestoR)

#set working directory to folder with all required files (edit as appropriate)
setwd("C:/Users/You/Folder")



##########################################
###                                    ###
### STEP ONE: AGGREGATE PLEDGE DATASET ###
###                                    ###
##########################################

#load pledge dataset and party names
pledge <- read.dta13("pledges_ajps.dta")
ajps_names <- read.csv("ajps_names.csv")

#make pledge count variable and attach to pledge dataset
ncount <- count(pledge, vars = idmanifesto)
colnames(ncount) <- c("idmanifesto", "ncount")
pledge <- merge(pledge, ncount, by="idmanifesto")
remove(ncount)

#select relevant variables from pledge dataset, customise as appropriate
pledge <- subset(pledge, select=c(year, idmanifesto, partyid,
                                  fulfil2, outalways, govparty,
                                  xtimeyrs, growey, growav, 
                                  chex, gtsinmin, gtsinmaj, 
                                  gtcomin, gtcomaj, ncount))

#aggregate pledge data
pledge <- aggregate(pledge, by=list(pledge$idmanifesto), FUN=mean)
pledge$Group.1 <- NULL

#create fulfilment percentage variable
pledge$fper <- pledge$fulfil2*100

#merge datasets to attach party names and generate platform party year variable
pledge <- merge(pledge, ajps_names, by="partyid", all=TRUE)
pledge$party <- as.character(pledge$name)
pledge$party_year <- paste(pledge$name,pledge$year, sep="_")
pledge$country <- substr(pledge$name, 1, 2)
pledge$country_year <- paste(pledge$country,pledge$year, sep="_")

#drop observations of unknown document
pledge <- subset(pledge, !party_year=="NA_2008")

#select and reorder required variables
pledge <- subset(pledge, select=c(idmanifesto, partyid, party, year, country, 
                          country_year, party_year, fper, outalways, 
                          govparty, xtimeyrs, growey, growav, 
                          chex, gtsinmin, gtsinmaj, gtcomin, gtcomaj, ncount))



##########################################
###                                    ###
###  STEP TWO: PREPARE MARPOR DATASET  ###
###                                    ###
##########################################


#here we use the manifestoR api to download the latest MARPOR dataset as a data.frame
#if you're using this method you'll need an api key, which can be obtained on the MARPOR site if you log in
#place the api key text file in the working directory
api_key <- mp_setapikey(key.file = "key.txt")
marpor <- mp_maindataset(version = "current", south_america = FALSE,
                         download_format = NULL, apikey = api_key, cache = FALSE)


#calculate rile variable and issue diversity variable using manifestoR functions
marpor$rile <- rile(marpor)
marpor$diversity <- 
  issue_attention_diversity(marpor, 
                            method = "shannon", 
                            prefix = "per",
                            include_variables = paste0("per", setdiff(v4_categories(), "uncod")),
                            aggregate_categories = list(c(101, 102), c(104, 105), c(107, 109),
                                                        c(108,110), c(203, 204), c(301, 302),
                                                        c(406, 407), c(409, 414), c(504, 505),
                                                        c(506,507), c(601, 602), c(603, 604),
                                                        c(607, 608), c(701, 702)))

#create MARPOR year variable
marpor$year <- substr(marpor$date,1,4)

#subset required MARPOR variables, customise as required
marpor <- data.frame(marpor$countryname, marpor$year, marpor$date, marpor$party, marpor$partyname, marpor$partyabbrev, 
                     marpor$parfam, marpor$pervote, marpor$total, marpor$diversity, marpor$rile,
                     stringsAsFactors=FALSE)

#rename MARPOR variables
names(marpor) <- c("country", "year", "date", "party", "partyname", "party_abbrev", 
                   "parfam", "pervote", "total", "diversity", "rile")

#change MARPOR country variable to character
marpor$country <- as.character(marpor$country)

#subset MARPOR dataset to pledge countries
pledge_countries <- c("United States", "United Kingdom", "Netherlands", 
                      "Ireland", "Sweden", "Spain", "Germany", "Italy", 
                      "Portugal", "Bulgaria", "Canada", "Austria")

marpor  <- subset(marpor, subset = country %in% pledge_countries)

#create MARPOR country abbreviations (used to attach to parties for merge with pledge data)
marpor$country [marpor$country=="United States"] <- "US"
marpor$country [marpor$country=="United Kingdom"] <- "UK"
marpor$country [marpor$country=="Netherlands"] <- "NL"
marpor$country [marpor$country=="Ireland"] <- "IE"
marpor$country [marpor$country=="Sweden"] <- "SE"
marpor$country [marpor$country=="Spain"] <- "ES"
marpor$country [marpor$country=="Germany"] <- "DE"
marpor$country [marpor$country=="Italy"] <- "IT"
marpor$country [marpor$country=="Portugal"] <- "PT"
marpor$country [marpor$country=="Bulgaria"] <- "BU"
marpor$country [marpor$country=="Canada"] <- "CA"
marpor$country [marpor$country=="Austria"] <- "AT"

#create country_year variable in MARPOR
marpor$country_year <- paste(marpor$country,marpor$year)
marpor <- data.frame(marpor, stringsAsFactors=FALSE)

#add party names for FG and FF (names missing in )
marpor$party_abbrev [marpor$partyname=="Familiy of the Irish"] <- "FG"
marpor$party_abbrev [marpor$partyname=="Soldiers of Destiny"]  <- "FF"
marpor$country_party <- paste(marpor$country,marpor$party_abbrev, sep="_")


#load and attach party names
marpor_names <- read.csv("marpor_names.csv")
marpor <- merge(marpor,marpor_names, by="country_party")

#create MARPOR party_year variable for merge
marpor$party_year <- paste(marpor$name,marpor$year, sep="_")



##########################################
###                                    ###
###     STEP THREE: MERGE DATASETS     ###
###                                    ###
##########################################

#remove duplicate and superfluous variables
marpor$partyid <- NULL
marpor$year <- NULL
marpor$country <- NULL
marpor$country_year <- NULL
marpor$name <- NULL
marpor$party <- NULL
marpor$partyname <- NULL
marpor$date <- NULL
marpor$party_abbrev <- NULL
marpor$country_party <- NULL
pledge$X <- NULL

#merge datasets
full <- merge(pledge,marpor,by="party_year")
head(full)

##########################################
###                                    ###
###  STEP FOUR: EXPORT JOINT DATASET   ###
###                                    ###
##########################################

## Excel ##
install.packages("rJava")
library(rJava)
install.packages("xlsx")
library(xlsx)
write.xlsx(final, "C:/Users/You/Folder/pledge_and_marpor.xlsx")

## Stata ## 
#just fyi something about the encoding screws up special characters in strings
install.packages("foreign")
library(foreign)
write.dta(final, "C:/Users/You/Folder/pledge_marpor.dta")

## SPSS ##
write.foreign(final, "C:/Users/You/Folder/pledge_and_marpor.sps",
              "C:/Users/You/Folder/pledge_and_marpor.sps", package="SPSS")

