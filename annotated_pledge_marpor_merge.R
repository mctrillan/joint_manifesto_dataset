###          ###
### PREAMBLE ###
###          ###

# This is a script for merging the CPPG and MARPOR datasets, with documents as observations
# It is annotated in detail throughout, and I recommend going step-by-step
# If you have comments, suggestions or questions please get in touch at fraser.mcmillan@strath.ac.uk

# If you just want the data with pre-selected variables all you need to do is
# 1. Log in at https://manifesto-project.wzb.eu and generate your api key
# 2. Set working directory below and ensure it contains BOTH datasets, BOTH csv files and...
#    your MARPOR api key text file, named key.txt
# 3. Run code from line 24 to line 176, creating the "final" data.frame object with 164 obs

# If you would like to choose more variables from the MARPOR dataset, you need to specify this below

# Code for export to other formats (Excel, Stata, SPSS) can be found at the bottom of the script

###              ###
### LOADING DATA ###
###              ###

#install and load required packages
install.packages("readstata13")
install.packages("manifestoR")
library(readstata13)
library(manifestoR)

#set working directory containing BOTH datasets and BOTH csv files
setwd("C:/Users/You/Folder")

#here we import the Stata file for the pledge dataset
pledge <- read.dta13("pledge_reduced.dta")

#here we use the manifestoR api to download the MARPOR dataset as a data.frame
#if you're using this method you'll need an api key, which can be obtained on the MARPOR site if you log in
#place the api key text file in the working directory
api_key <- mp_setapikey(key.file = "key.txt")
marpor <- mp_maindataset(version = "current", south_america = FALSE,
  download_format = NULL, apikey = api_key, cache = FALSE)

#you can also import the MARPOR Stata file if you prefer, but...
#this creates problems with some string variables (punctuation)
#and it's not possible to use all the manifestoR functions
#here's the code if you need it: marpor2 <- read.dta13("marpor.dta", generate.factors=TRUE)

###                          ###
### TRANSFORMING MARPOR DATA ###
###                          ###

#manifestoR contains functions to generate additional variables
#this one calculates the rile party position measure
marpor$rile <- rile(marpor)

#here's another, for Greene's (2016) issue diversity measure
marpor$diversity<-issue_attention_diversity(marpor, method = "shannon", prefix = "per",
  include_variables = paste0("per", setdiff(v4_categories(), "uncod")),
  aggregate_categories = list(c(101, 102), c(104, 105), c(107, 109), c(108,
  110), c(203, 204), c(301, 302), c(406, 407), c(409, 414), c(504, 505), c(506,
  507), c(601, 602), c(603, 604), c(607, 608), c(701, 702)))

#here we move the marpor data to a new data frame for the purpose of transforming
#we could do all of this with the marpor object but this saves time if you made a mistake
df <- data.frame(marpor)

#here we narrow down the variables we want to use, if you want them all skip this step
df <- data.frame(df$countryname, df$date, df$party, df$partyname, df$partyabbrev, 
                 df$parfam, df$pervote, df$total, df$diversity,
                 stringsAsFactors=FALSE)
names(df) <- c("country", "date", "party", "partyname", "party_abbrev", 
               "parfam", "pervote", "total", "diversity")

#removing day and month from the election date variable to generate year
df$year <- as.numeric(substr(df$date,1,4))
df <- subset(df, year>1973)

#making sure the country variable is considered a character string
df$country <- as.character(df$country)

#subsetting the MARPOR data by the pledge countries
pledge_countries <- c("United States", "United Kingdom", "Netherlands", 
                      "Ireland", "Sweden", "Spain", "Germany", "Italy", 
                      "Portugal", "Bulgaria", "Canada", "Austria")
df  <- subset(df, subset = country %in% pledge_countries)

#creating country year variable
df$cy <- paste(df$country,df$year)

#attaching pledge country codes
df$pc=NA 
df$pc [df$country=="United States"] <- 1
df$pc [df$country=="United Kingdom"] <- 2
df$pc [df$country=="Netherlands"] <- 3
df$pc [df$country=="Ireland"] <- 4
df$pc [df$country=="Sweden"] <- 5
df$pc [df$country=="Spain"] <- 6
df$pc [df$country=="Germany"] <- 7
df$pc [df$country=="Italy"] <- 8
df$pc [df$country=="Portugal"] <- 9
df$pc [df$country=="Bulgaria"] <- 10
df$pc [df$country=="Canada"] <- 11
df$pc [df$country=="Austria"] <- 12

#creating abbreviations for creation of country/party/year id variable
df$c_ab [df$country=="United States"] <- "US"
df$c_ab [df$country=="United Kingdom"] <- "UK"
df$c_ab [df$country=="Netherlands"] <- "NL"
df$c_ab [df$country=="Ireland"] <- "IE"
df$c_ab [df$country=="Sweden"] <- "SE"
df$c_ab [df$country=="Spain"] <- "ES"
df$c_ab [df$country=="Germany"] <- "DE"
df$c_ab [df$country=="Italy"] <- "IT"
df$c_ab [df$country=="Portugal"] <- "PT"
df$c_ab [df$country=="Bulgaria"] <- "BU"
df$c_ab [df$country=="Canada"] <- "CA"
df$c_ab [df$country=="Austria"] <- "AT"

#creating another new object
m<-data.frame(df, stringsAsFactors=FALSE)

#adding MARPOR's missing party abbreviations
m$party_abbrev [m$partyname=="Familiy of the Irish"] <- "FG"
m$party_abbrev [m$partyname=="Soldiers of Destiny"]  <- "FF"

#creating country/party variable
m$c_p <- paste(m$c_ab,m$party_abbrev, sep="_")

###                          ###
### TRANSFORMING PLEDGE DATA ###
###                          ###

#creating a new object for manipulation
p <- pledge

#this code is only for if you want to use a certain subset of pledge countries
#list <- c(#insert pledge country codes to include)
#p <- subset(pledge, subset = country %in% list)

#aggregate the pledge data by manifesto
#using the mean is fine for aggregating variables that don't change 
#but it might interfere with the fulfilment variables
p <- aggregate(p, by=list(p$idmanifesto), FUN=mean)


###                  ###
### MERGING DATASETS ###
###                  ###

#make sure the party name variables in each dataset are factors
m$partyname <- factor(m$partyname)
p$partyid <- factor(p$partyid)

#read in party name csv files
pledge_parties <- read.csv("party_names_pledges.csv")
marpor_parties <- read.csv("party_names_marpor.csv")

#attach party names to pledge dataset
pledge_parties$partyid <- as.factor(pledge_parties$partyid)

#create country/party/year manifesto ID
p$manifesto <- paste(p$name,p$year, sep="")

#merge names into pledge dataset (creating new object "pm")
pm<-merge(p,pledge_parties,by="partyid")

#merge names into marpor

mm<-merge(m,marpor_parties, by="c_p")

#make party year variables to merge
mm$party_year <- paste(mm$name,mm$year, sep="_")
pm$party_year <- paste(pm$name,pm$year, sep="_")

#merge datasets
final <- merge(pm,mm,by="party_year")

###           ###
### EXPORTING ###
###           ###

#this code exports the "final" data.frame to other formats if you'd prefer
#you need to specify the full directory

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
#not tested yet but should work 
write.foreign(final, "C:/Users/You/Folder/pledge_and_marpor.sps",
              "C:/Users/You/Folder/pledge_and_marpor.sps", package="SPSS")
