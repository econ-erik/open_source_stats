## UI State Claims read-in
# not all packages necessary here
library(tidyverse)
library(rvest)
library(magrittr)
library(lubridate)
library(pdftools)
library(httr)
library(janitor)
library(openintro)
# read in PDF
# if reading straight from the DOL website switch the argument in pdf_text() to "https://www.dol.gov/ui/data.pdf" 
ui_text <- pdf_text(paste(data_path,"ui_claims_may16.pdf",sep="")) # fill in most recent pdf
# subset to State UI Claims Page
ui_claims <- ui_text[[5]]
# split into a table based on row separator
ui_claims <- str_split(ui_claims, "\r", simplify = TRUE)
# replace extra spaces with pipes
ui_claims <- str_replace_all(ui_claims, "\\s{2,}","|")
ui_claims <- textConnection(ui_claims)
# use pipes to create a data frame
ui_claims<- read.csv(ui_claims, sep="|", header = FALSE)
# clean up column names
names(ui_claims) <- as.character(unlist(ui_claims[2,]))
# filter out additional rows and remove insured columns 
ui_claims_1 <- ui_claims[-c(1:2,57:62),-c(5:7)] # may need to adjust rows
ui_claims_1 <- janitor::clean_names(ui_claims_1)
names(ui_claims_1)[1] <- "state"
# convert columns to numeric
ui_claims_1$advance <- as.numeric(gsub(",","",ui_claims_1$advance))
ui_claims_1$prior_wk <- as.numeric(gsub(",","",ui_claims_1$prior_wk))
ui_claims_1$change <- as.numeric(gsub(",","",ui_claims_1$change))
# add a resort and state abbreviations and delete Puerto Rico and VI
ui_claims_1 <- ui_claims_1[!ui_claims_1$state %in% c("Virgin Islands","Puerto Rico"),]
ui_claims_1$abbr[!ui_claims_1$state %in% c("US Total")] <- state2abbr(ui_claims_1$state)
ui_claims_1 <- ui_claims_1 %>%
  select(abbr,state,advance,prior_wk,change) %>%
  arrange(abbr)
# write to csv
write_csv(ui_claims_1,paste(data_path,"ui_claims_",today()-1,".csv",sep=""), col_names = TRUE)
# grab continuing claims
# filter out additional rows and remove insured columns 
ui_claims_2 <- ui_claims[-c(1:2,57:62),-c(2:4)] # may need to adjust rows
ui_claims_2 <- janitor::clean_names(ui_claims_2)
names(ui_claims_2)[1] <- "state"
# convert columns to numeric
ui_claims_2$advance <- as.numeric(gsub(",","",ui_claims_2$advance))
ui_claims_2$prior_wk <- as.numeric(gsub(",","",ui_claims_2$prior_wk))
ui_claims_2$change <- as.numeric(gsub(",","",ui_claims_2$change))
# add a resort and state abbreviations and delete Puerto Rico and VI
ui_claims_2 <- ui_claims_2[!ui_claims_2$state %in% c("Virgin Islands","Puerto Rico"),]
ui_claims_2$abbr[!ui_claims_2$state %in% c("US Total")] <- state2abbr(ui_claims_2$state)
ui_claims_2 <- ui_claims_2 %>%
  select(abbr,state,advance,prior_wk,change) %>%
  arrange(abbr)
# write to csv
write_csv(ui_claims_2,paste(data_path,"ui_continue_",today()-1,".csv",sep=""), col_names = TRUE)

## PUA Claims
# subset to State UI Claims Page
ui_claims <- ui_text[[7]]
# split into a table based on row separator
ui_claims <- str_split(ui_claims, "\r", simplify = TRUE)
# replace extra spaces with pipes
ui_claims <- str_replace_all(ui_claims, "\\s{2,}","|")
ui_claims <- textConnection(ui_claims)
# use pipes to create a data frame
ui_claims<- read.csv(ui_claims, sep="|", header = FALSE)
# clean up column names
names(ui_claims) <- as.character(unlist(ui_claims[2,]))
# filter out additional rows and remove insured columns 
ui_claims_1 <- ui_claims[-c(1:2,57:62),-c(5:7)] # may need to adjust rows
ui_claims_1 <- janitor::clean_names(ui_claims_1)
names(ui_claims_1)[1] <- "state"
# convert columns to numeric
ui_claims_1$advance <- as.numeric(gsub(",","",ui_claims_1$advance))
ui_claims_1$prior_wk <- as.numeric(gsub(",","",ui_claims_1$prior_wk))
ui_claims_1$change <- as.numeric(gsub(",","",ui_claims_1$change))
# add a resort and state abbreviations and delete Puerto Rico and VI
ui_claims_1 <- ui_claims_1[!ui_claims_1$state %in% c("Virgin Islands","Puerto Rico"),]
ui_claims_1$abbr[!ui_claims_1$state %in% c("US Total")] <- state2abbr(ui_claims_1$state)
ui_claims_1 <- ui_claims_1 %>%
  select(abbr,state,advance,prior_wk,change) %>%
  arrange(abbr)
# correct MA PUA claims for May 16 -- delete after this week
ui_claims_1$advance[ui_claims_1$abbr=="MA"] <- 115952
ui_claims_1$change[ui_claims_1$abbr== "MA" ] <- 115925-70464
ui_claims_1$advance[ui_claims_1$state=="US Total"] <- 2226921 - (1184792-115952)
ui_claims_1$change[ui_claims_1$state=="US Total"] <- (2226921 - (1184792-115952)) - 850184
# write to csv
write_csv(ui_claims_1,paste(data_path,"ui_pua_claims_",today()-1,".csv",sep=""), col_names = TRUE)
# grab continuing claims
# filter out additional rows and remove insured columns 
ui_claims_2 <- ui_claims[-c(1:2,57:62),-c(2:4)] # may need to adjust rows
ui_claims_2 <- janitor::clean_names(ui_claims_2)
names(ui_claims_2) <- c("state","advance","prior_wk","change")
# convert columns to numeric
ui_claims_2$advance <- as.numeric(gsub(",","",ui_claims_2$advance))
ui_claims_2$prior_wk <- as.numeric(gsub(",","",ui_claims_2$prior_wk))
ui_claims_2$change <- as.numeric(gsub(",","",ui_claims_2$change))
# add a resort and state abbreviations and delete Puerto Rico and VI
ui_claims_2 <- ui_claims_2[!ui_claims_2$state %in% c("Virgin Islands","Puerto Rico"),]
ui_claims_2$abbr[!ui_claims_2$state %in% c("US Total")] <- state2abbr(ui_claims_2$state)
ui_claims_2 <- ui_claims_2 %>%
  select(abbr,state,advance,prior_wk,change) %>%
  arrange(abbr)
# write to csv
write_csv(ui_claims_2,paste(data_path,"ui_pua_continue_",today()-1,".csv",sep=""), col_names = TRUE)
