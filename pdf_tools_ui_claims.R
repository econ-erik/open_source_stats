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
ui_text <- pdf_text(paste(data_path,"ui_claims_apr11.pdf",sep="")) # fill in most recent pdf
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
ui_claims <- ui_claims[-c(1:2,57:62),-c(5:7)] # may need to adjust rows
ui_claims <- janitor::clean_names(ui_claims)
names(ui_claims)[1] <- "state"
# convert columns to numeric
ui_claims$advance <- as.numeric(gsub(",","",ui_claims$advance))
ui_claims$prior_wk <- as.numeric(gsub(",","",ui_claims$prior_wk))
ui_claims$change <- as.numeric(gsub(",","",ui_claims$change))
# add a resort and state abbreviations 
ui_claims$abbr[!ui_claims$state %in% c("US Total","Virgin Islands","")] <- state2abbr(ui_claims$state)
ui_claims <- ui_claims %>%
  select(abbr,state,advance,prior_wk,change)
# write to csv
write_csv(ui_claims,paste(data_path,"ui_claims",today(),".csv",sep=""), col_names = TRUE)
