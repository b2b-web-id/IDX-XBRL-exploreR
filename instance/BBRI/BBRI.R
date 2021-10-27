# get path
workdir <- getwd()
curwd <- normalizePath("instance/BBRI")

# Library
library(XBRL)
library(readr)
library(devtools)
library(dplyr)
# disable, can't use on 4.1.* 
#install_github("bergant/finstr")
#library(finstr)

# Function
files <- function()

# load
setwd(curwd)
# enable if you wanna re-init instance
#BBRI <- xbrlDoAll("instance.xbrl", cache.dir="BBRI_XBRLcache",
#                  prefix.out="BBRI", verbose=TRUE)

# Read csv
#BBRI_facts <- BBRI$fact
BBRI_facts <- read_csv("BBRI_facts.csv")
#BBRI_elements <- BBRI$element
BBRI_elements <- read_csv("BBRI_elements.csv")
#BBRI_definitions <- BBRI$definition
#BBRI_labels <- BBRI$label
BBRI_labels <- read_csv("BBRI_labels.csv")
#BBRI_roles <- BBRI$role
#BBRI_calcs <- BBRI$calculation
BBRI_contexts <- read_csv("BBRI_contexts.csv")

# cek
facts <- (BBRI_facts[, 2:5])
elements <-(BBRI_elements[, c('elementId','balance')])
contexts <- (BBRI_contexts[, c('contextId','startDate','endDate')])
labels <- BBRI_labels %>% filter(lang=='id') %>% select(elementId, labelString)

# report
report_2020 <- facts %>% left_join(elements, by=c("elementId")) %>% 
  left_join(labels, by=c("elementId")) %>% 
  left_join(contexts, by=c("contextId")) %>%
  filter(!is.na(fact) & substr(endDate,1,4)==2020) %>% 
  mutate(year=substr(endDate,1,4), element=gsub("idx-cor_","",elementId)) %>%
  mutate(fact2020=fact) %>% 
  select(labelString, balance, fact2020)
report_2019 <- facts %>% left_join(elements, by=c("elementId")) %>% 
  left_join(labels, by=c("elementId")) %>% 
  left_join(contexts, by=c("contextId")) %>%
  filter(!is.na(fact) & substr(endDate,1,4)==2019) %>% 
  mutate(year=substr(endDate,1,4), element=gsub("idx-cor_","",elementId)) %>% 
  mutate(fact2019=fact) %>% 
  select(labelString, balance, fact2019)

# balance
balance <- report_2020 %>%
  left_join(report_2019, by=c("labelString", "balance")) %>% 
  unique()

# profile
profile <- report_2020 %>% filter(is.na(balance)) %>% 
  select(labelString, fact2020)

# save
save(contexts, elements, facts, labels, report_2019, report_2020, balance,
     file="BBRI2020reports.RData")
