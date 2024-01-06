library(rjson)
library(ggplot2)
library(MASS)
library(funModeling)
library(tidyverse)
library(Hmisc)
library(tibble)
library(jsonlite)
library(corrplot)
# Loading Relevant Libraries

json_data_EPL <- fromJSON("events_England.json", flatten=TRUE)
#json_data_Euros <- fromJSON("events_European_Championship.json", flatten=TRUE)
#json_data_LigueUn <- fromJSON("events_France.json", flatten=TRUE)
#json_data_Bundesliga <- fromJSON("events_Germany.json", flatten=TRUE)
#json_data_SerieA <- fromJSON("events_Italy.json", flatten=TRUE)
#json_data_LaLiga <- fromJSON("events_Spain.json", flatten=TRUE)
#json_data_WorldCup <- fromJSON("events_World_Cup.json", flatten=TRUE)
# Importing JSON files into R

head(json_data_EPL)
#head(json_data_Euros)
#head(json_data_LigueUn)
#head(json_data_Bundesliga)
#head(json_data_SerieA)
#head(json_data_LaLiga)
#head(json_data_WorldCup)
# Viewing headings and first few rows of data to ensure the files have been loaded without error

str(json_data_EPL)
# Viewing type of objects in the file

colnames(json_data_EPL)
# Viewing column names of file

rownames(json_data_EPL)
# Viewing row names of file

summary(json_data_EPL)
# Summary of file

EPL_corr <- cor(json_data_EPL)

