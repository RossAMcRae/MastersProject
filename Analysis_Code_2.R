library(MASS)
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(cowplot)
library(ggforce)
library(xts)
library(ggrepel)
library(soccermatics)
library(rjson)
library(data.table)
# initialisations

my_data <- read.csv("export_dataframe.csv", header=TRUE)
my_data
# loading in events data

head(my_data, 5)
tail(my_data, 5)

summary(my_data)
string(my_data)
typeof(my_data)

sapply(my_data, is.numeric)

my_num_data <- my_data[, sapply(my_data, is.numeric)]
my_num_data_cor <- cor(my_num_data, use = "complete.obs", method="pearson")

corrplot(my_num_data_cor)

palette <- colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = my_num_data_cor, col = palette, symm = TRUE)

df <- data.frame(my_data$matchId, my_data$eventName, my_data$positions, my_data$teamId, my_data$playerId)

head(df, 5)

df_subset <- subset(df, my_data$matchId == 2499719)
head(df_subset, 100)

df_subset_pass <- subset(df, (my_data$eventName == "Pass" & my_data$matchId == 2499719 & my_data$teamId == 1609))

head(df_subset_pass, 100)


