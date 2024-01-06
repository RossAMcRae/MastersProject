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
library(ggsoccer)
library(tidyr)
# initialisations

setwd("~/Desktop/Docs/MSc Data Analytics/Project/Code/Code")

## Competitions ##

competitions <- fromJSON(file = "competitions.json") 
# Loading Competition dataset from JSON

competitions_df <- data.frame(do.call(rbind, competitions), stringsAsFactors = FALSE)
# Making list into dataframe

## Matches ##

matches_EPL <- fromJSON(file = "matches_England.json")
# Loading Matches dataset from JSON

matches_df <- data.frame(do.call(rbind, matches_EPL), stringsAsFactors = FALSE)
# Making list into dataframe


## Events ##

events_EPL <- fromJSON(file = "events_England.json")
# Loading Events dataset from JSON

events_df <- data.frame(do.call(rbind, events_EPL), stringAsFactors = FALSE)
# Making list into dataframe


## Players ##

players <- fromJSON(file = "players.json")
# Loading Players dataset from JSON

players_df <- data.frame(do.call(rbind, players), stringAsFactors = FALSE)
# Making list into dataframe

## Teams ## 

teams <- fromJSON(file = "teams.json")
# Loading Teams dataset from JSON

teams_df <- data.frame(do.call(rbind, teams), stringsAsFactors = FALSE)
# Making list into dataframe

## Exploratory Analysis ##

passindex <- which(unlist(lapply(events_df, function(x) events_df$eventName))=="Pass")
passindex
# Indices correspond to where you can find pass in events_df

# teamID 1609 = Arsenal

events_df$teamId[1]

events_df$positions

## Arsenal V Leicester Lacazette Passmap ##

events_df_subset_1609 <- subset(events_df, events_df$teamId == 1609 | events_df$eventName == "Pass")
# Refining events dataframe to only show passes for team 1609 - Arsenal
# This will be key for filtering through the data, as teams can be changed, matchID can be added, etc

players_df_subset_1609 <- subset(players_df, players_df$currentTeamId == "1609")
# This refines the players dataframe to only Arsenal players
# Allows us to easily find out playerID for pass maps of individual players

matches_df_arsenal_win <- subset(matches_df, matches_df$winner == "1609")

# matchID matches wyID in matches dataframe, so will use this to filter further

arsenal_vs_leicester_pass <- subset(events_df_subset_1609, events_df_subset_1609$matchId == "2499719" )
# Creating subset of events dataset to only show those referring to matchID of AFC v LCFC

laca_pass_LCFC <- subset(arsenal_vs_leicester_pass, (arsenal_vs_leicester_pass$playerId == "25413" & arsenal_vs_leicester_pass$eventName == "Pass" ))
# Creaing subset of only passes in AFC v LCFC made by Lacazette

laca_pass_LCFC$positions
# Looking at Xstart/end and Ystart/end co-ordinates of passes made by Lacazette

laca_pass_LCFC_positions <- data.frame(matrix(unlist(laca_pass_LCFC$positions), nrow=20, byrow=T), stringsAsFactors = FALSE)
# Unlisting positions column in dataframe to give a matrix of dimensions 20x4
# This allows for use of individual columns in the plotting of the passmap

names(laca_pass_LCFC_positions)[1] <- "X Start"
names(laca_pass_LCFC_positions)[2] <- "Y Start"
names(laca_pass_LCFC_positions)[3] <- "X End"
names(laca_pass_LCFC_positions)[4] <- "Y End"
# Renaming columns for ease and comprehension

## Creating Lacazette Passmap AFC v LCFC ##

lacaLCFCx1 <- as.vector(laca_pass_LCFC_positions[[1]], mode="integer")
lacaLCFCy1 <- as.vector(laca_pass_LCFC_positions[[2]], mode="integer")
lacaLCFCx2 <- as.vector(laca_pass_LCFC_positions[[3]], mode="integer")
lacaLCFCy2 <- as.vector(laca_pass_LCFC_positions[[4]], mode="integer")
# Defining variables to be used for x and y co-ordinates in pass map

ggplot() + annotate_pitch() + theme_pitch()

ggplot(laca_pass_LCFC_positions) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=lacaLCFCx1, y=lacaLCFCy1, xend=lacaLCFCx2, yend=lacaLCFCy2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Leicester City FC, 4-3", "A. Lacazette Passmap")
# Creating passmap using ggplot/ggsoccer

## Xhaka First and Second Half Passmaps AFC v WBA ##

afc_v_wba_pass <- subset(events_df_subset_1609, events_df_subset_1609$matchId == "2499769")
# Creating subset of events dataset to only show those referring to matchID of AFC v WBA

xhaka_pass_WBA_1H <- subset(afc_v_wba_pass, (afc_v_wba_pass$playerId == "49876" & afc_v_wba_pass$eventName == "Pass" & afc_v_wba_pass$matchPeriod == "1H"))
# Creaing subset of only passes in AFC v WBA made by Xhaka in the First Half

xhaka_pass_WBA_2H <- subset(afc_v_wba_pass, (afc_v_wba_pass$playerId == "49876" & afc_v_wba_pass$eventName == "Pass" & afc_v_wba_pass$matchPeriod == "2H"))
# Creaing subset of only passes in AFC v WBA made by Xhaka in the Second Half

xhaka_pass_WBA_1H$positions
xhaka_pass_WBA_2H$positions
# Looking at the Xstart/end and Ystart/end co-ordinates of passes made by Xhaka v WBA in both halves

xhaka_pass_WBA_positions_1H <- data.frame(matrix(unlist(xhaka_pass_WBA_1H$positions), nrow=26, byrow=T), stringsAsFactors = FALSE)
xhaka_pass_WBA_positions_2H <- data.frame(matrix(unlist(xhaka_pass_WBA_2H$positions), nrow=39, byrow=T), stringsAsFactors = FALSE)
# Unlisting positions column in dataframe to give a matrix of dimensions 26x4 and 39x4 for both First and Second Halves
# This allows for use of individual columns in the plotting of the passmap

names(xhaka_pass_WBA_positions_1H)[1] <- "X Start"
names(xhaka_pass_WBA_positions_1H)[2] <- "Y Start"
names(xhaka_pass_WBA_positions_1H)[3] <- "X End"
names(xhaka_pass_WBA_positions_1H)[4] <- "Y End"

names(xhaka_pass_WBA_positions_2H)[1] <- "X Start"
names(xhaka_pass_WBA_positions_2H)[2] <- "Y Start"
names(xhaka_pass_WBA_positions_2H)[3] <- "X End"
names(xhaka_pass_WBA_positions_2H)[4] <- "Y End"
# Renaming columns for ease and comprehension

## Creating Xhaka Passmap for Both Halves v WBA ##

xhaka_WBA_1H_x1 <- as.vector(xhaka_pass_WBA_positions_1H[[1]], mode="integer")
xhaka_WBA_1H_y1 <- as.vector(xhaka_pass_WBA_positions_1H[[2]], mode="integer")
xhaka_WBA_1H_x2 <- as.vector(xhaka_pass_WBA_positions_1H[[3]], mode="integer")
xhaka_WBA_1H_y2 <- as.vector(xhaka_pass_WBA_positions_1H[[4]], mode="integer")

xhaka_WBA_2H_x1 <- as.vector(xhaka_pass_WBA_positions_2H[[1]], mode="integer")
xhaka_WBA_2H_y1 <- as.vector(xhaka_pass_WBA_positions_2H[[2]], mode="integer")
xhaka_WBA_2H_x2 <- as.vector(xhaka_pass_WBA_positions_2H[[3]], mode="integer")
xhaka_WBA_2H_y2 <- as.vector(xhaka_pass_WBA_positions_2H[[4]], mode="integer")
# Defining variables to be used for x and y co-ordinates in pass map

ggplot() + annotate_pitch() + theme_pitch()

ggplot(xhaka_pass_WBA_positions_1H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=xhaka_WBA_1H_x1, y=xhaka_WBA_1H_y1, xend=xhaka_WBA_1H_x2, yend=xhaka_WBA_1H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs West Bromwich Albion, 2-0", "G. Xhaka First Half Passmap")

ggplot(xhaka_pass_WBA_positions_2H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=xhaka_WBA_2H_x1, y=xhaka_WBA_2H_y1, xend=xhaka_WBA_2H_x2, yend=xhaka_WBA_2H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs West Bromwich Albion, 2-0", "G. Xhaka Second Half Passmap")





