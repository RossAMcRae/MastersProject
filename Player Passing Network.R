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
setwd("~/Desktop/Docs/MSc Data Analytics/Project/Code/Code")
# Creating passmap using ggplot/ggsoccer


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


## Exploring ##

passindex <- which(unlist(lapply(events_df, function(x) events_df$eventName))=="Pass")
passindex
# Indices correspond to where you can find pass in events_df

# teamID 1609 = Arsenal

events_df$teamId[1]

events_df$positions


## Initial Arsenal Data Loading ##

events_df_subset_1609 <- subset(events_df, events_df$teamId == 1609 | events_df$eventName == "Pass")
# Refining events dataframe to only show passes for team 1609 - Arsenal
# This will be key for filtering through the data, as teams can be changed, matchID can be added, etc

players_df_subset_1609 <- subset(players_df, players_df$currentTeamId == "1609")
# This refines the players dataframe to only Arsenal players
# Allows us to easily find out playerID for pass maps of individual players

matches_df_arsenal_win <- subset(matches_df, matches_df$winner == "1609")
# This refines the matches dataframe to only those matches which Arsenal have won
# Allows us to easily find out matchID and results of matches

# matchID matches wyID in matches dataframe, so will use this to filter further

## Exploratory Analysis ##

afc_free_kick_all <- subset(events_df_subset_1609, events_df_subset_1609$subEventName == "Free Kick")
# Creating subset of amount of free kicks Arsenal have had to work out average free kicks per game

afc_clearance_all <- subset(events_df_subset_1609, events_df_subset_1609$subEventName == "Clearance")
# Creating subset of amount of clearances Arsenal have had to work out average clearances per game

afc_throw_ins_all <- subset(events_df_subset_1609, events_df_subset_1609$subEventName == "Throw in")
# Creating subset of amount of throw-ins Arsenal have had to work out average throw-ins per game

afc_fouls_all <- subset(events_df_subset_1609, events_df_subset_1609$subEventName == "Foul")
# Creating subset of amount of fouls Arsenal have had to work out average fouls per game

afc_touch_all <- subset(events_df_subset_1609, events_df_subset_1609$subEventName == "Touch")
afc_touch_all_bellerin <- subset(events_df_subset_1609, (events_df_subset_1609$subEventName == "Touch" & events_df_subset_1609$playerId == "167145"))
# Creating subset of amount of touches Arsenal and Bellerin have had to work out average touches per game

afc_bellerin_touch_WHU <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499760" & events_df_subset_1609$playerId == "167145" & events_df_subset_1609$subEventName == "Touch"))
# Creating subset of Bellerings touchs v WHU

afc_bellerin_touch_positions <- data.frame(matrix(unlist(afc_bellerin_touch_WHU$positions), ncol=4, byrow=FALSE), stringsAsFactors = FALSE)
# Unlisting positions column in dataframe to give a matrix of dimensions, 10x4
# This allows for use of individual columns in the plotting of the touch map

names(afc_bellerin_touch_positions)[1] <- "Y Start"
names(afc_bellerin_touch_positions)[2] <- "X Start"
# Renaming columns for ease and comprehension

afc_bellerin_touch_y1 <- as.vector(afc_bellerin_touch_positions[[1]], mode="integer")
afc_bellerin_touch_x1 <- as.vector(afc_bellerin_touch_positions[[2]], mode="integer")
# Defining variables to be used for x and y co-ordinates in pass map

ggplot(afc_bellerin_touch_WHU) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_bellerin_touch_x1, y=afc_bellerin_touch_y1), col = "blue", size = 3) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. West Ham United F.C.", "H. Bellerin Touch Map") 
# Plotting Bellerin touch map for one game


## AFC v WHU Entire Team Passmap ##

afc_v_whu <- subset(events_df_subset_1609, events_df_subset_1609$matchId == "2500060")
# Creating subset of events dataset to only show those referring to matchID of AFC v WHU 

afc_v_whu_pass <- subset(afc_v_whu, afc_v_whu$eventName == "Pass")
# Creating subset of only passes made by Arsenal in AFC v WHU 

afc_v_whu_pass_positions <- data.frame(matrix(unlist(afc_v_whu$positions), ncol=4, byrow=FALSE), stringsAsFactors = FALSE)
# Unlisting positions column in dataframe to give a matrix of dimensions 1
# This allows for use of individual columns in the plotting of the passmap

names(afc_v_whu_pass_positions)[1] <- "Y Start"
names(afc_v_whu_pass_positions)[2] <- "X Start"
names(afc_v_whu_pass_positions)[3] <- "Y End"
names(afc_v_whu_pass_positions)[4] <- "X End"
# Renaming columns for ease and comprehension

afc_v_whu_y1 <- as.vector(afc_v_whu_pass_positions[[1]], mode="integer")
afc_v_whu_x1 <- as.vector(afc_v_whu_pass_positions[[2]], mode="integer")
afc_v_whu_y2 <- as.vector(afc_v_whu_pass_positions[[3]], mode="integer")
afc_v_whu_x2 <- as.vector(afc_v_whu_pass_positions[[4]], mode="integer")
# Defining variables to be used for x and y co-ordinates in pass map

ggplot() + annotate_pitch() + theme_pitch()

ggplot(afc_v_whu_pass_positions) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_whu_x1, y=afc_v_whu_y1, xend=afc_v_whu_x2, yend=afc_v_whu_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs West Ham United, 4-1", "Arsenal Passmap")
# Creating passmap using ggplot/ggsoccer


## Arsenal V Leicester Lacazette Passmap ##

arsenal_vs_leicester_pass <- subset(events_df_subset_1609, events_df_subset_1609$matchId == "2499719" )
# Creating subset of events dataset to only show those referring to matchID of AFC v LCFC

laca_pass_LCFC <- subset(arsenal_vs_leicester_pass, (arsenal_vs_leicester_pass$playerId == "25413" & arsenal_vs_leicester_pass$eventName == "Pass" ))
# Creaing subset of only passes in AFC v LCFC made by Lacazette

laca_pass_LCFC$positions
# Looking at Xstart/end and Ystart/end co-ordinates of passes made by Lacazette

laca_pass_LCFC_positions <- data.frame(matrix(unlist(laca_pass_LCFC$positions), nrow=20, byrow=T), stringsAsFactors = FALSE)
# Unlisting positions column in dataframe to give a matrix of dimensions 20x4
# This allows for use of individual columns in the plotting of the passmap

names(laca_pass_LCFC_positions)[1] <- "Y Start"
names(laca_pass_LCFC_positions)[2] <- "X Start"
names(laca_pass_LCFC_positions)[3] <- "Y End"
names(laca_pass_LCFC_positions)[4] <- "X End"
# Renaming columns for ease and comprehension


## Creating Lacazette Passmap AFC v LCFC ##

lacaLCFCy1 <- as.vector(laca_pass_LCFC_positions[[1]], mode="integer")
lacaLCFCx1 <- as.vector(laca_pass_LCFC_positions[[2]], mode="integer")
lacaLCFCy2 <- as.vector(laca_pass_LCFC_positions[[3]], mode="integer")
lacaLCFCx2 <- as.vector(laca_pass_LCFC_positions[[4]], mode="integer")
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

names(xhaka_pass_WBA_positions_1H)[1] <- "Y Start"
names(xhaka_pass_WBA_positions_1H)[2] <- "X Start"
names(xhaka_pass_WBA_positions_1H)[3] <- "Y End"
names(xhaka_pass_WBA_positions_1H)[4] <- "X End"

names(xhaka_pass_WBA_positions_2H)[1] <- "Y Start"
names(xhaka_pass_WBA_positions_2H)[2] <- "X Start"
names(xhaka_pass_WBA_positions_2H)[3] <- "Y End"
names(xhaka_pass_WBA_positions_2H)[4] <- "X End"
# Renaming columns for ease and comprehension

## Creating Xhaka Passmap for Both Halves v WBA ##

xhaka_WBA_1H_y1 <- as.vector(xhaka_pass_WBA_positions_1H[[1]], mode="integer")
xhaka_WBA_1H_x1 <- as.vector(xhaka_pass_WBA_positions_1H[[2]], mode="integer")
xhaka_WBA_1H_y2 <- as.vector(xhaka_pass_WBA_positions_1H[[3]], mode="integer")
xhaka_WBA_1H_x2 <- as.vector(xhaka_pass_WBA_positions_1H[[4]], mode="integer")

xhaka_WBA_2H_y1 <- as.vector(xhaka_pass_WBA_positions_2H[[1]], mode="integer")
xhaka_WBA_2H_x1 <- as.vector(xhaka_pass_WBA_positions_2H[[2]], mode="integer")
xhaka_WBA_2H_y2 <- as.vector(xhaka_pass_WBA_positions_2H[[3]], mode="integer")
xhaka_WBA_2H_x2 <- as.vector(xhaka_pass_WBA_positions_2H[[4]], mode="integer")
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
# Creating passmap using ggplot/ggsoccer


## Ozil Passmap 10 minute increments v NUFC ##

afc_v_nufc_pass <- subset(events_df_subset_1609, events_df_subset_1609$matchId == "2499890")
# Creating subset of events dataset to only show those referring to matchID of AFC v NUFC

ozil_pass_nufc_1to10_1H <- subset(afc_v_nufc_pass, (afc_v_nufc_pass$playerId == "3319" & afc_v_nufc_pass$eventName == "Pass" & afc_v_nufc_pass$matchPeriod == "1H" & afc_v_nufc_pass$eventSec <= 599))
ozil_pass_nufc_11to20_1H <- subset(afc_v_nufc_pass, (afc_v_nufc_pass$playerId == "3319" & afc_v_nufc_pass$eventName == "Pass" & afc_v_nufc_pass$matchPeriod == "1H" & afc_v_nufc_pass$eventSec >= 600 & afc_v_nufc_pass$eventSec <= 1199))
ozil_pass_nufc_21to30_1H <- subset(afc_v_nufc_pass, (afc_v_nufc_pass$playerId == "3319" & afc_v_nufc_pass$eventName == "Pass" & afc_v_nufc_pass$matchPeriod == "1H" & afc_v_nufc_pass$eventSec >= 1200 & afc_v_nufc_pass$eventSec <= 1799))
ozil_pass_nufc_31to40_1H <- subset(afc_v_nufc_pass, (afc_v_nufc_pass$playerId == "3319" & afc_v_nufc_pass$eventName == "Pass" & afc_v_nufc_pass$matchPeriod == "1H" & afc_v_nufc_pass$eventSec >= 1800 & afc_v_nufc_pass$eventSec <= 2399))
ozil_pass_nufc_41plus_1H <- subset(afc_v_nufc_pass, (afc_v_nufc_pass$playerId == "3319" & afc_v_nufc_pass$eventName == "Pass" & afc_v_nufc_pass$matchPeriod == "1H" & afc_v_nufc_pass$eventSec >= 2400 & afc_v_nufc_pass$eventSec <= 2999))
ozil_pass_nufc_1to10_2H <- subset(afc_v_nufc_pass, (afc_v_nufc_pass$playerId == "3319" & afc_v_nufc_pass$eventName == "Pass" & afc_v_nufc_pass$matchPeriod == "2H" & afc_v_nufc_pass$eventSec <= 599))
ozil_pass_nufc_11to20_2H <- subset(afc_v_nufc_pass, (afc_v_nufc_pass$playerId == "3319" & afc_v_nufc_pass$eventName == "Pass" & afc_v_nufc_pass$matchPeriod == "2H" & afc_v_nufc_pass$eventSec >= 600 & afc_v_nufc_pass$eventSec <= 1199))
ozil_pass_nufc_21to30_2H <- subset(afc_v_nufc_pass, (afc_v_nufc_pass$playerId == "3319" & afc_v_nufc_pass$eventName == "Pass" & afc_v_nufc_pass$matchPeriod == "2H" & afc_v_nufc_pass$eventSec >= 1200 & afc_v_nufc_pass$eventSec <= 1799))
ozil_pass_nufc_31to40_2H <- subset(afc_v_nufc_pass, (afc_v_nufc_pass$playerId == "3319" & afc_v_nufc_pass$eventName == "Pass" & afc_v_nufc_pass$matchPeriod == "2H" & afc_v_nufc_pass$eventSec >= 1800 & afc_v_nufc_pass$eventSec <= 2399))
ozil_pass_nufc_41plus_2H <- subset(afc_v_nufc_pass, (afc_v_nufc_pass$playerId == "3319" & afc_v_nufc_pass$eventName == "Pass" & afc_v_nufc_pass$matchPeriod == "2H" & afc_v_nufc_pass$eventSec >= 2400 & afc_v_nufc_pass$eventSec <= 2999))
# Creating subset of only passes in AFC v NUFC made by Ozil in iindividual 10 minute periods

ozil_pass_nufc_positions_1to10_1H <- data.frame(matrix(unlist(ozil_pass_nufc_1to10_1H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
ozil_pass_nufc_positions_11to20_1H <- data.frame(matrix(unlist(ozil_pass_nufc_11to20_1H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
ozil_pass_nufc_positions_21to30_1H <- data.frame(matrix(unlist(ozil_pass_nufc_21to30_1H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
ozil_pass_nufc_positions_31to40_1H <- data.frame(matrix(unlist(ozil_pass_nufc_31to40_1H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
ozil_pass_nufc_positions_41plus_1H <- data.frame(matrix(unlist(ozil_pass_nufc_41plus_1H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
ozil_pass_nufc_positions_1to10_2H <- data.frame(matrix(unlist(ozil_pass_nufc_1to10_2H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
ozil_pass_nufc_positions_11to20_2H <- data.frame(matrix(unlist(ozil_pass_nufc_11to20_2H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
ozil_pass_nufc_positions_21to30_2H <- data.frame(matrix(unlist(ozil_pass_nufc_21to30_2H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
ozil_pass_nufc_positions_31to40_2H <- data.frame(matrix(unlist(ozil_pass_nufc_31to40_2H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
ozil_pass_nufc_positions_41plus_2H <- data.frame(matrix(unlist(ozil_pass_nufc_41plus_2H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
# Unlisting positions column in dataframe to give a matrix of dimensions 26x4 and 39x4 for both First and Second Halves
# This allows for use of individual columns in the plotting of the passmap

names(ozil_pass_nufc_positions_1to10_1H)[1] <- "Y Start"
names(ozil_pass_nufc_positions_1to10_1H)[2] <- "X Start"
names(ozil_pass_nufc_positions_1to10_1H)[3] <- "Y End"
names(ozil_pass_nufc_positions_1to10_1H)[4] <- "X End"

names(ozil_pass_nufc_positions_11to20_1H)[1] <- "Y Start"
names(ozil_pass_nufc_positions_11to20_1H)[2] <- "X Start"
names(ozil_pass_nufc_positions_11to20_1H)[3] <- "Y End"
names(ozil_pass_nufc_positions_11to20_1H)[4] <- "X End"

names(ozil_pass_nufc_positions_21to30_1H)[1] <- "Y Start"
names(ozil_pass_nufc_positions_21to30_1H)[2] <- "X Start"
names(ozil_pass_nufc_positions_21to30_1H)[3] <- "Y End"
names(ozil_pass_nufc_positions_21to30_1H)[4] <- "X End"

names(ozil_pass_nufc_positions_31to40_1H)[1] <- "Y Start"
names(ozil_pass_nufc_positions_31to40_1H)[2] <- "X Start"
names(ozil_pass_nufc_positions_31to40_1H)[3] <- "Y End"
names(ozil_pass_nufc_positions_31to40_1H)[4] <- "X End"

names(ozil_pass_nufc_positions_41plus_1H)[1] <- "Y Start"
names(ozil_pass_nufc_positions_41plus_1H)[2] <- "X Start"
names(ozil_pass_nufc_positions_41plus_1H)[3] <- "Y End"
names(ozil_pass_nufc_positions_41plus_1H)[4] <- "X End"

names(ozil_pass_nufc_positions_1to10_2H)[1] <- "Y Start"
names(ozil_pass_nufc_positions_1to10_2H)[2] <- "X Start"
names(ozil_pass_nufc_positions_1to10_2H)[3] <- "Y End"
names(ozil_pass_nufc_positions_1to10_2H)[4] <- "X End"

names(ozil_pass_nufc_positions_11to20_2H)[1] <- "Y Start"
names(ozil_pass_nufc_positions_11to20_2H)[2] <- "X Start"
names(ozil_pass_nufc_positions_11to20_2H)[3] <- "Y End"
names(ozil_pass_nufc_positions_11to20_2H)[4] <- "X End"

names(ozil_pass_nufc_positions_21to30_2H)[1] <- "Y Start"
names(ozil_pass_nufc_positions_21to30_2H)[2] <- "X Start"
names(ozil_pass_nufc_positions_21to30_2H)[3] <- "Y End"
names(ozil_pass_nufc_positions_21to30_2H)[4] <- "X End"

names(ozil_pass_nufc_positions_31to40_2H)[1] <- "Y Start"
names(ozil_pass_nufc_positions_31to40_2H)[2] <- "X Start"
names(ozil_pass_nufc_positions_31to40_2H)[3] <- "Y End"
names(ozil_pass_nufc_positions_31to40_2H)[4] <- "X End"

names(ozil_pass_nufc_positions_41plus_2H)[1] <- "Y Start"
names(ozil_pass_nufc_positions_41plus_2H)[2] <- "X Start"
names(ozil_pass_nufc_positions_41plus_2H)[3] <- "Y End"
names(ozil_pass_nufc_positions_41plus_2H)[4] <- "X End"
# Renaming columns for ease and comprehension

ozil_nufc_1to10_1H_y1 <- as.vector(ozil_pass_nufc_positions_1to10_1H[[1]], mode="integer")
ozil_nufc_1to10_1H_x1 <- as.vector(ozil_pass_nufc_positions_1to10_1H[[2]], mode="integer")
ozil_nufc_1to10_1H_y2 <- as.vector(ozil_pass_nufc_positions_1to10_1H[[3]], mode="integer")
ozil_nufc_1to10_1H_x2 <- as.vector(ozil_pass_nufc_positions_1to10_1H[[4]], mode="integer")

ozil_nufc_11to20_1H_y1 <- as.vector(ozil_pass_nufc_positions_11to20_1H[[1]], mode="integer")
ozil_nufc_11to20_1H_x1 <- as.vector(ozil_pass_nufc_positions_11to20_1H[[2]], mode="integer")
ozil_nufc_11to20_1H_y2 <- as.vector(ozil_pass_nufc_positions_11to20_1H[[3]], mode="integer")
ozil_nufc_11to20_1H_x2 <- as.vector(ozil_pass_nufc_positions_11to20_1H[[4]], mode="integer")

ozil_nufc_21to30_1H_y1 <- as.vector(ozil_pass_nufc_positions_21to30_1H[[1]], mode="integer")
ozil_nufc_21to30_1H_x1 <- as.vector(ozil_pass_nufc_positions_21to30_1H[[2]], mode="integer")
ozil_nufc_21to30_1H_y2 <- as.vector(ozil_pass_nufc_positions_21to30_1H[[3]], mode="integer")
ozil_nufc_21to30_1H_x2 <- as.vector(ozil_pass_nufc_positions_21to30_1H[[4]], mode="integer")

ozil_nufc_31to40_1H_y1 <- as.vector(ozil_pass_nufc_positions_31to40_1H[[1]], mode="integer")
ozil_nufc_31to40_1H_x1 <- as.vector(ozil_pass_nufc_positions_31to40_1H[[2]], mode="integer")
ozil_nufc_31to40_1H_y2 <- as.vector(ozil_pass_nufc_positions_31to40_1H[[3]], mode="integer")
ozil_nufc_31to40_1H_x2 <- as.vector(ozil_pass_nufc_positions_31to40_1H[[4]], mode="integer")

ozil_nufc_41plus_1H_y1 <- as.vector(ozil_pass_nufc_positions_41plus_1H[[1]], mode="integer")
ozil_nufc_41plus_1H_x1 <- as.vector(ozil_pass_nufc_positions_41plus_1H[[2]], mode="integer")
ozil_nufc_41plus_1H_y2 <- as.vector(ozil_pass_nufc_positions_41plus_1H[[3]], mode="integer")
ozil_nufc_41plus_1H_x2 <- as.vector(ozil_pass_nufc_positions_41plus_1H[[4]], mode="integer")

ozil_nufc_1to10_2H_y1 <- as.vector(ozil_pass_nufc_positions_1to10_2H[[1]], mode="integer")
ozil_nufc_1to10_2H_x1 <- as.vector(ozil_pass_nufc_positions_1to10_2H[[2]], mode="integer")
ozil_nufc_1to10_2H_y2 <- as.vector(ozil_pass_nufc_positions_1to10_2H[[3]], mode="integer")
ozil_nufc_1to10_2H_x2 <- as.vector(ozil_pass_nufc_positions_1to10_2H[[4]], mode="integer")

ozil_nufc_11to20_2H_y1 <- as.vector(ozil_pass_nufc_positions_11to20_2H[[1]], mode="integer")
ozil_nufc_11to20_2H_x1 <- as.vector(ozil_pass_nufc_positions_11to20_2H[[2]], mode="integer")
ozil_nufc_11to20_2H_y2 <- as.vector(ozil_pass_nufc_positions_11to20_2H[[3]], mode="integer")
ozil_nufc_11to20_2H_x2 <- as.vector(ozil_pass_nufc_positions_11to20_2H[[4]], mode="integer")

ozil_nufc_21to30_2H_y1 <- as.vector(ozil_pass_nufc_positions_21to30_2H[[1]], mode="integer")
ozil_nufc_21to30_2H_x1 <- as.vector(ozil_pass_nufc_positions_21to30_2H[[2]], mode="integer")
ozil_nufc_21to30_2H_y2 <- as.vector(ozil_pass_nufc_positions_21to30_2H[[3]], mode="integer")
ozil_nufc_21to30_2H_x2 <- as.vector(ozil_pass_nufc_positions_21to30_2H[[4]], mode="integer")

ozil_nufc_31to40_2H_y1 <- as.vector(ozil_pass_nufc_positions_31to40_2H[[1]], mode="integer")
ozil_nufc_31to40_2H_x1 <- as.vector(ozil_pass_nufc_positions_31to40_2H[[2]], mode="integer")
ozil_nufc_31to40_2H_y2 <- as.vector(ozil_pass_nufc_positions_31to40_2H[[3]], mode="integer")
ozil_nufc_31to40_2H_x2 <- as.vector(ozil_pass_nufc_positions_31to40_2H[[4]], mode="integer")

ozil_nufc_41plus_2H_y1 <- as.vector(ozil_pass_nufc_positions_41plus_2H[[1]], mode="integer")
ozil_nufc_41plus_2H_x1 <- as.vector(ozil_pass_nufc_positions_41plus_2H[[2]], mode="integer")
ozil_nufc_41plus_2H_y2 <- as.vector(ozil_pass_nufc_positions_41plus_2H[[3]], mode="integer")
ozil_nufc_41plus_2H_x2 <- as.vector(ozil_pass_nufc_positions_41plus_2H[[4]], mode="integer")
# Defining variables to be used for x and y co-ordinates in pass map

ggplot(ozil_pass_nufc_positions_1to10_1H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=ozil_nufc_1to10_1H_x1, y=ozil_nufc_1to10_1H_y1, xend=ozil_nufc_1to10_1H_x2, yend=ozil_nufc_1to10_1H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Newcastle United F.C., 1-0", "M. Ozil First Half 1'-10' Passmap")

ggplot(ozil_pass_nufc_positions_11to20_1H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=ozil_nufc_11to20_1H_x1, y=ozil_nufc_11to20_1H_y1, xend=ozil_nufc_11to20_1H_x2, yend=ozil_nufc_11to20_1H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Newcastle United F.C., 1-0", "M. Ozil First Half 11'-20' Passmap")

ggplot(ozil_pass_nufc_positions_21to30_1H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=ozil_nufc_21to30_1H_x1, y=ozil_nufc_21to30_1H_y1, xend=ozil_nufc_21to30_1H_x2, yend=ozil_nufc_21to30_1H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Newcastle United F.C., 1-0", "M. Ozil First Half 21'-30' Passmap")

ggplot(ozil_pass_nufc_positions_31to40_1H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=ozil_nufc_31to40_1H_x1, y=ozil_nufc_31to40_1H_y1, xend=ozil_nufc_31to40_1H_x2, yend=ozil_nufc_31to40_1H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Newcastle United F.C., 1-0", "M. Ozil First Half 31'-40' Passmap")

ggplot(ozil_pass_nufc_positions_41plus_1H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=ozil_nufc_41plus_1H_x1, y=ozil_nufc_41plus_1H_y1, xend=ozil_nufc_41plus_1H_x2, yend=ozil_nufc_41plus_1H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Newcastle United F.C., 1-0", "M. Ozil First Half 41'+ Passmap")

ggplot(ozil_pass_nufc_positions_1to10_2H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=ozil_nufc_1to10_2H_x1, y=ozil_nufc_1to10_2H_y1, xend=ozil_nufc_1to10_2H_x2, yend=ozil_nufc_1to10_2H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Newcastle United F.C., 1-0", "M. Ozil Second Half 1'-10' Passmap")

ggplot(ozil_pass_nufc_positions_11to20_2H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=ozil_nufc_11to20_2H_x1, y=ozil_nufc_11to20_2H_y1, xend=ozil_nufc_11to20_2H_x2, yend=ozil_nufc_11to20_2H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Newcastle United F.C., 1-0", "M. Ozil Second Half 11'-20' Passmap")

ggplot(ozil_pass_nufc_positions_21to30_2H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=ozil_nufc_21to30_2H_x1, y=ozil_nufc_21to30_2H_y1, xend=ozil_nufc_21to30_2H_x2, yend=ozil_nufc_21to30_2H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Newcastle United F.C., 1-0", "M. Ozil Second Half 21'-30' Passmap")

ggplot(ozil_pass_nufc_positions_31to40_2H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=ozil_nufc_31to40_2H_x1, y=ozil_nufc_31to40_2H_y1, xend=ozil_nufc_31to40_2H_x2, yend=ozil_nufc_31to40_2H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Newcastle United F.C., 1-0", "M. Ozil Second Half 31'-40' Passmap")

ggplot(ozil_pass_nufc_positions_41plus_2H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=ozil_nufc_41plus_2H_x1, y=ozil_nufc_41plus_2H_y1, xend=ozil_nufc_41plus_2H_x2, yend=ozil_nufc_41plus_2H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Newcastle United F.C., 1-0", "M. Ozil Second Half 41'+ Passmap")
# Creating passmaps using ggplot/ggsoccer


## Shotmaps ## 


## AFC v EFC Squad Shotmap ##

afc_v_efc <- subset(events_df_subset_1609, events_df_subset_1609$matchId == "2499970")
# Creating subset of events data for AFC v EFC

afc_v_efc_shot <- subset(afc_v_efc, afc_v_efc$eventName == "Shot")
# Creating subset of shots made in AFC v EFC (Both Halves)

afc_v_efc_shot_positions <- data.frame(matrix(unlist(afc_v_efc_shot$positions), nrow=14, byrow=T), stringsAsFactors = FALSE)
# Creating data frame of positions of shots

names(afc_v_efc_shot_positions)[1] <- "Y Start"
names(afc_v_efc_shot_positions)[2] <- "X Start"
names(afc_v_efc_shot_positions)[3] <- "Y End"
names(afc_v_efc_shot_positions)[4] <- "X End"
# Renaming columns for ease and comprehension

afc_v_efc_shot_time <- data.frame(matrix(unlist(afc_v_efc_shot$eventSec), nrow=14, byrow=T), stringsAsFactors = FALSE)
# Creating data frame of shot timings
# Have to do this to determine if goal or not, as dataset does not include this information

afc_v_efc_shot_minute <- afc_v_efc_shot_time/60
# Converting seconds to minutes

afc_v_efc_y1 <- as.vector(afc_v_efc_shot_positions[[1]], mode="integer")
afc_v_efc_x1 <- as.vector(afc_v_efc_shot_positions[[2]], mode="integer")
# Defining variables to be used for x and y co-ordinates in pass map

ggplot(afc_v_efc_shot) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_v_efc_x1, y=afc_v_efc_y1), col = "red", size = 2) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. Everton F.C.", "Squad Shot Map") 
# Creating shotmap using ggplot/ggsoccer


## Ramsey Shotmap vs EFC ##

afc_v_efc_ramsey_shot <- subset(afc_v_efc, (afc_v_efc$eventName == "Shot" & afc_v_efc$playerId == "7870"))
# Creating subset of shots made by Ramsey in AFC v EFC (Both Halves)

afc_v_efc_ramsey_shot_positions <- data.frame(matrix(unlist(afc_v_efc_ramsey_shot$positions), nrow=3, byrow=T), stringsAsFactors = FALSE)
# Creating data frame of positions of shots

names(afc_v_efc_ramsey_shot_positions)[1] <- "Y Start"
names(afc_v_efc_ramsey_shot_positions)[2] <- "X Start"
names(afc_v_efc_ramsey_shot_positions)[3] <- "Y End"
names(afc_v_efc_ramsey_shot_positions)[4] <- "X End"
# Renaming columns for ease and comprehension

afc_v_efc_ramsey_y1 <- as.vector(afc_v_efc_ramsey_shot_positions[[1]], mode="integer")
afc_v_efc_ramsey_x1 <- as.vector(afc_v_efc_ramsey_shot_positions[[2]], mode="integer")
# Defining variables to be used for x and y co-ordinates in pass map

ggplot(afc_v_efc_ramsey_shot) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_v_efc_ramsey_x1, y=afc_v_efc_ramsey_y1), col = "red", size = 2) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. Everton F.C.", "A. Ramsey Shot Map") 
# Creating shotmap using ggplot/ggsoccer


## Arsenal Squad Shotmap v EFC in 10 minute intervals

afc_shot_efc_1to10_1H <- subset(afc_v_efc, (afc_v_efc$eventName == "Shot" & afc_v_efc$matchPeriod == "1H" & afc_v_efc$eventSec <= 599))
afc_shot_efc_11to20_1H <- subset(afc_v_efc, (afc_v_efc$eventName == "Shot" & afc_v_efc$matchPeriod == "1H" & afc_v_efc$eventSec >= 600 & afc_v_efc$eventSec <= 1199))
afc_shot_efc_21to30_1H <- subset(afc_v_efc, (afc_v_efc$eventName == "Shot" & afc_v_efc$matchPeriod == "1H" & afc_v_efc$eventSec >= 1200 & afc_v_efc$eventSec <= 1799))
afc_shot_efc_31to40_1H <- subset(afc_v_efc, (afc_v_efc$eventName == "Shot" & afc_v_efc$matchPeriod == "1H" & afc_v_efc$eventSec >= 1800 & afc_v_efc$eventSec <= 2399))
afc_shot_efc_41plus_1H <- subset(afc_v_efc, (afc_v_efc$eventName == "Shot" & afc_v_efc$matchPeriod == "1H" & afc_v_efc$eventSec >= 2400 & afc_v_efc$eventSec <= 2999))
afc_shot_efc_1to10_2H <- subset(afc_v_efc, (afc_v_efc$eventName == "Shot" & afc_v_efc$matchPeriod == "2H" & afc_v_efc$eventSec <= 599))
afc_shot_efc_11to20_2H <- subset(afc_v_efc, (afc_v_efc$eventName == "Shot" & afc_v_efc$matchPeriod == "2H" & afc_v_efc$eventSec >= 600 & afc_v_efc$eventSec <= 1199))
afc_shot_efc_21to30_2H <- subset(afc_v_efc, (afc_v_efc$eventName == "Shot" & afc_v_efc$matchPeriod == "2H" & afc_v_efc$eventSec >= 1200 & afc_v_efc$eventSec <= 1799))
afc_shot_efc_31to40_2H <- subset(afc_v_efc, (afc_v_efc$eventName == "Shot" & afc_v_efc$matchPeriod == "2H" & afc_v_efc$eventSec >= 1800 & afc_v_efc$eventSec <= 2399))
afc_shot_efc_41plus_2H <- subset(afc_v_efc, (afc_v_efc$eventName == "Shot" & afc_v_efc$matchPeriod == "2H" & afc_v_efc$eventSec >= 2400 & afc_v_efc$eventSec <= 2999))
# Using previously made AFC v EFC subset
# Creating subset of only shots in AFC v NUFC made by Arsenal in iindividual 10 minute periods

afc_shot_efc_1to10_1H_positions <- data.frame(matrix(unlist(afc_shot_efc_1to10_1H$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_shot_efc_11to20_1H_positions <- data.frame(matrix(unlist(afc_shot_efc_11to20_1H$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_shot_efc_21to30_1H_positions <- data.frame(matrix(unlist(afc_shot_efc_21to30_1H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
# No shots
afc_shot_efc_31to40_1H_positions <- data.frame(matrix(unlist(afc_shot_efc_31to40_1H$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_shot_efc_41plus_1H_positions <- data.frame(matrix(unlist(afc_shot_efc_41plus_1H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
afc_shot_efc_1to10_2H_positions <- data.frame(matrix(unlist(afc_shot_efc_1to10_2H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
# No shots
afc_shot_efc_11to20_2H_positions <- data.frame(matrix(unlist(afc_shot_efc_11to20_2H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
afc_shot_efc_21to30_2H_positions <- data.frame(matrix(unlist(afc_shot_efc_21to30_2H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
afc_shot_efc_31to40_2H_positions <- data.frame(matrix(unlist(afc_shot_efc_31to40_2H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
# No shots
afc_shot_efc_41plus_2H_positions <- data.frame(matrix(unlist(afc_shot_efc_41plus_2H$positions), ncol=4, byrow=F), stringsAsFactors = FALSE)
# No shots
# Unlisting positions column in dataframe to give a matrix of dimensions of 4 columnds and respective rows for both First and Second Halves
# This allows for use of individual columns in the plotting of the shotmap

names(afc_shot_efc_1to10_1H_positions)[1] <- "Y Start"
names(afc_shot_efc_1to10_1H_positions)[2] <- "X Start"

names(afc_shot_efc_11to20_1H_positions)[1] <- "Y Start"
names(afc_shot_efc_11to20_1H_positions)[2] <- "X Start"

names(afc_shot_efc_31to40_1H_positions)[1] <- "Y Start"
names(afc_shot_efc_31to40_1H_positions)[2] <- "X Start"

names(afc_shot_efc_41plus_1H_positions)[1] <- "Y Start"
names(afc_shot_efc_41plus_1H_positions)[2] <- "X Start"

names(afc_shot_efc_11to20_2H_positions)[1] <- "Y Start"
names(afc_shot_efc_11to20_2H_positions)[2] <- "X Start"

names(afc_shot_efc_21to30_2H_positions)[1] <- "Y Start"
names(afc_shot_efc_21to30_2H_positions)[2] <- "X Start"
# Renaming columns for ease and comprehension

afc_efc_1to10_1H_y1 <- as.vector(afc_shot_efc_1to10_1H_positions[[1]], mode="integer")
afc_efc_1to10_1H_x1 <- as.vector(afc_shot_efc_1to10_1H_positions[[2]], mode="integer")

afc_efc_11to20_1H_y1 <- as.vector(afc_shot_efc_11to20_1H_positions[[1]], mode="integer")
afc_efc_11to20_1H_x1 <- as.vector(afc_shot_efc_11to20_1H_positions[[2]], mode="integer")

afc_efc_31to40_1H_y1 <- as.vector(afc_shot_efc_31to40_1H_positions[[1]], mode="integer")
afc_efc_31to40_1H_x1 <- as.vector(afc_shot_efc_31to40_1H_positions[[2]], mode="integer")

afc_efc_41plus_1H_y1 <- as.vector(afc_shot_efc_41plus_1H_positions[[1]], mode="integer")
afc_efc_41plus_1H_x1 <- as.vector(afc_shot_efc_41plus_1H_positions[[2]], mode="integer")

afc_efc_11to20_2H_y1 <- as.vector(afc_shot_efc_11to20_2H_positions[[1]], mode="integer")
afc_efc_11to20_2H_x1 <- as.vector(afc_shot_efc_11to20_2H_positions[[2]], mode="integer")

afc_efc_21to30_2H_y1 <- as.vector(afc_shot_efc_21to30_2H_positions[[1]], mode="integer")
afc_efc_21to30_2H_x1 <- as.vector(afc_shot_efc_21to30_2H_positions[[2]], mode="integer")
# Defining variables to be used for x and y co-ordinates in shot map

ggplot(afc_shot_efc_1to10_1H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_efc_1to10_1H_x1, y=afc_efc_1to10_1H_y1), col = "red", size = 2) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. Everton F.C.", "Squad Shot Map, 1'-10' First Half") 

ggplot(afc_shot_efc_11to20_1H_positions) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_efc_11to20_1H_x1, y=afc_efc_11to20_1H_y1), col = "red", size = 2) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. Everton F.C.", "Squad Shot Map, 11'-20' First Half") 

ggplot(afc_shot_efc_31to40_1H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_efc_31to40_1H_x1, y=afc_efc_31to40_1H_y1), col = "red", size = 2) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. Everton F.C.", "Squad Shot Map, 31'-40' First Half") 

ggplot(afc_shot_efc_41plus_1H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_efc_41plus_1H_x1, y=afc_efc_41plus_1H_y1), col = "red", size = 2) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. Everton F.C.", "Squad Shot Map, 41'+ First Half") 

ggplot(afc_shot_efc_11to20_2H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_efc_11to20_2H_x1, y=afc_efc_11to20_2H_y1), col = "red", size = 2) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. Everton F.C.", "Squad Shot Map, 11'-20' Second Half") 

ggplot(afc_shot_efc_21to30_2H) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_efc_21to30_2H_x1, y=afc_efc_21to30_2H_y1), col = "red", size = 2) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. Everton F.C.", "Squad Shot Map, 21'-30' Second Half") 
# Creating shotmap using ggplot/ggsoccer


## Case Study - Exemplar Match Report ##

# Arsenal v Huddersfield - 5-0, front 3 is Ozil, Laca, Alexis with all scoring 1 goal, Giroud coming off bench to score 2 - 3421 formation
# Analyse the team passmaps in 10 minute increments
# Analyse the shots of the forward players


## Case Study - AFC Passmaps ##

afc_v_htfc_pass_1H_1to10 <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId == "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "1H" & events_df_subset_1609$eventSec <= 599))
afc_v_htfc_pass_1H_11to20 <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId == "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "1H" & events_df_subset_1609$eventSec >= 600 & events_df_subset_1609$eventSec <= 1199))
afc_v_htfc_pass_1H_21to30 <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId == "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "1H" & events_df_subset_1609$eventSec >= 1200 & events_df_subset_1609$eventSec <= 1799))
afc_v_htfc_pass_1H_31to40 <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId == "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "1H" & events_df_subset_1609$eventSec >= 1800 & events_df_subset_1609$eventSec <= 2399))
afc_v_htfc_pass_1H_41plus <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId == "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "1H" & events_df_subset_1609$eventSec >= 2400 & events_df_subset_1609$eventSec <= 2999))

afc_v_htfc_pass_2H_1to10 <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId == "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "2H" & events_df_subset_1609$eventSec <= 599))
afc_v_htfc_pass_2H_11to20 <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId == "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "2H" & events_df_subset_1609$eventSec >= 600 & events_df_subset_1609$eventSec <= 1199))
afc_v_htfc_pass_2H_21to30 <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId == "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "2H" & events_df_subset_1609$eventSec >= 1200 & events_df_subset_1609$eventSec <= 1799))
afc_v_htfc_pass_2H_31to40 <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId == "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "2H" & events_df_subset_1609$eventSec >= 1800 & events_df_subset_1609$eventSec <= 2399))
afc_v_htfc_pass_2H_41plus <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId == "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "2H" & events_df_subset_1609$eventSec >= 2400 & events_df_subset_1609$eventSec <= 2999))
# Creating subsets of passes made by Arsenal Squad in 10 minute increments v HTFC

afc_v_htfc_1to10_1H_positions <- data.frame(matrix(unlist(afc_v_htfc_pass_1H_1to10$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_11to20_1H_positions <- data.frame(matrix(unlist(afc_v_htfc_pass_1H_11to20$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_21to30_1H_positions <- data.frame(matrix(unlist(afc_v_htfc_pass_1H_21to30$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_31to40_1H_positions <- data.frame(matrix(unlist(afc_v_htfc_pass_1H_31to40$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_41plus_1H_positions <- data.frame(matrix(unlist(afc_v_htfc_pass_1H_41plus$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)

afc_v_htfc_1to10_2H_positions <- data.frame(matrix(unlist(afc_v_htfc_pass_2H_1to10$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_11to20_2H_positions <- data.frame(matrix(unlist(afc_v_htfc_pass_2H_11to20$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_21to30_2H_positions <- data.frame(matrix(unlist(afc_v_htfc_pass_2H_21to30$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_31to40_2H_positions <- data.frame(matrix(unlist(afc_v_htfc_pass_2H_31to40$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_41plus_2H_positions <- data.frame(matrix(unlist(afc_v_htfc_pass_2H_41plus$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
# Unlisting positions column in dataframe to give a matrix of dimensions of 4 columnds and respective rows for both First and Second Halves
# This allows for use of individual columns in the plotting of the passmaps

names(afc_v_htfc_1to10_1H_positions)[1] <- "Y Start"
names(afc_v_htfc_1to10_1H_positions)[2] <- "X Start"
names(afc_v_htfc_1to10_1H_positions)[3] <- "Y End"
names(afc_v_htfc_1to10_1H_positions)[4] <- "X End"

names(afc_v_htfc_11to20_1H_positions)[1] <- "Y Start"
names(afc_v_htfc_11to20_1H_positions)[2] <- "X Start"
names(afc_v_htfc_11to20_1H_positions)[3] <- "Y End"
names(afc_v_htfc_11to20_1H_positions)[4] <- "X End"

names(afc_v_htfc_21to30_1H_positions)[1] <- "Y Start"
names(afc_v_htfc_21to30_1H_positions)[2] <- "X Start"
names(afc_v_htfc_21to30_1H_positions)[3] <- "Y End"
names(afc_v_htfc_21to30_1H_positions)[4] <- "X End"

names(afc_v_htfc_31to40_1H_positions)[1] <- "Y Start"
names(afc_v_htfc_31to40_1H_positions)[2] <- "X Start"
names(afc_v_htfc_31to40_1H_positions)[3] <- "Y End"
names(afc_v_htfc_31to40_1H_positions)[4] <- "X End"

names(afc_v_htfc_41plus_1H_positions)[1] <- "Y Start"
names(afc_v_htfc_41plus_1H_positions)[2] <- "X Start"
names(afc_v_htfc_41plus_1H_positions)[3] <- "Y End"
names(afc_v_htfc_41plus_1H_positions)[4] <- "X End"

names(afc_v_htfc_1to10_2H_positions)[1] <- "Y Start"
names(afc_v_htfc_1to10_2H_positions)[2] <- "X Start"
names(afc_v_htfc_1to10_2H_positions)[3] <- "Y End"
names(afc_v_htfc_1to10_2H_positions)[4] <- "X End"

names(afc_v_htfc_11to20_2H_positions)[1] <- "Y Start"
names(afc_v_htfc_11to20_2H_positions)[2] <- "X Start"
names(afc_v_htfc_11to20_2H_positions)[3] <- "Y End"
names(afc_v_htfc_11to20_2H_positions)[4] <- "X End"

names(afc_v_htfc_21to30_2H_positions)[1] <- "Y Start"
names(afc_v_htfc_21to30_2H_positions)[2] <- "X Start"
names(afc_v_htfc_21to30_2H_positions)[3] <- "Y End"
names(afc_v_htfc_21to30_2H_positions)[4] <- "X End"

names(afc_v_htfc_31to40_2H_positions)[1] <- "Y Start"
names(afc_v_htfc_31to40_2H_positions)[2] <- "X Start"
names(afc_v_htfc_31to40_2H_positions)[3] <- "Y End"
names(afc_v_htfc_31to40_2H_positions)[4] <- "X End"

names(afc_v_htfc_41plus_2H_positions)[1] <- "Y Start"
names(afc_v_htfc_41plus_2H_positions)[2] <- "X Start"
names(afc_v_htfc_41plus_2H_positions)[3] <- "Y End"
names(afc_v_htfc_41plus_2H_positions)[4] <- "X End"
# Renaming columns for ease and comprehension

afc_v_htfc_1to10_1H_y1 <- as.vector(afc_v_htfc_1to10_1H_positions[[1]], mode="integer")
afc_v_htfc_1to10_1H_x1 <- as.vector(afc_v_htfc_1to10_1H_positions[[2]], mode="integer")
afc_v_htfc_1to10_1H_y2 <- as.vector(afc_v_htfc_1to10_1H_positions[[3]], mode="integer")
afc_v_htfc_1to10_1H_x2 <- as.vector(afc_v_htfc_1to10_1H_positions[[4]], mode="integer")

afc_v_htfc_11to20_1H_y1 <- as.vector(afc_v_htfc_11to20_1H_positions[[1]], mode="integer")
afc_v_htfc_11to20_1H_x1 <- as.vector(afc_v_htfc_11to20_1H_positions[[2]], mode="integer")
afc_v_htfc_11to20_1H_y2 <- as.vector(afc_v_htfc_11to20_1H_positions[[3]], mode="integer")
afc_v_htfc_11to20_1H_x2 <- as.vector(afc_v_htfc_11to20_1H_positions[[4]], mode="integer")

afc_v_htfc_21to30_1H_y1 <- as.vector(afc_v_htfc_21to30_1H_positions[[1]], mode="integer")
afc_v_htfc_21to30_1H_x1 <- as.vector(afc_v_htfc_21to30_1H_positions[[2]], mode="integer")
afc_v_htfc_21to30_1H_y2 <- as.vector(afc_v_htfc_21to30_1H_positions[[3]], mode="integer")
afc_v_htfc_21to30_1H_x2 <- as.vector(afc_v_htfc_21to30_1H_positions[[4]], mode="integer")

afc_v_htfc_31to40_1H_y1 <- as.vector(afc_v_htfc_31to40_1H_positions[[1]], mode="integer")
afc_v_htfc_31to40_1H_x1 <- as.vector(afc_v_htfc_31to40_1H_positions[[2]], mode="integer")
afc_v_htfc_31to40_1H_y2 <- as.vector(afc_v_htfc_31to40_1H_positions[[3]], mode="integer")
afc_v_htfc_31to40_1H_x2 <- as.vector(afc_v_htfc_31to40_1H_positions[[4]], mode="integer")

afc_v_htfc_41plus_1H_y1 <- as.vector(afc_v_htfc_41plus_1H_positions[[1]], mode="integer")
afc_v_htfc_41plus_1H_x1 <- as.vector(afc_v_htfc_41plus_1H_positions[[2]], mode="integer")
afc_v_htfc_41plus_1H_y2 <- as.vector(afc_v_htfc_41plus_1H_positions[[3]], mode="integer")
afc_v_htfc_41plus_1H_x2 <- as.vector(afc_v_htfc_41plus_1H_positions[[4]], mode="integer")

afc_v_htfc_1to10_2H_y1 <- as.vector(afc_v_htfc_1to10_2H_positions[[1]], mode="integer")
afc_v_htfc_1to10_2H_x1 <- as.vector(afc_v_htfc_1to10_2H_positions[[2]], mode="integer")
afc_v_htfc_1to10_2H_y2 <- as.vector(afc_v_htfc_1to10_2H_positions[[3]], mode="integer")
afc_v_htfc_1to10_2H_x2 <- as.vector(afc_v_htfc_1to10_2H_positions[[4]], mode="integer")

afc_v_htfc_11to20_2H_y1 <- as.vector(afc_v_htfc_11to20_2H_positions[[1]], mode="integer")
afc_v_htfc_11to20_2H_x1 <- as.vector(afc_v_htfc_11to20_2H_positions[[2]], mode="integer")
afc_v_htfc_11to20_2H_y2 <- as.vector(afc_v_htfc_11to20_2H_positions[[3]], mode="integer")
afc_v_htfc_11to20_2H_x2 <- as.vector(afc_v_htfc_11to20_2H_positions[[4]], mode="integer")

afc_v_htfc_21to30_2H_y1 <- as.vector(afc_v_htfc_21to30_2H_positions[[1]], mode="integer")
afc_v_htfc_21to30_2H_x1 <- as.vector(afc_v_htfc_21to30_2H_positions[[2]], mode="integer")
afc_v_htfc_21to30_2H_y2 <- as.vector(afc_v_htfc_21to30_2H_positions[[3]], mode="integer")
afc_v_htfc_21to30_2H_x2 <- as.vector(afc_v_htfc_21to30_2H_positions[[4]], mode="integer")

afc_v_htfc_31to40_2H_y1 <- as.vector(afc_v_htfc_31to40_2H_positions[[1]], mode="integer")
afc_v_htfc_31to40_2H_x1 <- as.vector(afc_v_htfc_31to40_2H_positions[[2]], mode="integer")
afc_v_htfc_31to40_2H_y2 <- as.vector(afc_v_htfc_31to40_2H_positions[[3]], mode="integer")
afc_v_htfc_31to40_2H_x2 <- as.vector(afc_v_htfc_31to40_2H_positions[[4]], mode="integer")

afc_v_htfc_41plus_2H_y1 <- as.vector(afc_v_htfc_41plus_2H_positions[[1]], mode="integer")
afc_v_htfc_41plus_2H_x1 <- as.vector(afc_v_htfc_41plus_2H_positions[[2]], mode="integer")
afc_v_htfc_41plus_2H_y2 <- as.vector(afc_v_htfc_41plus_2H_positions[[3]], mode="integer")
afc_v_htfc_41plus_2H_x2 <- as.vector(afc_v_htfc_41plus_2H_positions[[4]], mode="integer")
# Defining variables to be used for x and y co-ordinates in pass map

ggplot(afc_v_htfc_pass_1H_1to10) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_1to10_1H_x1, y=afc_v_htfc_1to10_1H_y1, xend=afc_v_htfc_1to10_1H_x2, yend=afc_v_htfc_1to10_1H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Arsenal F.C. First Half 1'-10' Passmap")

ggplot(afc_v_htfc_pass_1H_11to20) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_11to20_1H_x1, y=afc_v_htfc_11to20_1H_y1, xend=afc_v_htfc_11to20_1H_x2, yend=afc_v_htfc_11to20_1H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Arsenal F.C. First Half 11'-20' Passmap")

ggplot(afc_v_htfc_pass_1H_21to30) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_21to30_1H_x1, y=afc_v_htfc_21to30_1H_y1, xend=afc_v_htfc_21to30_1H_x2, yend=afc_v_htfc_21to30_1H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Arsenal F.C. First Half 21'-30' Passmap")

ggplot(afc_v_htfc_pass_1H_31to40) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_31to40_1H_x1, y=afc_v_htfc_31to40_1H_y1, xend=afc_v_htfc_31to40_1H_x2, yend=afc_v_htfc_31to40_1H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Arsenal F.C. First Half 31'-40' Passmap")

ggplot(afc_v_htfc_pass_1H_41plus) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_41plus_1H_x1, y=afc_v_htfc_41plus_1H_y1, xend=afc_v_htfc_41plus_1H_x2, yend=afc_v_htfc_41plus_1H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Arsenal F.C. First Half 41'+ Passmap")

ggplot(afc_v_htfc_pass_2H_1to10) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_1to10_2H_x1, y=afc_v_htfc_1to10_2H_y1, xend=afc_v_htfc_1to10_2H_x2, yend=afc_v_htfc_1to10_2H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Arsenal F.C. Second Half 1'-10' Passmap")

ggplot(afc_v_htfc_pass_2H_11to20) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_11to20_2H_x1, y=afc_v_htfc_11to20_2H_y1, xend=afc_v_htfc_11to20_2H_x2, yend=afc_v_htfc_11to20_2H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Arsenal F.C. Second Half 11'-20' Passmap")

ggplot(afc_v_htfc_pass_2H_21to30) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_21to30_2H_x1, y=afc_v_htfc_21to30_2H_y1, xend=afc_v_htfc_21to30_2H_x2, yend=afc_v_htfc_21to30_2H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Arsenal F.C. Second Half 21'-30' Passmap")

ggplot(afc_v_htfc_pass_2H_31to40) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_31to40_2H_x1, y=afc_v_htfc_31to40_2H_y1, xend=afc_v_htfc_31to40_2H_x2, yend=afc_v_htfc_31to40_2H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Arsenal F.C. Second Half 31'-40' Passmap")

ggplot(afc_v_htfc_pass_2H_41plus) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_41plus_2H_x1, y=afc_v_htfc_41plus_2H_y1, xend=afc_v_htfc_41plus_2H_x2, yend=afc_v_htfc_41plus_2H_y2), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Arsenal F.C. Second Half 41'+ Passmap")
# Plotting passmaps


## Case Study - HTAFC Passmaps ##

afc_v_htfc_pass_1H_1to10_hud <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId != "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "1H" & events_df_subset_1609$eventSec <= 599))
afc_v_htfc_pass_1H_11to20_hud <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId != "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "1H" & events_df_subset_1609$eventSec >= 600 & events_df_subset_1609$eventSec <= 1199))
afc_v_htfc_pass_1H_21to30_hud <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId != "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "1H" & events_df_subset_1609$eventSec >= 1200 & events_df_subset_1609$eventSec <= 1799))
afc_v_htfc_pass_1H_31to40_hud <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId != "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "1H" & events_df_subset_1609$eventSec >= 1800 & events_df_subset_1609$eventSec <= 2399))
afc_v_htfc_pass_1H_41plus_hud <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId != "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "1H" & events_df_subset_1609$eventSec >= 2400 & events_df_subset_1609$eventSec <= 2999))

afc_v_htfc_pass_2H_1to10_hud <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId != "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "2H" & events_df_subset_1609$eventSec <= 599))
afc_v_htfc_pass_2H_11to20_hud <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId != "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "2H" & events_df_subset_1609$eventSec >= 600 & events_df_subset_1609$eventSec <= 1199))
afc_v_htfc_pass_2H_21to30_hud <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId != "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "2H" & events_df_subset_1609$eventSec >= 1200 & events_df_subset_1609$eventSec <= 1799))
afc_v_htfc_pass_2H_31to40_hud <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId != "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "2H" & events_df_subset_1609$eventSec >= 1800 & events_df_subset_1609$eventSec <= 2399))
afc_v_htfc_pass_2H_41plus_hud <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$teamId != "1609" & events_df_subset_1609$eventName == "Pass" & events_df_subset_1609$matchPeriod == "2H" & events_df_subset_1609$eventSec >= 2400 & events_df_subset_1609$eventSec <= 2999))
# Creating subsets of passes made by Arsenal Squad in 10 minute increments v HTFC

afc_v_htfc_1to10_1H_positions_hud <- data.frame(matrix(unlist(afc_v_htfc_pass_1H_1to10_hud$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_11to20_1H_positions_hud <- data.frame(matrix(unlist(afc_v_htfc_pass_1H_11to20_hud$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_21to30_1H_positions_hud <- data.frame(matrix(unlist(afc_v_htfc_pass_1H_21to30_hud$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_31to40_1H_positions_hud <- data.frame(matrix(unlist(afc_v_htfc_pass_1H_31to40_hud$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_41plus_1H_positions_hud <- data.frame(matrix(unlist(afc_v_htfc_pass_1H_41plus_hud$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)

afc_v_htfc_1to10_2H_positions_hud <- data.frame(matrix(unlist(afc_v_htfc_pass_2H_1to10_hud$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_11to20_2H_positions_hud <- data.frame(matrix(unlist(afc_v_htfc_pass_2H_11to20_hud$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_21to30_2H_positions_hud <- data.frame(matrix(unlist(afc_v_htfc_pass_2H_21to30_hud$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_31to40_2H_positions_hud <- data.frame(matrix(unlist(afc_v_htfc_pass_2H_31to40_hud$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
afc_v_htfc_41plus_2H_positions_hud <- data.frame(matrix(unlist(afc_v_htfc_pass_2H_41plus_hud$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
# Unlisting positions column in dataframe to give a matrix of dimensions of 4 columnds and respective rows for both First and Second Halves
# This allows for use of individual columns in the plotting of the passmaps

names(afc_v_htfc_1to10_1H_positions_hud)[1] <- "Y Start"
names(afc_v_htfc_1to10_1H_positions_hud)[2] <- "X Start"
names(afc_v_htfc_1to10_1H_positions_hud)[3] <- "Y End"
names(afc_v_htfc_1to10_1H_positions_hud)[4] <- "X End"

names(afc_v_htfc_11to20_1H_positions_hud)[1] <- "Y Start"
names(afc_v_htfc_11to20_1H_positions_hud)[2] <- "X Start"
names(afc_v_htfc_11to20_1H_positions_hud)[3] <- "Y End"
names(afc_v_htfc_11to20_1H_positions_hud)[4] <- "X End"

names(afc_v_htfc_21to30_1H_positions_hud)[1] <- "Y Start"
names(afc_v_htfc_21to30_1H_positions_hud)[2] <- "X Start"
names(afc_v_htfc_21to30_1H_positions_hud)[3] <- "Y End"
names(afc_v_htfc_21to30_1H_positions_hud)[4] <- "X End"

names(afc_v_htfc_31to40_1H_positions_hud)[1] <- "Y Start"
names(afc_v_htfc_31to40_1H_positions_hud)[2] <- "X Start"
names(afc_v_htfc_31to40_1H_positions_hud)[3] <- "Y End"
names(afc_v_htfc_31to40_1H_positions_hud)[4] <- "X End"

names(afc_v_htfc_41plus_1H_positions_hud)[1] <- "Y Start"
names(afc_v_htfc_41plus_1H_positions_hud)[2] <- "X Start"
names(afc_v_htfc_41plus_1H_positions_hud)[3] <- "Y End"
names(afc_v_htfc_41plus_1H_positions_hud)[4] <- "X End"

names(afc_v_htfc_1to10_2H_positions_hud)[1] <- "Y Start"
names(afc_v_htfc_1to10_2H_positions_hud)[2] <- "X Start"
names(afc_v_htfc_1to10_2H_positions_hud)[3] <- "Y End"
names(afc_v_htfc_1to10_2H_positions_hud)[4] <- "X End"

names(afc_v_htfc_11to20_2H_positions_hud)[1] <- "Y Start"
names(afc_v_htfc_11to20_2H_positions_hud)[2] <- "X Start"
names(afc_v_htfc_11to20_2H_positions_hud)[3] <- "Y End"
names(afc_v_htfc_11to20_2H_positions_hud)[4] <- "X End"

names(afc_v_htfc_21to30_2H_positions_hud)[1] <- "Y Start"
names(afc_v_htfc_21to30_2H_positions_hud)[2] <- "X Start"
names(afc_v_htfc_21to30_2H_positions_hud)[3] <- "Y End"
names(afc_v_htfc_21to30_2H_positions_hud)[4] <- "X End"

names(afc_v_htfc_31to40_2H_positions_hud)[1] <- "Y Start"
names(afc_v_htfc_31to40_2H_positions_hud)[2] <- "X Start"
names(afc_v_htfc_31to40_2H_positions_hud)[3] <- "Y End"
names(afc_v_htfc_31to40_2H_positions_hud)[4] <- "X End"

names(afc_v_htfc_41plus_2H_positions_hud)[1] <- "Y Start"
names(afc_v_htfc_41plus_2H_positions_hud)[2] <- "X Start"
names(afc_v_htfc_41plus_2H_positions_hud)[3] <- "Y End"
names(afc_v_htfc_41plus_2H_positions_hud)[4] <- "X End"
# Renaming columns for ease and comprehension

afc_v_htfc_1to10_1H_y1_hud <- as.vector(afc_v_htfc_1to10_1H_positions_hud[[1]], mode="integer")
afc_v_htfc_1to10_1H_x1_hud <- as.vector(afc_v_htfc_1to10_1H_positions_hud[[2]], mode="integer")
afc_v_htfc_1to10_1H_y2_hud <- as.vector(afc_v_htfc_1to10_1H_positions_hud[[3]], mode="integer")
afc_v_htfc_1to10_1H_x2_hud <- as.vector(afc_v_htfc_1to10_1H_positions_hud[[4]], mode="integer")

afc_v_htfc_11to20_1H_y1_hud <- as.vector(afc_v_htfc_11to20_1H_positions_hud[[1]], mode="integer")
afc_v_htfc_11to20_1H_x1_hud <- as.vector(afc_v_htfc_11to20_1H_positions_hud[[2]], mode="integer")
afc_v_htfc_11to20_1H_y2_hud <- as.vector(afc_v_htfc_11to20_1H_positions_hud[[3]], mode="integer")
afc_v_htfc_11to20_1H_x2_hud <- as.vector(afc_v_htfc_11to20_1H_positions_hud[[4]], mode="integer")

afc_v_htfc_21to30_1H_y1_hud <- as.vector(afc_v_htfc_21to30_1H_positions_hud[[1]], mode="integer")
afc_v_htfc_21to30_1H_x1_hud <- as.vector(afc_v_htfc_21to30_1H_positions_hud[[2]], mode="integer")
afc_v_htfc_21to30_1H_y2_hud <- as.vector(afc_v_htfc_21to30_1H_positions_hud[[3]], mode="integer")
afc_v_htfc_21to30_1H_x2_hud <- as.vector(afc_v_htfc_21to30_1H_positions_hud[[4]], mode="integer")

afc_v_htfc_31to40_1H_y1_hud <- as.vector(afc_v_htfc_31to40_1H_positions_hud[[1]], mode="integer")
afc_v_htfc_31to40_1H_x1_hud <- as.vector(afc_v_htfc_31to40_1H_positions_hud[[2]], mode="integer")
afc_v_htfc_31to40_1H_y2_hud <- as.vector(afc_v_htfc_31to40_1H_positions_hud[[3]], mode="integer")
afc_v_htfc_31to40_1H_x2_hud <- as.vector(afc_v_htfc_31to40_1H_positions_hud[[4]], mode="integer")

afc_v_htfc_41plus_1H_y1_hud <- as.vector(afc_v_htfc_41plus_1H_positions_hud[[1]], mode="integer")
afc_v_htfc_41plus_1H_x1_hud <- as.vector(afc_v_htfc_41plus_1H_positions_hud[[2]], mode="integer")
afc_v_htfc_41plus_1H_y2_hud <- as.vector(afc_v_htfc_41plus_1H_positions_hud[[3]], mode="integer")
afc_v_htfc_41plus_1H_x2_hud <- as.vector(afc_v_htfc_41plus_1H_positions_hud[[4]], mode="integer")

afc_v_htfc_1to10_2H_y1_hud <- as.vector(afc_v_htfc_1to10_2H_positions_hud[[1]], mode="integer")
afc_v_htfc_1to10_2H_x1_hud <- as.vector(afc_v_htfc_1to10_2H_positions_hud[[2]], mode="integer")
afc_v_htfc_1to10_2H_y2_hud <- as.vector(afc_v_htfc_1to10_2H_positions_hud[[3]], mode="integer")
afc_v_htfc_1to10_2H_x2_hud <- as.vector(afc_v_htfc_1to10_2H_positions_hud[[4]], mode="integer")

afc_v_htfc_11to20_2H_y1_hud <- as.vector(afc_v_htfc_11to20_2H_positions_hud[[1]], mode="integer")
afc_v_htfc_11to20_2H_x1_hud <- as.vector(afc_v_htfc_11to20_2H_positions_hud[[2]], mode="integer")
afc_v_htfc_11to20_2H_y2_hud <- as.vector(afc_v_htfc_11to20_2H_positions_hud[[3]], mode="integer")
afc_v_htfc_11to20_2H_x2_hud <- as.vector(afc_v_htfc_11to20_2H_positions_hud[[4]], mode="integer")

afc_v_htfc_21to30_2H_y1_hud <- as.vector(afc_v_htfc_21to30_2H_positions_hud[[1]], mode="integer")
afc_v_htfc_21to30_2H_x1_hud <- as.vector(afc_v_htfc_21to30_2H_positions_hud[[2]], mode="integer")
afc_v_htfc_21to30_2H_y2_hud <- as.vector(afc_v_htfc_21to30_2H_positions_hud[[3]], mode="integer")
afc_v_htfc_21to30_2H_x2_hud <- as.vector(afc_v_htfc_21to30_2H_positions_hud[[4]], mode="integer")

afc_v_htfc_31to40_2H_y1_hud <- as.vector(afc_v_htfc_31to40_2H_positions_hud[[1]], mode="integer")
afc_v_htfc_31to40_2H_x1_hud <- as.vector(afc_v_htfc_31to40_2H_positions_hud[[2]], mode="integer")
afc_v_htfc_31to40_2H_y2_hud <- as.vector(afc_v_htfc_31to40_2H_positions_hud[[3]], mode="integer")
afc_v_htfc_31to40_2H_x2_hud <- as.vector(afc_v_htfc_31to40_2H_positions_hud[[4]], mode="integer")

afc_v_htfc_41plus_2H_y1_hud <- as.vector(afc_v_htfc_41plus_2H_positions_hud[[1]], mode="integer")
afc_v_htfc_41plus_2H_x1_hud <- as.vector(afc_v_htfc_41plus_2H_positions_hud[[2]], mode="integer")
afc_v_htfc_41plus_2H_y2_hud <- as.vector(afc_v_htfc_41plus_2H_positions_hud[[3]], mode="integer")
afc_v_htfc_41plus_2H_x2_hud <- as.vector(afc_v_htfc_41plus_2H_positions_hud[[4]], mode="integer")
# Defining variables to be used for x and y co-ordinates in pass map

ggplot(afc_v_htfc_pass_1H_1to10_hud) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_1to10_1H_x1_hud, y=afc_v_htfc_1to10_1H_y1_hud, xend=afc_v_htfc_1to10_1H_x2_hud, yend=afc_v_htfc_1to10_1H_y2_hud), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Huddersfield Town A.F.C. First Half 1'-10' Passmap")

ggplot(afc_v_htfc_pass_1H_11to20_hud) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_11to20_1H_x1_hud, y=afc_v_htfc_11to20_1H_y1_hud, xend=afc_v_htfc_11to20_1H_x2_hud, yend=afc_v_htfc_11to20_1H_y2_hud), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Huddersfield Town A.F.C. First Half 11'-20' Passmap")

ggplot(afc_v_htfc_pass_1H_21to30_hud) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_21to30_1H_x1_hud, y=afc_v_htfc_21to30_1H_y1_hud, xend=afc_v_htfc_21to30_1H_x2_hud, yend=afc_v_htfc_21to30_1H_y2_hud), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Huddersfield Town A.F.C. First Half 21'-30' Passmap")

ggplot(afc_v_htfc_pass_1H_31to40_hud) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_31to40_1H_x1_hud, y=afc_v_htfc_31to40_1H_y1_hud, xend=afc_v_htfc_31to40_1H_x2_hud, yend=afc_v_htfc_31to40_1H_y2_hud), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Huddersfield Town A.F.C. First Half 31'-40' Passmap")

ggplot(afc_v_htfc_pass_1H_41plus_hud) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_41plus_1H_x1_hud, y=afc_v_htfc_41plus_1H_y1_hud, xend=afc_v_htfc_41plus_1H_x2_hud, yend=afc_v_htfc_41plus_1H_y2_hud), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Huddersfield Town A.F.C. First Half 41'+ Passmap")

ggplot(afc_v_htfc_pass_2H_1to10_hud) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_1to10_2H_x1_hud, y=afc_v_htfc_1to10_2H_y1_hud, xend=afc_v_htfc_1to10_2H_x2_hud, yend=afc_v_htfc_1to10_2H_y2_hud), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Huddersfield Town A.F.C. Second Half 1'-10' Passmap")

ggplot(afc_v_htfc_pass_2H_11to20_hud) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_11to20_2H_x1_hud, y=afc_v_htfc_11to20_2H_y1_hud, xend=afc_v_htfc_11to20_2H_x2_hud, yend=afc_v_htfc_11to20_2H_y2_hud), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Huddersfield Town A.F.C. Second Half 11'-20' Passmap")

ggplot(afc_v_htfc_pass_2H_21to30_hud) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_21to30_2H_x1_hud, y=afc_v_htfc_21to30_2H_y1_hud, xend=afc_v_htfc_21to30_2H_x2_hud, yend=afc_v_htfc_21to30_2H_y2_hud), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Huddersfield Town A.F.C. Second Half 21'-30' Passmap")

ggplot(afc_v_htfc_pass_2H_31to40_hud) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_31to40_2H_x1_hud, y=afc_v_htfc_31to40_2H_y1_hud, xend=afc_v_htfc_31to40_2H_x2_hud, yend=afc_v_htfc_31to40_2H_y2_hud), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Squad Huddersfield Town A.F.C. Second Half 31'-40' Passmap")

ggplot(afc_v_htfc_pass_2H_41plus_hud) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = TRUE, dimensions = pitch_opta) + 
  geom_segment(aes(x=afc_v_htfc_41plus_2H_x1_hud, y=afc_v_htfc_41plus_2H_y1_hud, xend=afc_v_htfc_41plus_2H_x2_hud, yend=afc_v_htfc_41plus_2H_y2_hud), arrow = arrow(length = unit(0.25, "cm"), type="closed")) +
  theme_pitch() + 
  direction_label() + 
  ggtitle("Arsenal FC vs Huddersfield Town A.F.C., 5-0", "Entire Huddersfield Town A.F.C. Squad Second Half 41'+ Passmap")
# Plotting passmaps


## Case Study Shotmaps ##

# In this part, we look at the shot maps of the 3 forwards, including all goalscorers 

## Case Study - Entire Squad Shotmap ##

afc_v_htfc_squad_shots <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$eventName == "Shot"))
# Creating subset of shots made by Arsenal in AFC v HTAFC (Both Halves)

afc_v_htfc_positions <- data.frame(matrix(unlist(afc_v_htfc_squad_shots$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
# Unlisting positions column in dataframe to give a matrix of dimensions of 4 columnds and respective rows for both First and Second Halves
# This allows for use of individual columns in the plotting of the shotmaps

names(afc_v_htfc_positions)[1] <- "Y Start"
names(afc_v_htfc_positions)[2] <- "X Start"
# Renaming columns for ease and comprehension

afc_v_htfc_squad_y1 <- as.vector(afc_v_htfc_positions[[1]], mode="integer")
afc_v_htfc_squad_x1 <- as.vector(afc_v_htfc_positions[[2]], mode="integer")
# Defining variables to be used for x and y co-ordinates in shot map

ggplot(afc_v_htfc_squad_shots) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_v_htfc_squad_x1, y=afc_v_htfc_squad_y1), col = "red", size = 2) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. Huddersfield Town A.F.C.", "Entire Squad Shot Map") 
# Creating shotmap using ggplot/ggsoccer


## Case Study - Alexis Shotmap ##

alexis <- subset(players_df, players_df$currentTeamId == "1611")
# Using this subset to extract the information of playerID

afc_v_htfc_alexis_shots <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$eventName == "Shot" & events_df_subset_1609$playerId == "3361"))
# For analysis, need to look manually for playerID although team would have this, as Alexis moved clubs in Janaury transfer window so does not appear on Arsenal players subset
# Creating subset of shots made by Alexis in AFC v HTAFC (Both Halves)

afc_v_htfc_alexis_positions <- data.frame(matrix(unlist(afc_v_htfc_alexis_shots$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
# Unlisting positions column in dataframe to give a matrix of dimensions of 4 columnds and respective rows for both First and Second Halves
# This allows for use of individual columns in the plotting of the shotmaps

names(afc_v_htfc_alexis_positions)[1] <- "Y Start"
names(afc_v_htfc_alexis_positions)[2] <- "X Start"
# Renaming columns for ease and comprehension

afc_v_htfc_alexis_y1 <- as.vector(afc_v_htfc_alexis_positions[[1]], mode="integer")
afc_v_htfc_alexis_x1 <- as.vector(afc_v_htfc_alexis_positions[[2]], mode="integer")
# Defining variables to be used for x and y co-ordinates in shot map

ggplot(afc_v_htfc_alexis_shots) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_v_htfc_alexis_x1, y=afc_v_htfc_alexis_y1), col = "blue", size = 2) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. Huddersfield Town A.F.C.", "A. Sanchez Shot Map") 
# Creating shotmap using ggplot/ggsoccer


## Case Study - Ozil Shotmap ##

afc_v_htfc_ozil_shots <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$eventName == "Shot" & events_df_subset_1609$playerId == "3319"))
# Creating subset of shots made by Ozil in AFC v HTAFC (Both Halves)

afc_v_htfc_ozil_positions <- data.frame(matrix(unlist(afc_v_htfc_ozil_shots$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
# Unlisting positions column in dataframe to give a matrix of dimensions of 4 columnds and respective rows for both First and Second Halves
# This allows for use of individual columns in the plotting of the shotmaps

names(afc_v_htfc_ozil_positions)[1] <- "Y Start"
names(afc_v_htfc_ozil_positions)[2] <- "X Start"
# Renaming columns for ease and comprehension

afc_v_htfc_ozil_y1 <- as.vector(afc_v_htfc_ozil_positions[[1]], mode="integer")
afc_v_htfc_ozil_x1 <- as.vector(afc_v_htfc_ozil_positions[[2]], mode="integer")
# Defining variables to be used for x and y co-ordinates in shot map

ggplot(afc_v_htfc_ozil_shots) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_v_htfc_ozil_x1, y=afc_v_htfc_ozil_y1), col = "yellow", size = 2) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. Huddersfield Town A.F.C.", "M. Özil Shot Map") 
# Creating shotmap using ggplot/ggsoccer


## Case Study - Laca Shotmap ##

afc_v_htfc_laca_shots <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$eventName == "Shot" & events_df_subset_1609$playerId == "25413"))
# Creating subset of shots made by Lacazette in AFC v HTAFC (Both Halves)

afc_v_htfc_laca_positions <- data.frame(matrix(unlist(afc_v_htfc_laca_shots$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
# Unlisting positions column in dataframe to give a matrix of dimensions of 4 columnds and respective rows for both First and Second Halves
# This allows for use of individual columns in the plotting of the shotmaps

names(afc_v_htfc_laca_positions)[1] <- "Y Start"
names(afc_v_htfc_laca_positions)[2] <- "X Start"
# Renaming columns for ease and comprehension

afc_v_htfc_laca_y1 <- as.vector(afc_v_htfc_laca_positions[[1]], mode="integer")
afc_v_htfc_laca_x1 <- as.vector(afc_v_htfc_laca_positions[[2]], mode="integer")
# Defining variables to be used for x and y co-ordinates in shot map

ggplot(afc_v_htfc_laca_shots) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_v_htfc_laca_x1, y=afc_v_htfc_laca_y1), col = "purple", size = 2) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. Huddersfield Town A.F.C.", "A. Lacazette Shot Map") 
# Creating shotmap using ggplot/ggsoccer


## Case Study - Giroud Shotmap ##

giroud <- subset(players_df, players_df$lastName == "Giroud")
# Using this subset to extract the information of playerID

afc_v_htfc_giroud_shots <- subset(events_df_subset_1609, (events_df_subset_1609$matchId == "2499850" & events_df_subset_1609$eventName == "Shot" & events_df_subset_1609$playerId == "26010"))
# Creating subset of shots made by Giroud in AFC v HTAFC (Both Halves)

afc_v_htfc_giroud_positions <- data.frame(matrix(unlist(afc_v_htfc_giroud_shots$positions), ncol=4, byrow=T), stringsAsFactors = FALSE)
# Unlisting positions column in dataframe to give a matrix of dimensions of 4 columnds and respective rows for both First and Second Halves
# This allows for use of individual columns in the plotting of the shotmaps

names(afc_v_htfc_giroud_positions)[1] <- "Y Start"
names(afc_v_htfc_giroud_positions)[2] <- "X Start"
# Renaming columns for ease and comprehension

afc_v_htfc_giroud_y1 <- as.vector(afc_v_htfc_giroud_positions[[1]], mode="integer")
afc_v_htfc_giroud_x1 <- as.vector(afc_v_htfc_giroud_positions[[2]], mode="integer")
# Defining variables to be used for x and y co-ordinates in shot map

ggplot(afc_v_htfc_giroud_shots) + 
  annotate_pitch(col = "white", fill = "chartreuse4", limits = FALSE) + 
  geom_point(aes(x=afc_v_htfc_giroud_x1, y=afc_v_htfc_giroud_y1), col = "magenta", size = 2) +
  theme_pitch() + 
  theme(plot.background = element_rect(fill = "chartreuse4"),title = element_text(colour = "white")) + 
  coord_flip(xlim=c(49,101), ylim=c(-1, 101)) + 
  ggtitle("Arsenal F.C. vs. Huddersfield Town A.F.C.", "O. Giroud Shot Map") 
# Creating shotmap using ggplot/ggsoccer