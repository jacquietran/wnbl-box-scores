##### Load libraries ###############################################################################

library(readr)
library(dplyr)
library(stringr)

##### Read data into R #############################################################################

IDs201415 <- read_csv("IDs201415.csv",
											col_names = TRUE, col_types = NULL)

WNBLBoxScores201415Raw <- read_csv("WNBLBoxScores201415Raw.csv",
																	 col_names = TRUE, col_types = NULL)

##### Data cleaning ################################################################################

WNBLBoxScores201415 <- WNBLBoxScores201415Raw

# Add gameIDs into the WNBLBoxScores201415 data frame
WNBLBoxScores201415 <- inner_join(WNBLBoxScores201415, IDs201415)

# Use gameIDs to identify year / month / day of game
WNBLBoxScores201415$date <- paste(str_sub(WNBLBoxScores201415$gameIDs201415, 1, 4),
																	str_sub(WNBLBoxScores201415$gameIDs201415, 5, 6),
																	str_sub(WNBLBoxScores201415$gameIDs201415, 7, 8),
																	sep = "-")
WNBLBoxScores201415$date <- as.Date(WNBLBoxScores201415$date, "%Y-%m-%d")

# Use gameIDs to identify time of game
WNBLBoxScores201415$time <- str_sub(WNBLBoxScores201415$gameIDs201415, 10, 13)
WNBLBoxScores201415$time <- format(strptime(WNBLBoxScores201415$time, "%H%M"), format = "%H%M")

# Specify which round each game was placed in
WNBLBoxScores201415 <- WNBLBoxScores201415[order(WNBLBoxScores201415$gameIDs201415), ]
WNBLBoxScores201415$round <- as.character(WNBLBoxScores201415$date)
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2014-10-17" & WNBLBoxScores201415$round <= "2014-10-18"] <- 1
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2014-10-23" & WNBLBoxScores201415$round <= "2014-10-26"] <- 2
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2014-10-30" & WNBLBoxScores201415$round <= "2014-11-01"] <- 3
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2014-11-05" & WNBLBoxScores201415$round <= "2014-11-09"] <- 4
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2014-11-14" & WNBLBoxScores201415$round <= "2014-11-16"] <- 5
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2014-11-21" & WNBLBoxScores201415$round <= "2014-11-22"] <- 6
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2014-11-28" & WNBLBoxScores201415$round <= "2014-11-30"] <- 7
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2014-12-05" & WNBLBoxScores201415$round <= "2014-12-07"] <- 8
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2014-12-12" & WNBLBoxScores201415$round <= "2014-12-14"] <- 9
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2014-12-19" & WNBLBoxScores201415$round <= "2014-12-20"] <- 10
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2015-01-02" & WNBLBoxScores201415$round <= "2015-01-04"] <- 11
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2015-01-09" & WNBLBoxScores201415$round <= "2015-01-11"] <- 12
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2015-01-15" & WNBLBoxScores201415$round <= "2015-01-18"] <- 13
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2015-01-23" & WNBLBoxScores201415$round <= "2015-01-25"] <- 14
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2015-01-30" & WNBLBoxScores201415$round <= "2015-02-01"] <- 15
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2015-02-06" & WNBLBoxScores201415$round <= "2015-02-08"] <- 16
WNBLBoxScores201415$round[
	WNBLBoxScores201415$round >= "2015-02-13" & WNBLBoxScores201415$round <= "2015-02-15"] <- 17

# Convert points margins to categorical team results
WNBLBoxScores201415$teamResult <- ifelse(WNBLBoxScores201415$pointsMargin > 0, "Win",
																				 ifelse(WNBLBoxScores201415$pointsMargin < 0, "Loss",
																				 			 "Draw"))

# Recode team names for consistency
WNBLBoxScores201415$teamName <- recode(WNBLBoxScores201415$teamName,
																			 `University of Canberra` = "Canberra",
																			 `UC Capitals` = "Canberra",
																			 `Sydney Uni` = "Sydney",
																			 `Sydney Uni Flames` ="Sydney",
																			 `Melbourne Boomers` = "Melbourne",
																			 `Adelaide Lightning` = "Adelaide",
																			 `Townsville Fire` = "Townsville",
																			 `Bendigo Spirit` = "Bendigo",
																			 `Perth Lynx` = "Perth",
																			 `Dandenong Rangers` = "Dandenong",
																			 `SEQ Stars` = "South-East Queensland")
WNBLBoxScores201415$oppName <- recode(WNBLBoxScores201415$oppName,
																			 `University of Canberra` = "Canberra",
																			 `UC Capitals` = "Canberra",
																			 `Sydney Uni` = "Sydney",
																			 `Sydney Uni Flames` ="Sydney",
																			 `Melbourne Boomers` = "Melbourne",
																			 `Adelaide Lightning` = "Adelaide",
																			 `Townsville Fire` = "Townsville",
																			 `Bendigo Spirit` = "Bendigo",
																			 `Perth Lynx` = "Perth",
																			 `Dandenong Rangers` = "Dandenong",
																			 `SEQ Stars` = "South-East Queensland")

# Re-calculate percentage variables to ensure accuracy
WNBLBoxScores201415$fieldGoalsPct <- round(
	WNBLBoxScores201415$fieldGoalsMade / WNBLBoxScores201415$fieldGoalsAtt * 100, 1)
WNBLBoxScores201415$threesPct <- round(
	WNBLBoxScores201415$threesMade / WNBLBoxScores201415$threesAtt * 100, 1)
WNBLBoxScores201415$freeThrowsPct <- round(
	WNBLBoxScores201415$freeThrowsMade / WNBLBoxScores201415$freeThrowsAtt * 100, 1)

# Re-organise variables
WNBLBoxScores201415 <- WNBLBoxScores201415 %>%
	select(season, round, date, time, gameIDs201415, teamName, homeAway, oppName, pointsTotal, oppPointsTotal,
				 pointsMargin, teamResult, fieldGoalsMade, fieldGoalsAtt, fieldGoalsPct, threesMade, threesAtt,
				 threesPct, freeThrowsMade, freeThrowsAtt, freeThrowsPct, reboundsDef, reboundsOff, reboundsTotal,
				 assists, turnovers, steals, blocks, blocksReceived, foulsPersonal, foulsOn, pointsFromTurnovers,
				 pointsSecondChance, pointsFastBreak, pointsBench, pointsInThePaint)

##### Add Four Factors metrics #####################################################################

# References
# Oliver, D. (2004) - http://www.rawbw.com/~deano/articles/20040601_roboscout.htm
# Kubatko et al. (2007) - https://www.degruyter.com/view/j/jqas.2007.3.3/jqas.2007.3.3.1070/jqas.2007.3.3.1070.xml

# Estimated number of possessions
WNBLBoxScores201415$possess <- round(
	WNBLBoxScores201415$fieldGoalsAtt + (0.44*WNBLBoxScores201415$freeThrowsAtt) - WNBLBoxScores201415$reboundsOff + WNBLBoxScores201415$turnovers, 0)

# Offensive rating
WNBLBoxScores201415$offRtg <- round((WNBLBoxScores201415$pointsTotal*100) / WNBLBoxScores201415$possess, 1)

# Defensive rating
WNBLBoxScores201415$defRtg <- round((WNBLBoxScores201415$oppPointsTotal*100) / WNBLBoxScores201415$possess, 1)

# Points margin rating
WNBLBoxScores201415$pointsMarginRtg <- WNBLBoxScores201415$offRtg - WNBLBoxScores201415$defRtg

# Offensive-to-defensive-rating ratio
WNBLBoxScores201415$offDefRatio <- round(WNBLBoxScores201415$offRtg / WNBLBoxScores201415$defRtg, 2)

# True shooting rating
WNBLBoxScores201415$trueShootingRtg <- round(
	(100 * (0.5*WNBLBoxScores201415$pointsTotal)) / (WNBLBoxScores201415$fieldGoalsAtt + (0.475*WNBLBoxScores201415$freeThrowsAtt)), 1)

# Effective field goal percentage
WNBLBoxScores201415$eFGpct <- round(
	100 * ((WNBLBoxScores201415$fieldGoalsMade + (0.5*WNBLBoxScores201415$threesMade)) / WNBLBoxScores201415$fieldGoalsAtt), 1)

# Turnover rate
WNBLBoxScores201415$turnoverRate <- round(
	100 * (WNBLBoxScores201415$turnovers / WNBLBoxScores201415$possess), 1)

# Free throw rate
WNBLBoxScores201415$freeThrowRate <- round(
	100 * WNBLBoxScores201415$freeThrowsAtt / WNBLBoxScores201415$fieldGoalsAtt, 1)

# Create new variable $oppReboundsDef as new variable to simplify calculation of ORR
rebDef_homeTeams <- subset(WNBLBoxScores201415, homeAway == "Home")
rebDef_homeTeams <- rebDef_homeTeams %>%
	select(gameIDs201415, homeAway, reboundsDef)
colnames(rebDef_homeTeams) <- c("gameIDs201415", "homeAway", "oppReboundsDef")
rebDef_homeTeams$homeAway <- "Away"

rebDef_awayTeams <- subset(WNBLBoxScores201415, homeAway == "Away")
rebDef_awayTeams <- rebDef_awayTeams %>%
	select(gameIDs201415, homeAway, reboundsDef)
colnames(rebDef_awayTeams) <- c("gameIDs201415", "homeAway", "oppReboundsDef")
rebDef_awayTeams$homeAway <- "Home"

rebDef_homeAway <- rbind(rebDef_homeTeams, rebDef_awayTeams)
WNBLBoxScores201415 <- inner_join(WNBLBoxScores201415, rebDef_homeAway)

# Offensive rebounding rate
WNBLBoxScores201415$offReboundRate <- round(
	100 * (WNBLBoxScores201415$reboundsOff / (WNBLBoxScores201415$reboundsOff + WNBLBoxScores201415$oppReboundsDef)), 1)

# Manipulate data to obtain opponents' Four Factors
fourFactors_oppTeams <- WNBLBoxScores201415 %>%
	select(gameIDs201415, homeAway, possess, offRtg, defRtg, pointsMarginRtg, offDefRatio, trueShootingRtg,
				 eFGpct, turnoverRate, freeThrowRate, offReboundRate)
colnames(fourFactors_oppTeams) <- c(
	"gameIDs201415", "homeAway", "oppPossess", "oppOffRtg", "oppDefRtg", "oppPointsMarginRtg", "oppOffDefRatio",
	"oppTrueShootingRtg", "oppeFGpct", "oppTurnoverRate", "oppFreeThrowRate", "oppOffReboundRate")
fourFactors_oppTeams$homeAway <- recode(fourFactors_oppTeams$homeAway,
																				Home = "Away", Away = "Home")
WNBLBoxScores201415 <- inner_join(WNBLBoxScores201415, fourFactors_oppTeams)

# Re-organise variables
WNBLBoxScores201415 <- WNBLBoxScores201415 %>%
	select(-oppReboundsDef)

##### Save to CSV ##################################################################################

write.csv(WNBLBoxScores201415, "WNBLBoxScores201415.csv", row.names = FALSE)