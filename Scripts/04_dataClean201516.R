##### Load libraries ###############################################################################

library(readr)
library(dplyr)
library(stringr)

##### Read data into R #############################################################################

IDs201516 <- read_csv("IDs201516.csv",
											col_names = TRUE, col_types = NULL)

WNBLBoxScores201516Raw <- read_csv("WNBLBoxScores201516Raw.csv",
																	 col_names = TRUE, col_types = NULL)

##### Data cleaning ################################################################################

WNBLBoxScores201516 <- WNBLBoxScores201516Raw

# Add gameIDs into the WNBLBoxScores201516 data frame
WNBLBoxScores201516 <- inner_join(WNBLBoxScores201516, IDs201516)

# Use gameIDs to identify year / month / day of game
WNBLBoxScores201516$date <- paste(str_sub(WNBLBoxScores201516$gameIDs201516, 1, 4),
																	str_sub(WNBLBoxScores201516$gameIDs201516, 5, 6),
																	str_sub(WNBLBoxScores201516$gameIDs201516, 7, 8),
																	sep = "-")
WNBLBoxScores201516$date <- as.Date(WNBLBoxScores201516$date, "%Y-%m-%d")

# Use gameIDs to identify time of game
WNBLBoxScores201516$time <- str_sub(WNBLBoxScores201516$gameIDs201516, 10, 13)
WNBLBoxScores201516$time <- format(strptime(WNBLBoxScores201516$time, "%H%M"), format = "%H%M")

# Specify which round each game was placed in
WNBLBoxScores201516 <- WNBLBoxScores201516[order(WNBLBoxScores201516$gameIDs201516), ]
WNBLBoxScores201516$round <- as.character(WNBLBoxScores201516$date)
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2015-10-09" & WNBLBoxScores201516$round <= "2015-10-10"] <- 1
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2015-10-14" & WNBLBoxScores201516$round <= "2015-10-18"] <- 2
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2015-10-22" & WNBLBoxScores201516$round <= "2015-10-25"] <- 3
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2015-10-30" & WNBLBoxScores201516$round <= "2015-11-02"] <- 4
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2015-11-06" & WNBLBoxScores201516$round <= "2015-11-08"] <- 5
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2015-11-13" & WNBLBoxScores201516$round <= "2015-11-15"] <- 6
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2015-11-20" & WNBLBoxScores201516$round <= "2015-11-22"] <- 7
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2015-11-28" & WNBLBoxScores201516$round <= "2015-11-29"] <- 8
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2015-12-04" & WNBLBoxScores201516$round <= "2015-12-06"] <- 9
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2015-12-11" & WNBLBoxScores201516$round <= "2015-12-13"] <- 10
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2015-12-18" & WNBLBoxScores201516$round <= "2015-12-20"] <- 11
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2015-12-31" & WNBLBoxScores201516$round <= "2016-01-02"] <- 12
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2016-01-07" & WNBLBoxScores201516$round <= "2016-01-09"] <- 13
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2016-01-22" & WNBLBoxScores201516$round <= "2016-01-25"] <- 14
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2016-01-29" & WNBLBoxScores201516$round <= "2016-01-31"] <- 15
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2016-02-05" & WNBLBoxScores201516$round <= "2016-02-07"] <- 16
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2016-02-12" & WNBLBoxScores201516$round <= "2016-02-14"] <- 17
WNBLBoxScores201516$round[
	WNBLBoxScores201516$round >= "2016-02-19" & WNBLBoxScores201516$round <= "2016-02-21"] <- 18

# Convert points margins to categorical team results
WNBLBoxScores201516$teamResult <- ifelse(WNBLBoxScores201516$pointsMargin > 0, "Win",
																				 ifelse(WNBLBoxScores201516$pointsMargin < 0, "Loss",
																				 			 "Draw"))

# Recode team names for consistency
WNBLBoxScores201516$teamName <- recode(WNBLBoxScores201516$teamName,
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
WNBLBoxScores201516$oppName <- recode(WNBLBoxScores201516$oppName,
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
WNBLBoxScores201516$fieldGoalsPct <- round(
	WNBLBoxScores201516$fieldGoalsMade / WNBLBoxScores201516$fieldGoalsAtt * 100, 1)
WNBLBoxScores201516$threesPct <- round(
	WNBLBoxScores201516$threesMade / WNBLBoxScores201516$threesAtt * 100, 1)
WNBLBoxScores201516$freeThrowsPct <- round(
	WNBLBoxScores201516$freeThrowsMade / WNBLBoxScores201516$freeThrowsAtt * 100, 1)

# Re-organise variables
WNBLBoxScores201516 <- WNBLBoxScores201516 %>%
	select(season, round, date, time, gameIDs201516, teamName, homeAway, oppName, pointsTotal, oppPointsTotal,
				 pointsMargin, teamResult, fieldGoalsMade, fieldGoalsAtt, fieldGoalsPct, threesMade, threesAtt,
				 threesPct, freeThrowsMade, freeThrowsAtt, freeThrowsPct, reboundsDef, reboundsOff, reboundsTotal,
				 assists, turnovers, steals, blocks, blocksReceived, foulsPersonal, foulsOn, pointsFromTurnovers,
				 pointsSecondChance, pointsFastBreak, pointsBench, pointsInThePaint)

##### Add Four Factors metrics #####################################################################

# References
# Oliver, D. (2004) - http://www.rawbw.com/~deano/articles/20040601_roboscout.htm
# Kubatko et al. (2007) - https://www.degruyter.com/view/j/jqas.2007.3.3/jqas.2007.3.3.1070/jqas.2007.3.3.1070.xml

# Estimated number of possessions
WNBLBoxScores201516$possess <- round(
	WNBLBoxScores201516$fieldGoalsAtt + (0.44*WNBLBoxScores201516$freeThrowsAtt) - WNBLBoxScores201516$reboundsOff + WNBLBoxScores201516$turnovers, 0)

# Offensive rating
WNBLBoxScores201516$offRtg <- round((WNBLBoxScores201516$pointsTotal*100) / WNBLBoxScores201516$possess, 1)

# Defensive rating
WNBLBoxScores201516$defRtg <- round((WNBLBoxScores201516$oppPointsTotal*100) / WNBLBoxScores201516$possess, 1)

# Points margin rating
WNBLBoxScores201516$pointsMarginRtg <- WNBLBoxScores201516$offRtg - WNBLBoxScores201516$defRtg

# Offensive-to-defensive-rating ratio
WNBLBoxScores201516$offDefRatio <- round(WNBLBoxScores201516$offRtg / WNBLBoxScores201516$defRtg, 2)

# True shooting rating
WNBLBoxScores201516$trueShootingRtg <- round(
	(100 * (0.5*WNBLBoxScores201516$pointsTotal)) / (WNBLBoxScores201516$fieldGoalsAtt + (0.475*WNBLBoxScores201516$freeThrowsAtt)), 1)

# Effective field goal percentage
WNBLBoxScores201516$eFGpct <- round(
	100 * ((WNBLBoxScores201516$fieldGoalsMade + (0.5*WNBLBoxScores201516$threesMade)) / WNBLBoxScores201516$fieldGoalsAtt), 1)

# Turnover rate
WNBLBoxScores201516$turnoverRate <- round(
	100 * (WNBLBoxScores201516$turnovers / WNBLBoxScores201516$possess), 1)

# Free throw rate
WNBLBoxScores201516$freeThrowRate <- round(
	100 * WNBLBoxScores201516$freeThrowsAtt / WNBLBoxScores201516$fieldGoalsAtt, 1)

# Create new variable $oppReboundsDef as new variable to simplify calculation of ORR
rebDef_homeTeams <- subset(WNBLBoxScores201516, homeAway == "Home")
rebDef_homeTeams <- rebDef_homeTeams %>%
	select(gameIDs201516, homeAway, reboundsDef)
colnames(rebDef_homeTeams) <- c("gameIDs201516", "homeAway", "oppReboundsDef")
rebDef_homeTeams$homeAway <- "Away"

rebDef_awayTeams <- subset(WNBLBoxScores201516, homeAway == "Away")
rebDef_awayTeams <- rebDef_awayTeams %>%
	select(gameIDs201516, homeAway, reboundsDef)
colnames(rebDef_awayTeams) <- c("gameIDs201516", "homeAway", "oppReboundsDef")
rebDef_awayTeams$homeAway <- "Home"

rebDef_homeAway <- rbind(rebDef_homeTeams, rebDef_awayTeams)
WNBLBoxScores201516 <- inner_join(WNBLBoxScores201516, rebDef_homeAway)

# Offensive rebounding rate
WNBLBoxScores201516$offReboundRate <- round(
	100 * (WNBLBoxScores201516$reboundsOff / (WNBLBoxScores201516$reboundsOff + WNBLBoxScores201516$oppReboundsDef)), 1)

# Manipulate data to obtain opponents' Four Factors
fourFactors_oppTeams <- WNBLBoxScores201516 %>%
	select(gameIDs201516, homeAway, possess, offRtg, defRtg, pointsMarginRtg, offDefRatio, trueShootingRtg,
				 eFGpct, turnoverRate, freeThrowRate, offReboundRate)
colnames(fourFactors_oppTeams) <- c(
	"gameIDs201516", "homeAway", "oppPossess", "oppOffRtg", "oppDefRtg", "oppPointsMarginRtg", "oppOffDefRatio",
	"oppTrueShootingRtg", "oppeFGpct", "oppTurnoverRate", "oppFreeThrowRate", "oppOffReboundRate")
fourFactors_oppTeams$homeAway <- recode(fourFactors_oppTeams$homeAway,
																				Home = "Away", Away = "Home")
WNBLBoxScores201516 <- inner_join(WNBLBoxScores201516, fourFactors_oppTeams)

# Re-organise variables
WNBLBoxScores201516 <- WNBLBoxScores201516 %>%
	select(-oppReboundsDef)

##### Save to CSV ##################################################################################

write.csv(WNBLBoxScores201516, "WNBLBoxScores201516.csv", row.names = FALSE)
