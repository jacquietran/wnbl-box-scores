##### Load libraries ###############################################################################

library(readr)
library(dplyr)
library(stringr)

##### Read data into R #############################################################################

IDs201617 <- read_csv("IDs201617.csv",
											col_names = TRUE, col_types = NULL)

WNBLBoxScores201617Raw <- read_csv("WNBLBoxScores201617Raw.csv",
																	 col_names = TRUE, col_types = NULL)

##### Data cleaning ################################################################################

WNBLBoxScores201617 <- WNBLBoxScores201617Raw

# Add gameIDs into the WNBLBoxScores201617 data frame
WNBLBoxScores201617 <- inner_join(WNBLBoxScores201617, IDs201617)

# Use gameIDs to identify year / month / day of game
WNBLBoxScores201617$date <- paste(str_sub(WNBLBoxScores201617$gameIDs201617, 1, 4),
																	str_sub(WNBLBoxScores201617$gameIDs201617, 5, 6),
																	str_sub(WNBLBoxScores201617$gameIDs201617, 7, 8),
																	sep = "-")
WNBLBoxScores201617$date <- as.Date(WNBLBoxScores201617$date, "%Y-%m-%d")

# Use gameIDs to identify time of game
WNBLBoxScores201617$time <- str_sub(WNBLBoxScores201617$gameIDs201617, 10, 13)
WNBLBoxScores201617$time <- format(strptime(WNBLBoxScores201617$time, "%H%M"), format = "%H%M")

# Specify which round each game was placed in
WNBLBoxScores201617 <- WNBLBoxScores201617[order(WNBLBoxScores201617$gameIDs201617), ]
WNBLBoxScores201617$round <- as.character(WNBLBoxScores201617$date)
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2016-10-07" & WNBLBoxScores201617$round <= "2016-10-09"] <- 1
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2016-10-14" & WNBLBoxScores201617$round <= "2016-10-16"] <- 2
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2016-10-21" & WNBLBoxScores201617$round <= "2016-10-23"] <- 3
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2016-10-28" & WNBLBoxScores201617$round <= "2016-10-31"] <- 4
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2016-11-05" & WNBLBoxScores201617$round <= "2016-11-06"] <- 5
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2016-11-11" & WNBLBoxScores201617$round <= "2016-11-13"] <- 6
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2016-11-18" & WNBLBoxScores201617$round <= "2016-11-20"] <- 7
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2016-11-25" & WNBLBoxScores201617$round <= "2016-11-27"] <- 8
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2016-12-02" & WNBLBoxScores201617$round <= "2016-12-04"] <- 9
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2016-12-08" & WNBLBoxScores201617$round <= "2016-12-11"] <- 10
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2016-12-16" & WNBLBoxScores201617$round <= "2016-12-18"] <- 11
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2016-12-29" & WNBLBoxScores201617$round <= "2016-12-31"] <- 12
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2017-01-06" & WNBLBoxScores201617$round <= "2017-01-08"] <- 13
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2017-01-12" & WNBLBoxScores201617$round <= "2017-01-14"] <- 14
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2017-01-20" & WNBLBoxScores201617$round <= "2017-01-22"] <- 15
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2017-01-25" & WNBLBoxScores201617$round <= "2017-01-29"] <- 16
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2017-02-03" & WNBLBoxScores201617$round <= "2017-02-05"] <- 17
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2017-02-10" & WNBLBoxScores201617$round <= "2017-02-12"] <- 18
WNBLBoxScores201617$round[
	WNBLBoxScores201617$round >= "2017-02-16" & WNBLBoxScores201617$round <= "2017-02-19"] <- 19

# Convert points margins to categorical team results
WNBLBoxScores201617$teamResult <- ifelse(WNBLBoxScores201617$pointsMargin > 0, "Win",
																				 ifelse(WNBLBoxScores201617$pointsMargin < 0, "Loss",
																				 			 "Draw"))

# Recode team names for consistency
WNBLBoxScores201617$teamName <- recode(WNBLBoxScores201617$teamName,
																			 `UC Capitals` = "Canberra",
																			 `Sydney Uni Flames` ="Sydney",
																			 `Melbourne Boomers` = "Melbourne",
																			 `Adelaide Lightning` = "Adelaide",
																			 `Townsville Fire` = "Townsville",
																			 `Bendigo Spirit` = "Bendigo",
																			 `Perth Lynx` = "Perth",
																			 `Dandenong Rangers` = "Dandenong")
WNBLBoxScores201617$oppName <- recode(WNBLBoxScores201617$oppName,
																			`UC Capitals` = "Canberra",
																			`Sydney Uni Flames` ="Sydney",
																			`Melbourne Boomers` = "Melbourne",
																			`Adelaide Lightning` = "Adelaide",
																			`Townsville Fire` = "Townsville",
																			`Bendigo Spirit` = "Bendigo",
																			`Perth Lynx` = "Perth",
																			`Dandenong Rangers` = "Dandenong")

# Re-calculate percentage variables to ensure accuracy
WNBLBoxScores201617$fieldGoalsPct <- round(
	WNBLBoxScores201617$fieldGoalsMade / WNBLBoxScores201617$fieldGoalsAtt * 100, 1)
WNBLBoxScores201617$threesPct <- round(
	WNBLBoxScores201617$threesMade / WNBLBoxScores201617$threesAtt * 100, 1)
WNBLBoxScores201617$freeThrowsPct <- round(
	WNBLBoxScores201617$freeThrowsMade / WNBLBoxScores201617$freeThrowsAtt * 100, 1)

# Re-organise variables
WNBLBoxScores201617 <- WNBLBoxScores201617 %>%
	select(season, round, date, time, gameIDs201617, teamName, homeAway, oppName, pointsTotal, oppPointsTotal,
				 pointsMargin, teamResult, fieldGoalsMade, fieldGoalsAtt, fieldGoalsPct, threesMade, threesAtt,
				 threesPct, freeThrowsMade, freeThrowsAtt, freeThrowsPct, reboundsDef, reboundsOff, reboundsTotal,
				 assists, turnovers, steals, blocks, blocksReceived, foulsPersonal, foulsOn, pointsFromTurnovers,
				 pointsSecondChance, pointsFastBreak, pointsBench, pointsInThePaint)

##### Add Four Factors metrics #####################################################################

# References
# Oliver, D. (2004) - http://www.rawbw.com/~deano/articles/20040601_roboscout.htm
# Kubatko et al. (2007) - https://www.degruyter.com/view/j/jqas.2007.3.3/jqas.2007.3.3.1070/jqas.2007.3.3.1070.xml

# Estimated number of possessions
WNBLBoxScores201617$possess <- round(
	WNBLBoxScores201617$fieldGoalsAtt + (0.44*WNBLBoxScores201617$freeThrowsAtt) - WNBLBoxScores201617$reboundsOff + WNBLBoxScores201617$turnovers, 0)

# Offensive rating
WNBLBoxScores201617$offRtg <- round((WNBLBoxScores201617$pointsTotal*100) / WNBLBoxScores201617$possess, 1)

# Defensive rating
WNBLBoxScores201617$defRtg <- round((WNBLBoxScores201617$oppPointsTotal*100) / WNBLBoxScores201617$possess, 1)

# Points margin rating
WNBLBoxScores201617$pointsMarginRtg <- WNBLBoxScores201617$offRtg - WNBLBoxScores201617$defRtg

# Offensive-to-defensive-rating ratio
WNBLBoxScores201617$offDefRatio <- round(WNBLBoxScores201617$offRtg / WNBLBoxScores201617$defRtg, 2)

# True shooting rating
WNBLBoxScores201617$trueShootingRtg <- round(
	(100 * (0.5*WNBLBoxScores201617$pointsTotal)) / (WNBLBoxScores201617$fieldGoalsAtt + (0.475*WNBLBoxScores201617$freeThrowsAtt)), 1)

# Effective field goal percentage
WNBLBoxScores201617$eFGpct <- round(
	100 * ((WNBLBoxScores201617$fieldGoalsMade + (0.5*WNBLBoxScores201617$threesMade)) / WNBLBoxScores201617$fieldGoalsAtt), 1)

# Turnover rate
WNBLBoxScores201617$turnoverRate <- round(
	100 * (WNBLBoxScores201617$turnovers / WNBLBoxScores201617$possess), 1)

# Free throw rate
WNBLBoxScores201617$freeThrowRate <- round(
	100 * WNBLBoxScores201617$freeThrowsAtt / WNBLBoxScores201617$fieldGoalsAtt, 1)

# Create new variable $oppReboundsDef as new variable to simplify calculation of ORR
rebDef_homeTeams <- subset(WNBLBoxScores201617, homeAway == "Home")
rebDef_homeTeams <- rebDef_homeTeams %>%
	select(gameIDs201617, homeAway, reboundsDef)
colnames(rebDef_homeTeams) <- c("gameIDs201617", "homeAway", "oppReboundsDef")
rebDef_homeTeams$homeAway <- "Away"

rebDef_awayTeams <- subset(WNBLBoxScores201617, homeAway == "Away")
rebDef_awayTeams <- rebDef_awayTeams %>%
	select(gameIDs201617, homeAway, reboundsDef)
colnames(rebDef_awayTeams) <- c("gameIDs201617", "homeAway", "oppReboundsDef")
rebDef_awayTeams$homeAway <- "Home"

rebDef_homeAway <- rbind(rebDef_homeTeams, rebDef_awayTeams)
WNBLBoxScores201617 <- inner_join(WNBLBoxScores201617, rebDef_homeAway)

# Offensive rebounding rate
WNBLBoxScores201617$offReboundRate <- round(
	100 * (WNBLBoxScores201617$reboundsOff / (WNBLBoxScores201617$reboundsOff + WNBLBoxScores201617$oppReboundsDef)), 1)

# Manipulate data to obtain opponents' Four Factors
fourFactors_oppTeams <- WNBLBoxScores201617 %>%
	select(gameIDs201617, homeAway, possess, offRtg, defRtg, pointsMarginRtg, offDefRatio, trueShootingRtg,
				 eFGpct, turnoverRate, freeThrowRate, offReboundRate)
colnames(fourFactors_oppTeams) <- c(
	"gameIDs201617", "homeAway", "oppPossess", "oppOffRtg", "oppDefRtg", "oppPointsMarginRtg", "oppOffDefRatio",
	"oppTrueShootingRtg", "oppeFGpct", "oppTurnoverRate", "oppFreeThrowRate", "oppOffReboundRate")
fourFactors_oppTeams$homeAway <- recode(fourFactors_oppTeams$homeAway,
																				Home = "Away", Away = "Home")
WNBLBoxScores201617 <- inner_join(WNBLBoxScores201617, fourFactors_oppTeams)

# Re-organise variables
WNBLBoxScores201617 <- WNBLBoxScores201617 %>%
	select(-oppReboundsDef)

##### Save to CSV ##################################################################################

write.csv(WNBLBoxScores201617, "WNBLBoxScores201617.csv", row.names = FALSE)