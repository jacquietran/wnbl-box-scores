##### Load libraries ###############################################################################

library(rjson)
library(purrr)
library(dplyr)

##### Compile game and page IDs: 2014 / 2015 #######################################################

gameIDs201415 <- c(
	"20141017-1930_MELvsCAN", "20141018-1500_DANvsMEL", "20141018-1900_CANvsSYD", "20141018-1830_WCWvsADL",
	"20141018-1900_TSVvsBEN", "20141023-1700_ADLvsDAN", "20141025-1500_BENvsCAN", "20141025-1830_DANvsSYD",
	"20141026-1300_MELvsBEN", "20141030-1930_BENvsDAN", "20141031-1900_TSVvsADL", "20141101-1500_CANvsDAN",
	"20141101-1830_WCWvsMEL", "20141105-1900_SYDvsCAN", "20141107-1930_WCWvsBEN", "20141108-1500_DANvsTSV",
	"20141109-1300_CANvsWCW", "20141109-1400_ADLvsBEN", "20141109-1300_MELvsDAN", "20141114-1900_SYDvsMEL",
	"20141114-1200_WCWvsCAN", "20141115-1700_ADLvsSYD", "20141115-1900_TSVvsMEL", "20141116-1400_DANvsBEN",
	"20141121-1700_ADLvsTSV", "20141122-1500_MELvsADL", "20141122-1830_DANvsCAN", "20141122-1930_WCWvsSYD",
	"20141128-1900_SYDvsADL", "20141129-1500_BENvsMEL", "20141129-1900_CANvsADL", "20141129-1900_TSVvsWCW",
	"20141130-1300_MELvsSYD", "20141205-1900_SYDvsBEN", "20141205-1930_DANvsWCW", "20141206-1500_MELvsWCW",
	"20141206-1930_BENvsADL", "20141207-1300_CANvsMEL", "20141207-1300_TSVvsDAN", "20141212-1700_ADLvsMEL",
	"20141214-1500_CANvsTSV", "20141212-1930_WCWvsDAN", "20141213-1300_SYDvsTSV", "20141213-1930_MELvsBEN",
	"20141219-1700_ADLvsCAN", "20141219-1930_BENvsSYD", "20141220-1500_DANvsSYD", "20141220-1830_WCWvsCAN",
	"20141220-1900_TSVvsMEL", "20150102-1900_TSVvsCAN", "20150103-1500_BENvsWCW", "20150103-1500_SYDvsDAN",
	"20150104-1100_ADLvsWCW", "20150104-1300_CANvsDAN", "20150109-1900_CANvsWCW", "20150110-1500_MELvsADL",
	"20150110-1900_SYDvsWCW", "20150110-1900_TSVvsDAN", "20150111-1400_BENvsADL", "20150115-1900_CANvsSYD",
	"20150116-1930_ADLvsTSV", "20150116-1930_DANvsBEN", "20150117-1500_SYDvsMEL", "20150118-1500_WCWvsTSV",
	"20150123-1930_BENvsCAN", "20150123-1930_DANvsADL", "20150124-1500_MELvsCAN", "20150125-1300_TSVvsSYD",
	"20150125-1300_WCWvsBEN", "20150130-1900_CANvsADL", "20150131-1500_DANvsWCW", "20150131-1900_SYDvsBEN",
	"20150131-1930_MELvsTSV", "20150201-1100_ADLvsWCW", "20150201-1800_BENvsTSV", "20150206-1700_ADLvsSYD",
	"20150206-1900_TSVvsCAN", "20150207-1500_MELvsDAN", "20150207-1830_WCWvsSYD", "20150208-1400_BENvsMEL",
	"20150213-1740_CANvsBEN", "20150213-1930_ADLvsDAN", "20150214-1830_WCWvsMEL", "20150214-1900_SYDvsTSV",
	"20150215-1700_DANvsTSV"
)

pageIDs201415 <- c(
	seq(64511, 64519, by = 1), 
	seq(64521, 64524, by = 1),
	seq(64526, 64540, by = 1),
	seq(64542, 64598, by = 1))

URLPrefix <- "http://www.fibalivestats.com/data/"

URLSuffix <- "/data.json"

IDs201415 <- data.frame(cbind(gameIDs201415, pageIDs201415))

# Notes - box score data missing for:
# gameID == 20141026-1300_TSVvsWCW, pageID == 64520
# gameID == 20141101-1900_SYDvsADL, pageID == 64525
# gameID == 20141122-1930_BENvsTSV, pageID == 64541

##### Data scraping: 2014 / 2015 ###################################################################

map_df(pageIDs201415, function(i) {
	
	# simple but effective progress indicator
	cat(".")
	
	url <- paste0(URLPrefix, i, URLSuffix)
	
	df <- fromJSON(file = url, method="C")
	
		# Home team
	# Unlist data, save into a data frame
	dfTeam1 <- data.frame(matrix(unlist(df$tm[[1]]),
															 ncol = length(matrix(unlist(df$tm[[1]]))),
															 byrow = TRUE))
	
	# Keep only the variables that are needed
	dfTeam1 <- dfTeam1 %>%
		select(X1, X2, X23, X24, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X40,
					 X41, X42, X43, X44, X45, X46, X47)
	
	# Rename variables
	colnames(dfTeam1) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff",  "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam1$season <- "2014 / 2015" # Season
	dfTeam1$pageIDs201415 <- i # Unique game identifier
	dfTeam1$homeAway <- "Home" # Home or away
	dfTeam1$oppName <- unlist(df$tm[[2]][[1]])
	dfTeam1$oppPointsTotal <- as.numeric(as.character(unlist(df$tm[[2]][[8]])))
	dfTeam1$pointsMargin <-	as.numeric(as.character(dfTeam1$pointsTotal)) - dfTeam1$oppPointsTotal
	
	# Away team
	# Unlist data, save into a data frame
	dfTeam2 <- data.frame(matrix(unlist(df$tm[[2]]),
															 ncol = length(matrix(unlist(df$tm[[2]]))),
															 byrow = TRUE))
	
	# Keep only the variables that are needed
	dfTeam2 <- dfTeam2 %>%
		select(X1, X2, X23, X24, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X40,
					 X41, X42, X43, X44, X45, X46, X47)
	
	# Rename variables
	colnames(dfTeam2) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff", "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam2$season <- "2014 / 2015" # Season
	dfTeam2$pageIDs201415 <- i # Unique game identifier
	dfTeam2$homeAway <- "Away" # Home or away
	dfTeam2$oppName <- unlist(df$tm[[1]][[1]])
	dfTeam2$oppPointsTotal <- as.numeric(as.character(unlist(df$tm[[1]][[8]])))
	dfTeam2$pointsMargin <- as.numeric(as.character(dfTeam2$pointsTotal)) - dfTeam2$oppPointsTotal
	
	# Merge team 1 and team 2 stats into one data frame
	dfBothTeams <- rbind(dfTeam1, dfTeam2, stringsAsFactors = FALSE)
	
	# Re-organise columns
	dfBothTeams <- dfBothTeams %>%
		select(season, pageIDs201415, teamName, teamShortName, homeAway, oppName, pointsTotal, oppPointsTotal,
					 pointsMargin, fieldGoalsMade, fieldGoalsAtt, fieldGoalsPct, threesMade, threesAtt, threesPct,
					 freeThrowsMade, freeThrowsAtt, freeThrowsPct, reboundsDef, reboundsOff, reboundsTotal, assists,
					 turnovers, steals, blocks, blocksReceived, foulsPersonal, foulsOn, pointsFromTurnovers,
					 pointsSecondChance, pointsFastBreak, pointsBench, pointsInThePaint)
	
}) -> WNBLBoxScores201415Raw

##### Save to CSV ##################################################################################

write.csv(IDs201415, "IDs201415.csv", row.names = FALSE)
write.csv(WNBLBoxScores201415Raw, "WNBLBoxScores201415Raw.csv", row.names = FALSE)