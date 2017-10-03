##### Load libraries ###############################################################################

library(rjson)
library(purrr)
library(dplyr)

##### Compile game and page IDs: 2015 / 2016 #######################################################

gameIDs201516 <- c(
	"20151009-1930_SEQvsBEN", "20151010-1900_PERvsSYD", "20151010-1900_TSVvsBEN", "20151010-1900_CANvsDAN",
	"20151010-1930_MELvsADL", "20151014-1900_SYDvsCAN", "20151016-1930_DANvsSEQ", "20151017-1930_ADLvsSYD",
	"20151017-1930_MELvsPER", "20151018-1500_BENvsPER", "20151018-1300_TSVvsCAN", "20151022-1900_CANvsSEQ",
	"20151023-1930_DANvsBEN", "20151024-1900_BENvsCAN", "20151024-1900_SYDvsMEL", "20151024-1900_TSVvsADL",
	"20151025-1300_PERvsDAN", "20151025-1600_SEQvsADL", "20151030-1900_PERvsBEN", "20151030-1930_SEQvsSYD",
	"20151031-1830_DANvsTSV", "20151031-1900_CANvsMEL", "20151101-1500_ADLvsBEN", "20151102_1930_MELvsTSV",
	"20151106-1900_SYDvsCAN", "20151107-1830_DANvsMEL", "20151107-1900_BENvsSEQ", "20151108-1300_TSVvsSYD",
	"20151108-1500_ADLvsPER", "20151108-1500_MELvsSEQ", "20151113-1900_PERvsDAN", "20151113_1900_SYDvsADL",
	"20151113-1930_SEQvsMEL", "20151114-1900_CANvsADL", "20151115-1400_BENvsPER", "20151115-1300_TSVvsMEL",
	"20151120-1900_PERvsADL", "20151120-1930_SEQvsTSV", "20151121-1700_SYDvsSEQ", "20151121-1930_MELvsBEN",
	"20151122-1400_BENvsDAN", "20151122-1300_CANvsTSV", "20151128-1830_DANvsSYD", "20151128-1900_TSVvsPER",
	"20151128-1900_CANvsMEL", "20151129-1500_ADLvsBEN", "20151129-1600_SEQvsPER", "20151205-1900_CANvsSYD",
	"20151204-1900_SYDvsADL", "20151204-1900_TSVvsBEN", "20151204-1930_DANvsPER", "20151205-1930_MELvsPER",
	"20151206-1500_ADLvsDAN", "20151206-1600_SEQvsBEN", "20151211-1900_PERvsSEQ", "20151212-1830_DANvsCAN",
	"20151212-1900_BENvsMEL", "20151213-1230_SYDvsTSV", "20151213-1500_ADLvsSEQ", "20151213-1500_MELvsCAN",
	"20151218-1900_PERvsCAN", "20151218-1930_BENvsSYD", "20151218-1930_DANvsMEL", "20151218-1930_SEQvsTSV",
	"20151219-1930_ADLvsCAN", "20151219-1930_MELvsSYD", "20151220-1500_TSVvsDAN", "20151231-1900_PERvsTSV",
	"20160102-1500_SYDvsMEL", "20160102-1900_BENvsCAN", "20160102-1900_SEQvsDAN", "20160102-1930_ADLvsTSV",
	"20160107-1900_SYDvsPER", "20160108-1930_ADLvsDAN", "20160109-1830_DANvsSYD", "20160109-1900_BENvsMEL",
	"20160109-1900_TSVvsSEQ", "20160122-1930_BENvsSYD", "20160122-1930_DANvsADL", "20160123-1900_SEQvsCAN",
	"20160124-1400_PERvsADL", "20160124-1500_TSVvsCAN", "20160125-1930_MELvsDAN", "20160129-1900_TSVvsSYD",
	"20160129-1930_ADLvsMEL", "20160130-1830_DANvsBEN", "20160130-1900_PERvsMEL", "20160131-1500_CANvsADL",
	"20160131-1600_SEQvsSYD", "20160205-1930_ADLvsSEQ", "20160206-1900_PERvsSEQ", "20160206-1900_SYDvsDAN",
	"20160206-1930_MELvsTSV", "20160207-1500_BENvsTSV", "20160207-1500_CANvsDAN", "20160212-1900_TSVvsDAN",
	"20160212-1900_CANvsPER", "20160213-1900_SYDvsPER", "20160213-1930_MELvsADL", "20160214-1400_BENvsADL",
	"20160214-1600_SEQvsCAN", "20160219-1600_PERvsTSV", "20160219-1900_SYDvsBEN", "20160221-1700_DANvsSEQ",
	"20160220-1900_CANvsBEN", "20160220-1930_ADLvsTSV", "20160220-1930_MELvsSEQ"
)

pageIDs201516 <- c(
	seq(137220, 137296, by = 1),
	seq(137298, 137327, by = 1))

# Create two separate vectors for pageIDs due to mid-season change in format of json file
pageIDs201516Part1 <- seq(137220, 137255, by = 1)
pageIDs201516Part2 <- c(
	seq(137256, 137296, by = 1),
	seq(137298, 137327, by = 1))

URLPrefix <- "http://www.fibalivestats.com/data/"

URLSuffix <- "/data.json"

IDs201516 <- data.frame(cbind(gameIDs201516, pageIDs201516))

# Notes - box score data missing for:
# gameID == 20160109-1900_CANvsPER, pageID == 137297
# Could obtain box score stats here?: http://websites.sportstg.com/round_info.cgi?a=MATCH&fixture=528191813&c=1-4478-0-375689-0&pool=1

##### Data scraping: 2015 / 2016 ###################################################################

map_df(pageIDs201516Part1, function(i) {
	
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
		select(X1, X2, X28, X29, X30, X31, X32, X33, X37, X38, X39, X40, X41, X42, X43, X44, X45, X46, X47, X48,
					 X49, X50, X51, X52, X53, X54, X55)
	
	# Rename variables
	colnames(dfTeam1) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff",  "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam1$season <- "2015 / 2016" # Season
	dfTeam1$pageIDs201516 <- i # Unique game identifier
	dfTeam1$homeAway <- "Home" # Home or away
	dfTeam1$oppName <- unlist(df$tm[[2]][[1]])
	dfTeam1$oppPointsTotal <- as.numeric(as.character(unlist(df$tm[[2]][[11]])))
	dfTeam1$pointsMargin <-	as.numeric(as.character(dfTeam1$pointsTotal)) - dfTeam1$oppPointsTotal
	
	# Away team
	# Unlist data, save into a data frame
	dfTeam2 <- data.frame(matrix(unlist(df$tm[[2]]),
															 ncol = length(matrix(unlist(df$tm[[2]]))),
															 byrow = TRUE))
	
	# Keep only the variables that are needed
	dfTeam2 <- dfTeam2 %>%
		select(X1, X2, X28, X29, X30, X31, X32, X33, X37, X38, X39, X40, X41, X42, X43, X44, X45, X46, X47, X48,
					 X49, X50, X51, X52, X53, X54, X55)
	
	# Rename variables
	colnames(dfTeam2) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff", "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam2$season <- "2015 / 2016" # Season
	dfTeam2$pageIDs201516 <- i # Unique game identifier
	dfTeam2$homeAway <- "Away" # Home or away
	dfTeam2$oppName <- unlist(df$tm[[1]][[1]])
	dfTeam2$oppPointsTotal <- as.numeric(as.character(unlist(df$tm[[1]][[11]])))
	dfTeam2$pointsMargin <- as.numeric(as.character(dfTeam2$pointsTotal)) - dfTeam2$oppPointsTotal
	
	# Merge team 1 and team 2 stats into one data frame
	dfBothTeams <- rbind(dfTeam1, dfTeam2, stringsAsFactors = FALSE)
	
	# Re-organise columns
	dfBothTeams <- dfBothTeams %>%
		select(season, pageIDs201516, teamName, teamShortName, homeAway, oppName, pointsTotal, oppPointsTotal,
					 pointsMargin, fieldGoalsMade, fieldGoalsAtt, fieldGoalsPct, threesMade, threesAtt, threesPct,
					 freeThrowsMade, freeThrowsAtt, freeThrowsPct, reboundsDef, reboundsOff, reboundsTotal, assists,
					 turnovers, steals, blocks, blocksReceived, foulsPersonal, foulsOn, pointsFromTurnovers,
					 pointsSecondChance, pointsFastBreak, pointsBench, pointsInThePaint)
	
}) -> WNBLBoxScores201516RawPart1

map_df(pageIDs201516Part2, function(i) {
	
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
		select(X1, X3, X55, X56, X57, X58, X59, X60, X64, X65, X66, X67, X68, X69, X70, X71, X72, X73, X74, X75,
					 X76, X77, X78, X79, X80, X81, X82)
	
	# Rename variables
	colnames(dfTeam1) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff",  "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam1$season <- "2015 / 2016" # Season
	dfTeam1$pageIDs201516 <- i # Unique game identifier
	dfTeam1$homeAway <- "Home" # Home or away
	dfTeam1$oppName <- unlist(df$tm[[2]][[1]])
	dfTeam1$oppPointsTotal <- as.numeric(as.character(unlist(df$tm[[2]][[18]])))
	dfTeam1$pointsMargin <-	as.numeric(as.character(dfTeam1$pointsTotal)) - dfTeam1$oppPointsTotal
	
	# Away team
	# Unlist data, save into a data frame
	dfTeam2 <- data.frame(matrix(unlist(df$tm[[2]]),
															 ncol = length(matrix(unlist(df$tm[[2]]))),
															 byrow = TRUE))
	
	# Keep only the variables that are needed
	dfTeam2 <- dfTeam2 %>%
		select(X1, X3, X55, X56, X57, X58, X59, X60, X64, X65, X66, X67, X68, X69, X70, X71, X72, X73, X74, X75,
					 X76, X77, X78, X79, X80, X81, X82)
	
	# Rename variables
	colnames(dfTeam2) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff", "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam2$season <- "2015 / 2016" # Season
	dfTeam2$pageIDs201516 <- i # Unique game identifier
	dfTeam2$homeAway <- "Away" # Home or away
	dfTeam2$oppName <- unlist(df$tm[[1]][[1]])
	dfTeam2$oppPointsTotal <- as.numeric(as.character(unlist(df$tm[[1]][[18]])))
	dfTeam2$pointsMargin <- as.numeric(as.character(dfTeam2$pointsTotal)) - dfTeam2$oppPointsTotal
	
	# Merge team 1 and team 2 stats into one data frame
	dfBothTeams <- rbind(dfTeam1, dfTeam2, stringsAsFactors = FALSE)
	
	# Re-organise columns
	dfBothTeams <- dfBothTeams %>%
		select(season, pageIDs201516, teamName, teamShortName, homeAway, oppName, pointsTotal, oppPointsTotal,
					 pointsMargin, fieldGoalsMade, fieldGoalsAtt, fieldGoalsPct, threesMade, threesAtt, threesPct,
					 freeThrowsMade, freeThrowsAtt, freeThrowsPct, reboundsDef, reboundsOff, reboundsTotal, assists,
					 turnovers, steals, blocks, blocksReceived, foulsPersonal, foulsOn, pointsFromTurnovers,
					 pointsSecondChance, pointsFastBreak, pointsBench, pointsInThePaint)
	
}) -> WNBLBoxScores201516RawPart2

# Merge *Part1 and *Part2 data frames
WNBLBoxScores201516Raw <- rbind(WNBLBoxScores201516RawPart1, WNBLBoxScores201516RawPart2)

##### Save to CSV ##################################################################################

write.csv(IDs201516, "IDs201516.csv", row.names = FALSE)
write.csv(WNBLBoxScores201516Raw, "WNBLBoxScores201516Raw.csv", row.names = FALSE)