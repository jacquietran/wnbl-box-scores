##### Load libraries ###############################################################################

library(rjson)
library(purrr)
library(dplyr)

##### Compile game and page IDs: 2016 / 2017 #######################################################

gameIDs201617 <- c(
	"20161007-1930_SYDvsPER", "20161008-1500_CANvsDAN", "20161008-1930_BENvsADL", "20161009-1300_TSVvsPER",
	"20161009-1500_MELvsADL", "20161014-1900_CANvsADL", "20161015-1900_BENvsDAN", "20161015-1930_MELvsPER",
	"20161016-1300_TSVvsADL", "20161016-1700_DANvsPER", "20161016-1700_SYDvsCAN", "20161021-1700_ADLvsMEL",
	"20161021-1930_BENvsSYD", "20161021-1930_DANvsTSV", "20161022-1900_PERvsCAN", "20161022-1900_SYDvsTSV",
	"20161023-1400_MELvsBEN", "20161028-1900_CANvsMEL", "20161028-1930_DANvsBEN", "20161030-1300_TSVvsBEN",
	"20161030-1300_PERvsADL", "20161031-1930_MELvsDAN", "20161105-1900_TSVvsSYD", "20161105-1930_BENvsCAN",
	"20161106-1400_DANvsCAN", "20161106-1400_PERvsMEL", "20161111-1930_SYDvsMEL", "20161112-1930_ADLvsPER",
	"20161112-1900_DANvsCAN", "20161112-1930_BENvsTSV", "20161113-1500_MELvsTSV", "20161118-1900_PERvsBEN",
	"20161119-1930_ADLvsBEN", "20161119-1900_CANvsTSV", "20161120-1400_DANvsMEL", "20161120-1500_SYDvsTSV",
	"20161125-1900_TSVvsPER", "20161125-1930_SYDvsBEN", "20161126-1830_DANvsADL", "20161127-1400_CANvsPER",
	"20161127-1500_BENvsADL", "20161202-1900_PERvsTSV", "20161202-1900_CANvsMEL", "20161203-1700_ADLvsTSV",
	"20161203-1830_DANvsSYD", "20161204-1430_MELvsSYD", "20161204-1500_BENvsDAN", "20161208-1900_CANvsSYD",
	"20161209-1930_ADLvsDAN", "20161210-1900_SYDvsBEN", "20161210-1900_TSVvsMEL", "20161211-1300_PERvsDAN",
	"20161216-1630_ADLvsTSV", "20161216-1930_BENvsCAN", "20161217-1900_SYDvsDAN", "20161217-1930_MELvsCAN",
	"20161218-1300_PERvsADL", "20161229-1930_ADLvsSYD", "20161230-1900_PERvsSYD", "20161231-1900_TSVvsMEL",
	"20170106-1900_TSVvsCAN", "20170107-1900_PERvsBEN", "20170107-1930_MELvsADL", "20170108-1400_DANvsADL",
	"20170108-1700_SYDvsCAN", "20170112-1630_ADLvsSYD", "20170113-1930_BENvsSYD", "20170114-1500_DANvsMEL",
	"20170114-1500_CANvsTSV", "20170120-1900_TSVvsBEN", "20170121-1630_ADLvsDAN", "20170121-1700_SYDvsPER",
	"20170121-1900_CANvsBEN", "20170122-1500_MELvsPER", "20170125-1930_MELvsDAN", "20170126-1500_CANvsSYD",
	"20170127-1930_ADLvsPER", "20170128-1900_TSVvsSYD", "20170128-1930_BENvsMEL", "20170129-1400_PERvsDAN",
	"20170203-1930_ADLvsCAN", "20170204-1830_DANvsTSV", "20170205-1400_PERvsCAN", "20170205-1500_BENvsTSV",
	"20170205-1500_MELvsSYD", "20170210-1900_TSVvsDAN", "20170210-1930_ADLvsMEL", "20170212-1200_CANvsBEN",
	"20170211-1900_SYDvsDAN", "20170212-1400_PERvsMEL", "20170216-1900_CANvsADL", "20170218-1830_DANvsPER",
	"20170218-1900_SYDvsADL", "20170218-1900_TSVvsCAN", "20170218-1930_MELvsBEN", "20170219-1500_BENvsPER"
)

pageIDs201617 <- seq(311810, 311905, by = 1)

# Create separate vectors for pageIDs due to mid-season change in format of json file
pageIDs201617Part1 <- c(311810,
												311811,
												311813,
												311814,
												seq(311816, 311830, by = 1))
pageIDs201617Part2 <- 311812
pageIDs201617Part3 <- c(311815,
												311831,
												seq(311833, 311846, by = 1),
												seq(311848, 311876, by = 1),
												seq(311878, 311898, by = 1),
												seq(311900, 311905, by = 1))
pageIDs201617Part4 <- c(311832, 311847, 311877, 311899)

URLPrefix <- "http://www.fibalivestats.com/data/"

URLSuffix <- "/data.json"

IDs201617 <- data.frame(cbind(gameIDs201617, pageIDs201617))

##### Data scraping: 2016 / 2017 ###################################################################

# Part 1
map_df(pageIDs201617Part1, function(i) {
	
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
		select(X1, X3, X58, X59, X60, X61, X62, X63, X67, X68, X69, X70, X71, X72, X73, X74, X75, X76,
					 X77, X78, X79, X80, X81, X82, X83, X84, X85)
	
	# Rename variables
	colnames(dfTeam1) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff",  "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam1$season <- "2016 / 2017" # Season
	dfTeam1$pageIDs201617 <- i # Unique game identifier
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
		select(X1, X3, X58, X59, X60, X61, X62, X63, X67, X68, X69, X70, X71, X72, X73, X74, X75, X76,
					 X77, X78, X79, X80, X81, X82, X83, X84, X85)
	
	# Rename variables
	colnames(dfTeam2) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff", "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam2$season <- "2016 / 2017" # Season
	dfTeam2$pageIDs201617 <- i # Unique game identifier
	dfTeam2$homeAway <- "Away" # Home or away
	dfTeam2$oppName <- unlist(df$tm[[1]][[1]])
	dfTeam2$oppPointsTotal <- as.numeric(as.character(unlist(df$tm[[1]][[18]])))
	dfTeam2$pointsMargin <- as.numeric(as.character(dfTeam2$pointsTotal)) - dfTeam2$oppPointsTotal
	
	# Merge team 1 and team 2 stats into one data frame
	dfBothTeams <- rbind(dfTeam1, dfTeam2, stringsAsFactors = FALSE)
	
	# Re-organise columns
	dfBothTeams <- dfBothTeams %>%
		select(season, pageIDs201617, teamName, teamShortName, homeAway, oppName, pointsTotal, oppPointsTotal,
					 pointsMargin, fieldGoalsMade, fieldGoalsAtt, fieldGoalsPct, threesMade, threesAtt, threesPct,
					 freeThrowsMade, freeThrowsAtt, freeThrowsPct, reboundsDef, reboundsOff, reboundsTotal, assists,
					 turnovers, steals, blocks, blocksReceived, foulsPersonal, foulsOn, pointsFromTurnovers,
					 pointsSecondChance, pointsFastBreak, pointsBench, pointsInThePaint)
	
}) -> WNBLBoxScores201617RawPart1

# Part 2
map_df(pageIDs201617Part2, function(i) {
	
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
		select(X1, X3, X59, X60, X61, X62, X63, X64, X68, X69, X70, X71, X72, X73, X74, X75, X76, X77,
					 X78, X79, X80, X81, X82, X83, X84, X85, X86)
	
	# Rename variables
	colnames(dfTeam1) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff",  "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam1$season <- "2016 / 2017" # Season
	dfTeam1$pageIDs201617 <- i # Unique game identifier
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
		select(X1, X3, X59, X60, X61, X62, X63, X64, X68, X69, X70, X71, X72, X73, X74, X75, X76, X77,
					 X78, X79, X80, X81, X82, X83, X84, X85, X86)
	
	# Rename variables
	colnames(dfTeam2) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff", "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam2$season <- "2016 / 2017" # Season
	dfTeam2$pageIDs201617 <- i # Unique game identifier
	dfTeam2$homeAway <- "Away" # Home or away
	dfTeam2$oppName <- unlist(df$tm[[1]][[1]])
	dfTeam2$oppPointsTotal <- as.numeric(as.character(unlist(df$tm[[1]][[18]])))
	dfTeam2$pointsMargin <- as.numeric(as.character(dfTeam2$pointsTotal)) - dfTeam2$oppPointsTotal
	
	# Merge team 1 and team 2 stats into one data frame
	dfBothTeams <- rbind(dfTeam1, dfTeam2, stringsAsFactors = FALSE)
	
	# Re-organise columns
	dfBothTeams <- dfBothTeams %>%
		select(season, pageIDs201617, teamName, teamShortName, homeAway, oppName, pointsTotal, oppPointsTotal,
					 pointsMargin, fieldGoalsMade, fieldGoalsAtt, fieldGoalsPct, threesMade, threesAtt, threesPct,
					 freeThrowsMade, freeThrowsAtt, freeThrowsPct, reboundsDef, reboundsOff, reboundsTotal, assists,
					 turnovers, steals, blocks, blocksReceived, foulsPersonal, foulsOn, pointsFromTurnovers,
					 pointsSecondChance, pointsFastBreak, pointsBench, pointsInThePaint)
	
}) -> WNBLBoxScores201617RawPart2

# Part 3
map_df(pageIDs201617Part3, function(i) {
	
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
		select(X1, X3, X65, X66, X67, X68, X69, X70, X74, X75, X76, X77, X78, X79, X80, X81, X82, X83,
					 X84, X85, X86, X87, X88, X89, X90, X91, X92)
	
	# Rename variables
	colnames(dfTeam1) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff",  "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam1$season <- "2016 / 2017" # Season
	dfTeam1$pageIDs201617 <- i # Unique game identifier
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
		select(X1, X3, X65, X66, X67, X68, X69, X70, X74, X75, X76, X77, X78, X79, X80, X81, X82, X83,
					 X84, X85, X86, X87, X88, X89, X90, X91, X92)
	
	# Rename variables
	colnames(dfTeam2) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff", "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam2$season <- "2016 / 2017" # Season
	dfTeam2$pageIDs201617 <- i # Unique game identifier
	dfTeam2$homeAway <- "Away" # Home or away
	dfTeam2$oppName <- unlist(df$tm[[1]][[1]])
	dfTeam2$oppPointsTotal <- as.numeric(as.character(unlist(df$tm[[1]][[18]])))
	dfTeam2$pointsMargin <- as.numeric(as.character(dfTeam2$pointsTotal)) - dfTeam2$oppPointsTotal
	
	# Merge team 1 and team 2 stats into one data frame
	dfBothTeams <- rbind(dfTeam1, dfTeam2, stringsAsFactors = FALSE)
	
	# Re-organise columns
	dfBothTeams <- dfBothTeams %>%
		select(season, pageIDs201617, teamName, teamShortName, homeAway, oppName, pointsTotal, oppPointsTotal,
					 pointsMargin, fieldGoalsMade, fieldGoalsAtt, fieldGoalsPct, threesMade, threesAtt, threesPct,
					 freeThrowsMade, freeThrowsAtt, freeThrowsPct, reboundsDef, reboundsOff, reboundsTotal, assists,
					 turnovers, steals, blocks, blocksReceived, foulsPersonal, foulsOn, pointsFromTurnovers,
					 pointsSecondChance, pointsFastBreak, pointsBench, pointsInThePaint)
	
}) -> WNBLBoxScores201617RawPart3

# Part 4
map_df(pageIDs201617Part4, function(i) {
	
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
		select(X1, X3, X66, X67, X68, X69, X70, X71, X75, X76, X77, X78, X79, X80, X81, X82, X83, X84,
					 X85, X86, X87, X88, X89, X90, X91, X92, X93)
	
	# Rename variables
	colnames(dfTeam1) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff",  "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam1$season <- "2016 / 2017" # Season
	dfTeam1$pageIDs201617 <- i # Unique game identifier
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
		select(X1, X3, X66, X67, X68, X69, X70, X71, X75, X76, X77, X78, X79, X80, X81, X82, X83, X84,
					 X85, X86, X87, X88, X89, X90, X91, X92, X93)
	
	# Rename variables
	colnames(dfTeam2) <- c(
		"teamName", "teamShortName", "fieldGoalsMade", "fieldGoalsAtt", "fieldGoalsPct", "threesMade",
		"threesAtt", "threesPct", "freeThrowsMade", "freeThrowsAtt", "freeThrowsPct", "reboundsDef",
		"reboundsOff", "reboundsTotal", "assists", "turnovers", "steals", "blocks", "blocksReceived",
		"foulsPersonal", "foulsOn", "pointsTotal", "pointsFromTurnovers", "pointsSecondChance",
		"pointsFastBreak", "pointsBench", "pointsInThePaint")
	
	# Add new variables
	dfTeam2$season <- "2016 / 2017" # Season
	dfTeam2$pageIDs201617 <- i # Unique game identifier
	dfTeam2$homeAway <- "Away" # Home or away
	dfTeam2$oppName <- unlist(df$tm[[1]][[1]])
	dfTeam2$oppPointsTotal <- as.numeric(as.character(unlist(df$tm[[1]][[18]])))
	dfTeam2$pointsMargin <- as.numeric(as.character(dfTeam2$pointsTotal)) - dfTeam2$oppPointsTotal
	
	# Merge team 1 and team 2 stats into one data frame
	dfBothTeams <- rbind(dfTeam1, dfTeam2, stringsAsFactors = FALSE)
	
	# Re-organise columns
	dfBothTeams <- dfBothTeams %>%
		select(season, pageIDs201617, teamName, teamShortName, homeAway, oppName, pointsTotal, oppPointsTotal,
					 pointsMargin, fieldGoalsMade, fieldGoalsAtt, fieldGoalsPct, threesMade, threesAtt, threesPct,
					 freeThrowsMade, freeThrowsAtt, freeThrowsPct, reboundsDef, reboundsOff, reboundsTotal, assists,
					 turnovers, steals, blocks, blocksReceived, foulsPersonal, foulsOn, pointsFromTurnovers,
					 pointsSecondChance, pointsFastBreak, pointsBench, pointsInThePaint)
	
}) -> WNBLBoxScores201617RawPart4

# Merge *Part1-4 data frames
WNBLBoxScores201617Raw <- rbind(WNBLBoxScores201617RawPart1, WNBLBoxScores201617RawPart2,
																WNBLBoxScores201617RawPart3, WNBLBoxScores201617RawPart4)

##### Save to CSV ##################################################################################

write.csv(IDs201617, "IDs201617.csv", row.names = FALSE)
write.csv(WNBLBoxScores201617Raw, "WNBLBoxScores201617Raw.csv", row.names = FALSE)