##### Load libraries ###############################################################################

library(rjson)
library(purrr)
library(dplyr)
library(htmltab)

##### Compile game and page IDs: 2014 / 2015 #######################################################

gameIDs201415 <- c(
	"20141017-1930_MELvsCAN", "20141018-1500_DANvsMEL", "20141018-1900_CANvsSYD", "20141018-1830_WCWvsADL",
	"20141018-1900_TSVvsBEN", "20141023-1700_ADLvsDAN", "20141025-1500_BENvsCAN", "20141025-1830_DANvsSYD",
	"20141026-1300_MELvsBEN", "20141026-1300_TSVvsWCW", "20141030-1930_BENvsDAN", "20141031-1900_TSVvsADL",
	"20141101-1500_CANvsDAN", "20141101-1830_WCWvsMEL", "20141101-1900_SYDvsADL", "20141105-1900_SYDvsCAN",
	"20141107-1930_WCWvsBEN", "20141108-1500_DANvsTSV", "20141109-1300_CANvsWCW", "20141109-1400_ADLvsBEN",
	"20141109-1300_MELvsDAN", "20141114-1900_SYDvsMEL", "20141114-1200_WCWvsCAN", "20141115-1700_ADLvsSYD",
	"20141115-1900_TSVvsMEL", "20141116-1400_DANvsBEN", "20141121-1700_ADLvsTSV", "20141122-1500_MELvsADL",
	"20141122-1830_DANvsCAN", "20141122-1930_WCWvsSYD", "20141122-1930_BENvsTSV", "20141128-1900_SYDvsADL",
	"20141129-1500_BENvsMEL", "20141129-1900_CANvsADL", "20141129-1900_TSVvsWCW", "20141130-1300_MELvsSYD",
	"20141205-1900_SYDvsBEN", "20141205-1930_DANvsWCW", "20141206-1500_MELvsWCW", "20141206-1930_BENvsADL",
	"20141207-1300_CANvsMEL", "20141207-1300_TSVvsDAN", "20141212-1700_ADLvsMEL", "20141214-1500_CANvsTSV",
	"20141212-1930_WCWvsDAN", "20141213-1300_SYDvsTSV", "20141213-1930_MELvsBEN", "20141219-1700_ADLvsCAN",
	"20141219-1930_BENvsSYD", "20141220-1500_DANvsSYD", "20141220-1830_WCWvsCAN", "20141220-1900_TSVvsMEL",
	"20150102-1900_TSVvsCAN", "20150103-1500_BENvsWCW", "20150103-1500_SYDvsDAN", "20150104-1100_ADLvsWCW",
	"20150104-1300_CANvsDAN", "20150109-1900_CANvsWCW", "20150110-1500_MELvsADL", "20150110-1900_SYDvsWCW",
	"20150110-1900_TSVvsDAN", "20150111-1400_BENvsADL", "20150115-1900_CANvsSYD", "20150116-1930_ADLvsTSV",
	"20150116-1930_DANvsBEN", "20150117-1500_SYDvsMEL", "20150118-1500_WCWvsTSV", "20150123-1930_BENvsCAN",
	"20150123-1930_DANvsADL", "20150124-1500_MELvsCAN", "20150125-1300_TSVvsSYD", "20150125-1300_WCWvsBEN",
	"20150130-1900_CANvsADL", "20150131-1500_DANvsWCW", "20150131-1900_SYDvsBEN", "20150131-1930_MELvsTSV",
	"20150201-1100_ADLvsWCW", "20150201-1800_BENvsTSV", "20150206-1700_ADLvsSYD", "20150206-1900_TSVvsCAN",
	"20150207-1500_MELvsDAN", "20150207-1830_WCWvsSYD", "20150208-1400_BENvsMEL", "20150213-1740_CANvsBEN",
	"20150213-1930_ADLvsDAN", "20150214-1830_WCWvsMEL", "20150214-1900_SYDvsTSV", "20150215-1700_DANvsTSV"
)

pageIDs201415 <- seq(64511, 64598, by = 1)

# Create separate vectors for pageIDs due to in-season changes in format of json file
# Also, exclude pageIDs == c(64520, 64525, 64541) due to missing data from FIBA Live Stats
pageIDs201415Part1 <- c(
	seq(64511, 64519, by = 1), 
	seq(64521, 64524, by = 1),
	seq(64526, 64540, by = 1),
	seq(64542, 64548, by = 1),
	seq(64550, 64592, by = 1),
	seq(64594, 64597, by = 1))
pageIDs201415Part2 <- c(64549, 64593, 64598)

URLPrefix <- "http://www.fibalivestats.com/data/"

URLSuffix <- "/data.json"

IDs201415 <- data.frame(cbind(gameIDs201415, pageIDs201415))

##### Data scraping: 2014 / 2015 ###################################################################

# Part 1
map_df(pageIDs201415Part1, function(i) {
	
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
	
}) -> WNBLBoxScores201415RawPart1

# Part 2
map_df(pageIDs201415Part2, function(i) {
	
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
		select(X1, X2, X24, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39,
					 X40, X41, X42, X43, X44, X45, X46, X47, X48)
	
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
		select(X1, X2, X24, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39,
					 X40, X41, X42, X43, X44, X45, X46, X47, X48)
	
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
	
}) -> WNBLBoxScores201415RawPart2

# Merge *Part1 and *Part2 data frames
WNBLBoxScores201415Raw <- rbind(WNBLBoxScores201415RawPart1, WNBLBoxScores201415RawPart2)

##### Data scraping: 2014 / 2015 - special case 1 ##################################################

# Box score data not in FIBA Live Stats for:
# gameID == 20141026-1300_TSVvsWCW, pageID == 64520
# ^ Alternate source for box score: http://wnbl.com.au/stats/?WHurl=%2Fmatch%2F64520%2Fboxscore%3F

# Step 1 - visit the listed website and save the webpages as static HTML files

# Step 2 - import the static html files into R, then parse tables using htmltab package
rawHTMLMissingGame1 <- paste(readLines("20141026-1300_TSVvsWCW.html"))
dataMissingGame1Team1 <- htmltab(doc = rawHTMLMissingGame1,
																 which = '//*[@id="hs-container"]/div[3]/div/div/div/div[5]/div[1]/table')
dataMissingGame1Team2 <- htmltab(doc = rawHTMLMissingGame1,
																 which = '//*[@id="hs-container"]/div[3]/div/div/div/div[5]/div[2]/table')

# Step 3a - Basic data cleaning to facilitate subsequent merge (team 1)
colnames(dataMissingGame1Team1) <- c(
	"drop1", "drop2", "assists", "blocks", "fieldGoalsAtt", "fieldGoalsMade", "fieldGoalsPct", "foulsOn",
	"freeThrowsAtt", "freeThrowsMade", "freeThrowsPct", "drop3", "pointsTotal", "reboundsTotal", "steals",
	"threesAtt", "threesMade", "threesPct", "turnovers", "drop4", "drop5", "drop6")
dataMissingGame1Team1 <- dataMissingGame1Team1 %>%
	select(-drop1, -drop2, -drop3, -drop4, -drop5, -drop6)
dataMissingGame1Team1[] <- lapply(dataMissingGame1Team1, function(x) as.numeric(as.character(x)))
dataMissingGame1Team1 <- dataMissingGame1Team1 %>%
	summarise(
		assists = sum(assists),
		blocks = sum(blocks),
		fieldGoalsAtt = sum(fieldGoalsAtt),
		fieldGoalsMade = sum(fieldGoalsMade),
		foulsOn = sum(foulsOn),
		freeThrowsAtt = sum(freeThrowsAtt),
		freeThrowsMade = sum(freeThrowsMade),
		pointsTotal = sum(pointsTotal),
		reboundsTotal = sum(reboundsTotal),
		steals = sum(steals),
		threesAtt = sum(threesAtt),
		threesMade = sum(threesMade),
		turnovers = sum(turnovers))
dataMissingGame1Team1$fieldGoalsPct <- round(
	dataMissingGame1Team1$fieldGoalsMade / dataMissingGame1Team1$fieldGoalsAtt * 100, 1)
dataMissingGame1Team1$freeThrowsPct <- round(
	dataMissingGame1Team1$freeThrowsMade / dataMissingGame1Team1$freeThrowsAtt * 100, 1)
dataMissingGame1Team1$threesPct <- round(
	dataMissingGame1Team1$threesMade / dataMissingGame1Team1$threesAtt * 100, 1)
dataMissingGame1Team1$season <- "2014 / 2015"
dataMissingGame1Team1$pageIDs201415 <- 64520
dataMissingGame1Team1$teamName <- "Townsville"
dataMissingGame1Team1$teamShortName <- "Fire"
dataMissingGame1Team1$homeAway <- "Home"
dataMissingGame1Team1$oppName <- "West Coast"

# Step 3b - Basic data cleaning to facilitate subsequent merge (team 2)
colnames(dataMissingGame1Team2) <- c(
	"drop1", "drop2", "assists", "blocks", "fieldGoalsAtt", "fieldGoalsMade", "fieldGoalsPct", "foulsOn",
	"freeThrowsAtt", "freeThrowsMade", "freeThrowsPct", "drop3", "pointsTotal", "reboundsTotal", "steals",
	"threesAtt", "threesMade", "threesPct", "turnovers", "drop4", "drop5", "drop6")
dataMissingGame1Team2 <- dataMissingGame1Team2 %>%
	select(-drop1, -drop2, -drop3, -drop4, -drop5, -drop6)
dataMissingGame1Team2[] <- lapply(dataMissingGame1Team2, function(x) as.numeric(as.character(x)))
dataMissingGame1Team2 <- dataMissingGame1Team2 %>%
	summarise(
		assists = sum(assists),
		blocks = sum(blocks),
		fieldGoalsAtt = sum(fieldGoalsAtt),
		fieldGoalsMade = sum(fieldGoalsMade),
		foulsOn = sum(foulsOn),
		freeThrowsAtt = sum(freeThrowsAtt),
		freeThrowsMade = sum(freeThrowsMade),
		pointsTotal = sum(pointsTotal),
		reboundsTotal = sum(reboundsTotal),
		steals = sum(steals),
		threesAtt = sum(threesAtt),
		threesMade = sum(threesMade),
		turnovers = sum(turnovers))
dataMissingGame1Team2$fieldGoalsPct <- round(
	dataMissingGame1Team2$fieldGoalsMade / dataMissingGame1Team2$fieldGoalsAtt * 100, 1)
dataMissingGame1Team2$freeThrowsPct <- round(
	dataMissingGame1Team2$freeThrowsMade / dataMissingGame1Team2$freeThrowsAtt * 100, 1)
dataMissingGame1Team2$threesPct <- round(
	dataMissingGame1Team2$threesMade / dataMissingGame1Team2$threesAtt * 100, 1)
dataMissingGame1Team2$season <- "2014 / 2015"
dataMissingGame1Team2$pageIDs201415 <- 64520
dataMissingGame1Team2$teamName <- "West Coast"
dataMissingGame1Team2$teamShortName <- "West Coast"
dataMissingGame1Team2$homeAway <- "Away"
dataMissingGame1Team2$oppName <- "Townsville"

# Step 4 - Calculate oppositional variables, merge team 1 and 2 data, finalise data cleaning
dataMissingGame1Team1$blocksReceived <- dataMissingGame1Team2$blocks
dataMissingGame1Team2$blocksReceived <- dataMissingGame1Team1$blocks
dataMissingGame1Team1$foulsPersonal <- dataMissingGame1Team2$foulsOn
dataMissingGame1Team2$foulsPersonal <- dataMissingGame1Team1$foulsOn
dataMissingGame1Team1$oppPointsTotal <- dataMissingGame1Team2$pointsTotal
dataMissingGame1Team2$oppPointsTotal <- dataMissingGame1Team1$pointsTotal
dataMissingGame1 <- rbind(dataMissingGame1Team1, dataMissingGame1Team2)
dataMissingGame1$pointsMargin <- dataMissingGame1$pointsTotal - dataMissingGame1$oppPointsTotal
dataMissingGame1$reboundsDef <- NA
dataMissingGame1$reboundsOff <- NA
dataMissingGame1$pointsFromTurnovers <- NA
dataMissingGame1$pointsSecondChance <- NA
dataMissingGame1$pointsFastBreak <- NA
dataMissingGame1$pointsBench <- NA
dataMissingGame1$pointsInThePaint <- NA

# Step 5 - Merge with WNBLBoxScores201415Raw file
WNBLBoxScores201415Raw <- rbind(WNBLBoxScores201415Raw, dataMissingGame1)

# Remove interstitial objects
rm(rawHTMLMissingGame1, dataMissingGame1, dataMissingGame1Team1, dataMissingGame1Team2)

##### Data scraping: 2014 / 2015 - special case 2 ##################################################

# Box score data not in FIBA Live Stats for:
# gameID == 20141101-1900_SYDvsADL, pageID == 64525
# ^ Alternate source for box score: http://wnbl.com.au/stats/?WHurl=%2Fmatch%2F64525%2Fboxscore%3F

# Step 1 - visit the listed website and save the webpages as static HTML files

# Step 2 - import the static html files into R, then parse tables using htmltab package
rawHTMLMissingGame2 <- paste(readLines("20141101-1900_SYDvsADL.html"))
dataMissingGame2Team1 <- htmltab(doc = rawHTMLMissingGame2,
																 which = '//*[@id="hs-container"]/div[3]/div/div/div/div[5]/div[1]/table')
dataMissingGame2Team2 <- htmltab(doc = rawHTMLMissingGame2,
																 which = '//*[@id="hs-container"]/div[3]/div/div/div/div[5]/div[2]/table')

# Step 3a - Basic data cleaning to facilitate subsequent merge (team 1)
colnames(dataMissingGame2Team1) <- c(
	"drop1", "drop2", "assists", "blocks", "fieldGoalsAtt", "fieldGoalsMade", "fieldGoalsPct", "foulsOn",
	"freeThrowsAtt", "freeThrowsMade", "freeThrowsPct", "drop3", "pointsTotal", "reboundsTotal", "steals",
	"threesAtt", "threesMade", "threesPct", "turnovers", "drop4", "drop5", "drop6")
dataMissingGame2Team1 <- dataMissingGame2Team1 %>%
	select(-drop1, -drop2, -drop3, -drop4, -drop5, -drop6)
dataMissingGame2Team1[] <- lapply(dataMissingGame2Team1, function(x) as.numeric(as.character(x)))
dataMissingGame2Team1 <- dataMissingGame2Team1 %>%
	summarise(
		assists = sum(assists),
		blocks = sum(blocks),
		fieldGoalsAtt = sum(fieldGoalsAtt),
		fieldGoalsMade = sum(fieldGoalsMade),
		foulsOn = sum(foulsOn),
		freeThrowsAtt = sum(freeThrowsAtt),
		freeThrowsMade = sum(freeThrowsMade),
		pointsTotal = sum(pointsTotal),
		reboundsTotal = sum(reboundsTotal),
		steals = sum(steals),
		threesAtt = sum(threesAtt),
		threesMade = sum(threesMade),
		turnovers = sum(turnovers))
dataMissingGame2Team1$fieldGoalsPct <- round(
	dataMissingGame2Team1$fieldGoalsMade / dataMissingGame2Team1$fieldGoalsAtt * 100, 1)
dataMissingGame2Team1$freeThrowsPct <- round(
	dataMissingGame2Team1$freeThrowsMade / dataMissingGame2Team1$freeThrowsAtt * 100, 1)
dataMissingGame2Team1$threesPct <- round(
	dataMissingGame2Team1$threesMade / dataMissingGame2Team1$threesAtt * 100, 1)
dataMissingGame2Team1$season <- "2014 / 2015"
dataMissingGame2Team1$pageIDs201415 <- 64525
dataMissingGame2Team1$teamName <- "Sydney Uni"
dataMissingGame2Team1$teamShortName <- "Sydney Uni"
dataMissingGame2Team1$homeAway <- "Home"
dataMissingGame2Team1$oppName <- "Adelaide"

# Step 3b - Basic data cleaning to facilitate subsequent merge (team 2)
colnames(dataMissingGame2Team2) <- c(
	"drop1", "drop2", "assists", "blocks", "fieldGoalsAtt", "fieldGoalsMade", "fieldGoalsPct", "foulsOn",
	"freeThrowsAtt", "freeThrowsMade", "freeThrowsPct", "drop3", "pointsTotal", "reboundsTotal", "steals",
	"threesAtt", "threesMade", "threesPct", "turnovers", "drop4", "drop5", "drop6")
dataMissingGame2Team2 <- dataMissingGame2Team2 %>%
	select(-drop1, -drop2, -drop3, -drop4, -drop5, -drop6)
dataMissingGame2Team2[] <- lapply(dataMissingGame2Team2, function(x) as.numeric(as.character(x)))
dataMissingGame2Team2 <- dataMissingGame2Team2 %>%
	summarise(
		assists = sum(assists),
		blocks = sum(blocks),
		fieldGoalsAtt = sum(fieldGoalsAtt),
		fieldGoalsMade = sum(fieldGoalsMade),
		foulsOn = sum(foulsOn),
		freeThrowsAtt = sum(freeThrowsAtt),
		freeThrowsMade = sum(freeThrowsMade),
		pointsTotal = sum(pointsTotal),
		reboundsTotal = sum(reboundsTotal),
		steals = sum(steals),
		threesAtt = sum(threesAtt),
		threesMade = sum(threesMade),
		turnovers = sum(turnovers))
dataMissingGame2Team2$fieldGoalsPct <- round(
	dataMissingGame2Team2$fieldGoalsMade / dataMissingGame2Team2$fieldGoalsAtt * 100, 1)
dataMissingGame2Team2$freeThrowsPct <- round(
	dataMissingGame2Team2$freeThrowsMade / dataMissingGame2Team2$freeThrowsAtt * 100, 1)
dataMissingGame2Team2$threesPct <- round(
	dataMissingGame2Team2$threesMade / dataMissingGame2Team2$threesAtt * 100, 1)
dataMissingGame2Team2$season <- "2014 / 2015"
dataMissingGame2Team2$pageIDs201415 <- 64525
dataMissingGame2Team2$teamName <- "Adelaide"
dataMissingGame2Team2$teamShortName <- "Adelaide"
dataMissingGame2Team2$homeAway <- "Away"
dataMissingGame2Team2$oppName <- "Sydney Uni"

# Step 4 - Calculate oppositional variables, merge team 1 and 2 data, finalise data cleaning
dataMissingGame2Team1$blocksReceived <- dataMissingGame2Team2$blocks
dataMissingGame2Team2$blocksReceived <- dataMissingGame2Team1$blocks
dataMissingGame2Team1$foulsPersonal <- dataMissingGame2Team2$foulsOn
dataMissingGame2Team2$foulsPersonal <- dataMissingGame2Team1$foulsOn
dataMissingGame2Team1$oppPointsTotal <- dataMissingGame2Team2$pointsTotal
dataMissingGame2Team2$oppPointsTotal <- dataMissingGame2Team1$pointsTotal
dataMissingGame2 <- rbind(dataMissingGame2Team1, dataMissingGame2Team2)
dataMissingGame2$pointsMargin <- dataMissingGame2$pointsTotal - dataMissingGame2$oppPointsTotal
dataMissingGame2$reboundsDef <- NA
dataMissingGame2$reboundsOff <- NA
dataMissingGame2$pointsFromTurnovers <- NA
dataMissingGame2$pointsSecondChance <- NA
dataMissingGame2$pointsFastBreak <- NA
dataMissingGame2$pointsBench <- NA
dataMissingGame2$pointsInThePaint <- NA

# Step 5 - Merge with WNBLBoxScores201415Raw file
WNBLBoxScores201415Raw <- rbind(WNBLBoxScores201415Raw, dataMissingGame2)

# Remove interstitial objects
rm(rawHTMLMissingGame2, dataMissingGame2, dataMissingGame2Team1, dataMissingGame2Team2)

##### Data scraping: 2014 / 2015 - special case 3 ##################################################

# Box score data not in FIBA Live Stats for:
# gameID == 20141122-1930_BENvsTSV, pageID == 64541
# ^ Alternate source for box score: http://wnbl.com.au/stats/?WHurl=%2Fmatch%2F64541%2Fboxscore%3F

# Step 1 - visit the listed website and save the webpages as static HTML files

# Step 2 - import the static html files into R, then parse tables using htmltab package
rawHTMLMissingGame3 <- paste(readLines("20141122-1930_BENvsTSV.html"))
dataMissingGame3Team1 <- htmltab(doc = rawHTMLMissingGame3,
																 which = '//*[@id="hs-container"]/div[3]/div/div/div/div[5]/div[1]/table')
dataMissingGame3Team2 <- htmltab(doc = rawHTMLMissingGame3,
																 which = '//*[@id="hs-container"]/div[3]/div/div/div/div[5]/div[2]/table')

# Step 3a - Basic data cleaning to facilitate subsequent merge (team 1)
colnames(dataMissingGame3Team1) <- c(
	"drop1", "drop2", "assists", "blocks", "fieldGoalsAtt", "fieldGoalsMade", "fieldGoalsPct", "foulsOn",
	"freeThrowsAtt", "freeThrowsMade", "freeThrowsPct", "drop3", "pointsTotal", "reboundsTotal", "steals",
	"threesAtt", "threesMade", "threesPct", "turnovers", "drop4", "drop5", "drop6")
dataMissingGame3Team1 <- dataMissingGame3Team1 %>%
	select(-drop1, -drop2, -drop3, -drop4, -drop5, -drop6)
dataMissingGame3Team1[] <- lapply(dataMissingGame3Team1, function(x) as.numeric(as.character(x)))
dataMissingGame3Team1 <- dataMissingGame3Team1 %>%
	summarise(
		assists = sum(assists),
		blocks = sum(blocks),
		fieldGoalsAtt = sum(fieldGoalsAtt),
		fieldGoalsMade = sum(fieldGoalsMade),
		foulsOn = sum(foulsOn),
		freeThrowsAtt = sum(freeThrowsAtt),
		freeThrowsMade = sum(freeThrowsMade),
		pointsTotal = sum(pointsTotal),
		reboundsTotal = sum(reboundsTotal),
		steals = sum(steals),
		threesAtt = sum(threesAtt),
		threesMade = sum(threesMade),
		turnovers = sum(turnovers))
dataMissingGame3Team1$freeThrowsMade <- 14 # Deduced based on data available on web page
dataMissingGame3Team1$freeThrowsAtt <- 15 # Deduced based on data available on web page
dataMissingGame3Team1$fieldGoalsPct <- round(
	dataMissingGame3Team1$fieldGoalsMade / dataMissingGame3Team1$fieldGoalsAtt * 100, 1)
dataMissingGame3Team1$freeThrowsPct <- round(
	dataMissingGame3Team1$freeThrowsMade / dataMissingGame3Team1$freeThrowsAtt * 100, 1)
dataMissingGame3Team1$threesPct <- round(
	dataMissingGame3Team1$threesMade / dataMissingGame3Team1$threesAtt * 100, 1)
dataMissingGame3Team1$season <- "2014 / 2015"
dataMissingGame3Team1$pageIDs201415 <- 64541
dataMissingGame3Team1$teamName <- "Bendigo"
dataMissingGame3Team1$teamShortName <- "Spirit"
dataMissingGame3Team1$homeAway <- "Home"
dataMissingGame3Team1$oppName <- "Townsville"

# Step 3b - Basic data cleaning to facilitate subsequent merge (team 2)
colnames(dataMissingGame3Team2) <- c(
	"drop1", "drop2", "assists", "blocks", "fieldGoalsAtt", "fieldGoalsMade", "fieldGoalsPct", "foulsOn",
	"freeThrowsAtt", "freeThrowsMade", "freeThrowsPct", "drop3", "pointsTotal", "reboundsTotal", "steals",
	"threesAtt", "threesMade", "threesPct", "turnovers", "drop4", "drop5", "drop6")
dataMissingGame3Team2 <- dataMissingGame3Team2 %>%
	select(-drop1, -drop2, -drop3, -drop4, -drop5, -drop6)
dataMissingGame3Team2[] <- lapply(dataMissingGame3Team2, function(x) as.numeric(as.character(x)))
dataMissingGame3Team2 <- dataMissingGame3Team2 %>%
	summarise(
		assists = sum(assists),
		blocks = sum(blocks),
		fieldGoalsAtt = sum(fieldGoalsAtt),
		fieldGoalsMade = sum(fieldGoalsMade),
		foulsOn = sum(foulsOn),
		freeThrowsAtt = sum(freeThrowsAtt),
		freeThrowsMade = sum(freeThrowsMade),
		pointsTotal = sum(pointsTotal),
		reboundsTotal = sum(reboundsTotal),
		steals = sum(steals),
		threesAtt = sum(threesAtt),
		threesMade = sum(threesMade),
		turnovers = sum(turnovers))
dataMissingGame3Team2$freeThrowsMade <- 13 # Deduced based on data available on web page
dataMissingGame3Team2$freeThrowsAtt <- 16 # Deduced based on data available on web page
dataMissingGame3Team2$fieldGoalsPct <- round(
	dataMissingGame3Team2$fieldGoalsMade / dataMissingGame3Team2$fieldGoalsAtt * 100, 1)
dataMissingGame3Team2$freeThrowsPct <- round(
	dataMissingGame3Team2$freeThrowsMade / dataMissingGame3Team2$freeThrowsAtt * 100, 1)
dataMissingGame3Team2$threesPct <- round(
	dataMissingGame3Team2$threesMade / dataMissingGame3Team2$threesAtt * 100, 1)
dataMissingGame3Team2$season <- "2014 / 2015"
dataMissingGame3Team2$pageIDs201415 <- 64541
dataMissingGame3Team2$teamName <- "Townsville"
dataMissingGame3Team2$teamShortName <- "Fire"
dataMissingGame3Team2$homeAway <- "Away"
dataMissingGame3Team2$oppName <- "Bendigo"

# Step 4 - Calculate oppositional variables, merge team 1 and 2 data, finalise data cleaning
dataMissingGame3Team1$blocksReceived <- dataMissingGame3Team2$blocks
dataMissingGame3Team2$blocksReceived <- dataMissingGame3Team1$blocks
dataMissingGame3Team1$foulsPersonal <- dataMissingGame3Team2$foulsOn
dataMissingGame3Team2$foulsPersonal <- dataMissingGame3Team1$foulsOn
dataMissingGame3Team1$oppPointsTotal <- dataMissingGame3Team2$pointsTotal
dataMissingGame3Team2$oppPointsTotal <- dataMissingGame3Team1$pointsTotal
dataMissingGame3 <- rbind(dataMissingGame3Team1, dataMissingGame3Team2)
dataMissingGame3$pointsMargin <- dataMissingGame3$pointsTotal - dataMissingGame3$oppPointsTotal
dataMissingGame3$reboundsDef <- NA
dataMissingGame3$reboundsOff <- NA
dataMissingGame3$pointsFromTurnovers <- NA
dataMissingGame3$pointsSecondChance <- NA
dataMissingGame3$pointsFastBreak <- NA
dataMissingGame3$pointsBench <- NA
dataMissingGame3$pointsInThePaint <- NA

# Step 5 - Merge with WNBLBoxScores201415Raw file
WNBLBoxScores201415Raw <- rbind(WNBLBoxScores201415Raw, dataMissingGame3)

# Remove interstitial objects
rm(rawHTMLMissingGame3, dataMissingGame3, dataMissingGame3Team1, dataMissingGame3Team2)

# Sort master data frame by pageID
WNBLBoxScores201415Raw <- WNBLBoxScores201415Raw[order(WNBLBoxScores201415Raw$pageIDs201415), ]

##### Save to CSV ##################################################################################

write.csv(IDs201415, "IDs201415.csv", row.names = FALSE)
write.csv(WNBLBoxScores201415Raw, "WNBLBoxScores201415Raw.csv", row.names = FALSE)