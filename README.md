# WNBL Box Scores - What's in this repository

Repo created and maintained by Jacquie Tran.

## Scripts
Use the R scripts in this repo to:

* Scrape data from FIBA LiveStats pages for Women's National Basketball League (WNBL) games played in the 2014 / 2015, 2015 / 2016, and 2016 / 2017 seasons.
* Clean the scraped data and create new variables (e.g., Dean Oliver's Four Factors)

## Outputs
The outputs from these scripts are also saved as CSV files if you want to jump straight to the data sets themselves (and you trust that my scraping and cleaning procedures are correct!). Outputs include:

* **WNBLBoxScores######Raw.csv:** Raw data obtained from scraping process, minimal cleaning performed.
* **WNBLBoxScores######.csv:** Clean version of data obtained from scraping process, with new (mostly derived) variables introduced.
* **IDs######.csv:** Key file with unique gameIDs (for distinguishing between unique games) and pageIDs (used to iterate the scraping process through a list of URLs)

## Known issues

* 2014 / 2015 season: Box scores for 3 games not available via FIBA Live Stats website (listed below). Alternate source used to obtain box score data (from WNBL.com.au) but this is missing some variables (e.g., defensive rebounds, offensive rebounds).
* 2015 / 2016 season: Box score for 1 game not available via FIBA Live Stats website (listed below). Alternate source used to obtain box score data (from SportsTG), but this is missing some variables (e.g., points from turnovers, points in the paint).

If anyone has access to the complete box scores for the games listed below, please get in touch!
* West Coast Waves vs. Townsville Fire, Oct 26 2014 @ 1300
* Sydney Uni Flames vs. Adelaide Lightning, Nov 1 2014 @ 1900
* Bendigo Spirit vs. Townsville Fire, Nov 22 2014 @ 1930
* University of Canberra Capitals vs. Perth Lynx, Jan 9 2016 @ 1900

## Next steps

[ ] Scrape box score data from earlier seasons (available through WNBL.com.au rather than FIBA Live Stats website)

## Version history

* v1.2: Published on 04/10/2017
     * [x] Incorporated box score data from an alternative source due to missing data on FIBA Live Stats website from 1 game in the 2015 / 2016 season
     * [x] Fixed errors in the data scraping script for 2015 / 2016 season
* v1.1: Published on 03/10/2017
     * [x] Incorporated box score data from an alternative source due to missing data on FIBA Live Stats website from 3 games in the 2014 / 2015 season
     * [x] Fixed errors in the data scraping script for 2014 / 2015 season
* v1.0: Published on 03/10/2017