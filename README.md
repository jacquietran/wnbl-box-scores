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

## Next steps

[ ] Scrape box score data from earlier seasons (available through WNBL.com.au rather than FIBA Live Stats website)

## Version history

* v1.1: Published on 03/10/2017
     * [x] Incorporated box score data from an alternative source due to missing data on FIBA Live Stats website
     * [x] Fixed errors in the data scraping script for 2014 / 2015 season
* v1.0: Published on 03/10/2017