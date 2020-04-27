# Gender's Effect on Speaker Points in National Circuit Lincoln-Douglas

## Introduction

Competitive debating has a little thing called “speaker points,” where, in addition to deciding the debate, judges give debaters points from 20-30 on how good a debater that judge thinks you are. If you won the round, but the judge didn’t like your demeanor, or your presentation, or your voice, you might get less points. These points are often used to set elimination brackets and people get awards for getting lots of points. Using pretty much every national tournament from the past three years, I investigated to see if there’s a gender bias in the amount of speaker points judges assign to debaters.

Check out the app [here!](https://ishanbhatt.shinyapps.io/debate_points_gender/)

## Sources

The data is sourced directly from each tournament's results page on [Tabroom.](https://www.tabroom.com/index/index.mhtml)

The gender approximation package can be found [here.](https://github.com/ropensci/gender)

The app is built using Shiny, with key functionality from packages including tidyverse and rvest.

## Navigation

**data_scraping files:** These contain the function I created to scrape data from tabroom and my initial cleaning/creation of the data.

**debate_points_gender:** This contains the shiny app. The raw-data folder contains the data compiled and converted to .csv from the data_scraping package. app.R contains the code to build the tool.

