---
title: "Happiness"
author: "Shuyi Bian, Carmina Inguito, Yutong Ji"
date: "2022-03-03"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---



## Load the libraries

```r
library(tidyverse)
library(RColorBrewer)
library(paletteer)
library(janitor)
library(here)
library(ggthemes)
library(shiny)
library(shinydashboard)
```


```r
options(scipen=999) #cancels the use of scientific notation for the session
```

## Import Data

```r
happiness_2015 <- readr::read_csv("data/2015.csv")%>% 
  clean_names()
```

```
## Rows: 158 Columns: 12
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): Country, Region
## dbl (10): Happiness Rank, Happiness Score, Standard Error, Economy (GDP per ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
happiness_2016 <- readr::read_csv("data/2016.csv")%>% 
  clean_names()
```

```
## Rows: 157 Columns: 13
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): Country, Region
## dbl (11): Happiness Rank, Happiness Score, Lower Confidence Interval, Upper ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
happiness_2017 <- readr::read_csv("data/2017.csv")%>% 
  clean_names()
```

```
## Rows: 155 Columns: 12
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (1): Country
## dbl (11): Happiness.Rank, Happiness.Score, Whisker.high, Whisker.low, Econom...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
happiness_2018 <- readr::read_csv("data/2018.csv")%>% 
  clean_names()
```

```
## Rows: 156 Columns: 9
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): Country or region, Perceptions of corruption
## dbl (7): Overall rank, Score, GDP per capita, Social support, Healthy life e...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
happiness_2019 <- readr::read_csv("data/2019.csv")%>% 
  clean_names()
```

```
## Rows: 156 Columns: 9
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (1): Country or region
## dbl (8): Overall rank, Score, GDP per capita, Social support, Healthy life e...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```
## Making Joining Files

```r
# Add year
happiness_2015_join <- happiness_2015 %>% 
  mutate(year = case_when(country != "NA" ~ "2015"))
happiness_2016_join <- happiness_2016 %>% 
  mutate(year = case_when(country != "NA" ~ "2016"))
happiness_2017_join <- happiness_2017 %>% 
  mutate(year = case_when(country != "NA" ~ "2017"))
happiness_2018_join <- happiness_2018 %>% 
  mutate(year = case_when(country_or_region != "NA" ~ "2018"))
happiness_2019_join <- happiness_2019 %>% 
  mutate(year = case_when(country_or_region != "NA" ~ "2019"))
```


```r
#Change Column names
happiness_2015_join <- happiness_2015_join %>% 
  rename(country_or_region = country)

happiness_2016_join <- happiness_2016_join %>% 
  rename(country_or_region = country)

happiness_2017_join <- happiness_2017_join %>% 
  rename(country_or_region = country)

happiness_2018_join <- happiness_2018_join %>% 
  rename(happiness_rank = overall_rank,
         happiness_score = score,
         economy_gdp_per_capita = gdp_per_capita,
         family = social_support,
         health_life_expectancy = healthy_life_expectancy,
         freedom = freedom_to_make_life_choices,
         trust_government_corruption = perceptions_of_corruption)

happiness_2019_join <- happiness_2019_join %>% 
  rename(happiness_rank = overall_rank,
         happiness_score = score,
         economy_gdp_per_capita = gdp_per_capita,
         family = social_support,
         health_life_expectancy = healthy_life_expectancy,
         freedom = freedom_to_make_life_choices,
         trust_government_corruption = perceptions_of_corruption)
```

Here is my way to do dataframe merge

```r
#drop unrelated column (test)
happiness_2015_join <- subset(happiness_2015_join, select = -c(region, standard_error, dystopia_residual))

happiness_2016_join <- subset(happiness_2016_join, select = -c(region, lower_confidence_interval, upper_confidence_interval, dystopia_residual))
```



```r
#merge files (test)
test_merge <- rbind(happiness_2015_join, happiness_2016_join)
```


