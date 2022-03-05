---
title: "Happiness"
author: "Shuyi Bian, Carmina Inguito, Yutong Ji"
date: "`2022-03-03`"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---


## Introduction 

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


## Laod and Clean the Data

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

## Exploring the Data 
**Here, we're interested in understanding what kind of data we're going to be working with by utilizing several functions to observer its structure.**

```r
summary(happiness_2016)
```

```
##    country             region          happiness_rank   happiness_score
##  Length:157         Length:157         Min.   :  1.00   Min.   :2.905  
##  Class :character   Class :character   1st Qu.: 40.00   1st Qu.:4.404  
##  Mode  :character   Mode  :character   Median : 79.00   Median :5.314  
##                                        Mean   : 78.98   Mean   :5.382  
##                                        3rd Qu.:118.00   3rd Qu.:6.269  
##                                        Max.   :157.00   Max.   :7.526  
##  lower_confidence_interval upper_confidence_interval economy_gdp_per_capita
##  Min.   :2.732             Min.   :3.078             Min.   :0.0000        
##  1st Qu.:4.327             1st Qu.:4.465             1st Qu.:0.6702        
##  Median :5.237             Median :5.419             Median :1.0278        
##  Mean   :5.282             Mean   :5.482             Mean   :0.9539        
##  3rd Qu.:6.154             3rd Qu.:6.434             3rd Qu.:1.2796        
##  Max.   :7.460             Max.   :7.669             Max.   :1.8243        
##      family       health_life_expectancy    freedom      
##  Min.   :0.0000   Min.   :0.0000         Min.   :0.0000  
##  1st Qu.:0.6418   1st Qu.:0.3829         1st Qu.:0.2575  
##  Median :0.8414   Median :0.5966         Median :0.3975  
##  Mean   :0.7936   Mean   :0.5576         Mean   :0.3710  
##  3rd Qu.:1.0215   3rd Qu.:0.7299         3rd Qu.:0.4845  
##  Max.   :1.1833   Max.   :0.9528         Max.   :0.6085  
##  trust_government_corruption   generosity     dystopia_residual
##  Min.   :0.00000             Min.   :0.0000   Min.   :0.8179   
##  1st Qu.:0.06126             1st Qu.:0.1546   1st Qu.:2.0317   
##  Median :0.10547             Median :0.2225   Median :2.2907   
##  Mean   :0.13762             Mean   :0.2426   Mean   :2.3258   
##  3rd Qu.:0.17554             3rd Qu.:0.3119   3rd Qu.:2.6646   
##  Max.   :0.50521             Max.   :0.8197   Max.   :3.8377
```


## Merging Data Frames

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


```r
#merge data frames together
#thank you Joel
happiness_2018_join$trust_government_corruption <- as.numeric(happiness_2018_join$trust_government_corruption)
```

```
## Warning: NAs introduced by coercion
```

```r
complete_happiness <- bind_rows(happiness_2015_join, happiness_2016_join, happiness_2017_join, happiness_2018_join, happiness_2019_join) #bind the data frames

complete_happiness <- complete_happiness %>% 
  select(-region, -standard_error, -dystopia_residual, -lower_confidence_interval, -upper_confidence_interval, -whisker_high, -whisker_low) #remove unwanted columns
```
## 

```r
#happiness factors change from 2015-2019
library(shiny)

complete_happiness$year <- as.character(complete_happiness$year)

ui <- dashboardPage(
  dashboardHeader(title = "Happiness"),
  dashboardSidebar(disable = T),
  dashboardBody(
  fluidRow(
  box(title = "Plot Options", width = 3,
  selectInput("factor", "Select Happiness Factors", choices = c("economy_gdp_per_capita", "family", "health_life_expectancy", "freedom", "trust_government_corruption"), 
              selected = "economy_gdp_per_capita"),
  helpText("Factors' values reflect the extent to which a factor contributes to the calculation of the Happiness Score")),
  
  box(title = "Plot of Happiness Factors Data", width = 7,
  plotOutput("plot", width = "600px", height = "500px")))))

server <- function(input, output, session) { 
  output$plot <- renderPlot({
  complete_happiness %>%
      group_by(year) %>% 
      summarize(mean_factor_value = mean(get(input$factor), na.rm=TRUE)) %>% 
      ggplot(aes_string(x="year", y="mean_factor_value", fill="year"))+
      geom_col()+
      geom_text(aes(label = round(mean_factor_value, 2)),
                position = position_dodge(width = 0.9),
                vjust = -0.5,
                color = "black",
                size = 5)+
      theme_linedraw()+
      scale_fill_brewer(palette = "Pastel1")+
      labs(x=NULL,
           y="mean factor value")+
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 18))

  })
  session$onSessionEnded(stopApp)
  }

shinyApp(ui, server)
```

`<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>`{=html}


```r
#happiness factors change from 2015-2019
library(shiny)

complete_happiness$year <- as.numeric(complete_happiness$year)

ui <- dashboardPage(
  dashboardHeader(title = "Happiness"),
  dashboardSidebar(disable = T),
  dashboardBody(
  fluidRow(
  box(title = "Plot Options", width = 3,
  selectInput("factor", "Select Happiness Factors", choices = c("economy_gdp_per_capita", "family", "health_life_expectancy", "freedom", "trust_government_corruption"), 
              selected = "economy_gdp_per_capita"),
  sliderInput("year",
              "Year:",
              min = 2015,
              max = 2019,
              value = 2015,
              step = 1),
  
  helpText("Factors' values reflect the extent to which a factor contributes to the calculation of the Happiness Score")),
  
  box(title = "Plot of Happiness Factors Data", width = 7,
  plotOutput("plot", width = "600px", height = "500px")))))

server <- function(input, output, session) { 
  output$plot <- renderPlot({
    complete_happiness %>% 
      filter(year==input$year) %>% 
      ggplot(aes_string(x=input$factor, y="happiness_score"))+
      geom_point(size = 4, alpha = 0.5)+
      geom_smooth(method=lm, se=T)+
      labs(y="Happiness Score")+
      theme_linedraw()+
      scale_color_brewer(palette = "Pastel1")+
      labs(x="factor value",
           y="happiness score")+
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 18),
            axis.title.x = element_text(size = 18))
      
    
  })
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
```

`<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>`{=html}


## Data Visualization

```r
names(complete_happiness)
```

```
##  [1] "country_or_region"           "happiness_rank"             
##  [3] "happiness_score"             "economy_gdp_per_capita"     
##  [5] "family"                      "health_life_expectancy"     
##  [7] "freedom"                     "trust_government_corruption"
##  [9] "generosity"                  "year"
```
##Anlysis

##Research Questions for Analysis 
