165 Project
================
2024-04-25

``` r
knitr::opts_chunk$set(fig.path='Figs/')
```

## R Markdown

``` r
library(readxl)
library(ggplot2)
library(MASS)
library(stats)

scores_data <- read_excel("/Users/setaranusratty/Desktop/All Scores 165.xlsx")

plot1 <- ggplot(scores_data, aes(x=Score)) +
  geom_histogram(aes(y=after_stat(density)), bins=12, fill="blue", color="black") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Distribution of All Scores", x="Score", y="Frequency")

plot1
```

![](Figs/cars-1.png)<!-- -->

``` r
# Normality test
shapiro_test <- shapiro.test(scores_data$Score)

# Log transformation for log-normal distribution check
log_scores <- log(scores_data$Score)
shapiro_log <- shapiro.test(log_scores)

# Print results
print(shapiro_test)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  scores_data$Score
    ## W = 0.98043, p-value = 0.1668

``` r
print(shapiro_log)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  log_scores
    ## W = 0.97029, p-value = 0.02933

``` r
mean(scores_data$Score)
```

    ## [1] 13.89584

``` r
# Load libraries
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:MASS':
    ## 
    ##     select

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# Load the datasets
all_scores <- read_excel("/Users/setaranusratty/Desktop/All Scores 165.xlsx")
top_scores <- read_excel("/Users/setaranusratty/Desktop/Scores 2008 Onwards 165.xlsx")

# Data Cleaning
all_scores <- all_scores %>% 
  filter(!is.na(Score)) %>%
  distinct()

top_scores <- top_scores %>% 
  filter(!is.na(Score)) %>%
  distinct()

# 1. Temporal Analysis: Plot scores over the years
plot2 <- ggplot(top_scores, aes(x = Year, y = Score, group = Place, color = factor(Place))) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Trend of Scores for Top 2 Places Over the Years",
       x = "Year",
       y = "Score",
       color = "Medal Position")
plot2
```

![](Figs/unnamed-chunk-1-1.png)<!-- -->

``` r
#word champion 2023 qualifier scores

# Updated Data
gymnasts <- c("Simone Biles", "Rebeca Andrade", "Flavia Saraiva", "Sabrina Maneca-Voinea", "Shilese Jones", "Naomi Visser", "Zhou Yaqin", "Alice Kinsella")
countries <- c("USA", "BRA", "BRA", "ROU", "USA", "NED", "CHN", "GBR")
scores <- c(14.633, 14.500, 13.966, 13.766, 13.666, 13.300, 13.300, 12.666)

data <- data.frame(gymnasts, countries, scores)

plot3 <- ggplot(data, aes(x=scores, y=reorder(gymnasts, scores), color=countries)) + 
  geom_point(size=4) +
  labs(title="2023 World Artistic Gymnastics Championships: Women's Floor Qualification",
       x="Score",
       y="Gymnast") +
  theme_minimal() +
  scale_color_brewer(palette="Set1")

plot3
```

![](Figs/unnamed-chunk-2-1.png)<!-- -->

``` r
library(randomForest)
```

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
years <- c(2008, 2012, 2016, 2020)
scores <- c(15.525, 15.133, 15.933, 13.966)  

rf_model <- randomForest(scores ~ years, data=data.frame(years, scores), ntree=1000, importance=TRUE)
```

    ## Warning in randomForest.default(m, y, ...): The response has five or fewer
    ## unique values.  Are you sure you want to do regression?

``` r
years_pred <- seq(2008, 2024, by=4)
predicted_scores <- predict(rf_model, data.frame(years=years_pred))

plot(years, scores, ylim=range(scores, predicted_scores), pch=19, xlab="Year", ylab="Scores", main="Random Forest Predictions vs Actual Scores")
lines(years_pred, predicted_scores, col='blue', type='o')
```

![](Figs/unnamed-chunk-3-1.png)<!-- -->

``` r
# Print the predicted score for 2024
predicted_score_2024 <- predicted_scores[length(predicted_scores)]
print(paste("Predicted score for 2024: ", predicted_score_2024))
```

    ## [1] "Predicted score for 2024:  14.5384070000002"
