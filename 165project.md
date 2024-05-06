165 Project
================
2024-04-25

## R Markdown

``` r
library(readxl)
library(ggplot2)
library(MASS)
library(stats)

scores_data <- read_excel("/Users/setaranusratty/Desktop/All Scores 165.xlsx")

ggplot(scores_data, aes(x=Score)) +
  geom_histogram(aes(y=..density..), bins=12, fill="blue", color="black") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Distribution of All Scores", x="Score", y="Frequency")
```

    ## Warning: The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(density)` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](165project_files/figure-gfm/cars-1.png)<!-- -->

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
ggplot(top_scores, aes(x = Year, y = Score, group = Place, color = factor(Place))) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Trend of Scores for Top 2 Places Over the Years",
       x = "Year",
       y = "Score",
       color = "Medal Position")
```

![](165project_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# 2. Athlete Performance Trends: Check for any repeating names and their scores
athlete_performance <- top_scores %>%
  group_by(Gymnast) %>%
  summarize(AverageScore = mean(Score), Appearances = n()) %>%
  filter(Appearances > 1)

print(athlete_performance)
```

    ## # A tibble: 1 × 3
    ##   Gymnast     AverageScore Appearances
    ##   <chr>              <dbl>       <int>
    ## 1 Aly Raisman         15.3           2

``` r
# 3. Display data for review
print(top_scores)
```

    ## # A tibble: 13 × 5
    ##     Year Gymnast            Score Place Team 
    ##    <dbl> <chr>              <dbl> <dbl> <chr>
    ##  1  2008 Sandra Izbasa       15.5     1 ROM  
    ##  2  2008 Shawn Johnson       15.5     2 USA  
    ##  3  2008 Anastasia Liukin    15.2     3 USA  
    ##  4  2012 Aly Raisman         15.1     1 USA  
    ##  5  2012 Catalina Ponor      15.1     2 ROM  
    ##  6  2012 Aliya Mustafina     14.6     3 RUS  
    ##  7  2016 Simone Biles        15.9     1 USA  
    ##  8  2016 Aly Raisman         15.4     2 USA  
    ##  9  2016 Amy Tinkler         14.9     3 BRI  
    ## 10  2020 Jade Carey          14.0     1 USA  
    ## 11  2020 Vanessa Ferrari     13.7     2 ITA  
    ## 12  2020 Angelina Melnikova  13.7     3 ROC  
    ## 13  2020 Mai Murakami        13.7     3 JAP

``` r
#word champion 2023 qualifier scores

# Updated Data
gymnasts <- c("Simone Biles", "Rebeca Andrade", "Flavia Saraiva", "Sabrina Maneca-Voinea", "Shilese Jones", "Naomi Visser", "Zhou Yaqin", "Alice Kinsella")
countries <- c("USA", "BRA", "BRA", "ROU", "USA", "NED", "CHN", "GBR")
scores <- c(14.633, 14.500, 13.966, 13.766, 13.666, 13.300, 13.300, 12.666)

data <- data.frame(gymnasts, countries, scores)

ggplot(data, aes(x=scores, y=reorder(gymnasts, scores), color=countries)) + 
  geom_point(size=4) +
  labs(title="2023 World Artistic Gymnastics Championships: Women's Floor Qualification",
       x="Score",
       y="Gymnast") +
  theme_minimal() +
  scale_color_brewer(palette="Set1")
```

![](165project_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

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

![](165project_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# Print the predicted score for 2024
predicted_score_2024 <- predicted_scores[length(predicted_scores)]
print(paste("Predicted score for 2024: ", predicted_score_2024))
```

    ## [1] "Predicted score for 2024:  14.4876960000002"
