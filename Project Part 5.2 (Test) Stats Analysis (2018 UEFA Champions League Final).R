## Check data (Real Madrid)
my_data <- read.csv('rmadrid2.csv')
my_data$X <- NULL
head(my_data)
#     compound neg neu   pos   period
#1:   0.0000   0   1.000 0.000 pre-event
#2:   0.0772   0   0.860 0.140 pre-event
#3:   0.8360   0   0.639 0.361 pre-event
#4:   0.0000   0   1.000 0.000 pre-event
#5:   0.0000   0   1.000 0.000 pre-event
#6:   0.4215   0   0.741 0.259 pre-event
levels(my_data$period)
#[1] "post-event" "pre-event" 
library("magrittr")
library("dplyr")
my_data$period <- ordered(my_data$period, levels = c('pre-event','post-event'))
levels(my_data$period)
# [1] "pre-event"  "post-event"
my_data %>% group_by(period) %>% summarise_all(funs(mean), na.rm = TRUE)
# A tibble: 2 x 5
#   period     compound  neg        neu       pos
#   <ord>      <dbl>     <dbl>      <dbl>     <dbl>
# 1 pre-event  0.2483620 0.04390328 0.7599803 0.1960289
# 2 post-event 0.1627922 0.08878334 0.7138247 0.1972127
my_data %>% group_by(period) %>% summarise_all(funs(sd), na.rm = TRUE)
# A tibble: 2 x 5
#   period     compound  neg       neu       pos
#   <ord>      <dbl>     <dbl>     <dbl>     <dbl>
# 1 pre-event  0.3974554 0.1181951 0.2497544 0.2395542
# 2 post-event 0.4506494 0.1816218 0.2703886 0.2499381
my_data %>% group_by(period) %>% summarise_all(funs(sum), na.rm = TRUE)
# A tibble: 2 x 5
#    period    compound  neg      neu      pos
#    <ord>     <dbl>     <dbl>    <dbl>    <dbl>
# 1 pre-event  2768.243  489.346  8470.74  2184.938
# 2 post-event 11596.017 6324.215 50847.16 14047.857
my_data %>% group_by(period) %>% summarise_all(funs(min, max), na.rm = TRUE)
# A tibble: 2 x 9
#   period     compound_min neg_min neu_min pos_min compound_max neg_max neu_max pos_max
#   <ord>      <dbl>        <dbl>   <dbl>   <dbl>   <dbl>        <dbl>   <dbl>   <dbl>
# 1 pre-event  -0.9552       0       0       0      0.9934       1       1       1
# 2 post-event -0.9983       0       0       0      0.9951       1       1       1

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library("ggplot2")
library("ggpubr")
par(mfrow=c(2,2))
boxplot(pos ~ period, data = my_data,
        ylab = "positive",
        frame = FALSE, col = c("#00AFBB", "#E7B800"))
boxplot(neg ~ period, data = my_data,
        ylab = "negative",
        frame = FALSE, col = c("#00AFBB", "#E7B800"))
boxplot(neu ~ period, data = my_data,
         ylab = "neutral",
        frame = FALSE, col = c("#00AFBB", "#E7B800"))
boxplot(compound ~ period, data = my_data,
        ylab = "overall compound",
        frame = FALSE, col = c("#00AFBB", "#E7B800"))

## one-way ANOVA 
res.aov <- aov(compound ~ period, data = my_data)
summary(res.aov)
#               Df   Sum   Sq Mean Sq F  value Pr(>F)    
#  period       1    71    70.57   358.3 <2e-16 ***
#  Residuals   82376 16227 0.20                   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
res.aov <- aov(pos ~ period, data = my_data)
summary(res.aov)
#                Df    Sum Sq Mean   Sq F    value Pr(>F)
# period          1      0 0.01351   0.219   0.64
# Residuals   82376   5089 0.06178               
res.aov <- aov(neg ~ period, data = my_data)
summary(res.aov)
#                Df     Sum Sq Mean   Sq F  value Pr(>F)    
# period          1    19.4   19.41   638.3 <2e-16 ***
#  Residuals   82376 2505.4    0.03                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
res.aov <- aov(neu ~ period, data = my_data)
summary(res.aov)
#                Df   Sum  Sq Mean    Sq F value Pr(>F)    
# period          1     21  20.532   286.5 <2e-16 ***
# Residuals   82376   5903   0.072                   
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Check data (Liverpool)
my_data <- read.csv('liverp2.CSV')
my_data$X <- NULL
head(my_data)
#     compound neg   neu   pos   period
# 1   0.0000   0.000 1.000 0.000 pre-event
# 2   0.7184   0.000 0.611 0.389 pre-event
# 3  -0.2617   0.128 0.872 0.000 pre-event
# 4   0.2120   0.000 0.813 0.187 pre-event
# 5   0.6369   0.000 0.625 0.375 pre-event
# 6   0.0000   0.000 1.000 0.000 pre-event
my_data$period <- ordered(my_data$period, levels = c('pre-event','post-event'))
levels(my_data$period)
# [1] "pre-event"  "post-event"
my_data %>% group_by(period) %>% summarise_all(funs(mean), na.rm = TRUE)
# A tibble: 2 x 5
#   period    compound  neg   neu   pos
#   <ord>         <dbl> <dbl> <dbl>  <dbl>
# 1 pre-event     0.199 0.0632 0.755 0.181
# 2 post-event    0.122 0.0924 0.741 0.165
my_data %>% group_by(period) %>% summarise_all(funs(sd), na.rm = TRUE)
# A tibble: 2 x 5
#   period     compound neg   neu   pos
#   <ord>         <dbl> <dbl> <dbl> <dbl>
# 1 pre-event     0.420 0.143 0.243 0.226
# 2 post-event    0.445 0.171 0.241 0.212
my_data %>% group_by(period) %>% summarise_all(funs(sum), na.rm = TRUE)
# A tibble: 2 x 5
#   period     compound  neg    neu    pos
#   <ord>         <dbl>  <dbl>  <dbl>  <dbl>
# 1 pre-event     7287.  2310.  27613.  6611.
# 2 post-event   21820. 16559. 132780. 29622.
my_data %>% group_by(period) %>% summarise_all(funs(min, max), na.rm = TRUE)
# A tibble: 2 x 9
#   period     compound_min neg_min neu_min pos_min compound_max neg_max neu_max pos_max
#   <ord>             <dbl>   <dbl>   <dbl>   <dbl>        <dbl>   <dbl>   <dbl>   <dbl>
# 1 pre-event        -0.980       0       0       0        0.988       1       1       1
# 2 post-event       -0.998       0       0       0        0.994       1       1       1
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library("ggplot2")
library("ggpubr")
par(mfrow=c(2,2))
boxplot(pos ~ period, data = my_data,
        ylab = "positive",
        frame = FALSE, col = c("#00AFBB", "#E7B800"))
boxplot(neg ~ period, data = my_data,
        ylab = "negative",
        frame = FALSE, col = c("#00AFBB", "#E7B800"))
boxplot(neu ~ period, data = my_data,
        ylab = "neutral",
        frame = FALSE, col = c("#00AFBB", "#E7B800"))
boxplot(compound ~ period, data = my_data,
        ylab = "overall compound",
        frame = FALSE, col = c("#00AFBB", "#E7B800"))

res.aov <- aov(compound ~ period, data = my_data)
summary(res.aov)
#                 Df Sum Sq Mean Sq  F value Pr(>F)    
# period           1    183  182.88   941.9  <2e-16 ***
# Residuals   215840  41909    0.19                   
# ---
# Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
res.aov <- aov(pos~ period, data = my_data)
summary(res.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)    
# period           1      7   7.385   160.1 <2e-16 ***
# Residuals   215840   9954   0.046                   
# ---
# Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
res.aov <- aov(neg ~ period, data = my_data)
summary(res.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)    
# period           1     26  25.869     934 <2e-16 ***
# Residuals   215840   5978   0.028                   
# ---
# Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
res.aov <- aov(neu ~ period, data = my_data)
summary(res.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)    
# period           1      7   6.536     112 <2e-16 ***
# Residuals   215840  12599   0.058                   
# ---
# Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1

