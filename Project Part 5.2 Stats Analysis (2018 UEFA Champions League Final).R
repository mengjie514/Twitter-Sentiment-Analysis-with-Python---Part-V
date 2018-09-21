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