# import the adequate libraries
library(tidyverse)
library(broom)
library(lme4)

data(sleepstudy)
?sleepstudy



# Here I performed descriptive analysis to see the first five rows of my data
head(sleepstudy)


#   Reaction Days Subject
# 1 249.5600    0     308
# 2 258.7047    1     308
# 3 250.8006    2     308
# 4 321.4398    3     308
# 5 356.8519    4     308

# I also checked the data for the number of observation which is 180 and with 3 variables or columns
str(sleepstudy)
# > str(sleepstudy)
# 'data.frame':   180 obs. of  3 variables:
#  $ Reaction: num  250 259 251 321 357 ...
#  $ Days    : num  0 1 2 3 4 5 6 7 8 9 ...
#  $ Subject : Factor w/ 18 levels "308","309","310",..: 1 1 1 1 1 1 1 1 1 1 ...


# ?sleepstudy we explored the variables in the datasets
# Reaction: Reaction time (ms) is the dependent variable.
# Days: Number of days of sleep deprivation (independent variable).
# Subject: Random effect variable indicating the individual.


# We also check summary() to check for the summary statistics of the datasets and we found that
# the means of the Reaction is 298.5 millisecs
# the means of the Days is 4.5days


summary(sleepstudy)

# > summary(sleepstudy)
#     Reaction          Days        Subject   
#  Min.   :194.3   Min.   :0.0   308    : 10
#  1st Qu.:255.4   1st Qu.:2.0   309    : 10
#  Median :288.7   Median :4.5   310    : 10
#  Mean   :298.5   Mean   :4.5   330    : 10
#  3rd Qu.:336.8   3rd Qu.:7.0   331    : 10  
#  Max.   :466.4   Max.   :9.0   332    : 10
#                                (Other):120



# Here we plot visualizations to further understand the data


# We plotted the graph of Reaction time to see the distribution more clearly,we plotted the Histogram
ggplot(sleepstudy, aes(x = Reaction)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Reaction Times", x = "Reaction Time (ms)", y = "Count")
```

# We also found the factor levels of days, since it is a categorical variable and saved it into a variable x
# Then we plotted a boxplot of reaction times by days where x is the days of sleep deprivation.

x = factor(sleepstudy$Days)

ggplot(sleepstudy, aes(x, y = Reaction)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  theme_minimal() +
  labs(title = "Boxplot of Reaction Times by Days", x = "Days of Sleep Deprivation", y = "Reaction Time (ms)")



ggplot(sleepstudy, aes(, y = Reaction)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  theme_minimal() +
  labs(title = "Boxplot of Reaction Times by Days", x = "Days of Sleep Deprivation", y = "Reaction Time (ms)")


ggplot(sleepstudy, aes(x = Days, y = Reaction, group = Subject, color = Subject)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Reaction Times by Subject Over Days", x = "Days", y = "Reaction Time (ms)")


str(sleepstudy)


# Since sleep deprivation begins from day 2, we can account for that in our visualization.


# Here we added a new column to sleepstudy, using a conditional, saying if the Days is greater than 2, then Deprivation started
# If not set to 'Non-Deprivation'
sleepstudy$Deprivation <- ifelse(sleepstudy$Days > 2, "Deprived", "Non-Deprived")



# We group the deprivation column and we performed descriptive analysis on the data for each of 
# deprived or non-deprived
sleepstudy %>%
  group_by(Deprivation) %>%
  summarise(
    Mean_Reaction = mean(Reaction),
    SD_Reaction = sd(Reaction),
    Median_Reaction = median(Reaction),
    Min_Reaction = min(Reaction),
    Max_Reaction = max(Reaction)
  )



#   Deprivation  Mean_Reaction SD_Reaction Median_Reaction Min_Reaction
#   <chr>                <dbl>       <dbl>           <dbl>        <dbl>
# 1 Deprived              314.        57.5            313.         205.
# 2 Non-Deprived          262.        31.4            267.         194.


# We used a boxplot to plot the Reaction Times against the two categories of deprivation - Deprived and Non-deprived
ggplot(sleepstudy, aes(x = Deprivation, y = Reaction, fill = Deprivation)) +
  geom_boxplot() +
  labs(title = "Reaction Time: Deprived vs Non-Deprived", 
       x = "Sleep Deprivation Status", 
       y = "Reaction Time (ms)") +
  theme_minimal()


# Bar plot for mean Reaction time by deprivation status
ggplot(sleepstudy, aes(x = Deprivation, y = Reaction, fill = Deprivation)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(title = "Reaction Time: Deprived vs Non-Deprived", 
       x = "Sleep Deprivation Status", 
       y = "Mean Reaction Time (ms)") +
  theme_minimal()


require(lattice)
xyplot(Reaction ~ Days | Subject, data = sleepstudy,
        # Grid, points, and regression lines
       type = c("g", "p", "r"), 
        # Regression line color
       col.line = "blue", 
         # Point style     
       pch = 19,  
       # Point size            
       cex = 1.2,     
        # Point color          
       col = "darkred",        
       # Sort by intercept
       index = function(x, y) coef(lm(y ~ x))[1], 
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)",
       main = "Reaction Time vs Sleep Deprivation by Subject", 
       aspect = "xy")



ggplot(sleepstudy, aes(x = Days, y = Reaction)) +
  geom_point() +
  scale_x_continuous(breaks = 0:9) +
  facet_wrap(~Subject)


experiment_started <- sleepstudy %>%
                    filter(Days >= 2L) %>%
                    mutate(days_deprived = Days - 2L)

head(experiment_started)

experiment_started %>% count(days_deprived, Days)


ggplot(experiment_started, aes(x = days_deprived, y = Reaction)) +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")


complete_pooling_mdl <- lm(Reaction ~ days_deprived, experiment_started)
summary(complete_pooling_mdl)


coef(complete_pooling_mdl)


ggplot(experiment_started, aes(x = days_deprived, y = Reaction)) +
  geom_abline(intercept = coef(complete_pooling_mdl)[1], slope = coef(complete_pooling_mdl)[2], color = 'red') +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = 'Reaction Time', x = 'Days deprived of sleep (0 = baseline)')


# NO POOL
experiment_started %>% summary()
#    Reaction          Days         Subject   days_deprived 
#  Min.   :203.0   Min.   :2.00   308    : 8   Min.   :0.00
#  1st Qu.:265.2   1st Qu.:3.75   309    : 8   1st Qu.:1.75  
#  Median :303.2   Median :5.50   310    : 8   Median :3.50
#  Mean   :308.0   Mean   :5.50   330    : 8   Mean   :3.50
#  3rd Qu.:347.7   3rd Qu.:7.25   331    : 8   3rd Qu.:5.25  
#  Max.   :466.4   Max.   :9.00   332    : 8   Max.   :7.00
#                                 (Other):96



no_pool_model <- lm(Reaction ~ days_deprived + Subject + days_deprived:Subject, data = experiment_started)


summary(no_pool_model)


all_intercepts <- c(coef(no_pool_model)["(Intercept)"],
                    coef(no_pool_model)[3:19] + coef(no_pool_model)["(Intercept)"])

all_slopes  <- c(coef(no_pool_model)["days_deprived"],
                 coef(no_pool_model)[20:36] + coef(no_pool_model)["days_deprived"])

ids <- experiment_started %>% pull(Subject) %>% levels() %>% factor()

# make a tibble with the data extracted above
np_coef <- tibble(Subject = ids,
                  intercept = all_intercepts,
                  slope = all_slopes)

np_coef


ggplot(experiment_started, aes(x = days_deprived, y = Reaction)) +
  geom_abline(data = np_coef,
              mapping = aes(intercept = intercept,
                            slope = slope),
              color = 'blue') +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")



?sleepstudy

final_model1 <- lmer(Reaction ~ days_deprived + (days_deprived | Subject), experiment_started)


final_model2 <- lmer(Reaction ~ days_deprived + (1 | Subject) + (0 + days_deprived | Subject), experiment_started)

anova(final_model1, final_model2)



newdata <- crossing(
  Subject = experiment_started %>% pull(Subject) %>% levels() %>% factor(),
  days_deprived = 0:7)

head(newdata, 17)


newdata2 <- newdata %>%
  mutate(Reaction = predict(mixed_effect_model, newdata))

  ggplot(experiment_started, aes(x = days_deprived, y = Reaction)) +
  geom_line(data = newdata2,
            color = 'red') +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")



fixef(mixed_effect_model)
confint(mixed_effect_model)
