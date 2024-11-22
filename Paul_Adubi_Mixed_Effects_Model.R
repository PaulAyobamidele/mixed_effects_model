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


# We also found the factor levels of days, since it is a categorical variable and saved it into a variable x
# Then we plotted a boxplot of reaction times by days where x is the days of sleep deprivation.

x <- factor(sleepstudy$Days)

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



ggplot(sleepstudy, aes(x = Reaction)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(
    title = "Distribution of Reaction Times",
    x = "Reaction Time (ms)",
    y = "Frequency"
  ) +
  theme_minimal()






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
  labs(
    title = "Reaction Time: Deprived vs Non-Deprived",
    x = "Sleep Deprivation Status",
    y = "Reaction Time (ms)"
  ) +
  theme_minimal()



# Here we plotted a bar plot for mean Reaction time against deprivation status
ggplot(sleepstudy, aes(x = Deprivation, y = Reaction, fill = Deprivation)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(
    title = "Reaction Time: Deprived vs Non-Deprived",
    x = "Sleep Deprivation Status",
    y = "Mean Reaction Time (ms)"
  ) +
  theme_minimal()


#
# Here we loaded the lattice package, used for creating advanced statistical grpahics
require(lattice)

# We plotted the response variable (REaction) on the y-axes and the
# explanatory variable (Days, sleep derivation)
xyplot(Reaction ~ Days | Subject,
  data = sleepstudy,
  # We defined components for the grid, points, and regression lines
  type = c("g", "p", "r"),
  # WE set the regression line color
  col.line = "blue",
  # We set our point style
  pch = 19,
  # We set the point size
  cex = 1.2,
  # Point color
  col = "darkred",
  # We then sorted by intercept of the plotted regression line
  index = function(x, y) coef(lm(y ~ x))[1],
  xlab = "Days of sleep deprivation",
  ylab = "Average reaction time (ms)",
  main = "Reaction Time vs Sleep Deprivation by Subject",
  aspect = "xy"
)

?sleepstudy()
# we experimented with the ggplot to plot the same graph
ggplot(sleepstudy, aes(x = Days, y = Reaction)) +
  geom_point() +
  scale_x_continuous(breaks = 0:9) +
  facet_wrap(~Subject)

# We created a new variable 'experiment_started'
# We piped the sleepstudy dataset into a filter
# The filter function is used to select the Days value of 2 or more
# Since Day 0, Day 1 were adaptation and training (T1/T2) respectively,
# day 2 was baseline (B); sleep deprivation started after day 2.
# Now, we says a new column called 'days_deprived'
experiment_started <- sleepstudy %>%
  filter(Days >= 2L) %>%
  mutate(days_deprived = Days - 2L)


# we printed the first six observation of the new tibble
head(experiment_started)

# > head(experiment_started)
#   Reaction Days Subject days_deprived
# 1 250.8006    2     308             0
# 2 321.4398    3     308             1
# 3 356.8519    4     308             2
# 4 414.6901    5     308             3
# 5 382.2038    6     308             4
# 6 290.1486    7     308             5


# We then performed a descriptive statistics of the new dataset
experiment_started %>% count(days_deprived, Days)
#   days_deprived Days  n
# 1             0    2 18
# 2             1    3 18
# 3             2    4 18
# 4             3    5 18
# 5             4    6 18
# 6             5    7 18
# 7             6    8 18
# 8             7    9 18


# Here we plotted the scatter plot by facets using the filtered data we called "experiment_started"
require(lattice)
xyplot(Reaction ~ Days | Subject,
  data = experiment_started,
  # We defined components for the grid, points, and regression lines
  type = c("g", "p", "r"),
  # WE set the regression line color
  col.line = "blue",
  # We set our point style
  pch = 19,
  # We set the point size
  cex = 1.2,
  # Point color
  col = "darkred",
  # We then sorted by intercept of the plotted regression line
  index = function(x, y) coef(lm(y ~ x))[1],
  xlab = "Days of sleep deprivation",
  ylab = "Average reaction time (ms)",
  main = "Reaction Time vs Sleep Deprivation by Subject with 2 as baseline",
  aspect = "xy"
)


# We plotted a facet_wrap histogram by days, to visualize the reactions of each subject per day
# using DAY2 as Baseline
ggplot(experiment_started, aes(x = Reaction, fill = factor(Days))) +
  geom_histogram(binwidth = 20, alpha = 0.8, position = "dodge") +
  facet_wrap(~Days, scales = "free_y") +
  labs(
    title = "Histogram of Reaction Times by Day",
    x = "Reaction Time",
    y = "Count",
    fill = "Day"
  ) +
  theme_minimal()

# Here, we plotted the boxplot by days, wrapping it in facets by the days.
# We used Day 2 as our new baseline
ggplot(experiment_started, aes(x = factor(Days), y = Reaction, fill = factor(Days))) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~days_deprived, scales = "free") +
  labs(title = "Boxplot of Reaction Times by Day and Deprivation Level", x = "Day", y = "Reaction Time", fill = "Day") +
  theme_minimal()


# We performed the normal linear regression, plotting the days_deprived
complete_pooling_mdl <- lm(Reaction ~ days_deprived, experiment_started)
summary(complete_pooling_mdl)



# Call:
# lm(formula = Reaction ~ days_deprived, data = experiment_started)

# Residuals:
#      Min       1Q   Median       3Q      Max
# -112.284  -26.732    2.143   27.734  140.453

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept)    267.967      7.737  34.633  < 2e-16 ***
# days_deprived   11.435      1.850   6.183 6.32e-09 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 50.85 on 142 degrees of freedom
# Multiple R-squared:  0.2121,    Adjusted R-squared:  0.2066
# F-statistic: 38.23 on 1 and 142 DF,  p-value: 6.316e-09


# (Intercept): 267.967
# This is the estimated value of Reaction when days_deprived is 0.
# In other words, when there is no deprivation, the expected reaction is approximately 268 units.


# days_deprived: 11.435
# This is the slope coefficient, representing the change in Reaction for each additional day of deprivation.
# For every day of deprivation, the model predicts that the Reaction will increase by approximately 11.435 units.



# The residual standard error is 50.85, which is a measure of the typical size of the residuals (errors) in the model.
# It indicates that, on average, the observed reactions deviate from the predicted values by 50.85 units.


# General Summary

# The model shows a positive relationship between days_deprived and Reaction. For every additional day of deprivation,
# the expected Reaction increases by 11.435 units.
# Both the intercept and the slope are statistically
# significant, meaning the model is a good fit for the data.
# The model explains about 21.2% of the variance in Reaction,
# suggesting that while days_deprived is an important predictor,
# other factors likely contribute to the variability in Reaction.




# I extracted the coefficient of the model
coef(complete_pooling_mdl)
#  (Intercept) days_deprived
#     267.96742      11.43543



# Plotted a one size fits all plot to visualize the regression across the days
complete <- ggplot(experiment_started, aes(x = days_deprived, y = Reaction)) +
  geom_abline(intercept = coef(complete_pooling_mdl)[1], slope = coef(complete_pooling_mdl)[2], color = "red") +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")

ggsave("complete.png", plot = complete, width = 8, height = 6, dpi = 300)
# We see a bad fit.


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


all_intercepts <- c(
  coef(no_pool_model)["(Intercept)"],
  coef(no_pool_model)[3:19] + coef(no_pool_model)["(Intercept)"]
)

all_slopes <- c(
  coef(no_pool_model)["days_deprived"],
  coef(no_pool_model)[20:36] + coef(no_pool_model)["days_deprived"]
)

ids <- experiment_started %>%
  pull(Subject) %>%
  levels() %>%
  factor()

# make a tibble with the data extracted above
np_coef <- tibble(
  Subject = ids,
  intercept = all_intercepts,
  slope = all_slopes
)

np_coef


nopool <- ggplot(experiment_started, aes(x = days_deprived, y = Reaction)) +
  geom_abline(
    data = np_coef,
    mapping = aes(
      intercept = intercept,
      slope = slope
    ),
    color = "blue"
  ) +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")

ggsave("nopool.png", plot = complete, width = 8, height = 6, dpi = 300)



# Mixed Effects Model

final_model1 <- lmer(Reaction ~ days_deprived + (days_deprived | Subject), experiment_started)


summary(final_model1)


# summary(final_model1)
# Linear mixed model fit by REML ['lmerMod']
# Formula: Reaction ~ days_deprived + (days_deprived | Subject)
#    Data: experiment_started

# REML criterion at convergence: 1404.1

# Scaled residuals:
#     Min      1Q  Median      3Q     Max
# -4.0157 -0.3541  0.0069  0.4681  5.0732

# Random effects:
#  Groups   Name          Variance Std.Dev. Corr
#  Subject  (Intercept)   958.35   30.957
#           days_deprived  45.78    6.766   0.18
#  Residual               651.60   25.526
# Number of obs: 144, groups:  Subject, 18

# Fixed effects:
#               Estimate Std. Error t value
# (Intercept)    267.967      8.266  32.418
# days_deprived   11.435      1.845   6.197

# Correlation of Fixed Effects:
#             (Intr)
# days_deprvd -0.062


# Random Effects Summary
# (Intercept):
# Variance = 958.35; Std.Dev. = 30.957.
# This indicates substantial variability in baseline reaction times (intercepts) across subjects.
# days_deprived:

# Variance = 45.78; Std.Dev. = 6.766.
# This indicates moderate variability in how sensitive different subjects are to days_deprived.
# Residual:

# Variance = 651.60; Std.Dev. = 25.526.
# The residual variability reflects the variability in Reaction not explained by the fixed or random effects.



# Fixed Effects Summary:
# (Intercept):

# Estimate = 267.967.
# This is the average baseline Reaction when days_deprived = 0, across all subjects.
# A t-value of 32.418 (very large) indicates the intercept is highly significant.
# days_deprived:

# Estimate = 11.435.
# For each additional day of deprivation, the Reaction increases by 11.435 units on average.
# The standard error (SE) = 1.845 is relatively small, leading to a high t-value of 6.197, indicating this effect is highly significant.




mixed <- ggplot(experiment_started, aes(x = days_deprived, y = Reaction)) +
  geom_point() + # Plot the raw data points
  geom_abline(aes(intercept = fixef(final_model1)[1], slope = fixef(final_model1)[2]),
    color = "red"
  ) + # Add the fixed effect regression line
  # Add random effect lines for each subject
  geom_abline(
    aes(
      intercept = fixef(final_model1)[1] + ranef(final_model1)$Subject[as.character(Subject), "(Intercept)"],
      slope = fixef(final_model1)[2] + ranef(final_model1)$Subject[as.character(Subject), "days_deprived"]
    ),
    color = "blue", alpha = 0.3
  ) + # Random effect lines (with transparency)
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)") +
  theme_minimal()

# Save the plot as an image
ggsave("mixed.png", plot = mixed, width = 8, height = 6, dpi = 300)

mixed
final_model2 <- lmer(Reaction ~ days_deprived + (1 | Subject) + (0 + days_deprived | Subject), experiment_started)

anova(final_model1, final_model2)



newdata <- crossing(
  Subject = experiment_started %>% pull(Subject) %>% levels() %>% factor(),
  days_deprived = 0:7
)

head(newdata, 17)


newdata2 <- newdata %>%
  mutate(Reaction = predict(final_model1, newdata))

mixed <- ggplot(experiment_started, aes(x = days_deprived, y = Reaction)) +
  geom_line(
    data = newdata2,
    color = "red"
  ) +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")

ggsave("mixed.png", plot = complete, width = 8, height = 6, dpi = 300)

mixed

fixef(mixed_effect_model)
confint(mixed_effect_model)
