# mixed_effects_model
This repository contains the analysis and visualization of data from a sleep study, conducted on a sleep study dataset.

The sleep study experiment examines the effects of sleep deprivation on reaction times. It was conducted to understand how increasing sleep deprivation impacts cognitive performance.

The study involved 18 individuals labed as *Subject*. 

Each participant was subjected to a controlled sleep deprivation regimen. Reaction times were measured for 10 consecutive days *Days*, starting from *baseline* (Day 0, fully rested).

The response variable *Reaction* is the reaction time (measured in milliseconds). The predictor variable *Days* is the number of days of sleep deprivation.


The aim is to assess the relationship between sleep deprivation (number of days) and reaction time, considering both population-level trends and individual differences.


Each individual reacts differently to sleep deprivation
There is a clear population-level trend of react times increasing with more days of sleep deprivation.


model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

Days is a fixed effect that captures the overall trend across all subjects.
(Days | Subject) specifies random intercepts and slopes for each subject, allowing for individual differences.

Fixed Effects: The overall effect of sleep deprivation on reaction time
Random Effects: Variations in baseline reaction time (random intercept) and response to sleep deprivation (random slope) for each subject.


Mixed Effects Models are particularly useful when the data has both:

Population-level trends (captured by fixed effects).
Individual/group-level variations (captured by random effects).
