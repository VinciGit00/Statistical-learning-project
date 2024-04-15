setwd("~/Github/Statistical-learning-project/Dataset")

df <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

correlation<- cor(model.matrix(~.-1,data=df));
## High correlation: test level (-0.81)

correlation_high <- correlation[correlation[, "Sleep.Duration"] > 0.8 | correlation[, "Sleep.Duration"] < -0.8, ];
correlation_high <- correlation_high[ "Sleep.Duration"];

#High correlation: Stress.Level and Quality.of.Sleep, we decide just to remove Quality.of.Sleep
df <- subset(df, select = -c(Quality.of.Sleep, Person.ID));

write.csv(df, "Sleep_health_and_lifestyle_dataset_adjusted.csv", row.names = FALSE)