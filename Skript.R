library("skimr")
library("palmerpenguins")
library("naniar")
library("tidyverse")

View(penguins_raw)
head(penguins_raw)
data <- penguins_raw
glimpse(data)

skim(penguins_raw)
# not every essential variable is complete, but the completion rates are relatively high

miss_var_summary(data)
vis_miss(data)

ggplot(data, aes(x = `Culmen Depth (mm)`)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Bill Depth",
    x = "Bill Depth in mm", y = "Count"
  )

ggplot(data, aes(x = `Culmen Depth (mm)`, y = `Body Mass (g)`, colour = `Species`)) +
  geom_point(alpha = 0.6) +
  labs(title = "Relationship between bill depth and weight")

summary(data$`Body Mass (g)`)
summary(data$`Flipper Length (mm)`)
summary(data$`Culmen Depth (mm)`)
