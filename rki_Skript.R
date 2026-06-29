# This is the exploration of a alternative messier data set
library(lintr)
library(readr)
library(tidyverse)
library(visdat)
library(tidyr)

rki_data <- read_tsv("GBE_Indikatoren_nichtuebertragbarer_Erkrankungen.tsv")
View(rki_data)
glimpse(rki_data)

# Number of observations for depressive symptoms (2040201) and
# for diagnosed depression (2040202)
rki_data %>%
  filter(Indikator_ID == 2040201) %>%
  nrow()
rki_data %>%
  filter(Indikator_ID == 2040202) %>%
  nrow()

# number of missing values in Fälle
rki_data %>%
  filter(is.na(Fälle)) %>%
  nrow()

lint(filename = "rki_Skript.R")

# Visualise column types
vis_dat(rki_data, warn_large_data = FALSE)

# Region ID is read as character, but is number
# Check whether there are entries that are not numbers
unique(rki_data$Region_ID)

# There seems to be some international data in there
# Let's look at that more closely
rki_data %>%
  mutate(Region_ID = as.numeric(Region_ID)) %>%
  filter(is.na(Region_ID)) %>%
  View()

# There seems to be data in there about policies on
# tabacco in different European countries. This is
# unconnected to our research questions, so we
# can safely drop the columns and proceed with
# converting the column

rki_data <- rki_data %>%
  mutate(Region_ID = as.numeric(Region_ID)) %>%
  filter(!is.na(Region_ID))

# Zeitraum ISO contains both start and end date and is therefore read as string
# For time series, it may be more suited to have the start and end date as two
# seperate columns that are read as Dates
rki_data <- rki_data %>%
  separate(
    Zeitraum_ISO,
    into = c("Start_Beobachtungszeitraum", "Ende_Beobachtungszeitraum"),
    sep = "--"
  ) %>%
  mutate(Start_Beobachtungszeitraum = as.Date(Start_Beobachtungszeitraum)) %>%
  mutate(Ende_Beobachtungszeitraum = as.Date(Ende_Beobachtungszeitraum))

# Visualise column types
vis_dat(rki_data, warn_large_data = FALSE)
# The conversion did not create NAs

#when the variable Zetraum_Name ist converted into an integer, coercions happen
#lets look at the different manifestations of the variable
year_meanifestations <- rki_data %>% group_by(Zeitraum_Name) %>% summarise()
#some observation appear to have been collected over two years (e.g. 2024/25)
#this does not allign with ideal tidy data, but I don't see a practical alternative



# Convert from dbl to int where reasonable
rki_data <- rki_data %>%
  mutate(Geschlecht_ID = as.integer(Geschlecht_ID)) %>%
  mutate(Kennzahl_ID = as.integer(Kennzahl_ID)) %>%
  mutate(Bildung_Casmin_ID = as.integer(Bildung_Casmin_ID)) %>%
  mutate(GISD_ID = as.integer(GISD_ID)) %>%
  mutate(Standardisierung_ID = as.integer(Standardisierung_ID)) %>%
  mutate(Unsicherheit = as.integer(Unsicherheit)) %>%
  mutate(Region_ID = as.integer(Region_ID)) %>%
  mutate(Berufliche_Qualifikation_ID = as.integer(Berufliche_Qualifikation_ID))

# Add unique identifier per row
rki_data <- rki_data %>%
  mutate(ID = row_number())

#distribution of variable Bildung_Casmin_Name
rki_data %>% 
  group_by(Bildung_Casmin_Name) %>%
  summarise(
    n = n()
  )

#isolate and examine observations 
#where distinct education level and depressive symptoms are given 
#and age-standardized
bildung_symptom <- rki_data %>%
  filter(Indikator_ID == 2040202) %>%
  filter(!is.na(Bildung_Casmin_Name)) %>%
  filter(Bildung_Casmin_Name != "Gesamt") %>%
  filter(Standardisierung_ID == 3)

#function to calculate weighted average
#'@description value should contain the values to be summed up, sample_size
#'indicates the weight of the value with the same position, the weight is 
#'#'calculated by deviding the individual sample_size value with the summ of 
#'sample_size
#'@param name value a numeric vector
#'@param name sample_size a numeric vector
weighted_average <- function(value, sample_size){
  if(!is.vector(value) | !is.vector(sample_size)){
    stop("At least one of the inputs isn't a vector")
  }
  s <- sum(sample_size)
  result <- 0
  for(x in 1:length(value)){
    result <- result + value[x] * sample_size[x] / s
  }
  return(result)
}
#function was corrected, tested on examplary vectors and stress tested

#graphic for proposal: visualization of depressive symptoms among different 
#educations levels for all genders in 2023
bild_dep_23 <- bildung_symptom %>%
  filter(Zeitraum_Name == "2023") %>%
  filter(Geschlecht_Name == "Gesamt") %>%
  select(Wert, Bildung_Casmin_Name)

ggplot(data = bild_dep_23,
       mapping = aes(
       x = Bildung_Casmin_Name,
       y = Wert)
       ) +
  geom_col() +
  labs(
    title = "prevalence of depressive symptoms among different educational levels",
    subtitle = "in Germany in the year 2023",
    x = ("Casmin education level"),
    y = ("prevalence of depressive symptoms")
  )

#second graphic for proposal: depression over time
#only the observations regarding all genders and ages, for simplification purposes

dep_time <- rki_data %>%
  filter(Geschlecht_Name == "Gesamt") %>%
  filter(Alter_Name == "Alle Altersgruppen") %>%
  filter(Standardisierung_Name == "beobachtet") %>%
  filter(Indikator_ID == 2040202) %>%
  select(Zeitraum_Name, Wert) %>%
  mutate(Zeitraum_Name = as.integer(Zeitraum_Name))

ggplot(data = dep_time,
       mapping = aes(
         x = Zeitraum_Name,
         y = Wert
       )) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Development of depressive symptoms over time in Germany",
       x = "years",
       y = "depressive symptoms")

#leo analysis: second research question

#point estimators: aggregated values of depressive symptoms for each year and 
#education level with the aggregated sample sizes
bildung_symptom %>%
  group_by(Zeitraum_Name, Bildung_Casmin_Name) %>%
  summarize(Wert = weighted_average(Wert, Stichprobe),
            sample_size = sum(Stichprobe))

#we already have confidence intervals so I will reverse the formula for
#the confidence interval to caluclate the standard deviation

#calculate standard deviation from lower confidence intervall
#for the confidence intervall we will assume a t-distribution and thus
#use 1.96 as t for the reverse 0.95-confidence interval formula
variance <- function(conf_low, mean_value, sample_size){
  variance <- ((mean_value - conf_low)/1.96)^2 * sample_size
  return(variance)
}
#the function appears to work

#now we will add a column with the variances for every observation
bildung_symptom <- bildung_symptom %>%
  mutate(Varianz = variance(Unteres_Konfidenzintervall, Wert, Stichprobe))

#this is aggregated values from before plus the weighted mean variances for
#every year
aggreg <- bildung_symptom %>%
  group_by(Zeitraum_Name, Bildung_Casmin_Name) %>%
  summarize(Wert = weighted_average(Wert, Stichprobe),
            sample_size = sum(Stichprobe),
            variance = weighted_average(Varianz, Stichprobe)
            )

#now we can calculate the confidence intervals for every yearly value
#so we need a function to calculate confidence intervals
#for the confidence intervall we will assume a t-distribution and thus
#use 1.98 as t for a 0.95-confidence
#function for lower confidence interval
conf_low <- function(value, variance, n){
  conf <-  value - 1.98 * sqrt(variance/n)
  return(conf)
}

#function for upper confidence interval
conf_up <- function(value, variance, n){
  conf <- value + 1.98 * sqrt(variance/n)
  return(conf)
}
#functions appear to work (yay!)

# we will add the confidence intervals to our aggregated tibble
aggreg <- aggreg %>% 
  mutate(low_confint = conf_low(Wert, variance, sample_size)) %>%
  mutate(up_confint = conf_up(Wert, variance, sample_size))

#we can see that the intervals never overlap except once!

#second part of the research question: did the gap of depressive symptoms
#between the high education group and the general education group decline?

#data only with high and general group:(bildung2 because it is the second part)
#this time we will have to remove an observation with an NA-value for depression
bildung2 <- rki_data %>%
  filter(Indikator_ID == 2040202) %>%
  filter(!is.na(Bildung_Casmin_Name)) %>%
  filter(Bildung_Casmin_Name == "Gesamt" |Bildung_Casmin_Name == "hoch") %>%
  filter(Standardisierung_ID == 3) %>%
  filter(!is.na(Wert))

# calculate the point estimators again
aggreg2 <- bildung2 %>%
  group_by(Zeitraum_Name, Bildung_Casmin_Name) %>%
  summarize(Wert = weighted_average(Wert, Stichprobe))

# I can already see that the difference doesn't become smaller; I will plot 
#a graph

