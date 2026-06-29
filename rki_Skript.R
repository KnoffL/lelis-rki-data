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

rki_data %>%
  filter(!is.na(Fälle)) %>%
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

# Get and view relevant rows for first research question
rki_data_1 <- rki_data %>%
  filter(Indikator_ID == 2040202 | Indikator_ID == 1010301) %>% # Get depression and social support rows
  filter(Standardisierung_ID == 3) %>% # Get age-adjusted data
  filter(Bildung_Casmin_ID == 0) # Get data across education levels

View(rki_data_1)
# For depression, there is only region specific data for 2014 and 2019

# Transform data so that there is only one row per subgroup
depression_data <- rki_data_1 %>%
  filter(Indikator_ID == 2040202)

socialsupport_data <- rki_data_1 %>%
  filter(Indikator_ID == 1010301)

rki_data_1 <- depression_data %>%
  inner_join(socialsupport_data, by = c("Zeitraum_Name", "Geschlecht_ID", "Alter_ID", "Region_ID"), suffix = c("_depression", "_socialsupport"))
View(rki_data_1)

rki_data_1 <- rki_data_1 %>%
  mutate(subgroup_name = paste(Geschlecht_Name_depression, Alter_Name_depression, Region_Name_depression), .keep = "unused")

# Check if Unsicherheit is ever not zero
rki_data_1_Unsicherheit_low <- rki_data_1 %>%
  filter(Unsicherheit_socialsupport > 0 | Unsicherheit_depression > 0)

View(rki_data_1_Unsicherheit_low)
# Depression is 1 8 times, 2 one time - 7 of the times it is in 2019
# Wert is not reported when Unsicherheit is 2, so we cannot analyze this data
# RKI says the following about Unsicherheit = 1: 'Berichtet, jedoch als unsicher
# markiert, werden Werte, die auf weniger als 10 Fällen basieren, deren
# Konfidenzintervall breiter als 20 Prozentpunkte ist oder wenn die Untergrenze
# weniger als ⅔ des Schätzers beträgt (Variationskoeffizient ≤ 16,6 %).
# Aufgrund der Unsicherheit sollten diese Werte mit Vorsicht interpretiert werden.'
# We will move on with our analyses for now but will keep it in mind.

### Check sample size for depression samples
# Check sample sizes for subgroups for regional analyses
rki_data_1_region_sample_size <- rki_data_1 %>%
  filter(Region_ID != 0)
ggplot(rki_data_1_region_sample_size, aes(x = Stichprobe_depression)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Sample Sizes for Depression in Regional Samples",
    x = "Sample Size",
    y = "Count"
  )

# Check sample sizes for subgroups for gender analyses for all ages
rki_data_1_gender_sample_size <- rki_data_1 %>%
  filter(Geschlecht_ID != 0 & Alter_ID == "00+")
ggplot(rki_data_1_gender_sample_size, aes(x = Stichprobe_depression, fill = factor(Geschlecht_ID))) +
  geom_histogram(bins = 30, position = "dodge") +
  scale_fill_manual(
    values = c("1" = "purple", "2" = "yellow"),
    labels = c("1" = "Women", "2" = "Men")
  ) +
  labs(
    title = "Distribution of Sample Sizes for Depression in Gender Samples",
    x = "Sample Size",
    y = "Count",
    fill = "Gender"
  )

# Check sample sizes for subgroups for gender analyses in age subgroups
rki_data_1_gender_age_sample_size <- rki_data_1 %>%
  filter(Geschlecht_ID != 0 & Alter_ID != "00+")
ggplot(rki_data_1_gender_age_sample_size, aes(x = Stichprobe_depression, fill = factor(Geschlecht_ID))) +
  geom_histogram(bins = 30, position = "dodge") +
  scale_fill_manual(
    values = c("1" = "purple", "2" = "yellow"),
    labels = c("1" = "Women", "2" = "Men")
  ) +
  labs(
    title = "Distribution of Sample Sizes for Depression in Gender Samples",
    x = "Sample Size",
    y = "Count",
    fill = "Gender"
  )
# Sample sizes vary a lot but (assessed by looking at the plots) not systemetically between men and women

# Check sample sizes for subgroups for age analyses
rki_data_1_age_sample_size <- rki_data_1 %>%
  filter(Alter_ID != "00+" & Geschlecht_ID == 0)
ggplot(rki_data_1_age_sample_size, aes(x = Stichprobe_depression)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Sample Sizes for Depression in Age Samples",
    x = "Sample Size",
    y = "Count"
  )

### Check sample size for social support samples
# Check sample sizes for subgroups for regional analyses
ggplot(rki_data_1_region_sample_size, aes(x = Stichprobe_socialsupport)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Sample Sizes for Social Support in Regional Samples",
    x = "Sample Size",
    y = "Count"
  )

# Check sample sizes for subgroups for gender analyses for all ages
ggplot(rki_data_1_gender_sample_size, aes(x = Stichprobe_socialsupport, fill = factor(Geschlecht_ID))) +
  geom_histogram(bins = 30, position = "dodge") +
  scale_fill_manual(
    values = c("1" = "purple", "2" = "yellow"),
    labels = c("1" = "Women", "2" = "Men")
  ) +
  labs(
    title = "Distribution of Sample Sizes for Social Support in Gender Samples",
    x = "Sample Size",
    y = "Count",
    fill = "Gender"
  )

# Check sample sizes for subgroups for gender analyses in age subgroups
ggplot(rki_data_1_gender_age_sample_size, aes(x = Stichprobe_socialsupport, fill = factor(Geschlecht_ID))) +
  geom_histogram(bins = 30, position = "dodge") +
  scale_fill_manual(
    values = c("1" = "purple", "2" = "yellow"),
    labels = c("1" = "Women", "2" = "Men")
  ) +
  labs(
    title = "Distribution of Sample Sizes for Social Support in Gender Samples",
    x = "Sample Size",
    y = "Count",
    fill = "Gender"
  )
# Sample sizes vary a lot but (assessed by looking at the plots) not systemetically between men and women

# Check sample sizes for subgroups for age analyses
ggplot(rki_data_1_age_sample_size, aes(x = Stichprobe_socialsupport)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Sample Sizes for Social Support in Age Samples",
    x = "Sample Size",
    y = "Count"
  )

# Sample sizes vary a lot for all inspected combinations, that is important to keep in mind

# Correlation between age-adjusted depression and social support over time
rki_data_1_corr <- rki_data_1 %>%
  filter(Geschlecht_ID == 0 & Alter_ID == "00+" & Region_ID == 0)

cor.test(rki_data_1_corr$Wert_depression, rki_data_1_corr$Wert_socialsupport)
# The correlation is 0.36 but confidence intervals are insanely wide - that is because we only use
# aggregated data from four time points here. It can therefore only be used as a starting point for
# analyses on individual data but on our aggregated level here, it is rather meaningless

# Correlation on a regional level (only for 2014 and 2019 as there is no regional data after)
rki_data_1_corr_regional <- rki_data_1 %>%
  filter(Geschlecht_ID == 0 & Alter_ID == "00+" & Region_ID != 0)

cor.test(rki_data_1_corr_regional$Wert_depression, rki_data_1_corr_regional$Wert_socialsupport)

# Correlation on a gender x age level
rki_data_1_corr_gender_age <- rki_data_1 %>%
  filter(Geschlecht_ID != 0 & Alter_ID != "00+" & Region_ID == 0)

cor.test(rki_data_1_corr_gender_age$Wert_depression, rki_data_1_corr_gender_age$Wert_socialsupport)

# Correlation on a gender level
rki_data_1_corr_gender <- rki_data_1 %>%
  filter(Geschlecht_ID != 0 & Alter_ID == "00+" & Region_ID == 0)

cor.test(rki_data_1_corr_gender$Wert_depression, rki_data_1_corr_gender$Wert_socialsupport)

# Correlation on a age level
rki_data_1_corr_age <- rki_data_1 %>%
  filter(Geschlecht_ID == 0 & Alter_ID != "00+" & Region_ID == 0)

cor.test(rki_data_1_corr_age$Wert_depression, rki_data_1_corr_age$Wert_socialsupport)

# Correlation on a regional level separate by time points (only for 2014 and 2019 as there is no regional data after)
rki_data_1_corr_regional %>%
  group_by(Zeitraum_Name) %>%
  summarise(
    cor = cor(Wert_depression, Wert_socialsupport, use = "complete.obs"), # one region is missing for 2019
    p_value = cor.test(Wert_depression, Wert_socialsupport)$p.value,
    n = sum(!is.na(Wert_depression) & !is.na(Wert_socialsupport))
  )

# Only the regional analysis is significant. There, higher levels of depression are associated with lower social support.
# Apart from that, the descriptive analysis is mixed. Population-wide aggregated data is very much sub-optimal for the question
# we are trying to answer
