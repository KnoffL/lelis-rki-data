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
