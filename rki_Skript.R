# This is the exploration of a alternative messier data set
library(lintr)
library(readr)
library(tidyverse)

rki_data <- read_tsv("GBE_Indikatoren_nichtuebertragbarer_Erkrankungen.tsv")
View(rki_data)
glimpse(rki_data)

#Number of observations for depressive symptoms (2040201) and
#for diagnosed depression (2040202)
rki_data |> filter(Indikator_ID == 2040201) |> nrow()
rki_data |> filter(Indikator_ID == 2040202) |> nrow()

rki_data |> filter(!is.na(Fälle)) |> nrow()

lint(filename = "rki_Skript.R")

