# This is the exploration of a alternative messier data set 

library(readr)
library(tidyverse)

rki_data <- read_tsv("GBE_Indikatoren_nichtuebertragbarer_Erkrankungen.tsv")
View(rki_data)
glimpse(rki_data)

