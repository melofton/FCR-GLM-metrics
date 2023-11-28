#Format data for DOY model for chl-a
#Author: Mary Lofton
#Date: 13MAR23

#Purpose: format chl-a observation data 

# packages
library(tidyverse)
library(lubridate)

# Read in catwalk data from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f" 
infile1 <- paste0("./observations/FCR_Catwalk_EDI_2018_2022.csv")
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

#'Function to format chlorophyll-a observation data from EXO in FCR
#'@param filepath filepath to exo data product from EDI for FCR

#load packages
library(tidyverse)
library(lubridate)

format_chla_obs <- function(filepath = "./observations/FCR_Catwalk_EDI_2018_2022.csv"){

exo <- read_csv(filepath) %>%
  filter(Flag_EXOChla_ugL_1 == 0 & year(DateTime) %in% c(2018:2022)) %>%
  select(DateTime, EXOChla_ugL_1) %>%
  mutate(DateTime = date(DateTime)) %>%
  group_by(DateTime) %>%
  summarize(PHY_TCHLA = median(EXOChla_ugL_1, na.rm = TRUE)) %>%
  add_column(Depth = 1.6) %>%
  arrange(DateTime) %>%
  select(DateTime, Depth, PHY_TCHLA)
    
return(exo)
}

exo <- format_chla_obs()

write.csv(exo, file = "./observations/EXOChla.csv", row.names = FALSE)
