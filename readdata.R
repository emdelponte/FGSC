
library(flexdashboard)
library(readxl)
library(tidyverse)
library(crosstalk)
library(plotly)
library(viridis)

Sys.setlocale("LC_ALL", 'pt_BR.UTF-8')

fgsc <- read_excel("fgsc-dat9.xlsx") %>% 
  select(1:9)

fgsc <- fgsc %>% 
  filter(TRI_genotype != "15-ADON, NIV")
library(plyr)
fgsc$FGSC <- revalue(fgsc$FGSC, 
                     c("Fusarium vorosii" = "F. vorosii",
                       "Fusarium nepalense" = "F. nepalense",
                       "Fusarium meridionale" = "F. meridionale",
                       "Fusarium graminearum" = "F. graminearum",
                       "Fusarium cortaderiae" = "F. cortaderiae",
                       "Fusarium boothii" = "F. boothii",
                       "Fusarium asiaticum" = "F. asiaticum"))

fgsc$TRI_genotype <- revalue(fgsc$TRI_genotype, 
                             c("DON/ZEA" = "DON",
                               "N-15-ADON" = "15-ADON",
                               "N-15ADON" = "15-ADON",
                               "s3-ADON" = "3-ADON",
                               "NX-2/3-ADON" = "NX-2",
                               "NX2" = "NX-2"))                       
library(crosstalk)
library(leaflet)
library(leaflet.extras)
library(DT)
sd <- SharedData$new(fgsc)
