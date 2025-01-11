#### Arm Care Execute File ####

# Umpire Report Execute File

suppressWarnings(suppressMessages({
  library(tidyverse)
  library(kableExtra)
  library(gridExtra)
  library(gtsummary)
  library(pak)
  library(kableExtra)
  library(knitr)
  library(pandoc)
  library(scales)
  library(readxl)
  library(ggrepel)
  library(ggplot2)
}))

file = read_xlsx("Daily Log 12-24-2024 2-21-48.xlsx", sheet = "All Data")

pitcher = 'Wyatt Mummert'

date = '12/22/24'

source('ArmCareFunctions.R')


ArmCareReport(file = file, pitcher)
