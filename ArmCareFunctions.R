# Ump Report Functions
library(tidyverse)
library(kableExtra)
library(gridExtra)
library(gtsummary)
library(pak)
library(kableExtra)
library(knitr)
library(pandoc)
library(scales)
library(ggrepel)
library(ggplot2)


ArmScorePlot = function(datafile) {
  Pitcher = datafile %>% 
    filter(`Exam Type` == "Fresh - Quick") %>% 
    mutate(
      `Exam Date` = as.Date(`Exam Date`, format = "%m/%d/%Y")
    ) %>% arrange(`Exam Date`)
  ggplot(Pitcher, aes(x = `Exam Date`)) +
    geom_point(aes(y = `Arm Score`), size = 3) +
    geom_text(aes(y = `Arm Score`, label = format(`Exam Date`, "%m-%d")), nudge_x = -3) +
    scale_x_date(date_labels = "%m/%d", date_breaks = '1 week') +
    geom_line(aes(y = `Arm Score`)) +
    theme_bw()
}


ArmCareReport = function(file, pitcher) {
  
  datafile = filter(file, `Last Name` == "Mummert" )
  
  
  arm_score_plot = ArmScorePlot(datafile = datafile)
  
  #### Parameters ####
  params = list(
    pitcher = pitcher,
    date = date
  )
  
  rmarkdown::render(input = "ArmCareReport.Rmd",
                    params = params,
                    quiet = FALSE)
}