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

#### Arm Score Plot ####
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

#### Report Function ####
ArmCareReport = function(file, pitcher) {
  
  datafile = filter(file, `Last Name` == "Mummert") %>% 
    mutate(
      `Exam Date` = as.Date(`Exam Date`, format = "%m/%d/%Y")
    )
  
  #### Pre Exam Stats ####
  Pre.Exam.Stats = datafile %>% 
    filter(`Exam Type` == "Fresh - Quick") %>% 
    .[which.max(.$`Exam Date`),] %>% 
    summarise(`Exam Date`, `Arm Score`, `IRTARM RS`, `ERTARM RS`,
              `STARM RS`, `GTARM RS`, `Shoulder Balance`)
  
  Pre.Exam.Weights = datafile %>% 
    filter(`Exam Type` == "Fresh - Quick") %>% 
    .[which.max(.$`Exam Date`),] %>%
    summarise(`Exam Date`,`Total Strength`, `IRTARM Strength`, `ERTARM Strength`, 
              `STARM Strength`, `GTARM Strength`,)
  
  Post.Exam.Weights = datafile %>% 
    filter(`Exam Type` == "Post") %>% 
    .[which.max(.$`Exam Date`),] %>% 
    summarise(`Exam Date`,`Total Strength Post`, `IRTARM Post Strength`, 
              `ERTARM Post Strength`, `STARM Post Strength`, `GTARM Post Strength`)
  
  Post.Exam.Losses = datafile %>% 
    filter(`Exam Type` == "Post") %>% 
    .[which.max(.$`Exam Date`),] %>% 
    summarise(`Exam Date`,`Exam Date`, `Post Strength Loss`, `IRTARM Post Loss`, 
              `ERTARM Post Loss`, `STARM Post Loss`, `GTARM Post Loss`)
  
  
  
  #### Redefine Plots ####
  arm_score_plot = ArmScorePlot(datafile = datafile)
  
  #### Parameters ####
  params = list(
    pitcher = pitcher,
    date = date,
    Pre.Exam.Stats = Pre.Exam.Stats,
    Pre.Exam.Weights = Pre.Exam.Weights,
    Post.Exam.Weights = Post.Exam.Weights,
    Post.Exam.Losses = Post.Exam.Losses
  )
  
  rmarkdown::render(input = "ArmCareReport.Rmd",
                    params = params,
                    quiet = FALSE)
}