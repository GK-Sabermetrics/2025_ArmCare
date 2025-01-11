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
  datafile %>% 
    filter(`Exam Type` == "Fresh - Quick") %>%
  ggplot(aes(x = `Short Date`, y = `Arm Score`)) +
    geom_point() +
    geom_line(group = 1) +
    labs(x = "Date", y = "Arm Score", title = "Arm Score") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#### Strength Plot ####
StrengthPlot = function(datafile) {
  datafile %>% 
    group_by(`Short Date`) %>% 
    summarise(`Total Strength`, `Total Strength Post`) %>% 
    pivot_longer(cols = c("Total Strength", 'Total Strength Post')) %>% 
    na.omit() %>%
  ggplot(aes(x = `Short Date`, y = value, group = name, color = name)) +
    geom_line() +
    geom_point() +
    labs(x = "Date", y = "Value", color = "Metric", title = "Total Strength Pre vs Post") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
}

#### IR ROM Plot ####
IRROMPlot = function(datafile) {
  datafile %>% filter(`Exam Type` != 'Arm Primer') %>% 
    select(`Short Date`, `Exam Type`, `IRTARM ROM`) %>% 
    pivot_longer(c(3)) %>% 
  ggplot(aes(x = `Short Date`, y = value, group = `Exam Type`, color = `Exam Type`)) +
    geom_point() +
    geom_line() +
    labs(x = "Date", y = "Degrees", color = "Exam", title = "IR Throwing Arm ROM") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
}

#### ER ROM Plot ####
ERROMPlot = function(datafile) {
  datafile %>% filter(`Exam Type` != 'Arm Primer') %>% 
    select(`Short Date`, `Exam Type`, `ERTARM ROM`) %>% 
    pivot_longer(c(3)) %>% 
  ggplot(aes(x = `Short Date`, y = value, group = `Exam Type`, color = `Exam Type`)) +
    geom_point() +
    geom_line() +
    labs(x = "Date", y = "Degrees", color = "Exam", title = "ER Throwing Arm ROM") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
}

#### F ROM Plot ####
FROMPlot = function(datafile) {
  datafile %>% filter(`Exam Type` != 'Arm Primer') %>% 
    select(`Short Date`, `Exam Type`, `FTARM ROM`) %>% 
    pivot_longer(c(3)) %>% 
  ggplot(aes(x = `Short Date`, y = value, group = `Exam Type`, color = `Exam Type`)) +
    geom_point() +
    geom_line() +
    labs(x = "Date", y = "Degrees", color = "Exam", title = "Flexion Throwing Arm ROM") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
}

#### TARC Plot ####
TARCPlot = function(datafile) {
  
  datafile %>% filter(`Exam Type` != 'Arm Primer') %>% 
    select(`Short Date`, `Exam Type`, `TARM TARC`) %>% 
    pivot_longer(c(3)) %>% 
  ggplot(aes(x = `Short Date`, y = value, group = `Exam Type`, color = `Exam Type`)) +
    geom_point() +
    geom_line() +
    labs(x = "Date", y = "Degrees", color = "Exam", title = "Throwing Arm Total Arc") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
    
}

#### Report Function ####
ArmCareReport = function(file, pitcher) {
  
  datafile = filter(file, `Last Name` == "Mummert") %>% 
    mutate(
      `Exam Date` = as.Date(`Exam Date`, format = "%m/%d/%Y"),
      "Short Date" = format(`Exam Date`, "%m-%d"), .after = `Exam Date`
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
  strength_plot = StrengthPlot(datafile = datafile)
  ir_rom_plot = IRROMPlot(datafile = datafile)
  er_rom_plot = ERROMPlot(datafile = datafile)
  f_rom_plot = FROMPlot(datafile = datafile)
  t_arc_plot = TARCPlot(datafile = datafile)
  
  #### Parameters ####
  params = list(
    pitcher = pitcher,
    date = date,
    arm_score_plot = arm_score_plot,
    strength_plot = strength_plot,
    ir_rom_plot = ir_rom_plot,
    er_rom_plot = er_rom_plot,
    f_rom_plot = f_rom_plot,
    t_arc_plot = t_arc_plot,
    Pre.Exam.Stats = Pre.Exam.Stats,
    Pre.Exam.Weights = Pre.Exam.Weights,
    Post.Exam.Weights = Post.Exam.Weights,
    Post.Exam.Losses = Post.Exam.Losses
  )
  
  rmarkdown::render(input = "ArmCareReport.Rmd",
                    params = params,
                    quiet = FALSE)
}