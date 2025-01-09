library(readxl)
library(tidyverse)



data = read_xlsx("Daily Log 12-24-2024 2-21-48.xlsx", sheet = "All Data")

pitcher = filter(data, `Last Name` == "Mummert" )

Pitcher = pitcher %>% 
  filter(`Exam Type` == "Fresh - Quick") %>% 
  mutate(
    `Exam Date` = as.Date(`Exam Date`, format = "%m/%d/%Y")
  ) %>% arrange(`Exam Date`)
  
strength_num = 
Pitcher %>% group_by(`Exam Date`) %>% 
  summarise(`Total Strength`, `Total Strength Post`) %>% 
  arrange(`Exam Date`)

Arm.Data = Pitcher %>% 
  select(`Exam Date`, `Exam Type`, `IRTARM ROM`, `ERTARM ROM`, `FTARM ROM`) %>% 
  mutate(`Exam Date` = as.character(`Exam Date`))

arm_data = pivot_longer(Arm.Data, cols = c(`IRTARM ROM`, `ERTARM ROM`), names_to = "Metric", values_to = 'Score')

ggplot(arm_data, aes(x = `Exam Date`, y = Score, color = `Exam Type`, linetype = Metric, group = interaction(`Metric`, `Exam Type`))) +
  geom_line() +
  geom_point() +  # Optional: Add points for clarity
  #scale_x_date(date_labels = "%m/%d", date_breaks = '7 days') +
  labs(x = "Date", y = "Value", color = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(Pitcher, aes(x = `Exam Date`)) +
  geom_point(aes(y = `Arm Score`), size = 3) +
  geom_text(aes(y = `Arm Score`, label = format(`Exam Date`, "%m-%d")), nudge_x = -3) +
  scale_x_date(date_labels = "%m/%d", date_breaks = '1 week') +
  geom_line(aes(y = `Arm Score`)) +
  theme_bw()


data_long = pivot_longer(strength_num, cols = c("Total Strength", 'Total Strength Post')) %>% na.omit()

ggplot(data_long, aes(x = `Exam Date`, y = value, group = name, color = name)) +
  geom_line() +
  geom_point() +  # Optional: Add points for clarity
  scale_x_date(date_labels = "%m/%d", date_breaks = '7 days') +
  labs(x = "Date", y = "Value", color = "Metric", title = "test") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

PitcherArmStats = 
Pitcher %>% 
  summarise(
    "Avg Arm Score" = mean(`Arm Score`),
    "Avg Total Strength" = mean(`Total Strength`),
    "Avg IR Strength" = mean(`IRTARM RS`)*100,
    "Avg ER Strength" = mean(`ERTARM RS`)*100,
    "Avg Scaption Strength" = mean(`STARM RS`)*100
    )

#PitcherArmExams = 
  Pitcher %>% 
  group_by(`Exam Date`) %>% 
  summarise(`Arm Score`, `Total Strength`, `IRTARM RS`, `ERTARM RS`, `STARM RS`)

PitcherArmStats %>% kable(format = "html",  linesep = "", label = "Pitch Stats", align = 'c') %>%
  kable_styling(latex_options = "HOLD_position", position = "center", font_size = 20) %>% 
  column_spec(1, color = case_when(
    PitcherArmStats$`Avg Arm Score` > 70 ~ 'black',
    PitcherArmStats$`Avg Arm Score` > 50 ~ 'yellow',
    PitcherArmStats$`Avg Arm Score` <= 50 ~ 'red',
  )) %>% 
  column_spec(3, color = case_when(
    PitcherArmStats$`Avg IR Strength` > 20 ~ 'black',
    PitcherArmStats$`Avg IR Strength` == 20 ~ 'yellow',
    PitcherArmStats$`Avg IR Strength` < 20 ~ 'orange',
    PitcherArmStats$`Avg IR Strength` < 15 ~ 'red',
  )) %>% 
  column_spec(4, color = case_when(
    PitcherArmStats$`Avg ER Strength` > 20 ~ 'black',
    PitcherArmStats$`Avg ER Strength` == 20 ~ 'yellow',
    PitcherArmStats$`Avg ER Strength` < 20 ~ 'orange',
    PitcherArmStats$`Avg ER Strength` < 15 ~ 'red',
  )) %>% 
  column_spec(5, color = case_when(
    PitcherArmStats$`Avg Scaption Strength` > 15 ~ 'black',
    PitcherArmStats$`Avg Scaption Strength` == 15 ~ 'yellow',
    PitcherArmStats$`Avg Scaption Strength` < 15 ~ 'orange',
    PitcherArmStats$`Avg Scaption Strength` < 10 ~ 'red',
  ))

PitcherArmExams %>% kable(format = "html",  linesep = "", label = "Pitch Stats", align = 'c') %>%
  kable_styling(latex_options = "HOLD_position", position = "center", font_size = 20) %>% 
  column_spec(2, color = case_when(
    PitcherArmExams$`Arm Score` > 70 ~ 'black',
    PitcherArmExams$`Arm Score` > 50 ~ 'yellow',
    PitcherArmExams$`Arm Score` <= 50 ~ 'red',
  )) %>% 
  column_spec(4, color = case_when(
    PitcherArmExams$`IRTARM RS` > .20 ~ 'black',
    PitcherArmExams$`IRTARM RS` > .15 ~ 'orange',
    PitcherArmExams$`IRTARM RS` < .15 ~ 'red',
  )) %>% 
  column_spec(5, color = case_when(
    PitcherArmExams$`ERTARM RS` > 20 ~ 'black',
    PitcherArmExams$`ERTARM RS` == 20 ~ 'yellow',
    PitcherArmExams$`ERTARM RS` < 20 ~ 'orange',
    PitcherArmExams$`ERTARM RS` < 15 ~ 'red',
  )) %>% 
  column_spec(6, color = case_when(
    PitcherArmExams$`STARM RS` > 15 ~ 'black',
    PitcherArmExams$`STARM RS` == 15 ~ 'yellow',
    PitcherArmExams$`STARM RS` < 15 ~ 'orange',
    PitcherArmExams$`STARM RS` < 10 ~ 'red',
  ))

paste(data$`First Name`, data$`Last Name`) %>% unique()

