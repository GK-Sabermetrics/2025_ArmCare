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

Pitcher[which.max(Pitcher$`Exam Date`),] %>%
  summarise(`Exam Date`, `Arm Score`, `Total Strength`, `IRTARM RS`, `ERTARM RS`,
            `STARM RS`, `GTARM RS`, `Shoulder Balance`)

tester = 
Pitcher[which.max(Pitcher$`Exam Date`),] %>%
  summarise(`Exam Date`, `Arm Score`, `Total Strength`, `IRTARM RS`, `ERTARM RS`,
            `STARM RS`, `GTARM RS`, `Shoulder Balance`)

tester

tester$`Arm Score` = cell_spec(tester$`Arm Score`, 
                               color = ifelse(tester$`Arm Score`<80, 'red', 'black'))

tester %>% kbl(format = 'html', escape = F) %>% kable_styling()

# 70, 50

x = 34
ifelse(x < 50, 'red', ifelse(x < 70, 'orange', 'black'))

tester %>% 
  kable(format = "latex",  linesep = "", align = 'c', escape = FALSE,
        caption = "TARM = Throwing Arm, RS = Relative Strength (Strength/Bodyweight)") %>%
  kable_styling(latex_options = "HOLD_position", position = "center", font_size = 18) %>% 
  row_spec(0, background = "#f76800", color = 'white', bold = T) %>% 
  column_spec(1, border_left = T) %>% 
  column_spec(ncol(tester), border_right = TRUE) %>% 
  add_header_above(c("Fresh - Quick Pre Exam" = 8), border_left = T, 
                   border_right = T, color = 'white', background = "#f76800") %>% 
  footnote(number = c("Internal Rotation", "External Rotation","Scaption", "Grip"),
           general = c("Test"))

colnames(tester)[4] = paste0("IRTARM RS", footnote_marker_number(1, format = "latex"))
colnames(tester)[5] = paste0("ERTARM RS", footnote_marker_number(2))
colnames(tester)[6] = paste0("STARM RS", footnote_marker_number(3))
colnames(tester)[7] = paste0("GTARM RS", footnote_marker_number(4))
colnames(tester)[8] = paste0("Shoulder Balance", footnote_marker_number(5))

footnote_marker_alphabet(1)

kbl(Pre.Exam.Stats, escape = F) %>% kable_styling(latex_options = 'scale_down')

Pre.Exam.Stats %>% kable(escape = F) %>% 
  kable_styling() %>% 
  column_spec(2, color = ifelse(Pre.Exam.Stats$`Arm Score` < 50, 'red', ifelse(Pre.Exam.Stats$`Arm Score` < 70, 'orange', 'black'))) %>% 
  column_spec(4, color = ifelse(Pre.Exam.Stats$`IRTARM RS` < .15, 'red', ifelse(Pre.Exam.Stats$`IRTARM RS` < .20, 'orange', 'black'))) %>% 
  column_spec(5, color = ifelse(Pre.Exam.Stats$`ERTARM RS` < .15, 'red', ifelse(Pre.Exam.Stats$`ERTARM RS` < .20, 'orange', 'black'))) %>% 
  column_spec(6, color = ifelse(Pre.Exam.Stats$`STARM RS` < .10, 'red', ifelse(Pre.Exam.Stats$`STARM RS` < .15, 'orange', 'black'))) %>% 
  column_spec(7, color = ifelse(Pre.Exam.Stats$`GTARM RS` < .10, 'red', ifelse(Pre.Exam.Stats$`GTARM RS` < .15, 'orange', 'black'))) %>% 
  column_spec(8, color = case_when(
    Pre.Exam.Stats$`Shoulder Balance` > 1.20 ~ 'red',
    between(Pre.Exam.Stats$`Shoulder Balance`, 1.06, 1.20) ~ 'orange',
    between(Pre.Exam.Stats$`Shoulder Balance`, .70, .84) ~ 'orange',
    Pre.Exam.Stats$`Shoulder Balance` < .70 ~ 'red',
    .default = 'black'
  ))

Post.Exam.Losses = datafile %>% 
  filter(`Exam Type` == "Post") %>% 
  .[which.max(.$`Exam Date`),] %>% 
  summarise(`Exam Date`, `Post Strength Loss`, `IRTARM Post Loss`, `ERTARM Post Loss`, `STARM Post Loss`, `GTARM Post Loss`)

Post.Exam.Losses %>% kable(format = "html",  linesep = "", align = 'c', escape = F) %>%
  kable_styling(latex_options = "HOLD_position", position = "center", font_size = 15) %>% 
  row_spec(0, background = "#f76800", color = 'white') %>% 
  column_spec(1, border_left = T) %>% 
  column_spec(ncol(Post.Exam.Losses), border_right = TRUE) %>% 
  add_header_above(c("Fresh - Quick Pre Exam" = 6), border_left = T, 
                   border_right = T, color = 'white', background = "#f76800")
  

Pre.Exam.Weights = datafile %>% 
  filter(`Exam Type` == "Fresh - Quick") %>% 
  .[which.max(.$`Exam Date`),] %>%
  summarise(`Total Strength`, `IRTARM Strength`, `ERTARM Strength`, `STARM Strength`, `GTARM Strength`,)

Post.Exam.Weights = datafile %>% 
  filter(`Exam Type` == "Post") %>% 
  .[which.max(.$`Exam Date`),] %>% 
  summarise(`Total Strength Post`, `IRTARM Post Strength`, `ERTARM Post Strength`, `STARM Post Strength`, `GTARM Post Strength`)

rbind(Pre.Exam.Weights, Post.Exam.Weights)


