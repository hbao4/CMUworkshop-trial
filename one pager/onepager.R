library(tidyverse)
library(lubridate)

tutor_log <- read_tsv('tutor_log_anonymized.tsv')
ob_event <- read_tsv('observation_events_anonymized.tsv')
event <- read_csv('event_master_file_D10_R500_RNG1000_sprint2_shou.csv')


#cleaning data
##cleaning session id data
unique(tutor_log$`Session Id`)
unique_ids <- unique(tutor_log$`Session Id`)
id_mapping <- setNames(1:length(unique_ids), unique_ids)
tutor_log$`Session Id` <- id_mapping[tutor_log$`Session Id`]
## cleaning student data
unique(tutor_log$`Anon Student Id`)
unique_ids <- unique(tutor_log$`Anon Student Id`)
id_mapping <- setNames(1:length(unique_ids), unique_ids)
tutor_log$`Anon Student Id` <- id_mapping[tutor_log$`Anon Student Id`]
##convert timestamp
tutor_log$Time <- as.POSIXct(tutor_log$Time, origin = "1970-01-01", tz = "America/New_York")
event$timestamp <- as.POSIXct(event$timestamp, origin = "1970-01-01", tz = "America/New_York")
teacher_position$time_stamp <- as.POSIXct(teacher_position$time_stamp, origin = "1970-01-01", tz = "America/New_York")
ob_event$timestamp <- as.POSIXct(ob_event$timestamp, origin = "1970-01-01", tz = "America/New_York")

tutor_log_filtered <- tutor_log %>%
  select(`Anon Student Id`, `Session Id`, `Time`, `CF (Student Response Type)`, `CF (Student Response Subtype)`, `CF (Tutor Response Type)`,`CF (Level Assignment)`, `CF (Level ProblemSet)`, `CF (Problem Start Time)`, `CF (Step Name)`, `CF (Attempt At Step)`, `CF (Outcome)`,`CF (Selection 25)`, `CF (Action 27)`, `CF (Class)`, `Problem Name`)

ob_event %>%
  filter(periodID == 1, dayID == 1) %>%
  summarise(min_timestamp = min(timestamp), max_timestamp = max(timestamp))

start_time <- as.POSIXct("2022-05-23 08:27:01", tz = "America/New_York")
end_time <- as.POSIXct("2022-05-23 08:51:49", tz = "America/New_York")
tutor_log_filtered_1 <- tutor_log_filtered[tutor_log_filtered$Time >= start_time & tutor_log_filtered$Time <= end_time, ]

tutor_log_filtered_day1_period1 <- tutor_log_filtered_1 %>%
  mutate(dayID = 1, PeriodID = 1)

tutor_event <- full_join(tutor_log_filtered_day1_period1, tutor_log_filtered_day1_period2)

tutor_event <- full_join(tutor_event, tutor_log_filtered_day3_period5)

ob1 <- ob_event %>%
  filter(periodID == 5, dayID == 3)
tutor_ob3_5 <- full_join(ob1, tutor_log_filtered_day3_period5, by = c("periodID" = "PeriodID", "dayID" = "dayID", "timestamp" = "Time"))
#rm(ob1)

#tutor_ob3 <- full_join(tutor_ob3_1, tutor_ob3_2)
#tutor_ob3 <- full_join(tutor_ob3, tutor_ob3_5)
#rm(tutor_ob3_1)

resultday1 <- tutor_ob1 %>%
  group_by(`Anon Student Id`, `CF (Student Response Type)`, `CF (Level ProblemSet)`) %>%
  summarise(
    response_count_response = n(),
    .groups = 'drop'
  ) %>%
  mutate(dayID = 1)


resultday1_1 <- tutor_ob1 %>%
  group_by(`Anon Student Id`, `CF (Outcome)`,  `CF (Level ProblemSet)`) %>%
  summarise(
    response_Outcome = n(),
    .groups = 'drop') %>%
  mutate(dayID = 1)
resultday1 <- left_join(resultday1, resultday1_1, by = c("Anon Student Id", "dayID", "CF (Level ProblemSet)"))
#rm(resultday1_1)

resultday3 <- tutor_ob3 %>%
  group_by(`Anon Student Id`, `CF (Student Response Type)`, `CF (Level ProblemSet)`) %>%
  summarise(
    response_count_response = n(),
    .groups = 'drop'
  ) %>%
  mutate(dayID = 3)

resultday1_1 <- tutor_ob3 %>%
  group_by(`Anon Student Id`, `CF (Outcome)`,  `CF (Level ProblemSet)`) %>%
  summarise(
    response_Outcome = n(),
    .groups = 'drop') %>%
  mutate(dayID = 3)
resultday3 <- left_join(resultday3, resultday1_1, by = c("Anon Student Id", "dayID", "CF (Level ProblemSet)"))


result <- full_join(result, resultday3)
#result$response_count <- NULL
#rm(resultday3)

#rm(test)

result %>%
  filter(`Anon Student Id` == 1) %>%  # Filter for the specific student
  ggplot(aes(x = dayID, y = response_Outcome, color = `CF (Outcome)`)) +  # Map dayID to x-axis and response_Outcome to y-axis
  geom_point(size = 2, color = "red") +  # Add red points for exact values
  geom_line()
  labs(
    title = "Response Outcome Over Days for Student 1",
    x = "Day",
    y = "Response Outcome"
  ) +  # Add labels
  theme_minimal()  # Apply minimal theme

  correct <- result %>%
    group_by(dayID, `CF (Outcome)`) %>%  # Group by dayID and CF (Outcome)
    summarise(sum_response_Outcome = sum(response_Outcome, na.rm = TRUE), .groups = 'drop') %>%  # Summarize the sum of response_Outcome
    distinct()

correct %>%
  ggplot(aes(x = dayID, y = `sum_response_Outcome`, color = `CF (Outcome)`)) +
  geom_point() +
  geom_line()

df1 <- result %>%
  filter(`CF (Student Response Type)` == "ATTEMPT") %>%
  filter(`CF (Outcome)` == "CORRECT") %>%
  group_by(`Anon Student Id`)
  
df2 <- df1 %>%
  group_by(`CF (Level ProblemSet)`, `dayID`) %>%
  summarise(level_correct_rate = mean(attempt_correct))

df2 <- df2 %>%
  mutate(
    `level(blue_red)` = str_extract(`CF (Level ProblemSet)`, "Lynnette (Blue|Red) Level"),
    level_number = as.numeric(str_extract(`CF (Level ProblemSet)`, "\\d+"))
  )

df2 %>%
  ggplot(aes(x = dayID, y = level_correct_rate, color = `level(blue_red)`)) +
  geom_point() 

joined_data <- joined_data %>%
  mutate(timestamp = ymd_hms(timestamp))

allday3 <- joined_data %>%
  filter(dayID == 3)

allday3 <- joined_data %>%
  filter(timestamp >= ymd_hms("2022-05-25 08:21:21") & timestamp <= ymd_hms("2022-05-25 14:29:43"))

allday1$dayID = 1

allday1 <- allday1 %>%
  mutate(timestamp = ymd_hms(timestamp))

allday1 <- allday1 %>%
  mutate(time_interval = floor_date(timestamp, unit = "15 minutes"))

t<-allday3 %>%
  filter(periodID ==5)

allday3 <- allday3 %>%
  mutate(periodID = ifelse(timestamp >= ymd_hms("2022-05-25 14:06:39") & timestamp < ymd_hms(
    "2022-05-25 14:29:43") & is.na(periodID), 
                           5, periodID))

#plot day1
behavior_countsday1 <- allday1 %>%
  mutate(teacher_behavior = ifelse(!is.na(actor), 1, 0),
         student_behavior = ifelse(!is.na(`Anon Student Id`), 1, 0)) %>%
  group_by(periodID) %>%
  summarise(teacher_count = sum(teacher_behavior),
            student_count = sum(student_behavior)) %>%
  ungroup()

behavior_countsday1 <- behavior_countsday1 %>%
  mutate(teacher_ratio = teacher_count / (teacher_count + student_count),
         student_ratio = student_count / (teacher_count + student_count))

ggplot(behavior_countsday1, aes(x = periodID)) +
  geom_point(aes(y = teacher_ratio, color = "Teacher Ratio"), size = 3) +
  geom_line(aes(y = teacher_ratio, color = "Teacher Ratio"), size = 1) +
  geom_point(aes(y = student_ratio, color = "Student Ratio"), size = 3) +
  geom_line(aes(y = student_ratio, color = "Student Ratio"), size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("Teacher Ratio" = "blue", "Student Ratio" = "red")) +
  labs(title = "Teacher vs Student Behavior Ratios day1",
       x = "Period ID",
       y = "Ratio",
       color = "Legend") +
  theme_minimal()

#plot day2

behavior_countsday2 <- allday2 %>%
  mutate(teacher_behavior = ifelse(!is.na(actor), 1, 0),
         student_behavior = ifelse(!is.na(`Anon Student Id`), 1, 0)) %>%
  group_by(periodID) %>%
  summarise(teacher_count = sum(teacher_behavior),
            student_count = sum(student_behavior)) %>%
  ungroup()

behavior_countsday2 <- behavior_countsday2 %>%
  mutate(teacher_ratio = teacher_count / (teacher_count + student_count),
         student_ratio = student_count / (teacher_count + student_count))

ggplot(behavior_countsday2, aes(x = periodID)) +
  geom_point(aes(y = teacher_ratio, color = "Teacher Ratio"), size = 1) +
  geom_line(aes(y = teacher_ratio, color = "Teacher Ratio"), size = 1) +
  geom_point(aes(y = student_ratio, color = "Student Ratio"), size = 1) +
  geom_line(aes(y = student_ratio, color = "Student Ratio"), size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("Teacher Ratio" = "blue", "Student Ratio" = "red")) +
  labs(title = "Teacher vs Student Behavior Ratios day2",
       x = "Period ID",
       y = "Ratio",
       color = "Legend") +
  theme_minimal()

#plot day 3
behavior_countsday3 <- allday3 %>%
  mutate(teacher_behavior = ifelse(!is.na(actor), 1, 0),
         student_behavior = ifelse(!is.na(`Anon Student Id`), 1, 0)) %>%
  group_by(periodID) %>%
  summarise(teacher_count = sum(teacher_behavior),
            student_count = sum(student_behavior)) %>%
  ungroup()

behavior_countsday3 <- behavior_countsday3 %>%
  mutate(teacher_ratio = teacher_count / (teacher_count + student_count),
         student_ratio = student_count / (teacher_count + student_count))

ggplot(behavior_countsday3, aes(x = periodID)) +
  geom_point(aes(y = teacher_ratio, color = "Teacher Ratio"), size = 3) +
  geom_line(aes(y = teacher_ratio, color = "Teacher Ratio"), size = 1) +
  geom_point(aes(y = student_ratio, color = "Student Ratio"), size = 3) +
  geom_line(aes(y = student_ratio, color = "Student Ratio"), size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("Teacher Ratio" = "blue", "Student Ratio" = "red")) +
  labs(title = "Teacher vs Student Behavior Ratios day3",
       x = "Period ID",
       y = "Ratio",
       color = "Legend") +
  theme_minimal()

behavior_counts <- full_join(behavior_counts, behavior_countsday3)

# Create the plot
ggplot(behavior_counts, aes(x = periodID)) +
  # Add student ratio points and lines
  geom_point(aes(y = student_ratio, color = as.factor(dayID)), size = 3) +
  geom_line(aes(y = student_ratio, color = as.factor(dayID), group = interaction(dayID, "student")), size = 1) +
  # Customize the y-axis
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  # Add a color scale for dayID
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green")) +
  # Add labels and title
  labs(title = "Student Behavior Ratios by Period and Day",
       x = "Period ID",
       y = "Student Ratio",
       color = "Day ID") +
  # Minimal theme
  theme_minimal()

#15 minutes unit behavior plot
allday1 <- allday1 %>%
  mutate(time_interval = floor_date(timestamp, unit = "15 minutes"))

#day1
behaviorday1 <- allday1 %>%
  mutate(
    teacher_behavior = ifelse(!is.na(actor), 1, 0),
    student_behavior = ifelse(!is.na(`Anon Student Id`), 1, 0),
    correct_number = ifelse(`CF (Outcome)` == "CORRECT", 1, 0),
    attempt_number = ifelse(`CF (Student Response Type)` == "ATTEMPT", 1, 0)
  ) %>%
  group_by(time_interval, periodID) %>% 
  summarise(
    teacher_count = sum(teacher_behavior, na.rm = TRUE),
    student_count = sum(student_behavior, na.rm = TRUE),
    correct_count = sum(correct_number, na.rm = TRUE),
    attempt_count = sum(attempt_number, na.rm = TRUE)
  ) %>%
  ungroup()

behaviorday1 <- behaviorday1 %>%
  mutate(teacher_ratio = teacher_count / (teacher_count + student_count),
         student_ratio = student_count / (teacher_count + student_count),
         correct_attempt_ratio = correct_count/attempt_count)

ggplot(behaviorday1, aes(x = time_interval)) +
  geom_point(aes(y = teacher_ratio, color = "Teacher Ratio"), size = 1) +
  geom_line(aes(y = teacher_ratio, color = "Teacher Ratio"), size = 1) +
  geom_point(aes(y = student_ratio, color = "Student Ratio"), size = 1) +
  geom_line(aes(y = student_ratio, color = "Student Ratio"), size = 1) +
  geom_point(aes(y = correct_attempt_ratio, color = "Correct Attempt Ratio"), size = 1) +
  geom_line(aes(y = correct_attempt_ratio, color = "Correct Attempt Ratio"), size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~ periodID, scales = "free_x", nrow = 1) +
  scale_color_manual(values = c("Teacher Ratio" = "lightblue", 
                                "Student Ratio" = "darkgreen", 
                                "Correct Attempt Ratio" = "pink")) +
  labs(
    title = "Classroom Modality and Outcome Change by Period Day1",
    subtitle = "How classroom modality changes every 15 minutes within each period",
    x = "Time Interval",
    y = "Ratio",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Tilt x-axis labels
  )


#day2
behaviorday2 <- allday2 %>%
  mutate(
    teacher_behavior = ifelse(!is.na(actor), 1, 0),
    student_behavior = ifelse(!is.na(`Anon Student Id`), 1, 0),
    correct_number = ifelse(`CF (Outcome)` == "CORRECT", 1, 0),
    attempt_number = ifelse(`CF (Student Response Type)` == "ATTEMPT", 1, 0)
  ) %>%
  group_by(time_interval, periodID) %>% 
  summarise(
    teacher_count = sum(teacher_behavior, na.rm = TRUE),
    student_count = sum(student_behavior, na.rm = TRUE),
    correct_count = sum(correct_number, na.rm = TRUE),
    attempt_count = sum(attempt_number, na.rm = TRUE)
  ) %>%
  ungroup()

behaviorday2 <- behaviorday2 %>%
  mutate(teacher_ratio = teacher_count / (teacher_count + student_count),
         student_ratio = student_count / (teacher_count + student_count),
         correct_attempt_ratio = correct_count/attempt_count)

ggplot(behaviorday2, aes(x = time_interval)) +
  geom_point(aes(y = teacher_ratio, color = "Teacher Ratio"), size = 1) +
  geom_line(aes(y = teacher_ratio, color = "Teacher Ratio"), size = 1) +
  geom_point(aes(y = student_ratio, color = "Student Ratio"), size = 1) +
  geom_line(aes(y = student_ratio, color = "Student Ratio"), size = 1) +
  geom_point(aes(y = correct_attempt_ratio, color = "Correct Attempt Ratio"), size = 1) +
  geom_line(aes(y = correct_attempt_ratio, color = "Correct Attempt Ratio"), size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~ periodID, scales = "free_x", nrow = 1) +
  scale_color_manual(values = c("Teacher Ratio" = "lightblue", 
                                "Student Ratio" = "darkgreen", 
                                "Correct Attempt Ratio" = "pink")) +
  labs(
    title = "Classroom Modality and Outcome Change by Period Day2",
    subtitle = "How classroom modality changes every 15 minutes within each period",
    x = "Time Interval",
    y = "Ratio",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Tilt x-axis labels
  )

#day3
behaviorday3 <- allday3 %>%
  mutate(
    teacher_behavior = ifelse(!is.na(actor), 1, 0),
    student_behavior = ifelse(!is.na(`Anon Student Id`), 1, 0),
    correct_number = ifelse(`CF (Outcome)` == "CORRECT", 1, 0),
    attempt_number = ifelse(`CF (Student Response Type)` == "ATTEMPT", 1, 0)
  ) %>%
  group_by(time_interval, periodID) %>% 
  summarise(
    teacher_count = sum(teacher_behavior, na.rm = TRUE),
    student_count = sum(student_behavior, na.rm = TRUE),
    correct_count = sum(correct_number, na.rm = TRUE),
    attempt_count = sum(attempt_number, na.rm = TRUE)
  ) %>%
  ungroup()

behaviorday3 <- behaviorday3 %>%
  mutate(teacher_ratio = teacher_count / (teacher_count + student_count),
         student_ratio = student_count / (teacher_count + student_count),
         correct_attempt_ratio = correct_count/attempt_count)

ggplot(behaviorday3, aes(x = time_interval)) +
  geom_point(aes(y = teacher_ratio, color = "Teacher Ratio"), size = 1) +
  geom_line(aes(y = teacher_ratio, color = "Teacher Ratio"), size = 1) +
  geom_point(aes(y = student_ratio, color = "Student Ratio"), size = 1) +
  geom_line(aes(y = student_ratio, color = "Student Ratio"), size = 1) +
  geom_point(aes(y = correct_attempt_ratio, color = "Correct Attempt Ratio"), size = 1) +
  geom_line(aes(y = correct_attempt_ratio, color = "Correct Attempt Ratio"), size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~ periodID, scales = "free_x", nrow = 1) +
  scale_color_manual(values = c("Teacher Ratio" = "lightblue", 
                                "Student Ratio" = "darkgreen", 
                                "Correct Attempt Ratio" = "pink")) +
  labs(
    title = "Classroom Modality and Outcome Change by Period Day3",
    subtitle = "How classroom modality changes every 15 minutes within each period",
    x = "Time Interval",
    y = "Ratio",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Tilt x-axis labels
  )

