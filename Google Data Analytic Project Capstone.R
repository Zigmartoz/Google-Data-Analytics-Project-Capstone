library(readxl)
activity <- read_excel("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.xlsx") #activity contains daily data on steps, distance, calories, and active minute
library(readr)
heartrate <- read_csv("Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
weight <- read_csv("Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv") #weight contains data on weight in Kg or Pounds
sleep <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv") #sleep contains data on the total time spent on bed and the total time asleep each day
METs <- read_csv("Fitabase Data 4.12.16-5.12.16/minuteMETsNarrow_merged.csv") #METs is the metabolic equivalent of task for every minute

library(pacman) #package used for managing other packages
library(tidyverse) #package used for data cleaning and manipulation

glimpse(activity) #To see the entire columns in activity table created

## Filter and clean the activity data
#selecting only the possible columns the will be used for data visualization 
activity_1 <- activity %>% 
  mutate(date = as_date(ActivityDate, "%Y-%m-%d")) %>% 
  mutate(month_year = format(date, "%b %Y")) %>% 
  select(Id, date, ActivityDate, TotalSteps, TotalDistance, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories, month_year) %>% 
  select(-ActivityDate) %>% 
  filter(!(Id %in% 4057192912))
head(activity_1)

glimpse(METs) #To see the entire columns in METs table created
METs_1 <- METs %>% 
  mutate(date_time = parse_date_time(ActivityMinute, "%m/%d/%Y %I:%M:%S %p")) %>% 
  select(-ActivityMinute)
head(METs_1)

glimpse(weight) #To see the entire columns in weight table created
weight_1 <- weight %>% 
  mutate(ID = as.character(Id)) %>% 
  select(ID, WeightKg) %>% 
  group_by(ID) %>% 
  drop_na() %>% 
  summarise(avg_weight = mean(WeightKg))
head(weight_1)

glimpse(sleep)
sleep_1 <- sleep %>% 
  mutate(Date_1 = as.character(SleepDay)) %>% 
  mutate(PST = TotalTimeInBed - TotalMinutesAsleep) %>% 
  separate(Date_1, c("date", "time"), sep = " ") %>% 
  mutate(Date = as.Date(date, "%m/%d/%Y", optional = FALSE)) %>% 
  select(-TotalSleepRecords, -SleepDay, -date, -time) %>% 
  drop_na() %>% 
  filter(!(Id %in% c(1644430081, 1844505072, 1927972279, 4020332650, 2320127002, 4558609924, 6775888955, 7007744171, 8053475328)))
head(sleep_1)

glimpse(heartrate)    
heartrate_1 <- heartrate %>% 
  mutate(date_time = parse_date_time(Time, "%m/%d/%Y %I:%M:%S %p")) %>% 
  select(-Time)
head(heartrate_1)


library(ggplot2)

ggplot(sleep_1,
       mapping = aes(
         x = Date,
         y = TotalMinutesAsleep)
       )+
  geom_line()+
  labs(x = "", 
       y = "DailySleep(mins)", 
       title = "Time Asleep per Day", 
       subtitle = "April & May")+
  theme(axis.text.x = element_text(angle=45, 
                                   hjust=0.8, 
                                   size = 8)
        )+
  scale_x_date(date_breaks = "2 week")+
  facet_wrap(~Id)


ggplot(sleep_1,
       mapping = aes(
         x = Date,
         y = PST)
       )+
  geom_line()+
  labs(x = "Date", 
       y = "Possible Stressed Time", 
       title = "Users on Bed without Sleeping", 
       subtitle = "April & May")+
  theme(axis.text.x = element_text(angle=45, 
                                   hjust=0.8, 
                                   size = 8)
        )+
  scale_x_date(date_breaks = "2 week")+
  facet_wrap(~Id)


ggplot(activity_1,
       mapping = aes(
         x = date,
         y = TotalDistance,
         colour = FairlyActiveMinutes)
       ) +
  geom_line() +
  scale_color_gradient(low = "#94E873", high = "#00441B") +
  labs(x = "Date",
       y = "Distances(Km)",
       title = "User's Daily Activity",
       subtitle = "April & May"
       ) +
  scale_x_date(date_minor_breaks = "month")+
  theme(axis.text.x = element_text(angle=45)
  )+
  theme(axis.text.y = element_text(size = 5))+
  theme(axis.text.x = element_text(size = 5))+
  facet_wrap(vars(Id))


ggplot(heartrate_1, 
       mapping = aes(
         x = date_time, 
         y = Value, 
         colour = Id)
       ) +
  geom_step() +
  scale_color_gradient(low = "#132B43", high = "#2A77B2") +
  labs(x = "Date_time",
       y = "Pulse Value per second",
       title = "Time Series of User's Heartbeat",
       subtitle = "April & May"
       ) +
  theme_light() +
  theme(axis.text.y = element_text(size = 5))+
  theme(axis.text.x = element_text(angle=45, 
                                   hjust=0.8,
                                   size = 5)
        )+
  facet_wrap(vars(Id), scales = "free")



ggplot(activity_1, 
       mapping = aes(
         x = date, 
         y = Calories, 
         colour = month_year)
       ) +
  geom_line() +
  scale_color_hue(direction = 1) +
  labs(x = "Date",
       y = "Calories (KJ)",
       title = "Showing Calories Burnt by Users",
       subtitle = "April & May"
       ) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 5))+
  theme(axis.text.x = element_text(angle=45,
                                   size = 5)
        )+
  facet_wrap(vars(Id))


ggplot(weight_1, 
       mapping = aes(
         x = ID, 
         y = avg_weight, 
         fill = avg_weight)
       )+
  geom_col() +
  theme_minimal()+
  labs(x = "ID",
       y = "Average Weight",
       title = "Average Weight of Users"
       ) +
  scale_fill_gradient(low = "#1881EB", high = "#08243A")



print(mean(activity_1$TotalDistance))
print(sd(activity_1$TotalDistance))

print(mean(sleep_1$TotalMinutesAsleep)/60)
print(sd(sleep_1$TotalMinutesAsleep)/60)
print(mean(sleep_1$PST))
print(sd(sleep_1$PST))