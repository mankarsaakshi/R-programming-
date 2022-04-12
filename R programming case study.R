#installing all the packages will be used

install.packages('tidyverse')
library(tidyverse)
library(readr)
install.packages('lubridate')
library(lubridate)
library(ggplot2)
library(dplyr)
library(skimr)
install.packages('janitor')
library(janitor)
library(tidyr)
install.packages("stringr")                                   
library("stringr")     
# importing all the datasets

dailyActivity_df <- read_csv(file="dailyActivity_merged.csv")
dailyCalories_df <- read_csv(file="dailyCalories_merged.csv")
dailyIntensities_df <- read_csv(file = "dailyIntensities_merged.csv")
dailySleep_df <- read_csv(file="sleepDay_merged.csv")
hourly_calories_df <- read_csv(file="hourlyCalories_merged.csv")
hourly_steps_df <- read_csv(file = "hourlySteps_merged.csv")
hourly_intensities_df <- read_csv(file="hourlyIntensities_merged.csv")
minuteIntensities_df <- read_csv(file="minuteIntensitiesNarrow_merged.csv")
min_steps_df <- read_csv(file="minuteStepsNarrow_merged.csv")
min_calories_df <- read_csv(file="minuteCaloriesNarrow_merged.csv")
minutesMET_df <- read_csv(file="minuteMETsNarrow_merged.csv")
weightLogInfo_df <- read_csv(file="weightLogInfo_merged.csv")
heartrate_seconds_df <- read_csv(file="heartrate_seconds_merged.csv")

#Getting to known about distinct users
str_c(n_distinct(activity$Id), " distinct users -> in the Daily Activities file")

str_c(n_distinct(Calories$Id), " distinct users -> in the Daily Calories file")

str_c(n_distinct(Sleep$Id), " distinct users -> in the Daily Sleep file")

str_c(n_distinct(intensities$Id), " distinct users -> in the Daily Intensities file")

str_c(n_distinct(Weight$Id), " distinct users -> in the Weight Log Info file")

str_c(n_distinct(Daily_METs_by_minutes$Id), " distinct users  -> in the Daily METs by Minutes file")

str_c(n_distinct(heartrate_seconds_df$Id), " distinct users -> in the Heart Rate Seconds file")

str_c(n_distinct(min_steps_df$Id), " distinct users -> in the Min_Steps_narrow file")

str_c(n_distinct(hourly_calories$Id), " distinct users -> in the Hourly Calories_merged file")

str_c(n_distinct(hourly_steps$Id), " distinct users -> in the Hourly_Steps file")

str_c(n_distinct(hourly_intensities$Id), " distinct users -> in the Hourly_intensities file")

# displaying starting 5 columns
#choosing the daily activities instead of every minute and hour for analysing

head(activity <- dailyActivity_df)
head(Weight <- weightLogInfo_df)
head(Calories<- dailyCalories_df)
head(Daily_METs_by_minutes <- minutesMET_df)
head(intensities <- dailyIntensities_df)
head(Sleep <- dailySleep_df)
head(hourly_calories <- hourly_calories_df)
head(hourly_steps <- hourly_steps_df)
head(hourly_intensities <- hourly_intensities_df)

# Analyze 
#seprate by date time 
Weight <- separate(Weight, Date, into = c('Date', 'Time', 'AM/PM'), sep = ' ')
head(Weight)

METs_sep_by_date_time <- separate(Daily_METs_by_minutes, ActivityMinute, into = c('ActivityDate', 'ActivityTime', 'AM/PM'), sep = ' ')
head(METs_sep_by_date_time)

Sleep <- separate(Sleep, SleepDay, into = c('SleepDate', 'SleepTime', 'AM/PM'), sep = ' ')
head(Sleep)

heart_rate_seconds <-  separate(heartrate_seconds_df, Time, into = c('Date', 'Time', 'AM/PM'), sep = ' ')
head(heart_rate_seconds)

#For hourly analysis we will use  POSIXct function (this function store date n time in secs)

activity_DT <- activity 
sleep_DT <- dailySleep_df

activity_DT$ActivityDate=as.POSIXct(activity_DT$ActivityDate,format= "%m/%d/%Y",tz=Sys.timezone())
activity_DT$Date <- format(activity_DT$ActivityDate,format="%m/%d/%Y", tz=Sys.timezone())
activity_DT$Wk_Day <- format(activity_DT$ActivityDate, format = "%A", tz=Sys.timezone())
glimpse(activity_DT)

hourly_intensities$ActivityHour=as.POSIXct(hourly_intensities$ActivityHour,format="%m/%d/%Y %I:%M:%S %p")
hourly_intensities$Hour <-  format(hourly_intensities$ActivityHour,format= "%H") 
glimpse(hourly_intensities)

hourly_steps$ActivityHour=as.POSIXct(hourly_steps$ActivityHour,format="%m/%d/%Y %I:%M:%S %p")
hourly_steps$Hour <-  format(hourly_steps$ActivityHour,format= "%H")
glimpse(hourly_steps)

#Based on this, a daily average MET (metabolic equivalent task) data was used in the analysis,
#rather than the summation of the minutes METs. Daily mean METs by Ids were summarized by Ids.
#Then, the Daily Activity and the METs data were joined for analysis.

Daily_mean_METs_df <- METs_sep_by_date_time %>%
  group_by(Id, ActivityDate) %>%
  summarize(Mean_METs_daily=mean(METs,na.rm=T))
glimpse(Daily_mean_METs_df)

joined_METs_activity <- merge(activity,Daily_mean_METs_df, all = TRUE)
glimpse(joined_METs_activity)

join_min_calories_steps <- merge(min_calories_df[1:3], min_steps_df[1:3])
glimpse(join_min_calories_steps)

#joined minute Steps and Calories Files and separated the Date column for daily data analysis.
min_calories_steps <-
  separate(merge(min_calories_df[1:3], min_steps_df[1:3]), 
           ActivityMinute, into = c('Date','Time', 'AMPM'), sep = ' ')

head(min_calories_steps)

#Joined minute Steps and Calories Files and separated the Date column for daily data analysis.

min_calories_steps <-
  separate(merge(min_calories_df[1:3], min_steps_df[1:3]), 
           ActivityMinute, into = c('Date','Time', 'AMPM'), sep = ' ')

head(min_calories_steps)

#Checked and filtered "0" and "NA" values for "Total Daily Steps".

head(select(joined_METs_activity,Id:TotalSteps
) %>%
  group_by(Id) %>%
  arrange(-TotalSteps))

joined_table <- joined_METs_activity %>%
  filter(!TotalSteps %in% c(0, NA))

glimpse(select(joined_table,Id:TotalSteps) %>%
          group_by(Id) %>%
          arrange(-TotalSteps))

# Total sleep in bed
Sleep <- mutate(Sleep, Minutes_Awake_in_bed = TotalTimeInBed - TotalMinutesAsleep)

awake_in_bed_duration_55 <- Sleep %>% 
  filter(Minutes_Awake_in_bed >= 55) %>% 
  group_by(Id) %>% 
  arrange(-Minutes_Awake_in_bed)
glimpse(awake_in_bed_duration_55)

# total minute awake in bed
awake_in_bed_duration_55 %>%  select(Id,SleepDate,Minutes_Awake_in_bed)

awake_in_bed_duration_55count <- awake_in_bed_duration_55 %>%
  count(Id)
awake_in_bed_duration_55count

#Heart Rate by seconds Data:
'''Daily minimum and maximum heart rates of the users were examined. The number of distinct users was
found to be 14'''

daily_heart_rate <- heart_rate_seconds %>%
  group_by(Id,Date) %>%
  summarise(max_hr=max(Value),min_hr=min(Value)) %>%
  mutate(min_hr , max_hr)
view((daily_heart_rate))

#Number of times each user met the goal during the course of the study, is printed in column "n".

ID_count_FAM <- joined_table %>%
  filter(FairlyActiveMinutes >= 21.4) %>% 
  group_by(Id) %>% 
  count(Id)
ID_count_FAM

#users met the 'Daily Vigorous Activity Minutes' goal.

ID_count_VAM <- joined_table %>% 
  filter(VeryActiveMinutes >= 10.7) %>% 
  group_by(Id) %>% 
  count(Id)
ID_count_VAM

AZM <-  joined_table %>% 
  filter(FairlyActiveMinutes>=21.4| VeryActiveMinutes>=10.7 ) %>% 
  group_by(Id) %>% 
  select(Id,FairlyActiveMinutes,VeryActiveMinutes)
AZM

#The number of users who either met the daily Fairly(Moderate) or the Very Active Minutes are found
ID_count_AZM <-AZM %>% 
  group_by(Id) %>% 
  count(Id)
ID_count_AZM

Active_minutes <- joined_table %>% 
  gather(key=Activity_level,value= Minutes,ends_with("minutes")) %>% 
  select(Id, ActivityDate,Calories, Activity_level, Minutes)
head(Active_minutes)

#to list the number of weight data during the study period, by each user.
Weight %>% group_by(Id) %>% filter(n()>=1) %>% summarise("# of weight entries per user"  =n())

Weight %>%
  group_by(Id) %>%  
  filter(IsManualReport == "FALSE")  %>%
  select(Id, IsManualReport) %>%
  summarise("# of weight entries via a connected scale"=n()) %>%
  distinct()

# Share METs data:

mean_METs_vs_total_steps <- ggplot(joined_table, mapping = aes(x=Mean_METs_daily, y=TotalSteps)) + 
  geom_jitter(aes(size=TotalSteps), alpha=1/3, color= "red", na.rm = TRUE, show.legend = FALSE) +
  geom_smooth(method = 'loess',formula = 'y ~ x', na.rm = TRUE) +
  labs(title = "FitBit:Daily Mean METs and Steps", subtitle="Data from 27 Users", caption ="Data Collected by Mobius") +
  annotate("text", x=20,  y=34000, label="User # 1503960366 has the maximum daily steps with 36,019", color= "purple", size=4)+
  theme(axis.text.x = element_text(angle = 0, size = 12))
mean_METs_vs_total_steps

#Plot Daily minutes awake in bed was plotted to visualize the time spent awake in bed.
Minutes_awake <- ggplot(data = Sleep,aes(x=TotalMinutesAsleep, y=Minutes_Awake_in_bed)) + 
  geom_jitter(aes(size= Minutes_Awake_in_bed), alpha= 1/3, color = "purple", show.legend = FALSE) +
  geom_hline(aes(yintercept =55), size=2, linetype=3) +
  geom_smooth(method = "loess", formula = y ~ x, se=FALSE) +
  labs(x = "Total minutes Asleep", y = "Minutes Awake in Bed", title = "FitBit: Daily Minutes Spent in Bed vs. Minutes Awake In Bed",
       color = "Sleep Type", caption="Data Collected by Mobius")+
  annotate("text", x=500, y=350, label="Longest time awake spent in bed is 371 min ~approx. 6 hrs", color="red",fontface="italic", size=4)+
  annotate("text",x=100,y=75, label="55 minutes", size=4)
Minutes_awake

#Plot Daily Intensities and Calories:

daily_intensities_calories <- ggplot(data = joined_table) + 
  geom_jitter(mapping=aes(x=Calories, y=FairlyActiveMinutes), color = "magenta", alpha = 1/3) +
  geom_smooth(method = 'loess',formula =y ~ x, mapping=aes(x=Calories, y=FairlyActiveMinutes, color=FairlyActiveMinutes), color = "magenta") +
  
  geom_jitter(mapping=aes(x=Calories, y=VeryActiveMinutes), color = "green", alpha = 1/3) +
  geom_smooth(method = 'loess',formula =y ~ x,mapping=aes(x=Calories, y=VeryActiveMinutes, color=VeryActiveMinutes), color = "green") +
  annotate("text", x=4800, y=220, label="Very Active", color="black", size=3)+
  annotate("text", x=4500, y=0, label="Fairly(moderately) Active", color="black", size=3)+
  labs(x = "Daily Calories", y = "Activity Minutes", title = "FitBit: Daily Calories vs.Vigorous & Moderate activity minutes", caption="Data Collected by Mobius")
daily_intensities_calories

#Plot Daily Steps and Calories:

daily_steps_vs_Calories <- ggplot(data = joined_table, mapping = aes(x=Calories, y=TotalSteps)) +
  geom_point(alpha=1/3,  size= 3, color= "red", na.rm = TRUE) +
  geom_smooth(method = 'loess', formula = y ~ x, na.rm = TRUE) +
  labs(title = "FitBit:Daily Steps vs. Calories", subtitle=paste0("Data from ",     str_c(n_distinct(joined_table$Id)," Users")), caption ="Data Collected by Mobius") +
  annotate("text", x=2700,  y=34000, label="Maximum Daily Total Steps are 36,019", color= "purple", size=4)+
  theme(axis.text.x = element_text(angle = 0, size = 12))
daily_steps_vs_Calories

#Hourly Comparative Steps Trend:
n_steps <-str_c(n_distinct(hourly_steps$Id))
hourly_steps <- rename(hourly_steps,TotalSteps = StepTotal)

p_hourly_steps <- 
  ggplot(data=hourly_steps, mapping=aes(x=Hour, y=TotalSteps, fill=Hour)) + 
  geom_bar(stat = "identity")+
  labs(title = "FitBit:Hourly Steps Trend", subtitle=paste0("Data from ", n_steps ," Users"), caption ="Data Collected by Mobius") +
  theme(axis.text.x = element_text(angle = 90, size = 8),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position = "none")
p_hourly_steps

#Hourly Steps by Week-day, Trend
n_activity <-str_c(n_distinct(activity_DT$Id))

plot_total_distance_by_wk_day <- activity_DT  %>%  
  group_by(Wk_Day=wday(ActivityDate, label = TRUE, week_start = 1)) %>%
  summarise(TotalDistance=sum(TotalDistance)) %>%  
  
  ggplot(activity_DT, mapping=aes(y=TotalDistance ,x=Wk_Day, fill=Wk_Day))+
  geom_bar(stat="identity")+
  labs(title = "FitBit: Activity Distance Trend by Week-Day", subtitle= paste0("Data from ",n_activity, " Users"), caption ="Data Collected by Mobius") +
  theme(axis.text.x = element_text(angle = 0, size = 8),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position = "none")
plot_total_distance_by_wk_day

#Activity_Level Minutes
#The Activity_level minutes graph shows that the users were active mostly in the Sedentary Level.
#Red dots show the mean values, whereas the black horizantal lines, the median.
p1 <- ggplot(data=Active_minutes,aes(x=Minutes,y=Activity_level)) +
  geom_boxplot(aes(fill=Activity_level))+
  stat_summary(fun = mean, geom="point", color="darkred", size=2)+
  scale_y_discrete(limits=c("SedentaryMinutes","LightlyActiveMinutes","FairlyActiveMinutes","VeryActiveMinutes"))+
  scale_x_continuous(breaks=seq(0, 1500, 100))+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 0, size = 8), legend.position="top")+
  labs(x = "Minutes of Activity", y = "Activity Level", title = "Minutes of Activity by Activity Level", caption="Data Collected by Mobius")
p1

'''Key Findings and Summary:

1- Looking from the functionality perspective, Bellabeat Leaf tracker is the device similar to the FitBit trackers whose data is analyzed in this project. Therefore, the usage trends analyzed during the project can well be utilized by the Bellabeat upper management for reshaping their marketing strategy.

2- Based on the data on the Daily Sleep patterns, users may be notified accordingly to improve their sleep quality.

3- Re-marketing the discontinued Integrated scale to automate the weight and BMI data synchronization for tracking practicality would add value to the business.'''
