library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)
library(jtools)
library(stargazer)

# set working directory to downloads
mps <- read.csv("Mother Jones - Mass Shootings Database, 1982 - 2023 - Sheet1.csv")

mps %>% 
  View()

mps %>% 
  count(weapons_obtained_legally, sort = TRUE) %>%
  filter(!weapons_obtained_legally %in% c("Kelley passed federal criminal background checks; the US Air Force failed to provide information on his criminal history to the FBI", 
                                          "TBD", "\nYes", "-")) %>% 
  mutate(weapons_obtained_legally = ifelse(weapons_obtained_legally=="yes","Yes", weapons_obtained_legally)) %>% 
  mutate(weapons_obtained_legally = ifelse(weapons_obtained_legally=="Yes ","Yes", weapons_obtained_legally))

mps %>%
  filter(year != 2023) %>% 
  count(year, sort = TRUE) %>% 
  ggplot(aes(year, n)) +
  geom_line(lwd = 2) +
  theme(axis.title.x = element_blank()) +
  theme_bw() +
  labs(y = "Shootings per Year")

mps %>% 
  count(fatalities) %>% 
  group_by(., year)

mps %>% 
  count(fatalities) %>%  
  ggplot(aes(fatalities, n)) +
  geom_col(fill = "red", col = "black")

# make a data set that groups by year
grp_mps <- mps %>% 
  group_by(year) %>% 
  summarise(sum(fatalities))

grp_mps %>% 
  ggplot(aes(year, `sum(fatalities)`)) +
  geom_smooth(se = F, col = "black") +
  geom_col(width = 1, fill = "darkred", col = "black")

grp_mps %>% 
  View()

mps_mean <- mps %>% 
  group_by(year) %>% 
  summarise(mean(fatalities))
  
mps_mean %>% 
  View()

#incidents
plot1 <- mps %>%
  filter(year != 2023) %>% 
  count(year, sort = TRUE) %>% 
  ggplot(aes(year, n)) +
  geom_col() +
  labs(y = "incidents")

plot1

#fatalities
plot2 <- mps_mean %>%
  filter(year != 2023) %>% 
  ggplot(aes(year, `mean(fatalities)`)) +
  geom_col(fill = "darkred", col = "black") +
  theme(axis.title.x = element_blank()) +
  labs(y = "Mean Fatalities per Shooting")

plot2

plot1 + plot2

mps %>% 
  filter(fatalities >= 4) %>% 
  filter(year != 2023) %>% 
  count(year, sort = TRUE) %>% 
  ggplot(aes(year, n)) +
  geom_col(fill = "darkred", col = "black") +
  labs(y = "Shootings per Year") +
  theme(axis.title.x = element_blank())

# Shootings 

#by state
mps %>%
  separate(location, c("city", "state"), sep = ", ",
           fill = "right") %>%
  count(state, sort = TRUE) %>% 
  ggplot(aes(reorder(state, -n), n)) + 
  geom_col(fill = "darkred", col = "black") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(y = "incidents", title = "MS incidents by state")

#% with mental health problems/signs
#cleaning upn this column, since its free input
mps %>%
  filter(!prior_signs_mental_health_issues %in% c("TBD", "Unknown")) %>% 
  mutate(prior_signs_mental_health_issues = ifelse(prior_signs_mental_health_issues=="yes", "Yes", prior_signs_mental_health_issues)) %>%
  mutate(prior_signs_mental_health_issues = ifelse(prior_signs_mental_health_issues=="Unclear ", "Unclear", prior_signs_mental_health_issues)) %>%
  filter(prior_signs_mental_health_issues != "-") %>% 
  count(prior_signs_mental_health_issues, sort = TRUE)

# of MS incidents with known data, 
# 68 had signs/mental health problems (62%)
# 24 the case was not clear (22%)
# 17 the perpetrator did not (16%)

# how to delete a row
mps1 <- mps[!(row.names(mps) %in% c("3", "53")),]

plot(fatalities ~ injured, data = mps1)

mps1 %>%
  mutate(gender = ifelse(gender=="M", "Male", gender)) %>%
  mutate(gender = ifelse(gender=="F", "Female", gender)) %>% 
  count(gender, sort = TRUE)

mps %>% 
  mutate(location.1=ifelse(location.1=="religious","Religious", location.1)) %>%
  mutate(location.1=ifelse(location.1=="workplace","Workplace", location.1)) %>%
  mutate(location.1=ifelse(location.1=="\nWorkplace","Workplace", location.1)) %>%
  mutate(location.1=ifelse(location.1=="Other\n", "Other", location.1)) %>%
  filter(location.1 != "Other") %>% 
  count(location.1, sort = TRUE) %>% 
  ggplot(aes(reorder(location.1, -n), n)) +
  geom_col() +
  labs(y = "incidents", title = "Mass shootings by location, 1982 - 2023",
       subtitle = "Data from Mother Jones") +
  theme(axis.title.x = element_blank())

mps %>% 
  mutate(race = ifelse(race=="white", "White", race)) %>%
  mutate(race = ifelse(race=="White ", "White", race)) %>% 
  mutate(race = ifelse(race=="black", "Black", race)) %>%
  filter(!(race %in% c("Unclear", "-", "unclear"))) %>% 
  count(race, sort = TRUE) %>% 
  ggplot(aes(reorder(race, -n), n)) +
  geom_col() +
  theme(axis.title.x = element_blank())

library(usmap)

mps %>% 
  View()

mps_us <- mps[-(1:17),]

mps_us$longitude <- as.numeric(mps_us$longitude)
mps_us$latitude <- as.numeric(mps_us$latitude)

mps_us %>% 
  View()


mps_us %>% 
  filter(between(longitude, -150, -50)) %>% 
  filter(latitude != "-") %>% 
  mutate(race = ifelse(race=="white", "White", race)) %>%
  mutate(race = ifelse(race=="White ", "White", race)) %>% 
  mutate(race = ifelse(race=="black", "Black", race)) %>%
  filter(!(race %in% c("Unclear", "-", "unclear"))) %>% 
  ggplot(aes(longitude, latitude, color = type)) +
  borders("world", region = "US") +
  geom_point() +
  xlim(-125, -65) +
  ylim(23, 50) +
  theme_bw()

mps %>% 
  filter(latitude != "-") %>% 
  mutate(race = ifelse(race=="white", "White", race)) %>%
  mutate(race = ifelse(race=="White ", "White", race)) %>% 
  mutate(race = ifelse(race=="black", "Black", race)) %>%
  filter(!(race %in% c("Unclear", "-", "unclear"))) %>% 
  ggplot(aes(longitude, latitude)) +
  borders("world", region = "US") +
  geom_point(size = .1, alpha = .25) +
  theme_map()

library(ggthemes)

states_map <- map_data("state")

plot_usmap(data = mps, values = c("longitude", "latitude")) +


mps %>% 
  filter(latitude != "-") %>% 
  range(latitude)

library(usmap)

mps %>% 
  count(type, sort = TRUE)

mps_pre_awb <- mps %>% 
  filter(between(date, ymd(19840912), ymd(199409012)))

over35 <- ifelse(mps$age_of_shooter >= 35, 1, 0)

glm1 <- glm(over35 ~ fatalities, data = mps, 
            family = "binomial")

summ(glm1)

# Do number of fatalities predict race?
mps_race <- mps %>% 
  filter(!(race %in% c("-", "unclear"))) %>% 
  mutate(race = ifelse(race %in% c("white", "White "), "White", race)) %>%
  mutate(race = ifelse(race == "black", "Black", race))

white <- ifelse(mps_race$race == "White", 1, 0)

glm2 <- glm(white ~ fatalities, data = mps_race, family = "binomial")  
summ(glm2)

black <- ifelse(mps_race$race == "Black", 1, 0)

mps_race$injured <- as.numeric(mps_race$injured)

glm3 <- glm(black ~ injured, data = mps_race, family = "binomial")
summ(glm3)

  

