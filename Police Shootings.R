library(stargazer)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(jtools)
#WaPo police shootings
Polshoot=read.csv("~/Downloads/2022-12-12-washington-post-police-shootings-export.csv")

View(Polshoot)
dim(Polshoot) #994 individuals in the sample

#Does Race effect the probability of being unarmed when shot? (2015 data)

#simple bargraph of the number of people shot by the police in 2015
Polshoot %>% 
  count(race, sort = TRUE) %>% 
  ggplot(aes(reorder(race, -n), n)) +
  geom_col() +
  theme(axis.title.x = element_blank())

#Looking at most common weapons used
Polshoot %>% 
  mutate(armed = ifelse(armed=="blunt_object,blunt_object", "blunt_object", armed)) %>%
  mutate(armed = ifelse(armed=="unknown", "undetermined", armed)) %>%
  count(armed, sort = TRUE) %>% 
  filter(n >= 49) %>% 
  ggplot(aes(reorder(armed, -n), n)) +
  geom_col() +
  theme(axis.title.x = element_blank()) +
  ylim(0, 600)

# Looking at police killings by state
Polshoot %>% 
  count(state, sort = TRUE) %>% 
  ggplot(aes(reorder(state, -n), n)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank())

# making a correlation matrix for select variables
Polshoot_cor <- Polshoot %>% 
  mutate(race = ifelse(race=="Black", 1, 0)) %>% 
  mutate(armed = ifelse(armed=="unarmed",1,0)) %>% 
  mutate(body_camera = ifelse(body_camera=="true",1,0)) %>% 
  mutate(flee = ifelse(flee=="not",0,1)) %>% 
  mutate(signs_of_mental_illness = ifelse(signs_of_mental_illness=="true", 1, 0)) %>% 
  mutate(gender = ifelse(gender=="male", 1, 0))

Polshoot_cor %>% 
  View()

# getting variables to make correlation matrix
Polshoot_cor2 <- Polshoot_cor[,c(4:6,9:11)]

Polshoot_cor2 %>% 
  View()

# Spearman's correlation matrix
matrix = cor(Polshoot_cor2, method = "spearman")
round(matrix, 2)
stargazer(matrix)

#ci's for the summary stats
t.test(Polshoot$age)$"conf.int"
t.test(Polshoot_cor$gender)$"conf.int"
t.test(Polshoot_cor$race)$"conf.int"
t.test(Polshoot_cor$flee)$"conf.int"
t.test(Polshoot_cor$body_camera)$"conf.int"
t.test(Polshoot_cor$signs_of_mental_illness)$"conf.int"

#Make a dummy for if the individual is black or not
#1=black,0=not black
library(fastDummies)
black=ifelse(Polshoot$race=="Black",1,0)
black
sum(black)
# There were 258 black people shot and killed by the police in 2015

#make a dummy for if the person had a gun or not
#1=gun,0=no gun
gun=ifelse(Polshoot$armed=="gun",1,0)
gun

#make a dummy for if the person was fleeing or not
#1 = fleeing, 0 = not
flee=ifelse(Polshoot$flee=="not",0,1)
flee

#make a dummy for if the police had a body camera
#1 = yes bodycam, 0=no bodycam
bodycam=ifelse(Polshoot$body_camera=="true",1,0)
bodycam
sum(bodycam) #police only had bodycam footage in 75/994 shootings (7.5%)

#make a dummy for if the person had mental illness or not
#1=yes,0=no
ment_ill=ifelse(Polshoot$signs_of_mental_illness=="true",1,0)
ment_ill
sum(ment_ill) #260/994, or 26% of people cops killed were mentally ill

#age
age=Polshoot$age

#logistic regression
glm1=glm(gun~black+age+flee+bodycam+ment_ill,family=binomial)
summary(glm1)
summ(glm1)
confint(glm1)
#Race=black had no effect on whether the person shot had a gun or not
#z=-0.357, p=0.72
#Body camera had a negative impact (e.g., police who had bodycams
#on were ~41% less likely to kill someone who had a gun)
#People with mental illness were ~35% less likely to have a gun
#when they were killed
#Age had an impact (a 10 year increase in age --> a 15% higher odds
#of having a gun when the individual was killed)

##### RELEVANT SECTION #####

# does "black" affect the probability of being unarmed?

#make a dummy for unarmed
#1 if unarmed, 0 if not
library(fastDummies)
unarmed=ifelse(Polshoot$armed=="unarmed",1,0)
unarmed

male=ifelse(Polshoot$gender=="male",1,0)

glm2=glm(unarmed~black+age+flee+bodycam+ment_ill,family=binomial)
summary(glm2)
summ(glm2)
#black individuals were 61% more likely to unarmed when shot and killed by the
#police compared to non-black individuals

#converting to latex
library(stargazer)
glm3=glm(unarmed~black,family=binomial)
summary(glm3)
glm4=glm(unarmed~black+age+flee,family=binomial)
summary(glm4)
glm2=glm(unarmed~black+age+flee+bodycam+ment_ill,family=binomial)
stargazer(glm3,glm4,glm2)

#black OR and CI
exp(0.47645+1.96*0.23505*c(-1,1)) #95% CI (1.02, 2.55)
exp(0.47645) #1.61

#age OR and CI
exp(-0.03061+1.96*0.01019*c(-1,1)) #95% CI (0.95, 0.99)
exp(-0.0306) #0.97

#flee OR and CI
exp(0.48126+1.96*0.23247*c(-1,1)) #95% CI (1.03, 2.55)
exp(0.48126) #1.62

#bodycam OR and CI
exp(0.67470+1.96*0.34388*c(-1,1)) #95% CI (1.00, 3.85)
exp(0.67470) #1.96

#ment_ill OR and CI
exp(-0.05377+1.96*0.27505*c(-1,1)) #95 CI (0.55, 1.62)
exp(-0.05377) #0.95

#making a forest plot
polshootfp <- data.frame(
  Index = c(1, 2, 3, 4, 5), ## This provides an order to the data
  label = c("black", "age", "flee", "bodycam", "ment_ill"),
  OR = c(1.61, 0.97, 1.62, 1.96, 0.95),
  LL = c(1.02, 0.95, 1.03, 1.00, 0.55),
  UL = c(2.55, 0.99, 2.55, 3.85, 1.62),
  CI = c("1.02, 2.55", "0.95, 0.99","1.03, 2.55","1.00, 3.85", "0.55, 1.62")
)
polshootfp

# forest plot (Figure 2 in the paper)
plot1 <- ggplot(polshootfp, aes(y = Index, x = OR)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:5, labels = polshootfp$label, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))
plot1

# different models, including the one that accounts for interaction effects
model1=glm(unarmed~black, family=binomial)
model2=glm(unarmed~black+age+flee+bodycam+ment_ill, family = binomial)
model3=glm(unarmed~black+age+flee+bodycam+ment_ill+ment_ill*black+
             age*black+flee*black+black*bodycam, family = binomial)
stargazer(model1, model2, model3)
export_summs(model1, model2, model3)

# How do ages differ for black and white subjects
Polshoot %>% 
  filter(race %in% c("Black", "White")) %>% 
  ggplot(aes(race, age)) +
  geom_boxplot(fill = "grey") +
  theme_bw() +
  annotate("text", 1.50, 55, label = "p < 0.0001")

t.test(Polshoot$age[Polshoot$race=="Black"],
       Polshoot$age[Polshoot$race=="White"])

Polshoot2 <- Polshoot %>% 
  filter(race == "Black") %>%
  filter(!is.na(age))

mean(Polshoot2$age[Polshoot2$race=="Black"])

Polshoot %>% 
  View()

#multiple dummy variables for race
library(fastDummies)

Polshoot_cor <- Polshoot %>%
  mutate(armed = ifelse(armed=="unarmed",1,0)) %>% 
  mutate(body_camera = ifelse(body_camera=="true",1,0)) %>% 
  mutate(flee = ifelse(flee=="not",0,1)) %>% 
  mutate(signs_of_mental_illness = ifelse(signs_of_mental_illness=="true", 1, 0)) %>% 
  mutate(gender = ifelse(gender=="male", 1, 0))

Polshoot_cor <- dummy_cols(Polshoot_cor, select_columns = "race")

Polshoot_cor %>% 
  View()

Polshoot_dummy <- Polshoot %>%
  filter(race %in% c("White","Black","Hispanic")) %>% 
  mutate(armed = ifelse(armed=="unarmed",1,0)) %>% 
  mutate(body_camera = ifelse(body_camera=="true",1,0)) %>% 
  mutate(flee = ifelse(flee=="not",0,1)) %>% 
  mutate(signs_of_mental_illness = ifelse(signs_of_mental_illness=="true", 1, 0)) %>% 
  mutate(gender = ifelse(gender=="male", 1, 0))

dim(Polshoot_dummy)

Polshoot_dummy <- dummy_cols(Polshoot_dummy, select_columns = "race")

dummy_model1 <- glm(armed ~ race_Black+race_Hispanic, family="binomial",
                    data = Polshoot_dummy)

dummy_model2 <- glm(armed ~ race_Black+race_Hispanic+age+
        signs_of_mental_illness+flee+body_camera, family="binomial",
        data = Polshoot_dummy)

dummy_model3 <- glm(armed ~ race_Black+race_Hispanic+age+
                    signs_of_mental_illness+flee+body_camera +
                    race_Black*age + race_Black*signs_of_mental_illness+
                    race_Black*flee + race_Black*body_camera+
                    race_Hispanic*age+race_Hispanic*signs_of_mental_illness+
                    race_Hispanic*flee + race_Hispanic*body_camera, 
                    family="binomial",
                    data = Polshoot_dummy)

summ(dummy_model3)

stargazer(dummy_model1, dummy_model2)


Polshoot %>% 
  filter(race %in% c("White", "Black", "Hispanic")) %>% 
  filter(armed=="unarmed") %>% 
  count(race, sort = TRUE) %>% 
  ggplot(aes(reorder(race, -n),n)) +
  geom_col() +
  theme_bw() +
  theme(axis.title.x = element_blank())

unarmed_table <- table(Polshoot$race, Polshoot$armed=="unarmed")
unarmed_table
stargazer(unarmed_table)

# comparing proportion unarmed by race
#blacks and whites
prop.test(x=c(38,31),n=c(258, 502),
          correct = FALSE)

#hispanics and whites
prop.test(x=c(21,31),n=c(174,502),
          correct = FALSE)

#natives and whites
prop.test(x=c(1,31),n=c(10,502))

summ(dummy_model1)

range(Polshoot2$age[Polshoot2$race=="Black"])
mean(Polshoot2$age[Polshoot2$race=="Black"])

model4 <- glm(unarmed~black+age+black*age, family = binomial,data = Polshoot)

summary(model4)

#non-black
exp(-0.70485 - .05046*20) / (1 + exp(-0.70485 - .05046*20)) # 20
exp(-0.70485 - .05046*25) / (1 + exp(-0.70485 - .05046*25)) # 25
exp(-0.70485 - .05046*30) / (1 + exp(-0.70485 - .05046*30)) # 30
exp(-0.70485 - .05046*35) / (1 + exp(-0.70485 - .05046*35)) # 35
exp(-0.70485 - .05046*40) / (1 + exp(-0.70485 - .05046*40)) # 40

# black 
exp(-0.70485 - 1.111360 - .05046*20 + 0.05179*20) / (1 + exp(-0.70485 - 1.111360 - .05046*20 + 0.05179*20)) # 20
exp(-0.70485 - 1.111360 - .05046*25 + 0.05179*25) / (1 + exp(-0.70485 - 1.111360 - .05046*25 + 0.05179*25)) # 25
exp(-0.70485 - 1.111360 - .05046*30 + 0.05179*30) / (1 + exp(-0.70485 - 1.111360 - .05046*30 + 0.05179*30)) # 30
exp(-0.70485 - 1.111360 - .05046*30 + 0.05179*30) / (1 + exp(-0.70485 - 1.111360 - .05046*30 + 0.05179*30)) # 35
exp(-0.70485 - 1.111360 - .05046*40 + 0.05179*40) / (1 + exp(-0.70485 - 1.111360 - .05046*40 + 0.05179*40)) # 40


exp(cbind(OR=coef(model4), confint(model4)))

dat <- data.frame(prob = c(0.153, 0.123, 0.098, 0.078, 0.062,
                           0.143, 0.144, 0.145, 0.146, 0.146),
                  age = rep(c(20, 25, 30, 35, 40), 2),
                  race = c(rep("non-black", 5), rep("black", 5)))

dat %>% 
  ggplot(aes(age, prob, color = race)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(y = "prob unarmed")

Polshoot_age <- Polshoot %>%
  filter(!is.na(age))

mean(Polshoot$age, na.rm = TRUE)

summary(model3)

model5 <- glm(unarmed ~ black + ment_ill + age + black * ment_ill, family = binomial, data = Polshoot)

# mental health co-morbidities of average age (black)
exp(-1.135 + 0.238 - 0.572 - 0.033*36.69 + 1.334) / (1 + exp(-1.135 + 0.238 - 0.572 - 0.033 + 1.334))

# mental health co-morbidities of average age (non-black)
exp(-1.135 - 0.572 - 0.033*36.69) / (1 + exp(-1.135 - 0.572 - 0.033*36.69))

