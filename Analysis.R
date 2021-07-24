data <- read.csv(file = 'Data_NHANES_2013_2018_Master_File_051921.csv')

library(tidyr)
library(dplyr)

#split into age groups

data_youth <- data[data$age <= 11 & data$age >= 6,]
data_teens <- data[data$age <= 17 & data$age >= 12,]


#1a 
data_youth$chage319 <- rep(1, nrow(data_youth))


#i nonsmokers

#1 & 2

data_youth_nosmoke <- subset(data_youth, cursmk30filt == 0 & curtob5filt == 0 & scotfilt == 1)
data_teens_nosmoke <- subset(data_teens, cursmk30filt == 0 & curtob5filt == 0 & scotfilt == 1)


#data_youth_by_wave <- split(data_youth_nosmoke, f=data_youth$WAVE)
#data_teen_by_wave <- split(data_teens_nosmoke, f=data_teens$WAVE)
#2 
two_a <- data_youth_nosmoke %>% group_by(race, hometse) %>%  summarise(n = n()) %>% mutate(freq = n / sum(n), percentage = freq*100)
two_b <- data_youth_nosmoke %>% group_by(race, tserest) %>%  summarise(n = n()) %>% mutate(freq = n / sum(n), percentage = freq*100)
two_c <- data_youth_nosmoke %>% group_by(race, tsecar) %>%  summarise(n = n()) %>% mutate(freq = n / sum(n), percentage = freq*100)
two_d <- data_youth_nosmoke %>% group_by(race, tseothhme) %>%  summarise(n = n()) %>% mutate(freq = n / sum(n), percentage = freq*100)
two_e <- data_youth_nosmoke %>% group_by(race, tseothind) %>%  summarise(n = n()) %>% mutate(freq = n / sum(n), percentage = freq*100)
two_f <- data_youth_nosmoke %>% group_by(race, tsecomp) %>%  summarise(n = n()) %>% mutate(freq = n / sum(n), percentage = freq*100)

two_a_teens <- data_teens_nosmoke %>% group_by(race, hometse) %>%  summarise(n = n()) %>% mutate(freq = n / sum(n), percentage = freq*100)
two_b_teens <- data_teens_nosmoke %>% group_by(race, tserest) %>%  summarise(n = n()) %>% mutate(freq = n / sum(n), percentage = freq*100)
two_c_teens <- data_teens_nosmoke %>% group_by(race, tsecar) %>%  summarise(n = n()) %>% mutate(freq = n / sum(n), percentage = freq*100)
two_d_teens <- data_teens_nosmoke %>% group_by(race, tseothhme) %>%  summarise(n = n()) %>% mutate(freq = n / sum(n), percentage = freq*100)
two_e_teens <- data_teens_nosmoke %>% group_by(race, tseothind) %>%  summarise(n = n()) %>% mutate(freq = n / sum(n), percentage = freq*100)
two_f_teens <- data_teens_nosmoke %>% group_by(race, tsecomp) %>%  summarise(n = n()) %>% mutate(freq = n / sum(n), percentage = freq*100)


#3	Run a model assessing the interaction of race*home TSE with serum cotinine (quantitative var name: scot) *note log-transform scot if needed
library(car)

model1 <- aov(scot ~ race*hometse, data_youth_nosmoke)
summary(model1)
plot(model1)

#res vs fitted good
# but not normal

model2 <- aov(log(scot) ~ race*hometse, data_youth_nosmoke)
summary(model2)
plot(model2)

#better 

#the interaction is significant 


# running it again for teens
model3 <- aov(scot ~ race*hometse, data_teens_nosmoke)
summary(model3)
plot(model3)

#not normal 

model4 <- aov(log(scot) ~ race*hometse, data_teens_nosmoke)
summary(model4)
plot(model4)

#4 Run similar models assessing the interaction of race with tsecomp predicting scot?
model5 <- aov(scot~race*tsecomp, data_youth_nosmoke)
summary(model5)

#check model assumptions 
plot(model5) #not normal


model6 <- aov(log(scot)~race*tsecomp, data_youth_nosmoke)
summary(model6) # interaction sig
#check plots
plot(model6) #looks good

#repeat for teens
model7 <- aov(scot ~ race*tsecomp, data_teens_nosmoke)
summary(model7)
plot(model7) #not normal

model8 <- aov(log(scot) ~ race*tsecomp, data_teens_nosmoke)
summary(model8) #not signficant 

plot(model8)
shapiro.test(sqrt(data_teens_nosmoke$scot)) #still not normal

#bc <- boxCox(log(scot) ~ race*tsecomp, data_teens_nosmoke) ???

