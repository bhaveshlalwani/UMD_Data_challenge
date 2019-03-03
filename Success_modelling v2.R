rm = (list = ls())

library(dplyr)
library(tidyverse)
#library(DataExplorer)


raw <- read.csv("tempraw.csv", stringsAsFactors = FALSE)
raw <- raw[1:17078,]

colnames(raw)


nrow(raw)
head(raw)
tail(raw)
str(raw)

temp <- raw %>% filter(Impact..Started.Business %in% c("Yes","No"))
temp <- temp %>% filter(!(Ownership.Gender=="Choose not to respond")) 
temp <- temp %>% filter(!(Owner.s.Race=="Choose not to respond"))
raw_regdata <- temp %>% filter(!(Owner.s.Race=="Choose not to respond"))
raw_regdata <- raw_regdata %>% filter(!(Owner.s.Hispanic.Origin == "Choose not to respond"))

raw_regdata$old_emp <- raw_regdata$Company.s.Total.employees - raw_regdata$Impact..Created.New.Jobs
nrow(raw_regdata)
table(raw_regdata$Business.Status)
table(raw_regdata$Impact..Started.Business)


attach(raw_regdata)

table(Owner.s.Hispanic.Origin, Impact..Started.Business)
table(Owner.s.Hispanic.Origin, Impact..Started.Business)
plot(table(Owner.s.Hispanic.Origin, Impact..Started.Business))
table(County, Impact..Started.Business)
plot(table(Owner.s.Race, Impact..Started.Business))

raw_regdata$started <- as.numeric(if_else(raw_regdata$Impact..Started.Business == "Yes", 1, 0))
table(raw_regdata$started)

#Creating factor variable for County/Service center

x<-as.data.frame(table(ï..Service.Center)) 
quantile(x$Freq)
y <- as.data.frame(table(County)) 
quantile(y$Freq)
temp <- y%>%filter(Freq>475)
high_counties <- temp%>%select(County)
raw_regdata$CC <- if_else(County %in% high_counties$County, 1,0)
table(raw_regdata$CC)

#Feature Engineering
raw_regdata$County <- as.factor(raw_regdata$County)
raw_regdata$County <- relevel(raw_regdata$County, ref = "Prince Georges")
raw_regdata$Ownership.Gender <-  as.factor(raw_regdata$Ownership.Gender)
raw_regdata$Ownership.Gender <- relevel(raw_regdata$Ownership.Gender, ref = "Woman-Owned")
raw_regdata$Owner.s.Hispanic.Origin <-  as.factor(raw_regdata$Owner.s.Hispanic.Origin)
raw_regdata$Owner.s.Hispanic.Origin <-  relevel(raw_regdata$Owner.s.Hispanic.Origin, ref = "Hispanic")
raw_regdata$Owner.s.Race <- as.factor(raw_regdata$Owner.s.Race)
raw_regdata$Owner.s.Race <- relevel(raw_regdata$Owner.s.Race, ref = "Black or African American")

#Regression building

reg <- glm(started ~ as.factor(County) + as.factor(Attended.Group.Training.) + old_emp + NAICS.code 
                    +as.factor(Initial.Services.Sought.at.First.Visit)
                    + as.factor(Owner.s.Hispanic.Origin) +as.factor(Ownership.Gender) 
                    +as.factor(Owner.s.Race) + Total.Counseling.Time..hrs
                    ,data = raw_regdata, family = binomial(link = "logit"), control = list(maxit = 50))

summary(reg)
predict.glm(reg)
head(raw_regdata)






