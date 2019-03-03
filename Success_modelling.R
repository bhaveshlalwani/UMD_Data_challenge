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

#Regression building
reg <- glm(started ~ as.factor(CC) + as.factor(Attended.Group.Training.) + Total.Counseling.Time..hrs
                    + as.factor(Owner.s.Hispanic.Origin) 
                    +as.factor(Ownership.Gender) + Company.s.Total.employees + Company.s.Gross.Revenue...
                    ,data = raw_regdata, family = binomial(link = "logit"), control = list(maxit = 50))

summary(reg)
predict.glm(reg)
head(raw_regdata)


#Aggregating some variables - only gender and NAICS makes sense.

raw_regdata$Male <- if_else(raw_regdata$Ownership.Gender == "Male-Owned",1,0)
table(raw_regdata$Male)

reg1 <- glm(started ~ as.factor(CC) + as.factor(Attended.Group.Training.) + Total.Counseling.Time..hrs
                      + Company.s.Total.employees + Company.s.Gross.Revenue...
                     ,data = raw_regdata, family = binomial(link = "logit"), control = list(maxit = 50))

summary(reg1)

write.csv(raw_regdata,"Success_reg_data.csv")

#aggregating and Checking Industry code

ind_data <- table(NAICS.code, raw_regdata$started)
ind_data$rate <- ind_data[,"1"]/ind_data[,"0"]

temp <- as.data.frame(ind_data$rate)
temp <- add_rownames(temp, c("Industry", "rate"))
quantile(temp$`ind_data$rate`)
  #0%   25%  50%  75%   100% 
  #0.0 0.30 0.33 0.39  0.600
high_ind <- temp%>%filter(`ind_data$rate`>= 0.39) %>% select(Industry)
raw_regdata$HI <- if_else(NAICS.code %in% high_ind$Industry, 1,0)

reg2 <- glm(started ~ as.factor(CC) + as.factor(Attended.Group.Training.) + Total.Counseling.Time..hrs
            + old_emp + Company.s.Gross.Revenue... + HI + Male
            ,data = raw_regdata, family = binomial(link = "logit"), control = list(maxit = 50))

summary(reg2)


#Trying balanced data set






