library(tidyverse)
data()
BOD
#ggplot(data=BOD,
      # mapping=aes(x=Time,
                   y= demand))+
  #geom_point(size=5)+
 # geom_line(colour="red")
  
ggplot(BOD, aes(Time,demand))+
  geom_point(size=3)+
  geom_line(colour="red")
data()
names(CO2)
view(CO2)

CO2 %>% 
  ggplot(aes(conc,uptake,
         colour=Treatment))+
  geom_point(size=3, alpha=0.5)+
  geom_smooth(method=lm,se=F)+
  facet_wrap(~Type)+
  labs(tittle="Concentration of CO2")+
  theme_bw()


View(mpg)

mpg
levels(mpg$drv)

names(mpg)

mpg %>% 
  filter(cty<25) %>% 
  ggplot(aes(displ,cty))+
  geom_point(aes(colour=drv,
                 size=trans),
             alpha=0.5)+
  geom_smooth(method=lm)+
  facet_wrap(~year,nrow=1)+
  labs(x="Engine",
       y="MPH",
       title="Fuel Eff")+
  theme_classic()
library(dplyr)  
data()
msleep
BOD
local_BOD=tbl_df(BOD)
local_BOD
print(local_BOD,n=4)
print(local_BOD,n=2)

local_BOD %>% 
  filter(demand==19.8|demand==2)

select(local_BOD,Time)
local_BOD[local_BOD,local_BOD$Time==2]

View(msleep)
df=data.frame(msleep)
head(df)

view(df)
na.omit(df)

df %>% 
  filter(sleep_total==12.1)
filter(df,genus==12.1)

select(df,order:awake, contains("sleep"))

df %>%
  na.omit(df) %>% 
  select(sleep_total) %>% 
  filter(sleep_total<=5) %>% 
  ggplot(aes(sleep_total))+
  geom_histogram(bins = 2)+
  theme_bw() 

names(df)
na.omit()
filter(select(df,order,sleep_total),sleep_total<5)
filter(select())

df3=filter(select(df,sleep_rem,awake,bodywt),bodywt>0.019)
df3 %>% 
  na.omit(sleep_rem) %>% 
  select(awake,bodywt) %>% 
  filter(awake>14 &bodywt<70) %>% 
  ggplot(aes(bodywt,awake))+
  geom_line()

df3[order(df3$bodywt),c("sleep_rem","awake","bodywt")]


flights <- df3 %>% 
  na.omit() %>% 
  select(sleep_rem,bodywt) %>%
  filter(bodywt<100) %>% 
  mutate(Obesity=bodywt/sleep_rem*1000)
flights  

df3 %>% 
  filter(sleep_rem<1.8) %>% 
  group_by(sleep_rem) %>% 
  summarise(avg_sleep_rem=mean(sleep_rem,na.rm=TRUE))

names(df)

df %>% 
 na.omit() %>% 
  group_by(order) %>% 
  summarise_each(funs(mean),sleep_rem,awake)


df %>% 
  group_by(order) %>% 
  summarise_each(funs(min(.,na.rm = TRUE),max(.,na.rm = TRUE)),matches("sleep"))

?Inf


df %>% 
  group_by(order) %>% 
  summarise(df_count=n()) %>% 
  arrange(desc(df_count))

df %>% 
  group_by(order) %>% 
  tally(sort=TRUE)


df %>% 
  group_by(conservation) %>% 
  summarise(df_count=n(),order_count=n_distinct(order))

View(Titanic) 

?ggplot2
  
df5 <- data.frame(xyz="a")
df5
df5$x
typeof(df5$x)
df6 <- tibble(xyz="a")
df6
is.data.frame(df6[,"xyz"])

?nycflights13

nycflights13::weather
str(mtcars)
View(mtcars)
mtcars %>% 
  ggplot(aes(vs,mpg)) +
  geom_point()+
  geom_line() %>%
  ggsave("mtcars.pdf", width=8, height=6)

names(mtcars)

library(rvest)
library(purrr)
library(readr)
library(dplyr)
library(lubridate)

mtcars
library(purrr)
mean <- map_dbl(mtcars<mean)


?Titanic

titanic <- read.csv("Titanic.csv", stringsAsFactors = False)

titanic1 <- as.data.frame(Titanic)

titanic1

titanic1$Sex <- as.factor(titanic1$Sex)
titanic1$Class <- as.factor(titanic1$Class)
titanic1$Survived <- as.factor(titanic1$Survived)
titanic1$Age <- as.factor(titanic1$Age)

str(titanic1)

library(ggplot2)


#What was the survival rate of all passengers?

ggplot(titanic1,aes(x=Survived))+
  theme_bw()+
  geom_bar()+
  labs(y ="Survival rates",
       x="Survival fate",
       title="Titanic Survical Rates")



#Q.2 What is the survival rate by sex?

ggplot(titanic1,aes(x=Sex, fill= Survived))+
  theme_bw()+
  geom_bar()+
  labs(y ="Survival rates",
       x="Survival fate",
       title="Titanic Survical Rates by Sex")

# What was the survival rate by class of ticket?


ggplot(titanic1,aes(x=Class, fill= Survived))+
  theme_bw()+
  geom_bar()+
  labs(y ="Survival rates",
       x="Survival fate",
       title="Titanic Survical Rates by Class")

# What was the survical rate by class of ticket and gender

ggplot(titanic1,aes(x=Sex, fill= Survived))+
  theme_bw()+
  geom_bar()+
  facet_wrap(~Class)+
  labs(y ="Survival rates",
       x="Survival fate",
       title="Titanic Survical Rates by Class and Sex")


# What is the frequency distribution of passengers

ggplot(titanic1,aes(x=Freq, na.rm=TRUE, fill= Survived))+
  theme_bw()+
  geom_histogram(binwidth =10)+
  labs(y ="Survival rates",
       x="Survival fate",
       title="Titanic Frequency Distribution")
ggsave("titanic1.pdf")



mouse.data <- data.frame(
  weight = c(0.9,1.8,2.4,3.5,3.9,4.4,5.1,5.6,6.3),
  size=c(1.4,2.6,1.0,3.7,5.5,3.2,3.0,4.9,6.3))

mouse.data
plot(mouse.data$weight, mouse.data$size)
 mouse.regr <- lm(size~weight, data=mouse.data)
summary(mouse.regr)

abline(mouse.regr, col="blue")


data <- mpg

levels(data$drv)

BOD

mpg


levels(mpg$cyl)

data()
msleepN
str(msleep)

starwars

install.packages("rmarkdown")
install.packages("tinytex")

Data()

? BOD

data ()

view(BOD)


library(tidyverse)


