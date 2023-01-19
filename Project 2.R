library(tidyverse)
data()
head(BOD)

BOD %>% 
  lm(demand~Time, data=.) %>% 
  summary()
  
mod <- lm(demand~Time, data=BOD)
mod
summary(mod)
attributes(mod)

mod$residuals

# model prediction

new_speed <- data.frame(Time= c(10,15,20))
View(new_speed)
predict(mod,new_speed) %>%
  round()


BOD %>% 
  lm(demand~Time, data=.) %>%
  predict(data.frame(Time=c(10,15,20))) %>% 
  round()



# t-test

install.packages("patchwork")

library(tidyverse)
library(patchwork)
library(gapminder)


data()
view(gapminder)

# i. One-sided t-test
# Hypothesis testing
# Ho: Life expectancy in Africa is equal to 50   
#Hi : Life expectancy in Africa not equal to 50

gapminder %>% 
  filter(continent== "Africa") %>% 
  select(lifeExp) %>% 
  t.test(mu=50)

# Conclusion: reject null hypothesis and accept null hypothesis that the life expectancy in Africa is not equal to 50( The life expectancy is statistically significant, i.e p value 0.002038 < 0.05 


# Two-sided t-test for difference of means
# Hypothesis testing
# Ho: The life expectancy in Africa and Europe are equal  
#Hi : The life expectancy in Africa and Europe are not equal 

gapminder %>% 
  filter(continent%in% c("Africa","Europe")) %>% 
  t.test(lifeExp~continent, data=.,
         alternative = "two.sided")

# Conclusion: There is a difference in life expectancy in Africa Europe ( The difference in life expectancy is statistically significant, i.e p value 2.2e-16 < 0.05 )



# one-sided test for difference of means

#Hypothesis testing:
# Ho: The life expectancy in Ireland and Switzerland are equal/ The difference is zero  
#Hi : The life expectancy in Ireland and Switzerland are not equal/ The difference is not zero  
gapminder %>% 
  filter(country %in% c("Ireland", "Switzerland")) %>% 
  t.test(lifeExp~country,data=.,
         alternative = "less",
         conf.level = 0.95)

# Conclusion: There is no difference in life expectancy in Ireland and Switzerland (P- value 0.05835> 0.05). We fail to reject the null hypothesis



# Paired t-test ( mostly used in medical research)

gapminder %>% 
  filter(year %in% c(1957,2007)& continent=="Africa") %>%
  mutate(year=factor(year, levels= c(2007,1957))) %>% 
  t.test(lifeExp~year, data=.,
         paired = TRUE)


var(gapminder$lifeExp[gapminder$country== "Switzerland"])
var(gapminder$lifeExp[gapminder$country== "Ireland"])

#Assumptions of t-test
# i. large representative samples
# ii. Values are normally distributed
# iii. Two samples have very similar variance
  
  

