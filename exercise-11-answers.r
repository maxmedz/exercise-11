# ==============================================================================
#  ____  _____ _____
# |    \|   __| __  |  Data Science with R
# |  |  |__   |    -|  Fall 2024
# |____/|_____|__|__|
#
# Economic insecurity and religious reassurance (ESS) -- student answer script
#
# Group number:
# Student 1:
# Student 2:
# Student 3:
# Date:
#
# Answers to questions
# --------------------
#
# [ANSWER 2.1]: 53.51
# [ANSWER 2.1]: 19.36
# [ANSWER 2.1]: 1.53
#
# [ANSWER 3]: m1<-lm(rlgatnd~age+sex+emplrel+marsts+subjinc+sexmar,data=df)
#
# [ANSWER 4.1]: YES
#
# [ANSWER 4.2]:
#
#The coefficient for income self-assessment is positive thus the more
#insecure respondents are, the more religious they are. Same for employment.
#Employed are less religious than those who work independently.
#
# [ANSWER 4.3]:
#
#Interaction coefficients controls for women. Measure how women marriage status
#affect their religiousness. Since its negative, the less married the women is
#the less she will be religious.
#
# [ANSWER 5]:
#
#Residuals vs fitted plot indicates that the model is super biased. residuals
#are not randomly distributed around 0 and higher residuals are associated with
#lower fitted values

#
# [ANSWER 6.1]: 1.4
# [ANSWER 6.2]:This will help to better understand the effect of different
#countries.Some countries may have a historically higher religiousness, and it's
#important to control for that. we might have some patterns within a country

#
# Feedback on the exercise
# ------------------------
#
# [Feel free to replace this paragraph with your impressions on this exercise.
# If your text goes beyond 80 characters per line, break it into shorter lines,
# as demonstrated in this example paragraph.]
#
# ============================= See README file for data sources and details ===

library(tidyverse)
library(dplyr)
library(broom)
library(haven)
# ------------------------------------------------------------------------------
# 1. Access a dataset
# ------------------------------------------------------------------------------

#we didnt find the way to read sav files from our previous sessions
#as a result, we have found a function on stackoverflow

x<-read_sav("data/ESS9e03_2.sav")
glimpse(x)
# ------------------------------------------------------------------------------
# 2. Recode some variables
# ------------------------------------------------------------------------------

###finding the variables in the documentation :
##age -> agea
##sex -> gndr -> to recode : 1 female; 0 male (luckily no missing values)
##employment relationship -> emplrel
##marital status -> marsts ->to be recoded in 3 groups
##subjective income -> hincfel
##religious attendance -> rlgatnd _> recode

df<-select(x,
           age = agea,
           sex = gndr,
           emplrel,
           marsts,
           subjinc = hincfel,
           rlgatnd,
           cntry
           )
#quickly explore the variables in question
table(as_factor(df$sex))
table(df$sex)
#since female is coded as 2, and male as 1 we can just -1
df<-mutate(df, sex=sex-1)
table(df$sex)
table(as_factor(df$emplrel))
table(df$emplrel)
df.emplrel<-as.factor(df$emplrel)
table(as_factor(df$marsts))
table(df$marsts)
#recoding marital status 1or2;3or4or5;6or66or77or88or99
  #eventually this code will transform NA into 3rd category
  #there havent been restrictions about that so we keep it
df<-mutate(df,marsts=ifelse(marsts %in% c(1,2),1,
                            ifelse(marsts %in% c(3,4,5),2,3)))
#reconding religion attendance
table(as_factor(df$rlgatnd))
table(df$rlgatnd)
df<-mutate(df,rlgatnd = 7-rlgatnd)

# [QUESTION 2.1] Report the percentage of females in the sample. 53.51%
female<-round(nrow(filter(df,sex==1))/nrow(df),4)*100
female
prop.table(table(df$sex))

# [QUESTION 2.2] Report the percentage of separated, divorced or widowed
#                individuals in the sample, after recoding martial status 19.36%
prop.table(table(df$marsts))
nonmarried<-round(nrow(filter(df,marsts==2))/nrow(df),4)*100
nonmarried
# [QUESTION 2.3] Report average religious attendance in the sample, after
#                recoding it, and treating it as a continuous measurement 1.53%
avg_rlgatnd<-round(mean(df$rlgatnd,na.rm=TRUE),2)
avg_rlgatnd
# ------------------------------------------------------------------------------
# 3. Write a multiple linear regression model
# ------------------------------------------------------------------------------
df<-mutate(df,sexmar=df$sex*df$marsts)
m1<-lm(rlgatnd~age+sex+emplrel+marsts+subjinc+sexmar,data=df)


# [QUESTION 3] Provide the formula of your model, in R syntax.
m1<-lm(rlgatnd~age+sex+emplrel+marsts+subjinc+sexmar,data=df)
# ------------------------------------------------------------------------------
# 4. Interpret regression results
# ------------------------------------------------------------------------------
summary(m1)
texreg::screenreg(m1)
##all coefficients r statistically significant under p<0.001 and p<0.05

# [QUESTION 4.1] Do females report higher religious attendance than males,
#                independently of age, employment status, marital status or
#                subjective income? (Answer Yes or No.)

###YES
#since the coefficient is positive

# [QUESTION 4.2] Does the model support the view that economic insecurity
#                increases religious attendance?

###YES


# [QUESTION 4.3] How do you interpret the interaction in the model?

###interaction coefficient in general : 0 for men.
###so it is controlling for women
###measuring how women marriage status affect their religiousness
###basically, since its negative the less married the women is
###the less she will be religious.
###whereas if she lives or has lived with a man,she tends to be more religious

# ------------------------------------------------------------------------------
# 5. Diagnose a linear regression model
# ------------------------------------------------------------------------------

##firstly, it is worth mentioning that the adjusted R-squared is super low
##which means that only 4% of variation in religousness is explained by the
##predictors

# [QUESTION 5] According to its residuals, how biased is the model, and what
#              does that mean _in terms of its capacity to predict religious
#              attendance_ from our list of predictors?

m1_aug<-augment(m1)
p <- ggplot(data = m1_aug,
            mapping = aes(x = .fitted, y = .resid))
p + geom_point()
##the residuals vs fitted plot indicates that the model is super biased
##residuals are not randomly distributed around 0 and higher residuals are
##associated with lower fitted values

# ------------------------------------------------------------------------------
# 6. Start thinking beyond 'flat' models
# ------------------------------------------------------------------------------



# [QUESTION 6.1] Report the country-level effect for Poland. 1.4
#                (Answer with a single number, rounded up to 1-digit precision.)

#first, lets have a look at countries
table(df$subjinc)
df$cntry<-as.factor(df$cntry)
#let's run the model with a country
m2<-lm(rlgatnd~age+sex+emplrel+marsts+subjinc+sexmar+cntry,data=df)

summary(m2)
texreg::screenreg(m2)
round(coef(m2)["cntryPL"],1)


# [QUESTION 6.2] Can you guess why I am asking you to include country of
#                residence as a predictor, and if so, what kind of modelling
#                strategy is being suggested to you here?

#this will help to better understand the effect of different countries. Because
#some countries might have a historically higher religiousness, and it is
#important to control for that. we might have some patterns within a country

# You are done -- thank you for your efforts!
