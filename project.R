

# load libraries

library(tidyverse)
library(ggplot2)
library(ggExtra)
library(stargazer)
library(plm)
library(lmtest)

# load data 

df <- read.csv("data.csv")
head(df)
str(df)

# print summary 

stargazer(df, type="text", summary = TRUE)


table(df$TIMETRND) # we have unbalanced data

      # for each individual we find the highest education attainment 
      
      df_edu <- df %>%  
        group_by(PERSONID) %>% 
        filter(EDUC == max(EDUC), TIMETRND == max(TIMETRND)) 
      
      # scatter plot timtrnd vs educ
      # we can observe that some observations have very low timetrnd 
      # since we are dealing with the highest education attainment
      # we considered these observations outliers,
      # and we need to remove them from the data 
      
      ggplot(df_edu, aes(x=TIMETRND, y=EDUC)) +
        geom_point(shape=19, alpha=0.5) +
        geom_smooth()
      
      # select rows that max(TIMETRND) >=3
      
      ID_to_drop <- subset(df_edu, TIMETRND <= 2)$PERSONID # length(ID_ID_to_drop) = 30
      df_edu <- subset(df_edu, TIMETRND >= 3)
      
      # child edu vs mother edu
      
      p1 <- ggplot(df_edu, aes(x=EDUC, y=MOTHERED)) +
        geom_point(shape=19, alpha=0.5) +
        geom_smooth()
      
      ggMarginal(p1, type="histogram")
      
      # child edu vs fatehr edu
      
      p2 <- ggplot(df_edu, aes(x=EDUC, y=FATHERED)) +
        geom_point(shape=19, alpha=0.5) +
        geom_smooth()
      
      ggMarginal(p2, type="histogram")
      
      # child edu vs average parents edu
      
      df_edu$AVGPRNT <- (df_edu$MOTHERED + df_edu$FATHERED)/2
      
      p3 <- ggplot(df_edu, aes(x=EDUC, y=avgPare)) +
        geom_point(shape=19, alpha=0.5) +
        geom_smooth()
      
      ggMarginal(p3, type="histogram")
      
      # there seems to be a linear relationship in above plots.
      
      # drop the PERSONID with few TIMETRND
      
      df <- subset(df, ! PERSONID %in% ID_to_drop)
      summary(df)
      
      # -----------------------------------
      #
      # baseline: OLS: child educ vs parent educ 
      # literature: Intergenerational transmission of education rural china (2019)
      # Holmlund (2011), Pronzato (2012)
      # 
      # -----------------------------------

      ols_mom <- lm(EDUC~MOTHERED+BRKNHOME+SIBLINGS, data=df_edu)
      ols_dad <- lm(EDUC~FATHERED+BRKNHOME+SIBLINGS, data=df_edu)
      ols_avg <- lm(EDUC~AVGPRNT+BRKNHOME+SIBLINGS, data=df_edu)
      
      stargazer(ols_mom, ols_dad, ols_avg, type = "text")
      
      # -----------------------------------
      #
      # FFE model: child educ vs parent educ 
      # literature: Intergenerational transmission of education rural china (2019)
      # Holmlund (2011), Pronzato (2012)
      # FFE model to eliminate unobservable characteristics shared in each household
      # 
      # -----------------------------------
      
      
      
      # -----------------------------------
      # 
      # logWage base model: OLS
      # 
      # -----------------------------------
      
      ols.base.1 <- plm(LOGWAGE~EDUC+POTEXPER+ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS, data=df, index=c("PERSONID", "TIMETRND"), model="pooling")
      ols.base.4 <- plm(LOGWAGE~EDUC+POTEXPER+ABILITY+FATHERED+BRKNHOME+SIBLINGS, data=df, index=c("PERSONID", "TIMETRND"), model="pooling")
      
      ols.base.2 <- plm(LOGWAGE~EDUC+POTEXPER+ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS+MOTHERED*FATHERED, data=df, index=c("PERSONID", "TIMETRND"), model="pooling")
      ols.base.3 <- plm(LOGWAGE~EDUC+I(EDUC^2)+POTEXPER+I(POTEXPER^2)+ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS+MOTHERED*FATHERED, data=df, index=c("PERSONID", "TIMETRND"), model="pooling")
      
      stargazer(ols.base.1, ols.base.4, ols.base.2, ols.base.3, type="text")


# -----------------------------------
# 
# panel model: log wage
# 
# -----------------------------------

library(plm)

ols <- plm(LOGWAGE~EDUC+POTEXPER+ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS+MOTHERED*FATHERED, data=df, index=c("PERSONID", "TIMETRND"), model="pooling")
fixed <-plm(LOGWAGE~EDUC+POTEXPER+ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS+MOTHERED*FATHERED, data=df, index=c("PERSONID", "TIMETRND"), model="within")
random <- plm(LOGWAGE~EDUC+POTEXPER+ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS+MOTHERED*FATHERED, data=df, index=c("PERSONID", "TIMETRND"), model="random")

# poolability test 
pFtest(fixed, ols)
# plm test
plmtest(ols, type=c("bp"))
# Hausman test - fixed model is selected 
phtest(fixed, random)

# compare the result
stargazer(ols, fixed, random, type ="text")

# -----------------------------------
# 
# panel model: 2SLS
# 
# -----------------------------------

# Z: instrumental var 
# use family background as excluded instruments:

ols.fam <- lm(EDUC~MOTHERED+FATHERED, data=df)
summary(ols.fam)
EDUChat <- fitted(ols.fam)

ols.fam.y <- lm(LOGWAGE~EDUChat+POTEXPER+ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS, data=df)
summary(ols.fam.y)

# use personal ability as excluded instruments:

ols.person <- lm(POTEXPER+ABILITY~BRKNHOME+SIBLINGS, data=df)
summary(ols.person)

PERSON_hat <- fitted(ols.person)
ols.person.y <- lm(LOGWAGE~PERSON_hat+EDUC++MOTHERED+FATHERED+BRKNHOME+SIBLINGS, data=df)
summary(ols.person.y)

# ability + educ

ols.pereduc <- lm(EDUC+ABILITY~MOTHERED+FATHERED, data=df)
summary(ols.pereduc)
PEREDUC_hat <- fitted(ols.pereduc)

ols.pereduc.y <- lm(LOGWAGE~PEREDUC_hat+POTEXPER+MOTHERED+FATHERED+BRKNHOME+SIBLINGS, data=df)
summary(ols.pereduc.y)

stargazer(ols, fixed, random, ols.fam.y, ols.person.y, ols.pereduc.y, type ="text")

# -----------------------------------
# 
# panel model: instrumental
# 
# -----------------------------------

library(AER)

ivedu <- ivreg(LOGWAGE~EDUC+POTEXPER+ABILITY+BRKNHOME+SIBLINGS | POTEXPER+ABILITY+BRKNHOME+SIBLINGS + MOTHERED+FATHERED, data=df)
summary(ivedu)

ivself <- ivreg(LOGWAGE ~ POTEXPER+ABILITY + EDUC+MOTHERED+FATHERED | EDUC+MOTHERED+FATHERED + BRKNHOME+SIBLINGS, data=df)
summary(ivself)





# can we use  MOTHERED as an instrumental var?

p4 <- ggplot(df, aes(x=SIBLINGS, y=MOTHERED)) +
  geom_point(shape=19, alpha=0.5) +
  geom_smooth()

ggMarginal(p4, type="histogram")


summary(fixed)

# Unbalanced Panel: n = 2148, T = 1-15, N = 17876
# 
# Residuals:
#   Min.   1st Qu.    Median   3rd Qu.      Max. 
# -2.442499 -0.149279  0.018568  0.176780  2.550171 
# 
# Coefficients:
#              Estimate Std. Error t-value  Pr(>|t|)    
#   EDUC     0.12370230 0.00576337  21.463 < 2.2e-16 ***
#   POTEXPER 0.03856836 0.00075864  50.839 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# R2 interpretation missing

#test for time effects

fixed.time <-plm(LOGWAGE~EDUC+POTEXPER+ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS+MOTHERED*FATHERED+factor(TIMETRND), data=df, index=c("PERSONID", "TIMETRND"), model="within")

pFtest(fixed.time,fixed)
plmtest(fixed, c("time"), type=("bp"))

# Testing for cross-sectional dependence/contemporaneous correlation:
# using Breusch-Pagan LM test of independence and Pasaran CD test
# p-value < 2.2e-16
pcdtest(fixed, test = c("lm")) 
pcdtest(fixed, test = c("cd"))


# Testing for heteroskedasticity 
# H1: hetero(p-value < 2.2e-16)
bptest(LOGWAGE~EDUC+POTEXPER+ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS+MOTHERED*FATHERED, data=df, studentize=F)

# Controlling for heteroskedasticity:
coeftest(fixed, vcovHC) # Heteroskedasticity consistent coefficients


# -----------------------------------
# 
# panel model: log wage if randome effects were to be used
# 
# -----------------------------------


# remove all insignificant estimators
summary(random)

# remove all insignificant
random.0 <- plm(LOGWAGE~EDUC+POTEXPER+ABILITY+BRKNHOME, data=df, index=c("PERSONID", "TIMETRND"), model="random")
summary(random.0)

# wald test missing

# general-to-specific approach
random.1 <- plm(LOGWAGE~EDUC+POTEXPER+ABILITY+MOTHERED+FATHERED+BRKNHOME+MOTHERED*FATHERED, data=df, index=c("PERSONID", "TIMETRND"), model="random")
summary(random.1)

random <- plm(LOGWAGE~EDUC+POTEXPER+MOTHERED+FATHERED+BRKNHOME+MOTHERED*FATHERED, data=df, index=c("PERSONID", "TIMETRND"), model="random")
summary(random)

random <- plm(LOGWAGE~EDUC+POTEXPER+FATHERED+BRKNHOME, data=df, index=c("PERSONID", "TIMETRND"), model="random")
summary(random)

