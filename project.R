# load libraries
library(tidyverse)    # for pipe operations
library(stargazer)    # for latex formatting
library(plm)          # for panel data regression 
library(aod)          # for wald test 
library(AER)          # for IV estimates ivreg 
library(lmtest)       # model test
library(ggplot2)      # for vidualizations
library(ggExtra) 
library(ggthemes)     
library(tikzDevice)   # for latex figures
#For some reason, Rstudio needs to know the time zone for tikzDevice
options(tz="PL")
library(reshape2)     # melt function
# set customized plot colors for visualizations
col_b = "#1d3557"
col_r = "#d62828"
col_g = "#2a9d8f"
col_i = "#264653"

# -----------------------------------------------------------------
# variables:
# 
# PERSONID  -   personal ID
# EDUC      -   educational level in years
# LOGWAGE   -   per hour logWage
# POTEXPER  -   individual's potential = age - education - 5
# TIMETRND  -   time trend
# ABILITY   -   individual's cognitive score
# MOTHERED  -   mother's educational level in years
# FATHERED  -   father's educational level in years 
# BRKNHOME  -   a dummy variable indicating if the individual is from a broken home
# SIBLINGS  -   number of siblings in the household
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# IMPORT DATA
# -----------------------------------------------------------------

# load data 
df <- read.csv("data.csv")
# take a glance at the dataframe
tibble::glimpse(df)

# print summary in a nice format
stargazer(df, type="text", summary = TRUE)

# check if the panel data is balanced
# count the number of observations by year
table(df$TIMETRND) 
# the result suggests that it is unbalanced dataset
# visualization
df %>% 
  group_by(TIMETRND) %>%
  summarise(CT = n()) %>%
  ggplot(., aes(x=TIMETRND, y=CT)) +
  geom_col(fill=col_b, width=0.5, alpha=0.5) +
  geom_text(aes(label=CT), position=position_dodge(width=0.9), vjust=-0.5) +
  ylim(0, 1800) +
  labs(
    x = "Year",
    y = "Counts"
  ) +
  scale_x_continuous(breaks = seq(0, 14, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1600, by = 200)) +
  theme_classic()

# unbalancedness of the data
punbalancedness(df, index=c("PERSONID", "TIMETRND"))

# for latex plot only
# ---------------------
# tikz(file = "logWageEduc.tex", width = 6, height = 3, bg = "transparent", pointsize = 10)
# plot <- ggplot(df, aes(x=as.factor(EDUC), y=LOGWAGE)) + 
#   geom_boxplot(fill=col_b, width=0.5, alpha=0.5, size=1, color=col_b, outlier.size=0.5, outlier.alpha=0.1) +
#   # geom_smooth(method = "lm", se=FALSE, color = col_r, size=2) +
#   labs(
#     y = "logWage",
#     x = "Schooling Years"
#   ) + 
#   # scale_x_continuous(breaks = seq(0, 20, by = 1)) +
#   theme_classic()
# 
# #This line is only necessary if you want to preview the plot right after compiling
# print(plot)
# #Necessary to close or the tikxDevice .tex file will not be written
# dev.off()
# 
# # for latex plot only
# tikz(file = "timetrnd.tex", width = 6, height = 2, bg = "transparent",  pointsize = 10)
# plot <- df %>% 
#   group_by(TIMETRND) %>% 
#   summarise(CT = n()) %>% 
#   ggplot(., aes(x=TIMETRND, y=CT)) + 
#   geom_col(fill=col_b, width=0.5, alpha=0.5) +
#   geom_text(aes(label=CT), position=position_dodge(width=0.9), vjust=-0.5) +
#   ylim(0, 1800) +
#   labs(
#     x = "Year",
#     y = "Counts"
#   ) + 
#   scale_x_continuous(breaks = seq(0, 14, by = 1)) +
#   scale_y_continuous(breaks = seq(0, 1600, by = 200)) +
#   theme_classic()
# #This line is only necessary if you want to preview the plot right after compiling
# print(plot)
# #Necessary to close or the tikxDevice .tex file will not be written
# dev.off()


# -----------------------------------------------------------------
# DATA CLEANING
# -----------------------------------------------------------------

# create a new dataframe `df_edu` that contains each individual's highest
# educational attainment only,
# use TIMETRND == max(TIMETRND) to filter the latest record
df_edu <- df %>%
  group_by(PERSONID) %>%
  filter(EDUC == max(EDUC), TIMETRND == max(TIMETRND))  %>% 
  mutate(FAMED = (MOTHERED+FATHERED)/2)

head(df_edu)

# count the number of occurences for each individual
n_TIME <- df %>% 
  group_by(PERSONID) %>% 
  select(TIMETRND) %>% 
  summarize(n())

# assign the new colnames
colnames(n_TIME) <- c("PERSONID", "TIMECTN")

# inner join df_edu & n_TIME on PERSONID, we have a new dataframe 
# that contains information of each individual's highest 
# educational attainment and his/her number of records in the dataset.
# The thresholds are set to be:
# TIMECTN <= 3, and EDUC < 9
# for these records are likely to be 
IDs_to_drop <- df_edu %>% 
  inner_join(n_TIME, by=("PERSONID" = "PERSONID")) %>% 
  filter(TIMECTN < 3 & EDUC <= 10)

# 42 individual records will be removed 
dim(IDs_to_drop)[1]

# print IDs_to_drop
IDs_to_drop

# drop them
df <- subset(df,!PERSONID %in% IDs_to_drop$PERSONID)
stargazer(df, type="text", summary = TRUE)

# -----------------------------------------------------------------
# DATA EXPLORATION
# -----------------------------------------------------------------

# basic distribution plots

# density plot of dependent var: LOGWAGE
ggplot(df, aes(LOGWAGE)) + 
  geom_density(fill=col_b, color="white", alpha=0.5) +
  labs(
    title="Density plot of LOGWAGE",
    x = "LogWage",
    y = "Density"
  ) +
  theme_classic()

# for latex plot only
# ---------------------
# tikz(file = "../report/figures/logWageDensity.pdf", width = 6, height = 3, bg = "transparent",  pointsize = 10)
# ggplot(df, aes(LOGWAGE)) + 
#   geom_density(fill=col_b, color="white", alpha=0.5) +
#   labs(
#     x = "LogWage",
#     y = "Density"
#   ) +
#   theme_classic()
# dev.off()

# distribution of father's, mother's and children's education
df_edu %>% 
  melt(id.vars=c("PERSONID", "LOGWAGE","POTEXPER", "TIMETRND", "ABILITY",
                 "BRKNHOME","SIBLINGS"), measure.vars=c("FATHERED", "MOTHERED", "EDUC"),
       variable.name = "FAMEMBER",
       value.name = "HIEDUC") %>% 
  group_by(FAMEMBER) %>% 
  ggplot(aes(HIEDUC, fill=FAMEMBER)) +
  geom_bar(color="white", alpha=0.7, width=0.75, position="dodge") +
  scale_fill_manual(values = c(col_b, col_r, col_g)) +
  scale_x_continuous(breaks=seq(0, 20, 1)) +
  scale_y_continuous(breaks=seq(0, 1000, 200)) +
  labs(
    title = "Counts per each family member by schooling years",
    x = "Schooling Years (highest)", 
    y = "Counts"
  ) +
  theme_classic()


# explanatory variables vs dependent variables 
# the intention is to explore the relationship between various explanatory 
# variables and the dependent variables

# EDUC vs. LOGWAGE
# according to Mincer, log-wage is linear in schooling
# from the visualization, it is clear that there is a positive relationship between
# log wage and schooling years. And the relationship is plausibly linear.
ggplot(df, aes(x=EDUC, y=LOGWAGE)) +
  geom_point(color=col_b, alpha=0.3, position = "jitter", size=0.5) +
  geom_smooth(method="lm",se=FALSE, color=col_r, size=2) +
  labs(
    title = "Education years vs. Log Wage",
    x = "Schooling years",
    y = "log Wage"
  ) +
  scale_x_continuous(breaks=seq(0,20,by=1)) +
  theme_classic()

# for latex plot only
# ---------------------
# tikz(file = "../report/figures/logWageSchooling.pdf", width = 6, height = 3, bg = "transparent",  pointsize = 10)
# ggplot(df, aes(x=EDUC, y=LOGWAGE)) +
#   geom_point(color=col_b, alpha=0.3, position = "jitter", size=0.5) +
#   geom_smooth(method="lm",se=FALSE, color=col_r, size=2) +
#   labs(
#     x = "Schooling years",
#     y = "log Wage"
#   ) +
#   scale_x_continuous(breaks=seq(0,20,by=1)) +
#   theme_classic()
# dev.off()

# POTEXPER vs LOGWAGE
# from the visualization that a quadratic relationship fits better 
# than a linear relationship
ggplot(df, aes(x=POTEXPER, y=LOGWAGE)) +
  geom_point(color=col_b, alpha=0.3, position = "jitter", size=1) +
  geom_smooth(method="lm",se=FALSE, color="#f28482", size=2, linetype = "dashed") +
  geom_smooth(se=FALSE, color=col_r, alpha=0.5, size=2) +
  labs(
    title = "Potential vs logWage",
    x = "Potential",
    y = "logWage"
  ) + 
  scale_x_continuous(breaks=seq(0,24,by=1)) +
  theme_classic()


# # for latex only
# tikz(file = "../report/figures/potexper.pdf", width = 6, height = 3, bg = "transparent",  pointsize = 10)
# ggplot(df, aes(x=POTEXPER, y=LOGWAGE)) +
#   geom_point(color=col_b, alpha=0.3, position = "jitter", size=0.5) +
#   geom_smooth(method="lm",se=FALSE, color="#f28482", size=2, linetype = "dashed") +
#   geom_smooth(se=FALSE, color=col_r, alpha=0.5, size=2) +
#   labs(
#     x = "Potential",
#     y = "logWage"
#   ) + 
#   scale_x_continuous(breaks=seq(0,24,by=1)) +
#   theme_classic()
# dev.off()

# ABILITY vs LOGWAGE
# a linear relationship seems fit 
ggplot(df, aes(x=ABILITY, y=LOGWAGE)) +
  geom_point(color=col_b, alpha=0.3, size=1) +
  geom_smooth(method="lm", se=FALSE, color=col_r, size=1.5) +
  labs(
    title = "Ability vs logWage",
    x = "Ability",
    y = "logWage"
  ) + 
  theme_classic()

# BRKNHOME vs LOGWAGE
# residing in a broken (BROKNHOME = 1) household seems to have a negative 
# impact on the LOGWAGE: having a lower mean wage than BROKNHOME = 0
ggplot(df, aes(x=factor(BRKNHOME), y=LOGWAGE)) +
  geom_boxplot(fill=c(col_b, col_r), width=0.5, size=1.5, alpha=0.5) +
  labs(
    title = "Broken Home vs logWage",
    x = "Broken Home",
    y = "logWage"
  ) + 
  theme_classic()

# SIBLINGS vs LOGWAGE
# a linear relationship seems fit
ggplot(df, aes(x=SIBLINGS, y=LOGWAGE)) +
  geom_point(color=col_b, alpha=0.3, size=1, position = "jitter") +
  geom_smooth(method="lm", se=FALSE, color=col_r, size=1.5) +
  labs(
    title = "Number of siblings vs logWage",
    x = "siblings",
    y = "logWage"
  ) + 
  theme_classic()

# FATHERED vs. LOGWAGE
# a linear relationship seems fit
ggplot(df, aes(x=FATHERED, y=LOGWAGE)) + 
  geom_point(color=col_b, alpha=0.3, position = "jitter") + 
  geom_smooth(method="lm", se=FALSE, color=col_r, size=1.5) +
  labs(
    title = "Father's education vs. children's Wage",
    x = "Father's Education Level (Years)",
    y = "Children's logWage"
  ) + 
  scale_x_continuous(breaks=seq(0, 20, 1)) + 
  theme_classic()

# MOTHERED vs. LOGWAGE
# a linear relationship seems fit
ggplot(df, aes(x=MOTHERED, y=LOGWAGE)) + 
  geom_point(color=col_b, alpha=0.3, position = "jitter") + 
  geom_smooth(method="lm", se=FALSE, color=col_r, size=1.5) +
  labs(
    title = "Mother's education vs. children's Wage",
    x = "Mother's Education Level (Years)",
    y = "Children's logWage"
  ) + 
  scale_x_continuous(breaks=seq(0, 20, 1)) + 
  theme_classic()

# from the visualizations above, I decided to construct a linear model
# between all the explanatory variables and the dependent variable LOGWAGE.
# A quadratic term of POTEXPER will be added to a second model for comparison

# -----------------------------------------------------------------
# PANEL MODELS
# -----------------------------------------------------------------

# test the model using `ols`, `fixed effects``, and `random effects`
# respectively.
# model: LOGWAGE~EDUC+POTEXPER+I(POTEXPER^2)+ABILITY+MOTHERED+FATHERED+
# BRKNHOME+SIBLINGS+MOTHERED:FATHERED

# Model 1:
# LOGWAGE ~ EDUC+POTEXPER+ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS+MOTHERED:FATHERED, 
# ----------
# ols model
ols.1 <- plm(LOGWAGE~EDUC+POTEXPER+ABILITY+MOTHERED+FATHERED+
             BRKNHOME+SIBLINGS+MOTHERED:FATHERED, 
           data=df, index=c("PERSONID", "TIMETRND"), model="pooling")

# check model summary
summary(ols.1)

# fixed model
fixed.1 <-plm(LOGWAGE~EDUC+POTEXPER+ABILITY+MOTHERED+FATHERED+
              BRKNHOME+SIBLINGS+MOTHERED:FATHERED, 
            data=df, index=c("PERSONID", "TIMETRND"), model="within")

# check model summary
summary(fixed.1)

# random model
random.1 <- plm(LOGWAGE~EDUC+POTEXPER+ABILITY+MOTHERED+FATHERED+
                BRKNHOME+SIBLINGS+MOTHERED:FATHERED, 
              data=df, index=c("PERSONID", "TIMETRND"), model="random")

# check model summary
summary(random.1)

# compare the models to find the most appropriate one
# poolability test 
pFtest(fixed.1, ols.1) # fixed effect is more appropriate

# plm test
plmtest(ols.1, type=c("bp")) # random effect is more appropriate

# Hausman test
phtest(fixed.1, random.1) # fixed effect is more appropriate

# compare the results
stargazer(ols.1, fixed.1, random.1, type ="text",
          column.labels = c("OLS", "FE","RE"))


# -----------------------------------------------------------------
# General to Specific Approach: RANDOM MODEL 1
# -----------------------------------------------------------------

# look at the model summary again
summary(random.1)

# check joint significanc - can we remove all insignificant variables at once?
# - MOTHERED
# - FATHERED
# - SIBLINGS
# - MOTHERED:FATHERED

random.1$coefficients



# use Wald test to test the hypothesis that all insignificant var are jointly
# insignificant
H <- rbind(
  c(0, 0, 0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 1)
)

# wald test to check if the smaller model performs better
wald.test(b = coef(random.1), Sigma = vcov(random.1), L = H)

# P(> X2) = 0.77 -> we can't reject the Null Hypothesis that these variables are all
# jointly insignificant based on 0.05 significance level

# remove all insignificant variables:

random.1s <- plm(LOGWAGE~EDUC+POTEXPER+ABILITY+BRKNHOME, 
                  data=df, index=c("PERSONID", "TIMETRND"), model="random")

# compare random.1 and random.1s 
stargazer(random.1, random.1s, type ="text",
          column.labels = c("RE1", "RE1s"))

# Model 2:
# LOGWAGE ~ EDUC+POTEXPER+I(POTEXPER^2)+ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS+MOTHERED:FATHERED, 
# ----------

# ols model
ols.2 <- plm(LOGWAGE~EDUC+POTEXPER+I(POTEXPER^2)+ABILITY+MOTHERED+FATHERED+
             BRKNHOME+SIBLINGS+MOTHERED:FATHERED, 
           data=df, index=c("PERSONID", "TIMETRND"), model="pooling")

# check model summary
summary(ols.2)

# fixed model
fixed.2 <-plm(LOGWAGE~EDUC+POTEXPER+I(POTEXPER^2)+ABILITY+MOTHERED+FATHERED+
              BRKNHOME+SIBLINGS+MOTHERED:FATHERED, 
            data=df, index=c("PERSONID", "TIMETRND"), model="within")

# check model summary
summary(fixed.2)

# random model
random.2 <- plm(LOGWAGE~EDUC+POTEXPER+I(POTEXPER^2)+ABILITY+MOTHERED+FATHERED+
                BRKNHOME+SIBLINGS+MOTHERED:FATHERED, 
              data=df, index=c("PERSONID", "TIMETRND"), model="random")

# check model summary
summary(random.2)

# compare the models to find the most appropriate one
# poolability test 
pFtest(fixed.2, ols.2) # fixed effect is more appropriate

# plm test
plmtest(ols.2, type=c("bp")) # random effect is more appropriate

# Hausman test
phtest(fixed.2, random.2) # fixed effect is more appropriate

# compare the results
stargazer(ols.2, fixed.2, random.2, type ="text",
          column.labels = c("OLS", "FE","RE"))

# -----------------------------------------------------------------
# General to Specific Approach: RANDOM MODEL 2
# -----------------------------------------------------------------

# look at the model summary again
summary(random.2)

# check joint significanc - can we remove all insignificant variables at once?
# - MOTHERED
# - FATHERED
# - SIBLINGS
# - MOTHERED:FATHERED

random.2$coefficients

# use Wald test to test the hypothesis that all insignificant var are jointly
# insignificant
H <- rbind(
  c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
)

# wald test to check if the smaller model performs better
wald.test(b = coef(random.2), Sigma = vcov(random.2), L = H)
# P(> X2) = 0.58 -> we can't reject the Null Hypothesis that these variables are all
# jointly insignificant based on 0.05 significance level

# remove all insignificant variables:
random.2s <- plm(LOGWAGE~EDUC+POTEXPER+I(POTEXPER^2)+ABILITY+BRKNHOME, 
                data=df, index=c("PERSONID", "TIMETRND"), model="random")
summary(random.2s)

# compare the results again
stargazer(ols.1, fixed.1, random.1, random.1s, ols.2, fixed.2, random.2, random.2s,
          type ="text", no.space=TRUE, title="Regression Results",
          column.labels = c("OLS1", "FE1","RE1", "RE1s", "OLS2", "FE2","RE2", "RE2s"))


# FE model is more appropriate as suggested by the Hausman Test. 
# One possible explanation is the engogeneity lies in the residuals, 
# causing RE model estimators to be biased. However, FE model ignores
# all time-invariant variables which are of interest for this project.
# Therefore, I chose to proceed with the RE model despite the Hausman
# test result.

# Final model: REs2

# -----------------------------------------------------------------
# Diagnostic tests for the final model: RE2s
# -----------------------------------------------------------------

# Testing for cross-sectional dependence/contemporaneous correlation:
# using Breusch-Pagan LM test of independence and Pasaran CD test
# H0: residuals across entities are not correlated
# H1: there is contemporaneous correlation (we have it)
# Breusch-Pagan's LM: bad for N > T panels
pcdtest(random.2s, test = c("lm")) 
# Pesaran's CD performs well even for small T and large N
pcdtest(random.2s, test = c("cd")) 
# p-value < 2.2e-16 -> reject the null hypothesis that there is no 
# contemporaneous correlation.
# there is contemporaneous correlation (can lead to bias in tests results)

# heteroskedasticity
# Breusch-Pagan Test for heteroskedasticity
# H0: homoskedasticity
# H1: heteroskedasticity
bptest(LOGWAGE~EDUC+POTEXPER+I(POTEXPER^2)+ABILITY+BRKNHOME, 
       data=df, studentize=F)
# p < 2.2e-16 -> reject the null hypothesis that there is homoskedasticity.
# there is heteroskedasticity (normally distributed accrdoing to the original paper)
# need to use a robust covariance matrix to account for it

# serial correlation
# Breusch--Godfrey test for serial correlation
# H0: there is no serial correlation
# H1: there is serial correlation
pbgtest(random.2s)
# p < 2.2e-16 -> reject the null hypothesis that there is no serial correlation.
# there is serial correlation
# Serial correlation causes the standard errors of the coefficients to be 
# smaller than they actually are and higher R-squared

# Robust Covariance Matrix Estimators
# cluster-robust estimator 
diagnose.1 = coeftest(random.2s, vcov.=vcovHC(random.2s, type="HC0"))

# heteroskedasticity-robust estimator 
diagnose.2 = coeftest(random.2s, vcov.=vcovHC(random.2s, type="HC0", cluster="group"))

# autocorrelation-robust estimator
diagnose.3 = coeftest(random.2s, vcov.=vcovNW(random.2s, type="HC0", cluster="group"))

# robust for cross-sectional and serial correlation estimator
diagnose.4 = coeftest(random.2s, vcov.=vcovSCC(random.2s, type="HC0", cluster="time"))

# put them together
stargazer(random.2s, diagnose.1, diagnose.2, diagnose.3, diagnose.4, no.space=TRUE, 
          title="Diagnostic tests", type="text")

# -----------------------------------------------------------------
# Instrumental variables approach 
# -----------------------------------------------------------------

# instrumental variables for comparison:
# 2SLS: ivreg(second stage | instrument, data)

# iv: both father and mother's educational level as instruments

iv <- ivreg(LOGWAGE ~ EDUC+POTEXPER+I(POTEXPER^2)+ABILITY+BRKNHOME
                +SIBLINGS | .-EDUC+MOTHERED+FATHERED, data=df)

summary(iv, diagnostics = TRUE)

# Diagnostic tests:
#                    df1   df2 statistic  p-value    
# Weak instruments     2 17847   270.541  < 2e-16 *** (stats > 10, we have strong instruments)
# Wu-Hausman           1 17847    21.781 3.08e-06 *** (iv is preferred over OLS)
# Sargan               1    NA     0.923    0.337     (instrument is valid)
      
stargazer(ols.2, fixed.2, random.2s, iv, type = "latex", no.space=TRUE, 
          column.labels = c("OLS2", "FE2", "RE2s", "parent's education"))
      