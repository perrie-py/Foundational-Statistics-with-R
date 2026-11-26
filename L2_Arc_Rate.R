Data <- load(file.choose())
View(Data)
library("ggplot2")
library(tidyverse)
library(car)

# column names
colnames(l2rate)
# speaker, sex, age, years_uk, task, artic_rate

rownames(l2rate)
# 1 - 144

#check all 3 tasks have equal participants 
table(l2rate$task)
# does every speaker have 3 rows?
table(table(l2rate$speaker))

#### 1 - DEMOGRAPHICS ####

#ignore tables, just to remind of which variables are demographic!
# each speaker gives 3 results, so this must be broken down 
speakers <- l2rate[l2rate$task == "READ",]


## SEX ##
table(speakers$sex)
# 19 female, 29 male 

## AGE ##
table(speakers$age)
mean(speakers$age)
sd(speakers$age)
range(speakers$age)

# RESULTS: mean = 38, sd = 8.807637, range = 22 58

## YEARS UK ##
table(speakers$years_uk)
mean(speakers$years_uk)
sd(speakers$years_uk)
range(speakers$years_uk)

#RESULTS: mean = 10.10208, sd = 5.642265, range = 0.8 19.0

####  2 - DESCRIPTIVE STATISTICS ABOUT ARTICULATION RATE ####
table(l2rate$task)
# CONV 48, PIC 48, READ 48


# mean + sd of artic_rate by task
l2rate %>%
  group_by(task) %>%
  summarize(M = mean(artic_rate),
            SD = sd(artic_rate),
            Min = min(artic_rate),
            Max = max(artic_rate),
            Med = median(artic_rate))

#task      M    SD   Min   Max   Med
#<fct> <dbl> <dbl> <dbl> <dbl> <dbl>
#  1 CONV   4.63 0.564  3.05  5.68  4.70
#2 PIC    4.25 0.463  3.15  4.99  4.28
#3 READ   4.81 0.380  3.70  5.54  4.89

ggplot(l2rate, aes(x = artic_rate)) +
  geom_histogram() +
  labs(
    x = "Articulation rate (syllables per second)", 
    y = "Number of observations"
  ) +
  theme_minimal()
# unimodal with slight negative skew

#### 3 - VISUALISATION ####

# Basic scatter plot: plot(x, y), ignore
plot(l2rate$years_uk, l2rate$artic_rate,
     main = "Title to be edited", # Title
     xlab = "Length of UK Residency (Years)",               # X-axis label
     ylab = "Articulation Rate (syllables per second)",           # Y-axis label                                # Point shape (19 is solid circle)
     col = "black",                             # Point color
     cex = 1.2,)                                # Point size


# multicoloured task + slope
ggplot(l2rate, aes(x = years_uk, y = artic_rate, colour = task)) +
  geom_point(alpha = 1) +
  geom_smooth(method = "lm") +
  scale_colour_discrete(labels = c("Conversation", "Picture", "Reading")) +
  labs(x = "Length of residence (years)",
       y = "Articulation rate (syllables per second)",
       colour = "Task") +
  theme_minimal()


# all lines sloped upwards, meaning longer residence articulate faster in every task
# non-parallel slopes suggest effect of length of residence depends on task
# ^ especially READ

#### 4 - REGRESSION MODEL PREDICTING ARTIC ####

m1 <- lm(artic_rate ~ years_uk + task, data = l2rate)
summary(m1)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.99203 -0.18611 -0.01748  0.17556  0.91159 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.991787   0.066305  60.204  < 2e-16 ***
#  years_uk     0.062803   0.004741  13.246  < 2e-16 ***
#  taskPIC     -0.372667   0.064841  -5.747 5.44e-08 ***
#  taskREAD     0.185167   0.064841   2.856  0.00495 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.3177 on 140 degrees of freedom
#Multiple R-squared:  0.6431,	Adjusted R-squared:  0.6354 
#F-statistic: 84.09 on 3 and 140 DF,  p-value: < 2.2e-16

# length of residence has positive effect (p < .001) - the longer someone has lived in the UK, the faster they articulate 
# TASK has significant effect - PIC is signif slower than CONV (p < .001)
# READ is significantly faster than CONV (p = .005)

#### 5 - IS THE EFFECGT OF LENGTH OF RESIDENCE CONSISTENT ACROSS DIFFERENT TASKS ####
# in m1, slopes were forced to be equal - now we differ them and test whether that improves the model

m2 <- lm(artic_rate ~ years_uk * task, data = l2rate)
summary(m2)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.79352 -0.17982 -0.00099  0.17313  0.76207 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        3.766027   0.088497  42.555  < 2e-16 ***
#  years_uk           0.085151   0.007667  11.106  < 2e-16 ***
#  taskPIC           -0.206027   0.125154  -1.646    0.102    
#taskREAD           0.695810   0.125154   5.560 1.35e-07 ***
#  years_uk:taskPIC  -0.016496   0.010843  -1.521    0.130    
#years_uk:taskREAD -0.050548   0.010843  -4.662 7.31e-06 ***

#Residual standard error: 0.2966 on 138 degrees of freedom
#Multiple R-squared:  0.6933,	Adjusted R-squared:  0.6822 
#F-statistic:  62.4 on 5 and 138 DF,  p-value: < 2.2e-16

# matches what figure 1 suggested
# READ is less than half as strong as in CONV
# PIC slope isn't signif different from CONV
# so READ is flatter

# anova = analysis of variance, used to compare 2 models and see whether the more complex one is worth it
anova(m1, m2)

#anova(m1, m2)
#Analysis of Variance Table

#Model 1: artic_rate ~ years_uk + task
#Model 2: artic_rate ~ years_uk * task
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1    140 14.127                                  
#2    138 12.138  2    1.9884 11.303 2.845e-05 ***
#  ---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# adding the interaction significantly reduced the unexplained variance
# residual sum of squares drops from 14.13 to 12.14
# model comparison is significant: F(2,138) = 11.30, p < .001
# CONCLUSION FOR 6 AND 7: the effect of length of residence is not consistent across tasks 
# positive in all 3, but weaker in reading (about 0.035 syllables/s per year) than in conversation (0.085)
# picture description doesn't differ significantly from conversation (0.069)

## read as reference level ##
# relevel so years_uk coefficient becomes the READ slope with its own SE and p-value
l2rate2 <- l2rate %>%
  mutate(task = relevel(task, ref = "READ"))

m2_read <- lm(artic_rate ~ years_uk * task, data = l2rate2)
summary(m2_read)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        4.461837   0.088497  50.418  < 2e-16 ***
#  years_uk           0.034603   0.007667   4.513 1.36e-05 ***
#  taskCONV          -0.695810   0.125154  -5.560 1.35e-07 ***
#  taskPIC           -0.901837   0.125154  -7.206 3.42e-11 ***
#  years_uk:taskCONV  0.050548   0.010843   4.662 7.31e-06 ***
#  years_uk:taskPIC   0.034053   0.010843   3.141  0.00206 ** 


#### 6 - EVALUATE MODEL ASSUMPTIONS ####

## Collinearity ##
# checked on m1 - interaction terms always inflate VIF, so m2
vif(m1)

#GVIF Df GVIF^(1/(2*Df))
#years_uk    1  1               1
#task        1  2               1

## Normality of Residuals ##

hist(m2$residuals)
# roughly symmetric, unimodal and centred on 0
# slight bump in the right tail, but not a strong skew 

# mostly unimodal

qqnorm(m2$residuals)
qqline(m2$residuals)
# points follow the line close through middle of distribution
# small departures at both end, but minor
# residuals are approximately normally distributed 

## CONSTANT VARIANCE ##
plot(fitted(m2), m2$residuals)
# residuals scattered evenly above and below 0, with no funnel shape 
# spread slightly narrower at highest fitted values 
# constant variance assumption is met 

## Linearity ##

plot(m2, which = 1)
# red line close to 0 across whole range of fitted values meaning model isn't missing a non-linear pattern

