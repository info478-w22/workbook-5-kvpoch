# Katherine Poch
# Workbook 6: analyze NHANES data

# Set up
library(foreign) 
library(survey) #for survey design
library(Hmisc) #sasxport.get() function for import xpt files

demo <- sasxport.get('DEMO_I.XPT')

alco <- sasxport.get('ALQ_I.XPT')

nhanes <- merge(x = demo, y = alco, by = 'seqn', all = TRUE)

wt_sum <- sum(nhanes$wtint2yr, na.rm = TRUE) #represents overall total US pop
#na.rm = True removes NA

#ANALYSIS

# ALQ151: want to 2 to be 0 (no) and ignore 7 and 9 (don't know and refused to answer)

nhanes$alq151[nhanes$alq151 == 2] <- 0 #change 2 to 0 (binary)

nhanes$alq151[nhanes$alq151 == 7] <- NA

nhanes$alq151[nhanes$alq151 == 9] <- NA

#to check 
max(nhanes$alq151, na.rm = TRUE )

#CREATE SURVEY DESIGN

#strata - division of population into smaller groups
#psu = id primary sampling unit

#~ from dataset
nhanes_survey <- svydesign(
  id = ~sdmvpsu,
  nest = TRUE, 
  strata = ~sdmvstra,
  weights = ~wtint2yr,
  data = nhanes

)

#calc survey mean
nhanes_mean <- svymean(~alq151, nhanes_survey, na.rm = T)

#mean by gender

mean_by_gender <- svyby(~alq151, ~riagendr, 
                        nhanes_survey, svymean, na.rm = T)
















