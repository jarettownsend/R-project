## A link to where the data can be found. This project uses the 2015 data. https://www.kaggle.com/datasets/cdc/behavioral-risk-factor-surveillance-system
## A link to the codebook https://www.cdc.gov/brfss/annual_data/2015/pdf/codebook15_llcp.pdf

# Loading all the packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openintro))
suppressPackageStartupMessages(library(lm.beta))
suppressPackageStartupMessages(library(ggplot2))

# Reading the data
brfss2015<- read_csv('BRFSS2015.csv')

# Q1: How many people have any kind of health care coverage?
# HLTHPLN1 is a variable where 1 means they do have health insurance, so I filtered only those rows, and then counted the total rows
Q1 <- brfss2015 %>%
  filter(HLTHPLN1 == 1) %>%
  nrow()


#Q2:  What is the average "Number of Days Mental Health Not Good" for those in Pennsylvania who have numeric data? 
#Here I created a new data set with just people from Pennsylvania
#Make sure to change the response corresponding to none to 0.
Pennsylvania <- brfss2015 %>%
  filter(brfss2015[,1] == 42)
#Here I created a new data set to filter out those who didn't answer
#I changed 88 which corresponded to None, to the actual number 0
Penn_Responses <- Pennsylvania %>%
  filter(MENTHLTH != 77, MENTHLTH != 99) %>%
  mutate(MENTHLTH = ifelse(MENTHLTH == 88, 0, MENTHLTH))
#The answer was 3.56 days
Q2 <- round(sum(Penn_Responses$MENTHLTH)/nrow(Penn_Responses), 2)


# Q3: Compare only those who have and have not had some form of arthritis, rheumatoid arthritis, gout, etc. 
# Q3: For those groupings, convert reported weight in kilograms to pounds. 
# Q3: Then, compute the mean and standard deviation of the newly created weight in pounds variable
Q3 <- brfss2015 %>%
  drop_na(WEIGHT2) %>%
  filter(HAVARTH3 == 1 | HAVARTH3 == 2) %>%
  filter(WEIGHT2 != 9999, WEIGHT2 != 7777) %>%
  mutate(WEIGHT2 = ifelse(WEIGHT2 > 9000, ((WEIGHT2 - 9000)*2.20462), WEIGHT2)) %>%
  group_by(HAVARTH3) %>%
  mutate(HAVARTH3 = ifelse(HAVARTH3 == 1, "haves", "have_nots")) %>%
  summarise(mean_weight = round(mean(WEIGHT2), 2),
            sd_weight = round(sd(WEIGHT2), 2), .groups = 'drop')


# Q4: Remove outliers from minutes of total physical activity per week using 0.997 and 0.003 as criteria.
# Q4: What percentage of observations remain? 
# Here I created a new data set filtering out people who refused to answer marital status and physical activity
mstatus <- brfss2015 %>%
  filter(MARITAL != 9) %>%
  filter(EXERANY2 == 1 | EXERANY2 == 2 | EXERANY2 == 7) %>%
  filter(PA1MIN_ < 99999)
# Here I filtered out the outliers
upper_bound <- quantile(mstatus$PA1MIN_, .997, na.rm = TRUE)
lower_bound <- quantile(mstatus$PA1MIN_, .003, na.rm = TRUE)
out_obs <- which(mstatus$PA1MIN_ > upper_bound, mstatus$PA1MIN_ < lower_bound)
noout <- mstatus[-out_obs,]
# 99.7% of the observations remain after filtering all of these out
Q4 <- round((nrow(mstatus) - length(out_obs))/nrow(mstatus)*100, 2)


# Q5: Group by marital status and calculate the mean, standard deviation, minimum, and maximum of total exercise, to two decimals.
# Here I created a new data set grouping by all of the above
unrounded5 <- noout %>%
  group_by(MARITAL) %>%
  summarise(ms_mean = mean(PA1MIN_, na.rm = TRUE),
            ms_sd = sd(PA1MIN_, na.rm = TRUE),
            ms_min = min(PA1MIN_, na.rm = TRUE),
            ms_max = max(PA1MIN_, na.rm = TRUE), .groups = 'drop')
# This is the answer above rounded to 2 decimals
Q5 <- round(unrounded5, 2)


# Q6: Create a boxplot for total exercise by marital status.
Q6 <- boxplot(PA1MIN_~MARITAL, noout, main="Total Exercise by Marital Status",
              xlab = "Marital Status", ylab = "Total Exercise in Minutes")


# Q7: Run a regression predicting exercise by marital status
Q7 <- summary(lm(PA1MIN_ ~ MARITAL, noout))


# Q9: Run a regression as in Q7, but add total fruits consumed per day.  
# Q9: Based on the R-squared and AIC, what is the better model?
# These models are the same but the top one has total fruits consumed per day
fruitlm <- lm(PA1MIN_ ~ MARITAL + `_FRUTSUM`, noout)
nonfruitlm <- lm(PA1MIN_ ~ MARITAL, noout)
# Here I compare the two AIC's
AIC(nonfruitlm)
AIC(fruitlm)
# The model with fruit was the better model
Q9 <- round(AIC(fruitlm), 2)

