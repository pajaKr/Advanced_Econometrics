################################
######### Code Final ###########
################################

rm(list=ls())
library(foreign)
library(stringr)
library(dplyr)
library(corrplot)
library(sandwich)
library(lmtest)
library(plm)
library(aod)
library(lmtest)
library(erer)

data = read.csv('/Users/pavlina/Documents/ies/1st_Semester/Advanced_Econometrics/Project/survey.csv')
unique(data$year)

###############################
######## Main Analysis ########
###############################
unique(data$wave)
our_data = subset(data, wave == "Apr 2020" | wave == "Apr 2019")
summary(our_data)
#our_data = subset(our_data, happening != "Don't know")
#our_data = subset(our_data, select = c(happening, wave, gender, age, race, marit_status, employment, house_own, house_size, income, educ, educ_category, party, region9))

our_data$party = as.factor(our_data$party)
unique(our_data$happening)
table(as.factor(our_data$happening))
our_data$happening = ifelse(our_data$happening == "Yes", 1, 0)

unique(our_data$income)
our_data$income_numeric = our_data$income %>%
  str_extract_all("\\$\\d+,?\\d*") %>%
  lapply(function(x) gsub("\\$", "", x)) %>%
  lapply(function(x) gsub(",", "", x)) %>%
  sapply(function(x) as.numeric(x)) %>%
  sapply(function(x) mean(x, na.rm = TRUE))

unique(our_data$educ)
our_data$years_education = ifelse(our_data$educ == "High school graduate - high school diploma or the equivalent (GED)", 12,
                                  ifelse(our_data$educ == "10th grade", 10,
                                         ifelse(our_data$educ == "Professional or Doctorate degree", 21,
                                                ifelse(our_data$educ == "Master's degree", 18,
                                                       ifelse(our_data$educ == "Some college, no degree", 14, # Average value, adjust as needed
                                                              ifelse(our_data$educ == "Bachelor's degree", 16,
                                                                     ifelse(our_data$educ == "Associate's degree", 14,
                                                                            ifelse(our_data$educ == "11th grade", 11,
                                                                                   ifelse(our_data$educ == "9th grade", 9,
                                                                                          ifelse(our_data$educ == "12th grade no diploma", 12,
                                                                                                 ifelse(our_data$educ == "No formal education", 0,
                                                                                                        ifelse(our_data$educ == "7th or 8th grade", 7.5, # Average of 7th and 8th grade
                                                                                                               ifelse(our_data$educ == "5th or 6th grade", 5.5, # Average of 5th and 6th grade
                                                                                                                      ifelse(our_data$educ == "1st, 2nd, 3rd, or 4th grade", 2.5, NA))))))))))))))
summary(our_data)
our_data$gender = as.factor(our_data$gender)

unique(our_data$employment)
our_data$Unemployed = ifelse(our_data$employment == "Not working - looking for work" | our_data$employment == "Not working - on temporary layoff from a job", 1, 0)

unique(our_data$race)
our_data$race = as.factor(our_data$race)

unique(our_data$marit_status)
our_data$marit_status_dummy = ifelse(our_data$marit_status == "Married", 1, 0)

unique(our_data$house_own)
our_data$house_own_dummy = ifelse(our_data$house_own =="Owned by you or someone in your household", 1, 0)

unique(our_data$wave)
our_data$wave = as.factor(our_data$wave)

head(our_data)
summary(our_data)

model_p2020 = lm(happening ~ income_numeric + gender + age + years_education + Unemployed + house_size + race + marit_status_dummy + house_own_dummy + wave + party, data = our_data)
summary(model_p2020)

#################################
### Linking unemployment data ###
#################################

unemp_regional = read.csv("/Users/pavlina/Documents/ies/1st_Semester/Advanced_Econometrics/Project/unemployment_regional.csv")

unique(our_data$region9)

unemp_regional$region9 = NA
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD810000000000003","New England", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD820000000000003","Mid-Atlantic", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD830000000000003","East-North Central", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD840000000000003","West-North Central", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD850000000000003","South Atlantic", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD860000000000003","East-South Central", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD870000000000003","West-South Central", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD880000000000003","Mountain", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD890000000000003","Pacific", unemp_regional$region9)

unemp_regional = subset(unemp_regional, Label == "2020 Apr" | Label == "2019 Apr")
unemp_regional$wave = ifelse(unemp_regional$Label == "2020 Apr", "Apr 2020", "Apr 2019")
unemp_regional = subset(unemp_regional, select = c(wave, region9, Value))
aggregate(unemp_regional$Value, by = list(unemp_regional$wave), FUN = mean, na.rm = TRUE)

our_data = merge(our_data, unemp_regional, by.x = c('wave', 'region9'), by.y = c('wave', 'region9'), all.x = T)

model_UR = lm(happening ~ income_numeric + gender + age + years_education + Unemployed + house_size + race + marit_status_dummy + house_own_dummy + Value + party, data = our_data)
summary(model_UR)

################################
############# Data #############
################################

# independent variables: income, gender, age, years_education, unemployed, house_size, race, marital_status, house_own, unemployment_rate, party

summary(our_data$happening)
our_data$harm_personally = as.factor(our_data$harm_personally)
our_data$harm_future_gen = as.factor(our_data$harm_future_gen)
our_data$reg_CO2_pollutant = as.factor(our_data$reg_CO2_pollutant)
summary(our_data$harm_personally)
summary(our_data$harm_future_gen)
summary(our_data$reg_CO2_pollutant)

summary(our_data$income_numeric)
summary(our_data$gender)
summary(our_data$age)
summary(our_data$years_education)
summary(our_data$unemployment)
summary(our_data$house_size)
summary(our_data$race)
summary(our_data$marit_status_dummy)
summary(our_data$house_own_dummy)
summary(our_data$Value)
summary(our_data$party) # i would do Democrat/republican/independent/other

unique(our_data$religion)

colnames(our_data)
data_models = subset(our_data, select = c(wave, happening, worry, harm_personally, harm_future_gen, reg_CO2_pollutant, fund_research, gender, age, race, party, religion, marit_status, 
                                          house_head, house_size, house_own, income_numeric, years_education, Value, region9, discuss_GW, cause_recoded, Unemployed))

# new columns and renaming columns
data_models$Happening = data_models$happening

data_models$Region = as.factor(data_models$region9)

unique(data_models$worry)
data_models$Worried = ifelse(data_models$worry == "Somewhat worried" | data_models$worry == "Very worried", 1, 0)

table(data_models$reg_CO2_pollutant)
data_models$Support_regulation = ifelse(data_models$reg_CO2_pollutant == "Somewhat support" | data_models$reg_CO2_pollutant == "Strongly support", 1, 0)

table(data_models$fund_research)
data_models$Fund_research = ifelse(data_models$fund_research == "Somewhat support" | data_models$fund_research == "Strongly support", 1, 0)

data_models$Female = ifelse(data_models$gender == "Female", 1, 0)
data_models$Age = data_models$age

unique(data_models$race)
data_models$Race_Black = ifelse(data_models$race == 'Black, Non-Hispanic', 1, 0)
data_models$Race_Hispanic = ifelse(data_models$race == 'Hispanic', 1, 0)
data_models$Race_Other = ifelse(data_models$race == 'Other, Non-Hispanic', 1, 0)
data_models$Race_White = ifelse(data_models$race == 'White, Non-Hispanic', 1, 0)

unique(data_models$party)
data_models$Democrat= ifelse(data_models$party == 'Democrat', 1, 0)
data_models$Republican= ifelse(data_models$party == 'Republican', 1, 0)
data_models$Independent= ifelse(data_models$party == 'Independent', 1, 0)
data_models$Party_other = ifelse(!data_models$party %in% c('Independent', 'Democrat', 'Republican'), 1, 0)

unique(data_models$marit_status)
data_models$marit_status = as.factor(data_models$marit_status)
table(data_models$marit_status)

data_models$Married= ifelse(data_models$marit_status == 'Married', 1, 0)

unique(data$employment)
data_models$Head_of_household= ifelse(data_models$house_head == 'Head of household', 1, 0)

data_models$Household_size = data_models$house_size

data_models$Own_house = ifelse(data_models$house_own =="Owned by you or someone in your household", 1, 0)

data_models$Income = data_models$income_numeric/10000

data_models$Education = data_models$years_education

data_models$Unemployment_rate = data_models$Value

data_models$Wave_2020= ifelse(data_models$wave == 'Apr 2020', 1, 0)

colnames(data_models)
#data_models_attempt = subset(data_models, select = -c(happening, worry, reg_CO2_pollutant, fund_research, gender, age, race,
#                                                      party, religion, marit_status, house_head, house_size, house_own, income_numeric, years_education, Value, wave, region9))

data_models = subset(data_models, select = -c(happening, worry, harm_personally, harm_future_gen, reg_CO2_pollutant, fund_research, gender, age, race, discuss_GW,
                                              party, religion, marit_status, house_head, house_size, house_own, income_numeric, years_education, Value, wave, region9, Fund_research))

colnames(data_models)

hist(data_models$Age)
hist(data_models$Household_size)
hist(data_models$Income)
hist(data_models$Education)

new_order <- c("cause_recoded", "Happening", "Region", "Worried" , "Support_regulation", "Age", "Female",
               "Income", "Education", "Race_White", "Race_Black", 
               "Race_Hispanic", "Race_Other", "Democrat", "Republican", "Independent", 
               "Party_other", "Married", "Head_of_household", "Household_size", 
               "Own_house", "Unemployed", "Wave_2020", "Unemployment_rate")
data_models = data_models[, new_order]

# correlation plot
col= colorRampPalette(c("#194f80", "white", "#cf1930"))
M = cor(data_models[,-c(1,2,3,4,5)])
corrplot(M,type = 'lower', method = 'color', col=col(200), tl.col="black", tl.srt=45, tl.cex=0.65, number.cex = 0.4, cl.cex=0.8, cl.ratio=0.1, addCoef.col = 'black')

# add stuff later - from the seminars!

#summary stats
as.data.frame(sapply(data_models[,-c(1,3)], sd))
summary(data_models)

################################
################################
########### Analysis ###########
################################
################################

############################
######### Happening ########
############################

# Linear Probability model - with binary for wave
happening_lpm = lm(Happening ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                     Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Wave_2020, data = data_models)
bptest(happening_lpm)
summary(happening_lpm)
vcov_W = vcovHC(happening_lpm, type="HC1")
coeftest(happening_lpm, vcov=vcov_W)
resettest(happening_lpm)

# probit
happening_probit = glm(Happening ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                         Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Wave_2020, family=binomial(link="probit"),data = data_models, x =T)
summary(happening_probit)
logLik(happening_probit)
#library('lmtest')

#confint(happening_probit)

# logit
happening_logit = glm(Happening ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                        Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Wave_2020, family=binomial(link="logit"), data = data_models, x = T)
summary(happening_logit)
logLik(happening_logit)

# do marginal effects !
get_ME_at_mean = function(estimated_model, f, F, data){
  # Extract betas from the model
  betas = t(data.frame(coef(estimated_model)))
  
  const = betas[1]
  b_Age = betas[2]
  b_Female = betas[3]
  b_Income = betas[4]
  b_Education = betas[5]
  b_Race_Black = betas[6]
  b_Race_Hispanic = betas[7]
  b_Race_Other = betas[8]
  b_Republican = betas[9]
  b_Independent = betas[10]
  b_Party_other = betas[11]
  b_Married = betas[12]
  b_Head_of_household = betas[13]
  b_Household_size = betas[14]
  b_Own_house = betas[15]
  b_Unemployed = betas[16]
  b_Wave_2020 = betas[17]
  
  # Calculate mean values of x
  x_Age = mean(data$Age)
  x_Female = mean(c(data$Female))
  x_Income = mean(data$Income)
  x_Education = mean(data$Education)
  x_Race_Black = mean(c(data$Race_Black))
  x_Race_Hispanic = mean(c(data$Race_Hispanic))
  x_Race_Other = mean(c(data$Race_Other))
  x_Republican = mean(c(data$Republican))
  x_Independent = mean(c(data$Independent))
  x_Party_other = mean(c(data$Party_other))
  x_Married = mean(c(data$Married))
  x_Head_of_household = mean(c(data$Head_of_household))
  x_Household_size = mean(data$Household_size)
  x_Own_house = mean(c(data$Own_house))
  x_Unemployed = mean(c(data$Unemployed))
  x_Wave_2020 = mean(c(data$Wave_2020))
  
  # Calculate x * beta at mean values of x
  mean_xb = const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020
  mean_Female_1 = F(const + b_Age*x_Age + b_Female*1 + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Female_0 = F(const + b_Age*x_Age + b_Female*0 + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Race_Black_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*1 + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Race_Black_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*0 + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Race_Hispanic_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*1 + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Race_Hispanic_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*0 + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Race_Other_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*1 +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Race_Other_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*0 +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Republican_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*1 + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Republican_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*0 + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Independent_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*0 + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Independent_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*1 + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Party_other_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*1 +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Party_other_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*0 +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Married_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*1 + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Married_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*0 + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Head_of_household_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*0 + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Head_of_household_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*1 + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Own_house_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*0 + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Own_house_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*1 + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Unemployed_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*0 + b_Wave_2020*x_Wave_2020)
  mean_Unemployed_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*1 + b_Wave_2020*x_Wave_2020)
  
  mean_Wave_2020_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*0)
  
  mean_Wave_2020_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*1)
  
  # Marginal effects
  Age_slope = f(mean_xb)*(b_Age)
  Female_slope = mean_Female_1 - mean_Female_0
  Income_slope = f(mean_xb)*(b_Income)
  Education_slope = f(mean_xb)*(b_Education)
  Race_Black_slope = mean_Race_Black_1 - mean_Race_Black_0
  Race_Hispanic_slope = mean_Race_Hispanic_1 - mean_Race_Hispanic_0
  Race_Other_slope = mean_Race_Other_1 - mean_Race_Other_0
  
  Republican_slope = mean_Republican_1 - mean_Republican_0
  Independent_slope = mean_Independent_1 - mean_Independent_0
  Party_other_slope = mean_Party_other_1 - mean_Party_other_0
  
  Married_slope = mean_Married_1 - mean_Married_0
  Head_of_household_slope = mean_Head_of_household_1 - mean_Head_of_household_0
  Household_size_slope = f(mean_xb)*(b_Household_size)
  Own_house_slope = mean_Own_house_1 - mean_Own_house_0
  Wave_2020_slope = mean_Wave_2020_1 - mean_Wave_2020_0
  Unemployed_slope = mean_Unemployed_1 - mean_Unemployed_0
  return(c(Age_slope, Female_slope, Income_slope, Education_slope, Race_Black_slope,
           Race_Hispanic_slope, Race_Other_slope, Republican_slope, Independent_slope,
           Party_other_slope, Married_slope, Head_of_household_slope, Household_size_slope, 
           Own_house_slope, Unemployed_slope, Wave_2020_slope))
}

LPM_ME_at_mean = get_ME_at_mean(happening_lpm, function(x){return(1)}, function(x){return(x)}, data_models)
summary(happening_probit)
probit_ME_at_mean = get_ME_at_mean(happening_probit, dnorm, pnorm, data_models)
probit_ME_at_mean_ER <- maBina(w = happening_probit, x.mean = TRUE, rev.dum = TRUE)

logit_ME_at_mean = get_ME_at_mean(happening_logit, dlogis, plogis, data_models)
logit_ME_at_mean_ER = maBina(w = happening_logit, x.mean = TRUE, rev.dum = TRUE)

results_happening = matrix(c(LPM_ME_at_mean, probit_ME_at_mean, logit_ME_at_mean), 
                 nrow=3, ncol=16,
                 dimnames=list(
                   c("LPM - ME at mean", "probit - ME at mean", "logit - ME at mean"),
                   c("Age", "Female",
                     "Income", "Education", "Race_Black", 
                     "Race_Hispanic", "Race_Other", "Republican", "Independent", 
                     "Party_other", "Married", "Head_of_household", "Household_size", 
                     "Own_house", "Unemployed", "Wave_2020")), byrow=TRUE)

round(results_happening, 3)

# average marginal effect
get_AME = function(estimated_model, f, F, data){
  # Extract betas from the model
  betas = t(data.frame(coef(estimated_model)))
  
  const = betas[1]
  b_Age = betas[2]
  b_Female = betas[3]
  b_Income = betas[4]
  b_Education = betas[5]
  b_Race_Black = betas[6]
  b_Race_Hispanic = betas[7]
  b_Race_Other = betas[8]
  b_Republican = betas[9]
  b_Independent = betas[10]
  b_Party_other = betas[11]
  b_Married = betas[12]
  b_Head_of_household = betas[13]
  b_Household_size = betas[14]
  b_Own_house = betas[15]
  b_Unemployed = betas[16]
  b_Wave_2020 = betas[17]
  
  # Calculate the vector of x * beta for each observation
  xb = const + 
    b_Age * data_models$Age + 
    b_Income * data_models$Income + 
    b_Education * data_models$Education + 
    b_Household_size * data_models$Household_size + 
    b_Female * data_models$Female + 
    b_Race_Black * data_models$Race_Black + 
    b_Race_Hispanic * data_models$Race_Hispanic + 
    b_Race_Other * data_models$Race_Other + 
    b_Republican * data_models$Republican + 
    b_Independent * data_models$Independent + 
    b_Party_other * data_models$Party_other + 
    b_Married * data_models$Married + 
    b_Head_of_household * data_models$Head_of_household + 
    b_Own_house * data_models$Own_house + 
    b_Unemployed * data_models$Unemployed + 
    b_Wave_2020 * data_models$Wave_2020

  calculate_xb <- function(data, exclude_var) {
    xb = const + 
      b_Age * data$Age + 
      b_Income * data$Income + 
      b_Education * data$Education + 
      b_Household_size * data$Household_size
    
    if (!is.null(exclude_var)) {
      if (exclude_var != "Female") xb = xb + b_Female * data$Female
      if (exclude_var != "Race_Black") xb = xb + b_Race_Black * data$Race_Black
      if (exclude_var != "Race_Hispanic") xb = xb + b_Race_Hispanic * data$Race_Hispanic
      if (exclude_var != "Race_Other") xb = xb + b_Race_Other * data$Race_Other
      if (exclude_var != "Republican") xb = xb + b_Republican * data$Republican
      if (exclude_var != "Independent") xb = xb + b_Independent * data$Independent
      if (exclude_var != "Party_other") xb = xb + b_Party_other * data$Party_other
      if (exclude_var != "Married") xb = xb + b_Married * data$Married
      if (exclude_var != "Head_of_household") xb = xb + b_Head_of_household * data$Head_of_household
      if (exclude_var != "Own_house") xb = xb + b_Own_house * data$Own_house
      if (exclude_var != "Unemployed") xb = xb + b_Unemployed * data$Unemployed
      if (exclude_var != "Wave_2020") xb = xb + b_Wave_2020 * data$Wave_2020
    } else {
      xb = xb + 
        b_Female * data$Female + 
        b_Race_Black * data$Race_Black + 
        b_Race_Hispanic * data$Race_Hispanic + 
        b_Race_Other * data$Race_Other + 
        b_Republican * data$Republican + 
        b_Independent * data$Independent + 
        b_Party_other * data$Party_other + 
        b_Married * data$Married + 
        b_Head_of_household * data$Head_of_household + 
        b_Own_house * data$Own_house + 
        b_Unemployed * data$Unemployed + 
        b_Wave_2020 * data$Wave_2020
    }
    
    return (xb)
  }
  variables <- c("Female", "Race_Black", "Race_Hispanic", "Race_Other", "Republican", 
                  "Independent", "Party_other", "Married", "Head_of_household", 
                  "Own_house", "Unemployed", "Wave_2020")
    
  effects <- list()
    
  for (var in variables) {
    xb_1 = calculate_xb(data, var) + get(paste0("b_", var))
    xb_0 = calculate_xb(data, var)
    effects[[paste0(var, "_effect")]] = mean(F(xb_1) - F(xb_0))
  }
    
  numeric_vars <- c("Age", "Income", "Education", "Household_size")
  for (var in numeric_vars) {
    xb = calculate_xb(data, NULL) # Using all variables
    effects[[paste0(var, "_effect")]] = mean(f(xb) * get(paste0("b_", var)))
  }
  return(effects)
}

LPM_AME_happening = get_AME(happening_lpm, function(x){return(1)}, function(x){return(x)}, data_models)
probit_AME_happening = get_AME(happening_probit, dnorm, pnorm, data_models)
logit_AME_happening = get_AME(happening_logit, dlogis, plogis, data_models)

LPM_AME_happening = as.data.frame(LPM_AME_happening)
probit_AME_happening = as.data.frame(probit_AME_happening)
logit_AME_happening = as.data.frame(logit_AME_happening)
row.names(LPM_AME_happening) <- "LPM"
row.names(probit_AME_happening) <- "Probit"
row.names(logit_AME_happening) <- "Logit"

# Combine the data frames
combined_AME <- rbind(LPM_AME_happening, probit_AME_happening, logit_AME_happening)

# Round the values to 3 decimals
combined_AME_rounded <- round(combined_AME, 3)

# Print the combined and rounded data frame
print(combined_AME_rounded)

# likelihood ratio index
LRI_probit = with(happening_probit, deviance/null.deviance) # -2 cancels out
LRI_probit = 1 - LRI_probit

LRI_logit = with(happening_logit, deviance/null.deviance)
LRI_logit = 1 - LRI_logit

LRI = data.frame(LRI_probit, LRI_logit)
round(LRI, 3)

# average probability of correct prediction - probit
coe = coef(happening_probit)
Age = data_models$Age
Income = data_models$Income
Education = data_models$Education
Household_size = data_models$Household_size
Female = as.integer(data_models$Female) - 1
Race_Black = as.integer(data_models$Race_Black) - 1
Race_Hispanic = as.integer(data_models$Race_Hispanic) - 1
Race_Other = as.integer(data_models$Race_Other) - 1
Republican = as.integer(data_models$Republican) - 1
Independent = as.integer(data_models$Independent) - 1
Party_other = as.integer(data_models$Party_other) - 1
Married = as.integer(data_models$Married) - 1
Head_of_household = as.integer(data_models$Head_of_household) - 1
Own_house = as.integer(data_models$Own_house) - 1
Unemployed = as.integer(data_models$Unemployed) - 1
Wave_2020 = as.integer(data_models$Wave_2020) - 1
Happening = data_models$Happening

probit_Fxb = round(pnorm(coe[1] + 
                           coe['Age']*Age + 
                           coe['Income']*Income + 
                           coe['Education']*Education + 
                           coe['Race_Black']*Race_Black + 
                           coe['Race_Hispanic']*Race_Hispanic + 
                           coe['Race_Other']*Race_Other + 
                           coe['Republican']*Republican +
                           coe['Independent']*Independent + 
                           coe['Party_other']*Party_other + 
                           coe['Married']*Married + 
                           coe['Head_of_household']*Head_of_household + 
                           coe['Household_size']*Household_size + 
                           coe['Own_house']*Own_house + 
                           coe['Unemployed']*Unemployed + 
                           coe['Wave_2020']*Wave_2020), digits=0)

APCP_probit = mean((probit_Fxb*Happening) + (1-Happening)*(1-probit_Fxb))
APCP_probit

# average probability of correct prediction - probit
coe = coef(happening_logit)
Age = data_models$Age
Income = data_models$Income
Education = data_models$Education
Household_size = data_models$Household_size
Female = as.integer(data_models$Female) - 1
Race_Black = as.integer(data_models$Race_Black) - 1
Race_Hispanic = as.integer(data_models$Race_Hispanic) - 1
Race_Other = as.integer(data_models$Race_Other) - 1
Republican = as.integer(data_models$Republican) - 1
Independent = as.integer(data_models$Independent) - 1
Party_other = as.integer(data_models$Party_other) - 1
Married = as.integer(data_models$Married) - 1
Head_of_household = as.integer(data_models$Head_of_household) - 1
Own_house = as.integer(data_models$Own_house) - 1
Unemployed = as.integer(data_models$Unemployed) - 1
Wave_2020 = as.integer(data_models$Wave_2020) - 1
Happening = data_models$Happening

logit_Fxb = round(plogis(coe[1] + 
                           coe['Age']*Age + 
                           coe['Income']*Income + 
                           coe['Education']*Education + 
                           coe['Race_Black']*Race_Black + 
                           coe['Race_Hispanic']*Race_Hispanic + 
                           coe['Race_Other']*Race_Other + 
                           coe['Republican']*Republican +
                           coe['Independent']*Independent + 
                           coe['Party_other']*Party_other + 
                           coe['Married']*Married + 
                           coe['Head_of_household']*Head_of_household + 
                           coe['Household_size']*Household_size + 
                           coe['Own_house']*Own_house + 
                           coe['Unemployed']*Unemployed + 
                           coe['Wave_2020']*Wave_2020), digits=0)

APCP_logit = mean((logit_Fxb*Happening) + (1-Happening)*(1-logit_Fxb))
APCP_logit

# Hypothesis testing
# Change in deviance
print('LR:')
with(happening_probit, null.deviance - deviance)
with(happening_logit, null.deviance - deviance)

# Change in degrees of freedom
print('df:')
with(happening_probit, df.null - df.residual)
with(happening_logit, df.null - df.residual)

# Chi square test p-value with given degrees of freedom
print('p-value')
with(happening_probit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail=FALSE)) # p-value
with(happening_logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail=FALSE)) # p-value

# For Probit
# a) joint significance of all regressors
wald.test(b=coef(happening_probit), Sigma=vcov(happening_probit), Terms=2:16) 

# For Logit
# a) joint significance of all regressors
wald.test(b=coef(happening_logit), Sigma=vcov(happening_logit), Terms=2:16)

# With unemployment rate!
happening_lpm_UR_pooled = lm(Happening ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                         Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, data = data_models)
happening_lpm_UR = plm(Happening ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                         Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, data = data_models, index = "Region", model = "within",)

pFtest(happening_lpm_UR, happening_lpm_UR_pooled)
bptest(happening_lpm_UR)
vcov_W = vcovHC(happening_lpm_UR, type="HC1")
coeftest(happening_lpm_UR, vcov=vcov_W)
summary(happening_lpm_UR_pooled)
# probit - add fe? if possible - no
happening_probit_UR = glm(Happening ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                            Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, family=binomial(link="probit"), data_models)
summary(happening_probit_UR)
confint(happening_probit_UR)

# logit - no
happening_probit_UR = glm(Happening ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                            Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, family=binomial(link="logit"), data_models)
summary(happening_probit_UR)

############################
########## causes  #########
############################
summary(as.factor(data_models$cause_recoded))
data_models$Human_Cause = ifelse(data_models$cause_recoded %in% c('Caused mostly by human activities', 'Caused by human activities and natural changes'), 1, 0)
#data_models = subset(data_models, cause_recoded == 'Caused mostly by human activities' )
# Linear Probability model
# Linear Probability model - with binary for wave
Human_Cause_lpm = lm(Human_Cause ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                       Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Wave_2020, data = data_models)
bptest(Human_Cause_lpm)
resettest(Human_Cause_lpm)
summary(Human_Cause_lpm)
vcov_W = vcovHC(Human_Cause_lpm, type="HC1")
coeftest(Human_Cause_lpm, vcov=vcov_W)

# probit
Human_Cause_probit = glm(Human_Cause ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                           Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Wave_2020, family=binomial(link="probit"), data = data_models, x = T)
summary(Human_Cause_probit)
logLik(Human_Cause_probit)
#confint(Human_Cause_probit)

# logit
Human_Cause_logit = glm(Human_Cause ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                          Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Wave_2020, family=binomial(link="logit"), data = data_models, x = T)
summary(Human_Cause_logit)
logLik(Human_Cause_logit)
# do marginal effects !
LPM_ME_at_mean = get_ME_at_mean(Human_Cause_lpm, function(x){return(1)}, function(x){return(x)}, data_models)

probit_ME_at_mean = get_ME_at_mean(happening_probit, dnorm, pnorm, data_models)
probit_ME_at_mean_ER <- maBina(w = Human_Cause_probit, x.mean = TRUE, rev.dum = TRUE)

logit_ME_at_mean = get_ME_at_mean(happening_logit, dlogis, plogis, data_models)
logit_ME_at_mean_ER = maBina(w = Human_Cause_logit, x.mean = TRUE, rev.dum = TRUE)
results_Human_Cause = matrix(c(LPM_ME_at_mean, probit_ME_at_mean, logit_ME_at_mean), 
                 nrow=3, ncol=16,
                 dimnames=list(
                   c("LPM - ME at mean", "probit - ME at mean", "logit - ME at mean"),
                   c("Age", "Female",
                     "Income", "Education", "Race_Black", 
                     "Race_Hispanic", "Race_Other", "Republican", "Independent", 
                     "Party_other", "Married", "Head_of_household", "Household_size", 
                     "Own_house", "Unemployed", "Wave_2020")), byrow=TRUE)
round(results_happening, 3)
round(results_Human_Cause, 3)

LPM_AME_Human_Cause = get_AME(Human_Cause_lpm, function(x){return(1)}, function(x){return(x)}, data_models)
probit_AME_Human_Cause = get_AME(Human_Cause_probit, dnorm, pnorm, data_models)
logit_AME_Human_Cause = get_AME(Human_Cause_logit, dlogis, plogis, data_models)

as.data.frame(LPM_AME_happening)
as.data.frame(probit_AME_happening)
as.data.frame(logit_AME_happening)

as.data.frame(LPM_AME_Human_Cause)
as.data.frame(probit_AME_Human_Cause)
as.data.frame(logit_AME_Human_Cause)

# likelihood ratio index
LRI_probit = with(Human_Cause_probit, deviance/null.deviance) # -2 cancels out
LRI_probit = 1 - LRI_probit

LRI_logit = with(Human_Cause_logit, deviance/null.deviance)
LRI_logit = 1 - LRI_logit

LRI = data.frame(LRI_probit, LRI_logit)
round(LRI, 3)

# Hypothesis testing
# Change in deviance
print('LR:')
with(Human_Cause_probit, null.deviance - deviance)
with(Human_Cause_logit, null.deviance - deviance)

# Change in degrees of freedom
print('df:')
with(Human_Cause_probit, df.null - df.residual)
with(Human_Cause_logit, df.null - df.residual)

# Chi square test p-value with given degrees of freedom
print('p-value')
with(Human_Cause_probit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail=FALSE)) # p-value
with(Human_Cause_logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail=FALSE)) # p-value

# For Probit
# a) joint significance of all regressors
wald.test(b=coef(Human_Cause_probit), Sigma=vcov(Human_Cause_probit), Terms=2:16) 

# For Logit
# a) joint significance of all regressors
wald.test(b=coef(Human_Cause_logit), Sigma=vcov(Human_Cause_logit), Terms=2:16)


# With unemployment rate!
Human_Cause_lpm_UR_pooled = lm(Human_Cause ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                           Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, data = data_models)
Human_Cause_lpm_UR = plm(Human_Cause ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                           Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, data = data_models, index = "Region", model = "within")

summary(Human_Cause_lpm_UR)
pFtest(Human_Cause_lpm_UR, Human_Cause_lpm_UR_pooled)
bptest(Human_Cause_lpm_UR)
vcov_W = vcovHC(Human_Cause_lpm_UR, type="HC1")
coeftest(Human_Cause_lpm_UR, vcov=vcov_W)

# probit 
Human_Cause_probit_UR = glm(Human_Cause ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                              Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, family=binomial(link="probit"), data_models)
summary(Human_Cause_probit_UR)
confint(Human_Cause_probit_UR)

# logit - no
Human_Cause_probit_UR = glm(Human_Cause ~  Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                              Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, family=binomial(link="logit"), data_models)
summary(Human_Cause_probit_UR)

##########################################################
##########################################################
##########################################################
##########   ROBUSTNESS CHECK - APRIL 2022 ###############
##########################################################
##########################################################
##########################################################

###############################
######## Main Analysis ########
###############################
unique(data$wave)
our_data = subset(data, wave == "Apr 2020" | wave == "Apr 2022")
#our_data = subset(our_data, happening != "Don't know")
#our_data = subset(our_data, select = c(happening, wave, gender, age, race, marit_status, employment, house_own, house_size, income, educ, educ_category, party, region9))

our_data$party = as.factor(our_data$party)
unique(our_data$happening)
table(as.factor(our_data$happening))
our_data$happening = ifelse(our_data$happening == "Yes", 1, 0)

unique(our_data$income)
our_data$income_numeric = our_data$income %>%
  str_extract_all("\\$\\d+,?\\d*") %>%
  lapply(function(x) gsub("\\$", "", x)) %>%
  lapply(function(x) gsub(",", "", x)) %>%
  sapply(function(x) as.numeric(x)) %>%
  sapply(function(x) mean(x, na.rm = TRUE))

unique(our_data$educ)
our_data$years_education = ifelse(our_data$educ == "High school graduate - high school diploma or the equivalent (GED)", 12,
                                  ifelse(our_data$educ == "10th grade", 10,
                                         ifelse(our_data$educ == "Professional or Doctorate degree", 21, # Adjust if needed
                                                ifelse(our_data$educ == "Master's degree", 18,
                                                       ifelse(our_data$educ == "Some college, no degree", 14, # Average value, adjust as needed
                                                              ifelse(our_data$educ == "Bachelor's degree", 16,
                                                                     ifelse(our_data$educ == "Associate's degree", 14,
                                                                            ifelse(our_data$educ == "11th grade", 11,
                                                                                   ifelse(our_data$educ == "9th grade", 9,
                                                                                          ifelse(our_data$educ == "12th grade no diploma", 12,
                                                                                                 ifelse(our_data$educ == "No formal education", 0,
                                                                                                        ifelse(our_data$educ == "7th or 8th grade", 7.5, # Average of 7th and 8th grade
                                                                                                               ifelse(our_data$educ == "5th or 6th grade", 5.5, # Average of 5th and 6th grade
                                                                                                                      ifelse(our_data$educ == "1st, 2nd, 3rd, or 4th grade", 2.5, NA))))))))))))))
summary(our_data)
our_data$gender = as.factor(our_data$gender)

unique(our_data$employment)
our_data$Unemployed = ifelse(our_data$employment == "Not working - looking for work" | our_data$employment == "Not working - on temporary layoff from a job", 1, 0)

unique(our_data$race)
our_data$race = as.factor(our_data$race)

unique(our_data$marit_status)
our_data$marit_status_dummy = ifelse(our_data$marit_status == "Married", 1, 0)

unique(our_data$house_own)
our_data$house_own_dummy = ifelse(our_data$house_own =="Owned by you or someone in your household", 1, 0)

unique(our_data$wave)
our_data$wave = as.factor(our_data$wave)

head(our_data)
summary(our_data)

model_p2020 = lm(happening ~ income_numeric + gender + age + years_education + Unemployed + house_size + race + marit_status_dummy + house_own_dummy + wave + party, data = our_data)
summary(model_p2020)

#################################
### Linking unemployment data ###
#################################

unemp_regional = read.csv("/Users/pavlina/Documents/ies/1st_Semester/Advanced_Econometrics/Project/unemployment_regional.csv")

unique(our_data$region9)

unemp_regional$region9 = NA
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD810000000000003","New England", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD820000000000003","Mid-Atlantic", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD830000000000003","East-North Central", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD840000000000003","West-North Central", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD850000000000003","South Atlantic", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD860000000000003","East-South Central", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD870000000000003","West-South Central", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD880000000000003","Mountain", unemp_regional$region9)
unemp_regional$region9 = ifelse(unemp_regional$Series.ID =="LASRD890000000000003","Pacific", unemp_regional$region9)

unemp_regional = subset(unemp_regional, Label == "2020 Apr" | Label == "2022 Apr")
unemp_regional$wave = ifelse(unemp_regional$Label == "2020 Apr", "Apr 2020", "Apr 2022")
unemp_regional = subset(unemp_regional, select = c(wave, region9, Value))


our_data = merge(our_data, unemp_regional, by.x = c('wave', 'region9'), by.y = c('wave', 'region9'), all.x = T)

model_UR = lm(happening ~ income_numeric + gender + age + years_education + Unemployed + house_size + race + marit_status_dummy + house_own_dummy + Value + party, data = our_data)
summary(model_UR)

################################
############# Data #############
################################

summary(our_data$happening)
our_data$harm_personally = as.factor(our_data$harm_personally)
our_data$harm_future_gen = as.factor(our_data$harm_future_gen)
our_data$reg_CO2_pollutant = as.factor(our_data$reg_CO2_pollutant)
summary(our_data$harm_personally)
summary(our_data$harm_future_gen)
summary(our_data$reg_CO2_pollutant)

summary(our_data$income_numeric)
summary(our_data$gender)
summary(our_data$age)
summary(our_data$years_education)
summary(our_data$unemployment)
summary(our_data$house_size)
summary(our_data$race)
summary(our_data$marit_status_dummy)
summary(our_data$house_own_dummy)
summary(our_data$Value)
summary(our_data$party) # i would do Democrat/republican/independent/other

colnames(our_data)
data_models = subset(our_data, select = c(wave, happening, worry, harm_personally, harm_future_gen, reg_CO2_pollutant, fund_research, gender, age, race, party, religion, marit_status, 
                                          house_head, house_size, house_own, income_numeric, years_education, Value, region9, discuss_GW, cause_recoded, Unemployed))

# new columns and renaming columns
data_models$Happening = data_models$happening

data_models$Region = as.factor(data_models$region9)
unique(data_models$Region)

unique(data_models$worry)
data_models$Worried = ifelse(data_models$worry == "Somewhat worried" | data_models$worry == "Very worried", 1, 0)

table(data_models$reg_CO2_pollutant)
data_models$Support_regulation = ifelse(data_models$reg_CO2_pollutant == "Somewhat support" | data_models$reg_CO2_pollutant == "Strongly support", 1, 0)

table(data_models$fund_research)
data_models$Fund_research = ifelse(data_models$fund_research == "Somewhat support" | data_models$fund_research == "Strongly support", 1, 0)

data_models$Female = ifelse(data_models$gender == "Female", 1, 0)
data_models$Age = data_models$age

unique(data_models$race)
data_models$Race_Black = ifelse(data_models$race == 'Black, Non-Hispanic', 1, 0)
data_models$Race_Hispanic = ifelse(data_models$race == 'Hispanic', 1, 0)
data_models$Race_Other = ifelse(data_models$race == 'Other, Non-Hispanic', 1, 0)
data_models$Race_White = ifelse(data_models$race == 'White, Non-Hispanic', 1, 0)

unique(data_models$party)
data_models$Democrat= ifelse(data_models$party == 'Democrat', 1, 0)
data_models$Republican= ifelse(data_models$party == 'Republican', 1, 0)
data_models$Independent= ifelse(data_models$party == 'Independent', 1, 0)
data_models$Party_other = ifelse(!data_models$party %in% c('Independent', 'Democrat', 'Republican'), 1, 0)

unique(data_models$marit_status)
data_models$marit_status = as.factor(data_models$marit_status)
table(data_models$marit_status)

data_models$Married= ifelse(data_models$marit_status == 'Married', 1, 0)

unique(data$employment)
data_models$Head_of_household= ifelse(data_models$house_head == 'Head of household', 1, 0)

data_models$Household_size = data_models$house_size

data_models$Own_house = ifelse(data_models$house_own =="Owned by you or someone in your household", 1, 0)

data_models$Income = data_models$income_numeric/10000

data_models$Education = data_models$years_education

data_models$Unemployment_rate = data_models$Value

data_models$Wave_2020= ifelse(data_models$wave == 'Apr 2020', 1, 0)

colnames(data_models)
#data_models_attempt = subset(data_models, select = -c(happening, worry, reg_CO2_pollutant, fund_research, gender, age, race,
#                                                      party, religion, marit_status, house_head, house_size, house_own, income_numeric, years_education, Value, wave, region9))

data_models = subset(data_models, select = -c(happening, worry, harm_personally, harm_future_gen, reg_CO2_pollutant, fund_research, gender, age, race, discuss_GW,
                                              party, religion, marit_status, house_head, house_size, house_own, income_numeric, years_education, Value, wave, region9, Fund_research))

colnames(data_models)

hist(data_models$Age)
hist(data_models$Household_size)
hist(data_models$Income)
hist(data_models$Education)

new_order <- c("cause_recoded", "Happening", "Region", "Worried" , "Support_regulation", "Age", "Female",
               "Income", "Education", "Race_White", "Race_Black", 
               "Race_Hispanic", "Race_Other", "Democrat", "Republican", "Independent", 
               "Party_other", "Married", "Head_of_household", "Household_size", 
               "Own_house", "Unemployed", "Wave_2020", "Unemployment_rate")
data_models = data_models[, new_order]

#summary stats
as.data.frame(sapply(data_models[,-c(1,3)], sd))
summary(data_models)

################################
################################
########### Analysis ###########
################################
################################

############################
######### Happening ########
############################

# Linear Probability model - with binary for wave
happening_lpm = lm(Happening ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                     Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Wave_2020, data = data_models)
bptest(happening_lpm)
summary(happening_lpm)
vcov_W = vcovHC(happening_lpm, type="HC1")
coeftest(happening_lpm, vcov=vcov_W)

# probit
happening_probit = glm(Happening ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                         Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Wave_2020, family=binomial(link="probit"), data_models)
summary(happening_probit)
logLik(happening_probit)
#confint(happening_probit)

# logit
happening_logit = glm(Happening ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                        Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Wave_2020, family=binomial(link="logit"), data_models)
summary(happening_logit)
logLik(happening_logit)

# do marginal effects !
get_ME_at_mean = function(estimated_model, f, F, data){
  # Extract betas from the model
  betas = t(data.frame(coef(estimated_model)))
  
  const = betas[1]
  b_Age = betas[2]
  b_Female = betas[3]
  b_Income = betas[4]
  b_Education = betas[5]
  b_Race_Black = betas[6]
  b_Race_Hispanic = betas[7]
  b_Race_Other = betas[8]
  b_Republican = betas[9]
  b_Independent = betas[10]
  b_Party_other = betas[11]
  b_Married = betas[12]
  b_Head_of_household = betas[13]
  b_Household_size = betas[14]
  b_Own_house = betas[15]
  b_Unemployed = betas[16]
  b_Wave_2020 = betas[17]
  
  # Calculate mean values of x
  x_Age = mean(data$Age)
  x_Female = mean(c(data$Female))
  x_Income = mean(data$Income)
  x_Education = mean(data$Education)
  x_Race_Black = mean(c(data$Race_Black))
  x_Race_Hispanic = mean(c(data$Race_Hispanic))
  x_Race_Other = mean(c(data$Race_Other))
  x_Republican = mean(c(data$Republican))
  x_Independent = mean(c(data$Independent))
  x_Party_other = mean(c(data$Party_other))
  x_Married = mean(c(data$Married))
  x_Head_of_household = mean(c(data$Head_of_household))
  x_Household_size = mean(data$Household_size)
  x_Own_house = mean(c(data$Own_house))
  x_Unemployed = mean(c(data$Unemployed))
  x_Wave_2020 = mean(c(data$Wave_2020))
  
  # Calculate x * beta at mean values of x
  mean_xb = const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
    b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
    b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
    b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
    b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020
  mean_Female_1 = F(const + b_Age*x_Age + b_Female*1 + b_Income*x_Income + b_Education*x_Education +
                      b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                      b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                      b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                      b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Female_0 = F(const + b_Age*x_Age + b_Female*0 + b_Income*x_Income + b_Education*x_Education +
                      b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                      b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                      b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                      b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Race_Black_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                          b_Race_Black*1 + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                          b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                          b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                          b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Race_Black_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                          b_Race_Black*0 + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                          b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                          b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                          b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Race_Hispanic_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                             b_Race_Black*x_Race_Black + b_Race_Hispanic*1 + b_Race_Other*x_Race_Other +
                             b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                             b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                             b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Race_Hispanic_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                             b_Race_Black*x_Race_Black + b_Race_Hispanic*0 + b_Race_Other*x_Race_Other +
                             b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                             b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                             b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Race_Other_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                          b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*1 +
                          b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                          b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                          b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Race_Other_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                          b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*0 +
                          b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                          b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                          b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Republican_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                          b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                          b_Republican*1 + b_Independent*x_Independent + b_Party_other*x_Party_other +
                          b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                          b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Republican_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                          b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                          b_Republican*0 + b_Independent*x_Independent + b_Party_other*x_Party_other +
                          b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                          b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Independent_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                           b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                           b_Republican*x_Republican + b_Independent*0 + b_Party_other*x_Party_other +
                           b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                           b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Independent_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                           b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                           b_Republican*x_Republican + b_Independent*1 + b_Party_other*x_Party_other +
                           b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                           b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Party_other_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                           b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                           b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*1 +
                           b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                           b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Party_other_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                           b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                           b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*0 +
                           b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                           b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Married_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                       b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                       b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                       b_Married*1 + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                       b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Married_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                       b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                       b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                       b_Married*0 + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                       b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Head_of_household_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                                 b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                                 b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                                 b_Married*x_Married + b_Head_of_household*0 + b_Household_size*x_Household_size +
                                 b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Head_of_household_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                                 b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                                 b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                                 b_Married*x_Married + b_Head_of_household*1 + b_Household_size*x_Household_size +
                                 b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Own_house_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                         b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                         b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                         b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                         b_Own_house*0 + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  mean_Own_house_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                         b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                         b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                         b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                         b_Own_house*1 + b_Unemployed*x_Unemployed + b_Wave_2020*x_Wave_2020)
  
  mean_Unemployed_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                          b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                          b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                          b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                          b_Own_house*x_Own_house + b_Unemployed*0 + b_Wave_2020*x_Wave_2020)
  mean_Unemployed_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                          b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                          b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                          b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                          b_Own_house*x_Own_house + b_Unemployed*1 + b_Wave_2020*x_Wave_2020)
  
  mean_Wave_2020_0 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                         b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                         b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                         b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                         b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*0)
  
  mean_Wave_2020_1 = F(const + b_Age*x_Age + b_Female*x_Female + b_Income*x_Income + b_Education*x_Education +
                         b_Race_Black*x_Race_Black + b_Race_Hispanic*x_Race_Hispanic + b_Race_Other*x_Race_Other +
                         b_Republican*x_Republican + b_Independent*x_Independent + b_Party_other*x_Party_other +
                         b_Married*x_Married + b_Head_of_household*x_Head_of_household + b_Household_size*x_Household_size +
                         b_Own_house*x_Own_house + b_Unemployed*x_Unemployed + b_Wave_2020*1)
  
  # Marginal effects
  Age_slope = f(mean_xb)*(b_Age)
  Female_slope = mean_Female_1 - mean_Female_0
  Income_slope = f(mean_xb)*(b_Income)
  Education_slope = f(mean_xb)*(b_Education)
  Race_Black_slope = mean_Race_Black_1 - mean_Race_Black_0
  Race_Hispanic_slope = mean_Race_Hispanic_1 - mean_Race_Hispanic_0
  Race_Other_slope = mean_Race_Other_1 - mean_Race_Other_0
  
  Republican_slope = mean_Republican_1 - mean_Republican_0
  Independent_slope = mean_Independent_1 - mean_Independent_0
  Party_other_slope = mean_Party_other_1 - mean_Party_other_0
  
  Married_slope = mean_Married_1 - mean_Married_0
  Head_of_household_slope = mean_Head_of_household_1 - mean_Head_of_household_0
  Household_size_slope = f(mean_xb)*(b_Household_size)
  Own_house_slope = mean_Own_house_1 - mean_Own_house_0
  Wave_2020_slope = mean_Wave_2020_1 - mean_Wave_2020_0
  Unemployed_slope = mean_Unemployed_1 - mean_Unemployed_0
  return(c(Age_slope, Female_slope, Income_slope, Education_slope, Race_Black_slope,
           Race_Hispanic_slope, Race_Other_slope, Republican_slope, Independent_slope,
           Party_other_slope, Married_slope, Head_of_household_slope, Household_size_slope, 
           Own_house_slope, Unemployed_slope, Wave_2020_slope))
}

LPM_ME_at_mean = get_ME_at_mean(happening_lpm, function(x){return(1)}, function(x){return(x)}, data_models)
probit_ME_at_mean = get_ME_at_mean(happening_probit, dnorm, pnorm, data_models)
logit_ME_at_mean = get_ME_at_mean(happening_logit, dlogis, plogis, data_models)
results_happening = matrix(c(LPM_ME_at_mean, probit_ME_at_mean, logit_ME_at_mean), 
                           nrow=3, ncol=16,
                           dimnames=list(
                             c("LPM - ME at mean", "probit - ME at mean", "logit - ME at mean"),
                             c("Age", "Female",
                               "Income", "Education", "Race_Black", 
                               "Race_Hispanic", "Race_Other", "Republican", "Independent", 
                               "Party_other", "Married", "Head_of_household", "Household_size", 
                               "Own_house", "Unemployed", "Wave_2020")), byrow=TRUE)

round(results_happening, 3)

# average marginal effect
get_AME = function(estimated_model, f, F, data){
  # Extract betas from the model
  betas = t(data.frame(coef(estimated_model)))
  
  const = betas[1]
  b_Age = betas[2]
  b_Female = betas[3]
  b_Income = betas[4]
  b_Education = betas[5]
  b_Race_Black = betas[6]
  b_Race_Hispanic = betas[7]
  b_Race_Other = betas[8]
  b_Republican = betas[9]
  b_Independent = betas[10]
  b_Party_other = betas[11]
  b_Married = betas[12]
  b_Head_of_household = betas[13]
  b_Household_size = betas[14]
  b_Own_house = betas[15]
  b_Unemployed = betas[16]
  b_Wave_2020 = betas[17]
  
  # Calculate the vector of x * beta for each observation
  xb = const + 
    b_Age * data_models$Age + 
    b_Income * data_models$Income + 
    b_Education * data_models$Education + 
    b_Household_size * data_models$Household_size + 
    b_Female * data_models$Female + 
    b_Race_Black * data_models$Race_Black + 
    b_Race_Hispanic * data_models$Race_Hispanic + 
    b_Race_Other * data_models$Race_Other + 
    b_Republican * data_models$Republican + 
    b_Independent * data_models$Independent + 
    b_Party_other * data_models$Party_other + 
    b_Married * data_models$Married + 
    b_Head_of_household * data_models$Head_of_household + 
    b_Own_house * data_models$Own_house + 
    b_Unemployed * data_models$Unemployed + 
    b_Wave_2020 * data_models$Wave_2020
  
  calculate_xb <- function(data, exclude_var) {
    xb = const + 
      b_Age * data$Age + 
      b_Income * data$Income + 
      b_Education * data$Education + 
      b_Household_size * data$Household_size
    
    if (!is.null(exclude_var)) {
      if (exclude_var != "Female") xb = xb + b_Female * data$Female
      if (exclude_var != "Race_Black") xb = xb + b_Race_Black * data$Race_Black
      if (exclude_var != "Race_Hispanic") xb = xb + b_Race_Hispanic * data$Race_Hispanic
      if (exclude_var != "Race_Other") xb = xb + b_Race_Other * data$Race_Other
      if (exclude_var != "Republican") xb = xb + b_Republican * data$Republican
      if (exclude_var != "Independent") xb = xb + b_Independent * data$Independent
      if (exclude_var != "Party_other") xb = xb + b_Party_other * data$Party_other
      if (exclude_var != "Married") xb = xb + b_Married * data$Married
      if (exclude_var != "Head_of_household") xb = xb + b_Head_of_household * data$Head_of_household
      if (exclude_var != "Own_house") xb = xb + b_Own_house * data$Own_house
      if (exclude_var != "Unemployed") xb = xb + b_Unemployed * data$Unemployed
      if (exclude_var != "Wave_2020") xb = xb + b_Wave_2020 * data$Wave_2020
    } else {
      xb = xb + 
        b_Female * data$Female + 
        b_Race_Black * data$Race_Black + 
        b_Race_Hispanic * data$Race_Hispanic + 
        b_Race_Other * data$Race_Other + 
        b_Republican * data$Republican + 
        b_Independent * data$Independent + 
        b_Party_other * data$Party_other + 
        b_Married * data$Married + 
        b_Head_of_household * data$Head_of_household + 
        b_Own_house * data$Own_house + 
        b_Unemployed * data$Unemployed + 
        b_Wave_2020 * data$Wave_2020
    }
    
    return (xb)
  }
  variables <- c("Female", "Race_Black", "Race_Hispanic", "Race_Other", "Republican", 
                 "Independent", "Party_other", "Married", "Head_of_household", 
                 "Own_house", "Unemployed", "Wave_2020")
  
  effects <- list()
  
  for (var in variables) {
    xb_1 = calculate_xb(data, var) + get(paste0("b_", var))
    xb_0 = calculate_xb(data, var)
    effects[[paste0(var, "_effect")]] = mean(F(xb_1) - F(xb_0))
  }
  
  numeric_vars <- c("Age", "Income", "Education", "Household_size")
  for (var in numeric_vars) {
    xb = calculate_xb(data, NULL) # Using all variables
    effects[[paste0(var, "_effect")]] = mean(f(xb) * get(paste0("b_", var)))
  }
  return(effects)
}

LPM_AME_happening = get_AME(happening_lpm, function(x){return(1)}, function(x){return(x)}, data_models)
probit_AME_happening = get_AME(happening_probit, dnorm, pnorm, data_models)
logit_AME_happening = get_AME(happening_logit, dlogis, plogis, data_models)

LPM_AME_happening = as.data.frame(LPM_AME_happening)
probit_AME_happening = as.data.frame(probit_AME_happening)
logit_AME_happening = as.data.frame(logit_AME_happening)
row.names(LPM_AME_happening) <- "LPM"
row.names(probit_AME_happening) <- "Probit"
row.names(logit_AME_happening) <- "Logit"

# Combine the data frames
combined_AME <- rbind(LPM_AME_happening, probit_AME_happening, logit_AME_happening)

# Round the values to 3 decimals
combined_AME_rounded <- round(combined_AME, 3)

# Print the combined and rounded data frame
print(combined_AME_rounded)

# likelihood ratio index
LRI_probit = with(happening_probit, deviance/null.deviance) # -2 cancels out
LRI_probit = 1 - LRI_probit

LRI_logit = with(happening_logit, deviance/null.deviance)
LRI_logit = 1 - LRI_logit

LRI = data.frame(LRI_probit, LRI_logit)
round(LRI, 3)

# average probability of correct prediction - probit
coe = coef(happening_probit)
Age = data_models$Age
Income = data_models$Income
Education = data_models$Education
Household_size = data_models$Household_size
Female = as.integer(data_models$Female) - 1
Race_Black = as.integer(data_models$Race_Black) - 1
Race_Hispanic = as.integer(data_models$Race_Hispanic) - 1
Race_Other = as.integer(data_models$Race_Other) - 1
Republican = as.integer(data_models$Republican) - 1
Independent = as.integer(data_models$Independent) - 1
Party_other = as.integer(data_models$Party_other) - 1
Married = as.integer(data_models$Married) - 1
Head_of_household = as.integer(data_models$Head_of_household) - 1
Own_house = as.integer(data_models$Own_house) - 1
Unemployed = as.integer(data_models$Unemployed) - 1
Wave_2020 = as.integer(data_models$Wave_2020) - 1
Happening = data_models$Happening

probit_Fxb = round(pnorm(coe[1] + 
                           coe['Age']*Age + 
                           coe['Income']*Income + 
                           coe['Education']*Education + 
                           coe['Race_Black']*Race_Black + 
                           coe['Race_Hispanic']*Race_Hispanic + 
                           coe['Race_Other']*Race_Other + 
                           coe['Republican']*Republican +
                           coe['Independent']*Independent + 
                           coe['Party_other']*Party_other + 
                           coe['Married']*Married + 
                           coe['Head_of_household']*Head_of_household + 
                           coe['Household_size']*Household_size + 
                           coe['Own_house']*Own_house + 
                           coe['Unemployed']*Unemployed + 
                           coe['Wave_2020']*Wave_2020), digits=0)

APCP_probit = mean((probit_Fxb*Happening) + (1-Happening)*(1-probit_Fxb))
APCP_probit

# average probability of correct prediction - probit
coe = coef(happening_logit)
Age = data_models$Age
Income = data_models$Income
Education = data_models$Education
Household_size = data_models$Household_size
Female = as.integer(data_models$Female) - 1
Race_Black = as.integer(data_models$Race_Black) - 1
Race_Hispanic = as.integer(data_models$Race_Hispanic) - 1
Race_Other = as.integer(data_models$Race_Other) - 1
Republican = as.integer(data_models$Republican) - 1
Independent = as.integer(data_models$Independent) - 1
Party_other = as.integer(data_models$Party_other) - 1
Married = as.integer(data_models$Married) - 1
Head_of_household = as.integer(data_models$Head_of_household) - 1
Own_house = as.integer(data_models$Own_house) - 1
Unemployed = as.integer(data_models$Unemployed) - 1
Wave_2020 = as.integer(data_models$Wave_2020) - 1
Happening = data_models$Happening

logit_Fxb = round(plogis(coe[1] + 
                           coe['Age']*Age + 
                           coe['Income']*Income + 
                           coe['Education']*Education + 
                           coe['Race_Black']*Race_Black + 
                           coe['Race_Hispanic']*Race_Hispanic + 
                           coe['Race_Other']*Race_Other + 
                           coe['Republican']*Republican +
                           coe['Independent']*Independent + 
                           coe['Party_other']*Party_other + 
                           coe['Married']*Married + 
                           coe['Head_of_household']*Head_of_household + 
                           coe['Household_size']*Household_size + 
                           coe['Own_house']*Own_house + 
                           coe['Unemployed']*Unemployed + 
                           coe['Wave_2020']*Wave_2020), digits=0)

APCP_logit = mean((logit_Fxb*Happening) + (1-Happening)*(1-logit_Fxb))
APCP_logit

# Hypothesis testing
# Change in deviance
print('LR:')
with(happening_probit, null.deviance - deviance)
with(happening_logit, null.deviance - deviance)

# Change in degrees of freedom
print('df:')
with(happening_probit, df.null - df.residual)
with(happening_logit, df.null - df.residual)

# Chi square test p-value with given degrees of freedom
print('p-value')
with(happening_probit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail=FALSE)) # p-value
with(happening_logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail=FALSE)) # p-value

# For Probit
# a) joint significance of all regressors
wald.test(b=coef(happening_probit), Sigma=vcov(happening_probit), Terms=2:16) 

# For Logit
# a) joint significance of all regressors
wald.test(b=coef(happening_logit), Sigma=vcov(happening_logit), Terms=2:16)

# With unemployment rate!
happening_lpm_UR = plm(Happening ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                         Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, data = data_models, index = "Region", model = "within",)

bptest(happening_lpm_UR)
vcov_W = vcovHC(happening_lpm_UR, type="HC1")
coeftest(happening_lpm_UR, vcov=vcov_W)

# probit - add fe? if possible
happening_probit_UR = glm(Happening ~  Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                            Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, family=binomial(link="probit"), data_models)
summary(happening_probit_UR)
confint(happening_probit_UR)

# logit
happening_probit_UR = glm(Happening ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                            Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, family=binomial(link="logit"), data_models)
summary(happening_probit_UR)

############################
########## causes  #########
############################
summary(as.factor(data_models$cause_recoded))
data_models$Human_Cause = ifelse(data_models$cause_recoded %in% c('Caused mostly by human activities', 'Caused by human activities and natural changes '), 1, 0)
#data_models = subset(data_models, cause_recoded == 'Caused mostly by human activities')
# Linear Probability model
# Linear Probability model - with binary for wave
Human_Cause_lpm = lm(Human_Cause ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                       Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Wave_2020, data = data_models)
bptest(Human_Cause_lpm)
summary(Human_Cause_lpm)
vcov_W = vcovHC(Human_Cause_lpm, type="HC1")
coeftest(Human_Cause_lpm, vcov=vcov_W)

# probit
Human_Cause_probit = glm(Human_Cause ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                           Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Wave_2020, family=binomial(link="probit"), data_models)
summary(Human_Cause_probit)
logLik(Human_Cause_probit)
#confint(Human_Cause_probit)

# logit
Human_Cause_logit = glm(Human_Cause ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                          Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Wave_2020, family=binomial(link="logit"), data_models)
summary(Human_Cause_logit)
logLik(Human_Cause_logit)

# do marginal effects !
LPM_ME_at_mean = get_ME_at_mean(Human_Cause_lpm, function(x){return(1)}, function(x){return(x)}, data_models)
probit_ME_at_mean = get_ME_at_mean(Human_Cause_probit, dnorm, pnorm, data_models)
logit_ME_at_mean = get_ME_at_mean(Human_Cause_logit, dlogis, plogis, data_models)
results_Human_Cause = matrix(c(LPM_ME_at_mean, probit_ME_at_mean, logit_ME_at_mean), 
                             nrow=3, ncol=16,
                             dimnames=list(
                               c("LPM - ME at mean", "probit - ME at mean", "logit - ME at mean"),
                               c("Age", "Female",
                                 "Income", "Education", "Race_Black", 
                                 "Race_Hispanic", "Race_Other", "Republican", "Independent", 
                                 "Party_other", "Married", "Head_of_household", "Household_size", 
                                 "Own_house", "Unemployed", "Wave_2020")), byrow=TRUE)
round(results_happening, 3)
round(results_Human_Cause, 3)

LPM_AME_Human_Cause = get_AME(Human_Cause_lpm, function(x){return(1)}, function(x){return(x)}, data_models)
probit_AME_Human_Cause = get_AME(Human_Cause_probit, dnorm, pnorm, data_models)
logit_AME_Human_Cause = get_AME(Human_Cause_logit, dlogis, plogis, data_models)

as.data.frame(LPM_AME_happening)
as.data.frame(probit_AME_happening)
as.data.frame(logit_AME_happening)

as.data.frame(LPM_AME_Human_Cause)
as.data.frame(probit_AME_Human_Cause)
as.data.frame(logit_AME_Human_Cause)

# likelihood ratio index
LRI_probit = with(Human_Cause_probit, deviance/null.deviance) # -2 cancels out
LRI_probit = 1 - LRI_probit

LRI_logit = with(Human_Cause_logit, deviance/null.deviance)
LRI_logit = 1 - LRI_logit

LRI = data.frame(LRI_probit, LRI_logit)
round(LRI, 3)

# Hypothesis testing
# Change in deviance
print('LR:')
with(Human_Cause_probit, null.deviance - deviance)
with(Human_Cause_logit, null.deviance - deviance)

# Change in degrees of freedom
print('df:')
with(Human_Cause_probit, df.null - df.residual)
with(Human_Cause_logit, df.null - df.residual)

# Chi square test p-value with given degrees of freedom
print('p-value')
with(Human_Cause_probit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail=FALSE)) # p-value
with(Human_Cause_logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail=FALSE)) # p-value

# For Probit
# a) joint significance of all regressors
wald.test(b=coef(Human_Cause_probit), Sigma=vcov(Human_Cause_probit), Terms=2:16) 

# For Logit
# a) joint significance of all regressors
wald.test(b=coef(Human_Cause_logit), Sigma=vcov(Human_Cause_logit), Terms=2:16)


# With unemployment rate!
Human_Cause_lpm_UR = plm(Human_Cause ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                           Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, data = data_models, index = "Region", model = "within",)

bptest(Human_Cause_lpm_UR)
vcov_W = vcovHC(Human_Cause_lpm_UR, type="HC1")
coeftest(Human_Cause_lpm_UR, vcov=vcov_W)

# probit - add fe? if possible
Human_Cause_probit_UR = glm(Human_Cause ~  Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                              Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, family=binomial(link="probit"), data_models)
summary(Human_Cause_probit_UR)
confint(Human_Cause_probit_UR)

# logit
Human_Cause_probit_UR = glm(Human_Cause ~ Age + Female + Income + Education + Race_Black + Race_Hispanic + Race_Other + Republican +
                              Independent + Party_other + Married + Head_of_household + Household_size + Own_house + Unemployed + Unemployment_rate, family=binomial(link="logit"), data_models)
summary(Human_Cause_probit_UR)