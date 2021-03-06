title: "Machine learning example in R Example"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Stack variables

Post stacking transformations

All other grants
0600 = Baseline
0601 = 6 month resasessment


CCBHC
For assessment see below 3-months is actually vitals and so on
3-month vitals
6 month reassessment
9-month vitals
12 month reassessment

Assessment: 
0600 = Baseline Assessment
0301 = 3 Month Reassessment (vitals)
0302 = 6 Month Reassessment
0303 = 9 Month Reassessment (vitals)
0304 = 12 Month Reassessment
0699 = clincial discharge

Assessment_new
0 = Baseline
1 = 3 month reassessment (vitals)
2 = 6 month reassessment (302 from CCBHC and 601 from all other grants)
3 = 9 month reassessment (vitals)
4 = 12 month reassessment
5 = clinical discharge

Now review the missing data
Only include Baseline and 6-month
telehealth.y means they were in telehealth at 6 months which is what we want

# Data mergeing
For CCBHC IN, IL all the same

telehealth: Telehealth = 1; Pre-telehealth = 0 telehealth defined as those with any assessment date on or after 4-2-2020

### Run this prior to any analysis to load data ####
```{r}
library(prettyR)
library(see)
library(performance)

###
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks")

## Run this for machine learning
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks")
IN_2019 =  read.csv("CCBHC_IN_9_23_20_2019.csv", header = TRUE, na.strings =  c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
IN_2020 = read.csv("CCBHC_IN_9_23_20_2020.csv", header = TRUE, na.strings =  c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
IN = rbind(IN_2019, IN_2020)
# Run this FHHC for machine learning data
FHHC = read.csv("fhhc_noms_9_23_20.csv", header= TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))

ICP = read.csv("SPARS Data Download 5.23.2020_ICP.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
SOCAT = read.csv("SOCAT NOMs download 5.27.20.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
#Run this for machine learning
IL = read.csv("IL_9_23_20.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))

FL_ACT = read.csv("FL-ACT SPARS data download  5.28.2020.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))

## Now stack them
### Create an empty data and then fill it with NAs.  Keep the first 44 those are correct and match
SOCAT$RespondentType = NULL
SOCAT_matrix = matrix(NA, ncol = 185-43, nrow = dim(SOCAT)[1])
SOCAT_matrix = data.frame(SOCAT_matrix)
colnames(SOCAT_matrix) = colnames(ICP[,44:185])
SOCAT_full = data.frame(SOCAT[,1:43], SOCAT_matrix)
dim(SOCAT_full)
### Change variables that match
SOCAT_full$Nervous = SOCAT$Nervous
SOCAT_full$Hopeless = SOCAT$Hopeless
SOCAT_full$Restless = SOCAT$Restless
SOCAT_full$Depressed = SOCAT$Depressed
SOCAT_full$EverythingEffort = SOCAT$EverythingEffort
SOCAT_full$Worthless = SOCAT$Worthless
SOCAT_full$Tobacco_Use = SOCAT$Tobacco_Use
SOCAT_full$Alcohol_Use = SOCAT$Alcohol_Use
SOCAT_full$StreetOpioids_Use = SOCAT$StreetOpioids_Use
SOCAT_full$RxOpioids_Use = SOCAT$RxOpioids_Use
SOCAT_full$NightsHomeless = SOCAT$NightsHomeless
SOCAT_full$NightsHospitalMHC = SOCAT$NightsHospitalMHC
SOCAT_full$NightsDetox = SOCAT$NightsDetox
SOCAT_full$NightsJail = SOCAT$NightsJail
SOCAT_full$TimesER = SOCAT$TimesER
SOCAT_full$Housing = SOCAT$Housing
SOCAT = SOCAT_full



IN_IL_CCBHC = rbind(IN[,1:185], IL[,1:185])
dim(IN_IL_CCBHC)
FHHC = FHHC[,1:185]
ICP = ICP[,1:185]
FL_ACT = FL_ACT[,1:185]
dim(ICP)
dim(SOCAT)
### Add grant ID
IN_IL_CCBHC$grant = rep("IN_IL_CCBHC", dim(IN_IL_CCBHC)[1])
FHHC$grant = rep("FHHC", dim(FHHC)[1])
ICP$grant = rep("ICP", dim(ICP)[1])
SOCAT$grant = rep("SOCAT", dim(SOCAT)[1])
FL_ACT$grant = rep("FL_ACT", dim(FL_ACT)[1])
dim(SOCAT)
telehealth_noms = rbind(IN_IL_CCBHC, FHHC, ICP, SOCAT, FL_ACT)
dim(telehealth_noms)
### Create a new ConsumerID that is a mix of grant and ConsumerID
telehealth_noms$ConsumerID_grant = paste0(telehealth_noms$ConsumerID, telehealth_noms$GrantID)

### Figure out how you can stack FHHC data
dim(telehealth_noms)

## Rename to the above

## No one has multiple reassessments


## Create recoded assessment variable
telehealth_noms$Assessment_new = ifelse(telehealth_noms$Assessment == 600, 0, ifelse(telehealth_noms$Assessment == 301, 1, ifelse(telehealth_noms$Assessment == 302, 2, ifelse(telehealth_noms$Assessment == 303, 3, ifelse(telehealth_noms$Assessment == 601,2, NA)))))
telehealth_noms$Assessment_new = as.numeric(telehealth_noms$Assessment_new)
describe.factor(telehealth_noms$Assessment_new, decr.order= FALSE)
### Create full date variable
telehealth_noms$date = paste0(telehealth_noms$FFY, "-", telehealth_noms$Month, "-", "01")
library(lubridate)
telehealth_noms$date = ymd(telehealth_noms$date)
head(telehealth_noms$date)

telehealth_noms$telehealth = ifelse(telehealth_noms$date >= "2020-04-01", 1, 0)
dim(telehealth_noms)
describe.factor(telehealth_noms$grant)
range(telehealth_noms$date)
### Create a NOMS data set  
telehealth_noms_wide = subset(telehealth_noms, Assessment_new == 0 | Assessment_new == 2)
dim(telehealth_noms)[1]
describe.factor(telehealth_noms$Assessment_new)


####################
library(naniar)
miss_var_summary(telehealth_noms)
miss_var_summary(subset(telehealth_noms, Assessment_new == 2))
miss_var_summary(subset(telehealth_noms, Assessment_new == 0))

### These people have two baselines delete them 'A00276''A00295''A00298'

telehealth_noms_wide = telehealth_noms_wide[order(telehealth_noms_wide$ConsumerID),]
telehealth_noms_wide_test = subset(telehealth_noms_wide, ConsumerID == "'A00276'" | ConsumerID == "'A00295'" | ConsumerID == "'A00298'")
## If there is no interview then delete the second, if there is only one interview delete the none interview, if there are two interviews for baseline delete the second see conductedinterview variable
telehealth_noms_wide[c(1942, 1960, 1965),]
telehealth_noms_wide = telehealth_noms_wide[-c(1942, 1960, 1965),] 

telehealth_noms_base_noms = subset(telehealth_noms_wide,Assessment_new == 0)
telehealth_noms_month6_noms = subset(telehealth_noms_wide,Assessment_new == 2)
describe.factor(telehealth_noms_base_noms$grant)
describe.factor(telehealth_noms_month6_noms$grant)

head(telehealth_noms_base_noms)
dim(telehealth_noms_month6_noms)
telehealth_noms_wide_noms = merge(telehealth_noms_base_noms, telehealth_noms_month6_noms, by = "ConsumerID_grant", all.x = TRUE)
dim(telehealth_noms_wide_noms)
telehealth_noms_wide_noms = telehealth_noms_wide_noms[order(telehealth_noms_wide_noms$ConsumerID_grant),]
telehealth_noms_month6_noms = telehealth_noms_month6_noms[order(telehealth_noms_month6_noms$ConsumerID_grant),]
telehealth_noms_wide_noms$drop_out = ifelse(telehealth_noms_wide_noms$ConductedInterview.y == 0, 1, 0)
describe.factor(telehealth_noms_wide_noms$drop_out)
head(telehealth_noms_month6_noms)
### Use first received services, because the date is always there; however, some vairation (0 to 15 days)
telehealth_noms_wide_noms$FirstReceivedServicesDate.y = mdy(telehealth_noms_wide_noms$FirstReceivedServicesDate.y)
### Only clients who are eligible for 6-month reassessments
### Change this to 2020-09-30
telehealth_noms_wide_noms = subset(telehealth_noms_wide_noms, FirstReceivedServicesDate.y < Sys.Date()-6*30)
```

The code below is an example of using machine learning to predict housing generally based on Kuhn (2019) guide: https://topepo.github.io/caret/

telehealth_noms_wide_noms is a data set from the national outcomes measures (NOMS), and the codebook is available here: https://drive.google.com/file/d/1BBKuV3A6tAaACF5DljuvYLDHRxr1q_iG/view?usp=sharing 

In the section below, I am sub-setting the variables that I want to include.  The variables consist of counts, ordinal, and binary variables.  These data are matched pairs from intake to 6-month (.x is at intake, and .y is 6-month).

Below I demonstrate first how I selected variables that myself and a team of content experts believed were potentially predictive of housing at 6-months.  I then identified that all the "VT" variables, school and work, and living conditions satisfaction variables were missing more significantly more than 50%.  Given these variables were missing significantly more than 50% and research generally discourages imputing variables (discussed later) with more than 50% missing data, we removed those variables (Manly & Wells, 2015).  

We then created several binary demographics from nominal variables.  First, we evaluated the top three primary diagnoses at baseline and created binary variables for those diagnoses.  We also created a sexual identity variable as another sexual identity and heterosexual and a gender variable as male and all other gender identities.  We also included housing at baseline and 6-months (i.e., the outcome variable). 
```{r}

telehealth_noms_wide_noms
machine_dat =  telehealth_noms_wide_noms[c("Quarter.x", "DiagnosisOne.x", "Gender.x", "HispanicLatino.x", "RaceWhite.x", "RaceBlack.x", "Agegroup.x", "SexualIdentity.x", "OverallHealth.x", "CapableManagingHealthCareNeeds.x", "HandlingDailyLife.x", "ControlLife.x", "DealWithCrisis.x", "GetsAlongWithFamily.x", "SocialSituations.x", "SchoolOrWork.x", "FunctioningHousing.x", "Symptoms.x", "Nervous.x", "Hopeless.x", "Restless.x", "Depressed.x", "EverythingEffort.x", "Worthless.x", "PsychologicalEmotionalProblems.x", "LifeQuality.x", "EnoughEnergyForEverydayLife.x", "PerformDailyActivitiesSatisfaction.x", "HealthSatisfaction.x", "RelationshipSatisfaction.x", "SelfSatisfaction.x", "Tobacco_Use.x", "Alcohol_Use.x", "Cannabis_Use.x", "Cocaine_Use.x", "Meth_Use.x", "RxOpioids_Use.x", "StreetOpioids_Use.x", "ViolenceTrauma.x", "VT_NightmaresThoughts.x", "VT_NotThinkAboutIt.x", "VT_OnGuard.x", "VT_NumbDetached.x", "PhysicallyHurt.x", "NightsHospitalMHC.x", "NightsDetox.x", "NightsJail.x", "TimesER.x", "Housing.x", "LivingConditionsSatisfaction.x", "Education.x", "Employment.x", "EnoughMoneyForNeeds.x", "NumTimesArrested.x", "Friendships.x", "EnjoyPeople.x", "BelongInCommunity.x", "SupportFromFamily.x", "SupportiveFamilyFriends.x", "GenerallyAccomplishGoal.x", "telehealth.x", "grant.x", "Inhalants_Use.x", "Sedatives_Use.x", "Hallucinogens_Use.x", "Other_Use.x", "Stimulants_Use.x", "EverServed.x", "ActiveDuty_Else.x", "NightsHomeless.x", "drop_out")]
library(naniar)
miss_var_summary(machine_dat)
# Remove variables missing significantly more than 50%
machine_dat[,c("VT_NightmaresThoughts.x", "VT_NotThinkAboutIt.x", "VT_OnGuard.x", "VT_NumbDetached.x", "SchoolOrWork.x", "LivingConditionsSatisfaction.x")] = list(NULL)


#DiagnosisOne.x 62 = anxiety, 59 = mdd recurrent, 58 mdd single episode
machine_dat$anxiety = ifelse(machine_dat$DiagnosisOne.x == 62, 1, 0) 
machine_dat$mdd_r= ifelse(machine_dat$DiagnosisOne.x == 59, 1, 0)
machine_dat$mdd_s = ifelse(machine_dat$DiagnosisOne.x == 58, 1, 0)
machine_dat$DiagnosisOne.x = NULL
#SexualIdentity.x 1,2,3,4 create another category for 3 and 4
machine_dat$another_sex_ident =  ifelse(machine_dat$SexualIdentity.x > 1, 1, 0)
machine_dat$SexualIdentity.x = NULL

### Change gender to male or female / another gender identity
machine_dat$Gender.x = ifelse(machine_dat$Gender.x == 1,1,0)

#### Employment 
machine_dat$Employment.x = ifelse(machine_dat$Employment.x <3,1,0)

#Housing.x and y 1 = OWNED OR RENTED HOUSE, APARTMENT, TRAILER, ROOM
machine_dat$Housing.x = ifelse(machine_dat$Housing.x == 1, 1,0)
#1= EMPLOYED FULL TIME (35+ HOURS PER WEEK, OR WOULD HAVE BEEN)
#2 = EMPLOYED PART TIME

## Add grant.x break into CCBHC or not
describe.factor(machine_dat$grant.x)
machine_dat$grant.x = ifelse(machine_dat$grant.x == "IN_IL_CCBHC", 1, 0)

```
We next continued to pre-process the data by identifying near-zero variance variables.  We used the nearZeroVar function in caret to identify variables with large (i.e., higher than 19) ratios of the first to second most frequent variables.

We found two variables (Hispanic and physical hurt) the team decided were not critical, and we had no way of increasing the variance.  Therefore we removed both variables.

Next, we identified that drug use, nights/times in the hospital or ER, and times arrested and nights in jail all had low variance.  Given the importance of these variables, we summed each of them, respectively.  Although some variables had mixed types (i.e., nights and times), our goal is to predict housing not necessarily to have meaningful variables.  Also, after summation, jail/arrests and drug use were still near-zero variance.  However, because of their importance in predicting housing, we kept all the summed variables.
```{r}
library(caret)

nzv = nearZeroVar(machine_dat, saveMetrics = TRUE)
nzv

# HispanicLatino.x, PhysicallyHurt.x
machine_dat$HispanicLatino.x = NULL
machine_dat$PhysicallyHurt.x = NULL


machine_dat$drug_use = machine_dat$Cocaine_Use.x + machine_dat$Meth_Use.x + machine_dat$StreetOpioids_Use.x + machine_dat$RxOpioids_Use.x + machine_dat$Stimulants_Use.x + machine_dat$Inhalants_Use.x + machine_dat$Sedatives_Use.x + machine_dat$Hallucinogens_Use.x + machine_dat$Other_Use.x
machine_dat$Cocaine_Use.x = NULL
machine_dat$Meth_Use.x = NULL
machine_dat$StreetOpioids_Use.x = NULL
machine_dat$RxOpioids_Use.x = NULL
machine_dat$Inhalants_Use.x = NULL
machine_dat$Sedatives_Use.x = NULL
machine_dat$Hallucinogens_Use.x = NULL
machine_dat$Other_Use.x = NULL
machine_dat$Stimulants_Use.x = NULL

machine_dat$er_hos_use_base = machine_dat$NightsDetox.x + machine_dat$NightsHospitalMHC.x + machine_dat$TimesER.x
### Drop other variables
machine_dat[,c("NightsDetox.x", "NightsHospitalMHC.x", "TimesER.x")] = list(NULL)


machine_dat$jail_arrest_base = machine_dat$NumTimesArrested.x + machine_dat$NightsJail.x
machine_dat$NightsJail.x = NULL
machine_dat$NumTimesArrested.x = NULL

```



Our next step is to identify if there are any high (i.e., .90 or greater) Spearman correlations between variables.  We used a Spearman correlation, as most variables are generally not normally distributed.  We found no correlation over .9.  

Additionally, we evaluated the variance inflation factors (VIFs) for a logistic regression model with housing as the outcome variable.  This logistic regression model will be similar to the final model used; therefore, the VIFs can provide some insight into potential multicollinearity.  Overall we only found two VIFs above five quarter at 6.12 and telehealth at 6.4, providing evidence that multicollinearity is generally not a concern. 

```{r}
descCor = cor(machine_dat, use = "pairwise.complete.obs", method = "spearman")
hig_corr = findCorrelation(descCor)
hig_corr
library(car)
vif_model = glm(drop_out ~ ., data = machine_dat)
summary(vif_model)
vif_list =  vif(vif_model)
vif_list = data.frame(vif_list) 
vif_list = subset(vif_list, vif_list > 5)
vif_list
```
Quick review of descriptives
```{r}
machine_dat[,c(1:4, 32, 34, 42:45,47,48:51)] = apply(machine_dat[,c(1:4, 32, 34, 42:45, 47, 48:51)], 2, function(x){as.factor(x)})

colnames(machine_dat) = gsub(".x", "", colnames(machine_dat))
part_charac =  prettyR::describe(machine_dat)
num_charac = data.frame(part_charac$Numeric)
num_charac = num_charac[c(1,4),]
num_charac = t(num_charac)
num_charac = round(num_charac,2) 

write.csv(num_charac, "num_charac.csv")

fac_charac = data.frame(part_charac$Factor)
fac_charac = round(fac_charac,2)
fac_charac = t(fac_charac)
write.csv(fac_charac, "fac_charac.csv")

```
Write the csv
```{r}
write.csv(machine_dat, "drop_dat_9_23_20.csv", row.names = FALSE)
```

