
library(lmerTest)
library(data.table)
library(ggplot2)

## Exploration of the beta dataset

## From Redcap:
#source("BETAStudy_R_2024-12-12_1333.r")
## I have had to make the following change:
# Comment out the "about soups" line because its all NA and the labelling didnt work.

## From Redcap 03 04 2025
source("BETAStudy_R_2025-04-03_1201.r")



# Get the names of the variables labelled 'hba1c'
hba1cvars <- names(data) |> grep(pattern="hba1c", value=TRUE)
glucosevars <- names(data) |> grep(pattern="^glucose_", value=TRUE)
fructovars <- names(data) |> grep(pattern="fructo_", value=TRUE)
insulinvars <- names(data) |> grep(pattern="insulin", value=TRUE)
cpeptidevars <- names(data) |> grep(pattern="cpeptide", value=TRUE)
triglyceridevars <- names(data) |> grep(pattern="trig", value=TRUE)
cholvars <- names(data) |> grep(pattern="chol_", value=TRUE, fixed = TRUE)
hdlvars <- names(data) |> grep(pattern="^hdl", value=TRUE)
ldlvars <- names(data) |> grep(pattern="^ldl", value=TRUE)

## TC and non_hdl are not being imported properly.
tcvars <- names(data) |> grep(pattern="^tc", value=TRUE)
nonhdlvars <- names(data) |> grep(pattern="^non", value=TRUE)

## I can't find the OGTT in the Redcap outputs.
## It isn't being exported, probably entered as text variable.
## HbA1c is the primary outcome anyway.

## Visits are:
# Visit 1 (screen in Redcap) Screen - HbA1c screen is here
# Visit 2 (week 0, visit 1 in Redcap) - baseline for period 1 - HbA1c baseline 1 is here
# Visit 3 (midpoint of period 1, visit 2 in Redcap)
# Visit 4 (8 weeks into period 1, visit 3 in Redcap)
# Visit 5 (end (12 weeks into period 1, visit 4 in Redcap) - HbA1c follow up 1 is here

# Visit 6 (baseline for period 2, visit 5 in redcap, 24 weeks) - HbA1c baseline 2 is here
# Visit 7 (baseline for period 2, visit 6 in redcap, 28 weeks)
# Visit 8 (baseline for period 2, visit 7 in redcap, 32 weeks)
# Visit 9 (baseline for period 2, visit 8 in redcap, 36 weeks) - HbA1c follow up 2 is here

## Get the data that we need 

datasmall <- data[, c("study_id",
                      glucosevars,
                      fructovars,
                      cpeptidevars,
                      triglyceridevars,
                      hdlvars,
                      ldlvars,
                      cholvars,
                      insulinvars,
                      setdiff(hba1cvars,c("hba1c_coll","hba1c_coll.factor")))]
setDT(datasmall)

datasmall |> melt(id.vars = "study_id")

## Link the allocation sequence

# Batch1 (participants 1-8 and the first for participant 9)
# 
# Active:  "1A" "3A" "6A" "7A" "9A" "2B" "4B" "5B" "8B"
# Control:  "2A" "4A" "5A" "8A" "1B" "3B" "6B" "7B" "9B"
# 
# Batch 2 (second and third for participant 9 and then all of participants 10-40)
# 
# Active: "9A"  "11A" "14A" "16A" "17A" "20A" "22A" "23A" "26A" "27A" "29A" "32A" "33A" "36A" "37A" "40A" "10B" "12B" "13B" "15B" "18B" "19B" "21B" "24B" "25B" "28B" "30B" "31B" "34B" "35B" "38B" "39B"
# 
# Control:  "10A" "12A" "13A" "15A" "18A" "19A" "21A" "24A" "25A" "28A" "30A" "31A" "34A" "35A" "38A" "39A" "9B"  "11B" "14B" "16B" "17B" "20B" "22B" "23B" "26B" "27B" "29B" "32B" "33B" "36B" "37B" "40B"


activeFirst = c(1,3,6,7,9,11,14,16,17,20,22,23,26,27,29,32,33,36,37,40)
controlFirst = c(2,4,5,8,10,12,13,15,18,19,21,24,25,28,30,31,34,35,38)



allocs <- data.table(study_id=datasmall$study_id)
allocs[ , study_number := as.numeric(substr(study_id,2,3))]

# From Protocol:
#
# To determine if the dietary intervention affects the primary outcome measure HbA1c, an 
# analysis of covariance (ANCOVA) will be applied, estimating the within-person differences at 
# the end of each treatment period, with the baseline values (measured at the start of each 
# period), the period effect and calendar data included as covariates.
#

## I am interpreting this to mean that the difference between treatment A and B will be compared with 0.
## Or a treatment effect in a mixed model, with the participant ID as the random effect and the relative baselines as covariates?

## Calendar date is date-shifted by Redcap.  That shouldn't make a difference I think so long as we believe the date has a linear effect.


## Make a long format dataset 


datlong <- datasmall |> melt(id.vars = "study_id")
datlong[ , study_number := as.numeric(substr(study_id,2,3)) ]

datlong[ variable %like% "v1" , visit := "v1"]
datlong[ variable %like% "v2" , visit := "v2"]
datlong[ variable %like% "v3" , visit := "v3"]
datlong[ variable %like% "v4" , visit := "v4"]
datlong[ variable %like% "v5" , visit := "v5"]
datlong[ variable %like% "v6" , visit := "v6"]
datlong[ variable %like% "v7" , visit := "v7"]
datlong[ variable %like% "v8" , visit := "v8"]

datlong[ visit %in% c("v1","v2","v3","v4") & study_number %in% controlFirst , treatment := "control"]
datlong[ visit %in% c("v5","v6","v7","v8") & study_number %in% controlFirst , treatment := "active"]

datlong[ visit %in% c("v1","v2","v3","v4") & study_number %in% activeFirst , treatment := "active"]
datlong[ visit %in% c("v5","v6","v7","v8") & study_number %in% activeFirst , treatment := "control"]

datlong[ variable %like% "screening" , treatment := "screening" ]
datlong[ variable %like% "screening" , visit     := "screening" ]


datlong[ visit %in% c("v1", "v5") , time := "pre" ]
datlong[ visit %in% c("v2", "v6") , time := "mid1" ]
datlong[ visit %in% c("v3", "v7") , time := "mid2" ]
datlong[ visit %in% c("v4", "v8") , time := "post" ]
datlong[ visit == "screening" , time := "screening" ]

datlong[ visit %in% c("v1","v2","v3","v4") , order := "A" ]
datlong[ visit %in% c("v5","v6","v7", "v8") , order := "B" ]

datlong[ , outcome := sub("_.*", "", variable) ]



##### HBA1C

##### Model 1, just comparing the outcomes:

## No evidence for an effect
ggplot(datlong[outcome=="hba1c" & time=="pre"] , aes(x=treatment, y=value)) + geom_point() + geom_line(aes(group=study_id))

ggplot(datlong[outcome=="hba1c" & time=="post"] , aes(x=treatment, y=value)) + geom_point() + geom_line(aes(group=study_id))+
  labs(y="Hba1c", x="Treatment") + theme_bw()

ggplot(datlong[outcome=="hba1c" & time=="post" & !(study_id %in% c("E13","E25"))] , aes(x=treatment, y=value)) + geom_point() + geom_line(aes(group=study_id))+
  labs(y="Hba1c", x="Treatment") + theme_bw()


## No evidence for an effect
lmer(data=datlong[outcome=="hba1c" & time=="post"] , value ~ treatment + (1|study_id)) |> summary()
lmer(data=datlong[outcome=="hba1c" & time=="pre"] , value ~ treatment + (1|study_id)) |> summary()

## Without the large outlier
lmer(data=datlong[outcome=="hba1c" & time=="post" & value<50] , value ~ treatment + (1|study_id)) |> summary() 

## If you exclude both possible outliers it gets close.
## Still not really, and I don't think this is justifiable anyway.
lmer(data=datlong[outcome=="hba1c" & time=="post" & !(study_id %in% c("E13","E25")) ] , value ~ treatment + (1|study_id)) |> summary() 

## There's no difference at all pre treatment when you remove the outliers
lmer(data=datlong[outcome=="hba1c" & time=="pre" & !(study_id %in% c("E13","E25")) ] , value ~ treatment + (1|study_id)) |> summary() 

datlong[outcome=="hba1c" &  study_id %in% datlong[value%in%c(35,53),study_id] ][order(study_id)]

##### Model 2, incorporating the baselines and the period:
##### Calendar date is not included, since the period by treatment is confounded with date?
##### Maybe?

baselines = datlong[ outcome=="hba1c" & time=="pre" , .(baseline=value,treatment,study_id)]
datlongb = baselines[datlong[outcome=="hba1c"] , on=c("treatment", "study_id")]

## datlongb only has the hba1c included
lmer(data=datlongb[time=="post"] , value ~ treatment + (1|study_id)) |> summary()
lmer(data=datlongb[time=="post"] , value ~ treatment +order+ (1|study_id)) |> summary()

lmer(data=datlongb[time=="post"] , value ~ treatment +baseline+ (1|study_id)) |> summary()
lmer(data=datlongb[time=="post"] , value ~ treatment +baseline+order+(1|study_id)) |> summary()

### OK, you get a significant result if you adjust for baselines *and* you exclude the potential outliers.

### Only include completers - gets even stronger.
lmer(data=datlongb[time=="post"& !(study_id %in% c("E10", "E11", "E12", "E13","E25"))] , value ~ treatment +baseline+order+ (1|study_id)) |> summary()
lmer(data=datlongb[time=="post"& !(study_id %in% c("E13","E25"))] , value ~ treatment +baseline+order+ (1|study_id)) |> summary()

### This is just the completers, but not the outliers.
lmer(data=datlongb[time=="post"& !(study_id %in% c("E10", "E11", "E12"))] , value ~ treatment +baseline+order+ (1|study_id)) |> summary()

#lmer(data=datlongb[time=="post"& !(study_id %in% c("E10", "E11", "E12"))] , value ~ treatment*order +baseline+ (1|study_id)) |> summary()

## We didn't have a plan for this, did we?
ggplot(datlongb[!is.na(visit)] , aes(x=visit, y=value)) + 
  facet_wrap(~study_id,nrow=4) + geom_point() + geom_line(aes(group=study_id)) + 
  theme(axis.text.x=element_text(angle=45,hjust=1))

datlong[study_id=="E03"]


#### Fasting glucose (excluding or including the outliers?)


datlong_glucose <- datlong[outcome=="glucose"]
datlong_glucose[ , time := factor(time, levels=c("pre","mid1","mid2","post")) ]
  
ggplot(datlong_glucose , aes(x=visit, y=value)) + 
  facet_wrap(~study_id, nrow=4) + geom_point() + 
  geom_line(aes(group=study_id)) + labs(y="Glucose")

ggplot(datlong_glucose[time=="post"] , aes(x=treatment, y=value)) + 
    geom_point() + geom_line(aes(group=study_id)) + theme_bw()+ 
    labs(y="Glucose") 

ggplot(datlong_glucose[time=="pre"] , aes(x=treatment, y=value)) + 
  geom_point() + geom_line(aes(group=study_id)) + theme_bw()

## This is regression to the mean I guess.
lmer( data=datlong_glucose , value ~ time + (1|study_id)) |> summary()

lmer( data=datlong_glucose , value ~ time*treatment + (1|study_id)) |> summary()
lmer( data=datlong_glucose[!(study_id %in% c("E10", "E11", "E12", "E13","E25"))] , 
      value ~ time*treatment + time*order + (1|study_id)) |> summary()

lmer( data=datlong_glucose[time=="post"] , value ~ treatment + order + (1|study_id)) |> summary()

lmer( data=datlong_glucose , 
      value ~ time*treatment + time*order + (1|study_id)) |> summary()



###### Other Metabolites
# Fructosamine
# Fructosamine has strange values in some individuals.
# Very variable
# No evidence for differences

datlong_fructosamine <- datlong[outcome=="fructo"]
datlong_fructosamine[ , time := factor(time, levels=c("pre","mid1","mid2","post")) ]

ggplot(datlong_fructosamine , aes(x=visit, y=value)) + 
  facet_wrap(~study_id, nrow=4) + geom_point() + 
  geom_line(aes(group=study_id)) + labs(y="Fructosamine")

ggplot(datlong_fructosamine[time=="post"] , aes(x=treatment, y=value)) + 
  geom_point() + geom_line(aes(group=study_id)) + theme_bw()+ 
  labs(y="Fructosamine") 

lmer( data=datlong_fructosamine , value ~ time*treatment + (1|study_id)) |> summary()
lmer( data=datlong_fructosamine , value ~ time*treatment + (1|study_id)) |> anova()
lmer( data=datlong_fructosamine[time=="post"] , value ~ treatment + order + (1|study_id)) |> summary()
lmer( data=datlong_fructosamine[time=="post"& !(study_id %in% c("E10", "E11", "E12", "E13","E25"))] , value ~ treatment +order+ (1|study_id)) |> summary()


# Insulin

### 25 has extremely high insulin compared to everybody else

datlong_insulin <- datlong[outcome=="insulin"]
datlong_insulin[ , time := factor(time, levels=c("pre","mid1","mid2","post")) ]

ggplot(datlong_insulin, aes(x=visit, y=value)) + 
  facet_wrap(~study_id, nrow=4) + geom_point() + 
  geom_line(aes(group=study_id)) + labs(y="Insulin")

ggplot(datlong_insulin[time=="post"] , aes(x=treatment, y=value)) + 
  geom_point() + geom_line(aes(group=study_id)) + theme_bw()+ 
  labs(y="Insulin")  + scale_y_log10()

ggplot(datlong_insulin[time=="post"] , aes(x=treatment, y=value)) + 
  geom_point() + geom_line(aes(group=study_id)) + theme_bw()+ 
  labs(y="Insulin") 

lmer( data=datlong_insulin , value ~ time*treatment + (1|study_id)) |> summary()
lmer( data=datlong_insulin , value ~ time*treatment + (1|study_id)) |> anova()
## If anything, insulin is slightly higher in the controls
lmer( data=datlong_insulin[time=="post"] , value ~ treatment + order + (1|study_id)) |> summary()
lmer( data=datlong_insulin[time=="mid2"] , value ~ treatment + order + (1|study_id)) |> summary()
lmer( data=datlong_insulin[time=="pre"] , value ~ treatment + order + (1|study_id)) |> summary()
lmer( data=datlong_insulin[time=="mid1"] , value ~ treatment + order + (1|study_id)) |> summary()
## Similar here, insulin higher in controls if anything
lmer( data=datlong_insulin[time=="post"& !(study_id %in% c("E10", "E11", "E12", "E13","E25"))] , value ~ treatment +order+ (1|study_id)) |> summary()



# C-peptide

### 25 has extremely high insulin compared to everybody else

datlong_cpeptide <- datlong[outcome=="cpeptide"]
datlong_cpeptide[, time := factor(time, levels=c("pre","mid1","mid2","post")) ]

ggplot(datlong_cpeptide, aes(x=visit, y=value)) + 
  facet_wrap(~study_id, nrow=4) + geom_point() + 
  geom_line(aes(group=study_id)) + labs(y="Insulin")

ggplot(datlong_insulin[time=="post"] , aes(x=treatment, y=value)) + 
  geom_point() + geom_line(aes(group=study_id)) + theme_bw()+ 
  labs(y="Insulin")  + scale_y_log10()

ggplot(datlong_insulin[time=="post"] , aes(x=treatment, y=value)) + 
  geom_point() + geom_line(aes(group=study_id)) + theme_bw()+ 
  labs(y="Insulin") 

lmer( data=datlong_insulin , value ~ time*treatment + (1|study_id)) |> summary()
lmer( data=datlong_insulin , value ~ time*treatment + (1|study_id)) |> anova()
## If anything, insulin is slightly higher in the controls
lmer( data=datlong_insulin[time=="post"] , value ~ treatment + order + (1|study_id)) |> summary()
lmer( data=datlong_insulin[time=="mid2"] , value ~ treatment + order + (1|study_id)) |> summary()
lmer( data=datlong_insulin[time=="pre"] , value ~ treatment + order + (1|study_id)) |> summary()
lmer( data=datlong_insulin[time=="mid1"] , value ~ treatment + order + (1|study_id)) |> summary()
## Similar here, insulin higher in controls if anything
lmer( data=datlong_insulin[time=="post"& !(study_id %in% c("E10", "E11", "E12", "E13","E25"))] , value ~ treatment +order+ (1|study_id)) |> summary()


###########################################
### Compare all of the outcomes at once ###
###########################################

ggplot(datlong[time=="post"] , aes(x=treatment, y=value)) + geom_point() + geom_line(aes(group=study_id)) + 
  facet_wrap(~outcome, scale="free_y")

### Do a pairs plot and generate some derived outcomes:
datwide <- datlong[time=="post" , .(study_id, value, visit, treatment, order, outcome)] |> 
  dcast( study_id + order+treatment+visit ~ outcome, value.var="value"  )

GGally::ggpairs( datwide[ , .(fructo, glucose, hba1c, hdl, insulin, ldl, trig,chol)])

datwide[ , tc := chol / hdl]


## Everything slightly favours control
lmer( data=datwide , chol ~ treatment + order + (1|study_id)) |> summary()
lmer( data=datwide , hdl ~ treatment + order + (1|study_id)) |> summary()
lmer( data=datwide , ldl ~ treatment + order + (1|study_id)) |> summary()
lmer( data=datwide , trig ~ treatment + order + (1|study_id)) |> summary()
lmer( data=datwide , tc ~ treatment + order + (1|study_id)) |> summary()


## Where is glucosamine?

names(data) |> grep("gluco", x=_, value=TRUE)






###### OGTT

names(data) |> grep("min", x=_, value=TRUE)
