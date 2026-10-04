##############################
# R Code for the OpAQ dataset publication
# Written by Lukas Röseler
# email: lukas.roeseler@uni-bamberg.de
# please email me directly if you see any errors or have any questions
# last update: 2022-08-10
# outline: press Ctrl+Shift+O in RStudio to open document outline
##############################



# OpAQ Dataset Publication ------------------------------------------------

### load packages [and install if necessary]
install.packages("psych")
library(psych)

# OPEN CURRENT OPAQ DATASET

# let R directly download the dataset from the OSF
ds <- read.csv("https://osf.io/67r5h/download/") # refresh to newest version, make sure it includes /download/ at the end

# # or open from working directory after having manually downloaded the dataset [comment the next line in]
# ds <- read.csv("opaq_67a.csv") # refresh to newest version

ds <- ds[!is.na(ds$estimate), ]
ds <- ds[!is.na(ds$anchoring_item), ]
write.csv(ds, "OPAQ_JOPD.csv")

## 2.3 Location of data collection
unique(ds$nationality)

## 2.4 sampling, sample, and data collection

# k studies
length(unique(ds$reference_short))

# references
length(unique(ds$reference))

# total sample size
sum(aggregate(participant_id ~ reference_short, data = ds, FUN = function(x) length(unique(x)))$participant_id)

# anchoring items
length(unique(ds$anchoring_item))

# total number of trials
nrow(ds)

# gender
cad_gender <- aggregate(sex ~ participant_id + reference_short, data = ds, FUN = "mean")
table(cad_gender$sex, useNA = "always")
sum(aggregate(participant_id ~ reference_short, data = ds, FUN = function(x) length(unique(x)))$participant_id) - sum(table(cad_gender$sex, useNA = "always"))

# age
cad_age <- aggregate(age ~ participant_id + reference_short, data = ds, FUN = "mean")
psych::describe(cad_age$age)
# sum(aggregate(participant_id ~ reference_short, data = ds, FUN = function(x) length(unique(x)))$participant_id) - psych::describe(cad_age$age)$n

# incentive
cad_inc <- aggregate(incentive ~ participant_id + reference_short, data = ds, FUN = "mean")
table(cad_inc$incentive, useNA = "always")


## 2.5 materials/ survey instruments

# number of anchoring items
length(unique(ds$anchoring_item))

# number of items with true values
ds_true <- ds[!is.na(ds$true_value), ]
length(unique(ds_true$anchoring_item))
length(unique(ds_true$anchoring_item))/length(unique(ds$anchoring_item))

# links to the original dataset
ds_link <- ds[!is.na(ds$link),]
length(unique(ds_link$reference_short))
length(unique(ds_link$reference_short)) / length(unique(ds$reference_short))
