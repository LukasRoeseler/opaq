#### Data Curation for OpAQ ####

## What you need
# - IMPORTANT NOTE: this template is ideal for long datasets (i.e., multiple rows per participant, multiple anchoring items) OR datasets with only one anchoring item. If you have data in a wide format (multiple columns with anchoring estimate), please use the data_curation_template.R (https://osf.io/mpwfy).
# - dataset from one study (it does not need to have been published)
# - there must be at least one question for which a high and a low anchor were presented
# - some analyses require there to be some sort of "correct answer" for the anchoring question. This can be the true value (e.g., height of Mt. Everest is 8848 m) or an unanchored mean estimate from the same or another sample.

## How to use the script
# 1. go through it line by line and make the code match your dataset
# 2. run the script and check if conversion has worked
# 3. send the processed data to lukas.roeseler@uni-bamberg.de. He will add it to the latest version of the OpAQ dataset
# 4. please indicate in the e-mail whether you want to be listed as a co-author for Data curation and Resources.
# If you have any questions, please send us an e-mail and we try to help you!



## open dataset
ds <- read.csv(file = "name_of_file_in_long_format.csv") # insert name of dataset here

## prepare data
# set variables
ds$age <- as.numeric(ds$age.variable)
ds$sex <- ifelse(ds$sex.variable == 1, 2, ifelse(ds$sex.variable == 2, 1, 3)) # recode (if necessary), 1 = male, 2 = female, 3 = diverse

ds.long <- data.frame("age" = rep(NA, nrow(ds)))
ds.long$sex <- ds$sex

# estimate
ds.long$estimate <- ds$estimation.variable

# anchor
ds.long$anchor <- 600 # anchor values (for multiple values, you can use dplyr::recode(variable
						#		, "old value" = "new value"
						#		, "old value2" = "new value2" )

# true value
ds.long$true_value <- 8848 # true values


## create remaining variables
ds.long$participant_id <- ds.long$id # this is the original ID used in your study (or simply the rownumber if there was no id)
ds.long$id <- 400000 + ds.long$id # use a higher number than the highest id in the OpAQ data set, 400K should do
ds.long$reference <- "long reference (APA 7 style, preferably with DOI)"
ds.long$reference_short <- "short reference (APA 7 style)" # Example: Testname, 2021 [if unpublished, please use year of data collection]
ds.long$link <- "link to original data"
ds.long$anchoring_item <- as.factor(ds.long$item)

ds.long$anchoring_item <- "Short but informative name" # e.g., Weight of an elefant (kg) [please put units in parentheses]

ds.long$tasktype <- "weight" # insert the corresponding task type(s) for all items

ds.long$direction <- 0 # 1 = direction of adjustment was known, 0 = direction was unknown
ds.long$incentive <- 0 # 0 = not incentivized, 1 = incentivized for participation, 2 = incentivized for accurate estimation (can be coupled with incentives for participation); receiving feedback does not count as incentive
ds.long$comparative_question <- 1 # 0 = no, 1 = yes
ds.long$experiment_type <- 1 # 1 = online, 2 = lab, 3 = class, 4 = field, 5 = mixed
ds.long$stimulitype <- 1 # 1 = short written, 2 = rich written, 3 = image/audio/video, 4 = 2&3, 9 = other
ds.long$sampletype <- 1 # lay, 2 = professional/expert, 3 = mixed
ds.long$scaletype <- 1 # 1 = open, 2 = closed (in both), 3 = visual
ds.long$anchortype <- 2 # 1 = explicitly random, 2 = fixed and provided without explanation, 3 = having some relevance with the target, 4 = self-generated, 5 = subliminal or incidental
ds.long$published <- 1 # 0 = no, 1 = preprint, 2 = published in a peer-reviewed journal, 0 = not published
ds.long$preregistered <- "0" # 0 = no, if yes, please paste link here, "embargoed" if preregistered but not yet public (please notify us after embargo has been lifted)
ds.long$preregtype <- "AsPredicted" # see labels file for abbreviations of templates (https://osf.io/mdgze/)
ds.long$nationality <- "German"

ds.long$item <- NULL
ds.long$anchorhigh <- ifelse(ds.long$anchor > ds.long$true_value, 1, 0) # this only works if there is a true value

write.csv(ds.long, file = "datasetyear.csv") # Add informative name here and send the data to lukas.roeseler@uni-bamberg