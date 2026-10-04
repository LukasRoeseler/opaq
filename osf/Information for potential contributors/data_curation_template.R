#### Data Curation for OpAQ ####

## What you need
# - IMPORTANT NOTE: this template is ideal for wide datasets (i.e., 1 row per participant, multiple columns for different anchoring items). If you have data in a long format (multiple rows per participant) OR ONLY ONE anchoring item, please use the data_curation_template_long.R (https://osf.io/tj67c).
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
ds <- read.csv(file = "name_of_file_in_wide_format.csv") # insert name of dataset here

## prepare data
# set variables
ds$age <- as.numeric(ds$age.variable)
ds$sex <- ifelse(ds$sex.variable == 1, 2, ifelse(ds$sex.variable == 2, 1, 3)) # recode (if necessary), 1 = male, 2 = female, 3 = diverse

# estimates (repeat for all estimation items)
ds$e1.estimate <- ds$estimation.variable.1
ds$e2.estimate <- ds$estimation.variable.2

# anchors (repeat for all estimation items)
ds$e1.anchor <- 600 # all anchor values of first estimation item
ds$e2.anchor <- 14 # all anchor values of second estimation item

# true values (repeat for all estimation items)
ds$e1.true <- 8848 # true value of first estimation item
ds$e2.true <- 54 # true value of second estimation item

## reshape data
ds.long <- stats::reshape(data = ds, direction = "long", sep = "."
                          , varying = list(  c("e1.estimate", "e2.estimate") # please add or remove variable names here
                                             , c("e1.anchor", "e2.anchor") # please add or remove variable names here
                                             , c("e1.true", "e2.true")) # please add or remove variable names here
                          , timevar = "item"
                          , times = c("e1", "e2") # please add or remove variable names here
                          , v.names = c("estimate", "anchor", "true")
                          , idvar = "id"
)

## create remaining variables
ds.long$participant_id <- ds.long$id # this is the original ID used in your study (or simply the rownumber if there was no id)
ds.long$id <- 100000 + ds.long$id # use a higher number than the highest id in the OpAQ data set, 100K should do
ds.long$reference <- "long reference (APA 7 style, preferably with DOI)"
ds.long$reference_short <- "short reference (APA 7 style)" # Example: Testname, 2021 [if unpublished, please use year of data collection]
ds.long$link <- "link to original data"
ds.long$anchoring_item <- as.factor(ds.long$item)

levels(ds.long$anchoring_item) <- c( # e.g., Weight of an elefant (kg) [please put units in parentheses]
  "name of first anchoring item" 
  , "name of second anchoring item"
)

ds.long$tasktype <- as.factor(ds.long$item)
levels(ds.long$tasktype) <- c("height", "quantity") # insert the corresponding task type(s) for all items

ds.long$direction <- 0 # 1 = direction of adjustment was known, 0 = direction was unknown
ds.long$incentive <- 0 # 0 = not incentivized, 1 = incentivized for participation, 2 = incentivized for accurate estimation (can be coupled with incentives for participation); receiving feedback does not count as incentive
ds.long$comparative_question <- 1 # 0 = no, 1 = yes
ds.long$experiment_type <- 1 # 1 = online, 2 = lab, 3 = class, 4 = field
ds.long$stimulitype <- 1 # 1 = short written, 2 = rich written, 3 = image/audio/video, 4 = 2&3
ds.long$sampletype <- 1 # lay
ds.long$scaletype <- 1 # 1 = open, 2 = closed (in both), 3 = visual
ds.long$anchortype <- 2 # 1 = explicitly random, 2 = fixed and provided without explanation, 3 = having some relevance with the target, 4 = self-generated
ds.long$published <- 1 # 0 = no, 1 = preprint, 2 = published in a peer-reviewed journal, 0 = not published
ds.long$preregistered <- "0" # 0 = no, if yes, insert link, "embargoed" = still under embargo (please notify us as soon as the embargo has been lifted)
ds.long$preregtype <- "AsPredicted" # see labels file for abbreviations of templates (https://osf.io/mdgze/)
ds.long$nationality <- "German"


ds.long$true_value <- ds.long$true
ds.long$true <- NULL
ds.long$item <- NULL
ds.long$anchorhigh <- ifelse(ds.long$anchor > ds.long$true_value, 1, 0)

write.csv(ds.long, file = "datasetyear.csv") # Add informative name here and send the data to lukas.roeseler@uni-bamberg