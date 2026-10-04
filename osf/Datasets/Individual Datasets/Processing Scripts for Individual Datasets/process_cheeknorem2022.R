library(stats)
library(dplyr)



# Cheek & Norem, 2022 -----------------------------------------------------


## open dataset
ds <- read.csv(file = "https://osf.io/xr2qf/download")

##### exclude participants -> M?ssen die raus, bei denen NA steht?

## prepare data
# set variables
ds$age <- as.numeric(ds$Age)
ds$Gender <- ifelse(ds$Gender > 2, NA, ds$Gender)
ds$sex <- ifelse(ds$Gender == 1, 2, 1) # recode (if necessary), 1 = male, 2 = female, 3 = diverse, else = NA 

# estimates (repeat for all estimation items)
ds$e1.estimate <- ds$UN_Estimate
ds$e2.estimate <- ds$Babies_Estimate
ds$e3.estimate <- ds$Cat_Estimate
ds$e4.estimate <- ds$Gas_Estimate
ds$e5.estimate <- ds$Ant_Estimate
ds$e6.estimate <- ds$Tele_Estimate

# anchors (repeat for all estimation items)
ds$e1.anchor <- ifelse(ds$UN_Condition     == 1, 18, 193) 
ds$e2.anchor <- ifelse(ds$Babies_Condition == 1, 300, 67500) 
ds$e3.anchor <- ifelse(ds$Cat_Condition    == 1, 8, 30) 
ds$e4.anchor <- ifelse(ds$Gas_Condition    == 1, 28, 200) 
ds$e5.anchor <- ifelse(ds$Ant_Condition    == 1, -50, 10) 
ds$e6.anchor <- ifelse(ds$Tele_Condition   == 1, 1830, 1915) 

# true values (repeat for all estimation items)
ds$e1.true <- 193
ds$e2.true <- 10557 # source: https://www.cdc.gov/nchs/fastats/births.htm
ds$e3.true <- 30 # source: https://kittyclysm.com/how-fast-can-cats-run/ ## true value = anchor
ds$e4.true <- 374 # source: https://www.eia.gov/tools/faqs/faq.php?id=23&t=10, https://de.statista.com/statistik/daten/studie/19320/umfrage/gesamtbevoelkerung-der-usa/.
ds$e5.true <- -68.8 #-56°C # source: https://niwa.co.nz/education-and-training/schools/resources/climate/antarctic.
ds$e6.true <- 1861

## reshape data
ds.long <- stats::reshape(data = ds, direction = "long", sep = "."
                          , varying = list(  c("e1.estimate", "e2.estimate", "e3.estimate", "e4.estimate", "e5.estimate", "e6.estimate")
                                             , c("e1.anchor", "e2.anchor", "e3.anchor", "e4.anchor", "e5.anchor", "e6.anchor")
                                             , c("e1.true", "e2.true", "e3.true", "e4.true", "e5.true", "e6.true"))
                          , timevar = "item"
                          , times = c("e1", "e2", "e3", "e4", "e5", "e6")
                          , v.names = c("estimate", "anchor", "true")
                          , idvar = "id"
)

## create remaining variables
ds.long$participant_id <- ds.long$id
ds.long$id <- 35000 + ds.long$id 
ds.long$reference <- "Cheek, N. N., & Norem, J. K. (2022). Individual differences in anchoring susceptibility: Verbal reasoning, autistic tendencies, and narcissism. Personality and Individual Differences, 184, 111212."
ds.long$reference_short <- "Cheek & Norem, 2022"
ds.long$link <- "https://osf.io/xr2qf/"
ds.long$anchoring_item <- as.factor(ds.long$item)

levels(ds.long$anchoring_item) <- c(
  "Number of United Nations (UN) member states"
  , "Average number of babies born per day in the United States"
  , "Maximum speed of a housecat (mph)"
  , "Amount of gas used per month by an American (gallons)"
  , "Average winter temperature of Antarctica (F)"
  , "Year the telephone was invented"
)

ds.long$tasktype <- as.factor(ds.long$item)
levels(ds.long$tasktype) <- c("quantity", "birth rate", "speed", "quantity", "temperature", "date") # insert the corresponding task type(s) for all items

ds.long$direction <- 0 # 1 = direction of adjustment was known, 0 = direction was unknown
ds.long$incentive <- 1 # 0 = not incentivized, 1 = incentivized for participation (e.g., MTurkers), 2 = incentivized for accurate estimation (can be coupled with incentives for participation); receiving feedback does not count as incentive
ds.long$comparative_question <- 1 # 0 = no, 1 = yes
ds.long$experiment_type <- 1 # 1 = online, 2 = lab, 3 = class, 4 = field
ds.long$stimulitype <- 1 # 1 = short written, 2 = rich written, 3 = image/audio/video, 4 = 2&3
ds.long$sampletype <- 1 # 1 = lay, 2 = professional or expert, 3 = mixed
ds.long$scaletype <- 1 # 1 = open, 2 = closed (in both), 3 = visual
ds.long$anchortype <- 2 # 1 = explicitly random, 2 = fixed and provided without explanation, 3 = having some relevance with the target, 4 = self-generated

ds.long$true_value <- ds.long$true
ds.long$true <- NULL
ds.long$item <- NULL
ds.long$anchorhigh <- ifelse(ds.long$anchor > ds.long$true_value, 1, 0)
ds.long$published <- 1

write.csv(ds.long, file = "dataset.cheeknorem2022.csv") # Add informative name here and send the data to lukas.roeseler@uni-bamberg




# Merge dataset with OpAQ dataset -----------------------------------------

names(ds.long)
ds.long <- ds.long[, 241:262]

cadraw <- read.csv("opaq_21a.csv") # most recent OPAQ dataset version
unique(cadraw$reference_short)


cad <- dplyr::bind_rows(cadraw, ds.long)
head(cad)
cad[, 1:2] <- NULL


View(cad)

table(cad$reference_short, useNA = "always")




write.csv(cad, file = "opaq_22.csv")

