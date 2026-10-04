library(stats)
library(dplyr)

## open dataset
ds1 <- read.table("https://osf.io/s94px/download", sep = "\t", head = T, as.is = T)
ds2 <- read.table("https://osf.io/b7p2y/download", sep = "\t", head = T, as.is = T)
# NOTE: demographics not available from the osf
ds <- cbind(ds1, ds2)
mean(ds1$id == ds2$id)
ds[, 5:6] <- NULL

##### exclude participants who failed criteria
ds <- ds[ds$absolute < 3000,]
ds$estimate <- ds$absolute

## prepare data
# set variables
ds$age <- NA
ds$sex <- NA # recode (if necessary), 1 = male, 2 = female, 3 = diverse, else = NA
### NOTE: data is in long format already, reshaping is not necessary
ds.long <- ds


## create remaining variables
ds.long$participant_id <- as.numeric(as.factor(ds.long$id))
ds.long$id <- 50000 + ds.long$participant_id # use a higher number than the highest id in the OpAQ data set 
ds.long$reference <- "Bahník, Š. (2020, June 26). Anchoring without scale distortion. https://doi.org/10.31234/osf.io/2q8hj"
ds.long$reference_short <- "Bahník, 2020"
ds.long$link <- "https://osf.io/3t2hw/" # link to project bc there are 2 datasets that have to be merged
ds.long$anchoring_item <- as.factor(ds.long$item)
ds.long$true_value <- as.factor(ds.long$item)

levels(ds.long$anchoring_item) <- c( 
  "length of the Nusle bridge (metres)"
  , "height of the Eiffel Tower (metres)"
  , "length of Petřín funicular (metres)"
  , "height of the highest (Cheops) Pyramid in Giza (metres)"
  , "length of the longest ship (metres)"
  , "height of the tallest skyscraper Burj Khalifa (metres)"
  , "length of a typical football field (metres)"
  , "length of the Great Strahov Stadion (metres)"
  , "distance of the subway tracks between stations Muzeum and Hlavní nádraží (metres)"
  , "height of the highest tree in the world (metres)"
  , "length of the Wenceslas Square (metres)"
  , "height of the tallest bridge viaduct Millau (metres)"
  , "height of the tallest waterfall Salto Angel (metres)"
)

levels(ds.long$true_value) <- c( 
  485
  , 324
  , 510
  , 139
  , 458
  , 828
  , 105
  , 310
  , 425
  , 116
  , 750
  , 341
  , 979
)

ds.long$true_value <- as.numeric(as.character(ds.long$true_value))

ds.long$tasktype <- as.factor(ds.long$item)
levels(ds.long$tasktype) <- c(
  "length"
  , "height"
  , "length"
  , "height"
  , "length"
  , "height"
  , "length"
  , "length"
  , "distance"
  , "height"
  , "length"
  , "height"
  , "height"
  )


ds.long$direction <- 0 # 1 = direction of adjustment was known, 0 = direction was unknown
ds.long$incentive <- 1 # 1 = not incentivized, 1 = incentivized for participation, 2 = incentivized for accurate estimation (can be coupled with incentives for participation); receiving feedback does not count as incentive XXX Woher wissen wir das hier?
ds.long$comparative_question <- 1 # 0 = no, 1 = yes
ds.long$experiment_type <- 1 # 1 = online, 2 = lab, 3 = class, 4 = field
ds.long$stimulitype <- 1 # 1 = short written, 2 = rich written, 3 = image/audio/video, 4 = 2&3
ds.long$sampletype <- 1 # 1 = lay, 2 = professional or expert, 3 = mixed
ds.long$scaletype <- 1 # 1 = open, 2 = closed (in both), 3 = visual
ds.long$anchortype <- 1 # 1 = explicitly random, 2 = fixed and provided without explanation, 3 = having some relevance with the target, 4 = self-generated

# ds.long$true_value <- ds.long$true 
# ds.long$true <- NULL
ds.long$item <- NULL
ds.long$anchorhigh <- ifelse(ds.long$anchor >= ds.long$true_value, 1, 0) # high anchor corresponds to true values for some items (housecat, members of the UN)
ds.long$published <- 1
names(ds.long)
ds.long <- ds.long[, c(1, 4, 7:26)]

write.csv(ds.long, file = "dataset.bahnik2020.csv") # Add informative name here and send the data to lukas.roeseler@uni-bamberg

# check if anchor is continuous or binary
length(unique(ds.long$anchor)) # continuous

# Merge datasets ----------------------------------------------------------

# Datensatz an OpAQ anhängen
cadraw <- read.csv("opaq_23a.csv") # most recent OPAQ dataset version
unique(cadraw$reference_short)


cad <- dplyr::bind_rows(cadraw, ds.long)
head(cad)
cad[, 1] <- NULL


View(cad)

table(cad$reference_short, useNA = "always")

write.csv(cad, file = "opaq_24.csv")
