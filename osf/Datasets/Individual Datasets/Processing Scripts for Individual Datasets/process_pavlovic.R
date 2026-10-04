
# Zoran Pavlovic Data -----------------------------------------------------


# Open dataset
ds.long <- read.csv("pavlovic_Anchoring_data.csv")

# check dataset

# correct scaletype for % question
ds.long$scaletype
ds.long[ds.long$anchoring_item == "The % of African nations members of the UN", "scaletype"] <- 2
ds.long[ds.long$anchoring_item == "The % of African nations members of the UN", "estimate"] # closed
ds.long$scaletype

# correct item name
table(ds.long$anchoring_item) # looks like ws (85x3 items)
ds.long[ds.long$anchoring_item == "Height of the tallest sequioa tree (in 8meters)", "anchoring_item"] <- "Height of the tallest sequioa tree (m)"
ds.long[ds.long$anchoring_item == "Height of the tallest sequioa tree (in meters)", "anchoring_item"] <- "Height of the tallest sequioa tree (m)"
ds.long[ds.long$anchoring_item == "The % of African nations members of the UN", "anchoring_item"] <- "Percentage of African nations in the UN"


table(ds.long$anchoring_item) 
table(ds.long$id)

ds.long$reference <- "Pavlović, Z. (2021, December 7). Anchoring data. Retrieved from osf.io/sze8x"
ds.long$reference_short <- "Pavlovic, 2021"
ds.long$link <- "https://osf.io/ek6c7/"
ds.long$published <- 0
ds.long$preregistered <- 0

ds.long$participant_id <- ds.long$id
ds.long$id <- ds.long$id + 75000

# Merge dataset with OpAQ dataset -----------------------------------------

names(ds.long)

cadraw <- read.csv("opaq_43a.csv") # most recent OPAQ dataset version
unique(cadraw$reference_short)


cad <- dplyr::bind_rows(cadraw, ds.long)
names(cad)
cad$X <- NULL


View(cad)

table(cad$reference_short, useNA = "always")




write.csv(cad, file = "opaq_44.csv")