
# Bickenbach et al. 2021 --------------------------------------------------

# Open dataset
ds.long <- read.csv("bickenbachetal2021.csv")
ds.long$X <- NULL
ds.long$published <- 0
ds.long <- ds.long[!is.na(ds.long$estimate), ]
ds.long <- ds.long[ds.long$anchoring_item != "Height of Mount Everest", ]
# Merge dataset with OpAQ dataset -----------------------------------------


cadraw <- read.csv("opaq_52a.csv") # most recent OPAQ dataset version
unique(cadraw$reference_short)

cadraw <- cadraw[cadraw$reference_short != "Bickenbach et al., 2021", ]

cad <- dplyr::bind_rows(cadraw, ds.long)
names(cad)
cad$X <- NULL


View(cad)

table(cad$reference_short, useNA = "always")




write.csv(cad, file = "opaq_53.csv")
