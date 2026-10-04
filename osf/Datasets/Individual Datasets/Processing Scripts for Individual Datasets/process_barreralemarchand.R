
# Frech et al. Day-Night data ---------------------------------------------



# Open dataset
ds.long <- read.csv("barrera_experiment1-2018dataset.csv")
# View(ds.long)

ds.long$reference <- "Barrera, F. & Navajas, J. (2018). Unpublished data."
ds.long$reference_short <- "Barrera & Navajas, 2018, Study 1"
ds.long$preregistered <- "0"
ds.long$X <- NULL

unique(ds.long$anchoring_item)


write.csv(ds.long, file = "barreralemarchand2018s1.csv") # Add informative name here and send the data to lukas.roeseler@uni-bamberg

# Open dataset
ds2.long <- read.csv("barrera_experiment2-2018dataset.csv")
# View(ds.long)

ds2.long$reference <- "Barrera, F. & Navajas, J. (2018). Unpublished data."
ds2.long$reference_short <- "Barrera & Navajas, 2018, Study 2"
ds2.long$preregistered <- "1"
ds2.long$X <- NULL

unique(ds2.long$anchoring_item)

write.csv(ds.long, file = "barreralemarchand2018s2.csv") # Add informative name here and send the data to lukas.roeseler@uni-bamberg





# Merge dataset with OpAQ dataset -----------------------------------------


cadraw <- read.csv("opaq_53a.csv") # most recent OPAQ dataset version
unique(cadraw$reference_short)

cadraw <- cadraw[cadraw$reference_short != "Barrera Lemarchand, 2018", ]

cad <- dplyr::bind_rows(cadraw, ds.long, ds2.long)
names(cad)
cad$X <- NULL


View(cad)

table(cad$reference_short, useNA = "always")




write.csv(cad, file = "opaq_54.csv")
