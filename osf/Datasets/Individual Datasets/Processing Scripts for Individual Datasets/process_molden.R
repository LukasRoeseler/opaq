
# Molden ------------------------------------------------------------------

ds.long <- read.csv("Molden_2020_online class exercise.csv")

names(ds.long)
ds.long$low.anchor <- NULL # comparative question response
ds.long$participant_id <- ds.long$participant.id
ds.long$participant.id <- NULL # comparative question response
ds.long$reference_short <- "Molden, 2020"
ds.long$reference <- "Molden, D. C. (2020, October 22). An undergraduate class exercise about anchoring."

# Merge dataset with OpAQ dataset -----------------------------------------

names(ds.long)

cadraw <- read.csv("opaq_42a.csv") # most recent OPAQ dataset version
unique(cadraw$reference_short)


cad <- dplyr::bind_rows(cadraw, ds.long)
names(cad)
cad$X <- NULL


View(cad)

table(cad$reference_short, useNA = "always")

cad$graphbedingung <- NULL

unique(ds.long$reference)
unique(ds.long$reference_short)

write.csv(cad, file = "opaq_43.csv")
