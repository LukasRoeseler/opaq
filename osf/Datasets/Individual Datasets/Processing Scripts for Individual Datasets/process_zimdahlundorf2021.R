
# Zimdahl & Undorf, 2021, S3 ----------------------------------------------

ds.long <- read.csv("Zimdahl & Undorf, 2021, Study 3.csv", sep = ";")
View(ds.long)

# Check data


# Add missing variables
ds.long$reference <- "Zimdahl, M. F., & Undorf, M. (2021). Hindsight bias in metamemory: Outcome knowledge influences the recollection of judgments of learning. Memory, 29(5), 559–572. https://doi.org/10.1080/09658211.2021.1919144"
ds.long$published <- 1
ds.long$preregistered <- "https://osf.io/mrkzq"
ds.long$id <- ds.long$id + 100000
ds.long$estimate <- as.numeric(gsub(",", ".", gsub("\\.", "", ds.long$estimate)))



# Merge dataset with OpAQ dataset -----------------------------------------


cadraw <- read.csv("opaq_50a.csv") # most recent OPAQ dataset version
unique(cadraw$reference_short)


cad <- dplyr::bind_rows(cadraw, ds.long)
names(cad)
cad$X <- NULL


View(cad)

table(cad$reference_short, useNA = "always")




write.csv(cad, file = "opaq_51.csv")