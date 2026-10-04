

# Röseler, L. Generalizability in psychological and economic resea --------

# Röseler, L. Generalizability in psychological and economic research: A theoretical and empirical investigation. Advance online publication. https://doi.org/10.13140/RG.2.1.5094.4889

library(haven)
library(dplyr)

## Read data
ba <- haven::read_sav("https://osf.io/ducg5/download")

## Process data 

# constant variables
bac <- data.frame(id = ba$COUNTER)
bac$anchor <- ifelse(ba$Ankerhoch == 1, 90, 10)
bac$sex <- ifelse(ba$V31_S1 == 1, 2, 1) # ba: 1 = female, 2 = male; cad: 1 = male, 2 = female
bac$age <- ba$V32


# estimates
bac$number25a <- ba$Z11
bac$number25b <- ba$Z12
bac$number75a <- ba$Z21
bac$number75b <- ba$Z22
bac$percentage25a <- ba$P11
bac$percentage25b <- ba$P12
bac$percentage75a <- ba$P21
bac$percentage75b <- ba$P22


## Reshape data
bacwide <- reshape::melt(bac, id.vars = c("id", "sex", "age", "anchor"), measure.vars = c("number25a", "number25b", "number75a", "number75b", "percentage25a", "percentage25b", "percentage75a", "percentage75b")) # XXX check anchor
bacwide$participant_id <- bac$id
bacwide$id <- 10000+bacwide$participant_id
bacwide$X <- NA
bacwide$participant_id <- 10000 + bacwide$id
bacwide$reference <- "Röseler, L. Generalizability in psychological and economic research: A theoretical and empirical investigation. Advance online publication. https://doi.org/10.13140/RG.2.1.5094.4889"
bacwide$link <- "https://osf.io/ducg5/"
bacwide$anchorhigh <- ifelse(bacwide$anchor == 90, 1, 0)
bacwide$anchoring_item <- bacwide$variable
bacwide$true_value <- as.numeric(gsub("[^\\d]+", "", bacwide$anchoring_item, perl = TRUE))
bacwide$estimate <- bacwide$value

bacwide$tasktype <- "quantity" # quantity estimation
bacwide$direction <- 0 # uncertain
bacwide$incentive <- 0 # none
bacwide$comparative_question <- 1 # yes
bacwide$experiment_type <- 1 # online
bacwide$stimulitype <- 3 # picture 
bacwide$sampletype <- 1 # lay
bacwide$scaletype <- ifelse(substr(bacwide$anchoring_item, 1, 1) == "p", 2, 1) # open (number) closed (percentage) XXX
bacwide$anchortype <- 2 # provided without explanation




cadraw <- read.csv("ankerseminar_irrelevantanchors1.csv", sep = ";", dec = ",")

cad <- dplyr::bind_rows(cadraw, bacwide)
cad$variable <- NULL
cad$value <- NULL
write.csv(cad, file = "OAO_03.csv")



