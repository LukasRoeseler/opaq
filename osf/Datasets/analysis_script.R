

# OpAQ Dataset ------------------------------------------------------------


# TO DO
# mean-estimates: korrelation mit sta-scores so machen, dass mean-estimates vorher z-scored wird
# EG rep References aktualisieren (kurz und lang), wenn preprint raus ist


## Load packages
# install.packages("reshape", dependencies = TRUE)
# install.packages("sjstats", dependencies = TRUE)
library(reshape)
library(dplyr)
library(lme4)
library(sjstats)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(GGally)
library(splithalf)

## Open Dataset

cad <- read.csv("opaq_65.csv", sep = ",", dec = ".")
names(cad)
cad$X <- NULL

# if necessary, convert Umlaute (ä, ö, ü, ß)
# cad$reference <- iconv(cad[, "reference"], "UTF-8", "WINDOWS-1252")
# cad$reference_short <- iconv(cad[, "reference_short"], "UTF-8", "WINDOWS-1252")

# Check if conversion has lead to NAs
table(cad$reference_short, useNA = "always")
table(cad$reference, useNA = "always")

# cad$reference_short <- ifelse(is.na(cad$reference_short), "Röseler & Röseler, 2017", cad$reference_short)
# cad$reference <- ifelse(is.na(cad$reference), "Röseler, L., & Röseler, J. J. (2017). Anchoring and Construal Level. [Unpublished data]. Retrieved from osf.io/kd4cf"
# , cad$reference)
# 
# cad$reference <- dplyr::recode(cad$reference_short
#               ,"Papenmeier et al., 2021a" = "Papenmeier, F., Intelmann, P., & Wolf, H. (2021). [Unpublished raw data collected within the course Experimentalpsychologisches Praktikum 1 (group 1)]. University of Tübingen."    
#               , "Papenmeier, 2021" = "Papenmeier, F. (2021). [Unpublished raw data collected within the course Experimentalpsychologisches Praktikum 1 (group 2)]. University of Tübingen."                               
#               , "Papenmeier et al., 2021b"   = "Papenmeier, F., Panse, F., & Schreiner, N. B. (2021). [Unpublished raw data collected within the course Experimentalpsychologisches Praktikum 1 (group 9)]. University of Tübingen."
#               , "Papenmeier & Rebholz, 2021" = "Papenmeier, F., & Rebholz, T. R. (2021). [Unpublished raw data collected within the course Experimentalpsychologisches Praktikum 2]. University of Tübingen."
#               , .default = cad$reference
# )



# Corrections -------------------------------------------------------------

# lower case letters for task types
cad$tasktype <- tolower(cad$tasktype)


## WTP/WTA items by Ioannidis et al., 2020 and Bergman et al., 2010: Personal monetary valuation of [product] (UNIT)
unique(cad[cad$reference_short == "Ioannidis et al., 2020", "anchoring_item"])
cad[cad$reference_short == "Ioannidis et al., 2020" & cad$anchoring_item == "Price of one bottle of wine (Arrogant Frog Cabernet Sauvignon Merlot) (EUR)", "anchoring_item"] <- "Personal monetary valuation of one bottle of wine (Arrogant Frog Cabernet Sauvignon Merlot) (EUR)"
cad[cad$reference_short == "Ioannidis et al., 2020" & cad$anchoring_item == "Price of one bottle of wine (Arrogant Frog Chardonnay Viognier) (EUR)", "anchoring_item"] <- "Personal monetary valuation of one bottle of wine (Arrogant Frog Chardonnay Viognier) (EUR)"      
cad[cad$reference_short == "Ioannidis et al., 2020" & cad$anchoring_item == "Price of one bottle of wine (Adriatico Verdicchio) (EUR)", "anchoring_item"] <- "Personal monetary valuation of one bottle of wine (Adriatico Verdicchio) (EUR)"                   
cad[cad$reference_short == "Ioannidis et al., 2020" & cad$anchoring_item == "Price of one bottle of wine (Piccini Chianti) (EUR)", "anchoring_item"] <- "Personal monetary valuation of one bottle of wine (Piccini Chianti) (EUR)" 
  
unique(cad[cad$reference_short == "Bergman et al., 2010", "anchoring_item"])
cad[cad$reference_short == "Bergman et al., 2010" & cad$anchoring_item == "Price of a quality wine (SEK)", "anchoring_item"] <- "Personal monetary valuation of a quality wine (SEK)"                      
cad[cad$reference_short == "Bergman et al., 2010" & cad$anchoring_item == "Price of an average wine (SEK)", "anchoring_item"] <- "Personal monetary valuation of an average wine (SEK)"                    
cad[cad$reference_short == "Bergman et al., 2010" & cad$anchoring_item == "Price of handmade chocolate truffles (SEK)", "anchoring_item"] <- "Personal monetary valuation of handmade chocolate truffles (SEK)"         
cad[cad$reference_short == "Bergman et al., 2010" & cad$anchoring_item == "Price of Belgian chocolates (SEK)", "anchoring_item"] <- "Personal monetary valuation of Belgian chocolates (SEK)"                 
cad[cad$reference_short == "Bergman et al., 2010" & cad$anchoring_item == "Price of a book on interior design (SEK)", "anchoring_item"] <- "Personal monetary valuation of a book on interior design (SEK)"           
cad[cad$reference_short == "Bergman et al., 2010" & cad$anchoring_item == "Price of a radio transmitter for mp3-players (SEK)", "anchoring_item"] <- "Personal monetary valuation of a radio transmitter for mp3-players (SEK)"

## cheek 2020 preregistered
cad[cad$reference_short == "Cheek & Norem, 2020", "preregistered"] <- "https://aspredicted.org/h7qi4.pdf"
cad[cad$reference_short == "Cheek & Norem, 2020", "preregtype"] <- "AsPredicted"


## Cheek 2018 STUDY
# https://online.ucpress.edu/collabra/article/4/1/12/112966/On-Moderator-Detection-in-Anchoring-Research
# https://osf.io/e37xk/

## ONUKI add semantic description to anchor
# cad[cad$reference_short == "Onuki et al., 2021, Study 1a", "anchoring_item"]
# "Onuki et al., 2021, Study 1a"                 "Onuki et al., 2021, Study 1b"                
# [63] "Onuki et al., 2021, Study 3"





# Compute susceptibility scores -------------------------------------------

## Estimate
cad$estimate <- as.numeric(cad$estimate) # this is no susceptibility score but estimate reliability might affect anchoring score reliability
table(is.na(cad$estimate))
cad$anchor <- as.numeric(cad$anchor)

## Adjustment
cad$adjustment <- cad$estimate - cad$anchor

## Absolute Adjustment
cad$absadjustment <- abs(cad$estimate - cad$anchor)

## 01-Score 
# cad$score <- (cad$estimate - cad$anchor) / (cad$anchor - cad$true_value) # first iteration (v0)
# cad$score <- (cad$estimate - cad$true_value) / (cad$anchor - cad$true_value) # until v1
cad$score <- (cad$estimate - cad$anchor) / (cad$true_value - cad$anchor) # new (since v1.0.7)

## 01r-Score
cad$restr_score <- ifelse(cad$score < 0, 0,
                          ifelse(cad$score > 1, 1, cad$score))


# Distribution of Scores --------------------------------------------------

## Compute z-scores to compare different studies
cad$zestimate <- NA
cad$zadjustment <- NA
cad$zabsadjustment <- NA
cad$zscore <- NA

# Assign z-scores to rows
for (i in unique(cad$reference_short)) {
  anchoring_itemlist_i <- unique(cad[(cad$reference_short == i), "anchoring_item"])
  for (j in anchoring_itemlist_i) {
    cad[(cad$reference_short == i & cad$anchoring_item == j), "zestimate"] <- scale(cad[(cad$reference_short == i & cad$anchoring_item == j), "estimate"])
    cad[(cad$reference_short == i & cad$anchoring_item == j), "zadjustment"] <- scale(cad[(cad$reference_short == i & cad$anchoring_item == j), "adjustment"])
    cad[(cad$reference_short == i & cad$anchoring_item == j), "zabsadjustment"] <- scale(cad[(cad$reference_short == i & cad$anchoring_item == j), "absadjustment"])
    cad[(cad$reference_short == i & cad$anchoring_item == j), "zscore"] <- scale(cad[(cad$reference_short == i & cad$anchoring_item == j), "score"])
  }
}


# Determine outlier exclusion criterion
zmax <- 5
# Apply z-score filters
cad$outlier <- 0
cad$outlier <- ifelse(abs(cad$zestimate) >= zmax, 1, cad$outlier)
cad$outlier <- ifelse(abs(cad$zadjustment) >= zmax, 1, cad$outlier)
cad$outlier <- ifelse(abs(cad$zabsadjustment) >= zmax, 1, cad$outlier)
cad$outlier <- ifelse(abs(cad$zscore) >= zmax, 1, cad$outlier)

cadfiltered <- cad[cad$outlier == 0, ] # cad$outlier == 0 | is.na(cad$outlier)

p0 <- ggplot(cadfiltered, aes(x = adjustment, color = reference_short)) + geom_density() + theme_bw() + guides(color = guide_legend(title = "Reference"))
p1 <- ggplot(cadfiltered, aes(x = zestimate, color = reference_short)) + geom_density() + theme_bw() + theme(legend.position = "none")
p2 <- ggplot(cadfiltered, aes(x = zadjustment, color = reference_short)) + geom_density() + theme_bw() + theme(legend.position = "none")
p3 <- ggplot(cadfiltered, aes(x = zabsadjustment, color = reference_short)) + geom_density() + theme_bw() + theme(legend.position = "none")
p4 <- ggplot(cadfiltered, aes(x = zscore, color = reference_short)) + geom_density() + theme_bw() + theme(legend.position = "none")
p5 <- ggplot(cadfiltered, aes(x = restr_score, color = reference_short)) + geom_density() + theme_bw() + theme(legend.position = "none")
p_legend <- ggpubr::as_ggplot(ggpubr::get_legend(p0))
p_overview <- gridExtra::grid.arrange(p1, p2, p3, p4, p5, p_legend, nrow = 2) # this takes a long time

c1 <- which( colnames(cad) == "zestimate" )
cn <- which( colnames(cad) == "zscore" )
# GGally::ggpairs(cad[, c1:cn]) + theme_bw() # this takes a long time

cadraw <- cad
# cad <- cadfiltered
cad <- cad[!is.na(cad$anchoring_item), ]


# Analyze Reliabilities ---------------------------------------------------


## Prepare dataframe for summary statistics
ri <- data.frame("reference" = as.numeric()
                 , "items" = as.numeric()
                 , "n" = as.numeric()
                 , "estimate" = as.numeric()
                 , "adjustment" = as.numeric()
                 , "absadjustment" = as.numeric()
                 , "score" = as.numeric()
                 , "restr_score" = as.numeric()
                 )

## run loop to compute statistics for each dataset
x <- 0
for (i in unique(cad$reference_short)) {
    x <- x + 1 # counter for rownumber
  
    for (j in c("estimate", "adjustment", "absadjustment", "score", "restr_score")) { 
    
    cadi <- cad[cad$reference_short == i, ]
    # cadi <- cadi[cadi$outlier != 1, ] # exclude outliers
    cadi_wide <- reshape::cast(cadi, participant_id ~ anchoring_item, value = j)
    # remove missing values so that alpha can be computed
    # cadi_wide <- cadi_wide[!is.na(rowSums(cadi_wide[, 2:ncol(cadi_wide)])), ]
    
    # # remove outliers so that alpha is not skewed
    # for (k in 2:ncol(cadi_wide)) {
    #   cadi_wide[, k] <- ifelse(abs(scale(cadi_wide[, k])) > zmax, NA, cadi_wide[, k])
    # }
    
    
    alpha <- try(psych::alpha(cadi_wide[, 2:ncol(cadi_wide)]), silent = TRUE )
    ri[x, "reference"] <- i
    ri[x, "items"] <- tryCatch(length(alpha$item.stats$n), error = function(e) print(NA))
    ri[x, "n"] <- tryCatch(min(alpha$item.stats$n), error = function(e) print(NA)) # number of participants for item with fewest participants
    ri[x, j] <- tryCatch(alpha$total$average_r, error = function(e) print(NA))
    
    # Save study details
    
    ri[x, "incentive"] <- paste(unique(cadi$incentive), collapse =" / ")
    ri[x, "direction"] <- paste(unique(cadi$direction), collapse =" / ")
    ri[x, "anchortype"] <- paste(unique(cadi$"anchortype"), collapse =" / ")
    ri[x, "tasktype"] <- paste(unique(cadi$"tasktype"), collapse =" / ")
    ri[x, "comparative_question"] <- paste(unique(cadi$"comparative_question"), collapse =" / ")
    ri[x, "experiment_type"] <- paste(unique(cadi$"experiment_type"), collapse =" / ")
    ri[x, "stimulitype"] <- paste(unique(cadi$"stimulitype"), collapse =" / ")
    ri[x, "sampletype"] <- paste(unique(cadi$"sampletype"), collapse =" / ")
    ri[x, "scaletype"] <- paste(unique(cadi$"scaletype"), collapse =" / ")
    
    ri[x, "published"] <- max(cadi$published)
    ri[x, "prop_female"] <- mean(cadi$sex == 2, na.rm = TRUE)
    ri[x, "mean_age"] <- mean(cadi$age, na.rm = TRUE)
    # ri[x, "incentive"] <- max(cadi$incentive, na.rm = TRUE)
    # ri[x, "direction"] <- max(cadi$direction, na.rm = TRUE)
    # ri[x, "anchortype"] <- cadi$anchortype[1]
    # ri[x, "tasktype"] <- cadi$tasktype[1]
    ri[x, "tasktypes"] <- length(unique(cadi$tasktype))
    # ri[x, "comparative_question"] <- max(cadi$comparative_question, na.rm = TRUE)
    # ri[x, "experiment_type"] <- max(cadi$experiment_type, na.rm = TRUE)
    # ri[x, "stimulitype"] <- max(cadi$stimulitype, na.rm = TRUE)
    ri[x, "stimulitypes"] <- length(unique(cadi$stimulitype))
    # ri[x, "sampletype"] <- max(cadi$sampletype, na.rm = TRUE)
    # ri[x, "scaletype"] <- max(cadi$scaletype, na.rm = TRUE)
    ri[x, "itemlist"] <- paste(unique(cadi$anchoring_item), collapse =" / ")
    ri[x, "itemtypelist"] <- paste(unique(cadi$tasktype), collapse =" / ")
    ri[x, "prereg"] <- ifelse(cadi$preregistered[1] == "0", "no", "yes")
    ri[x, "nationality"] <- paste(unique(cadi$nationality), collapse =" / ")
    
    ri[x, "r_absadj_estimate"] <- tryCatch(cor(cadi$absadjustment, cadi$estimate, use = "complete.obs"), error = function(e) print(NA))
    ri[x, "r_score_estimate"] <- tryCatch(cor(cadi$score, cadi$estimate, use = "complete.obs"), error = function(e) print(NA))
    ri[x, "sd_accuracy"] <- tryCatch(sd(abs(cadi$estimate - cadi$true_value)/cadi$estimate, na.rm = TRUE), error = function(e) print(NA))
    ri[x, "r_absadj_accuracy"] <- tryCatch(cor(cadi$absadjustment, (cadi$estimate - cadi$true_value)), error = function(e) print(NA))
    
    
  }
}

# View(ri)

ri$items <- 1 + stringr::str_count(ri$itemlist, " / ")

# Plot inter-item correlations
rilong <- stats::reshape(ri, direction = "long"
               , varying = list(c("adjustment", "absadjustment", "score", "restr_score"))
               , v.names = "r", idvar = "reference", timevar = "sta_score"
               )
rilong$sta_score <- dplyr::recode(rilong$sta_score
                           , "1" = "adjustment"
                           , "2" = "absadjustment"
                           , "3" = "score"
                           , "4" = "restr_score"
                           )

rilong$anchortype <- dplyr::recode(rilong$anchortype
                                   , "1" = "explicitly random"
                                   , "2" = "fixed and provided without explanation"
                                   , "3" = "having some relevance"
                                   , "4" = "self-generated"
                                   , "5" = "incidental/subliminal"
                                   , .default = "mixed"
                                   )
rilong$stimulitype <- dplyr::recode(rilong$stimulitype
                                    , "1" = "short written"
                                    , "2" = "rich written"
                                    , "3" = "image/audio/video"
                                    , "4" = "2&3"
                                    , "9" = "Other"
                                    )

rilong$reference <- as.factor(rilong$reference)

ggplot(data = rilong, aes(x = r, y = reference, color = sta_score)) + geom_point(size = 2) + 
  theme_bw() + scale_y_discrete(limits = rev(levels(rilong$reference))) + 
  guides(color = guide_legend(title = "")) +
  # scale_color_brewer(palette = "Accent")
  scale_color_manual(values=c("dark blue", "light blue", "red", "orange"))

cor(ri[, 4:7], use = "complete.obs")






# #### Split Half Reliability with Parsons package
# 
# ## Prepare dataframe for summary statistics
# rish <- data.frame("reference" = as.numeric()
#                    , "items" = as.numeric()
#                    , "n" = as.numeric()
#                    , "estimate" = as.numeric()
#                    , "adjustment" = as.numeric()
#                    , "absadjustment" = as.numeric()
#                    , "score" = as.numeric()
#                    , "restr_score" = as.numeric()
# )
# 
# ## run loop to compute statistics for each dataset
# x <- 0
# for (i in unique(cad$reference_short)) {
#   x <- x + 1 # counter for rownumber
#   
#   for (j in c("estimate", "adjustment", "absadjustment", "score", "restr_score")) { 
#     
#     cadi <- cad[cad$reference_short == i, ]
#     # cadi <- cadi[cadi$outlier != 1, ] # exclude outliers
#     cadi_wide <- reshape::cast(cadi, participant_id ~ anchoring_item, value = j)
#     # remove missing values so that alpha can be computed
#     # cadi_wide <- cadi_wide[!is.na(rowSums(cadi_wide[, 2:ncol(cadi_wide)])), ]
#     
#     # # remove outliers so that alpha is not skewed
#     # for (k in 2:ncol(cadi_wide)) {
#     #   cadi_wide[, k] <- ifelse(abs(scale(cadi_wide[, k])) > zmax, NA, cadi_wide[, k])
#     # }
#     
#     if (length(unique(cadi$anchoring_item)) > 1) {
#       sh <- try(splithalf::splithalf(data = cadi
#                                      , var.participant = "participant_id"
#                                      , var.trialnum = "anchoring_item"
#                                      , var.compare = "anchorhigh"
#                                      , compare1 = "0"
#                                      , compare2 = "1"
#                                      , outcome = "accuracy"
#                                      , score = "average"
#                                      , var.ACC = j
#                                      , plot = FALSE)
#                 , silent = TRUE)
#     } else {
#       sh <- NA
#     }
#     
#     
#     
#     
#     # sh$final_estimates$splithalf
#     
#     alpha <- try(psych::alpha(cadi_wide[, 2:ncol(cadi_wide)]), silent = TRUE )
#     rish[x, "reference"] <- i
#     rish[x, "items"] <- length(unique(cadi$anchoring_item))
#     rish[x, "n"] <-     tryCatch(sh$final_estimates$n, error = function(e) print(NA)) # number of participants for item with fewest participants
#     rish[x, j] <-       tryCatch(sh$final_estimates$splithalf, error = function(e) print(NA))
#     
#     
#   }
# } # takes ~ >3 minutes
# 
# ## Merge ri and rish
# names(rish)[4:8] <- paste(names(rish)[4:8], "_sh", sep = "")
# ri <- (merge(ri, rish[, c(1, 4:8)], by = "reference"))
# 
# # compare average r and splithalf
# plot(ri$estimate, ri$estimate_sh)
# abline(0,1)
# # plot(ri$adjustment, ri$adjustment_sh)
# plot(ri$absadjustment, ri$absadjustment_sh)
# abline(0,1)
# # plot(ri$score, ri$score_sh)
# # abline(0,1)
# plot(ri$restr_score, ri$restr_score_sh)
# cor.test(ri$score, ri$score_sh)
# cor.test(ri$restr_score, ri$restr_score_sh)
# cor.test(ri$absadjustment, ri$absadjustment_sh)
# plot(ri$absadjustment, ri$absadjustment_sh)
# cor.test(ri$estimate, ri$estimate_sh)
# abline(0,1)
# 
# rishlong <- stats::reshape(ri, direction = "long"
#                          , varying = list(c("adjustment", "absadjustment", "score", "restr_score", "adjustment_sh", "absadjustment_sh", "score_sh", "restr_score_sh"))
#                          , v.names = "r", idvar = "reference", timevar = "sta_score"
# )
# rishlong$reltype <- ifelse(rishlong$sta_score < 5, "average_r", "splithalf")
# rishlong$sta_score <- as.factor(rishlong$sta_score)
# 
# rishlong$sta_score <- dplyr::recode(rishlong$sta_score
#                                   , "1" = "adjustment"
#                                   , "2" = "absadjustment"
#                                   , "3" = "score"
#                                   , "4" = "restr_score"
#                                   , "5" = "adjustment"
#                                   , "6" = "absadjustment"
#                                   , "7" = "score"
#                                   , "8" = "restr_score"
# )
# 
# rishlong_clean <- rishlong[!is.na(rishlong$r), ]
# ggplot(data = rishlong_clean, aes(x = r, y = reference, color = sta_score)) + geom_point(size = 2) + 
#   theme_bw() + scale_y_discrete(limits = rev(levels(rilong$reference))) + 
#   guides(color = guide_legend(title = "")) + facet_wrap("reltype") +
#   geom_vline(xintercept = 0) + # coord_flip() +
#   # scale_color_brewer(palette = "Accent")
#   scale_color_manual(values=c("dark blue", "light blue", "red", "orange"))
# 
# ggplot(data = ri, aes(x = absadjustment, y = absadjustment_sh)) + geom_point() +
#   geom_abline(intercept = 0, slope = 1) + theme_bw() + geom_smooth(method = "lm") +
#   xlab("average interitem r") + ylab("mean splithalf r") + ggtitle("Absolute Adjustment")
# 
# ggplot(data = ri, aes(x = restr_score, y = restr_score_sh)) + geom_point() +
#   geom_abline(intercept = 0, slope = 1) + theme_bw() + geom_smooth(method = "lm") +
#   xlab("average interitem r") + ylab("mean splithalf r") + ggtitle("Restricted 0-1-score")
# 
# ggplot(data = ri, aes(x = score, y = score_sh)) + geom_point() +
#   geom_abline(intercept = 0, slope = 1) + theme_bw() + geom_smooth(method = "lm") +
#   xlab("average interitem r") + ylab("mean splithalf r") + ggtitle("0-1-score")

# What does reliability depend on?
# summary(lmerTest::lmer(r ~ 1 + sta_score + estimate + g + (1 | reference), data = rilong))


# Anchoring effects -------------------------------------------------------

## Plot effect sizes
es <- data.frame("reference_short" = as.numeric()
                 , "item" = as.numeric()
                 , "high_anchor" = as.numeric()
                 , "low_anchor" = as.numeric()
                 , "true_value" = as.numeric()
                 , "n" = as.numeric()
                 , "dlower" = as.numeric()
                 , "d" = as.numeric()
                 , "dupper" = as.numeric()
                 # , "tasktype" = as.character()
                 # , "direction" = as.numeric()
                 # , "incentive" = as.character()
                 # , "comparative_question" = as.numeric()
                 # , "experiment_type" = as.numeric()
                 # , "stimulitype" = as.numeric()
                 # , "sampletype" = as.numeric()
                 # , "scaletype" = as.numeric()
                 # , "anchortype" = as.character()
                 # , "anchormanipulation" = as.character()
                 # , "published" = as.numeric()
                 , "prop_female" = as.numeric()
                 , "mean_age" = as.numeric()
                 , "mean_estimate" = as.numeric()
                 , "sd_estimate" = as.numeric()
                 , "mean_anchor" = as.numeric()
                 , "precision" = as.numeric()
                 )

         

x <- 0

for (i in unique(cad$reference_short)) {
  for (j in unique(cad[cad$reference_short == i, "anchoring_item"])) { 
    x <- x+1 # counter for rownumber
    
    # filter dataset
    cadi <- cad[cad$reference_short == i, ] 
    cadi <- cadi[cadi$anchoring_item == j, ] 
    # cadi <- cadi[cadi$outlier != 1, ] # exclude outliers
    
    
    # save variables per dataset
    es[x, "reference_short"] <- i
    es[x, "item"] <- j
    es[x, "n"] <- length(unique(cadi$participant_id)) # number of participants
    es[x, "published"] <- max(cadi$published)
    es[x, "prop_female"] <- mean(cadi$sex == 2, na.rm = TRUE)
    es[x, "mean_age"] <- mean(cadi$age, na.rm = TRUE)
    # paste(unique(cadi$anchoring_item), collapse =" / ")
    # es[x, "incentive"] <- max(cadi$incentive, na.rm = TRUE)
    # es[x, "direction"] <- max(cadi$direction, na.rm = TRUE)
    # es[x, "anchortype"] <- cadi$anchortype[1]
    # es[x, "tasktype"] <- cadi$tasktype[1]
    # es[x, "comparative_question"] <- max(cadi$comparative_question, na.rm = TRUE)
    # es[x, "experiment_type"] <- max(cadi$experiment_type, na.rm = TRUE)
    # es[x, "stimulitype"] <- max(cadi$stimulitype, na.rm = TRUE)
    # es[x, "sampletype"] <- max(cadi$sampletype, na.rm = TRUE)
    # es[x, "scaletype"] <- max(cadi$scaletype, na.rm = TRUE)

    es[x, "incentive"] <- paste(unique(cadi$incentive), collapse =" / ")
    es[x, "direction"] <- paste(unique(cadi$direction), collapse =" / ")
    es[x, "anchortype"] <- paste(unique(cadi$"anchortype"), collapse =" / ")
    es[x, "tasktype"] <- paste(unique(cadi$"tasktype"), collapse =" / ")
    es[x, "comparative_question"] <- paste(unique(cadi$"comparative_question"), collapse =" / ")
    es[x, "experiment_type"] <- paste(unique(cadi$"experiment_type"), collapse =" / ")
    es[x, "stimulitype"] <- paste(unique(cadi$"stimulitype"), collapse =" / ")
    es[x, "sampletype"] <- paste(unique(cadi$"sampletype"), collapse =" / ")
    es[x, "scaletype"] <- paste(unique(cadi$"scaletype"), collapse =" / ")
    es[x, "prereg"] <- ifelse(cadi$preregistered[1] == "0", "no", "yes")
    es[x, "preregtype"] <- paste(unique(cadi$"preregtype"), collapse =" / ")
    es[x, "nationality"] <- paste(unique(cadi$"nationality"), collapse =" / ")
    
    es[x, "mean_estimate"] <- mean(cadi$estimate, na.rm = TRUE)
    es[x, "sd_estimate"] <- sd(cadi$estimate, na.rm = TRUE)
    es[x, "mean_anchor"] <- mean(cadi$anchor, na.rm = TRUE)
    es[x, "precise"] <- paste(unique(cadi$precise), collapse =" / ")
    
    
    # count number of anchors and save d or convert via r
    nanchors <- length(unique(cadi$anchor))
    
    if (nanchors == 2) {
      
      # two anchor groups (compute Cohen's d)
      di <- tryCatch(psych::cohen.d(cadi$estimate, group = cadi$anchor), error = function(e) print(NA)) # if no two anchors exist for one item, NA will be printed
      
      es[x, "dlower"] <- tryCatch(di[["cohen.d"]][1], error = function(e) print(NA))
      es[x, "d"] <- tryCatch(di[["cohen.d"]][2], error = function(e) print(NA))
      es[x, "dupper"] <- tryCatch(di[["cohen.d"]][3], error = function(e) print(NA))
      es[x, "anchormanipulation"] <- "high vs. low"
      es[x, "high_anchor"] <- max(cadi$anchor, na.rm = TRUE)
      es[x, "low_anchor"]  <- min(cadi$anchor, na.rm = TRUE)
      es[x, "true_value"]  <- mean(cadi$true_value, na.rm = TRUE)
      

      
    } else {
      
      # more than two anchors (compute r and convert it to d)
      di <- tryCatch(cor.test(cadi$estimate, cadi$anchor), error = function(e) print(NA))
      
      es[x, "dlower"] <- tryCatch(psych::r2d(di$conf.int[1]), error = function(e) print(NA))
      es[x, "d"] <- tryCatch(psych::r2d(di$estimate), error = function(e) print(NA))
      es[x, "dupper"] <- tryCatch(psych::r2d(di$conf.int[2]), error = function(e) print(NA))
      es[x, "anchormanipulation"] <- "continuous"
      es[x, "high_anchor"] <- max(cadi$anchor, na.rm = TRUE) # XXX WARNING: mean distance might be overestimated here
      es[x, "low_anchor"]  <- min(cadi$anchor, na.rm = TRUE) # XXX WARNING: mean distance might be overestimated here
      es[x, "true_value"]  <- mean(cadi$true_value, na.rm = TRUE)
      
    }
    
  }
}

# determine variable types
es$direction <- as.factor(ifelse(es$direction == 1, "yes", "no"))
es$incentive <- dplyr::recode(es$incentive
                              , "0" = "not incentivized"
                              , "1" = "incentivized for participation"
                              , "2" = "incentivized for accurate estimation"
                              , .default = "mixed"
                              )
es$comparative_question <- as.factor(ifelse(es$comparative_question == 1, "yes", "no"))
es$experiment_type <- dplyr::recode(es$experiment_type
                                              , "1" = "online" 
                                              , "2" = "lab"
                                              , "3" = "class"
                                              , "4" = "field"
                                              , "5" = "mixed"
                                    , .default = "mixed")
es$stimulitype <- dplyr::recode(es$stimulitype
                                          , "1" = "short written"
                                          , "2" = "rich written"
                                          , "3" = "image, audio, or video"
                                          , "4" = "2 and 3"
                                          , "9" = "other"
                                , .default = "mixed")
es$sampletype <- dplyr::recode(es$sampletype
                               , "1" = "lay"
                               , "2" = "professional or expert"
                               , "3" = "mixed"
                               , .default = "mixed")
es$scaletype <- dplyr::recode(es$scaletype
                              , "1" = "open"
                              , "2" = "closed"
                              , "3" = "visual"
                              , .default = "mixed")
es$published <- dplyr::recode(es$published
                              , "2" = "peer-reviewed"
                              , "1" = "pre-print"
                              , "0" = "unpublished"
                              , .default = "mixed")
es$anchortype <- dplyr::recode(es$anchortype
                              , "1" = "explicitly random"
                              , "2" = "fixed and provided without explanation"
                              , "3" = "having some relevance"
                              , "4" = "self-generated"
                              , "5" = "incidental or subliminal"
                              , .default = "mixed"
                              )

es$precise <- dplyr::recode(es$precise
                               , "no" = "no"
                               , "yes" = "yes"
                               , "NA" = "NA"
                               , .default = "mixed")
es$precise <- as.factor(ifelse(es$precise == "NA", NA, es$precise))

es$yearofpublication <- as.numeric(substr(as.numeric(gsub('\\D+','', es$reference_short)), 1, 4))

es$anchorinterval <- paste("[", es$low_anchor, ", ", es$high_anchor, "]", sep = "")


es$g <- esc::hedges_g(es$d, totaln = es$n)
es$glower <- esc::hedges_g(es$dlower, totaln = es$n)
es$gupper <- esc::hedges_g(es$dupper, totaln = es$n)

es$true_value_log <- log(es$true_value)

es$anchordistance <- (abs(es$high_anchor - es$true_value) + abs(es$low_anchor - es$true_value)) / 2 / es$true_value
es$anchordistance <- ifelse(es$anchordistance > 20, NA, es$anchordistance)
# hist(es$anchordistance) #, xlim = c(0, 20), breaks = 100)
# plot(es$anchordistance, es$d)

## Compute mean anchoring effect size by study
rid <- aggregate(d ~ reference_short, data = es, FUN = "mean")
rig <- aggregate(g ~ reference_short, data = es, FUN = "mean")
ri <- (merge(ri, rig, all.x = TRUE, all.y = FALSE, by.x = "reference", by.y = "reference_short"))
rilong <- (merge(rilong, rig, all.x = TRUE, all.y = FALSE, by.x = "reference", by.y = "reference_short"))

ri$n <- NULL
rilong$n <- NULL
rin <- aggregate(n ~ reference_short, data = es, FUN = "min")
ri <- (merge(ri, rin, all.x = TRUE, all.y = FALSE, by.x = "reference", by.y = "reference_short"))
rilong <- (merge(rilong, rin, all.x = TRUE, all.y = FALSE, by.x = "reference", by.y = "reference_short"))

# Standardized anchoring effect measure -----------------------------------

rel_es <- data.frame("reference_short" = as.numeric()
                 , "item" = as.numeric()
                 , "anchor" = as.numeric()
                 # , "high_anchor" = as.numeric()
                 # , "low_anchor" = as.numeric()
                 # , "true_value" = as.numeric()
                 , "n" = as.numeric()
                 , "mean" = as.numeric()
                 , "sd" = as.numeric()
                 , "mean_unrestricted" = as.numeric()
                 , "sd_unrestricted" = as.numeric()
)

x <- 0

for (i in unique(cad$reference_short)) {
  cadi <- cad[cad$reference_short == i, ] 
  
  for (j in unique(cadi[cadi$reference_short == i, "anchoring_item"])) { 
    
    # filter dataset
    
    cadij <- cadi[cadi$anchoring_item == j, ] 
    
    for (k in unique(cadij$anchor)) {
      x <- x+1 # counter for rownumber
      
      cadijk <- cadij[cadij$anchor == k, ]
      
      # compute variables
      rel_es[x, "reference_short"] <- i
      rel_es[x, "item"] <- j
      rel_es[x, "anchor"] <- k
      # rel_es[x, "true_value"] <- k
      rel_es[x, "anchorhigh"] <- mean(cadijk$anchorhigh, na.rm = TRUE)
      rel_es[x, "n"] <- length(unique(cadijk$participant_id)) # number of participants
      rel_es[x, "mean"] <- mean(cadijk$restr_score, na.rm = TRUE)
      rel_es[x, "sd"] <- sd(cadijk$restr_score, na.rm = TRUE)
      rel_es[x, "mean_unrestricted"] <- mean(cadijk$score, na.rm = TRUE)
      rel_es[x, "sd_unrestricted"] <- sd(cadijk$score, na.rm = TRUE)
    }
  }
}

# exclude rows with less than 10 cases
rel_es <- rel_es[rel_es$n >= 10, ]

rel_es$ucl <- rel_es$mean + qnorm(.975) * rel_es$sd / sqrt(rel_es$n)
rel_es$lcl <- rel_es$mean - qnorm(.975) * rel_es$sd / sqrt(rel_es$n)

rel_es$ucl_unrestricted <- rel_es$mean_unrestricted + qnorm(.975) * rel_es$sd_unrestricted / sqrt(rel_es$n)
rel_es$lcl_unrestricted <- rel_es$mean_unrestricted - qnorm(.975) * rel_es$sd_unrestricted / sqrt(rel_es$n)

rel_es$se <- rel_es$sd * sqrt(rel_es$n)
rel_es$se_unrestricted <- rel_es$sd_unrestricted * sqrt(rel_es$n)

# View(rel_es)

# rel_es$condition <- paste(rel_es$item, rel_es$anchor)
rel_es$anchorhigh <- ifelse(rel_es$anchorhigh == 1, "high anchor", ifelse(rel_es$anchorhigh == 0, "low anchor", "no true value"))

rel_es <- rel_es[base::order(rel_es$mean, decreasing = TRUE), ]
rel_es <- transform(rel_es, variable = reorder(item, -mean) )
rownames(rel_es) <- 1:nrow(rel_es)

forest_standardized <- ggplot2::ggplot(data = rel_es, aes(x = mean, y = item, col = reference_short, shape = anchorhigh)) + 
  geom_vline(xintercept = c(0, 1), col = "dark grey", lwd = 1) +
  geom_point() + xlim(c(-.25, 1.25)) +
  geom_errorbar(aes(xmin = rel_es$lcl, xmax = rel_es$ucl)) +
  theme_classic() + guides(col = guide_legend(ncol = 1)) +
  theme(legend.title = element_blank())

forest_standardized_unrestricted <- ggplot2::ggplot(data = rel_es, aes(x = mean_unrestricted, y = item, col = reference_short, shape = anchorhigh)) + 
  geom_vline(xintercept = c(0, 1), col = "dark grey", lwd = 1) +
  geom_point() + xlim(c(-.25, 1.25)) +
  geom_errorbar(aes(xmin = rel_es$lcl_unrestricted, xmax = rel_es$ucl_unrestricted)) +
  theme_classic() + guides(col = guide_legend(ncol = 1)) +
  theme(legend.title = element_blank())

ggsave("forestplot_standardized.tiff", forest_standardized, width = 4000, height = 6000, units = "px", scale = 2)



### Meta-analytic model (relative effects)
# rel_es <- rel_es[!is.na(rel_es$mean_unrestricted), ]
# rel_es <- rel_es[rel_es$se_unrestricted > 0, ]
# remles_all <- metafor::rma.mv(yi = mean
#                             , V = se^2
#                             , random = ~1 | reference_short
#                             , tdist = TRUE
#                             , data = rel_es
#                             , mods = ~ anchor
#                             , method = "ML")
# remles_all



# Literature lists --------------------------------------------------------
refs_dataset <- sort(unique(cad$reference))
studies_dataset <- sort(unique(cad$reference_short))
studies_reliabilityproblem <- sort(intersect(cad$reference_short, ri[!is.na(ri$adjustment), "reference"]))
studies_metaanalysis <- sort(intersect(cad$reference_short, es[!is.na(es$g), "reference_short"]))



# # Validity of sta scores --------------------------------------------------
# plot(ri$absadjustment, ri$r_absadj_estimate)
# validityplot1 <- ggplot2::ggplot(ri, aes(x = absadjustment, y = r_absadj_estimate, fill = reference, color = reference)) +
#   geom_point() + theme_bw()
# plotly::ggplotly(validityplot1)
# # hist(ri$r_absadj_estimate)
# 
# validityplot2 <- ggplot2::ggplot(ri, aes(x = score, y = r_score_estimate, fill = reference, color = reference)) +
#   geom_point() + theme_bw()
# plotly::ggplotly(validityplot2)




# Remove variables
es$mean_estimate <- NULL
es$sd_estimate <- NULL
es$mean_anchor <- NULL



# Write current results ---------------------------------------------------
xlsx::write.xlsx(ri, file="opaq_results.xlsx", sheetName = "reliabilities", row.names = FALSE)
xlsx::write.xlsx(rilong, file="opaq_results.xlsx", sheetName = "reliabilities_long", append = TRUE, row.names = FALSE)
xlsx::write.xlsx(es, file="opaq_results.xlsx", sheetName = "effectsizes", append = TRUE, row.names = FALSE)

xlsx::write.xlsx(refs_dataset, file="opaq_references.xlsx", sheetName = "References", append = FALSE, row.names = FALSE)
xlsx::write.xlsx(studies_dataset, file="opaq_references.xlsx", sheetName = "Studies in Complete Dataset", append = TRUE, row.names = FALSE)
xlsx::write.xlsx(studies_reliabilityproblem, file="opaq_references.xlsx", sheetName = "Studies in Reliability Analysis", append = TRUE, row.names = FALSE)
xlsx::write.xlsx(studies_metaanalysis, file="opaq_references.xlsx", sheetName = "Studies in Meta-Analysis", append = TRUE, row.names = FALSE)

# xlsx::write.xlsx(cad, file="opaq_results.xlsx", sheetName = "data", append = TRUE, row.names = TRUE) # this will take a while
write.csv(cad, file = "opaq_65a.csv", fileEncoding = "UTF-8")
# write.csv(cadraw, file = "opaq_raw.csv", fileEncoding = "UTF-8") # no outliers (currently z_abs < 5 for estimate and all scores except for restr_score)


  

