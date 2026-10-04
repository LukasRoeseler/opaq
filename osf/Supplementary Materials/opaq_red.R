
# Compute Anchoring Effects for Replication Database ----------------------


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
library(openxlsx)

## Open Dataset

cad <- read.csv("opaq_71a.csv", sep = ",", dec = ".")



# Anchoring effects -------------------------------------------------------


## Plot effect sizes
es <- data.frame("reference_short" = as.numeric()
                 , "item" = as.numeric()
                 , "n" = as.numeric()
                 , "d" = as.numeric()
)



x <- 0

for (i in unique(cad$reference_short)) {
  for (j in unique(cad[cad$reference_short == i, "anchoring_item"])) { 
    x <- x+1 # counter for rownumber
    
    # filter dataset
    cadi <- cad[cad$reference_short == i, ] 
    cadi <- cadi[cadi$anchoring_item == j, ] 
    
    # save variables per dataset
    es[x, "reference_short"] <- i
    es[x, "item"] <- j
    es[x, "n"] <- length(unique(cadi$participant_id)) # number of participants
    es[x, "published"] <- max(cadi$published)
    
    es[x, "incentive"] <- paste(unique(cadi$incentive), collapse =" / ")
    es[x, "direction"] <- paste(unique(cadi$direction), collapse =" / ")
    es[x, "anchortype"] <- paste(unique(cadi$"anchortype"), collapse =" / ")
    es[x, "tasktype"] <- paste(unique(cadi$"tasktype"), collapse =" / ")
    es[x, "comparative_question"] <- paste(unique(cadi$"comparative_question"), collapse =" / ")
    es[x, "experiment_type"] <- paste(unique(cadi$"experiment_type"), collapse =" / ")
    es[x, "stimulitype"] <- paste(unique(cadi$"stimulitype"), collapse =" / ")
    es[x, "sampletype"] <- paste(unique(cadi$"sampletype"), collapse =" / ")
    es[x, "scaletype"] <- paste(unique(cadi$"scaletype"), collapse =" / ")
    es[x, "prereg"] <- cadi$preregistered[1]
    es[x, "preregtype"] <- paste(unique(cadi$"preregtype"), collapse =" / ")
    es[x, "nationality"] <- paste(unique(cadi$"nationality"), collapse =" / ")
    es[x, "ref_long"] <- cadi$reference[1]
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
      
      # compute anchoring index for two anchor items
      lowanchor <- min(unique(cadi$anchor))  # determine low anchor
      highanchor <-  max(unique(cadi$anchor)) # determine high anchor
      median_lowanchor <- median(cadi[cadi$anchor == lowanchor, "estimate"], na.rm = TRUE)   # determine median for low anchor
      median_highanchor <- median(cadi[cadi$anchor == highanchor, "estimate"], na.rm = TRUE)  # determine median for high anchor
      es[x, "anchoring_index"] <- (median_highanchor - median_lowanchor) / (highanchor - lowanchor)
      
      
      
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
                              , "2" = "2" #"peer-reviewed"
                              , "1" = "1" #"pre-print"
                              , "0" = "0" #"unpublished"
                              , .default = "mixed")
es$anchortype <- dplyr::recode(es$anchortype
                               , "1" = "explicitly random"
                               , "2" = "fixed and provided without explanation"
                               , "3" = "having some relevance"
                               , "4" = "self-generated"
                               , "5" = "incidental or subliminal"
                               , .default = "mixed"
)


es$yearofpublication <- as.numeric(substr(as.numeric(gsub('\\D+','', es$reference_short)), 1, 4))



# Convert to ReD format ---------------------------------------------------


red <- data.frame(
  "id" = paste("OpAQ_", 1:nrow(es), sep = "")
  , "validated" = 1
  , "validated_person" = "LR"
  , "source" = "OpAQ"
  , "discipline" = "Judgment and Decision Making"
  , "effect" = "Anchoring Effect"
  , "tags" = "anchoring, anchor, numerical estimates, anchoring and adjustment, heuristics and biases"
  , "description" = "When giving numerical estimates, people are biased towards a previously considered value"
  , "notes" = "These effects have been taken from the Open Anchoring Quest version 71a, see https://osf.io/3xhgw for conversion code. Unfortunately, J&K 1995 do not report test statistics for the original effect size but only for the anchoring index which includes a comparison of high and low anchors."
  , "contributors" = "LR"
  , "date_entered" = "22/11/2023"
  , "notes_validation" = NA
  , "exclusion" = NA
  , "es_original" = NA
  , "es_replication" = NA
  , "n_original" = 103
  , "n_replication" = es$n
  , "ref_original" = "Jacowitz, K. E., & Kahneman, D. (1995). Measures of anchoring in estimation tasks. Personality and Social Psychology Bulletin, 21(11), 1161-1166."
  , "doi_original" = "10.1177/01461672952111004"
  , "ref_replication" = es$ref_long
  , "doi_replication" = NA
  , "es_orig" = NA
  , "es_rep" = NA
  , "es_orig_value" = NA
  , "es_orig_estype" = NA
  , "es_rep_value" = es$d
  , "es_rep_estype" = "d"
  , "es_orig_RRR" = NA
  , "es_orig_RRR_estype" = NA
  , "es_rep_RRR" = NA
  , "es_rep_RRR_estype" = NA
  , "osf_link" = "https://osf.io/ygnvb/"
  , "outcome" = NA
  , "published_rep" = NA
  , "id_sample" = es$reference_short
  , "same_design" = NA
  , "nesting" = NA
  , "same_test" = NA
  , "original_authors" = 0
  , "study_orig" = NA
  , "study_rep" = NA
  , "teststatistic_orig" = "t(102) = 7.99"
  , "teststatistic_rep" = NA
  , "p_es_orig" = NA
  , "p_es_rep" = NA
  , "p_n_orig" = "1162"
  , "p_n_rep" = NA
  , "result" = NA
  , "preregistration" = es$prereg
  , "closeness_instructions" = NA
  , "closeness_measures" = NA
  , "closeness_stimuli" = NA
  , "closeness_procedure" = NA
  , "closeness_location" = NA
  , "closeness_renumeration" = NA
  , "closeness_participants" = NA
  , "closeness_exclusions" = NA
  , "closeness_language" = NA
  , "closeness_nationality" = NA
  , "differences" = NA
  , "vi_orig" = NA
  , "vi_rep" = NA
  , "ci.lower_original" = NA
  , "ci.upper_original" = NA
  , "ci.lower_replication" = NA
  , "ci.upper_replication" = NA
  , "significant_original" = NA
  , "significant_replication" = NA
  , "power" = NA
  , "orig_journal" = "Personality and Social Psychology Bulletin"
)


openxlsx::write.xlsx(red, file = "opaq_for_red.xlsx")

