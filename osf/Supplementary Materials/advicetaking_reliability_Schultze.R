
# Additional analyses of Schultze 2017 ------------------------------------
# Schultze, T., Mojzisch, A., & Schulz-Hardt, S. (2017). On the Inability to Ignore Useless Advice. Experimental Psychology, 64(3), 170–183. https://doi.org/10.1027/1618-3169/a000361
# supplementary materials: https://econtent.hogrefe.com/doi/suppl/10.1027/1618-3169/a000361
# NOTE: RUN SCHULTZES SCRIPT FIRST! (1618-3169_a000361_esm5.R)

# STA-weight estimator
sta <- function(estimate_unanchored, estimate_anchored, anchor, model = "adm") {
  
  if (model == "adm") {
    w1 <- (estimate_anchored - estimate_unanchored)/(anchor - estimate_unanchored)
    # w2 <- 1-w1
    
    return(w1)
    
  } else if (model == "rdm") {
    
    w1 <- NULL
    
    for (i in 1:length(estimate_unanchored)) {
      estimate_unanchored_i <- estimate_unanchored[i]
      estimate_anchored_i <- estimate_anchored[i]
      anchor_i <- anchor[i]
      
      
      rdm_pred <- function(estimate_unanchored_i, anchor_i, weight) {
        e_pred <- sqrt(estimate_unanchored_i^(2-weight)*anchor_i^weight)  # computes the weighted geometric mean of estimate and anchor
        return(e_pred)
      }
      
      
      f <- function(x) {
        abs(rdm_pred(estimate_unanchored = estimate_unanchored_i
                     , anchor = anchor_i
                     , weight = x) - estimate_anchored_i)
      }
      
      optimize <- (optimize(f, lower = -50, upper = 50, maximum = FALSE))$minimum
      w1 <- c(w1, optimize)
      # w2 <- 2-w1
    }
    return(w1)
    
    
  } else {
    return("Model must be adm or rdm.")
  }
}




# Analysis of data1 -------------------------------------------------------

data1 <- data1[!is.na(data1$AT), ]
data1$IE <- ifelse(data1$IE <= 0, .001, data1$IE)

data1$sta_adm <- sta(estimate_unanchored = data1$IE
                     , estimate_anchored = data1$FE
                     , anchor = data1$AD
                     , model = "adm")

# Are the estimates reliable?
vpcol <- which(colnames(data1) == "VP")
trialcol <- which(colnames(data1) == "trial")
sta_admcol <- which(colnames(data1) == "sta_adm")

data1_short_adm <- data1[, c(vpcol, trialcol, sta_admcol)] # VP, trial, sta_adm
data1_adm_wide <- reshape(data1_short_adm, idvar = "VP", timevar = c("trial"), direction = "wide")
alpha_adm <- psych::alpha(data1_adm_wide[, 2:101]) # average_r = .066

data1_avr_adm <- alpha_adm$total$average_r
data1_avr_rdm <- alpha_rdm$total$average_r

# Is absolute adjustment reliable?
# Compute adjustment from anchor without initial estimate
data1$adjustment <- abs(data1$FE-data1$AD)

# Are the estimates reliable?
vpcol <- which(colnames(data1) == "VP")
trialcol <- which(colnames(data1) == "trial")
adjcol <- which(colnames(data1) == "adjustment")

data1_short_adj <- data1[, c(vpcol, trialcol, adjcol)] # VP, trial, adjcol
data1_adj_wide <- reshape(data1_short_adj, idvar = "VP", timevar = c("trial"), direction = "wide")
alpha_adj <- psych::alpha(data1_adj_wide[, 2:ncol(data1_adj_wide)])
data1_alpha_adj <- alpha_adj$total$raw_alpha
data1_avr_adj <- alpha_adj$total$average_r



# Analysis of data2 -------------------------------------------------------

data2 <- data2[!is.na(data2$AT), ]
data2$IE <- ifelse(data2$IE <= 0, .001, data2$IE)

data2$sta_adm <- sta(estimate_unanchored = data2$IE
                     , estimate_anchored = data2$FE
                     , anchor = data2$AD
                     , model = "adm")

# Are the estimates reliable?
vpcol <- which(colnames(data2) == "VP")
trialcol <- which(colnames(data2) == "trial")
sta_admcol <- which(colnames(data2) == "sta_adm")

data2_short_adm <- data2[, c(vpcol, trialcol, sta_admcol)] # VP, trial, sta_adm
data2_adm_wide <- reshape(data2_short_adm, idvar = "VP", timevar = c("trial"), direction = "wide")
alpha_adm <- psych::alpha(data2_adm_wide[, 2:101]) # average_r = .066

data2_avr_adm <- alpha_adm$total$average_r


# Is absolute adjustment reliable?
# Compute adjustment from anchor without initial estimate
data2$adjustment <- abs(data2$FE-data2$AD)

# Are the estimates reliable?
vpcol <- which(colnames(data2) == "VP")
trialcol <- which(colnames(data2) == "trial")
adjcol <- which(colnames(data2) == "adjustment")

data2_short_adj <- data2[, c(vpcol, trialcol, adjcol)] # VP, trial, adjcol
data2_adj_wide <- reshape(data2_short_adj, idvar = "VP", timevar = c("trial"), direction = "wide")
alpha_adj <- psych::alpha(data2_adj_wide[, 2:ncol(data2_adj_wide)]) # average_r = .066
data2_alpha_adj <- alpha_adj$total$raw_alpha
data2_avr_adj <- alpha_adj$total$average_r

# Analysis of data3 -------------------------------------------------------

data3 <- data3[!is.na(data3$AT), ]
data3$IE <- ifelse(data3$IE <= 0, .001, data3$IE)

data3$sta_adm <- sta(estimate_unanchored = data3$IE
                     , estimate_anchored = data3$FE
                     , anchor = data3$AD
                     , model = "adm")

# Are the estimates reliable?
vpcol <- which(colnames(data3) == "VP")
trialcol <- which(colnames(data3) == "trial")
sta_admcol <- which(colnames(data3) == "sta_adm")

data3_short_adm <- data3[, c(vpcol, trialcol, sta_admcol)] # VP, trial, sta_adm
data3_adm_wide <- reshape(data3_short_adm, idvar = "VP", timevar = c("trial"), direction = "wide")
alpha_adm <- psych::alpha(data3_adm_wide[, 2:101]) # average_r = .066

data3_avr_adm <- alpha_adm$total$average_r

# Is absolute adjustment reliable?
# Compute adjustment from anchor without initial estimate
data3$adjustment <- abs(data3$FE-data3$AD)

# Are the estimates reliable?
vpcol <- which(colnames(data3) == "VP")
trialcol <- which(colnames(data3) == "trial")
adjcol <- which(colnames(data3) == "adjustment")

data3_short_adj <- data3[, c(vpcol, trialcol, adjcol)] # VP, trial, adjcol
data3_adj_wide <- reshape(data3_short_adj, idvar = "VP", timevar = c("trial"), direction = "wide")
alpha_adj <- psych::alpha(data3_adj_wide[, 2:ncol(data3_adj_wide)]) # average_r = .066
data3_alpha_adj <- alpha_adj$total$raw_alpha
data3_avr_adj <- alpha_adj$total$average_r


# Analysis of data4 -------------------------------------------------------

data4 <- data4[!is.na(data4$AT), ]
data4$IE <- ifelse(data4$IE <= 0, .001, data4$IE)

data4$sta_adm <- sta(estimate_unanchored = data4$IE
                     , estimate_anchored = data4$FE
                     , anchor = data4$AD
                     , model = "adm")

# Are the estimates reliable?
vpcol <- which(colnames(data4) == "VP")
trialcol <- which(colnames(data4) == "trial")
sta_admcol <- which(colnames(data4) == "sta_adm")

data4_short_adm <- data4[, c(vpcol, trialcol, sta_admcol)] # VP, trial, sta_adm
data4_adm_wide <- reshape(data4_short_adm, idvar = "VP", timevar = c("trial"), direction = "wide")
alpha_adm <- psych::alpha(data4_adm_wide[, 2:101]) # average_r = .066

data4_avr_adm <- alpha_adm$total$average_r

# Is absolute adjustment reliable?
# Compute adjustment from anchor without initial estimate
data4$adjustment <- abs(data4$FE-data4$AD)

# Are the estimates reliable?
vpcol <- which(colnames(data4) == "VP")
trialcol <- which(colnames(data4) == "trial")
adjcol <- which(colnames(data4) == "adjustment")

data4_short_adj <- data4[, c(vpcol, trialcol, adjcol)] # VP, trial, adjcol
data4_adj_wide <- reshape(data4_short_adj, idvar = "VP", timevar = c("trial"), direction = "wide")
alpha_adj <- psych::alpha(data4_adj_wide[, 2:ncol(data4_adj_wide)]) # average_r = .066
data4_alpha_adj <- alpha_adj$total$raw_alpha
data4_avr_adj <- alpha_adj$total$average_r






# Analysis of The effects of advice precision on advice taking
# Schultze, T., & Loschelder, D. D. (2020, August 28). The effects of advice precision on advice taking. https://doi.org/10.17605/OSF.IO/JGMD6
# data: https://osf.io/8mqb7/
data_sl <- read.csv("https://osf.io/8mqb7/download/")

# Remove missing cases and change 0 so that WOA can be computed
data_sl <- data_sl[!is.na(data_sl$IE), ]
data_sl$IE <- ifelse(data_sl$IE <= 0, .001, data_sl$IE)

data_sl$sta_adm <- sta(estimate_unanchored = data_sl$IE
                       , estimate_anchored = data_sl$FE
                       , anchor = data_sl$AD
                       , model = "adm")

data_sl$sta_adm <- ifelse(abs(data_sl$sta_adm) == Inf, NA, data_sl$sta_adm)


# Are the estimates reliable?
vpcol <- which(colnames(data_sl) == "VP")
trialcol <- which(colnames(data_sl) == "trial")
sta_admcol <- which(colnames(data_sl) == "sta_adm")

datasl_short_ad_sl <- data_sl[, c(vpcol, trialcol, sta_admcol)] # VP, trial, sta_adm
datasl_adm_wid_sl <- reshape(datasl_short_ad_sl, idvar = "VP", timevar = c("trial"), direction = "wide")
alpha_adm <- psych::alpha(datasl_adm_wid_sl[, 2:ncol(datasl_adm_wid_sl)])

data_sl_avr_ad <- alpha_adm$total$average_r

# Is absolute adjustment reliable?
# Compute adjustment from anchor without initial estimate
data_sl$adjustment <- abs(data_sl$FE-data_sl$AD)

# Are the estimates reliable?
vpcol <- which(colnames(data_sl) == "VP")
trialcol <- which(colnames(data_sl) == "trial")
adjcol <- which(colnames(data_sl) == "adjustment")

data_sl_short_adj <- data_sl[, c(vpcol, trialcol, adjcol)] # VP, trial, adjcol
data_sl_adj_wide <- reshape(data_sl_short_adj, idvar = "VP", timevar = c("trial"), direction = "wide")
alpha_adj <- psych::alpha(data_sl_adj_wide[, 2:ncol(data_sl_adj_wide)]) # average_r = .066
data_sl_alpha_adj <- alpha_adj$total$raw_alpha
data_sl_avr_adj <- alpha_adj$total$average_r










# Average Correlations ----------------------------------------------------

data1_avr_adm
data2_avr_adm
data3_avr_adm
data4_avr_adm
data_sl_avr_ad

data1_avr_adj
data2_avr_adj
data3_avr_adj
data4_avr_adj
data_sl_avr_adj

