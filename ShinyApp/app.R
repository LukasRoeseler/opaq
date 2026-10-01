
###################### To Do ###################### 

## - Daten
# - Replikationen EG (2x, March/April)
# - CS Overadjustment Study (1x, March)
# Röseler, L., Bögler, H. L., Koßmann, L., & Krueger, S. (2022). Replication of Epley & Gilovich, 2005, Study 2. [Unpublished data]. University of Bamberg.

## - Analysen
# - Stichprobe: Anteil öffentlich verfügbarer Datensätze (= mit Link)
# - Unterschiede zw Adjustierung vom hohen vs. niedrigen Anker
# - evtl. Robustheitschecks (vgl. bodypositions)



###################### Script ######################

library(shiny)
library(shinycssloaders)
library(dplyr)
library(openxlsx)
library(DT)
library(ggplot2)
library(forcats)
library(gridExtra)
library(ggpubr)
library(reshape)
library(plotly)
library(metafor)
# some code taken from: https://yihui.shinyapps.io/DT-info/ and https://rstudio.github.io/DT/shiny.html



# BASIC INFO --------------------------------------------------------------
cad <- read.csv("opaq_70a.csv", sep = ",", dec = ".", fileEncoding = "UTF-8") # , fileEncoding = "UTF-8"
version <- "Version 1.1.48.95" # 1.newanalyses?.errorscorectedorauthorsadded?.no_of_studies?
date <- "10 February, 2023" # enter last update here

# CHANGELOG ---------------------------------------------------------------
changelog <- HTML(paste("<h3><b>Changelog</b></h3><h5>"
                        , "<i>This is a list of changes for the OpAQ-website and the dataset starting on March, 10, 2022 with versions opaq_61a.csv (dataset) and OpAQ v 1.1.38.90</i>"
                        
                        # TEMPLATE FOR NEW VERSIONS
                        # , "</br><b>dd.mm.yyyy</b>"
                        # , "</br><i>App version: Version 1.1.xx.xx</i>"
                        # , "</br><i>Dataset name: opaq_xxx.csv</i>"
                        # , "</br>- ..."
                        
                        , "</br><b>10.02.2023</b>"
                        , "</br><i>App version: Version 1.1.48.97</i>"
                        , "</br><i>Dataset name: opaq_70a.csv</i>"
                        , "</br>- Temporarily removed \"Bickenbach et al., 2021\" and \"Seida & Röseler\" studies due to errors in the item names or values."
                        
                        , "</br><b>19.01.2023</b>"
                        , "</br><i>App version: Version 1.1.47.97</i>"
                        , "</br><i>Dataset name: opaq_69a.csv</i>"
                        , "</br>- Added dataset."
                        , "</br>- Updated OpAQ References."
                        , "</br>- Updated OpAQ Literature"
                        
                        , "</br><b>04.11.2022</b>"
                        , "</br><i>App version: Version 1.1.46.96</i>"
                        , "</br><i>Dataset name: opaq_68a.csv</i>"
                        , "</br>- Error in the processing of Bahník, 2021b detected and corrected (affected reliabilities due to format of participant_id)."
                        , "</br>- Added entry to literature table."
                        
                        , "</br><b>28.10.2022</b>"
                        , "</br><i>App version: Version 1.1.44.96</i>"
                        , "</br><i>Dataset name: opaq_67a.csv</i>"
                        , "</br>- Fixed an error where the true value for Knappe et al. was coded wrongly."
                        , "</br>- Added the OpAQ dataset publication to the literature list."
                        
                        , "</br><b>06.09.2022</b>"
                        , "</br><i>App version: Version 1.1.43.96</i>"
                        , "</br><i>Dataset name: opaq_67a.csv</i>"
                        , "</br>- Added dataset and contributors"
                        
                        , "</br><b>12.08.2022</b>"
                        , "</br><i>App version: Version 1.1.43.95</i>"
                        , "</br><i>Dataset name: opaq_66a.csv</i>"
                        , "</br>- Added a table with all published and unpublished reports of anchoring from our literature search, personal communications, calls for data, etc."
                        
                        , "</br><b>27.07.2022y</b>"
                        , "</br><i>App version: Version 1.1.42.95</i>"
                        , "</br>- Added a contributor"
                        
                        , "</br><b>26.07.2022y</b>"
                        , "</br><i>App version: Version 1.1.42.95</i>"
                        , "</br><i>Dataset name: opaq_66a.csv</i>"
                        , "</br>- Changed order of contributors to reflect the amount of time invested (e.g., moved Resources & Data-Curation authors up)"
                        , "</br>- Corrected an instance where a contributor was mentioned twice"
                        , "</br>- Added dataset and contributor: Ambrus et al., 2021"
                        , "</br>- Replaced xlsx-package with openxlsx-package due to bugs."
                        
                        , "</br><b>31.03.2022</b>"
                        , "</br><i>App version: Version 1.1.42.94</i>"
                        , "</br><i>Dataset name: opaq_65a.csv</i>"
                        , "</br>- Added contributors"
                        , "</br>- Changed wording of anchoring items for Ioannidis et al., 2020 and Bergman et al. 2010 (personal monetary evaluation instead of price)"
                        , "</br>- Corrected preregistration and -type for Cheek & Norem, 2020"
                        , "</br>- Reversed the order of the changelog entries so that newest entries are on top"
                        
                        , "</br><b>26.03.2022</b>"
                        , "</br><i>App version: Version 1.1.41.94</i>"
                        , "</br><i>Dataset name: opaq_65a.csv</i>"
                        , "</br>- Added information about the table's variables in the Anchoring Effect Sizes description."
                        , "</br>- Added study and contributor"
                        
                        , "</br><b>16.03.2022</b>"
                        , "</br><i>App version: Version 1.1.41.93</i>"
                        , "</br><i>Dataset name: opaq_64a.csv</i>"
                        , "</br>- Added study and contributors: Ioannidis et al., 2020"
                        , "</br>- Added new category (3) to variable incentive (estimate is monetary and coupled to anchor, e.g., WTA with consequences)"
                        , "</br>- Coded Bergman et al. 2010 incentive as 3"
                        , "</br>- Fixed an indexing error in the reliabilities table that prevented the table from being displayed"
                        , "</br>- Increased the height of the reliabilities plot"
                        
                        , "</br><b>15.03.2022</b>"
                        , "</br><i>App version: Version 1.1.40.92</i>"
                        , "</br><i>Dataset name: opaq_63a.csv</i>"
                        , "</br>- Added study and contributor: Imhoff & Barker, 2022, Study 2"
                        
                        , "</br><b>14.03.2022</b>" # NOT YET UPLOADED
                        , "</br><i>App version: Version 1.1.40.91</i>"
                        , "</br><i>Dataset name: opaq_62a.csv</i>"
                        , "</br>- Added new contributors"
                        , "</br>- Changed spelling for some moderators to uppercase (e.g., Type of Anchoring <b>T</b>ask"
                        , "</br>- Added study: Bahník, 2021b; Bahník, 2021 was changed to 2021<b>a</b>" 
                        , "</br>- Bahník, 2021: corrected experiment_type (lab, formerly online)"
                        
                        , "</br><b>10.03.2022</b>"
                        , "</br><i>App version: Version 1.1.39.90</i>"
                        , "</br><i>Dataset name: opaq_61b.csv</i>"
                        , "</br>- Molden, 2020: changed true value to 55 instead of 84 lbs; changed anchoring item to beef instead of meat eaten"
                        , "</br>- Schreiter et al., 2021: changed codings for sex"
                        , "</br>- Bahník, 2021: corrected year (formerly 2020)"
                        , "</br>- Added new variable nationalty (many missings for unpublished datasets)"
                        , "</br>- Added contributors and corrected names of contributors"
                        , "</br>- Moved Call for Data into About tab in the ShinyApp"
                        , "</br>- Added this Changelog to the Summary tab in the ShinyApp"
                        
                        , "</h5><br/><br/>"
                        , sep = ""))





# Read Data ---------------------------------------------------------------



# read file line is in the changelog
cad <- cad[!is.na(cad$estimate), ]
cad <- cad[!is.na(cad$anchoring_item), ]
# cad <- cad[cad$outlier != 1, ] # exclude outliers
cad$reference <- enc2utf8(cad$reference)
cad$reference_short <- enc2utf8(cad$reference_short)
cad$prereg <- ifelse(cad$preregistered == "0", 0, 1)

ri <- openxlsx::read.xlsx("opaq_results.xlsx", sheet = "reliabilities")
ri <- ri[!is.na(as.numeric(ri$estimate)), ]
for (i in 4:8) {
    ri[, i] <- as.numeric(ri[, i])
}

ri[, c("g", "estimate", "adjustment", "absadjustment", "score", "restr_score")] <- round(ri[, c("g", "estimate", "adjustment", "absadjustment", "score", "restr_score")], digits = 3)

ri <- ri[!is.na(ri$n), ]



# Compute Cr. Alpha for 01-score with 10 items
# items <- 10
# ri$Cr.Alpha <- (items*ri$restr_score) / (1+(items-1)*ri$restr_score) # formula taken from: https://de.wikipedia.org/wiki/Cronbachsches_Alpha

# Plot inter-item correlations
riwide <- stats::reshape(ri, direction = "long"
                         , varying = list(c("adjustment", "absadjustment", "score", "restr_score"))
                         , v.names = "r", idvar = "reference", timevar = "sta_score"
)
riwide$sta_score <- recode(riwide$sta_score
                           , "1" = "adjustment"
                           , "2" = "absadjustment"
                           , "3" = "score"
                           , "4" = "restr_score"
)
riwide$reference <- as.factor(riwide$reference)

es <- openxlsx::read.xlsx("opaq_results.xlsx", sheet = "effectsizes")
es <- es[!is.na(es$g), ]
es$g <- round(es$g, digits = 3)
es$glower <- round(es$glower, digits = 3)
es$gupper <- round(es$gupper, digits = 3)
es$item <- (gsub('(.{1,70})(\\s|$)', '\\1\n', es$item))
es$item <- as.factor(es$item)
es <- es[base::order(es$g, decreasing = TRUE), ]

es <- transform(es, variable = reorder(item, -d) )
# es$itemno <- as.numeric(as.factor(es$item))

es$item <- forcats::fct_reorder(.f = es$item, .x = es$g, .fun = mean, .desc = FALSE)
# dataframe$myfactor= fct_reorder(f = dataframe$myfactor,x = dataframe$var2,fun = mean)









# Datatable styling # code from https://rstudio.github.io/DT/010-style.html
brks <- quantile((ri[, 4:8]), probs = seq(.05, .95, .05), na.rm = TRUE)
clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
## datatable(df) %>% formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))










cadfiltered <- cad[cad$outlier == 0, ]
# p0 <- ggplot2::ggplot(cad, aes(x = adjustment, color = reference_short)) + geom_density() + theme_bw() + guides(color = guide_legend(title = "Reference")) + 
#     theme(legend.key.size = unit(.2, 'cm')) + guides(color = guide_legend(ncol = 1)) + labs(color = "") # change legend size here
# p1 <- ggplot2::ggplot(cadfiltered, aes(x = zestimate, color = reference_short)) + geom_density() + theme_bw() + theme(legend.position = "none")
# p2 <- ggplot2::ggplot(cadfiltered, aes(x = zadjustment, color = reference_short)) + geom_density() + theme_bw() + theme(legend.position = "none")
# p3 <- ggplot2::ggplot(cadfiltered, aes(x = zabsadjustment, color = reference_short)) + geom_density() + theme_bw() + theme(legend.position = "none")
# p4 <- ggplot2::ggplot(cadfiltered, aes(x = zscore, color = reference_short)) + geom_density() + theme_bw() + theme(legend.position = "none")
# p5 <- ggplot2::ggplot(cadfiltered, aes(x = restr_score, color = reference_short)) + geom_density() + theme_bw() + theme(legend.position = "none")
# p_legend <- ggpubr::as_ggplot(ggpubr::get_legend(p0))

reliabilities_plotheight <- 800


reliabilities_forestplot <- ggplot2::ggplot(data = riwide, aes(x = r, y = reference, color = sta_score)) + 
    geom_vline(aes(xintercept = 0), color = "grey") + geom_point(size = 2) + 
    theme_bw() + scale_y_discrete(limits = rev(levels(riwide$reference))) + 
    guides(color = guide_legend(title = "")) + ylab("Reference")  +
    scale_color_manual(values=c("dark blue", "light blue", "red", "orange")) +
    guides(col = guide_legend(ncol = 1))




### Large overview table of dataset
cadoverview <- aggregate(id ~ reference_short, data = cad, FUN = "length")
cadoverview$n <- aggregate(participant_id ~ reference_short, data = cad, FUN = function(x) length(unique(x)))$participant_id
cadoverview$items <- aggregate(anchoring_item ~ reference_short, data = cad, FUN = function(x) length(unique(x)))$anchoring_item
names(cadoverview) <- c("Reference", "Trials", "N", "Items")


# Bar plot
cadoverviewperc <- cadoverview
cadoverviewperc$Trials <- cadoverview$Trials / sum(cadoverview$Trials)
cadoverviewperc$Items <- cadoverview$Items / sum(cadoverview$Items)
cadoverviewperc$N <- cadoverview$N / sum(cadoverview$N)
cadoverviewlong <- reshape::melt(cadoverviewperc, id = "Reference")
library(RColorBrewer)
n <- nrow(cadoverviewperc)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# cols <- sample(col_vector, n)
# cols <- sample(colors(), n)
# cols <- sample(grDevices::palette(rainbow(n)), n)
cols <- sample(c(grDevices::palette(rainbow(100)), col_vector), n, replace = TRUE)
overviewplot_plot <- ggplot2::ggplot(data = cadoverviewlong, aes(x = variable, y = value, fill = Reference)) + geom_bar(stat = "identity") +
    theme_bw() + scale_fill_manual(values = cols) + xlab("Variable") + ylab("Percentage") + #coord_flip() + # https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf
    guides(fill = guide_legend(ncol = 1))




## Meta-Analysis
# Assumption: Nhigh = Nlow
es$se <- sqrt(((es$n/(es$n/2)^2) + es$g^2 / (2*es$n)))

reml <- metafor::rma.mv(yi = g
                        , V = se^2 
                        , random = ~1 | reference_short
                        , tdist = TRUE 
                        , data = es
                        , method = "ML") 

nmeta <- sum(aggregate(n ~ reference_short, data = es, FUN = "min")$n, na.rm = TRUE)

metagen_model <- meta::metagen(TE = es$g, seTE = es$se, studlab = es$reference_short)
eggers <- meta::metabias(metagen_model, k.min = 3, method = "linreg")





### Datasets/Literature overview
lit <- openxlsx::read.xlsx("OpAQ_AnchoringDatasets.xlsx")
lit$source <- dplyr::recode(lit$`Source.1.=.literature.search,.2.=.file.drawer,.3.=.direct.communication,.4.=.Call.for.data,.5.=.other,.6.=.literature.alerts."anchoring.effect"`
                           , "1" = "literature search"
                           , "2" = "file drawer"
                           , "3" = "direct communication"
                           , "4" = "call for data"
                           , "5" = "other"
                           , "6" = "literature alerts on AE"
                           , .default = "NA")
lit$opendata <- dplyr::recode(lit$`Data.openly.accessible?.1.=.yes,.0.=.no`
                              , "1" = "yes"
                              , "0" = "no"
                              , .default = "NA")

lit$eligible <- dplyr::recode(lit$`Data.meets.inclusion.criterion.(at.least.2.anchors.+.subsequent.numeric.estimates).1.=.yes,.0.=.no`
                              , "1" = "yes"
                              , "0" = "no"
                              , .default = "NA")

lit$included <- dplyr::recode(lit$`Data.included.in.OpAQ.1.=.yes,.0.=.no`
                              , "1" = "yes"
                              , "0" = "no"
                              , .default = "NA")

lit <- lit[, c("ID", "Reference"
               , "source"
               , "opendata"
               , "eligible"
               , "included"
               # , "Source.1.=.literature.search,.2.=.file.drawer,.3.=.direct.communication,.4.=.Call.for.data,.5.=.other,.6.=.literature.alerts.\"anchoring.effect\""
               # , "Data.openly.accessible?.1.=.yes,.0.=.no"     
               # , "Data.meets.inclusion.criterion.(at.least.2.anchors.+.subsequent.numeric.estimates).1.=.yes,.0.=.no"
               # , "Data.included.in.OpAQ.1.=.yes,.0.=.no"
               , "notes"
               , "date.the.data.was.added"
)]

lit <- lit[!is.na(lit$Reference), ] # remove rows without reference
lit$ID <- 1:nrow(lit) # update rownumber



# # number of datasets yet to be added
# sum(lit$notes == "dataset is pending addition", na.rm = TRUE)
# # number of references included in OpAQ
# sum(lit$`Data.included.in.OpAQ.1.=.yes,.0.=.no` == 1, na.rm = TRUE)




# ABOUT text --------------------------------------------------------------
about <- HTML(paste("<h4><b>OpAQ-App ", version, "</b></h3>"
                    , "<br/><b>Last Update:</b> ", date
                    
                    ### Core team
                    , "<br/><b>Citation:</b> Röseler, L., Weber, L., Helgerth, K. A. C., Stich, E., Günther, M., Tegethoff, P., Wagner, F. S."
                    
                    ### Resources & Data Curation
                    , ", Ambrus, E., Antunovic, M., Barrera, F., Halali, E., Ioannidis, K., McKay, R., Milstein, N., Molden, D. C., Papenmeier, F., Rinn, R., Schreiter, M. L., Zimdahl, M., "
                    
                    ### Resources
                    , "Allen, E., Bahník, S., Baumeister, R. F., Bermeitinger, C., Bickenbach, S. L. C., Blank, P. A., Blower, F. B. N., Bögler, H. L., Boo, F. L., Boruchowicz, C., Bühler, R. L., Burgmer, P.,
                    Cheek, N., N., Dohle, S., Dorsch, L., Dück, M. S., Fels, S.-A., Fischer, A. L., Frech, M.-L., Freira, L., Friedinger, K., Genschow, O.,
                    Harris, A., Hartig, B., Häusser, J. A., Hedgebeth, M., Henkel, M., Horvath, D., Hügel, J. C., Igna, E. L. E., Imhoff, R., Intelmann, P., Karg, A. H., Klamar, A., Klein, C., Klusmann, B., Knappe, E., Köppel, L.-M., Koßmann, L., Kraft, P., Kroworsch, M. K., Krueger, S. M., Kühling, S.,
                    Lagator, S., Lammers, J., Loschelder, D. D., Navajas, J., Norem, J., K., Novak, J.
                    Onuki, Y., Page, E., Panse, F., Pavlovic, Z., Pearton, J., Rebholz, T. R., Rodgers, S., Röseler, J. J., Rostekova, A., Roßmaier, K. V.,
                    Sartorio, M., Scheelje, L., Schindler, S., Schreiner, N. B., Seida, C., Shanks, D. R., Siems, M.-C., Stitz, M., Starkulla, M., Stäglich, M., Thies, K., Thum, E., Undorf, M., Unger, B. D.,
                    Urlichich, D., Vadillo, M. A., Wackershauser-Sablotny, V., Wessel, I., Wolf, H., Zhou, A.,
                    & Schütz, A. (2022). <i>OpAQ: Open Anchoring Quest, ", version, "</i>. <a href=https://dx.doi.org/10.17605/OSF.IO/YGNVB>https://dx.doi.org/10.17605/OSF.IO/YGNVB</a>"
                    , "<br/><b>Data and Materials:</b> <a href=https://osf.io/ygnvb/>https://osf.io/ygnvb/</a>"
                    , "<br/><b>Contribute:</b> Please send an e-mail to lukas.roeseler(at)uni-bamberg.de if you want to contribute an anchoring dataset, an analysis, or process other researchers' data (or see the call for data at the bottom of this page)."
                    , "<br/><b>License:</b> CC-By Attribution 4.0 International"
                    , "<br/><b>Acknowledgements:</b> This work has been supported by the University of Bamberg CatchUP+ program."
                    , "<br/><b>Important note:</b> This is work in progress. Please beware that there might be bugs or errors in the dataset."
                    , sep = ""))

author_contributions <- HTML(paste("<h4><b>Author Contributions (CRediT)</b></h3>" # https://casrai.org/credit/
                    , "<b>Röseler, L.:</b> Conceptualization, Data curation, Formal analysis, Funding acquisition, Investigation, Project administration, Resources, Software, Supervision, Validation, Visualization #, Writing - original draft" # Writing - review & editing
                    , "<br/><b>Weber, L.:</b> Data curation, Investigation, Resources, Software, Visualization" # Writing - review & editing
                    , "<br/><b>Helgerth, K. A: C.:</b> Validation"
                    , "<br/><b>Stich, E.:</b> Data curation, Software "
                    , "<br/><b>Günther, M.:</b> Data curation"
                    , "<br/><b>Tegethoff, P.:</b> Investigation, Data Curation, Resources, Validation"
                    , "<br/><b>Wagner, F. S.:</b> Investigation, Resources"
                    , "<br/><b>Ambrus, E.:</b> Resources, Data Curation"
                    , "<br/><b>Antunovic, M.:</b> Resources, Data Curation"
                    , "<br/><b>Barrera, F.:</b> Resources, Data Curation"
                    , "<br/><b>Halali, E.:</b> Resources, Data curation" 
                    , "<br/><b>Ioannidis, K.:</b> Resources, Data curation"
                    , "<br/><b>McKay, R.:</b> Resources, Data curation" 
                    , "<br/><b>Milstein, N.:</b> Resources, Data curation" 
                    , "<br/><b>Molden, D. C.:</b> Resources, Data curation" 
                    , "<br/><b>Papenmeier, F.:</b> Resources, Data Curation"
                    , "<br/><b>Rinn, R.:</b> Resources, Data curation"
                    , "<br/><b>Schreiter, M. L.:</b> Resources, Data Curation"
                    , "<br/><b>Zimdahl, M.:</b> Resources, Data curation"
                    , "<br/><b>Allen, E.:</b> Resources"
                    , "<br/><b>Bahník, Š.:</b> Resources"
                    , "<br/><b>Baumeister, R. F.:</b> Resources"
                    , "<br/><b>Bermeitinger, C.:</b> Resources"
                    , "<br/><b>Bickenbach, S. L. C.:</b> Resources"
                    , "<br/><b>Blank, P. A.:</b> Resources"
                    , "<br/><b>Blower, F. B. N.:</b> Resources"
                    , "<br/><b>Bögler, H. L.:</b> Resources"
                    , "<br/><b>Boruchowicz, C.:</b> Resources"
                    , "<br/><b>Boo, F. L.:</b> Resources"
                    , "<br/><b>Bühler, R. L.:</b> Resources" 
                    , "<br/><b>Burgmer, P.:</b> Resources" 
                    , "<br/><b>Cheek, N. N.:</b> Resources"
                    , "<br/><b>Dohle, S.:</b> Resources"
                    , "<br/><b>Dorsch, L.:</b> Resources"
                    , "<br/><b>Dück, M. S.:</b> Resources"
                    , "<br/><b>Fels, S.-A.:</b> Resources" 
                    , "<br/><b>Fischer, A. L.:</b> Resources" 
                    , "<br/><b>Frech, M.-L.:</b> Resources" 
                    , "<br/><b>Freira, L.:</b> Resources"
                    , "<br/><b>Friedinger, K.:</b> Resources"
                    , "<br/><b>Genschow, O.:</b> Resources"
                    , "<br/><b>Harris, A.:</b> Resources"
                    , "<br/><b>Hartig, B.:</b> Resources"
                    , "<br/><b>Häusser, J. A.:</b> Resources"
                    , "<br/><b>Hedgebeth, M.:</b> Resources"
                    , "<br/><b>Henkel, M.:</b> Resources"
                    , "<br/><b>Horvath, D.:</b> Resources"
                    , "<br/><b>Hügel, J. C.:</b> Resources"
                    , "<br/><b>Igna, E. L. E.:</b> Resources"
                    , "<br/><b>Imhoff, R.:</b> Resources"
                    , "<br/><b>Intelmann, P.:</b> Resources"
                    , "<br/><b>Karg, A. H.:</b> Resources" 
                    , "<br/><b>Klamar, A.:</b> Resources" 
                    , "<br/><b>Klein, C.:</b> Resources" 
                    , "<br/><b>Klusmann, B.:</b> Resources" 
                    , "<br/><b>Knappe, E.:</b> Resources" 
                    , "<br/><b>Köppel, L.-M.:</b> Resources" 
                    , "<br/><b>Koßmann, L.:</b> Resources" 
                    , "<br/><b>Kraft, P.:</b> Resources" 
                    , "<br/><b>Kroworsch, M. K.:</b> Resources" 
                    , "<br/><b>Krueger, S. M.:</b> Resources" 
                    , "<br/><b>Kühling, S.:</b> Resources" 
                    , "<br/><b>Lagator, S.:</b> Resources" 
                    , "<br/><b>Lammers, J.:</b> Resources" 
                    , "<br/><b>Loschelder, D. D.:</b> Resources" 
                    , "<br/><b>Navajas, J.:</b> Resources"
                    , "<br/><b>Norem, J. K.:</b> Resources" 
                    , "<br/><b>Novak, J.:</b> Resources"
                    , "<br/><b>Onuki, Y.:</b> Resources"
                    , "<br/><b>Page, E.:</b> Resources"
                    , "<br/><b>Panse, F.:</b> Resources"
                    , "<br/><b>Pavlovic, Z.:</b> Resources"
                    , "<br/><b>Pearton, J.:</b> Resources"
                    , "<br/><b>Rebholz, T. R.:</b> Resources"
                    , "<br/><b>Rodgers, S.:</b> Resources"
                    , "<br/><b>Röseler, J. J.:</b> Resources"
                    , "<br/><b>Rostekova, A.:</b> Resources"
                    , "<br/><b>Roßmaier, K. V.:</b> Resources"
                    , "<br/><b>Sartorio, M.:</b> Resources"
                    , "<br/><b>Scheelje, L.:</b> Resources"
                    , "<br/><b>Schindler, S.:</b> Resources"
                    , "<br/><b>Schreiner, N. B.:</b> Resources"
                    , "<br/><b>Seida, C.:</b> Resources"
                    , "<br/><b>Shanks, D. R.:</b> Resources"
                    , "<br/><b>Siems, M.-C.:</b> Resources"
                    , "<br/><b>Stitz, M.:</b> Resources"
                    , "<br/><b>Starkulla, M.:</b> Resources"
                    , "<br/><b>Stäglich, P.:</b> Resources"
                    , "<br/><b>Thies, K.:</b> Resources"
                    , "<br/><b>Thum, E.:</b> Resources"
                    , "<br/><b>Undorf, M.:</b> Resources"
                    , "<br/><b>Unger, B. D.:</b> Resources"
                    , "<br/><b>Urlichich, D.:</b> Resources"
                    , "<br/><b>Vadillo, M. A.:</b> Resources"
                    , "<br/><b>Wackershauser-Sablotny, V.:</b> Resources"
                    , "<br/><b>Wessel, I.:</b> Resources"
                    , "<br/><b>Wolf, H.:</b> Resources"
                    , "<br/><b>Zhou, A.:</b> Resources"
                    , "<br/><b>Schütz, A.:</b> Funding acquisition, Resources, Supervision" # Writing - review & editing
                    , sep = ""))

info <- HTML(paste("<h3>Welcome to the Open Anchoring Quest!"
                    , "<h4><br/><br/><b>What is OpAQ?</b><br/> Research on anchoring effects is currently challenged by the lack of reliability of anchoring scores. That is, susceptibility to anchoring for one item is almost completely uncorrelated with susceptibility to anchoring on any other item. With OpAQ, we try to tackle this problem by meta-analytically investigating different measures of anchoring susceptibility as well as factors influencing their reliability. For this purpose, we have created an open, free, and ever-growing dataset already containing thousands of trials from several studies on anchoring effects – and with your support even more."
                    , "<br/><br/><b>What are your benefits of joining us?</b><br/> You are very welcome to contribute data from your anchoring experiment(s)! In return, (apart from being rewarded by the good feeling of helping research on anchoring effects to improve) we will list you as a co-author of the OpAQ. If you provide us your data, we can convert it and add it to the OpAQ dataset. You can also use our data curation template and convert your data yourself. Accordingly, we will mention your contribution either as “Resources” or additionally as “Data curation”.
                        <br/><br/>Unpublished datasets as well as data from classroom experiments are also highly appreciated."
                    , "<br/><br/><b>What can you do with the OpAQ dataset?</b><br/> The OpAQ dataset can be accessed and downloaded by everyone via our <a href=https://metaanalyses.shinyapps.io/OpAQ/>ShinyApp</a> or our <a href=https://osf.io/ygnvb/>OSF project</a>. Feel free to get in touch with us if you need help using the data!
                        <br/><br/>In our ShinyApp, you can explore the dataset and the dynamic meta-analysis on anchoring effect sizes and the reliabilities of anchoring susceptibility scores. The dataset also provides the opportunity to test additional hypotheses, such as file-drawer-effects, effects of anchor extremeness, or age effects.
                        <br/><br/>In the OSF project, you can find further information and files on our project. There, you also can send us a contribution request to the project."
                    , "<br/><br/><b>What does the OpAQ dataset look like?</b><br/>The OpAQ dataset is an item-based dataset with a row for every single estimation of all the anchoring items it contains. For every estimate, different variables and anchoring scores are specified. We also provide a <a href=https://osf.io/j2xmt/>text file</a> with descriptions of all variables that are included in the dataset.
                        <br/><br/>For questions or comments, please send an e-mail to lukas.roeseler(at)uni-bamberg.de"
                    , "<br/><br/>"
                    , sep = ""))

dataset_info1 <- HTML(paste("<h4><b>OpAQ Dataset Overview</b>"
                           , "<h5><br/>The OpAQ dataset is an ever-growing dataset containing thousands of trials from anchoring experiments. Here is an overview of the current version."
                           , "<br/><br/>"
                           , sep = ""))

dataset_info1b <- HTML(paste("<h5><br/><b>Study-Overview</b>"
                            , "<br/><br/>"
                            , sep = ""))

dataset_info2 <- HTML(paste("<h4><b>OpAQ Dataset</b>"
                           , "<h5><br/>Browse, filter, and download the entire OpAQ dataset here. For variable explanations, please see <a href=https://osf.io/j2xmt/>https://osf.io/j2xmt/</a>"
                           , "<br/><br/>"
                           , sep = ""))

moderators_info <- HTML(paste("<h4><b>OpAQ Moderators of Anchoring Effects</b>"
                              , "<h5><br/>Choose the moderator on the left and explore its relationship with anchoring effect sizes. Please beware that there is collinearity among some of the moderators."
                              , "<br/><br/>"
                              , sep = ""))

reliabilities_info <- HTML(paste("<h4><b>Reliabilities of Susceptibility to Anchoring Scores</b>"
                                 , "<h5><br/>Despite over one decade of personality moderator research, the fact that most anchoring scores are extremely unreliable (e.g., Cronbach's Alphas below .5 in many cases), investigation of factors increasing reliability <a href=https://osf.io/9gju4/>has only just begun</a>). Here you can see an overview of the reliability (i.e., average interitem correlations) of different susceptibility-scores from different studies." # Outliers +/-5SD have been excluded."
                                 , "<br/><br/>"
                                 , sep = ""))

reliabilities_formulaeinfo <- HTML(paste("<h5><br/><b>Definitions of Susceptibility Scores</b>"
                                 , "<br/><br/>"
                                 , sep = ""))

reliabilities_formulae <- data.frame("Score" = c("estimate", "adjustment", "absadjustment", "score", "restr_score", "g")
                                     , "Interpretation" = c("response given to the anchored question"
                                                            , "difference between estimate and anchor"
                                                            , "absolute difference between estimate and anchor"
                                                            , "difference between estimate and anchor divided by difference between true value and anchor"
                                                            , "the above score but with cut-offs at 0 and 1"
                                                            , "average Hedges's g for all anchoring effects in the respective study"
                                                            ))

reliabilities_distinfo <- HTML(paste("<h4><b>Distributions of Susceptibility to Anchoring Scores</b>"
                                 , "<h5><br/>Scores have been standardized by study and by item (except for restr_score) to facilitate comparisons. Outliers +/-5SD have been excluded."
                                 , "<br/><br/>"
                                 , sep = ""))

reliabilities_forestinfo <- HTML(paste("<h4><b>Reliability Scores Plot</b>"
                                     , "<h5><br/>Plot of average inter-item correlations by study and score type.<br/>"
                                     , "<br/><br/>"
                                     , sep = ""))

effectsizes_info <- HTML(paste("<h4><b>Anchoring Effect Sizes</b>"
                               , "<h5><br/>Anchoring effects are said to be among the largest of (social) psychology. However, our initial results indicate that they are extremely heterogeneous. Here you can explore which effects are how large. Click on effects in the table and dots will be highlighted in the forest plot below."
                               , " Dashed lines indicate REML-estimate with 95% confidence intervals. The table includes for each effect the minimum and maximum anchor values (anchorinterval), sample size (n), Hedges's g and the 95% CI for g."
                               , sep = ""))

meta_info <- HTML(paste("<h5><br/><b>Current effect size: </b><i>g</i> = "
                        , ifelse(nchar(round(reml$b, digits = 3)) == 4, paste(round(reml$b, digits = 3), 0, sep = ""), round(reml$b, digits = 3)), ", <i>p</i> ", ifelse(round(reml$pval, digits = 3) == 0, "< .001", paste("=", round(reml$pval, digits = 3)))
                        , ", 95% CI [", round(reml$ci.lb, digits = 3), ", ", round(reml$ci.ub, digits = 3), "]"
                        , ", 95% PI [", round(predict(reml)$pi.lb, digits = 3), ", ", round(predict(reml)$pi.ub, digits = 3), "]"
                        , ", σ² = ", round(reml$sigma2, digits = 3)
                        , ", <i>N<sub>total</sub></i> = ", nmeta
                        , ", <i>k</i> = ", reml$k
                        , "<br/><br/>"
                        , sep = ""))

funnel_info <- HTML(paste("<h4><b>Funnel Plot</b>"
                          , "<h5><br/>Asymmetric distributions of effects can indicate publication bias."
                          , " Grey areas represent effects with <i>p</i> < .100 (light grey), <i>p</i> < .050 (grey), and <i>p</i> < .010 (dark grey). The dashed line represents the mean effect size."
                          , "<br/>Egger's test", ifelse(eggers$pval < .05, " indicates", " does not indicate"), " funnel plot asymmetry, <i>t</i>(", eggers$df, ") = ", round(eggers$statistic, digits = 2), " <i>p</i> = ", round(eggers$p.value, digits = 3), ", 	&tau; = ", round(eggers$tau, digits = 2)
                          , sep = ""))

distance_info <- HTML(paste("<h4><b>Distance Between Anchor and True Value</b>"
                                 , "<h5><br/>It has long been known (e.g., Chapman & Johnson, 1994) that the distance between the anchor and the true value (if there is one) affects the anchoring effect strength. Here you can explore effect size as a function of anchor distance." # Outliers +/-5SD have been excluded."
                                 , "<br/><br/><b>Relative distance of anchor</b> is (anchor - true value) / true value. A value of 0 means the anchor is the true value. A value of 1 means the anchor is one times the distance of anchor and true value above the true value. Values below -1 indicate negative anchors. <br><br><b>Relative adjustment</b> is (estimate - true value) / (anchor - true value). A value of 0 means the estimate is correct, a value of 1 means the estimate is the anchor."
                                 , "<br/><br/>Please note that studies without a true value have no points in the plot below because the distances cannot be computed."
                                 , "<br/><br/>"
                                 , sep = ""))

reference_info <- HTML(paste("<h4><b>References Currently Included in the OpAQ Dataset</b><h5>"
                             , sep = ""))
reference_list <- HTML(paste("<br/><br/>- ", sort(unique(cad$reference)),  sep = ""))

opaq_reference_info_talks <- HTML(paste("<br/><br/><br/><h4><b>Talks about the OpAQ</b><h5>"
                             , sep = ""))
opaq_reference_info_articles <- HTML(paste("<br/><br/><br/><h4><b>References making use of the OpAQ Dataset</b><h5>"
                             , sep = ""))

opaq_reference_list_talks <- HTML(paste(sort(c(
                                      "<br/><br/>-Röseler, L., Weber, L., Stich, E., Günther, M., & Schütz, A. (2022, March). The Open Anchoring Quest (OpAQ): Explaining variance of the heterogeneous but large anchoring effects. Talk (online) at the 64th Conference of Experimental Psychologists, Köln, Germany."
                                      , "<br/><br/>- Röseler, L., Weber, L., Stich, E., Günther, M., & Schütz, A. (2021, September). The Open Anchoring Quest (OpAQ): Tackling the reliability problem and boosting the power of anchoring research. Talk (online) at the Biennial Conference of the German Psychological Society - Personality Psychology and Psychological Diagnostics (DPPD) Section, Ulm, Germany."
                                      , "<br/><br/>- Röseler, L. (2021, March). Are some people more susceptible to anchoring effects than others? Talk (online) at the 63rd Conference of Experimental Psychologists, Ulm, Germany."
                                      ))
                                  ,  sep = ""))

opaq_reference_list_articles <- HTML(paste(sort(c(
                                       "<br/><br/>- Röseler, L., & Schütz, A. (2022, March 9). Hanging the Anchor Off a New Ship: A Meta-Analysis of Anchoring Effects. https://doi.org/10.31234/osf.io/wf2tn"
                                      ,  "<br/><br/>- Röseler, L., Weber, L., Helgerth, K., Stich, E., Günther, M., Tegethoff, P., Wagner, F., Antunovic, M., Barrera- Lemarchand, F., Halali, E., Ioannidis, K., Genschow, O., Milstein, N., Molden, D. C., Papenmeier, F., Pavlovic, Z., Rinn, R., Schreiter, M. L., Zimdahl, M. F., Bahník, Š., Bermeitinger, C., Blower, F. B. N., Bögler, H. L., Burgmer, P., Cheek, N. N., Dorsch, L., Fels, S., Frech, M.-L., Freira, L., Harris, A. J. L., Häusser, J. A., Hedgebeth, M. V., Henkel, M., Horvath, D., Intelmann, P., Klamar, A., Knappe, E., Köppel, L.-M., Krueger, S. M., Lagator, S., Lopez-Boo, F., Navajas, J., Norem, J. K., Novak, J., Onuki, Y., Page, E., Rebholz, T. R., Sartorio, M., Schindler, S., Shanks, D. R., Siems, M.-C., Stäglich, P., Starkulla, M., Stitz, M., Straube, T., Thies, K., Thum, E., Ueda, K., Undorf, M., Urlichich, D., Vadillo, M. A., Wolf, H., Zhou, A., & Schütz, A. (2022). The Open Anchoring Quest Dataset: Anchored Estimates from 96 Studies on Anchoring Effects. Journal of Open Psychology Data, 10: 16, pp. 1–12. DOI: https://doi. org/10.5334/jopd.67"
                                      , "<br/><br/>- Röseler, L., Weber, L., Stich, E., Helgerth, K., Günther, M., Wagner, F.-S., & Schütz, A. (in press). Measurements of Susceptibility to Anchoring are Unreliable: Meta-Analytic Evidence From More Than 50,000 Anchored Estimates. Meta Psychology. https://doi.org/10.31234/osf.io/b6t35"
                                      , "<br/><br/>- Weber, L., & Röseler, L. (2022, August 11). Testing the Reliability of Anchoring Susceptibility Scores. https://doi.org/10.31234/osf.io/2kfh3"
                                      ))
                                  ,  sep = ""))


packages_info <- HTML(paste("<br/><br/><br/><h4><b>R-packages used for this App</b><h5>"
                       , sep = ""))

packages_list <- HTML(paste("<br/><br/>- ", names(sessionInfo()[["otherPkgs"]]),  sep = ""))



callfordata <- HTML(paste("<h4><b>Call for Data: Open Anchoring Quest </b>"
                , "<h5><br/><b>Inclusion criterion:</b> We invite authors to share datasets that include an anchor manipulation and anchored estimates. There are no further inclusion criteria."
                , "<h5><br/><b>Co-Authorship eligibility:</b> Authors providing their raw data are eligible for co-authorship (CRediT: Resources) of the dataset that we plan to publish in a dataset journal. Authors processing their data so that it matches the data format (see the data curation template for datasets in <a href=https://osf.io/mpwfy/>wide</a> or <a href=https://osf.io/tj67c/>long</a> format) are eligible for co-authorship (CrediT: Resources and Data curation)"
                , "<h5><br/><b>Variables in the processed dataset:</b> An overview of the variables in the dataset is available at <a href=https://osf.io/mdgze/>https://osf.io/mdgze/</a>. Crucial variables are <i>reference, anchoring_item, anchor,</i> and <i>estimate</i>. If the dataset is as yet unpublished, we recommend creating an OSF-project that includes the data and a brief description. The OSF will provide you with a reference and even allow you to create a DOI."
                , "<br/><br/>"
                , sep = ""))



literature <- HTML(paste("<h4><b>Anchoring Literature</b>"
                          , "<h5><br/>This is an overview of research on anchoring."
                          , "<br/><br>Currently, we have ", sum(lit$notes == "dataset is pending addition", na.rm = TRUE)
                         , " articles with eligible and available data pending addition to the OpAQ dataset."
                          , "<br/><br/>Write us an e-mail or tweet @aufdroeseler if we missed something or if you want to join us by providing data or by adding yours or somebody elses data!"
                          , "<br/><br/>Variables: You can see whether at least one dataset for the corresponding reference is openly available (opendata), whether the dataset meets the OpAQ inclusion criteria (i.e., at least two different anchors with subsequent numeric estimates; eligible), and whether it is already included here (included)."
                          , sep = ""))

# # number of datasets yet to be added
# 
# # number of references included in OpAQ
# sum(lit$`Data.included.in.OpAQ.1.=.yes,.0.=.no` == 1, na.rm = TRUE)





breaks <- HTML(paste("<br/><br/>",  sep = ""))

ui <- fluidPage(

    # Choose study to explore 
    navbarPage(title = "Open Anchoring Quest"
                        , tabPanel(img(src = "opaq.png", height = 92/4, width = 266/4), fluidRow(
                            column(6, info)
                            ))
                        # , tabPanel("Call for Data", fluidRow(column(6, callfordata)))
                        , tabPanel("Summary", dataset_info1
                                   , withSpinner(shiny::tableOutput("overview")), dataset_info1b
                                   , fluidRow(
                                       column(4
                                       
                                       , withSpinner(shiny::tableOutput("overview2")))
                                   , column(8, withSpinner(plotly::plotlyOutput("overviewplot")))
                                   ) 
                                   )

               , tabPanel("Dataset and Changelog"
                          , dataset_info2
                          , p(class = 'text-left', downloadButton('caddownload', 'Download Dataset (~40MB)'))
                          , withSpinner(DT::DTOutput("dataset"))
                          , changelog
               )
                        
               , tabPanel("Anchoring Effect Sizes",
                       fluidRow(effectsizes_info, meta_info)
                       , fluidRow(
                            column(6, withSpinner(DT::DTOutput("effectsizes")))
                           , column(6, funnel_info, withSpinner(shiny::plotOutput("funnelplot")))
                       )
                       , fluidRow(plotly::plotlyOutput("forest"))
                       ) 
               , tabPanel("Moderators"
                          , moderators_info
                          , shiny::selectInput("moderator", label = "Moderators"
                                               , choices = list(
                                                   "Published" = "published"
                                                                  , "Pre-Registered" = "prereg"
                                                                  , "Year of Publication" = "yearofpublication"
                                                                  , "Age" = "mean_age"
                                                                  , "Gender" = "prop_female"
                                                                  , "Nationality" = "nationality"
                                                                  , "Known direction of adjustment" = "direction"
                                                                  , "Monetary incentive" = "incentive"
                                                                  , "Comparative Question" = "comparative_question"
                                                                  , "Mean Relative Anchor Distance" = "anchordistance"
                                                                  , "Type of Experiment" = "experiment_type"
                                                                  , "Type of Stimulus" = "stimulitype"
                                                                  # , "Sample type" = "sampletype" # currently only 1 group
                                                                  # , "Magnitude (log of true value)" = "true_value_log"
                                                                  , "Type of Anchor" = "anchortype"
                                                                  , "Type of Anchor Manipulation" = "anchormanipulation"
                                                                  , "Type of Anchoring Task" = "tasktype"
                                                                  , "Type of Response Scale" = "scaletype"
                                                                  , "Type of Preregistration" = "preregtype"
                                                                  # , "Precision of anchors" = "precise" # current method does not make sense (anchors close to true values tend to be more precise)
                                                                  ))
                          , column(4, DT::DTOutput("flexiblemodtable"), shiny::htmlOutput("flexiblemoderatortext"))
                          , column(7, withSpinner(plotly::plotlyOutput("flexibleplot")))
                          )
                                   # , column(7, shiny::verbatimTextOutput("flexiblemoderatormodel"))
               , tabPanel("Reliabilities"
                          , reliabilities_info
                          , withSpinner(DT::DTOutput("reliabilities"))
                          , reliabilities_formulaeinfo
                          , DT::DTOutput("reliabilities_formulae")
                          # , fluidRow(reliabilities_distinfo, withSpinner(plotly::plotlyOutput("reliabilities_distributions")))
                          , fluidRow(reliabilities_forestinfo, withSpinner(plotly::plotlyOutput("reliabilities_forest")))
                            
                          )
               , tabPanel("Anchor Extremeness"
                        , distance_info
                        , withSpinner(plotly::plotlyOutput("distance"))) # XXX
               # distance_by_study <- distance + geom_point(aes(color = reference_short)) + facet_wrap( ~ reference_short)
               
               , tabPanel("Anchoring Literature"
                          , literature
                          , withSpinner(DT::DTOutput("literaturetable"))
               )
               
               , tabPanel("References" 
                          , opaq_reference_info_articles, opaq_reference_list_articles
                          , breaks, opaq_reference_info_talks, opaq_reference_list_talks
                          , breaks, reference_info, reference_list, breaks
                          , packages_info, packages_list
                          
               )
               , tabPanel("About", about, breaks
                          , author_contributions, breaks
                          , fluidRow(column(6, callfordata))
                          , img(src = "logos.png", height = 125) # , width = 500
                          , img(src = "opaq.png", height = 125)
               
               , tags$style(HTML("
                                .navbar-default .navbar-brand {color:black;}
        .navbar-default .navbar-brand:hover {color:black;}
        .navbar { background-color:#EAEAEA;}
        .navbar-default .navbar-nav > li > a {color: dark grey;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color:black;background-color:#3399FF;}
        .navbar-default .navbar-nav > li > a:hover {color:black;background-color:#A6A6A6;text-decoration}
                               "))
               
               
               
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

# Overview Table ----------------------------------------------------------

    
    output$overview <- shiny::renderTable({
        overview <- data.frame("References" = length(unique(cad$reference))
            , "Studies" = length(unique(cad$reference_short))
            , "N" = sum(aggregate(participant_id ~ reference_short, data = cad, FUN = function(x) length(unique(x)))$participant_id)
            , "Items" = length(unique(cad$anchoring_item))
            , "Trials" = length(cad$id)
            # , "Published.Studies" = round(mean(aggregate(data = cad, published ~ reference_short, FUN = "mean")$published), digits = 2)
            # , "Published.Trials" = round(mean(cad$published), digits = 2)
        )
        
        print(overview)
    })
    
    output$overview2 <- shiny::renderTable({
        
        print(cadoverview[, c("Reference", "N", "Items", "Trials")])
        
        
        
    })
    

# Overview Plot -----------------------------------------------------------

    
    output$overviewplot <- plotly::renderPlotly({
        overviewplotly <- plotly::ggplotly(overviewplot_plot) %>%
            config(displayModeBar = FALSE) %>% layout(height = 800, width = 900) %>% 
            layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    }) # , height = 800
    
    output$dataset <- DT::renderDT(DT::datatable(cad[, c("id", "reference_short", "participant_id", "sex", "age", "anchoring_item"
                                           , "true_value", "anchor", "anchorhigh", "estimate", "tasktype" ,"direction"
                                           , "incentive", "comparative_question", "experiment_type", "stimulitype", "sampletype"
                                           , "scaletype", "anchortype", "adjustment", "absadjustment", "score", "restr_score", "link", "preregistered")]
                                         , rownames = FALSE))
    
    output$caddownload <- downloadHandler(
        filename = function() {
            paste("OpAQ-", Sys.Date(), ".csv", sep="")
        },
        content = function(con) {
            write.csv(cad, con, fileEncoding = "WINDOWS-1252") # XXX nochmal prüfen
        }
    )
    
    

# Moderator Plot ----------------------------------------------------------


    
    # output$flexibleplot <- shiny::renderPlot({
    output$flexibleplot <- plotly::renderPlotly({
        # flexible plot
        mod <- es[, input$moderator]
        es$mod <- es[, input$moderator]
        
        if (is.numeric(mod)) {
            p <- ggplot2::ggplot(data = es, aes(y = g, x = mod, color = reference_short)) + geom_point() + theme_bw() + xlab(input$moderator) + ylab("Hedges's g of Anchoring Effects") +
                geom_smooth(data = es, aes(y = g, x = mod, color = NULL), formula = y ~ x) + labs(color = "Reference") + #  , method = "lm", family = (gaussian(link = "log"))
                geom_hline(yintercept = 0, linetype = "dashed")
        } else {
            p <- ggplot2::ggplot(data = es, aes(y = g, x = fct_rev(mod))) + geom_violin(fill = NA) +  # geom_boxplot(width = .1) + 
                theme_bw() +  # stat_summary(fun.y = mean, geom = "point", shape = 12, size = 7, color = "black", fill = "black") +
                geom_jitter(data = es, aes(y = g, x = mod, color = reference_short), width = .1) + labs(color = "Reference") +
                xlab(input$moderator) + ylab("Hedges's g")  +  # (levels())
                geom_hline(yintercept = 0, linetype = "dashed") + # scale_x_discrete(limits = rev(unique(levels(mod)))) +
                coord_flip() 
        }
        plotly::ggplotly(p) %>%
                    config(displayModeBar = FALSE) %>% # layout(height = 800, width = 900) %>%
                    layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    })
    

# Moderator Model ---------------------------------------------------------

    
    output$flexiblemoderatormodel <- shiny::renderPrint({
        
        es$mod <- es[, input$moderator]
        model <- metafor::rma.mv(yi = g
                        , V = se^2
                        , random = ~1 | reference_short
                        , tdist = TRUE
                        , data = es
                        , mods = ~ mod - 1
                        , method = "ML")
        
        
        print(model)
        
    })

# Moderator Text ----------------------------------------------------------

    
    output$flexiblemoderatortext <- shiny::renderText({
        
        es$mod <- es[, input$moderator]
        model <- metafor::rma.mv(yi = g
                               , V = se^2
                               , random = ~1 | reference_short
                               , tdist = TRUE
                               , data = es
                               , mods = ~ mod
                               , method = "ML")
        
        
        HTML(paste("<br><br>The effect of ", "<b>", input$moderator, "</b>", " on anchoring effect sizes is ", ifelse(model[["QMp"]] < .05, "", "<b>not</b> ")
              , "significant at the 5% level. Test of moderators: <i>F</i>(", model[["QMdf"]][1], ", ", model[["QMdf"]][2]
              , ") = ", round(model[["QM"]], digits = 2), ", <i>p</i> "
              , ifelse(round(model[["QMp"]], digits = 3) == 0, "< .001", paste("=", round(model[["QMp"]], digits = 3)))
              , "."
              , sep = ""))
        
    })

# Moderator Table ---------------------------------------------------------

    
    output$flexiblemodtable <- DT::renderDT({
        
        mod <- es[, input$moderator]
        es$mod <- es[, input$moderator]
        model <- metafor::rma.mv(yi = g
                                 , V = se^2
                                 , random = ~1 | reference_short
                                 , tdist = TRUE
                                 , data = es
                                 , mods = ~ mod - 1
                                 , method = "ML")
        
        if (is.numeric(mod)) {
            
            modtable <- psych::describe(es$mod, fast = TRUE) #[c(2:5, 8, 9)]
            rownames(modtable) <- substring(input$moderator, first = 4)
            modtable[, 2:6] <- round(as.data.frame(modtable)[, 2:6], digits = 2)
            modelbeta <- metafor::rma.mv(yi = g
                                     , V = se^2
                                     , random = ~1 | reference_short
                                     , tdist = TRUE
                                     , data = es
                                     , mods = ~ mod
                                     , method = "ML")
            modtable$beta <- round(modelbeta$b[2], digits = 2)
            modtable$vars <- NULL
            modtable$range <- NULL
            modtable$se <- NULL
        } else {
            modtable <- data.frame("Moderator_Levels" = substring(rownames(model$b), first = 4)
                                   , "g" =        round(model$b, digits = 3)
                                   , "ci_lower" = round(model$ci.lb, digits = 3)
                                   , "ci_upper" = round(model$ci.ub, digits = 3)
                                   , "k" = as.numeric(paste(table(es$mod)))
            )
        }
        
        DT::datatable(modtable, options = list(options = list(pageLength = 200, dom = 't'))
                      , rownames = FALSE)
    })
    
    

# Reliability Table -------------------------------------------------------

    
    output$reliabilities <- DT::renderDT(DT::datatable(ri[, c("reference", "items", "n", "estimate", "adjustment"
                                                              , "absadjustment", "score", "restr_score", "g", "itemtypelist")], options = list(pageLength = 200, dom = 't'), autoHideNavigation = TRUE, rownames = FALSE) 
                                         %>% DT::formatStyle(names(c("adjustment", "absadjustment", "score", "restr_score", "g")), backgroundColor = styleInterval(brks, clrs))
                                         )
    
    output$reliabilities_formulae <- DT::renderDT(DT::datatable(reliabilities_formulae, options = list(pageLength = 10, dom = 't'), autoHideNavigation = TRUE, rownames = FALSE) 
    )
    

# Reliabilities Distributions ---------------------------------------------

    output$reliabilities_distributions <- plotly::renderPlotly({
        # gridExtra::grid.arrange(p1, p2, p3, p4, p5, p_legend, nrow = 2) # XXX SCHÖN MACHEN
        subplot(p1, p2, p3, p4, p5, p_legend, nrows = 1)  %>%
            config(displayModeBar = FALSE) %>% layout(height = reliabilities_plotheight) %>%  # , width = 900
            layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    })# , height = reliabilities_plotheight)



    output$reliabilities_forest <- plotly::renderPlotly({
        rf <- ggplotly(reliabilities_forestplot)  %>%
            config(displayModeBar = FALSE) %>% layout(height = reliabilities_plotheight) %>%  # , width = 900
            layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
        rf
    }) #, height = reliabilities_plotheight)
    

# Effectsize Table --------------------------------------------------------

    
    output$effectsizes <- DT::renderDT(DT::datatable(es[, c("reference_short", "item", "anchorinterval", "n", "g", "glower", "gupper"), ], options = list(pageLength = 10), rownames = FALSE)
    )
    

# Effectsize Forestplot ---------------------------------------------------

    
    output$forest <- plotly::renderPlotly({
        s1 <- input$effectsizes_rows_current  # rows on the current page
        s2 <- input$effectsizes_rows_all      # rows on all pages (after being filtered)
        s3 <- input$effectsizes_rows_selected # selected rows
        
        # add line breaks (max char per line = 90)
        # es$item <- (gsub('(.{1,90})(\\s|$)', '\\1\n', es$item))
        # es$item <- (ifelse(nchar(es$item) > 80, paste(substr(es$item, 1, 80), "..."), es$item))
        
        forestplot <- ggplot2::ggplot(data = es, aes(x = g, y = item, col = reference_short)) + geom_vline(xintercept = 0, col = "dark grey", lwd = 1) +
            theme_classic() + geom_vline(xintercept = seq(-5, 5, 1.0), col = rgb(0,0,0,.05), lwd = 0.5, lty = 1) +
            theme(text = element_text(size = 14)) + 
            xlim(c(floor(min(es$glower, na.rm = TRUE)), ceiling(max(es$gupper, na.rm = TRUE)))) +
        
            # REML estimates
            geom_vline(xintercept = c(reml$ci.ub, reml$ci.lb), color = "dark blue", lty = 2, lwd = .3) +
            geom_vline(xintercept = reml$b, color = "dark blue", lty = 2, lwd = .5) +
            # geom_area(aes(x = ifelse(x > reml$ci.lb & x < reml$ci.lb, x, 0)), fill = "grey") +
            
            # Effects
            geom_point() + xlab("Hedges's g") +
            geom_errorbar(aes(xmin = es$glower, xmax = es$gupper)) +
            
            # Legend
            theme(legend.position="none") + # remove legend
            # theme(legend.title = element_blank()) + guides(col = guide_legend(ncol = 1)) +
            
            # Highlighted effects
            geom_point(data = es[s1, ], mapping = aes(x = g, y = item), shape = 21, size = 2, color = "black") +
            geom_point(data = es[s3, ], mapping = aes(x = g, y = item), shape = 10, size = 4, color = "dark red") +
            
            # Title
            ggtitle("Forest plot of anchoring effects")
        
        p <- ggplotly(forestplot) %>%
            config(displayModeBar = FALSE) %>% layout(height = 10000, width = 1200) %>% 
            layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
        p
        
    }
    )
    

# Effectsize Funnelplot ---------------------------------------------------

    
    output$funnelplot <- shiny::renderPlot({
        
        es$published <- as.factor(es$published)
        
        s1 <- input$effectsizes_rows_current  # rows on the current page
        s2 <- input$effectsizes_rows_all      # rows on all pages (after being filtered)
        s3 <- input$effectsizes_rows_selected # selected rows
        
        # Funnel plot -------------------------------------------------------------
        triangle90 <- data.frame("x" = c(qnorm(.050), 0, qnorm(.950)), "y" = c(Inf, 0, Inf))
        triangle95 <- data.frame("x" = c(qnorm(.025), 0, qnorm(.975)), "y" = c(Inf, 0, Inf))
        triangle99 <- data.frame("x" = c(qnorm(.005), 0, qnorm(.995)), "y" = c(Inf, 0, Inf))
        funnel <- ggplot2::ggplot(es, aes(x = g, y = se, color = reference_short)) + 
            geom_polygon(data = triangle90, mapping = aes(x = x, y = y, color = "x"), fill = rgb(.5,.5,.5,.15), color = "white") +
            geom_polygon(data = triangle95, mapping = aes(x = x, y = y, color = "x"), fill = rgb(.5,.5,.5,.15), color = "white") +
            geom_polygon(data = triangle99, mapping = aes(x = x, y = y, color = "x"), fill = rgb(.5,.5,.5,.15), color = "white") +
            # geom_point(aes(shape = published), size = 4, alpha = 0.5) +  scale_shape_manual(values=c(18, 19, 20)) +
            geom_point(aes(shape = published), size = 4, alpha = 0.5) +  scale_shape_manual(values=c(20, 18, 19)) +
            geom_point(data = es[s1, ], mapping = aes(x = g, y = se), shape = 21, size = 4, color = "black") +
            geom_point(data = es[s3, ], mapping = aes(x = g, y = se), shape = 10, size = 6, color = "dark red") +
            theme_bw() + xlab("Hedges's g") + ylab("Standard Error") + scale_y_reverse() + # #lim=c(10,0)
            guides(color = "none") +
            geom_vline(xintercept = reml$b, lty = 2) +
            geom_vline(xintercept = 0, lty = 1, color = "dark grey")
        
        funnel
        
    })
    


# Literature Overview -----------------------------------------------------

    output$literaturetable <- DT::renderDT(DT::datatable(lit
                                                 , rownames = FALSE))
    
    
# Distance Plot -----------------------------------------------------------

    
    output$distance <- plotly::renderPlotly({
        cad$x <- (cad$anchor - cad$true_value) / cad$true_value
        cad$y <- (cad$estimate - cad$true_value) / (cad$anchor - cad$true_value)
        relative_adjustment <- ggplot(cad, aes(x = x
                                               , y = y
                                               , color = reference_short)) +
            geom_point() + # size = 1
            ylim(0, 10) + xlim(-1, 2) +
            xlab("Relative Distance of Anchor") + ylab("Relative Adjustment") + theme_bw() + labs(color = "") +
            # theme(text = element_text(size = 6)) +
            guides(col = guide_legend(ncol = 1))
        dist <- plotly::ggplotly(relative_adjustment) %>% config(displayModeBar = FALSE) %>% layout(height = 800, width = 900) %>% 
            layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
        dist
    })
    
    # output$downloadPlot <- downloadHandler(
    #     filename = function(){paste("opaq_ae_forestplot",'.png',sep='')},
    #     content = function(file){
    #         ggsave(file,plot=data$plot))

}

# Run the application 
shinyApp(ui = ui, server = server)
