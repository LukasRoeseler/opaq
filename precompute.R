suppressMessages({
  library(openxlsx)
  library(dplyr)
  library(forcats)
  library(metafor)
  library(meta)
  library(jsonlite)
})

setwd("C:/Users/lroesele.IVV5NET/AppData/Local/Temp/opencode/opaq/Datasets")
outdir <- "C:/Users/lroesele.IVV5NET/AppData/Local/Temp/opencode/opaq/data"
dir.create(outdir, showWarnings = FALSE)

# ---- cad (main dataset) ----
cad <- read.csv("opaq_70a.csv", sep = ",", dec = ".", fileEncoding = "UTF-8")
cad <- cad[!is.na(cad$estimate), ]
cad <- cad[!is.na(cad$anchoring_item), ]
cad$reference <- enc2utf8(cad$reference)
cad$reference_short <- enc2utf8(cad$reference_short)
cad$prereg <- ifelse(cad$preregistered == "0", 0, 1)

# ---- overview ----
overview <- data.frame(
  "References" = length(unique(cad$reference)),
  "Studies" = length(unique(cad$reference_short)),
  "N" = sum(aggregate(participant_id ~ reference_short, data = cad, FUN = function(x) length(unique(x)))$participant_id),
  "Items" = length(unique(cad$anchoring_item)),
  "Trials" = length(cad$id)
)
write_json(list(labels = names(overview), values = as.numeric(overview[1, ])),
           path = file.path(outdir, "overview.json"), pretty = FALSE)

# ---- cadoverview (per-study) ----
cadoverview <- aggregate(id ~ reference_short, data = cad, FUN = "length")
cadoverview$n <- aggregate(participant_id ~ reference_short, data = cad, FUN = function(x) length(unique(x)))$participant_id
cadoverview$items <- aggregate(anchoring_item ~ reference_short, data = cad, FUN = function(x) length(unique(x)))$anchoring_item
names(cadoverview) <- c("Reference", "Trials", "N", "Items")
write_json(cadoverview, path = file.path(outdir, "cadoverview.json"), pretty = FALSE)

# ---- ri (reliabilities) ----
ri <- openxlsx::read.xlsx("opaq_results.xlsx", sheet = "reliabilities")
ri <- ri[!is.na(as.numeric(ri$estimate)), ]
for (i in 4:8) { ri[, i] <- as.numeric(ri[, i]) }
ri[, c("g", "estimate", "adjustment", "absadjustment", "score", "restr_score")] <-
  round(ri[, c("g", "estimate", "adjustment", "absadjustment", "score", "restr_score")], digits = 3)
ri <- ri[!is.na(ri$n), ]
ri_table <- ri[, c("reference", "items", "n", "estimate", "adjustment", "absadjustment", "score", "restr_score", "g", "itemtypelist")]
names(ri_table) <- c("reference", "items", "n", "estimate", "adjustment", "absadjustment", "score", "restr_score", "g", "itemtypelist")
write_json(ri_table, path = file.path(outdir, "reliabilities.json"), pretty = FALSE)

reliabilities_formulae <- data.frame(
  "Score" = c("estimate", "adjustment", "absadjustment", "score", "restr_score", "g"),
  "Interpretation" = c("response given to the anchored question",
                       "difference between estimate and anchor",
                       "absolute difference between estimate and anchor",
                       "difference between estimate and anchor divided by difference between true value and anchor",
                       "the above score but with cut-offs at 0 and 1",
                       "average Hedges's g for all anchoring effects in the respective study")
)
write_json(reliabilities_formulae, path = file.path(outdir, "reliabilities_formulae.json"), pretty = FALSE)

# ---- riwide (forest) ----
riwide <- stats::reshape(ri, direction = "long",
                         varying = list(c("adjustment", "absadjustment", "score", "restr_score")),
                         v.names = "r", idvar = "reference", timevar = "sta_score")
riwide$sta_score <- dplyr::recode(riwide$sta_score,
                                  "1" = "adjustment", "2" = "absadjustment", "3" = "score", "4" = "restr_score")
riwide$reference <- as.factor(riwide$reference)
riwide_out <- data.frame(reference = as.character(riwide$reference),
                         score = riwide$sta_score,
                         r = riwide$r)
write_json(riwide_out, path = file.path(outdir, "riwide.json"), pretty = FALSE)

# ---- es (effectsizes) ----
es <- openxlsx::read.xlsx("opaq_results.xlsx", sheet = "effectsizes")
es <- es[!is.na(es$g), ]
es$g <- round(es$g, digits = 3)
es$glower <- round(es$glower, digits = 3)
es$gupper <- round(es$gupper, digits = 3)
es$item <- (gsub('(.{1,70})(\\s|$)', '\\1\n', es$item))
es$item <- as.factor(es$item)
es <- es[base::order(es$g, decreasing = TRUE), ]
es <- transform(es, variable = reorder(item, -d))
es$item <- forcats::fct_reorder(.f = es$item, .x = es$g, .fun = mean, .desc = FALSE)

es$se <- sqrt(((es$n / (es$n / 2)^2) + es$g^2 / (2 * es$n)))

reml <- metafor::rma.mv(yi = g, V = se^2, random = ~1 | reference_short,
                        tdist = TRUE, data = es, method = "ML")
nmeta <- sum(aggregate(n ~ reference_short, data = es, FUN = "min")$n, na.rm = TRUE)

metagen_model <- meta::metagen(TE = es$g, seTE = es$se, studlab = es$reference_short)
eggers <- meta::metabias(metagen_model, k.min = 3, method = "linreg")

meta_out <- list(
  b = as.numeric(reml$b),
  pval = as.numeric(reml$pval),
  ci.lb = as.numeric(reml$ci.lb),
  ci.ub = as.numeric(reml$ci.ub),
  pi.lb = as.numeric(predict(reml)$pi.lb),
  pi.ub = as.numeric(predict(reml)$pi.ub),
  sigma2 = as.numeric(reml$sigma2),
  k = reml$k,
  nmeta = nmeta,
  eggers = list(statistic = as.numeric(eggers$statistic),
                df = as.numeric(eggers$df),
                p.value = as.numeric(eggers$p.value),
                tau = as.numeric(eggers$tau))
)
write_json(meta_out, path = file.path(outdir, "meta.json"), auto_unbox = TRUE, pretty = FALSE)

# ---- es export for plots ----
es_cols <- c("reference_short", "item", "anchorinterval", "n", "g", "glower", "gupper",
             "published", "prereg", "yearofpublication", "mean_age", "prop_female",
             "nationality", "direction", "incentive", "comparative_question",
             "anchordistance", "experiment_type", "stimulitype", "anchortype",
             "anchormanipulation", "tasktype", "scaletype", "preregtype", "se")
es_export <- es[, es_cols]
es_export$item <- gsub("\n", " ", es_export$item)
write_json(es_export, path = file.path(outdir, "effectsizes.json"), pretty = FALSE)

# ---- moderators ----
moderators <- c("published", "prereg", "yearofpublication", "mean_age", "prop_female",
                "nationality", "direction", "incentive", "comparative_question",
                "anchordistance", "experiment_type", "stimulitype", "anchortype",
                "anchormanipulation", "tasktype", "scaletype", "preregtype")
numeric_mods <- c("yearofpublication", "mean_age", "prop_female", "anchordistance")

mods_out <- list()
for (m in moderators) {
  es$mod <- es[[m]]
  mod <- es[[m]]
  is_num <- m %in% numeric_mods

  model <- metafor::rma.mv(yi = g, V = se^2, random = ~1 | reference_short,
                           tdist = TRUE, data = es, mods = ~ mod, method = "ML")

  # overall test of moderators
  test <- list(QM = as.numeric(model$QM), QMdf = as.numeric(model$QMdf),
               QMp = as.numeric(model$QMp))

  if (is_num) {
    # descriptive stats (mirror psych::describe fast)
    d <- data.frame(v = as.numeric(es$mod))
    d <- d[!is.na(d$v), , drop = FALSE]
    desc <- data.frame(
      n = nrow(d),
      mean = round(mean(d$v), 2),
      sd = round(sd(d$v), 2),
      median = round(median(d$v), 2),
      min = round(min(d$v), 2),
      max = round(max(d$v), 2)
    )
    modelbeta <- metafor::rma.mv(yi = g, V = se^2, random = ~1 | reference_short,
                                 tdist = TRUE, data = es, mods = ~ mod, method = "ML")
    desc$beta <- round(as.numeric(modelbeta$b[2]), 2)
    table <- desc

    # loess smooth curve for plot
    ok <- !is.na(es$g) & !is.na(es$mod)
    ld <- data.frame(y = es$g[ok], mod = es$mod[ok])
    lo <- loess(y ~ mod, data = ld, span = 0.75)
    xr <- seq(min(ld$mod, na.rm = TRUE), max(ld$mod, na.rm = TRUE), length.out = 60)
    ypred <- predict(lo, newdata = data.frame(mod = xr))
    smooth <- data.frame(x = xr, y = as.numeric(ypred))
  } else {
    model2 <- metafor::rma.mv(yi = g, V = se^2, random = ~1 | reference_short,
                              tdist = TRUE, data = es, mods = ~ mod - 1, method = "ML")
    tbl <- data.frame(
      Moderator_Levels = substring(rownames(model2$b), first = 4),
      g = round(as.numeric(model2$b), digits = 3),
      ci_lower = round(as.numeric(model2$ci.lb), digits = 3),
      ci_upper = round(as.numeric(model2$ci.ub), digits = 3),
      k = as.numeric(paste(table(es$mod)))
    )
    table <- tbl
    smooth <- NULL
  }

  mods_out[[m]] <- list(
    type = ifelse(is_num, "numeric", "categorical"),
    test = test,
    table = table,
    smooth = smooth
  )
}
write_json(mods_out, path = file.path(outdir, "moderators.json"), auto_unbox = TRUE, pretty = FALSE)

# ---- literature ----
lit <- openxlsx::read.xlsx("OpAQ_AnchoringDatasets.xlsx")
lit$source <- dplyr::recode(lit$`Source.1.=.literature.search,.2.=.file.drawer,.3.=.direct.communication,.4.=.Call.for.data,.5.=.other,.6.=.literature.alerts."anchoring.effect"`,
                            "1" = "literature search", "2" = "file drawer", "3" = "direct communication",
                            "4" = "call for data", "5" = "other", "6" = "literature alerts on AE", .default = "NA")
lit$opendata <- dplyr::recode(lit$`Data.openly.accessible?.1.=.yes,.0.=.no`, "1" = "yes", "0" = "no", .default = "NA")
lit$eligible <- dplyr::recode(lit$`Data.meets.inclusion.criterion.(at.least.2.anchors.+.subsequent.numeric.estimates).1.=.yes,.0.=.no`,
                              "1" = "yes", "0" = "no", .default = "NA")
lit$included <- dplyr::recode(lit$`Data.included.in.OpAQ.1.=.yes,.0.=.no`, "1" = "yes", "0" = "no", .default = "NA")
lit <- lit[, c("ID", "Reference", "source", "opendata", "eligible", "included", "notes", "date.the.data.was.added")]
lit <- lit[!is.na(lit$Reference), ]
lit$ID <- 1:nrow(lit)
names(lit) <- c("ID", "Reference", "Source", "Open data", "Eligible", "Included", "Notes", "Date added")
write_json(lit, path = file.path(outdir, "literature.json"), pretty = FALSE)

# ---- distance scatter ----
cad$x <- (cad$anchor - cad$true_value) / cad$true_value
cad$y <- (cad$estimate - cad$true_value) / (cad$anchor - cad$true_value)
d <- data.frame(x = round(cad$x, 4), y = round(cad$y, 4), ref = cad$reference_short)
d <- d[!is.na(d$x) & !is.na(d$y), ]
write_json(d, path = file.path(outdir, "distance.json"), pretty = FALSE)

# ---- dataset table (properly typed JSON) ----
dcols <- c("id", "reference_short", "participant_id", "sex", "age", "anchoring_item",
           "true_value", "anchor", "anchorhigh", "estimate", "tasktype", "direction",
           "incentive", "comparative_question", "experiment_type", "stimulitype", "sampletype",
           "scaletype", "anchortype", "adjustment", "absadjustment", "score", "restr_score",
           "link", "preregistered")
dtable <- cad[, dcols]
# keep id/participant_id as character (they are identifiers)
dtable$id <- as.character(dtable$id)
dtable$participant_id <- as.character(dtable$participant_id)
# ensure numeric columns are numeric and rounded sensibly
for (cc in c("true_value", "anchor", "anchorhigh", "estimate", "anchortype",
             "adjustment", "absadjustment", "score", "restr_score")) {
  dtable[[cc]] <- as.numeric(dtable[[cc]])
}
dtable$true_value <- round(dtable$true_value, 2)
dtable$anchor <- round(dtable$anchor, 2)
dtable$estimate <- round(dtable$estimate, 2)
dtable$adjustment <- round(dtable$adjustment, 2)
dtable$absadjustment <- round(dtable$absadjustment, 2)
dtable$score <- round(dtable$score, 3)
dtable$restr_score <- round(dtable$restr_score, 3)
dtable$anchorhigh <- as.integer(dtable$anchorhigh)
dtable$anchortype <- as.integer(dtable$anchortype)
# factors to strings
for (cc in names(dtable)) if (is.factor(dtable[[cc]])) dtable[[cc]] <- as.character(dtable[[cc]])
dtable_json <- jsonlite::toJSON(dtable, dataframe = "columns", auto_unbox = TRUE, na = "null")
writeLines(paste0('{"columns":', jsonlite::toJSON(dcols, auto_unbox = TRUE), ',"rows":', dtable_json, '}'),
           file.path(outdir, "dataset.json"), useBytes = TRUE)

cat("DONE. Files written:\n")
print(list.files(outdir))
cat("\nmeta.json:\n"); print(read_json(file.path(outdir, "meta.json")))
