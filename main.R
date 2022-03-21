library(readxl) # load readxl to read excel
library(writexl)
library(mice)
library(dplyr)
library(tidyr)
library(purrr)
library(rms)
library(MASS)

ap_data <- read_excel("AP for cox.xlsx") # read ap.xlsx from filei

Outcome <- "Surv(total_LOS, non_surv)"

SurvOutcome <- "non_surv"

TimeVariable <- "total_LOS"

DecimalCandidates <- c("age", "weight", "heartrate", "MAP", "resprate", "temperature", "wbc", "heamoglobin", "platelet", "albumin", "bilirubin", "creatinine", "BUN", "potassium", "calcium", "lactate", "GCS")
Candidates <- c("age", "albumin", "creatinine")
summary(ap_data)

data <- subset(ap_data, age <= 100 & rowSums(is.na(ap_data)) <= 4)

new_data <- data

write_xlsx(new_data, "subset-of-data.xlsx")

md.pattern(new_data)

data[DecimalCandidates]

replace_rare_value_with_na <- function(data) {
  if (typeof(data) == "character") return(data)
  high <- quantile(data, .99, na.rm = TRUE)
  low <- quantile(data, .01, na.rm = TRUE)
  na <- if (typeof(data) == "integer") NA_integer_ else NA_real_
  return(if_else((data >= high | data <= low), na, data))
}


new_data[DecimalCandidates] <- map(data[DecimalCandidates], replace_rare_value_with_na)


write_xlsx(new_data, "replaced.xlsx")

imp <- mice(new_data, m = 50, seed = 9527)

completed_data <- complete(imp)

write_xlsx(completed_data, "after-mice.xlsx")

library(survival)

Formula <- formula(paste(paste(Outcome, "~", collapse = " "),
                         paste(Candidates, collapse = " + ")))

fit <- coxph(Formula, data = completed_data)
sum.surv <- summary(fit)
c_index <- sum.surv$concordance
c_index

library(boot)

c_index <- function(formula, data, indices) {
  tran.data <- data[indices,]
  vali.data <- data[-indices,]
  fit <- coxph(formula, data = tran.data)
  result <- survConcordance(Surv(vali.data$total_LOS, vali.data$non_surv) ~ predict(fit, vali.data))
  index <- as.numeric(result$concordance)
  index
}

set.seed(9527)
results <- boot(data = completed_data, statistic = c_index, R = 1000, formula = Formula)
results$t0
results$t
results$statistic
mean(results$t)
# boot.ci(results)


cindex <- function(time, event, variable, data) {
  require(rms)
  surv <- Surv(time, event)
  form <- as.formula(paste("surv~", paste(variable, collapse = " + ")))
  fit.coxph <- coxph(form, data)
  fit.cph <- cph(form, data = data, x = TRUE, y = TRUE, surv = TRUE)
  sum.surv <- summary(fit.coxph)
  c_index <- sum.surv$concordance
  res <- paste(c_index[1], " (", c_index[1] - 1.96 * c_index[2], " - ", c_index[1] + 1.96 * c_index[2], ")", sep = "")
  set.seed(9527)
  v <- rms::validate(fit.cph, dxy = TRUE, B = 1000)
  Dxy <- v[rownames(v) == "Dxy", colnames(v) == "index.corrected"]
  bias_corrected_c_index <- abs(Dxy) / 2 + 0.5
  final <- list()
  final["C-index and 95%CI"] <- res
  final["Bias corrected C-index"] <- bias_corrected_c_index
  final
  return(v)
}

# create c index
v <- cindex(completed_data$total_LOS, completed_data$non_surv, Candidates, completed_data)
v


# smp_size <- floor(0.9 * nrow(completed_data))
# set.seed(0121)
# train_ind <- sample(seq_len(nrow(completed_data)), size = smp_size)
# train <- completed_data[train_ind,]
# test <- completed_data[-train_ind,]
# write_xlsx(train, "trainingset.xlsx")
# write_xlsx(test, "testset.xlsx")
dd <- datadist(completed_data)

options(datadist = 'dd')

# create a formula
Formula <- formula(paste(paste(Outcome, "~", collapse = " "),
                         paste(Candidates, collapse = " + ")))


library(pROC)

pancer <- as.data.frame(completed_data)
coxm <- cph(Formula, x = T, y = T, data = pancer, surv = T)
surv <- Survival(coxm) # 建立生存函数


# surv1 <- function(x)surv(1*3,lp=x)
# surv2 <- function(x)surv(1*6,lp=x)
# surv3 <- function(x)surv(1*12,lp=x)
surv4 <- function(x)surv(1 * 30, lp = x)
nomogramResult <- nomogram(coxm, fun = list(surv4), lp = F, funlabel = '30-Day survival probability', maxscale = 100, fun.at = c('0.9', '0.85', '0.80', '0.70', '0.6', '0.5', '0.4', '0.3', '0.2', '0.1'))

plot(nomogramResult, xfrac = .2,
     cex.axis = 1.05,
     force.label = TRUE,
     tcl = 0.8,
     lmgp = 0.1,
     vnames = "labels",
     col.grid = gray(c(0.85, 0.95)))

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

km_fit <- survfit(coxm)
summary(km_fit, times = c(30))
autoplot(km_fit)

aa_fit <- aareg(coxm, data = pancer)
autoplot(aa_fit)

r_fit <- ranger(coxm, data = pancer, importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)


SurvFormula <- formula(paste(paste(SurvOutcome, "~", collapse = " "),
                             paste(Candidates, collapse = " + ")))


model.full <- glm(SurvFormula, data = completed_data, family = binomial)
model.final <- stepAIC(model.full, direction = "both")

summary(model.final)
lrm.final <- lrm(formula = model.final$formula, data = completed_data, x = TRUE, y = TRUE)

nom <- nomogram(lrm.final, fun = plogis,
                fun.at = c(0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.99),
                lp = F,
                funlabel = "30-Day Survival Probability"
)

plot(nom, xfrac = .2,
     cex.axis = 1.05,
     force.label = TRUE,
     tcl = 0.8,
     lmgp = 0.1,
     vnames = "labels",
     col.grid = gray(c(0.85, 0.95)))


validate(lrm.final, method = "boot", B = 1000)
cal1 <- calibrate(lrm.final, method = "boot", B = 1000)
plot(
  cal1,
  xlim = c(0.6, 1.0),
  ylim = c(0.5, 1.0),
  xlab = "Predicted Probability",
  ylab = "Observed Probability",
  legend = FALSE
)

abline(0, 1, col = "black", lty = 2, lwd = 2)
lines(cal1[, c("predy", "calibrated.orig")], type = "l", lwd = 2, col = "red", pch = 16)
lines(cal1[, c("predy", "calibrated.corrected")], type = "l", lwd = 2, col = "blue", pch = 16)
legend(0.4, 0.3,
       c("Apparent", "Ideal", "Bias-corrected"),
       lty = c(2, 1, 1),
       lwd = c(2, 1, 1),
       col = c("black", "red", "blue"),
       bty = "n")


roc_curve <- function(data) {
  pre <- predict(model.final, newdata = data, type = 'response')
  rocplot1 <- roc(data$non_surv, pre,  smoothed = TRUE,
                  # arguments for ci
                  ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                  # arguments for plot
                  plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                  print.auc=TRUE, show.thres=TRUE)
  ci.auc(rocplot1)
  sens.ci <- ci.se(rocplot1)
  plot(sens.ci, type="shape", col="lightblue")
  plot(sens.ci, type="bars")


# ,
  # print.thres="best", print.thres.best.method="youden",
  #          print.thres.best.weights=c(50, 0.2),
  #          print.thres.adj = c(1.1, 1.25),
  #          add = TRUE)
}

roc_curve(completed_data)





