## code for install ##
packages <- c("data.table","tidyverse","skimr","here","remotes", "xgboost",
              "SuperLearner","glmnet","ranger","origami","caret", "ROCR",
              "gtsummary", "expss", "dplyr", "tidyr", "broom", "earth")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

remotes::install_github("tlverse/sl3")
library(sl3)

#Formats figures to be as close to AJE preferences as possible
thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

Sys.info()
here()

## Reading in data ##
if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|
   Sys.info()["nodename"]=="EPI9TPH7M3"|
   Sys.info()["nodename"]=="LAPTOP-KHT5IA2Q"|
   Sys.info()["nodename"]=="BLACKBEAN"){
  a <- read_csv(here("data","numomfull.csv"))
  a_scaled <- read_csv(here("data","2022_07-02-scaled_hei_full.csv"))
} 

names(a)
names(a_scaled)

covars <- names(a_scaled)
mydata <- cbind(tibble(ptb37 = a$ptb37),a_scaled)
#mydata <- a

head(mydata)

freq <- table (mydata$ptb37)
freq
summary(mydata$ptb37)


# summarize the data for Table 1
a_label = apply_labels(a, 
                       heiy1_totalveg = "Total Vegetables",
                       heiy2_green_and_bean = "Greens and Beans",
                       heiy3_totalfruit = "Total Fruit",
                       heiy4_wholefruit = "Whole Fruit",
                       heiy5_wholegrain = "Whole Grain",
                       heiy6_totaldairy = "Total Dairy",
                       heiy7_totprot = "Total Protein Foods",
                       heiy8_seaplant_prot = "Seafood and Plant Proteins",
                       heiy9_fattyacid = "Fatty Acids",
                       heiy10_sodium = "Sodium",
                       heiy11_refinedgrain = "Refined Grains",
                       heiy12_addsug = "Added Sugars",
                       heiy13_sfa = "Saturated Fats",
                       ptb37 = c("Term" = 0,
                              "Preterm"=1)
                       )
table1 <- 
  tbl_summary(
    a_label %>% select (-numomid),
    by = ptb37, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  #add_n() %>% # add column with total number of non-missing observations
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**HEI-2015 Component**") %>% # update the column header
  bold_labels() 
table1

set.seed(123)

#Defining ML task - sl3_task object keeps track of 
# 1- the data 
# 2- the roles that each variable has 
# 3- any metadata
#This step builds the ensemble model using cross-validation (CV)
# (If not specified, default is 10 folds)
#Based on the information in the SL3 textbook, okay to create ensemble
# using entire data set because of the cross validation
task <- make_sl3_Task(
  data = mydata,
  covariates = covars,
  outcome = "ptb37",
  folds = 5L
)
#Note: By default, sl3 will impute missing covariates.
# It will provide warning if it detects missing data and does this.
# For each covariate column with missing values, 
# sl3 uses the median to impute missing continuous covariates 
# and the mode to impute binary and categorical covariates.

length(task$folds) # Confirm the number of folds for the task

head(task$folds[[1]]$training_set) # row indices for fold 1 training
head(task$folds[[3]]$validation_set) # row indices for fold 3 test (validation)

# CREATE SUPERLEARNER LIBRARY
# choose base learners

sl3_list_learners(c("binomial"))

lrnr_mean <- make_learner(Lrnr_mean)
lrnr_glm <- make_learner(Lrnr_glm)

#earth (multivariate adaptive regression splines) learner
lrnr_earth <- make_learner(Lrnr_earth)
#defaults: GCV penalty per knot = 3, degree (max degree of interactions) = 2,
# pmethod (pruning) = backward, nfold (cross validation) = 0 (none)

# ranger learner
grid_params1 <- list(num.trees = c(250, 500, 1000),
                     mtry = c(2,3,4),
                     min.node.size = c(25,50,100))
grid <- expand.grid(grid_params1, KEEP.OUT.ATTRS = FALSE)
lrnr_ranger <- vector("list", length = nrow(grid))
for(i in 1:nrow(grid)){
  lrnr_ranger[[i]] <- make_learner(Lrnr_ranger, 
                                   num.trees=grid[i,]$num.trees, 
                                   mtry=grid[i,]$mtry,
                                   min.node.size=grid[i,]$min.node.size)
}
#lrnr_ranger <- make_learner(Lrnr_ranger)

# glmnet learner
grid_params2 <- seq(0,1,by=.25)
lrnr_glmnet <- vector("list", length = length(grid_params2))
for(i in 1:length(grid_params2)){
  lrnr_glmnet[[i]] <- make_learner(Lrnr_glmnet, alpha = grid_params2[i])
}
#lrnr_glmnet <- make_learner(Lrnr_glmnet)

# xgboost learner
grid_params3 <- list(max_depth = c(2, 4, 6, 8),
                     eta = c(0.01, 0.1, 0.3))
grid1 <- expand.grid(grid_params3, KEEP.OUT.ATTRS = FALSE)
lrnr_xgboost <- vector("list", length = nrow(grid1))
for(i in 1:nrow(grid1)){
  lrnr_xgboost[[i]] <- make_learner(Lrnr_xgboost, max_depth=grid1[i,]$max_depth, eta=grid1[i,]$eta)
}
#lrnr_xgboost <- make_learner(Lrnr_xgboost)

learner_stack <- make_learner(Stack, unlist(list(
  lrnr_mean, 
  lrnr_glm
  ,
                                                 lrnr_ranger, lrnr_glmnet,
                                                 lrnr_xgboost, lrnr_earth
  ), 
                                            recursive = TRUE))
learner_stack

#Lrnr_sl object specifies the Super Learner
sl <- Lrnr_sl$new(learner_stack)
sl

#TRAINING - The SL algorithm fits a metalearner on the validation-set
# predictions in a cross-validated manner, thereby avoiding overfitting.
sl_fit <- sl$train(task)
sl_fit

#SL outcome predictions for each subject based on the ensemble
sl_preds <- sl_fit$predict()
head(sl_preds)
summary(sl_preds) #mean should reflect % with outcome in sample

preds <- cbind(a_label,sl_preds)
preds2 <- preds %>%
  mutate(ptbclass = case_when(sl_preds >= 0.089 ~ 'Preterm', 
                              sl_preds < 0.089 ~ 'Term')) %>%
  mutate(ptbref = case_when(ptb37 == 1 ~ 'Preterm', 
                            ptb37 == 0 ~ 'Term'))

table(preds2$ptbref)

confusion <- confusionMatrix (factor(preds2$ptbclass), factor(preds2$ptbref))
confusion
confusion <- as.table(confusion)
confusion
write.table(confusion, file = "ConfusionTable.SL.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

predstat <- tapply(sl_preds, mydata$ptb37, summary) 
predstat
#mean should be higher in group with outcome


#PLOT of predicted values for those with and without the outcome
#qplot(mydata$ptb37, sl_preds, main = 
        # "lrn_mean"
        # "lrn_glm"
        # "lrn_ranger100"
        "SuperLearner"
#) + theme_minimal()#

#Same type of info but set up as side by side box plot (which I like better)
probability <- sl_preds
preterm <- a_label$ptb37
boxplot(probability ~ preterm)

#PLOT AUC curve
AUCsl <- performance(prediction(sl_preds, mydata$ptb37), measure = "auc")@y.values[[1]]
plot(performance(prediction(sl_preds, mydata$ptb37), "tpr", "fpr"), main = 
       # "lrn_mean"
       # "lrn_glm"
       # "lrn_ranger100"
       "SuperLearner" 
)
AUCsl


# Cross validation using SL3 - if not specified 10 folds is default
#We can cross-validate the SL to see how well the SL performs on unseen data, 
#and obtain an estimate of the cross-validated risk of the SL
task_CVsl <- make_sl3_Task(
  data = mydata,
  outcome = "ptb37",
  covariates = covars,
  drop_missing_outcome = TRUE,
  folds = origami::make_folds(
    n = sum(!is.na(mydata$ptb37)),
    fold_fun = folds_vfold,
    V = 5
  )
)


CVsl <- CV_lrnr_sl(sl_fit, task_CVsl, loss_loglik_binomial) #CV risk using NLL
#Note: Default CV risk based on MSE
CVsl

#Cross validated predictions
full_fit_preds <- sl_fit$fit_object$cv_fit$predict_fold(
  task = task, fold_number = "full"
)
head(full_fit_preds)
summary(full_fit_preds) #mean should reflect % with outcome in sample

# Permutation based variable importance plot

# sl3 variable importance plot
## why do we use sl_fit here, and not an output of the second CV run?
varimp <- importance(sl_fit, loss_loglik_binomial, type = "permute")
varimp

var_implabel <- varimp %>%
  mutate(covariate = case_when(covariate == 'heiy1_totalveg' ~ 'Total Vegetables', 
                               covariate == 'heiy2_green_and_bean' ~ 'Greens and Beans',
                               covariate == 'heiy3_totalfruit' ~ 'Total Fruit',
                               covariate == 'heiy4_wholefruit' ~ 'Whole Fruit',
                               covariate == 'heiy5_wholegrain' ~ 'Whole Grain',
                               covariate == 'heiy6_totaldairy' ~ 'Total Dairy',
                               covariate == 'heiy7_totprot' ~ 'Total Protein Foods',
                               covariate == 'heiy8_seaplant_prot' ~ 'Seafood and Plant Proteins',
                               covariate == 'heiy9_fattyacid' ~ 'Fatty Acids',
                               covariate == 'heiy10_sodium' ~ 'Sodium',
                               covariate == 'heiy11_refinedgrain' ~ 'Refined Grains',
                               covariate == 'heiy12_addsug' ~ 'Added Sugars',
                               covariate == 'heiy13_sfa' ~ 'Saturated Fats'))


# plot variable importance
importance_plot(
  var_implabel
)

#importance automatically set based on outcome type
#for constant and binomial outcomes set to loss_squared_error
#see https://github.com/tlverse/sl3/blob/master/R/importance.R

#code to create variable importance plot based on my prior RF work
dotchart(labels = varimp$covariate, varimp$NLL_difference, 
xlab="Variable Importance", col='darkblue', pch=16, cex=1.1)
abline(v=abs(min(varimp$NLL_difference)), col='red', lty='longdash', lwd=2)

sapply(pkgs, require, character.only = T) #load 

# Create table
ols_table <- CVsl %>%
  select(-fold_sd, -fold_min_NLL, -fold_max_NLL) %>%
  mutate_each(funs(round(., 4)), -learner) %>%
  rename(Weight = coefficients)
ols_table

# Export
write.table(ols_table, file = "Table.HEISL_CVRisk.txt", sep = ",", quote = FALSE, row.names = F)

#install.packages("xtable")
#library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

# make dataset with a few variables to summarize
CVrisk <- CVsl %>% select(-fold_sd, -fold_min_NLL, -fold_max_NLL) %>%
  mutate_each(funs(round(., 4)), -learner) %>%
  rename(Weight = coefficients)

data(CVrisk)
xtable(CVrisk)

install.packages("gridExtra")
install.packages("ggplot2")
library(gridExtra)
library(ggplot2)

tg <- tableGrob(CVrisk)
ggsave("CVrisk.pdf", tg)