#session-timeout-minutes=4320

## code for install ##
packages <- c("data.table","tidyverse","skimr","here","remotes", "xgboost",
              "SuperLearner","glmnet","ranger","origami","caret", "ROCR",
              "gtsummary", "expss", "dplyr", "tidyr", "broom", "earth", "e1071",
              "nnet", "iml", "shapr")

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
   Sys.info()["nodename"]=="BLACKBEAN"|
   Sys.info()["nodename"]=="COPH-JPETERLTAP"){
  a <- read_csv(here("data","heiml_full.csv"))
  a_scaled <- read_csv(here("data","2025-10-19-scaled_hei.csv"))
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
  folds = 10L
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
                     eta = 0.3)
grid1 <- expand.grid(grid_params3, KEEP.OUT.ATTRS = FALSE)
lrnr_xgboost <- vector("list", length = nrow(grid1))
for(i in 1:nrow(grid1)){
  lrnr_xgboost[[i]] <- make_learner(Lrnr_xgboost, max_depth=grid1[i,]$max_depth, eta=grid1[i,]$eta)
}
#lrnr_xgboost <- make_learner(Lrnr_xgboost)

# --- SVM learners ---
#grid_params_svm <- expand.grid(
#  kernel = c("linear", "radial"),
#  cost = c(0.1, 1, 10)
#)

#lrnr_svm <- vector("list", nrow(grid_params_svm))
#for (i in seq_len(nrow(grid_params_svm))) {
#  lrnr_svm[[i]] <- make_learner(
#    Lrnr_svm,
#    kernel = grid_params_svm$kernel[i],
#    cost = grid_params_svm$cost[i]
#  )
#}

# --- Neural Network (Perceptron) learners ---
#grid_params_nn <- expand.grid(
#  size = c(2, 5, 10),   # number of hidden units
#  decay = c(0.001, 0.01)  # weight decay (regularization)
#)

#lrnr_nnet <- vector("list", nrow(grid_params_nn))
#for (i in seq_len(nrow(grid_params_nn))) {
#  lrnr_nnet[[i]] <- make_learner(
#    Lrnr_nnet,
#    size = grid_params_nn$size[i],
#    decay = grid_params_nn$decay[i],
#    maxit = 200   # increase for convergence if needed
#  )
#}

learner_stack <- make_learner(Stack, unlist(list(lrnr_mean, lrnr_glm,
                                                 lrnr_ranger, lrnr_glmnet,
                                                 lrnr_xgboost, lrnr_earth
#                                                 , lrnr_svm, lrnr_nnet
), 
                                            recursive = TRUE))
learner_stack

#Lrnr_sl object specifies the Super Learner
sl <- Lrnr_sl$new(learner_stack)
sl

set.seed(123)
#TRAINING - The SL algorithm fits a metalearner on the validation-set
# predictions in a cross-validated manner, thereby avoiding overfitting.
options(future.globals.maxSize = +Inf)
sl_fit <- sl$train(task)
sl_fit
save(sl_fit, file = "PTB_sl_fit_table_251129_seed123.RData")

#help("future.globals.maxSize", package = "future")

#SL outcome predictions for each subject based on the ensemble
set.seed(123)
sl_preds <- sl_fit$predict()
head(sl_preds)
summary(sl_preds) #mean should reflect % with outcome in sample

# Permutation based variable importance plot

set.seed(123)
# sl3 variable importance plot
## why do we use sl_fit here, and not an output of the second CV run?
varimp <- importance(sl_fit, loss_loglik_binomial, type = "permute")
varimp

#Risk Differences

#Check which seed to use
set.seed(123)
#set.seed(45678)
#set.seed(9)
shuffle <- mydata[sample(1:nrow(mydata)), ]     # Randomly reorder rows

#Total Vegetables 
a_nototalveg <- mydata %>% select(-heiy1_totalveg)
a_totalveg0 <- cbind(tibble(heiy1_totalveg = shuffle$heiy1_totalveg),a_nototalveg)

#aggregate(x = a_totalveg0$heiy1_totalveg,                # Specify data column
#by = list(a_totalveg0$pree_acog),              # Specify group indicator
#FUN = mean)                           # Specify function (i.e. mean)

#aggregate(x = a_totalveg0$heiy1_totalveg,                # Specify data column
#by = list(a_totalveg0$pree_acog),              # Specify group indicator
#FUN = median)                           # Specify function (i.e. median)

task_totalveg <- make_sl3_Task(
  data = a_totalveg0,
  covariates = covars,
  outcome = "ptb37",
  #outcome = "sgahad",
  #outcome = "pree_acog",
  #outcome = "gdm",
  folds = 10L
)

sl_preds_totalveg0 <- sl_fit$predict(task = task_totalveg)
indivdiff_totalveg = abs (sl_preds - sl_preds_totalveg0)
mean(indivdiff_totalveg)
#meandiff_totalveg = mean(sl_preds) - mean(sl_preds_totalveg0)
#meandiff_totalveg

#Greens and Beans
a_nogreen_and_bean <- mydata %>% select(-heiy2_green_and_bean)
a_green_and_bean0 <- cbind(tibble(heiy2_green_and_bean = shuffle$heiy2_green_and_bean),a_nogreen_and_bean)

task_green_and_bean <- make_sl3_Task(
  data = a_green_and_bean0,
  covariates = covars,
  outcome = "ptb37",
  #outcome = "sgahad",
  #outcome = "pree_acog",
  #outcome = "gdm",
  folds = 10L
)

sl_preds_green_and_bean0 <- sl_fit$predict(task = task_green_and_bean)
indivdiff_green_and_bean = abs (sl_preds - sl_preds_green_and_bean0)
mean(indivdiff_green_and_bean)
#meandiff_green_and_bean = mean(sl_preds) - mean(sl_preds_green_and_bean0)
#meandiff_green_and_bean

#Total Fruit
a_nototalfruit <- mydata %>% select(-heiy3_totalfruit)
a_totalfruit0 <- cbind(tibble(heiy3_totalfruit = shuffle$heiy3_totalfruit),a_nototalfruit)

task_totalfruit <- make_sl3_Task(
  data = a_totalfruit0,
  covariates = covars,
  outcome = "ptb37",
  #outcome = "sgahad",
  #outcome = "pree_acog",
  #outcome = "gdm",
  folds = 10L
)

sl_preds_totalfruit0 <- sl_fit$predict(task = task_totalfruit)
indivdiff_totalfruit = abs (sl_preds - sl_preds_totalfruit0)
mean(indivdiff_totalfruit)
#meandiff_totalfruit = mean(sl_preds) - mean(sl_preds_totalfruit0)
#meandiff_totalfruit

#Whole Fruit
a_nowholefruit <- mydata %>% select(-heiy4_wholefruit)
a_wholefruit0 <- cbind(tibble(heiy4_wholefruit = shuffle$heiy4_wholefruit),a_nowholefruit)

task_wholefruit <- make_sl3_Task(
  data = a_wholefruit0,
  covariates = covars,
  outcome = "ptb37",
  #outcome = "sgahad",
  #outcome = "pree_acog",
  #outcome = "gdm",
  folds = 10L
)

sl_preds_wholefruit0 <- sl_fit$predict(task = task_wholefruit)
indivdiff_wholefruit = abs (sl_preds - sl_preds_wholefruit0)
mean(indivdiff_wholefruit)
#meandiff_wholefruit = mean(sl_preds) - mean(sl_preds_wholefruit0)
#meandiff_wholefruit

#Whole Grain
a_nowholegrain <- mydata %>% select(-heiy5_wholegrain)
a_wholegrain0 <- cbind(tibble(heiy5_wholegrain = shuffle$heiy5_wholegrain),a_nowholegrain)

task_wholegrain <- make_sl3_Task(
  data = a_wholegrain0,
  covariates = covars,
  outcome = "ptb37",
  #outcome = "sgahad",
  #outcome = "pree_acog",
  #outcome = "gdm",
  folds = 10L
)

sl_preds_wholegrain0 <- sl_fit$predict(task = task_wholegrain)
indivdiff_wholegrain = abs (sl_preds - sl_preds_wholegrain0)
mean(indivdiff_wholegrain)
#meandiff_wholegrain = mean(sl_preds) - mean(sl_preds_wholegrain0)
#meandiff_wholegrain

#Total Dairy
a_nototaldairy <- mydata %>% select(-heiy6_totaldairy)
a_totaldairy0 <- cbind(tibble(heiy6_totaldairy = shuffle$heiy6_totaldairy),a_nototaldairy)

task_totaldairy <- make_sl3_Task(
  data = a_totaldairy0,
  covariates = covars,
  outcome = "ptb37",
  #outcome = "sgahad",
  #outcome = "pree_acog",
  #outcome = "gdm",
  folds = 10L
)

sl_preds_totaldairy0 <- sl_fit$predict(task = task_totaldairy)
indivdiff_totaldairy = abs (sl_preds - sl_preds_totaldairy0)
mean(indivdiff_totaldairy)
#meandiff_totaldairy = mean(sl_preds) - mean(sl_preds_totaldairy0)
#meandiff_totaldairy

#Total Protein Foods
a_nototprot <- mydata %>% select(-heiy7_totprot)
a_totprot0 <- cbind(tibble(heiy7_totprot = shuffle$heiy7_totprot),a_nototprot)

task_totprot <- make_sl3_Task(
  data = a_totprot0,
  covariates = covars,
  outcome = "ptb37",
  #outcome = "sgahad",
  #outcome = "pree_acog",
  #outcome = "gdm",
  folds = 10L
)

sl_preds_totprot0 <- sl_fit$predict(task = task_totprot)
indivdiff_totprot = abs (sl_preds - sl_preds_totprot0)
mean(indivdiff_totprot)
#meandiff_totprot = mean(sl_preds) - mean(sl_preds_totprot0)
#meandiff_totprot

#Seafood and Plant Proteins
a_noseaplant_prot <- mydata %>% select(-heiy8_seaplant_prot)
a_seaplant_prot0 <- cbind(tibble(heiy8_seaplant_prot = shuffle$heiy8_seaplant_prot),a_noseaplant_prot)

task_seaplant_prot <- make_sl3_Task(
  data = a_seaplant_prot0,
  covariates = covars,
  outcome = "ptb37",
  #outcome = "sgahad",
  #outcome = "pree_acog",
  #outcome = "gdm",
  folds = 10L
)

sl_preds_seaplant_prot0 <- sl_fit$predict(task = task_seaplant_prot)
indivdiff_seaplant_prot = abs (sl_preds - sl_preds_seaplant_prot0)
mean(indivdiff_seaplant_prot)
#meandiff_seaplant_prot = mean(sl_preds) - mean(sl_preds_seaplant_prot0)
#meandiff_seaplant_prot

#Fatty Acid Ratio
a_nofattyacid <- mydata %>% select(-heiy9_fattyacid)
a_fattyacid0 <- cbind(tibble(heiy9_fattyacid = shuffle$heiy9_fattyacid),a_nofattyacid)

task_fattyacid <- make_sl3_Task(
  data = a_fattyacid0,
  covariates = covars,
  outcome = "ptb37",
  #outcome = "sgahad",
  #outcome = "pree_acog",
  #outcome = "gdm",
  folds = 10L
)

sl_preds_fattyacid0 <- sl_fit$predict(task = task_fattyacid)
indivdiff_fattyacid = abs (sl_preds - sl_preds_fattyacid0)
mean(indivdiff_fattyacid)
#meandiff_fattyacid = mean(sl_preds) - mean(sl_preds_fattyacid0)
#meandiff_fattyacid

#Sodium
a_nosodium <- mydata %>% select(-heiy10_sodium)
a_sodium0 <- cbind(tibble(heiy10_sodium = shuffle$heiy10_sodium),a_nosodium)

task_sodium <- make_sl3_Task(
  data = a_sodium0,
  covariates = covars,
  outcome = "ptb37",
  #outcome = "sgahad",
  #outcome = "pree_acog",
  #outcome = "gdm",
  folds = 10L
)

sl_preds_sodium0 <- sl_fit$predict(task = task_sodium)
indivdiff_sodium = abs (sl_preds - sl_preds_sodium0)
mean(indivdiff_sodium)
#meandiff_sodium = mean(sl_preds) - mean(sl_preds_sodium0)
#meandiff_sodium

#Refined Grains
a_norefinedgrain <- mydata %>% select(-heiy11_refinedgrain)
a_refinedgrain0 <- cbind(tibble(heiy11_refinedgrain = shuffle$heiy11_refinedgrain),a_norefinedgrain)

task_refinedgrain <- make_sl3_Task(
  data = a_refinedgrain0,
  covariates = covars,
  outcome = "ptb37",
  #outcome = "sgahad",
  #outcome = "pree_acog",
  #outcome = "gdm",
  folds = 10L
)

sl_preds_refinedgrain0 <- sl_fit$predict(task = task_refinedgrain)
indivdiff_refinedgrain = abs (sl_preds - sl_preds_refinedgrain0)
mean(indivdiff_refinedgrain)
#meandiff_refinedgrain = mean(sl_preds) - mean(sl_preds_refinedgrain0)
#meandiff_refinedgrain

#Added Sugars
a_noaddsug <- mydata %>% select(-heiy12_addsug)
a_addsug0 <- cbind(tibble(heiy12_addsug = shuffle$heiy12_addsug),a_noaddsug)

task_addsug <- make_sl3_Task(
  data = a_addsug0,
  covariates = covars,
  outcome = "ptb37",
  #outcome = "sgahad",
  #outcome = "pree_acog",
  #outcome = "gdm",
  folds = 10L
)

sl_preds_addsug0 <- sl_fit$predict(task = task_addsug)
indivdiff_addsug = abs (sl_preds - sl_preds_addsug0)
mean(indivdiff_addsug)
#meandiff_addsug = mean(sl_preds) - mean(sl_preds_addsug0)
#meandiff_addsug

#Saturated Fats
a_nosfa <- mydata %>% select(-heiy13_sfa)
a_sfa0 <- cbind(tibble(heiy13_sfa = shuffle$heiy13_sfa),a_nosfa)

task_sfa <- make_sl3_Task(
  data = a_sfa0,
  covariates = covars,
  outcome = "ptb37",
  #outcome = "sgahad",
  #outcome = "pree_acog",
  #outcome = "gdm",
  folds = 10L
)

sl_preds_sfa0 <- sl_fit$predict(task = task_sfa)
indivdiff_sfa = abs (sl_preds - sl_preds_sfa0)
mean(indivdiff_sfa)
#meandiff_sfa = mean(sl_preds) - mean(sl_preds_sfa0)
#meandiff_sfa
