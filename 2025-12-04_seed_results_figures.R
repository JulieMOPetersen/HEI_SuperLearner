################################################################################
#######   HEI SuperLearner_Plotting Results     ################################
#######   AUTHOR: JMP                           ################################
#######   DATE: 12.16.2025                      ################################
################################################################################

#INSTALL AND LOAD PACKAGES
packages <- c("here", "readr", "gtsummary", "expss")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}
for (package in packages) {
  library(package, character.only=T)
}

sessionInfo()

#Read in data - Preterm Birth

if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|
   Sys.info()["nodename"]=="EPI9TPH7M3"|
   Sys.info()["nodename"]=="LAPTOP-KHT5IA2Q"|
   Sys.info()["nodename"]=="BLACKBEAN"|
   Sys.info()["nodename"]=="COPH-JPETERLTAP"){
  ptb_seed <- read_csv (here("data","Variable importance_seed results 2025-12-01.csv"))
}

my_data <- ptb_seed

summary(my_data)

ADPP = subset(my_data, select = c(Seed, indivdiff_totalveg,
                                   indivdiff_green_and_bean,
                                   indivdiff_totalfruit,
                                   indivdiff_wholefruit,
                                   indivdiff_wholegrain,
                                   indivdiff_totaldairy,
                                   indivdiff_totprot,
                                   indivdiff_seaplant_prot,
                                   indivdiff_fattyacid,
                                   indivdiff_sodium,
                                   indivdiff_refinedgrain,
                                   indivdiff_addsug,
                                   indivdiff_sfa) )

col_order <-c("indivdiff_totaldairy", "indivdiff_wholegrain", "indivdiff_refinedgrain",
              "indivdiff_seaplant_prot", "indivdiff_totprot", "indivdiff_totalveg",
              "indivdiff_fattyacid", "indivdiff_totalfruit", "indivdiff_sfa", 
              "indivdiff_sodium", "indivdiff_wholefruit", "indivdiff_green_and_bean",
              "indivdiff_addsug")

ADPP <- ADPP[, col_order]

# summarize the data for Table 1
ADPP_label = apply_labels(ADPP,
                        indivdiff_totalveg = "Total Vegetables",
                        indivdiff_green_and_bean = "Greens and Beans",
                        indivdiff_totalfruit = "Total Fruit",
                        indivdiff_wholefruit = "Whole Fruit",
                        indivdiff_wholegrain = "Whole Grain",
                        indivdiff_totaldairy = "Total Dairy",
                        indivdiff_totprot = "Total Protein Foods",
                        indivdiff_seaplant_prot = "Seafood and Plant Proteins",
                        indivdiff_fattyacid = "Fatty Acids",
                        indivdiff_sodium = "Sodium",
                        indivdiff_refinedgrain = "Refined Grains",
                        indivdiff_addsug = "Added Sugars",
                        indivdiff_sfa = "Saturated Fats"
)
names(ADPP_label)

tableADPP <-
  tbl_summary(
    ADPP_label ,
    #by = ptb37, # split table by group
    #by = sgahad, # split table by group
    #by = pree_acog, # split table by group
    #by = gdm, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  #add_n() %>% # add column with total number of non-missing observations
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**HEI-2015 Component**") %>% # update the column header
  bold_labels()
tableADPP

par(mar = c(11.5, 4.1, 0.5, 2.1))
boxplot(ADPP_label, las = 2, 
        names = c("Total Dairy", "Whole Grains", "Refine Grains",
                  "Seafood and Plant Proteins", "Total Protein Foods", 
                  "Total Vegetables",
                  "Fatty Acids", "Total Fruit", "Saturated Fats", 
                  "Sodium", "Whole Fruit", "Greens and Beans",
                  "Added Sugars"))

CNLLs = subset(my_data, select = c(Seed, heiy1_totalveg,
                                        heiy2_green_and_bean,
                                        heiy3_totalfruit,
                                        heiy4_wholefruit,
                                        heiy5_wholegrain,
                                        heiy6_totaldairy,
                                        heiy7_totprot,
                                        heiy8_seaplant_prot,
                                        heiy9_fattyacid,
                                        heiy10_sodium,
                                        heiy11_refinedgrain,
                                        heiy12_addsug,
                                        heiy13_sfa) )

col_order2 <- c("heiy11_refinedgrain", "heiy6_totaldairy", "heiy7_totprot",
               "heiy9_fattyacid", "heiy8_seaplant_prot", "heiy1_totalveg",
               "heiy3_totalfruit", "heiy10_sodium", "heiy13_sfa", 
               "heiy4_wholefruit", "heiy2_green_and_bean", "heiy12_addsug")

CNLLs <- CNLLs[, col_order2]

# summarize the data for Table 1
CNLL_label = apply_labels(CNLLs,
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
                        heiy13_sfa = "Saturated Fats"
)
names(CNLL_label)

tableCNLL <-
  tbl_summary(
    CNLL_label ,
    #by = ptb37, # split table by group
    #by = sgahad, # split table by group
    #by = pree_acog, # split table by group
    #by = gdm, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  #add_n() %>% # add column with total number of non-missing observations
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**HEI-2015 Component**") %>% # update the column header
  bold_labels()
tableCNLL

par(mar = c(11.5, 4.1, 0.5, 2.1))
boxplot(CNLL_label, las = 2, 
        names = c("Refined Grains", "Total Dairy", "Total Protein Foods",
                  "Fatty Acids", "Seafood & Plant Proteins", "Total Vegetables",
                  "Total Fruit", "Sodium", "Saturated Fats", 
                  "Whole Fruit", "Greens and Beans", "Added Sugars"))


#Read in data - SGA Birth

if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|
   Sys.info()["nodename"]=="EPI9TPH7M3"|
   Sys.info()["nodename"]=="LAPTOP-KHT5IA2Q"|
   Sys.info()["nodename"]=="BLACKBEAN"|
   Sys.info()["nodename"]=="COPH-JPETERLTAP"){
  sga_seed <- read_csv (here("data","Variable importance_SGA seed results 2025-12-04.csv"))
}

my_data <- sga_seed

summary(my_data)

ADPP = subset(my_data, select = c(Seed, indivdiff_totalveg,
                                  indivdiff_green_and_bean,
                                  indivdiff_totalfruit,
                                  indivdiff_wholefruit,
                                  indivdiff_wholegrain,
                                  indivdiff_totaldairy,
                                  indivdiff_totprot,
                                  indivdiff_seaplant_prot,
                                  indivdiff_fattyacid,
                                  indivdiff_sodium,
                                  indivdiff_refinedgrain,
                                  indivdiff_addsug,
                                  indivdiff_sfa) )

col_order <-c("indivdiff_wholefruit", "indivdiff_refinedgrain", "indivdiff_totalfruit", 
              "indivdiff_seaplant_prot", "indivdiff_totprot", "indivdiff_sodium",  
              "indivdiff_addsug", "indivdiff_green_and_bean", "indivdiff_fattyacid", 
              "indivdiff_wholegrain", "indivdiff_sfa", 
               "indivdiff_totalveg", "indivdiff_totaldairy" 
              )

ADPP <- ADPP[, col_order]

# summarize the data for Table 1
ADPP_label = apply_labels(ADPP,
                          indivdiff_totalveg = "Total Vegetables",
                          indivdiff_green_and_bean = "Greens and Beans",
                          indivdiff_totalfruit = "Total Fruit",
                          indivdiff_wholefruit = "Whole Fruit",
                          indivdiff_wholegrain = "Whole Grain",
                          indivdiff_totaldairy = "Total Dairy",
                          indivdiff_totprot = "Total Protein Foods",
                          indivdiff_seaplant_prot = "Seafood and Plant Proteins",
                          indivdiff_fattyacid = "Fatty Acids",
                          indivdiff_sodium = "Sodium",
                          indivdiff_refinedgrain = "Refined Grains",
                          indivdiff_addsug = "Added Sugars",
                          indivdiff_sfa = "Saturated Fats"
)
names(ADPP_label)

tableADPP <-
  tbl_summary(
    ADPP_label ,
    #by = ptb37, # split table by group
    #by = sgahad, # split table by group
    #by = pree_acog, # split table by group
    #by = gdm, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  #add_n() %>% # add column with total number of non-missing observations
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**HEI-2015 Component**") %>% # update the column header
  bold_labels()
tableADPP

par(mar = c(11.5, 4.1, 0.5, 2.1))
boxplot(ADPP_label, las = 2, 
        names = c("Whole Fruit", "Refine Grains", "Total Fruit",
                  "Seafood and Plant Proteins", "Total Protein Foods", 
                  "Sodium", "Added Sugars", "Greens and Beans",
                  "Fatty Acids", "Whole Grains", "Saturated Fats",
                  "Total Vegetables", "Total Dairy"))

CNLLs = subset(my_data, select = c(Seed, heiy1_totalveg,
                                   heiy2_green_and_bean,
                                   heiy3_totalfruit,
                                   heiy4_wholefruit,
                                   heiy5_wholegrain,
                                   heiy6_totaldairy,
                                   heiy7_totprot,
                                   heiy8_seaplant_prot,
                                   heiy9_fattyacid,
                                   heiy10_sodium,
                                   heiy11_refinedgrain,
                                   heiy12_addsug,
                                   heiy13_sfa) )

col_order2 <- c("heiy8_seaplant_prot", "heiy3_totalfruit", "heiy4_wholefruit", 
                 "heiy11_refinedgrain", "heiy10_sodium", "heiy7_totprot",
                "heiy12_addsug", "heiy1_totalveg", "heiy9_fattyacid", 
                "heiy5_wholegrain",  "heiy2_green_and_bean",
                "heiy13_sfa", "heiy6_totaldairy")

CNLLs <- CNLLs[, col_order2]

# summarize the data for Table 1
CNLL_label = apply_labels(CNLLs,
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
                          heiy13_sfa = "Saturated Fats"
)
names(CNLL_label)

tableCNLL <-
  tbl_summary(
    CNLL_label ,
    #by = ptb37, # split table by group
    #by = sgahad, # split table by group
    #by = pree_acog, # split table by group
    #by = gdm, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  #add_n() %>% # add column with total number of non-missing observations
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**HEI-2015 Component**") %>% # update the column header
  bold_labels()
tableCNLL

par(mar = c(11.5, 4.1, 0.5, 2.1))
boxplot(CNLL_label, las = 2, 
        names = c("Seafood & Plant Proteins", "Total Fruit", "Whole Fruit", 
                  "Refined Grains", "Sodium", "Total Protein Foods",
                  "Added Sugars", "Total Vegetables", "Fatty Acids", 
                  "Whole Grain", "Greens and Beans", "Saturated Fats", 
                  "Total Dairy"))

#Read in data - Preeclampsia

if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|
   Sys.info()["nodename"]=="EPI9TPH7M3"|
   Sys.info()["nodename"]=="LAPTOP-KHT5IA2Q"|
   Sys.info()["nodename"]=="BLACKBEAN"|
   Sys.info()["nodename"]=="COPH-JPETERLTAP"){
  pree_seed <- read_csv (here("data","Variable importance_PREE seed results 2025-12-22.csv"))
}

my_data <- pree_seed

summary(my_data)

ADPP = subset(my_data, select = c(Seed, indivdiff_totalveg,
                                  indivdiff_green_and_bean,
                                  indivdiff_totalfruit,
                                  indivdiff_wholefruit,
                                  indivdiff_wholegrain,
                                  indivdiff_totaldairy,
                                  indivdiff_totprot,
                                  indivdiff_seaplant_prot,
                                  indivdiff_fattyacid,
                                  indivdiff_sodium,
                                  indivdiff_refinedgrain,
                                  indivdiff_addsug,
                                  indivdiff_sfa) )

col_order <-c( "indivdiff_totalfruit", "indivdiff_totaldairy", "indivdiff_wholegrain", 
               "indivdiff_wholefruit", "indivdiff_totalveg", "indivdiff_refinedgrain",
               "indivdiff_sodium", "indivdiff_addsug", "indivdiff_totprot",
               "indivdiff_seaplant_prot", "indivdiff_green_and_bean", "indivdiff_fattyacid",
               "indivdiff_sfa")

ADPP <- ADPP[, col_order]

# summarize the data for Table 1
ADPP_label = apply_labels(ADPP,
                          indivdiff_totalveg = "Total Vegetables",
                          indivdiff_green_and_bean = "Greens and Beans",
                          indivdiff_totalfruit = "Total Fruit",
                          indivdiff_wholefruit = "Whole Fruit",
                          indivdiff_wholegrain = "Whole Grain",
                          indivdiff_totaldairy = "Total Dairy",
                          indivdiff_totprot = "Total Protein Foods",
                          indivdiff_seaplant_prot = "Seafood and Plant Proteins",
                          indivdiff_fattyacid = "Fatty Acids",
                          indivdiff_sodium = "Sodium",
                          indivdiff_refinedgrain = "Refined Grains",
                          indivdiff_addsug = "Added Sugars",
                          indivdiff_sfa = "Saturated Fats"
)
names(ADPP_label)

tableADPP <-
  tbl_summary(
    ADPP_label ,
    #by = ptb37, # split table by group
    #by = sgahad, # split table by group
    #by = pree_acog, # split table by group
    #by = gdm, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  #add_n() %>% # add column with total number of non-missing observations
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**HEI-2015 Component**") %>% # update the column header
  bold_labels()
tableADPP

par(mar = c(11.5, 4.1, 0.5, 2.1))
boxplot(ADPP_label, las = 2, 
        names = c("Total Fruit", "Total Dairy", "Whole Grains",
                  "Whole Fruit", "Total Vegetables", "Refine Grains", 
                  "Sodium", "Added Sugars", "Total Protein Foods", 
                  "Seafood and Plant Proteins", "Greens and Beans", "Fatty Acids",
                  "Saturated Fats"))

CNLLs = subset(my_data, select = c(Seed, heiy1_totalveg,
                                   heiy2_green_and_bean,
                                   heiy3_totalfruit,
                                   heiy4_wholefruit,
                                   heiy5_wholegrain,
                                   heiy6_totaldairy,
                                   heiy7_totprot,
                                   heiy8_seaplant_prot,
                                   heiy9_fattyacid,
                                   heiy10_sodium,
                                   heiy11_refinedgrain,
                                   heiy12_addsug,
                                   heiy13_sfa) )

col_order2 <- c("heiy3_totalfruit", "heiy5_wholegrain", "heiy10_sodium", 
                "heiy4_wholefruit", "heiy1_totalveg", "heiy11_refinedgrain",
                "heiy6_totaldairy", "heiy12_addsug", "heiy7_totprot",
                "heiy9_fattyacid", "heiy8_seaplant_prot", "heiy2_green_and_bean",
                "heiy13_sfa")

CNLLs <- CNLLs[, col_order2]

# summarize the data for Table 1
CNLL_label = apply_labels(CNLLs,
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
                          heiy13_sfa = "Saturated Fats"
)
names(CNLL_label)

tableCNLL <-
  tbl_summary(
    CNLL_label ,
    #by = ptb37, # split table by group
    #by = sgahad, # split table by group
    #by = pree_acog, # split table by group
    #by = gdm, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  #add_n() %>% # add column with total number of non-missing observations
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**HEI-2015 Component**") %>% # update the column header
  bold_labels()
tableCNLL

par(mar = c(11.5, 4.1, 0.5, 2.1))
boxplot(CNLL_label, las = 2, 
        names = c("Total Fruit", "Whole Grains", "Sodium", "Whole Fruit", 
                  "Total Vegetables", "Refined Grains", "Total Dairy",
                  "Added Sugars", "Total Protein Foods",
                  "Fatty Acids",  "Seafood & Plant Proteins",
                  "Greens and Beans", "Saturated Fats"))

#Read in data - Gestational Diabetes

if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|
   Sys.info()["nodename"]=="EPI9TPH7M3"|
   Sys.info()["nodename"]=="LAPTOP-KHT5IA2Q"|
   Sys.info()["nodename"]=="BLACKBEAN"|
   Sys.info()["nodename"]=="COPH-JPETERLTAP"){
  gdm_seed <- read_csv (here("data","Variable importance_GDM seed results 2025-01-02.csv"))
}

my_data <- gdm_seed

summary(my_data)

ADPP = subset(my_data, select = c(Seed, indivdiff_totalveg,
                                  indivdiff_green_and_bean,
                                  indivdiff_totalfruit,
                                  indivdiff_wholefruit,
                                  indivdiff_wholegrain,
                                  indivdiff_totaldairy,
                                  indivdiff_totprot,
                                  indivdiff_seaplant_prot,
                                  indivdiff_fattyacid,
                                  indivdiff_sodium,
                                  indivdiff_refinedgrain,
                                  indivdiff_addsug,
                                  indivdiff_sfa) )

col_order <-c( "indivdiff_refinedgrain", "indivdiff_totalfruit", 
               "indivdiff_wholefruit", "indivdiff_totalveg", "indivdiff_sodium",
               "indivdiff_green_and_bean", "indivdiff_fattyacid", "indivdiff_addsug", 
               "indivdiff_totaldairy", "indivdiff_sfa", "indivdiff_wholegrain", 
               "indivdiff_seaplant_prot",
                "indivdiff_totprot")

ADPP <- ADPP[, col_order]

# summarize the data for Table 1
ADPP_label = apply_labels(ADPP,
                          indivdiff_totalveg = "Total Vegetables",
                          indivdiff_green_and_bean = "Greens and Beans",
                          indivdiff_totalfruit = "Total Fruit",
                          indivdiff_wholefruit = "Whole Fruit",
                          indivdiff_wholegrain = "Whole Grain",
                          indivdiff_totaldairy = "Total Dairy",
                          indivdiff_totprot = "Total Protein Foods",
                          indivdiff_seaplant_prot = "Seafood and Plant Proteins",
                          indivdiff_fattyacid = "Fatty Acids",
                          indivdiff_sodium = "Sodium",
                          indivdiff_refinedgrain = "Refined Grains",
                          indivdiff_addsug = "Added Sugars",
                          indivdiff_sfa = "Saturated Fats"
)
names(ADPP_label)

tableADPP <-
  tbl_summary(
    ADPP_label ,
    #by = ptb37, # split table by group
    #by = sgahad, # split table by group
    #by = pree_acog, # split table by group
    #by = gdm, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  #add_n() %>% # add column with total number of non-missing observations
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**HEI-2015 Component**") %>% # update the column header
  bold_labels()
tableADPP

par(mar = c(11.5, 4.1, 0.5, 2.1))
boxplot(ADPP_label, las = 2, 
        names = c("Refine Grains", "Total Fruit", 
                  "Whole Fruit", "Total Vegetables", "Sodium", 
                  "Greens and Beans", "Fatty Acids", "Added Sugars", 
                  "Total Dairy", "Saturated Fats", "Whole Grains",
                  "Seafood and Plant Proteins",
                  "Total Protein Foods"))


CNLLs = subset(my_data, select = c(Seed, heiy1_totalveg,
                                   heiy2_green_and_bean,
                                   heiy3_totalfruit,
                                   heiy4_wholefruit,
                                   heiy5_wholegrain,
                                   heiy6_totaldairy,
                                   heiy7_totprot,
                                   heiy8_seaplant_prot,
                                   heiy9_fattyacid,
                                   heiy10_sodium,
                                   heiy11_refinedgrain,
                                   heiy12_addsug,
                                   heiy13_sfa) )

col_order2 <- c("heiy11_refinedgrain", "heiy1_totalveg", "heiy3_totalfruit", 
                "heiy10_sodium", "heiy5_wholegrain", 
                "heiy6_totaldairy", "heiy9_fattyacid", "heiy12_addsug",
                "heiy13_sfa", "heiy2_green_and_bean", "heiy4_wholefruit",
                 "heiy8_seaplant_prot", "heiy7_totprot")

CNLLs <- CNLLs[, col_order2]

# summarize the data for Table 1
CNLL_label = apply_labels(CNLLs,
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
                          heiy13_sfa = "Saturated Fats"
)
names(CNLL_label)

tableCNLL <-
  tbl_summary(
    CNLL_label ,
    #by = ptb37, # split table by group
    #by = sgahad, # split table by group
    #by = pree_acog, # split table by group
    #by = gdm, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  #add_n() %>% # add column with total number of non-missing observations
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**HEI-2015 Component**") %>% # update the column header
  bold_labels()
tableCNLL

par(mar = c(11.5, 4.1, 0.5, 2.1))
boxplot(CNLL_label, las = 2, 
        names = c("Refined Grains", "Total Vegetables", "Total Fruit",
                  "Sodium", "Whole Grains", "Total Dairy", 
                  "Fatty Acids", "Added Sugars",
                  "Saturated Fats", "Greens and Beans",
                  "Whole Fruit", 
                  "Seafood & Plant Proteins",
                  "Total Protein Foods"
                  ))
