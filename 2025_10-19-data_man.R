packages <- c("data.table","tidyverse","skimr","here","haven","VIM")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

here()

Sys.info()

# read in the raw HEI component scores
if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"
   |Sys.info()["nodename"]=="EPI9TPH7M3"
   |Sys.info()["nodename"]=="COPH-JPETERLTAP"){
  a <- read_csv(here("data","heiml_components.csv"))
} else{
  a <- read.csv("C:\\Diet R01\\HEI Super Learner\\heiml_components.csv")  
}

summary (a)

## Scale variables ##
a_scaled <- within(a, {
  heiy1_totalveg   <- scale(heiy1_totalveg   )
  heiy2_green_and_bean <- scale(heiy2_green_and_bean)
  heiy3_totalfruit <- scale(heiy3_totalfruit)
  heiy4_wholefruit <- scale(heiy4_wholefruit)
  heiy5_wholegrain <- scale(heiy5_wholegrain)
  heiy6_totaldairy <- scale(heiy6_totaldairy)
  heiy7_totprot    <- scale(heiy7_totprot)
  heiy8_seaplant_prot <- scale(heiy8_seaplant_prot)
  heiy9_fattyacid  <- scale(heiy9_fattyacid)
  heiy10_sodium   <- scale(heiy10_sodium)
  heiy11_refinedgrain <- scale(heiy11_refinedgrain)
  heiy12_addsug  <- scale(heiy12_addsug)  
  heiy13_sfa    <- scale(heiy13_sfa)
})

summary (a_scaled)

names(a_scaled)

write.csv(a_scaled, "C:\\Users\\julpetersen\\OneDrive - University of Nebraska Medical Center\\Documents\\Lisa and Ashley Diet R01\\Submitted to BJN\\data and code\\data\\2025-10-19-scaled_hei.csv")