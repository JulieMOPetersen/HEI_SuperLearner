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

# read in the raw CSV file (n=750 randomly selected observations
if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|Sys.info()["nodename"]=="EPI9TPH7M3"){
  a <- read_csv(here("data","numom750.csv"))
} else{
  a <- read.csv("C:\\Diet R01\\HEI Super Learner\\numom750.csv")  
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

a_scaled = subset(a_scaled, select = -c(numomid, ptb37) )

names(a_scaled)

write.csv(a_scaled, "C:\\Diet R01\\HEI Super Learner\\2022_04_05-scaled_hei.csv")