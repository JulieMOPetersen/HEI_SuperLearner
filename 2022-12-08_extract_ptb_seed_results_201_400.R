################################################################################
#######   HEI SuperLearner_Extracting Results   ################################
#######   AUTHOR: JMP                           ################################
#######   DATE: 12.08.2022                      ################################
################################################################################

#INSTALL AND LOAD PACKAGES
packages <- c("here", "readr", "stringr", "dplyr", "tidyr")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}
for (package in packages) {
  library(package, character.only=T)
}

sessionInfo()

#Read in data

if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|
   Sys.info()["nodename"]=="EPI9TPH7M3"|
   Sys.info()["nodename"]=="LAPTOP-KHT5IA2Q"|
   Sys.info()["nodename"]=="BLACKBEAN"){
  my_data <- read.delim2 (here("data","sim_run_2.txt"))
}

subset1 <-my_data[c(291, 461, 631, 801, 971, 1141, 1311, 1481, 1651, 1821, 
                      1991, 2161, 2331, 2501, 2671, 2841, 3011, 3181, 3351, 3521,
                      3691, 3861, 4031, 4201, 4371, 4541, 4711, 4881, 5051, 5221,
                      5391, 5561, 5731, 5901, 6071, 6241, 6411, 6581, 6751, 6921,
                      7091, 7261, 7431, 7601, 7771, 7941, 8111, 8281, 8451, 8621, 
                      8791, 8961, 9131, 9301, 9471, 9641, 9811, 9981, 10151, 10321, 
                      10491, 10661, 10831, 11001, 11171, 11341, 11511, 11681, 11851, 12021,
                      12191, 12361, 12531, 12701, 12871, 13041, 13211, 13381, 13551, 13721,
                      13891, 14061, 14231, 14401, 14571, 14741, 14911, 15081, 15251, 15421,
                      15591, 15761, 15931, 16101, 16271, 16441, 16611, 16781, 16951, 17121,
                      17291, 17461, 17631, 17801, 17971, 18141, 18311, 18481, 18651, 18821,
                      18991, 19161, 19331, 19501, 19671, 19841, 20011, 20181, 20351, 20521,
                      20691, 20861, 21031, 21201, 21371, 21541, 21711, 21881, 22051, 22221, 
                      22391, 22561, 22731, 22901, 23071, 23241, 23411, 23581, 23751, 23921,
                      24091, 24261, 24431, 24601, 24771, 24941, 25111, 25281, 25451, 25621,
                      25791, 25961, 26131, 26301, 26471, 26641, 26811, 26981, 27151, 27321,
                      27491, 27661, 27831, 28001, 28171, 28341, 28511, 28681, 28851, 29021,
                      29191, 29361, 29531, 29701, 29871, 30041, 30211, 30381, 30551, 30721,
                      30891, 31061, 31231, 31401, 31571, 31741, 31911, 32081, 32251, 32421,
                      32591, 32761, 32931, 33101, 33271, 33441, 33611, 33781, 33951, 34121
                      ), 1, drop=FALSE]
subset1 <- subset1 %>%
  mutate_at("running", str_replace, "1  ", "") %>%
  mutate_at("running", str_replace, "ptb37", "") %>%
  mutate_at("running", str_replace, " 201 ", "") %>%
  mutate_at("running", str_replace, " 202 ", "") %>%
  mutate_at("running", str_replace, " 203 ", "") %>%
  mutate_at("running", str_replace, " 204 ", "") %>%
  mutate_at("running", str_replace, " 205 ", "") %>%
  mutate_at("running", str_replace, " 206 ", "") %>%
  mutate_at("running", str_replace, " 207 ", "") %>%
  mutate_at("running", str_replace, " 208 ", "") %>%
  mutate_at("running", str_replace, " 209 ", "") %>%
  mutate_at("running", str_replace, " 210 ", "") %>%
  mutate_at("running", str_replace, " 211 ", "") %>%
  mutate_at("running", str_replace, " 212 ", "") %>%
  mutate_at("running", str_replace, " 213 ", "") %>%
  mutate_at("running", str_replace, " 214 ", "") %>%
  mutate_at("running", str_replace, " 215 ", "") %>%
  mutate_at("running", str_replace, " 216 ", "") %>%
  mutate_at("running", str_replace, " 217 ", "") %>%
  mutate_at("running", str_replace, " 218 ", "") %>%
  mutate_at("running", str_replace, " 219 ", "") %>%
  mutate_at("running", str_replace, " 220 ", "") %>%   
  mutate_at("running", str_replace, " 221 ", "") %>%
  mutate_at("running", str_replace, " 222 ", "") %>%
  mutate_at("running", str_replace, " 223 ", "") %>%
  mutate_at("running", str_replace, " 224 ", "") %>%
  mutate_at("running", str_replace, " 225 ", "") %>%
  mutate_at("running", str_replace, " 226 ", "") %>%
  mutate_at("running", str_replace, " 227 ", "") %>%
  mutate_at("running", str_replace, " 228 ", "") %>%
  mutate_at("running", str_replace, " 229 ", "") %>%
  mutate_at("running", str_replace, " 230 ", "") %>%   
  mutate_at("running", str_replace, " 231 ", "") %>%
  mutate_at("running", str_replace, " 232 ", "") %>%
  mutate_at("running", str_replace, " 233 ", "") %>%
  mutate_at("running", str_replace, " 234 ", "") %>%
  mutate_at("running", str_replace, " 235 ", "") %>%
  mutate_at("running", str_replace, " 236 ", "") %>%
  mutate_at("running", str_replace, " 237 ", "") %>%
  mutate_at("running", str_replace, " 238 ", "") %>%
  mutate_at("running", str_replace, " 239 ", "") %>%
  mutate_at("running", str_replace, " 240 ", "") %>%   
  mutate_at("running", str_replace, " 241 ", "") %>%
  mutate_at("running", str_replace, " 242 ", "") %>%
  mutate_at("running", str_replace, " 243 ", "") %>%
  mutate_at("running", str_replace, " 244 ", "") %>%
  mutate_at("running", str_replace, " 245 ", "") %>%
  mutate_at("running", str_replace, " 246 ", "") %>%
  mutate_at("running", str_replace, " 247 ", "") %>%
  mutate_at("running", str_replace, " 248 ", "") %>%
  mutate_at("running", str_replace, " 249 ", "") %>%
  mutate_at("running", str_replace, " 250 ", "") %>%   
  mutate_at("running", str_replace, " 251 ", "") %>%
  mutate_at("running", str_replace, " 252 ", "") %>%
  mutate_at("running", str_replace, " 253 ", "") %>%
  mutate_at("running", str_replace, " 254 ", "") %>%
  mutate_at("running", str_replace, " 255 ", "") %>%
  mutate_at("running", str_replace, " 256 ", "") %>%
  mutate_at("running", str_replace, " 257 ", "") %>%
  mutate_at("running", str_replace, " 258 ", "") %>%
  mutate_at("running", str_replace, " 259 ", "") %>%
  mutate_at("running", str_replace, " 260 ", "") %>%   
  mutate_at("running", str_replace, " 261 ", "") %>%
  mutate_at("running", str_replace, " 262 ", "") %>%
  mutate_at("running", str_replace, " 263 ", "") %>%
  mutate_at("running", str_replace, " 264 ", "") %>%
  mutate_at("running", str_replace, " 265 ", "") %>%
  mutate_at("running", str_replace, " 266 ", "") %>%
  mutate_at("running", str_replace, " 267 ", "") %>%
  mutate_at("running", str_replace, " 268 ", "") %>%
  mutate_at("running", str_replace, " 269 ", "") %>%
  mutate_at("running", str_replace, " 270 ", "") %>%   
  mutate_at("running", str_replace, " 271 ", "") %>%
  mutate_at("running", str_replace, " 272 ", "") %>%
  mutate_at("running", str_replace, " 273 ", "") %>%
  mutate_at("running", str_replace, " 274 ", "") %>%
  mutate_at("running", str_replace, " 275 ", "") %>%
  mutate_at("running", str_replace, " 276 ", "") %>%
  mutate_at("running", str_replace, " 277 ", "") %>%
  mutate_at("running", str_replace, " 278 ", "") %>%
  mutate_at("running", str_replace, " 279 ", "") %>%
  mutate_at("running", str_replace, " 280 ", "") %>%   
  mutate_at("running", str_replace, " 281 ", "") %>%
  mutate_at("running", str_replace, " 282 ", "") %>%
  mutate_at("running", str_replace, " 283 ", "") %>%
  mutate_at("running", str_replace, " 284 ", "") %>%
  mutate_at("running", str_replace, " 285 ", "") %>%
  mutate_at("running", str_replace, " 286 ", "") %>%
  mutate_at("running", str_replace, " 287 ", "") %>%
  mutate_at("running", str_replace, " 288 ", "") %>%
  mutate_at("running", str_replace, " 289 ", "") %>%
  mutate_at("running", str_replace, " 290 ", "") %>%   
  mutate_at("running", str_replace, " 291 ", "") %>%
  mutate_at("running", str_replace, " 292 ", "") %>%
  mutate_at("running", str_replace, " 293 ", "") %>%
  mutate_at("running", str_replace, " 294 ", "") %>%
  mutate_at("running", str_replace, " 295 ", "") %>%
  mutate_at("running", str_replace, " 296 ", "") %>%
  mutate_at("running", str_replace, " 297 ", "") %>%
  mutate_at("running", str_replace, " 298 ", "") %>%
  mutate_at("running", str_replace, " 299 ", "") %>%
  mutate_at("running", str_replace, " 300 ", "") 

subset1 <- subset1 %>%
  mutate_at("running", str_replace, " 301 ", "") %>%
  mutate_at("running", str_replace, " 302 ", "") %>%
  mutate_at("running", str_replace, " 303 ", "") %>%
  mutate_at("running", str_replace, " 304 ", "") %>%
  mutate_at("running", str_replace, " 305 ", "") %>%
  mutate_at("running", str_replace, " 306 ", "") %>%
  mutate_at("running", str_replace, " 307 ", "") %>%
  mutate_at("running", str_replace, " 308 ", "") %>%
  mutate_at("running", str_replace, " 309 ", "") %>%
  mutate_at("running", str_replace, " 310 ", "") %>%
  mutate_at("running", str_replace, " 311 ", "") %>%
  mutate_at("running", str_replace, " 312 ", "") %>%
  mutate_at("running", str_replace, " 313 ", "") %>%
  mutate_at("running", str_replace, " 314 ", "") %>%
  mutate_at("running", str_replace, " 315 ", "") %>%
  mutate_at("running", str_replace, " 316 ", "") %>%
  mutate_at("running", str_replace, " 317 ", "") %>%
  mutate_at("running", str_replace, " 318 ", "") %>%
  mutate_at("running", str_replace, " 319 ", "") %>%
  mutate_at("running", str_replace, " 320 ", "") %>%   
  mutate_at("running", str_replace, " 321 ", "") %>%
  mutate_at("running", str_replace, " 322 ", "") %>%
  mutate_at("running", str_replace, " 323 ", "") %>%
  mutate_at("running", str_replace, " 324 ", "") %>%
  mutate_at("running", str_replace, " 325 ", "") %>%
  mutate_at("running", str_replace, " 326 ", "") %>%
  mutate_at("running", str_replace, " 327 ", "") %>%
  mutate_at("running", str_replace, " 328 ", "") %>%
  mutate_at("running", str_replace, " 329 ", "") %>%
  mutate_at("running", str_replace, " 330 ", "") %>%   
  mutate_at("running", str_replace, " 331 ", "") %>%
  mutate_at("running", str_replace, " 332 ", "") %>%
  mutate_at("running", str_replace, " 333 ", "") %>%
  mutate_at("running", str_replace, " 334 ", "") %>%
  mutate_at("running", str_replace, " 335 ", "") %>%
  mutate_at("running", str_replace, " 336 ", "") %>%
  mutate_at("running", str_replace, " 337 ", "") %>%
  mutate_at("running", str_replace, " 338 ", "") %>%
  mutate_at("running", str_replace, " 339 ", "") %>%
  mutate_at("running", str_replace, " 340 ", "") %>%   
  mutate_at("running", str_replace, " 341 ", "") %>%
  mutate_at("running", str_replace, " 342 ", "") %>%
  mutate_at("running", str_replace, " 343 ", "") %>%
  mutate_at("running", str_replace, " 344 ", "") %>%
  mutate_at("running", str_replace, " 345 ", "") %>%
  mutate_at("running", str_replace, " 346 ", "") %>%
  mutate_at("running", str_replace, " 347 ", "") %>%
  mutate_at("running", str_replace, " 348 ", "") %>%
  mutate_at("running", str_replace, " 349 ", "") %>%
  mutate_at("running", str_replace, " 350 ", "") %>%   
  mutate_at("running", str_replace, " 351 ", "") %>%
  mutate_at("running", str_replace, " 352 ", "") %>%
  mutate_at("running", str_replace, " 353 ", "") %>%
  mutate_at("running", str_replace, " 354 ", "") %>%
  mutate_at("running", str_replace, " 355 ", "") %>%
  mutate_at("running", str_replace, " 356 ", "") %>%
  mutate_at("running", str_replace, " 357 ", "") %>%
  mutate_at("running", str_replace, " 358 ", "") %>%
  mutate_at("running", str_replace, " 359 ", "") %>%
  mutate_at("running", str_replace, " 360 ", "") %>%   
  mutate_at("running", str_replace, " 361 ", "") %>%
  mutate_at("running", str_replace, " 362 ", "") %>%
  mutate_at("running", str_replace, " 363 ", "") %>%
  mutate_at("running", str_replace, " 364 ", "") %>%
  mutate_at("running", str_replace, " 365 ", "") %>%
  mutate_at("running", str_replace, " 366 ", "") %>%
  mutate_at("running", str_replace, " 367 ", "") %>%
  mutate_at("running", str_replace, " 368 ", "") %>%
  mutate_at("running", str_replace, " 369 ", "") %>%
  mutate_at("running", str_replace, " 370 ", "") %>%   
  mutate_at("running", str_replace, " 371 ", "") %>%
  mutate_at("running", str_replace, " 372 ", "") %>%
  mutate_at("running", str_replace, " 373 ", "") %>%
  mutate_at("running", str_replace, " 374 ", "") %>%
  mutate_at("running", str_replace, " 375 ", "") %>%
  mutate_at("running", str_replace, " 376 ", "") %>%
  mutate_at("running", str_replace, " 377 ", "") %>%
  mutate_at("running", str_replace, " 378 ", "") %>%
  mutate_at("running", str_replace, " 379 ", "") %>%
  mutate_at("running", str_replace, " 380 ", "") %>%   
  mutate_at("running", str_replace, " 381 ", "") %>%
  mutate_at("running", str_replace, " 382 ", "") %>%
  mutate_at("running", str_replace, " 383 ", "") %>%
  mutate_at("running", str_replace, " 384 ", "") %>%
  mutate_at("running", str_replace, " 385 ", "") %>%
  mutate_at("running", str_replace, " 386 ", "") %>%
  mutate_at("running", str_replace, " 387 ", "") %>%
  mutate_at("running", str_replace, " 388 ", "") %>%
  mutate_at("running", str_replace, " 389 ", "") %>%
  mutate_at("running", str_replace, " 390 ", "") %>%   
  mutate_at("running", str_replace, " 391 ", "") %>%
  mutate_at("running", str_replace, " 392 ", "") %>%
  mutate_at("running", str_replace, " 393 ", "") %>%
  mutate_at("running", str_replace, " 394 ", "") %>%
  mutate_at("running", str_replace, " 395 ", "") %>%
  mutate_at("running", str_replace, " 396 ", "") %>%
  mutate_at("running", str_replace, " 397 ", "") %>%
  mutate_at("running", str_replace, " 398 ", "") %>%
  mutate_at("running", str_replace, " 399 ", "") %>%
  mutate_at("running", str_replace, " 400 ", "") 
  
subset1$running<-str_squish(subset1$running)  
subset1 <-separate(subset1, col=running, into=c('heiy10_sodium_RD', 'heiy11_refinedgrain_RD', 'heiy12_addsug_RD'), sep=' ')
subset1<-within(subset1, {
  heiy10_sodium_RD <- as.numeric(heiy10_sodium_RD)
  heiy11_refinedgrain_RD <- as.numeric(heiy11_refinedgrain_RD)
  heiy12_addsug_RD <- as.numeric(heiy12_addsug_RD)})
subset1<-within(subset1, {
  heiy10_sodium_RD <- abs(heiy10_sodium_RD)
  heiy11_refinedgrain_RD <- abs(heiy11_refinedgrain_RD)
  heiy12_addsug_RD <- abs(heiy12_addsug_RD)})
summary(subset1)

subset2 <-my_data[c(293, 463, 633, 803, 973, 1143, 1313, 1483, 1653, 1823, 
                      1993, 2163, 2333, 2503, 2673, 2843, 3013, 3183, 3353, 3523,
                      3693, 3863, 4033, 4203, 4373, 4543, 4713, 4883, 5053, 5223,
                      5393, 5563, 5733, 5903, 6073, 6243, 6413, 6583, 6753, 6923,
                      7093, 7263, 7433, 7603, 7773, 7943, 8113, 8283, 8453, 8623, 
                      8793, 8963, 9133, 9303, 9473, 9643, 9813, 9983, 10153, 10323, 
                      10493, 10663, 10833, 11003, 11173, 11343, 11513, 11683, 11853, 12023,
                      12193, 12363, 12533, 12703, 12873, 13043, 13213, 13383, 13553, 13723,
                      13893, 14063, 14233, 14403, 14573, 14743, 14913, 15083, 15253, 15423,
                      15593, 15763, 15933, 16103, 16273, 16443, 16613, 16783, 16953, 17123,
                      17293, 17463, 17633, 17803, 17973, 18143, 18313, 18483, 18653, 18823,
                      18993, 19163, 19333, 19503, 19673, 19843, 20013, 20183, 20353, 20523,
                      20693, 20863, 21033, 21203, 21373, 21543, 21713, 21883, 22053, 22223, 
                      22393, 22563, 22733, 22903, 23073, 23243, 23413, 23583, 23753, 23923,
                      24093, 24263, 24433, 24603, 24773, 24943, 25113, 25283, 25453, 25623,
                      25793, 25963, 26133, 26303, 26473, 26643, 26813, 26983, 27153, 27323,
                      27493, 27663, 27833, 28003, 28173, 28343, 28513, 28683, 28853, 29023,
                      29193, 29363, 29533, 29703, 29873, 30043, 30213, 30383, 30553, 30723,
                      30893, 31063, 31233, 31403, 31573, 31743, 31913, 32083, 32253, 32423,
                      32593, 32763, 32933, 33103, 33273, 33443, 33613, 33783, 33953, 34123
), 1, drop=FALSE]
subset2 <- subset2 %>%
  mutate_at("running", str_replace, "1 ", "") 

subset2$running<-str_squish(subset2$running)  
subset2 <-separate(subset2, col=running, into=c('heiy13_sfa_RD', 
                                                    'heiy1_totalveg_RD', 
                                                    'heiy2_green_and_bean_RD', 
                                                    'heiy3_totalfruit_RD'), sep=' ')
subset2<-within(subset2, {
  heiy13_sfa_RD <- as.numeric(heiy13_sfa_RD)
  heiy1_totalveg_RD <- as.numeric(heiy1_totalveg_RD)
  heiy2_green_and_bean_RD <- as.numeric(heiy2_green_and_bean_RD)
  heiy3_totalfruit_RD <- as.numeric(heiy3_totalfruit_RD)})
subset2<-within(subset2, {
  heiy13_sfa_RD <- abs(heiy13_sfa_RD)
  heiy1_totalveg_RD <- abs(heiy1_totalveg_RD)
  heiy2_green_and_bean_RD <- abs(heiy2_green_and_bean_RD)
  heiy3_totalfruit_RD <- abs(heiy3_totalfruit_RD)})
summary(subset2)


subset3 <-my_data[c(295, 465, 635, 805, 975, 1145, 1315, 1485, 1655, 1825, 
                    1995, 2165, 2335, 2505, 2675, 2845, 3015, 3185, 3355, 3525,
                    3695, 3865, 4035, 4205, 4375, 4545, 4715, 4885, 5055, 5225,
                    5395, 5565, 5735, 5905, 6075, 6245, 6415, 6585, 6755, 6925,
                    7095, 7265, 7435, 7605, 7775, 7945, 8115, 8285, 8455, 8625, 
                    8795, 8965, 9135, 9305, 9475, 9645, 9815, 9985, 10155, 10325, 
                    10495, 10665, 10835, 11005, 11175, 11345, 11515, 11685, 11855, 12025,
                    12195, 12365, 12535, 12705, 12875, 13045, 13215, 13385, 13555, 13725,
                    13895, 14065, 14235, 14405, 14575, 14745, 14915, 15085, 15255, 15425,
                    15595, 15765, 15935, 16105, 16275, 16445, 16615, 16785, 16955, 17125,
                    17295, 17465, 17635, 17805, 17975, 18145, 18315, 18485, 18655, 18825,
                    18995, 19165, 19335, 19505, 19675, 19845, 20015, 20185, 20355, 20525,
                    20695, 20865, 21035, 21205, 21375, 21545, 21715, 21885, 22055, 22225, 
                    22395, 22565, 22735, 22905, 23075, 23245, 23415, 23585, 23755, 23925,
                    24095, 24265, 24435, 24605, 24775, 24945, 25115, 25285, 25455, 25625,
                    25795, 25965, 26135, 26305, 26475, 26645, 26815, 26985, 27155, 27325,
                    27495, 27665, 27835, 28005, 28175, 28345, 28515, 28685, 28855, 29025,
                    29195, 29365, 29535, 29705, 29875, 30045, 30215, 30385, 30555, 30725,
                    30895, 31065, 31235, 31405, 31575, 31745, 31915, 32085, 32255, 32425,
                    32595, 32765, 32935, 33105, 33275, 33445, 33615, 33785, 33955, 34125
), 1, drop=FALSE]
subset3 <- subset3 %>%
  mutate_at("running", str_replace, "1 ", "") 

subset3$running<-str_squish(subset3$running)  
subset3 <-separate(subset3, col=running, into=c('heiy4_wholefruit_RD',
  'heiy5_wholegrain_RD', 'heiy6_totaldairy_RD', 'heiy7_totprot_RD'), sep=' ')
subset3<-within(subset3, {
  heiy4_wholefruit_RD <- as.numeric(heiy4_wholefruit_RD)
  heiy5_wholegrain_RD <- as.numeric(heiy5_wholegrain_RD)
  heiy6_totaldairy_RD <- as.numeric(heiy6_totaldairy_RD)
  heiy7_totprot_RD <- as.numeric(heiy7_totprot_RD)})
subset3<-within(subset3, {
  heiy4_wholefruit_RD <- abs(heiy4_wholefruit_RD)
  heiy5_wholegrain_RD <- abs(heiy5_wholegrain_RD)
  heiy6_totaldairy_RD <- abs(heiy6_totaldairy_RD)
  heiy7_totprot_RD <- abs(heiy7_totprot_RD)})
summary(subset3)


subset4 <-my_data[c(297, 467, 637, 807, 977, 1147, 1317, 1487, 1657, 1827, 
                    1997, 2167, 2337, 2507, 2677, 2847, 3017, 3187, 3357, 3527,
                    3697, 3867, 4037, 4207, 4377, 4547, 4717, 4887, 5057, 5227,
                    5397, 5567, 5737, 5907, 6077, 6247, 6417, 6587, 6757, 6927,
                    7097, 7267, 7437, 7607, 7777, 7947, 8117, 8287, 8457, 8627, 
                    8797, 8967, 9137, 9307, 9477, 9647, 9817, 9987, 10157, 10327, 
                    10497, 10667, 10837, 11007, 11177, 11347, 11517, 11687, 11857, 12027,
                    12197, 12367, 12537, 12707, 12877, 13047, 13217, 13387, 13557, 13727,
                    13897, 14067, 14237, 14407, 14577, 14747, 14917, 15087, 15257, 15427,
                    15597, 15767, 15937, 16107, 16277, 16447, 16617, 16787, 16957, 17127,
                    17297, 17467, 17637, 17807, 17977, 18147, 18317, 18487, 18657, 18827,
                    18997, 19167, 19337, 19507, 19677, 19847, 20017, 20187, 20357, 20527,
                    20697, 20867, 21037, 21207, 21377, 21547, 21717, 21887, 22057, 22227, 
                    22397, 22567, 22737, 22907, 23077, 23247, 23417, 23587, 23757, 23927,
                    24097, 24267, 24437, 24607, 24777, 24947, 25117, 25287, 25457, 25627,
                    25797, 25967, 26137, 26307, 26477, 26647, 26817, 26987, 27157, 27327,
                    27497, 27667, 27837, 28007, 28177, 28347, 28517, 28687, 28857, 29027,
                    29197, 29367, 29537, 29707, 29877, 30047, 30217, 30387, 30557, 30727,
                    30897, 31067, 31237, 31407, 31577, 31747, 31917, 32087, 32257, 32427,
                    32597, 32767, 32937, 33107, 33277, 33447, 33617, 33787, 33957, 34127
), 1, drop=FALSE]
subset4 <- subset4 %>%
  mutate_at("running", str_replace, "1 ", "") 

subset4$running<-str_squish(subset4$running)  
subset4 <-separate(subset4, col=running, into=c('heiy8_seaplant_prot_RD',
                                                'heiy9_fattyacid_RD', 
                                                'heiy10_sodium_CNLL'), sep=' ')
subset4<-within(subset4, {
  heiy8_seaplant_prot_RD <- as.numeric(heiy8_seaplant_prot_RD)
  heiy9_fattyacid_RD <- as.numeric(heiy9_fattyacid_RD)
  heiy10_sodium_CNLL <- as.numeric(heiy10_sodium_CNLL)})
subset4<-within(subset4, {
  heiy8_seaplant_prot_RD <- abs(heiy8_seaplant_prot_RD)
  heiy9_fattyacid_RD <- abs(heiy9_fattyacid_RD)})
summary(subset4)


subset5 <-my_data[c(299, 469, 639, 809, 979, 1149, 1319, 1489, 1659, 1829, 
                    1999, 2169, 2339, 2509, 2679, 2849, 3019, 3189, 3359, 3529,
                    3699, 3869, 4039, 4209, 4379, 4549, 4719, 4889, 5059, 5229,
                    5399, 5569, 5739, 5909, 6079, 6249, 6419, 6589, 6759, 6929,
                    7099, 7269, 7439, 7609, 7779, 7949, 8119, 8289, 8459, 8629, 
                    8799, 8969, 9139, 9309, 9479, 9649, 9819, 9989, 10159, 10329, 
                    10499, 10669, 10839, 11009, 11179, 11349, 11519, 11689, 11859, 12029,
                    12199, 12369, 12539, 12709, 12879, 13049, 13219, 13389, 13559, 13729,
                    13899, 14069, 14239, 14409, 14579, 14749, 14919, 15089, 15259, 15429,
                    15599, 15769, 15939, 16109, 16279, 16449, 16619, 16789, 16959, 17129,
                    17299, 17469, 17639, 17809, 17979, 18149, 18319, 18489, 18659, 18829,
                    18999, 19169, 19339, 19509, 19679, 19849, 20019, 20189, 20359, 20529,
                    20699, 20869, 21039, 21209, 21379, 21549, 21719, 21889, 22059, 22229, 
                    22399, 22569, 22739, 22909, 23079, 23249, 23419, 23589, 23759, 23929,
                    24099, 24269, 24439, 24609, 24779, 24949, 25119, 25289, 25459, 25629,
                    25799, 25969, 26139, 26309, 26479, 26649, 26819, 26989, 27159, 27329,
                    27499, 27669, 27839, 28009, 28179, 28349, 28519, 28689, 28859, 29029,
                    29199, 29369, 29539, 29709, 29879, 30049, 30219, 30389, 30559, 30729,
                    30899, 31069, 31239, 31409, 31579, 31749, 31919, 32089, 32259, 32429,
                    32599, 32769, 32939, 33109, 33279, 33449, 33619, 33789, 33959, 34129
), 1, drop=FALSE]
subset5 <- subset5 %>%
  mutate_at("running", str_replace, "1 ", "") 

subset5$running<-str_squish(subset5$running)  
subset5 <-separate(subset5, col=running, into=c('heiy11_refinedgrain_CNLL',
                                                'heiy12_addsug_CNLL', 
                                                'heiy13_sfa_CNLL'), sep=' ')
subset5<-within(subset5, {
  heiy11_refinedgrain_CNLL <- as.numeric(heiy11_refinedgrain_CNLL)
  heiy12_addsug_CNLL <- as.numeric(heiy12_addsug_CNLL)
  heiy13_sfa_CNLL <- as.numeric(heiy13_sfa_CNLL)})
summary(subset5)


subset6 <-my_data[c(301, 471,   641,  811,  981, 1151, 1321, 1491, 1661, 1831, 
                    2001, 2171, 2341, 2511, 2681, 2851, 3021, 3191, 3361, 3531, 
                    3701, 3871, 4041, 4211, 4381, 4551, 4721, 4891, 5061, 5231, 
                    5401, 5571, 5741, 5911, 6081, 6251, 6421, 6591, 6761, 6931, 
                    7101, 7271, 7441, 7611, 7781, 7951, 8121, 8291, 8461, 8631, 
                    8801, 8971, 9141, 9311, 9481, 9651, 9821, 9991,10161,10331,
                    10501,10671,10841,11011,11181,11351,11521,11691,11861,12031,
                    12201,12371,12541,12711,12881,13051,13221,13391,13561,13731,
                    13901,14071,14241,14411,14581,14751,14921,15091,15261,15431,
                    15601,15771,15941,16111,16281,16451,16621,16791,16961,17131,
                    17301,17471,17641,17811,17981,18151,18321,18491,18661,18831,
                    19001,19171,19341,19511,19681,19851,20021,20191,20361,20531,
                    20701,20871,21041,21211,21381,21551,21721,21891,22061,22231,
                    22401,22571,22741,22911,23081,23251,23421,23591,23761,23931,
                    24101,24271,24441,24611,24781,24951,25121,25291,25461,25631,
                    25801,25971,26141,26311,26481,26651,26821,26991,27161,27331,
                    27501,27671,27841,28011,28181,28351,28521,28691,28861,29031,
                    29201,29371,29541,29711,29881,30051,30221,30391,30561,30731,
                    30901,31071,31241,31411,31581,31751,31921,32091,32261,32431,
                    32601,32771,32941,33111,33281,33451,33621,33791,33961,34131
), 1, drop=FALSE]
subset6 <- subset6 %>%
  mutate_at("running", str_replace, "1 ", "") 

subset6$running<-str_squish(subset6$running)  
subset6 <-separate(subset6, col=running, into=c('heiy1_totalveg_CNLL',
                                                'heiy2_green_and_bean_CNLL',
                                                'heiy3_totalfruit_CNLL'), sep=' ')
subset6<-within(subset6, {
  heiy1_totalveg_CNLL <- as.numeric(heiy1_totalveg_CNLL)
  heiy2_green_and_bean_CNLL <- as.numeric(heiy2_green_and_bean_CNLL)
  heiy3_totalfruit_CNLL <- as.numeric(heiy3_totalfruit_CNLL)})
summary(subset6)

subset7 <-my_data[c(303,   473,   643,   813,   983,  1153,  1323,  1493,  1663,  1833,  
                    2003,  2173,  2343,  2513,  2683,  2853, 3023,  3193,  3363,  3533,  
                    3703,  3873,  4043,  4213,  4383,  4553,  4723,  4893,  5063,  5233,  
                    5403,  5573, 5743,  5913,  6083,  6253,  6423,  6593,  6763,  6933,  
                    7103,  7273,  7443,  7613,  7783,  7953,  8123,  8293, 8463,  8633,  
                    8803,  8973,  9143,  9313,  9483,  9653,  9823,  9993, 10163, 10333, 
                    10503, 10673, 10843, 11013, 11183, 11353, 11523, 11693, 11863, 12033, 
                    12203, 12373, 12543, 12713, 12883, 13053, 13223, 13393, 13563, 13733,
                    13903, 14073, 14243, 14413, 14583, 14753, 14923, 15093, 15263, 15433, 
                    15603, 15773, 15943, 16113, 16283, 16453, 16623, 16793, 16963, 17133, 
                    17303, 17473, 17643, 17813, 17983, 18153, 18323, 18493, 18663, 18833, 
                    19003, 19173, 19343, 19513, 19683, 19853, 20023, 20193, 20363, 20533, 
                    20703, 20873, 21043, 21213, 21383, 21553, 21723, 21893, 22063, 22233, 
                    22403, 22573, 22743, 22913, 23083, 23253, 23423, 23593, 23763, 23933, 
                    24103, 24273, 24443, 24613, 24783, 24953, 25123, 25293, 25463, 25633, 
                    25803, 25973, 26143, 26313, 26483, 26653, 26823, 26993, 27163, 27333,
                    27503, 27673, 27843, 28013, 28183, 28353, 28523, 28693, 28863, 29033, 
                    29203, 29373, 29543, 29713, 29883, 30053, 30223, 30393, 30563, 30733, 
                    30903, 31073, 31243, 31413, 31583, 31753, 31923, 32093, 32263, 32433, 
                    32603, 32773, 32943, 33113, 33283, 33453, 33623, 33793, 33963, 34133 
), 1, drop=FALSE]
subset7 <- subset7 %>%
  mutate_at("running", str_replace, "1 ", "") 

subset7$running<-str_squish(subset7$running)  
subset7 <-separate(subset7, col=running, into=c('heiy4_wholefruit_CNLL',
                                                'heiy5_wholegrain_CNLL', 
                                                'heiy6_totaldairy_CNLL'), sep=' ')
subset7<-within(subset7, {
  heiy4_wholefruit_CNLL <- as.numeric(heiy4_wholefruit_CNLL)
  heiy5_wholegrain_CNLL <- as.numeric(heiy5_wholegrain_CNLL)
  heiy6_totaldairy_CNLL <- as.numeric(heiy6_totaldairy_CNLL)})
summary(subset7)


subset8 <-my_data[c(305,    475,    645,    815,    985,   1155,   1325,   1495,   1665,   1835,   
                    2005,   2175,   2345,   2515,   2685,   2855,  3025,   3195,   3365,   3535,   
                    3705,   3875,   4045,   4215,   4385,   4555,   4725,   4895,   5065,   5235,   
                    5405,   5575,  5745,   5915,   6085,   6255,   6425,   6595,   6765,   6935,   
                    7105,   7275,   7445,   7615,   7785,   7955,   8125,   8295,  8465,   8635,   
                    8805,   8975,   9145,   9315,   9485,   9655,   9825,   9995,  10165,  10335,  
                    10505,  10675,  10845,  11015,  11185,  11355,  11525,  11695,  11865,  12035,  
                    12205,  12375,  12545,  12715,  12885,  13055,  13225,  13395,  13565,  13735, 
                    13905,  14075,  14245,  14415,  14585,  14755,  14925,  15095,  15265,  15435,  
                    15605,  15775,  15945,  16115,  16285,  16455,  16625,  16795,  16965,  17135,  
                    17305,  17475,  17645,  17815,  17985,  18155,  18325,  18495,  18665,  18835,  
                    19005,  19175,  19345,  19515,  19685,  19855,  20025,  20195,  20365,  20535,  
                    20705,  20875,  21045,  21215,  21385,  21555,  21725,  21895,  22065,  22235,  
                    22405,  22575,  22745,  22915,  23085,  23255,  23425,  23595,  23765,  23935,  
                    24105,  24275,  24445,  24615,  24785,  24955,  25125,  25295,  25465,  25635,  
                    25805,  25975,  26145,  26315,  26485,  26655,  26825,  26995,  27165,  27335, 
                    27505,  27675,  27845,  28015,  28185,  28355,  28525,  28695,  28865,  29035,  
                    29205,  29375,  29545,  29715,  29885,  30055,  30225,  30395,  30565,  30735,  
                    30905,  31075,  31245,  31415,  31585,  31755,  31925,  32095,  32265,  32435,  
                    32605,  32775,  32945,  33115,  33285,  33455,  33625,  33795,  33965,  34135 
), 1, drop=FALSE]
subset8 <- subset8 %>%
  mutate_at("running", str_replace, "1 ", "") 

subset8$running<-str_squish(subset8$running)  
subset8 <-separate(subset8, col=running, into=c('heiy7_totprot_CNLL',
                                                'heiy8_seaplant_prot_CNLL',
                                                'heiy9_fattyacid_CNLL'), sep=' ')
subset8<-within(subset8, {
  heiy7_totprot_CNLL <- as.numeric(heiy7_totprot_CNLL)
  heiy8_seaplant_prot_CNLL <- as.numeric(heiy8_seaplant_prot_CNLL)
  heiy9_fattyacid_CNLL <- as.numeric(heiy9_fattyacid_CNLL)})
summary(subset8)

count <- 201:400
count <- as.data.frame(count)

my_data <- cbind(count, subset1, subset2, subset3, subset4, subset5, subset6, subset7, subset8)

saveRDS(my_data, file = here("data",paste0("ptb_seed_201_400.Rds")))
#seq(303, 40000, by = 170)
