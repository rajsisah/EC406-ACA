## loading packages 

library(haven)
library(dplyr)
library(reshape2)
library(data.table)

#Loading Data 


head(consumption68)
cons <- read_dta('level1_2_3_5 merged_perfect.dta')

################################################################################

#Data Cleaning

#Selecting Relevant Variables

cons$State_Region

cons1 <- cons%>%dplyr::select("HHID", "Item_Code","State_Region" , "Sector", "Total_Consumption_Quantity", 
                              "Total_Consumption_Value", "Cooking_Code", "Lighting_Code", "Dwelling_unit_Code", 
                              "Regular_salary_earner", "Possess_ration_card", "type_of_ration_card", "MPCE_URP", 
                              "MPCE_MRP", "HH_Size", "HH_Type", "Religion", "Social_Group", "whether_Land_owned", 
                              "Land_total_possessed", "NSS", "NSC", "MLT", "Combined_multiplier", 
                              "Subsample_multiplier")

#Dividing the Data Frame into Subsections 

total_cons_qty <- cons1%>%dplyr::select("HHID", "Item_Code", "Total_Consumption_Quantity")
total_cons_value <- cons1%>%dplyr::select("HHID", "Item_Code", "Total_Consumption_Value")
covariates <- cons1%>%dplyr::select("HHID", "State_Region","Cooking_Code", "Lighting_Code", "Dwelling_unit_Code", 
                                    "Regular_salary_earner", "Possess_ration_card", "type_of_ration_card", "MPCE_URP", 
                                    "MPCE_MRP", "HH_Size", "HH_Type", "Religion", "Social_Group", "whether_Land_owned", 
                                    "Land_total_possessed", "NSS", "NSC", "MLT", "Combined_multiplier", 
                                    "Subsample_multiplier" )

#Reshaping the Total Consumption Quantity File 

wide_total_cons_qty <- total_cons_qty%>%dcast(HHID ~ Item_Code, value.var = "Total_Consumption_Quantity")

#Reshaping the Total Consumption Value File

wide_total_cons_value <- total_cons_value%>%dcast(HHID ~ Item_Code, value.var = "Total_Consumption_Value")

################################################################################

#Renaming Item_Code from the NSS 68 Layout File

colnames(wide_total_cons_qty)

old_names_qty <-  c("HHID", "101" ,  "102" ,  "103" ,  "104"  , "105" ,  "106" ,  "107"  ,
                    "108" ,  "110"  , "111" ,  "112" ,  "113" ,  "114" , 
                    "115" ,  "116",   "117",   "118",   "120" ,"121" ,"122" ,  "129" ,  "139" ,  "140" ,  "141" ,  "142" ,  "143",   "144",   "145" , 
                    "146"   ,"147",   "148",   "150",   "151" ,"152"  , "159" ,  "160",   "161" ,  "162",  "163", "164",   "165" ,  "166",   "167" , 
                    "169" ,  "170" ,  "171",   "172",   "173"  , "174" ,"175" , "179" ,  "180",   "181" , "182" ,  "183"  , "184" ,  "185" ,  "189" , 
                    "190"  , "191",   "192",   "193",   "194" ,  "195", "196"  , "199" ,  "200",   "201" ,  "202" ,  "203",   "204" ,  "205",   "206",  
                    "207",   "208",   "210",   "211",   "212" ,  "213",   "214" ,  "215",   "216" ,  "217",   "219",   "220",   "221",   "222" ,  "223" , 
                    "224",   "225",  "226",   "227",   "228"  , "230" ,  "231","232"  , "233" ,  "234",   "235",   "236" ,  "237",   "238",   "239",  
                    "240",   "241",   "242",   "243",   "244"  , "245",   "246" ,  "247",   "249",   "250" ,  "251" ,  "252",   "253",   "254" ,  "255",  
                    "256",   "257",   "258",   "260",   "261" ,  "269",   "270" , "271"  , "272",   "273",   "274",   "275" ,  "276",   "277",   "279" , 
                    "280",   "281",   "282",   "283",   "284"  , "289",   "290" ,  "291" ,  "292",   "293",   "294" ,  "295",   "296",   "299",   "300",  
                    "301",   "302",   "309",   "310",   "311" ,  "312",   "313" ,  "314", "315",   "316", "317",   "319" ,  "320",   "321" ,  "322" , 
                    "323",   "324",   "325",   "329",   "330" ,  "331",   "332" ,  "333" ,  "334",   "335",   "336"  , "337"  , "338",   "340",   "341",  
                    "342",   "343" ,  "344",   "345",   "349")

new_names_qty <- c("HHID", "qty_101" ,  "qty_102" ,  "qty_103" ,  "qty_104"  , "qty_105" ,  "qty_106" ,  "qty_107"  ,
                   "qty_108" ,  "qty_110"  , "qty_111" ,  "qty_112" ,  "qty_113" ,  "qty_114" , 
                   "qty_115" ,  "qty_116",   "qty_117",   "qty_118",   "qty_120" ,"qty_121" ,"qty_122" ,  "qty_129" ,  "qty_139" ,  "qty_140" ,  "qty_141" ,  "qty_142" ,  "qty_143",   "qty_144",   "qty_145" , 
                   "qty_146"   ,"qty_147",   "qty_148",   "qty_150",   "qty_151" ,"qty_152"  , "qty_159" ,  "qty_160",   "qty_161" ,  "qty_162",  "qty_163", "qty_164",   "qty_165" ,  "qty_166",   "qty_167" , 
                   "qty_169" ,  "qty_170" ,  "qty_171",   "qty_172",   "qty_173"  , "qty_174" ,"qty_175" , "qty_179" ,  "qty_180",   "qty_181" , "qty_182" ,  "qty_183"  , "qty_184" ,  "qty_185" ,  "qty_189" , 
                   "qty_190"  , "qty_191",   "qty_192",   "qty_193",   "qty_194" ,  "qty_195", "qty_196"  , "qty_199" ,  "qty_200",   "qty_201" ,  "qty_202" ,  "qty_203",   "qty_204" ,  "qty_205",   "qty_206",  
                   "qty_207",   "qty_208",   "qty_210",   "qty_211",   "qty_212" ,  "qty_213",   "qty_214" ,  "qty_215",   "qty_216" ,  "qty_217",   "qty_219",   "qty_220",   "qty_221",   "qty_222" ,  "qty_223" , 
                   "qty_224",   "qty_225",  "qty_226",   "qty_227",   "qty_228"  , "qty_230" ,  "qty_231","qty_232"  , "qty_233" ,  "qty_234",   "qty_235",   "qty_236" ,  "qty_237",   "qty_238",   "qty_239",  
                   "qty_240",   "qty_241",   "qty_242",   "qty_243",   "qty_244"  , "qty_245",   "qty_246" ,  "qty_247",   "qty_249",   "qty_250" ,  "qty_251" ,  "qty_252",   "qty_253",   "qty_254" ,  "qty_255",  
                   "qty_256",   "qty_257",   "qty_258",   "qty_260",   "qty_261" ,  "qty_269",   "qty_270" , "qty_271"  , "qty_272",   "qty_273",   "qty_274",   "qty_275" ,  "qty_276",   "qty_277",   "qty_279" , 
                   "qty_280",   "qty_281",   "qty_282",   "qty_283",   "qty_284"  , "qty_289",   "qty_290" ,  "qty_291" ,  "qty_292",   "qty_293",   "qty_294" ,  "qty_295",   "qty_296",   "qty_299",   "qty_300",  
                   "qty_301",   "qty_302",   "qty_309",   "qty_310",   "qty_311" ,  "qty_312",   "qty_313" ,  "qty_314", "qty_315",   "qty_316", "qty_317",   "qty_319" ,  "qty_320",   "qty_321" ,  "qty_322" , 
                   "qty_323",   "qty_324",   "qty_325",   "qty_329",   "qty_330" ,  "qty_331",   "qty_332" ,  "qty_333" ,  "qty_334",   "qty_335",   "qty_336"  , "qty_337"  , "qty_338",   "qty_340",   "qty_341",  
                   "qty_342",   "qty_343" ,  "qty_344",   "qty_345",   "qty_349" )


old_names_val <-  c("HHID", "101" ,  "102" ,  "103" ,  "104"  , "105" ,  "106" ,  "107"  ,
                    "108" ,  "110"  , "111" ,  "112" ,  "113" ,  "114" , 
                    "115" ,  "116",   "117",   "118",   "120" ,"121" ,"122" ,  "129" ,  "139" ,  "140" ,  "141" ,  "142" ,  "143",   "144",   "145" , 
                    "146"   ,"147",   "148",   "150",   "151" ,"152"  , "159" ,  "160",   "161" ,  "162",  "163", "164",   "165" ,  "166",   "167" , 
                    "169" ,  "170" ,  "171",   "172",   "173"  , "174" ,"175" , "179" ,  "180",   "181" , "182" ,  "183"  , "184" ,  "185" ,  "189" , 
                    "190"  , "191",   "192",   "193",   "194" ,  "195", "196"  , "199" ,  "200",   "201" ,  "202" ,  "203",   "204" ,  "205",   "206",  
                    "207",   "208",   "210",   "211",   "212" ,  "213",   "214" ,  "215",   "216" ,  "217",   "219",   "220",   "221",   "222" ,  "223" , 
                    "224",   "225",  "226",   "227",   "228"  , "230" ,  "231","232"  , "233" ,  "234",   "235",   "236" ,  "237",   "238",   "239",  
                    "240",   "241",   "242",   "243",   "244"  , "245",   "246" ,  "247",   "249",   "250" ,  "251" ,  "252",   "253",   "254" ,  "255",  
                    "256",   "257",   "258",   "260",   "261" ,  "269",   "270" , "271"  , "272",   "273",   "274",   "275" ,  "276",   "277",   "279" , 
                    "280",   "281",   "282",   "283",   "284"  , "289",   "290" ,  "291" ,  "292",   "293",   "294" ,  "295",   "296",   "299",   "300",  
                    "301",   "302",   "309",   "310",   "311" ,  "312",   "313" ,  "314", "315",   "316", "317",   "319" ,  "320",   "321" ,  "322" , 
                    "323",   "324",   "325",   "329",   "330" ,  "331",   "332" ,  "333" ,  "334",   "335",   "336"  , "337"  , "338",   "340",   "341",  
                    "342",   "343" ,  "344",   "345",   "349" )


new_names_val <-  c("HHID", "val_101" ,  "val_102" ,  "val_103" ,  "val_104"  , "val_105" ,  "val_106" ,  "val_107"  ,
                    "val_108" ,  "val_110"  , "val_111" ,  "val_112" ,  "val_113" ,  "val_114" , 
                    "val_115" ,  "val_116",   "val_117",   "val_118",   "val_120" ,"val_121" ,"val_122" ,  "val_129" ,  "val_139" ,  "val_140" ,  "val_141" ,  "val_142" ,  "val_143",   "val_144",   "val_145" , 
                    "val_146"   ,"val_147",   "val_148",   "val_150",   "val_151" ,"val_152"  , "val_159" ,  "val_160",   "val_161" ,  "val_162",  "val_163", "val_164",   "val_165" ,  "val_166",   "val_167" , 
                    "val_169" ,  "val_170" ,  "val_171",   "val_172",   "val_173"  , "val_174" ,"val_175" , "val_179" ,  "val_180",   "val_181" , "val_182" ,  "val_183"  , "val_184" ,  "val_185" ,  "val_189" , 
                    "val_190"  , "val_191",   "val_192",   "val_193",   "val_194" ,  "val_195", "val_196"  , "val_199" ,  "val_200",   "val_201" ,  "val_202" ,  "val_203",   "val_204" ,  "val_205",   "val_206",  
                    "val_207",   "val_208",   "val_210",   "val_211",   "val_212" ,  "val_213",   "val_214" ,  "val_215",   "val_216" ,  "val_217",   "val_219",   "val_220",   "val_221",   "val_222" ,  "val_223" , 
                    "val_224",   "val_225",  "val_226",   "val_227",   "val_228"  , "val_230" ,  "val_231","val_232"  , "val_233" ,  "val_234",   "val_235",   "val_236" ,  "val_237",   "val_238",   "val_239",  
                    "val_240",   "val_241",   "val_242",   "val_243",   "val_244"  , "val_245",   "val_246" ,  "val_247",   "val_249",   "val_250" ,  "val_251" ,  "val_252",   "val_253",   "val_254" ,  "val_255",  
                    "val_256",   "val_257",   "val_258",   "val_260",   "val_261" ,  "val_269",   "val_270" , "val_271"  , "val_272",   "val_273",   "val_274",   "val_275" ,  "val_276",   "val_277",   "val_279" , 
                    "val_280",   "val_281",   "val_282",   "val_283",   "val_284"  , "val_289",   "val_290" ,  "val_291" ,  "val_292",   "val_293",   "val_294" ,  "val_295",   "val_296",   "val_299",   "val_300",  
                    "val_301",   "val_302",   "val_309",   "val_310",   "val_311" ,  "val_312",   "val_313" ,  "val_314", "val_315",   "val_316", "val_317",   "val_319" ,  "val_320",   "val_321" ,  "val_322" , 
                    "val_323",   "val_324",   "val_325",   "val_329",   "val_330" ,  "val_331",   "val_332" ,  "val_333" ,  "val_334",   "val_335",   "val_336"  , "val_337"  , "val_338",   "val_340",   "val_341",  
                    "val_342",   "val_343" ,  "val_344",   "val_345",   "val_349" )

colnames(wide_total_cons_qty) <- new_names_qty
colnames(wide_total_cons_value) <- new_names_val

################################################################################

#Merging with Level 6 NSS 68 Data

level6 <- read_dta("C:/Users/Rajsi Sah/Desktop/Nss68_1.0_Type1_new format/Nesstar DTA/Consumption of clothing, bedding and footwear during last 30 and 365 days - Block 7 and 8  - Level 6 -  68.dta")

head(level6)

level6_qty <- level6%>%dplyr::select("HHID", "Item_Code", "Last_365days_Quantity")
level6_val <- level6%>%dplyr::select("HHID", "Item_Code", "Last_365days_Value")

level6_qty_wide <- level6_qty %>% dcast(HHID ~ Item_Code, value.var = "Last_365days_Quantity")

level6_val_wide <- level6_val %>% dcast(HHID ~ Item_Code, value.var = "Last_365days_Value")

old_names_qty_6 <- c("HHID", "350",  "351",  "352",  "353",  "354",  "355",  "356",
                     "357",  "358",  "360",  "361",  "362",  "363",  "364",  "365",  "366",  "367", 
                     "368",  "370",  "371",  "372",  "373",  "374",  "375",  "376",  "379",  "380",
                     "381",  "382",  "383",  "384",  "385",  "389",  "390",  "391", 
                     "392",  "393",  "394",  "395",  "399" )

new_names_qty_6 <- c("HHID", "qty_350",  "qty_351",  "qty_352",  "qty_353",  "qty_354",  "qty_355",  "qty_356",
                     "qty_357",  "qty_358",  "qty_360",  "qty_361",  "qty_362",  "qty_363",  "qty_364",  "qty_365",  "qty_366",  "qty_367", 
                     "qty_368",  "qty_370",  "qty_371",  "qty_372",  "qty_373",  "qty_374",  "qty_375",  "qty_376",  "qty_379",  "qty_380",
                     "qty_381",  "qty_382",  "qty_383",  "qty_384",  "qty_385",  "qty_389",  "qty_390",  "qty_391", 
                     "qty_392",  "qty_393",  "qty_394",  "qty_395",  "qty_399" )

old_names_val_6 <- c("HHID", "350",  "351",  "352",  "353",  "354",  "355",  "356",
                     "357",  "358",  "360",  "361",  "362",  "363",  "364",  "365",  "366",  "367", 
                     "368",  "370",  "371",  "372",  "373",  "374",  "375",  "376",  "379",  "380",
                     "381",  "382",  "383",  "384",  "385",  "389",  "390",  "391", 
                     "392",  "393",  "394",  "395",  "399" )


new_names_val_6 <- c("HHID", "val_350",  "val_351",  "val_352",  "val_353",  "val_354",  "val_355",  "val_356",
                     "val_357",  "val_358",  "val_360",  "val_361",  "val_362",  "val_363",  "val_364",  "val_365",  "val_366",  "val_367", 
                     "val_368",  "val_370",  "val_371",  "val_372",  "val_373",  "val_374",  "val_375",  "val_376",  "val_379",  "val_380",
                     "val_381",  "val_382",  "val_383",  "val_384",  "val_385",  "val_389",  "val_390",  "val_391", 
                     "val_392",  "val_393",  "val_394",  "val_395",  "val_399" )


setnames(level6_qty_wide, old = old_names_qty_6, new = new_names_qty_6)
setnames(level6_val_wide, old = old_names_val_6, new = new_names_val_6)


level6_qty_wide$qty_379 <- rowSums(level6_qty_wide[c("qty_350",  "qty_351",  "qty_352",  "qty_353",  "qty_354",  "qty_355",  "qty_356",
                                                     "qty_357",  "qty_358",  "qty_360",  "qty_361",  "qty_362",  "qty_363",  "qty_364",  "qty_365",  "qty_366",  "qty_367", 
                                                     "qty_368",  "qty_370",  "qty_371",  "qty_372",  "qty_373",  "qty_374",  "qty_375",  "qty_376")], na.rm = TRUE)



wide_quantity <- inner_join(wide_total_cons_qty, level6_qty_wide, by  = "HHID")
wide_value <- inner_join(wide_total_cons_value, level6_val_wide, by = "HHID")

################################################################################

#Dealing with 0 variables 

wide_quantity$qty_169 <- rowSums(wide_quantity[c("qty_160",   "qty_161" ,  "qty_162",  "qty_163", "qty_164",   "qty_165" , 
                                                 "qty_166",   "qty_167")], na.rm = TRUE)


wide_quantity$qty_199 <- rowSums(wide_quantity[c("qty_190"  , "qty_191",   "qty_192",   "qty_193",   "qty_194" ,  
                                                 "qty_195", "qty_196")], na.rm = TRUE)

wide_quantity$qty_219 <- rowSums(wide_quantity[c("qty_200",   "qty_201" ,  "qty_202" ,  "qty_203",   "qty_204" ,  "qty_205",   "qty_206",  
                                                 "qty_207",   "qty_208",   "qty_210",   "qty_211",   "qty_212" ,  "qty_213",   "qty_214" , 
                                                 "qty_215",   "qty_216" ,  "qty_217")], na.rm = TRUE)


wide_quantity$qty_239 <- rowSums(wide_quantity[c("qty_220",   "qty_221",   "qty_222" ,  "qty_223" , 
                                                 "qty_224",   "qty_225",  "qty_226",   "qty_227",  
                                                 "qty_228"  , "qty_230" ,  "qty_231","qty_232"  , "qty_233" ,  
                                                 "qty_234",   "qty_235",   "qty_236" ,  "qty_237",   "qty_238")], na.rm = TRUE)

wide_quantity$qty_279 <- rowSums(wide_quantity[c("qty_270" , "qty_271"  , "qty_272",   "qty_273",   "qty_274",   "qty_275" ,  "qty_276", 
                                                 "qty_277")], na.rm = TRUE)

wide_quantity$qty_289 <- rowSums(wide_quantity[c("qty_280",   "qty_281",   "qty_282",   "qty_283",   "qty_284")], na.rm = TRUE)

wide_quantity$qty_299 <- rowSums(wide_quantity[c("qty_290" ,  "qty_291" ,  "qty_292",   "qty_293",   "qty_294" ,  
                                                 "qty_295",   "qty_296")], na.rm = TRUE)

wide_quantity$qty_309 <- rowSums(wide_quantity[c("qty_300",  
                                                 "qty_301",   "qty_302")], na.rm = TRUE)

wide_quantity$qty_319 <- rowSums(wide_quantity[c("qty_310",   "qty_311" ,  "qty_312",   "qty_313" ,  "qty_314", "qty_315",  
                                                 "qty_316", "qty_317")], na.rm = TRUE)

wide_quantity$qty_329 <- rowSums(wide_quantity[c("qty_321" ,  "qty_322" , 
                                                 "qty_323",   "qty_324",   "qty_325")], na.rm = TRUE)

wide_quantity$qty_389 <- rowSums(wide_quantity[c("qty_380","qty_381",  "qty_382",  "qty_383",  "qty_384",  "qty_385" )], na.rm = TRUE)

wide_quantity$qty_399 <- rowSums(wide_quantity[c("qty_390",  "qty_391", 
                                                 "qty_392",  "qty_393",  "qty_394",  "qty_395")], na.rm = TRUE)

wide_quantity$qty_349 <- rowSums(wide_quantity[c("qty_330", "qty_331", "qty_332", "qty_333", "qty_334", "qty_335", "qty_336", "qty_337", "qty_338", "qty_340", "qty_341", "qty_342", "qty_343", "qty_344", "qty_345")], na.rm = TRUE)

################################################################################

#Unit Adjustment and Price Imputation for all other commodities *********


nonfood_val <- wide_value%>%dplyr::select("HHID", "val_330" ,  "val_331",   "val_332" ,  "val_333" ,  "val_334",   "val_335",   "val_336"  , "val_337"  , "val_338",   "val_340",   "val_341",  
                                          "val_342",   "val_343" ,  "val_344",   "val_345",   "val_349", "val_350",  "val_351",  "val_352",  "val_353",  "val_354",  "val_355",  "val_356",
                                          "val_357",  "val_358",  "val_360",  "val_361",  "val_362",  "val_363",  "val_364",  "val_365",  "val_366",  "val_367", 
                                          "val_368",  "val_370",  "val_371",  "val_372",  "val_373",  "val_374",  "val_375",  "val_376",  "val_379",  "val_380",
                                          "val_381",  "val_382",  "val_383",  "val_384",  "val_385",  "val_389",  "val_390",  "val_391", 
                                          "val_392",  "val_393",  "val_394",  "val_395",  "val_399")

nonfood_qty <- wide_quantity%>%dplyr::select("HHID", "qty_330" ,  "qty_331",   "qty_332" ,  "qty_333" ,  "qty_334",   "qty_335",   "qty_336"  , "qty_337"  , "qty_338",   "qty_340",   "qty_341",  
                                             "qty_342",   "qty_343" ,  "qty_344",   "qty_345",   "qty_349", "qty_350",  "qty_351",  "qty_352",  "qty_353",  "qty_354",  "qty_355",  "qty_356",
                                             "qty_357",  "qty_358",  "qty_360",  "qty_361",  "qty_362",  "qty_363",  "qty_364",  "qty_365",  "qty_366",  "qty_367", 
                                             "qty_368",  "qty_370",  "qty_371",  "qty_372",  "qty_373",  "qty_374",  "qty_375",  "qty_376",  "qty_379",  "qty_380",
                                             "qty_381",  "qty_382",  "qty_383",  "qty_384",  "qty_385",  "qty_389",  "qty_390",  "qty_391", 
                                             "qty_392",  "qty_393",  "qty_394",  "qty_395",  "qty_399" )


nonfood_combined <- inner_join(nonfood_qty, nonfood_val, by = "HHID")

nonfood_combined[is.na(nonfood_combined)] <- 0


#Divide 330, 331, 333, 337, 338, 340, 342, 345 by 1000 since in kgs:

nonfood_combined <- nonfood_combined%>%dplyr::mutate(across(c("qty_330", "qty_331", "qty_333", "qty_337", "qty_338", "qty_340", "qty_342", "qty_345"), function(x) x/1000))

#Litres: 334, 335, 343, 344: also divide by 1000

nonfood_combined <- nonfood_combined%>%dplyr::mutate(across(c("qty_334", "qty_335", "qty_343", "qty_344"), function(x) x/1000))

################################################################################

#For Fuel and Light 

#creating weights 

nonfood_combined$share330 <- nonfood_combined$val_330/nonfood_combined$val_349
nonfood_combined$share331 <- nonfood_combined$val_331/nonfood_combined$val_349
nonfood_combined$share332 <- nonfood_combined$val_332/nonfood_combined$val_349
nonfood_combined$share333 <- nonfood_combined$val_333/nonfood_combined$val_349
nonfood_combined$share334 <- nonfood_combined$val_334/nonfood_combined$val_349
nonfood_combined$share335 <- nonfood_combined$val_335/nonfood_combined$val_349
nonfood_combined$share336 <- nonfood_combined$val_336/nonfood_combined$val_349
nonfood_combined$share337 <- nonfood_combined$val_337/nonfood_combined$val_349
nonfood_combined$share338 <- nonfood_combined$val_338/nonfood_combined$val_349
nonfood_combined$share340 <- nonfood_combined$val_340/nonfood_combined$val_349
nonfood_combined$share341 <- nonfood_combined$val_341/nonfood_combined$val_349
nonfood_combined$share342 <- nonfood_combined$val_342/nonfood_combined$val_349
nonfood_combined$share343 <- nonfood_combined$val_343/nonfood_combined$val_349
nonfood_combined$share344 <- nonfood_combined$val_344/nonfood_combined$val_349
nonfood_combined$share345 <- nonfood_combined$val_345/nonfood_combined$val_349

#creating prices

nonfood_combined$price330 <- nonfood_combined$val_330/nonfood_combined$qty_330
nonfood_combined$price331 <- nonfood_combined$val_331/nonfood_combined$qty_331
nonfood_combined$price332 <- nonfood_combined$val_332/nonfood_combined$qty_332
nonfood_combined$price333 <- nonfood_combined$val_333/nonfood_combined$qty_333
nonfood_combined$price334 <- nonfood_combined$val_334/nonfood_combined$qty_334
nonfood_combined$price335 <- nonfood_combined$val_335/nonfood_combined$qty_335
nonfood_combined$price336 <- nonfood_combined$val_336/nonfood_combined$qty_336
nonfood_combined$price337 <- nonfood_combined$val_337/nonfood_combined$qty_337
nonfood_combined$price338 <- nonfood_combined$val_338/nonfood_combined$qty_338
nonfood_combined$price340 <- nonfood_combined$val_340/nonfood_combined$qty_340
nonfood_combined$price341 <- nonfood_combined$val_341/nonfood_combined$qty_341
nonfood_combined$price342 <- nonfood_combined$val_342/nonfood_combined$qty_342
nonfood_combined$price343 <- nonfood_combined$val_343/nonfood_combined$qty_343
nonfood_combined$price344 <- nonfood_combined$val_344/nonfood_combined$qty_344
nonfood_combined$price345 <- nonfood_combined$val_345/nonfood_combined$qty_345


#Replacing NA in Prices with Sample Mean 

nonfood_combined$price330[is.na(nonfood_combined$price330)] <- mean(nonfood_combined$price330, na.rm = TRUE)
nonfood_combined$price331[is.na(nonfood_combined$price331)] <- mean(nonfood_combined$price331, na.rm = TRUE)
nonfood_combined$price332[is.na(nonfood_combined$price332)] <- mean(nonfood_combined$price332, na.rm = TRUE)
nonfood_combined$price333[is.na(nonfood_combined$price333)] <- mean(nonfood_combined$price333, na.rm = TRUE)
nonfood_combined$price334[is.na(nonfood_combined$price334)] <- mean(nonfood_combined$price334, na.rm = TRUE)
nonfood_combined$price335[is.na(nonfood_combined$price335)] <- mean(nonfood_combined$price335, na.rm = TRUE)
nonfood_combined$price336[is.na(nonfood_combined$price336)] <- mean(nonfood_combined$price336, na.rm = TRUE)
nonfood_combined$price337[is.na(nonfood_combined$price337)] <- mean(nonfood_combined$price337, na.rm = TRUE)
nonfood_combined$price338[is.na(nonfood_combined$price338)] <- mean(nonfood_combined$price338, na.rm = TRUE)
nonfood_combined$price340[is.na(nonfood_combined$price340)] <- mean(nonfood_combined$price340, na.rm = TRUE)
nonfood_combined$price341[is.na(nonfood_combined$price341)] <- mean(nonfood_combined$price341, na.rm = TRUE)
nonfood_combined$price342[is.na(nonfood_combined$price342)] <- mean(nonfood_combined$price342, na.rm = TRUE)
nonfood_combined$price343[is.na(nonfood_combined$price343)] <- mean(nonfood_combined$price343, na.rm = TRUE)
nonfood_combined$price344[is.na(nonfood_combined$price344)] <- mean(nonfood_combined$price344, na.rm = TRUE)
nonfood_combined$price345[is.na(nonfood_combined$price345)] <- mean(nonfood_combined$price345, na.rm = TRUE)

#Composite Price of Fuel and Light 

nonfood_combined$price_349 <- (nonfood_combined$share330*nonfood_combined$price330) + (nonfood_combined$share331*nonfood_combined$price331) + (nonfood_combined$share332*nonfood_combined$price332)+(nonfood_combined$share333*nonfood_combined$price333)+
  (nonfood_combined$share334*nonfood_combined$price334)+(nonfood_combined$share335*nonfood_combined$price335)+(nonfood_combined$share336*nonfood_combined$price336)+(nonfood_combined$share337*nonfood_combined$price337)+
  (nonfood_combined$share338*nonfood_combined$price338)+(nonfood_combined$share340*nonfood_combined$price340)+(nonfood_combined$share341*nonfood_combined$price341)+(nonfood_combined$share342*nonfood_combined$price342)+
  (nonfood_combined$share343*nonfood_combined$price343)+(nonfood_combined$share344*nonfood_combined$price344)+(nonfood_combined$share345*nonfood_combined$price345)

################################################################################

#For Clothing: 

#creating weights 

nonfood_combined$share350 <- nonfood_combined$val_350/nonfood_combined$val_379
nonfood_combined$share351 <- nonfood_combined$val_351/nonfood_combined$val_379
nonfood_combined$share352 <- nonfood_combined$val_352/nonfood_combined$val_379
nonfood_combined$share353 <- nonfood_combined$val_353/nonfood_combined$val_379
nonfood_combined$share354 <- nonfood_combined$val_354/nonfood_combined$val_379
nonfood_combined$share355 <- nonfood_combined$val_355/nonfood_combined$val_379
nonfood_combined$share356 <- nonfood_combined$val_356/nonfood_combined$val_379
nonfood_combined$share357 <- nonfood_combined$val_357/nonfood_combined$val_379
nonfood_combined$share358 <- nonfood_combined$val_358/nonfood_combined$val_379
nonfood_combined$share360 <- nonfood_combined$val_360/nonfood_combined$val_379
nonfood_combined$share361 <- nonfood_combined$val_361/nonfood_combined$val_379
nonfood_combined$share362 <- nonfood_combined$val_362/nonfood_combined$val_379
nonfood_combined$share363 <- nonfood_combined$val_363/nonfood_combined$val_379
nonfood_combined$share364 <- nonfood_combined$val_364/nonfood_combined$val_379
nonfood_combined$share365 <- nonfood_combined$val_365/nonfood_combined$val_379
nonfood_combined$share366 <- nonfood_combined$val_366/nonfood_combined$val_379
nonfood_combined$share367 <- nonfood_combined$val_367/nonfood_combined$val_379
nonfood_combined$share368 <- nonfood_combined$val_368/nonfood_combined$val_379
nonfood_combined$share370 <- nonfood_combined$val_370/nonfood_combined$val_379
nonfood_combined$share371 <- nonfood_combined$val_371/nonfood_combined$val_379
nonfood_combined$share372 <- nonfood_combined$val_372/nonfood_combined$val_379
nonfood_combined$share373 <- nonfood_combined$val_373/nonfood_combined$val_379
nonfood_combined$share374 <- nonfood_combined$val_374/nonfood_combined$val_379
nonfood_combined$share375 <- nonfood_combined$val_375/nonfood_combined$val_379
nonfood_combined$share376 <- nonfood_combined$val_376/nonfood_combined$val_379

#creating prices

nonfood_combined$price350 <- nonfood_combined$val_350/nonfood_combined$qty_350
nonfood_combined$price351 <- nonfood_combined$val_351/nonfood_combined$qty_351
nonfood_combined$price352 <- nonfood_combined$val_352/nonfood_combined$qty_352
nonfood_combined$price353 <- nonfood_combined$val_353/nonfood_combined$qty_353
nonfood_combined$price354 <- nonfood_combined$val_354/nonfood_combined$qty_354
nonfood_combined$price355 <- nonfood_combined$val_355/nonfood_combined$qty_355
nonfood_combined$price356 <- nonfood_combined$val_356/nonfood_combined$qty_356
nonfood_combined$price357 <- nonfood_combined$val_357/nonfood_combined$qty_357
nonfood_combined$price358 <- nonfood_combined$val_358/nonfood_combined$qty_358
nonfood_combined$price360 <- nonfood_combined$val_360/nonfood_combined$qty_360
nonfood_combined$price361 <- nonfood_combined$val_361/nonfood_combined$qty_361
nonfood_combined$price362 <- nonfood_combined$val_362/nonfood_combined$qty_362
nonfood_combined$price363 <- nonfood_combined$val_363/nonfood_combined$qty_363
nonfood_combined$price364 <- nonfood_combined$val_364/nonfood_combined$qty_364
nonfood_combined$price365 <- nonfood_combined$val_365/nonfood_combined$qty_365
nonfood_combined$price366 <- nonfood_combined$val_366/nonfood_combined$qty_366
nonfood_combined$price367 <- nonfood_combined$val_367/nonfood_combined$qty_367
nonfood_combined$price368 <- nonfood_combined$val_368/nonfood_combined$qty_368
nonfood_combined$price370 <- nonfood_combined$val_370/nonfood_combined$qty_370
nonfood_combined$price371 <- nonfood_combined$val_371/nonfood_combined$qty_371
nonfood_combined$price372 <- nonfood_combined$val_372/nonfood_combined$qty_372
nonfood_combined$price373 <- nonfood_combined$val_373/nonfood_combined$qty_373
nonfood_combined$price374 <- nonfood_combined$val_374/nonfood_combined$qty_374
nonfood_combined$price375 <- nonfood_combined$val_375/nonfood_combined$qty_375
nonfood_combined$price376 <- nonfood_combined$val_376/nonfood_combined$qty_376

#Replacing NA in Prices with Sample Mean 

nonfood_combined$price350[is.na(nonfood_combined$price350)] <- mean(nonfood_combined$price350, na.rm = TRUE)
nonfood_combined$price351[is.na(nonfood_combined$price351)] <- mean(nonfood_combined$price351, na.rm = TRUE)
nonfood_combined$price352[is.na(nonfood_combined$price352)] <- mean(nonfood_combined$price352, na.rm = TRUE)
nonfood_combined$price353[is.na(nonfood_combined$price353)] <- mean(nonfood_combined$price353, na.rm = TRUE)
nonfood_combined$price354[is.na(nonfood_combined$price354)] <- mean(nonfood_combined$price354, na.rm = TRUE)
nonfood_combined$price355[is.na(nonfood_combined$price355)] <- mean(nonfood_combined$price355, na.rm = TRUE)
nonfood_combined$price356[is.na(nonfood_combined$price356)] <- mean(nonfood_combined$price356, na.rm = TRUE)
nonfood_combined$price357[is.na(nonfood_combined$price357)] <- mean(nonfood_combined$price357, na.rm = TRUE)
nonfood_combined$price358[is.na(nonfood_combined$price358)] <- mean(nonfood_combined$price358, na.rm = TRUE)
nonfood_combined$price360[is.na(nonfood_combined$price360)] <- mean(nonfood_combined$price360, na.rm = TRUE)
nonfood_combined$price361[is.na(nonfood_combined$price361)] <- mean(nonfood_combined$price361, na.rm = TRUE)
nonfood_combined$price362[is.na(nonfood_combined$price362)] <- mean(nonfood_combined$price362, na.rm = TRUE)
nonfood_combined$price363[is.na(nonfood_combined$price363)] <- mean(nonfood_combined$price363, na.rm = TRUE)
nonfood_combined$price364[is.na(nonfood_combined$price364)] <- mean(nonfood_combined$price364, na.rm = TRUE)
nonfood_combined$price365[is.na(nonfood_combined$price365)] <- mean(nonfood_combined$price365, na.rm = TRUE)
nonfood_combined$price366[is.na(nonfood_combined$price366)] <- mean(nonfood_combined$price366, na.rm = TRUE)
nonfood_combined$price367[is.na(nonfood_combined$price367)] <- mean(nonfood_combined$price367, na.rm = TRUE)
nonfood_combined$price368[is.na(nonfood_combined$price368)] <- mean(nonfood_combined$price368, na.rm = TRUE)
nonfood_combined$price370[is.na(nonfood_combined$price370)] <- mean(nonfood_combined$price370, na.rm = TRUE)
nonfood_combined$price371[is.na(nonfood_combined$price371)] <- mean(nonfood_combined$price371, na.rm = TRUE)
nonfood_combined$price372[is.na(nonfood_combined$price372)] <- mean(nonfood_combined$price372, na.rm = TRUE)
nonfood_combined$price373[is.na(nonfood_combined$price373)] <- mean(nonfood_combined$price373, na.rm = TRUE)
nonfood_combined$price374[is.na(nonfood_combined$price374)] <- mean(nonfood_combined$price374, na.rm = TRUE)
nonfood_combined$price375[is.na(nonfood_combined$price375)] <- mean(nonfood_combined$price375, na.rm = TRUE)
nonfood_combined$price376[is.na(nonfood_combined$price376)] <- mean(nonfood_combined$price376, na.rm = TRUE)

#Composite Price of Clothing

nonfood_combined$price_379 <- (nonfood_combined$share350*nonfood_combined$price350) + 
  (nonfood_combined$share351*nonfood_combined$price351) + 
  (nonfood_combined$share352*nonfood_combined$price352)+(nonfood_combined$share353*nonfood_combined$price353)+
  (nonfood_combined$share354*nonfood_combined$price354)+(nonfood_combined$share355*nonfood_combined$price355)+
  (nonfood_combined$share356*nonfood_combined$price356)+(nonfood_combined$share357*nonfood_combined$price357)+
  (nonfood_combined$share358*nonfood_combined$price358)+(nonfood_combined$share360*nonfood_combined$price360)+
  (nonfood_combined$share361*nonfood_combined$price361)+(nonfood_combined$share362*nonfood_combined$price362)+
  (nonfood_combined$share363*nonfood_combined$price363)+(nonfood_combined$share364*nonfood_combined$price364)+
  (nonfood_combined$share365*nonfood_combined$price365)+(nonfood_combined$share366*nonfood_combined$price366)+
  (nonfood_combined$share367*nonfood_combined$price367)+(nonfood_combined$share368*nonfood_combined$price368)+
  (nonfood_combined$share370*nonfood_combined$price370)+(nonfood_combined$share371*nonfood_combined$price371)+
  (nonfood_combined$share372*nonfood_combined$price372)+(nonfood_combined$share373*nonfood_combined$price373)+
  (nonfood_combined$share374*nonfood_combined$price374)+(nonfood_combined$share375*nonfood_combined$price375)+
  (nonfood_combined$share376*nonfood_combined$price376)


################################################################################

#For Bedding:

#Creating weights

nonfood_combined$share380 <- nonfood_combined$val_380/nonfood_combined$val_389
nonfood_combined$share381 <- nonfood_combined$val_381/nonfood_combined$val_389
nonfood_combined$share382 <- nonfood_combined$val_382/nonfood_combined$val_389
nonfood_combined$share383 <- nonfood_combined$val_383/nonfood_combined$val_389
nonfood_combined$share384 <- nonfood_combined$val_384/nonfood_combined$val_389
nonfood_combined$share385 <- nonfood_combined$val_385/nonfood_combined$val_389

#creating prices

nonfood_combined$price380 <- nonfood_combined$val_380/nonfood_combined$qty_380
nonfood_combined$price381 <- nonfood_combined$val_381/nonfood_combined$qty_381
nonfood_combined$price382 <- nonfood_combined$val_382/nonfood_combined$qty_382
nonfood_combined$price383 <- nonfood_combined$val_383/nonfood_combined$qty_383
nonfood_combined$price384 <- nonfood_combined$val_384/nonfood_combined$qty_384
nonfood_combined$price385 <- nonfood_combined$val_385/nonfood_combined$qty_385

#Replacing NA prices with sample means 

nonfood_combined$price380[is.na(nonfood_combined$price380)] <- mean(nonfood_combined$price380, na.rm = TRUE)
nonfood_combined$price381[is.na(nonfood_combined$price381)] <- mean(nonfood_combined$price381, na.rm = TRUE)
nonfood_combined$price382[is.na(nonfood_combined$price382)] <- mean(nonfood_combined$price382, na.rm = TRUE)
nonfood_combined$price383[is.na(nonfood_combined$price383)] <- mean(nonfood_combined$price383, na.rm = TRUE)
nonfood_combined$price384[is.na(nonfood_combined$price384)] <- mean(nonfood_combined$price384, na.rm = TRUE)
nonfood_combined$price385[is.na(nonfood_combined$price385)] <- mean(nonfood_combined$price385, na.rm = TRUE)

#Composite Price of Bedding

nonfood_combined$price_389 <- (nonfood_combined$share380*nonfood_combined$price380) + (nonfood_combined$share381*nonfood_combined$price381)+
  (nonfood_combined$share382*nonfood_combined$price382)+(nonfood_combined$share383*nonfood_combined$price383)+(nonfood_combined$share384*nonfood_combined$price384)+
  (nonfood_combined$share385*nonfood_combined$price385)

################################################################################

#For Footwear:

#creating weights

nonfood_combined$share390 <- nonfood_combined$val_390/nonfood_combined$val_399
nonfood_combined$share391 <- nonfood_combined$val_391/nonfood_combined$val_399
nonfood_combined$share392 <- nonfood_combined$val_392/nonfood_combined$val_399
nonfood_combined$share393 <- nonfood_combined$val_393/nonfood_combined$val_399
nonfood_combined$share394 <- nonfood_combined$val_394/nonfood_combined$val_399
nonfood_combined$share395 <- nonfood_combined$val_395/nonfood_combined$val_399


#creating prices

nonfood_combined$price390 <- nonfood_combined$val_390/nonfood_combined$qty_390
nonfood_combined$price391 <- nonfood_combined$val_391/nonfood_combined$qty_391
nonfood_combined$price392 <- nonfood_combined$val_392/nonfood_combined$qty_392
nonfood_combined$price393 <- nonfood_combined$val_393/nonfood_combined$qty_393
nonfood_combined$price394 <- nonfood_combined$val_394/nonfood_combined$qty_394
nonfood_combined$price395 <- nonfood_combined$val_395/nonfood_combined$qty_395

#Replacing NA in prices with sample mean 

nonfood_combined$price390[is.na(nonfood_combined$price390)] <- mean(nonfood_combined$price390, na.rm = TRUE)
nonfood_combined$price391[is.na(nonfood_combined$price391)] <- mean(nonfood_combined$price391, na.rm = TRUE)
nonfood_combined$price392[is.na(nonfood_combined$price392)] <- mean(nonfood_combined$price392, na.rm = TRUE)
nonfood_combined$price393[is.na(nonfood_combined$price393)] <- mean(nonfood_combined$price393, na.rm = TRUE)
nonfood_combined$price394[is.na(nonfood_combined$price394)] <- mean(nonfood_combined$price394, na.rm = TRUE)
nonfood_combined$price395[is.na(nonfood_combined$price395)] <- mean(nonfood_combined$price395, na.rm = TRUE)

#Composite Price for footwear:

nonfood_combined$price_399 <- (nonfood_combined$share390*nonfood_combined$price390) + (nonfood_combined$share391*nonfood_combined$price391) + 
  (nonfood_combined$share392*nonfood_combined$price392) + (nonfood_combined$share393*nonfood_combined$price393) + (nonfood_combined$share394*nonfood_combined$price394) +
  (nonfood_combined$share395*nonfood_combined$price395)



################################################################################

#Selecting and Creating Relevant Variables

#Variable Descriptions and Item Codes:

# - Cereals is 129 
# - Cereal Substitutes is 139 
# - Pulses is 159 
# - Milk and milk products is 169
# - Salt and Sugar is 179
# - Edible oil is 189 
# - Meat Eggs Fish is 199
# - Vegetables is 219
# - Fresh Fruits is 239 
# - Fruits Dry is 249
# - Spices is 269
# - Beverages is 279
# - Served Processed Food is 289
# - Packaged Processed Food is 299
# - Paan is 309
# - Tobacco is 319
# - Intoxicants is 329
# - Fuel and Light is 349 
# - Clothing is 379
# - Bedding is 389
# - Footwear is 399


food_qty <- wide_quantity%>%dplyr::select("HHID", "qty_129", "qty_139", "qty_159", "qty_169", "qty_179", "qty_189", "qty_199", "qty_219", "qty_239", "qty_249", "qty_269", "qty_279", "qty_289", "qty_299", "qty_309", "qty_319", "qty_329", "qty_349", "qty_379", "qty_389", "qty_399")

food_val <- wide_value%>%dplyr::select("HHID", "val_129", "val_139", "val_159", "val_169", "val_179", "val_189", "val_199", "val_219", "val_239", "val_249", "val_269", "val_279", "val_289", "val_299", "val_309", "val_319", "val_329", "val_349", "val_379", "val_389", "val_399")


#Keeping only distinct values of the covariates data set 

unique_cov <- dplyr::distinct(covariates)

#Removing NA Observations in the food_qty and food_val data sets 

food_qty[is.na(food_qty)] <- 0
food_val[is.na(food_val)] <- 0

#Calculating Total Expenditure on Food using food_val dataset 

food_val <- food_val%>%dplyr::mutate(foodexp = val_129 + val_139 + val_159 + val_169 + val_179 + val_189 + val_199 + val_219 + val_239 + val_249 + val_269 + val_279 + val_289 + val_299 + val_309 + val_319 + val_329 + val_349 + val_379 + val_389 + val_399)

################################################################################

#Merging the food_qty, food_val and unique_cov datasets 

f1 <- dplyr::inner_join(food_qty, food_val, by = "HHID")
food_merged <- dplyr::inner_join(f1, unique_cov, by = "HHID")

food_merged$state <- substr(food_merged$State_Region, 1, 2)
food_merged$region <- substr(food_merged$State_Region, 3, 3)


################################################################################

#Converting quantities to kilogram (kg) terms for food by diving by 1000

food_merged <- food_merged%>%dplyr::mutate(across(c("qty_129", "qty_139", "qty_159", "qty_169", "qty_179", "qty_189", "qty_199", "qty_219", "qty_239", "qty_249", "qty_269", "qty_279", "qty_289", "qty_299", "qty_309", "qty_319", "qty_329"), function(x) x/1000))

################################################################################

#Creating Final Price Variables in the food_merged Data Set 

#Prices of Food Items: 

food_merged$price129 <- food_merged$val_129/food_merged$qty_129
food_merged$price139 <- food_merged$val_139/food_merged$qty_139
food_merged$price159 <- food_merged$val_159/food_merged$qty_159
food_merged$price169 <- food_merged$val_169/food_merged$qty_169
food_merged$price179 <- food_merged$val_179/food_merged$qty_179
food_merged$price189 <- food_merged$val_189/food_merged$qty_189
food_merged$price199 <- food_merged$val_199/food_merged$qty_199
food_merged$price219 <- food_merged$val_219/food_merged$qty_219
food_merged$price239 <- food_merged$val_239/food_merged$qty_239
food_merged$price249 <- food_merged$val_249/food_merged$qty_249
food_merged$price269 <- food_merged$val_269/food_merged$qty_269
food_merged$price279 <- food_merged$val_279/food_merged$qty_279
food_merged$price289 <- food_merged$val_289/food_merged$qty_289
food_merged$price299 <- food_merged$val_299/food_merged$qty_299
food_merged$price309 <- food_merged$val_309/food_merged$qty_309
food_merged$price319 <- food_merged$val_319/food_merged$qty_319
food_merged$price329 <- food_merged$val_329/food_merged$qty_329

###############################################################################

#Grouping and Price Imputation for Food Variables

#Creating the following categories:

# - Cereals and Cereal Substitutes (129 + 139)
# - Pulses (159)
# - Milk and Milk Products (169)
# - Edible Oils (189)
# - Meat Eggs and Fish (199)
# - Fruits and Vegetables (219, 239, 249)
# - Other Foods (269, 279, 289, 299, 309, 319, 329)
# - Fuel and Lights (349)
# - Clothing, Bedding and Footwear (379, 389, 399)

food_merged$ccs_val <- food_merged$val_129 + food_merged$val_139

food_merged$share129 <- food_merged$val_129/food_merged$ccs_val
food_merged$share139 <- food_merged$val_139/food_merged$ccs_val

food_merged$price129[is.na(food_merged$price129)] <- mean(food_merged$price129, na.rm = TRUE)
food_merged$price139[is.na(food_merged$price139)] <- mean(food_merged$price139, na.rm = TRUE)



food_merged$price_ccs <- (food_merged$share129*food_merged$price129) + (food_merged$share139*food_merged$price139)

food_merged$fv_val <- food_merged$val_219 + food_merged$val_239 + food_merged$val_249

food_merged$share219 <- food_merged$val_219/food_merged$fv_val
food_merged$share239 <- food_merged$val_239/food_merged$fv_val
food_merged$share249 <- food_merged$val_249/food_merged$fv_val

food_merged$price139[is.na(food_merged$price139)] <- mean(food_merged$price139, na.rm = TRUE)
food_merged$price139[is.na(food_merged$price139)] <- mean(food_merged$price139, na.rm = TRUE)
food_merged$price139[is.na(food_merged$price139)] <- mean(food_merged$price139, na.rm = TRUE)


food_merged$price_fv <- (food_merged$share219*food_merged$price219) + (food_merged$share239*food_merged$price239) + (food_merged$share249*food_merged$price249)

food_merged$otherfd <- food_merged$val_269+food_merged$val_279+food_merged$val_289+food_merged$val_299 +food_merged$val_309+
  food_merged$val_319+food_merged$val_329+ food_merged$val_179

food_merged$share269 <- food_merged$val_269/food_merged$otherfd
food_merged$share279 <- food_merged$val_279/food_merged$otherfd
food_merged$share289 <- food_merged$val_289/food_merged$otherfd
food_merged$share299 <- food_merged$val_299/food_merged$otherfd
food_merged$share309 <- food_merged$val_309/food_merged$otherfd
food_merged$share319 <- food_merged$val_319/food_merged$otherfd
food_merged$share329 <- food_merged$val_329/food_merged$otherfd
food_merged$share179 <- food_merged$val_179/food_merged$otherfd

food_merged$price179 <- food_merged$val_179/food_merged$qty_179
food_merged$price269 <- food_merged$val_269/food_merged$qty_269
food_merged$price279 <- food_merged$val_279/food_merged$qty_279
food_merged$price289 <- food_merged$val_289/food_merged$qty_289
food_merged$price299 <- food_merged$val_299/food_merged$qty_299
food_merged$price309 <- food_merged$val_309/food_merged$qty_309
food_merged$price319 <- food_merged$val_319/food_merged$qty_319
food_merged$price329 <- food_merged$val_329/food_merged$qty_329

food_merged$price179[is.na(food_merged$price179)] <- mean(food_merged$price179, na.rm = TRUE)
food_merged$price269[is.na(food_merged$price269)] <- mean(food_merged$price269, na.rm = TRUE)
food_merged$price279[is.na(food_merged$price279)] <- mean(food_merged$price279, na.rm = TRUE)
food_merged$price289[is.na(food_merged$price289)] <- mean(food_merged$price289, na.rm = TRUE)
food_merged$price299[is.na(food_merged$price299)] <- mean(food_merged$price299, na.rm = TRUE)
food_merged$price309[is.na(food_merged$price309)] <- mean(food_merged$price309, na.rm = TRUE)
food_merged$price319[is.na(food_merged$price319)] <- mean(food_merged$price319, na.rm = TRUE)
food_merged$price329[is.na(food_merged$price329)] <- mean(food_merged$price329, na.rm = TRUE)

food_merged$price_otherfd <- (food_merged$share269*food_merged$price269) + 
  (food_merged$share279*food_merged$price279) + 
  (food_merged$share289*food_merged$price289)+(food_merged$share299*food_merged$price299)+
  (food_merged$share309*food_merged$price309)+(food_merged$share319*food_merged$price319)+
  (food_merged$share329*food_merged$price329) + (food_merged$share179*food_merged$price179)


#Extracting Relevant Variables 

colnames(food_merged)

food_selected <- food_merged%>%dplyr::select("HHID" , "ccs_val" , "val_159", "val_169",  "val_189" , "val_199" ,"fv_val",  "otherfd" ,"price_ccs" ,"price159", "price169" ,"price189" , "price199" , "price_fv" , "price_otherfd" ,"foodexp",
                                             "State_Region", "Cooking_Code"  , "Lighting_Code"  ,  "Dwelling_unit_Code" ,      "Regular_salary_earner"  ,  "Possess_ration_card",     
                                             "type_of_ration_card" ,     "MPCE_URP"          ,       "MPCE_MRP"      ,           "HH_Size"      ,            "HH_Type"  ,               
                                             "Religion"      ,     "Social_Group" ,            "whether_Land_owned"  ,     "Land_total_possessed"    , "NSS" ,                  
                                             "NSC" ,         "MLT"  ,     "Combined_multiplier" , "Subsample_multiplier")

colnames(nonfood_combined)

nonfood_selected <- nonfood_combined%>%dplyr::select("HHID" , "val_349", "val_379", "val_389", "val_399", "price_349", "price_379", "price_389", "price_399"   )
################################################################################

#Merging Non-Food Composite Prices: food_selected merged with 

combined_selected <- inner_join(food_selected, nonfood_selected, by = "HHID")

#add non food val to foodexp to get total exp 

colnames(combined_selected)

combined_selected <- combined_selected%>%dplyr::mutate(totalexp = ccs_val + val_159 + val_169 + val_189 + val_199 + fv_val + otherfd + val_349 + val_379 + val_389 + val_399)


#Making the State Variable: Jharkhand (20) and Chattisgarh (22)

combined_selected$state <- substr(combined_selected$State_Region, 1, 2)
combined_selected$region <- substr(combined_selected$State_Region, 3,3)


################################################################################


#Extracting Data for Jharkhand (State Code 20) and Chattisgarh (State Code 22)

state_data <- combined_selected%>%dplyr::filter(state == 22 | state == 20)

################################################################################


#Correcting NA values in prices in Prices

print(sum(complete.cases(state_data)))
print(sum(is.na(state_data)))

#Finding the NAs

print(sum(is.na(state_data$price_ccs))) #87 NAs
print(sum(is.na(state_data$price159))) #136 NAs
print(sum(is.na(state_data$price169))) #2049 NAs
print(sum(is.na(state_data$price189))) #95 NAs
print(sum(is.na(state_data$price199))) #1436 NAs
print(sum(is.na(state_data$price_fv))) #3828 NAs
print(sum(is.na(state_data$price_otherfd))) #4465 NAs

state_data$price_ccs[is.infinite(state_data$price_ccs)] <- NA
state_data$price169[is.infinite(state_data$price169)] <- NA
state_data$price199[is.infinite(state_data$price199)] <- NA
state_data$price_otherfd[is.infinite(state_data$price_otherfd)] <- NA
state_data$price_fv[is.infinite(state_data$price_fv)] <- NA



#Imputing Values to correct for NA observations 

state_data$price_ccs[is.na(state_data$price_ccs)] <- mean(state_data$price_ccs, na.rm = TRUE)

print(sum(is.na(state_data$price_ccs))) 

state_data$price159[is.na(state_data$price159)] <- mean(state_data$price159, na.rm = TRUE)

print(sum(is.na(state_data$price159)))

state_data$price169[is.na(state_data$price169)] <- mean(state_data$price169, na.rm = TRUE)

print(sum(is.na(state_data$price169)))

state_data$price189[is.na(state_data$price189)] <- mean(state_data$price189, na.rm = TRUE)

print(sum(is.na(state_data$price189)))

state_data$price199[is.na(state_data$price199)] <- mean(state_data$price199, na.rm = TRUE)

print(sum(is.na(state_data$price199)))

state_data$price_fv[is.na(state_data$price_fv)] <- mean(state_data$price_fv, na.rm = TRUE)

print(sum(is.na(state_data$price_fv)))

state_data$price_otherfd[is.na(state_data$price_otherfd)] <- mean(state_data$price_otherfd, na.rm = TRUE)

print(sum(is.na(state_data$price_otherfd)))

################################################################################

#Calculating Expenditure Shares 

state_data$expenditure_share_ccs <- state_data$ccs_val/state_data$totalexp
state_data$expenditure_share_159 <- state_data$val_159/state_data$totalexp
state_data$expenditure_share_169 <- state_data$val_169/state_data$totalexp
state_data$expenditure_share_189 <- state_data$val_189/state_data$totalexp
state_data$expenditure_share_199 <- state_data$val_199/state_data$totalexp
state_data$expenditure_share_fv <- state_data$fv_val/state_data$totalexp
state_data$expenditure_share_othfood <- state_data$otherfd/state_data$totalexp
state_data$expenditure_share_349 <- state_data$val_349/state_data$totalexp
state_data$expenditure_share_379 <- state_data$val_379/state_data$totalexp
state_data$expenditure_share_389 <- state_data$val_389/state_data$totalexp
state_data$expenditure_share_399 <- state_data$val_399/state_data$totalexp


################################################################################


#Downloading the Final Data Set


#colnames(state_data)[colnames(state_data) == "NSC.x"] <- "NSC"
#colnames(state_data)[colnames(state_data) == "MLT.x"] <- "MLT"
#colnames(state_data)[colnames(state_data) == "Combined_multiplier.y.y"] <- "Combined_Multiplier"
#colnames(state_data)[colnames(state_data) == "Subsample_multiplier.y.y"] <- "Subsample_Multiplier"




write_dta(data = state_data, "state_data.dta")
