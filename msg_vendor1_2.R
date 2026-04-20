library(dplyr)
library(tidyr)

setwd("/Users/dsuolang/Desktop/FRAME/MSG_Data")

vendor1_demos <- read.csv("Vendor1_Demos.csv")
vendor2_demos <- read.csv("Vendor2_Demos.csv")
mi_puma_sample <- read.csv("MI_PUMA_Sample.csv")

# lower-case variable names
colnames(mi_puma_sample)<-tolower(colnames(mi_puma_sample))
names(vendor1_demos) <- tolower(names(vendor1_demos))
names(vendor2_demos) <- tolower(names(vendor2_demos))

### A.	MI_PUMA_Sample.csv ### 
mi_puma_sample_sel <- mi_puma_sample %>%
  rename_with(tolower) %>%
  select(
    msgid,
    routetype,
    deliverypointusagecode,
    dropcount,
    seasonalcode,
    vacantcode,
    pumaid,
    fips,
    tract,
    blockgroup,
    block,
    latitude,
    longitude,
    dwellingtype
  )



### B.	Vendor1_Demos.csv ### 
# count unique values
vendor1_demos %>%
  summarise(
    n_msgid = n_distinct(msgid),
    n_pid   = n_distinct(pid)
  )

vendor1_hh <- vendor1_demos %>%
  rename_with(tolower) %>%
  # keep only fields we need (hh + person)
  select(
    msgid,
    # Household-level
    lifestylegroup, esthhincome, esthomevalue, yearhomebuilt,
    lengthofresidence, ownrent, numadults, numchildren, dwellingunitsize,
    # Person-level
    pid, age, gender, maritalstatus, education,
    ethnicgroup, hispaniccountryoforigin, hispanicsurname,
    asiansurname, languagespoken, occupation
  )

# 1) Household-level retained variables (one row per MSGID)
vendor1_hh_core <- vendor1_hh %>%
  select(
    msgid, lifestylegroup, esthhincome, esthomevalue, yearhomebuilt,
    lengthofresidence, ownrent, numadults, numchildren, dwellingunitsize
  ) %>%
  distinct(msgid, .keep_all = TRUE)

# 2) Person-derived household summaries
vendor1_hh_summ <- vendor1_hh %>%
  group_by(msgid) %>%
  summarise(
    has_vendor_data   = any(!is.na(pid)),
    n_persons         = n_distinct(pid, na.rm = TRUE),
    highest_education = suppressWarnings(max(education, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    highest_education = ifelse(is.infinite(highest_education), NA, highest_education)
  )

# 3) Person-level characteristics widened to 1 row per MSGID (up to 16 people)
vendor1_people_wide <- vendor1_hh %>%
  filter(!is.na(pid)) %>%
  group_by(msgid) %>%
  mutate(person_num = dense_rank(pid)) %>%
  ungroup() %>%
  filter(person_num <= 16) %>%
  select(
    msgid, person_num,
    age, gender, maritalstatus, education,
    ethnicgroup, hispaniccountryoforigin, hispanicsurname,
    asiansurname, languagespoken, occupation
  ) %>%
  pivot_wider(
    names_from  = person_num,
    values_from = c(age, gender, maritalstatus, education, ethnicgroup, 
                    hispaniccountryoforigin, hispanicsurname, asiansurname, languagespoken, occupation),
    names_glue  = "{.value}_{person_num}"
  )

# 4) Final dataset
vendor1_demos_final <- vendor1_hh_core %>%
  left_join(vendor1_hh_summ, by = "msgid") %>%
  left_join(vendor1_people_wide, by = "msgid")
write.csv(vendor1_demos_final, "vendor1_demos_final.csv", row.names = FALSE)

# 5) Value recoding
df <- vendor1_demos_final
# =======standardize missing values cleaning
df[] <- lapply(df, function(x) {
  if (is.character(x)) {
    x <- trimws(x)
    
    x[x %in% c(
      "", " ", "NA", "N/A",
      "Missing", "missing",
      "Unknown", "unknown", 
      "NULL", "null"
    )] <- NA
  }
  x
})

# =======age groups
age_cols <- grep("^age_", names(df), value = TRUE)
df[age_cols] <- lapply(df[age_cols], function(x) {
  x <- as.numeric(x)
  
  case_when(
    x >= 18 & x <= 34 ~ "18-34",
    x >= 35 & x <= 49 ~ "35-49",
    x >= 50 & x <= 64 ~ "50-64",
    x >= 65 ~ "65+",
    TRUE ~ NA_character_
  )
})
# =======gender
gender_cols <- grep("^gender_", names(df), value = TRUE)

df[gender_cols] <- lapply(df[gender_cols], function(x) {
  x <- toupper(trimws(as.character(x)))
  
  case_when(
    x == "M" ~ "m",
    x == "F" ~ "f",
    TRUE ~ NA_character_
  )
})
# =======education
edu_cols <- grep("^education_", names(df), value = TRUE)
df[edu_cols] <- lapply(df[edu_cols], function(x) {
  x <- as.numeric(x)
  case_when(
    x == 1 ~ "high school diploma",
    x == 2 ~ "some college or vocational school",
    x == 3 ~ "bachelor degree",
    x %in% c(4,5,6) ~ "graduate degree",
    TRUE ~ NA_character_
  )
})
# =======ethnicity (hispanic override if from hispanic origin country)
eth_cols <- grep("^ethnicgroup_", names(df), value = TRUE)
#hisp_cols <- grep("^hispaniccountryoforigin_", names(df), value = TRUE)
hisp_codes <- c(13)
white_codes <- c(5,6,7,8,9,10,11,15,16,23,25,26,27,28,29,30,33)
black_codes <- c(1,2)
other_codes <- c(3,4,12,14,17,18,19,20,21,22,24,32)
df[eth_cols] <- Map(function(eth, hisp) {
  eth <- as.numeric(eth)
  #hisp <- as.numeric(hisp)
  case_when(
    eth %in% hisp_codes  ~ "hispanic",
    eth %in% black_codes ~ "non-hispanic black",
    eth %in% white_codes ~ "non-hispanic white",
    eth %in% other_codes ~ "other",
    TRUE ~ NA_character_
  )
}, df[eth_cols])

# =======language
lang_cols <- grep("^languagespoken_", names(df), value = TRUE)
asian_lang <- c(50,53,56,57,58,62)
other_lang <- c(34,46,68,70,
                10,17,19,21,22,30,33)
df[lang_cols] <- lapply(df[lang_cols], function(x) {
  x <- as.numeric(x)
  case_when(
    x == 1 ~ "english",
    x == 20 ~ "spanish",
    x %in% asian_lang ~ "asian languages",
    x %in% other_lang ~ "other languages",
    TRUE ~ NA_character_
  )
})
# =======occupation
occ_cols <- grep("^occupation_", names(df), value = TRUE)

not_employed <- c(1,8,9,52)
mgmt <- c(3,4,10,20)
self_home <- c(11,16)
prof <- c(2,23,32,33,34,36,51,38)
health_edu_social <- c(17,21,39,40,41,42,43,44,45,46,47,48,49,50)
service <- c(5,6,22,18,37,53,30,31,54)
manual <- c(7,12,15,35)
gov_mil_rel <- c(13,14,55)

df[occ_cols] <- lapply(df[occ_cols], function(x) {
  x <- as.numeric(x)
  
  dplyr::case_when(
    x %in% not_employed ~ "notemployed",
    x %in% mgmt ~ "management/business",
    x %in% self_home ~ "selfemployed/home-based",
    x %in% prof ~ "professional/technical",
    x %in% health_edu_social ~ "health/education/social",
    x %in% service ~ "sales/admin/finance/services/other",
    x %in% manual ~ "skilled/labor/manual/agriculture",
    x %in% gov_mil_rel ~ "government/military/religious",
    TRUE ~ NA_character_
  )
})
# =======lifestyle group
life_cols <- grep("^lifestylegroup_", names(df), value = TRUE)
df[life_cols] <- lapply(df[life_cols], function(x) {
  x <- as.numeric(x)
  case_when(
    x >= 1 & x <= 4 ~ "high income",
    x >= 5 & x <= 8 ~ "above average",
    x >= 9 & x <= 12 ~ "below average",
    x >= 13 & x <= 16 ~ "lower income",
    TRUE ~ NA_character_
  )
})
# =======income quintiles
income_cols <- grep("^esthhincome", names(df), value = TRUE)
q1 <- c(1,15,20,25,30)
q2 <- c(35,40,45,50,55,60,65,70)
q3 <- c(75,80,85,90,95,100,105,110,115,120,125,130,135,140,145)
q4 <- c(150,160,170,175,190)
q5 <- c(200,225,250)
df[income_cols] <- lapply(df[income_cols], function(x) {
  x <- as.numeric(x)
  case_when(
    x %in% q1 ~ "q1",
    x %in% q2 ~ "q2",
    x %in% q3 ~ "q3",
    x %in% q4 ~ "q4",
    x %in% q5 ~ "q5",
    TRUE ~ NA_character_
  )
})
# =======number of adults
adult_cols <- grep("^numadults", names(df), value = TRUE)
df[adult_cols] <- lapply(df[adult_cols], function(x) {
  x <- as.numeric(x)
  case_when(
    x == 1 ~ "1",
    x == 2 ~ "2",
    x >= 3 ~ ">=3",
    TRUE ~ NA_character_
  )
})
# =======length of residence
res_cols <- grep("^lengthofresidence", names(df), value = TRUE)
df[res_cols] <- lapply(df[res_cols], function(x) {
  x <- as.numeric(x)
  case_when(
    x >= 0 & x <= 4 ~ "0-4",
    x >= 5 & x <= 11 ~ "5-11",
    x >= 12 & x <= 19 ~ "12-19",
    x >= 20 ~ "20+",
    TRUE ~ NA_character_
  )
})
# =======dwelling unit size
du_cols <- grep("^dwellingunitsize", names(df), value = TRUE)
df[du_cols] <- lapply(df[du_cols], function(x) {
  x <- toupper(trimws(as.character(x)))
  case_when(
    x == "A" ~ "one unit",
    x %in% c("B","C","D","E","F","G","H","I") ~ "multiple units",
    TRUE ~ NA_character_
  )
})

# final recoded dataset
vendor1_demos_final_recoded <- df
write.csv(vendor1_demos_final_recoded, "vendor1_demos_final_recoded.csv", row.names = FALSE)



### C.	Vendor2_Demos.csv ### 
vendor2_demos %>%
  summarise(
    n_msgid = n_distinct(msgid),
    n_pid   = n_distinct(pid)
  )

vendor2_hh <- vendor2_demos %>%
  rename_with(tolower) %>%
  select(
    msgid,
    lifestylegroup, esthhincome, lengthofresidence, ownrent,
    numadults, numchildren,
    singleparent_in_hh, workingwoman_in_hh, hh_parties_affiliation, veteran_in_hh,
    pid, age, gender, maritalstatus, education,
    ethnicgroup, hispaniccountryoforigin, hispanicsurname,
    asiansurname, languagespoken,
    occupation, registeredvoter, individualpartyaffiliation
  )

# 1) Household-level retained variables (one row per MSGID)
vendor2_hh_core <- vendor2_hh %>%
  select(
    msgid,
    lifestylegroup, esthhincome, lengthofresidence, ownrent,
    numadults, numchildren,
    singleparent_in_hh, workingwoman_in_hh, hh_parties_affiliation, veteran_in_hh
  ) %>%
  distinct(msgid, .keep_all = TRUE)

# 2) Person-derived household summaries
vendor2_hh_summ <- vendor2_hh %>%
  group_by(msgid) %>%
  summarise(
    has_vendor_data   = any(!is.na(pid)),
    n_persons         = n_distinct(pid, na.rm = TRUE),
    highest_education = suppressWarnings(max(education, na.rm = TRUE)),
    any_registered_voter = any(
      !is.na(registeredvoter) &
        (
          registeredvoter %in% c(1, "1", TRUE, "true", "TRUE", "Y", "y", "Yes", "YES")
        )
    ),
    .groups = "drop"
  ) %>%
  mutate(
    highest_education = ifelse(is.infinite(highest_education), NA, highest_education)
  )

# 3) Person-level characteristics widened (up to 9 people)
vendor2_people_wide <- vendor2_hh %>%
  filter(!is.na(pid)) %>%
  group_by(msgid) %>%
  mutate(person_num = dense_rank(pid)) %>%
  ungroup() %>%
  filter(person_num <= 9) %>%
  select(
    msgid, person_num,
    age, gender, maritalstatus, education,
    ethnicgroup, hispaniccountryoforigin, hispanicsurname,
    asiansurname, languagespoken,
    occupation, registeredvoter, individualpartyaffiliation
  ) %>%
  pivot_wider(
    names_from  = person_num,
    values_from = c(age, gender, maritalstatus, education, ethnicgroup,
                    hispaniccountryoforigin, hispanicsurname, asiansurname, languagespoken,
                    occupation, registeredvoter, individualpartyaffiliation),
    names_glue  = "{.value}_{person_num}"
  )

# 4) Final dataset
vendor2_demos_final <- vendor2_hh_core %>%
  left_join(vendor2_hh_summ, by = "msgid") %>%
  left_join(vendor2_people_wide, by = "msgid")

write.csv(vendor2_demos_final, "vendor2_demos_final.csv", row.names = FALSE)

# 5) Value recoding
df <- vendor2_demos_final

# =======standardize missing values
df[] <- lapply(df, function(x) {
  if (is.character(x)) {
    x <- trimws(x)
    
    x[x %in% c(
      "", " ", "NA", "N/A",
      "Missing", "missing",
      "Unknown", "unknown", 
      "NULL", "null"
    )] <- NA
  }
  x
})

# =======age groups
age_cols <- grep("^age_", names(df), value = TRUE)
df[age_cols] <- lapply(df[age_cols], function(x) {
  x <- as.numeric(x)
  case_when(
    x >= 18 & x <= 34 ~ "18-34",
    x >= 35 & x <= 49 ~ "35-49",
    x >= 50 & x <= 64 ~ "50-64",
    x >= 65 ~ ">=65",
    TRUE ~ NA_character_
  )
})
# =======gender
gender_cols <- grep("^gender_", names(df), value = TRUE)
df[gender_cols] <- lapply(df[gender_cols], function(x) {
  x <- toupper(trimws(as.character(x)))
  case_when(
    x == "M" ~ "m",
    x == "F" ~ "f",
    TRUE ~ NA_character_
  )
})
# =======marital status
mar_cols <- grep("^maritalstatus_", names(df), value = TRUE)
df[mar_cols] <- lapply(df[mar_cols], function(x) {
  x <- toupper(trimws(as.character(x)))
  case_when(
    x %in% c("A", "M") ~ "m",
    x %in% c("B", "S") ~ "s",
    TRUE ~ NA_character_
  )
})
# =======education
edu_cols <- grep("^education_", names(df), value = TRUE)
df[edu_cols] <- lapply(df[edu_cols], function(x) {
  x <- as.numeric(x)
  case_when(
    x == 1 ~ "less than high school",
    x == 2 ~ "high school diploma",
    x %in% c(3,4) ~ "some college or vocational school",
    x == 5 ~ "bachelor degree",
    x == 6 ~ "graduate degree",
    TRUE ~ NA_character_
  )
})
# =======ethnicity (hispanic override if from hispanic origin country)
eth_cols <- grep("^ethnicgroup_", names(df), value = TRUE)
#hisp_cols <- grep("^hispaniccountryoforigin_", names(df), value = TRUE)
hisp_codes <- c(13)
white_codes <- c(5,6,7,8,9,10,11,15,16,23,25,26,27,28,29,30,33)
black_codes <- c(1,2)
other_codes <- c(3,4,12,14,17,18,19,20,21,22,24,32)
df[eth_cols] <- Map(function(eth, hisp) {
  eth <- as.numeric(eth)
  #hisp <- as.numeric(hisp)
  case_when(
    eth %in% hisp_codes ~  "hispanic",
    eth %in% black_codes ~ "non-hispanic black",
    eth %in% white_codes ~ "non-hispanic white",
    eth %in% other_codes ~ "other",
    TRUE ~ NA_character_
  )
}, df[eth_cols])
# =======language
lang_cols <- grep("^languagespoken_", names(df), value = TRUE)
asian_lang <- c(48,49,50,51,52,53,54,56,57,58,59,60,61,62,63,64,65,
               "7A","7E","80","7F","94","9N")
other_lang <- c(27,29,31,32,34,44,45,46,47,68,70,
                3,4,5,6,7,8,9,10,12,13,14,17,19,21,22,23,24,25,
                30,35,36,37,38,40,41,
                "95","97","9E","9F","9J","9K","9L","9O","9R","9S")
df[lang_cols] <- lapply(df[lang_cols], function(x) {
  x <- as.character(x)
  case_when(
    x == "01" ~ "english",
    x == "20" ~ "spanish",
    x %in% asian_lang ~ "asian languages",
    x %in% other_lang ~ "other languages",
    TRUE ~ NA_character_
  )
})
# =======occupation
occ_cols <- grep("^occupation_", names(df), value = TRUE)

df[occ_cols] <- lapply(df[occ_cols], function(x) {
  x <- as.character(x)
  
  dplyr::case_when(
    x %in% c("F","G","H") ~ "notemployed",
    
    x %in% c("B") ~ "management/business",
    
    x %in% c("L","M","N","O","P","Q","R","S","T","U") ~ "selfemployed/home-based",
    
    x %in% c("A","W","X") ~ "professional/technical",
    
    x %in% c("V","Y") ~ "health/education/social",
    
    x %in% c("C","D","2","3","4","Z") ~ "sales/admin/finance/services/other",
    
    x %in% c("E","I","6","8") ~ "skilled/labor/manual/agriculture",
    
    x %in% c("J","K","1") ~ "government/military/religious",
    
    TRUE ~ NA_character_
  )
})
# =======registered voter
rv_cols <- grep("^registeredvoter_", names(df), value = TRUE)
df[rv_cols] <- lapply(df[rv_cols], function(x) {
  x <- as.character(x)
  case_when(
    x == "Y" ~ 1,
    TRUE ~ NA_real_
  )
})

# =======lifestyle group
life_cols <- grep("^lifestylegroup_", names(df), value = TRUE)
df[life_cols] <- lapply(df[life_cols], function(x) {
  x <- as.numeric(x)
  case_when(
    x >= 1 & x <= 4 ~ "high income",
    x >= 5 & x <= 8 ~ "above average",
    x >= 9 & x <= 12 ~ "below average",
    x >= 13 & x <= 16 ~ "lower income",
    TRUE ~ NA_character_
  )
})
# =======estimated income
income_cols <- grep("^esthhincome", names(df), value = TRUE)
df[income_cols] <- lapply(df[income_cols], function(x) {
  x <- as.character(x)
  case_when(
    x %in% c("A","B","C") ~ "q1",
    x %in% c("D","E") ~ "q2",
    x %in% c("F","G","H") ~ "q3",
    x %in% c("I","J") ~ "q4",
    x %in% c("K","L") ~ "q5",
    TRUE ~ NA_character_
  )
})
# =======ownrent
df$ownrent <- toupper(trimws(as.character(df$ownrent)))
df$ownrent <- case_when(
  df$ownrent %in% c("H","9") ~ "h",
  df$ownrent == "R" ~ "r",
  TRUE ~ NA_character_
)
# =======number of adults
df$numadults <- as.numeric(df$numadults)
df$numadults <- case_when(
  df$numadults == 1 ~ "1",
  df$numadults == 2 ~ "2",
  df$numadults >= 3 ~ ">=3",
  TRUE ~ NA_character_
)
# =======length of residence
df$lengthofresidence <- as.numeric(df$lengthofresidence)
df$lengthofresidence <- case_when(
  df$lengthofresidence >= 0 & df$lengthofresidence <= 4 ~ "0-4",
  df$lengthofresidence >= 5 & df$lengthofresidence <= 11 ~ "5-11",
  df$lengthofresidence >= 12 & df$lengthofresidence <= 19 ~ "12-19",
  df$lengthofresidence >= 20 ~ ">=20",
  TRUE ~ NA_character_
)
# =======additional household indicators (Y = 1)
bin_vars <- c("singleparent_in_hh",
              "workingwoman_in_hh",
              "hh_parties_affiliation",
              "veteran_in_hh")
for (v in bin_vars) {
  df[[v]] <- as.character(df[[v]])
  df[[v]] <- case_when(
    df[[v]] == "Y" ~ 1,
    TRUE ~ NA_real_
  )
}

# final recoded dataset
vendor2_demos_final_recoded <- df
write.csv(vendor1_demos_final_recoded, "vendor2_demos_final_recoded.csv", row.names = FALSE)