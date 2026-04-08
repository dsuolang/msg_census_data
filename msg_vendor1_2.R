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
    lengthofresidence, ownrent, numadults, numchildren,
    # Person-level
    pid, age, gender, maritalstatus, education,
    ethnicgroup, hispaniccountryoforigin, hispanicsurname,
    asiansurname, languagespoken
  )

# 1) Household-level retained variables (one row per MSGID)
vendor1_hh_core <- vendor1_hh %>%
  select(
    msgid, lifestylegroup, esthhincome, esthomevalue, yearhomebuilt,
    lengthofresidence, ownrent, numadults, numchildren
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
    asiansurname, languagespoken
  ) %>%
  pivot_wider(
    names_from  = person_num,
    values_from = c(age, gender, maritalstatus, education, ethnicgroup,
                    hispaniccountryoforigin, hispanicsurname, asiansurname, languagespoken),
    names_glue  = "{.value}_{person_num}"
  )

# 4) Final dataset
vendor1_demos_final <- vendor1_hh_core %>%
  left_join(vendor1_hh_summ, by = "msgid") %>%
  left_join(vendor1_people_wide, by = "msgid")
write.csv(vendor1_demos_final, "vendor1_demos_final.csv", row.names = FALSE)



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
