# PURPOSE: To clean VAERS Data that is used for analysis


# Import data needed ------------------------------------------------------

library(tidyverse)

filesData <- list.files("~/VAERS-Project-ST-495/RawData/RegularData")

setwd("~/VAERS-Project-ST-495/RawData/RegularData")

vaersData <- bind_rows(lapply(filesData, read_csv))

filesSymptoms <- list.files("~/VAERS-Project-ST-495/RawData/SymptomData") # Imports all VAERS data (big data sets)

setwd("~/VAERS-Project-ST-495/RawData/SymptomData")

symptomData <- bind_rows(lapply(filesSymptoms, read_csv)) # Imports all symptoms data

setwd("~/VAERS-Project-ST-495")


# Manipulate data how needed ----------------------------------------------

# Do VAERS Data Here
vaersDataUpdated <- vaersData %>%
  dplyr::select(-SYMPTOM_TEXT) %>%
  mutate(RECVDATE = as.Date(RECVDATE, tryFormats = "%m/%d/%Y"),
         RPT_DATE = as.Date(RPT_DATE, tryFormats = "%m/%d/%Y"),
         DATEDIED = as.Date(DATEDIED, tryFormats = "%m/%d/%Y"),
         VAX_DATE = as.Date(VAX_DATE, tryFormats = "%m/%d/%Y"),
         ONSET_DATE = as.Date(ONSET_DATE, tryFormats = "%m/%d/%Y"),
         TODAYS_DATE = as.Date(TODAYS_DATE, tryFormats = "%m/%d/%Y"),
         DIED = ifelse(is.na(DIED), "N", DIED),
         L_THREAT = ifelse(is.na(L_THREAT), "N", L_THREAT),
         ER_ED_VISIT = ifelse(is.na(ER_ED_VISIT), "N", ER_ED_VISIT),
         HOSPITAL = ifelse(is.na(HOSPITAL), "N", HOSPITAL),
         HOSPDAYS = ifelse(is.na(HOSPDAYS), 0, HOSPDAYS),
         X_STAY = ifelse(is.na(X_STAY), "N", X_STAY),
         DISABLE = ifelse(is.na(DISABLE), "N", DISABLE),
         RECOVD = ifelse(is.na(RECOVD), "U", RECOVD),
         V_ADMINBY = ifelse(is.na(V_ADMINBY), "UNK", V_ADMINBY),
         V_FUNDBY = ifelse(is.na(V_FUNDBY), "UNK", V_FUNDBY),
         BIRTH_DEFECT = ifelse(is.na(BIRTH_DEFECT), "N", BIRTH_DEFECT),
         OFC_VISIT = ifelse(is.na(OFC_VISIT), "N", OFC_VISIT)
  )

# Do Symptoms Data Here
symptomDataSelected <- symptomData %>%
  dplyr::select(VAERS_ID, SYMPTOM1, SYMPTOM2, SYMPTOM3, SYMPTOM4, SYMPTOM5)

symptomDataUpdated <- symptomDataSelected %>%
  group_by(VAERS_ID) %>%
  mutate(SYMPTOM_TEXT = paste(c(SYMPTOM1, SYMPTOM2, SYMPTOM3, SYMPTOM4, SYMPTOM5), collapse = ", ")) %>%
  dplyr::select(VAERS_ID, SYMPTOM_TEXT) %>%
  ungroup() 

# Clean the SYMPTOM_TEXT column up
symptomDataUpdated <- symptomDataUpdated[! duplicated(symptomDataUpdated), ]
symptomDataUpdated$SYMPTOM_TEXT <- gsub("NA, ", "", symptomDataUpdated$SYMPTOM_TEXT)
symptomDataUpdated$SYMPTOM_TEXT <- gsub(", NA", "", symptomDataUpdated$SYMPTOM_TEXT)
symptomDataUpdated$SYMPTOM_TEXT <- ifelse(grepl("No adverse event", symptomDataUpdated$SYMPTOM_TEXT), "None", symptomDataUpdated$SYMPTOM_TEXT)

# Merge Data Into One File
finalVAERSData <- merge(vaersDataUpdated, symptomDataUpdated, by = "VAERS_ID") # Note there are 5 people who are in the regular data file but did not get the vaccine so we excluded them.

finalVAERSData <- finalVAERSData %>%
  dplyr::select(-VAERS_ID)


# Separate Data By Years --------------------------------------------------

n1 <- as.Date("2017-01-01")
n2 <- as.Date("2018-01-01")
n3 <- as.Date("2019-01-01")
n4 <- as.Date("2020-01-01")
n5 <- as.Date("2021-01-01")

finalVAERSData2016 <- finalVAERSData %>%
  filter(RECVDATE < n1)
finalVAERSData2017 <- finalVAERSData %>%
  filter(between(RECVDATE, n1, n2-1))
finalVAERSData2018 <- finalVAERSData %>%
  filter(between(RECVDATE, n2, n3-1))
finalVAERSData2019 <- finalVAERSData %>%
  filter(between(RECVDATE, n3, n4-1))
finalVAERSData2020 <- finalVAERSData %>%
  filter(between(RECVDATE, n4, n5-1))
finalVAERSData2021 <- finalVAERSData %>%
  filter(RECVDATE >= n5)


# Make files of new data --------------------------------------------------

write_csv(finalVAERSData, "~/VAERS-Project-ST-495/CreatedData/2016-21VAERSData.csv") #Full data set

write_csv(finalVAERSData2016, "~/VAERS-Project-ST-495/CreatedData/2016VAERSData.csv") #2016 data set

write_csv(finalVAERSData2017, "~/VAERS-Project-ST-495/CreatedData/2017VAERSData.csv") #2017 data set

write_csv(finalVAERSData2018, "~/VAERS-Project-ST-495/CreatedData/2018VAERSData.csv") #2018 data set

write_csv(finalVAERSData2019, "~/VAERS-Project-ST-495/CreatedData/2019VAERSData.csv") #2019 data set

write_csv(finalVAERSData2020, "~/VAERS-Project-ST-495/CreatedData/2020VAERSData.csv") #2020 data set

write_csv(finalVAERSData2021, "~/VAERS-Project-ST-495/CreatedData/2021VAERSData.csv") #2021 data set