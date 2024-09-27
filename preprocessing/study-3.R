#Load packages
library(tidyverse) #for data wrangling

############
#Data wrangling
############

dataDir <- "data/study-3.csv"

set.seed(54865) #set seed for reproducable code
#Qualtrics csv output is (m)essy
dat.m <- read_csv(dataDir)
#1. Remove redundant rows
dat.m <- dat.m[-c(1:2), ]
#2. Embedded data and other variables are used by Qualtrics as background variables
# Not required for analysis
embedVars <- c("UniqueID", "code", "Progress", "Finished", "gc", "Q_TotalDuration")
ev <- embedVars[{embedVars %in% colnames(dat.m)}]

dat.m <- dat.m %>%
    select(-ev)

#3. Rename variables
#Create function to assist
rename_v <- function(tb, orig, new){
  #Take tibble tb, replaces part of a variable name (orig) with a new part (new), for all vars
  tb %>%
    rename_at(vars(contains(!!orig)), 
              funs(str_replace(., orig, new))) %>%
    return()
}
#Rename variables
#Firstly, for Qualtrics administration of Qsort
dat.clean <- dat.m %>% 
  rename(time_start = StartDate) %>%
  rename(time_end = EndDate) %>%
  rename(time_to_complete = Duration..in.seconds.) %>%
  rename_v("Q2_", "pif") %>%
  rename(pcf = Q3) %>%
  rename_v("Q5_", "pcf_") %>%
  rename(age = Q7) %>%
  rename(gender = Q8) %>%
  rename_v("Q9_", "demographics_") %>%
  rename_v("Q11", "code_entered") %>%
  rename_v("Q12", "qualtrics_qsort") %>%
  rename_v("Q14", "qualtrics_suvery_info") %>%
  rename_v("CharID", "qualtrics_qsort_id") %>%
  rename_v("Q15_", "SS_") %>%
  rename_v("Q16_", "SS_") %>%
  rename(PA = Q17) %>%
  rename_v("Q18_", "PA_") %>%
  rename_v("Q19_", "NCS_6_") %>%
  rename_v("Q20_", "NCS_6_") %>%
  rename(MMS_human = Q21) %>%
  rename_v("Q22_", "MMS_human_") %>%
  rename_v("Q23_", "MMS_cause_") %>%
  rename_v("Q24_", "MMS_cause_") %>%
  rename_v("Q25_", "MMS_consequence_") %>%
  rename_v("Q26_", "MMS_consequence_") %>%
  rename_v("Q27_", "MMS_mitigation_") %>%
  rename_v("Q28_", "MMS_mitigation_") %>%
  rename_v("Q29_", "SVSS_") %>%
  rename_v("Q30_", "SVSS_") %>%
  rename(KV = Q31) %>%
  rename_v("Q32_", "KV_") %>%
  rename_v("Q33_", "SJ_") %>%
  rename_v("Q34_", "SJ_") %>%
  rename_v("Q35_", "CI_") %>%
  rename_v("Q36_", "CI_") %>%
  rename(W = Q37) %>%
  rename_v("Q38_", "W_") %>%
  rename_v("Q39_", "CFC_S_") %>%
  rename_v("Q40_", "CFC_S_") %>%
  rename_v("Q41_", "BFI_10_") %>%
  rename_v("Q42_", "BFI_10_") %>%
  rename_v("Q43_", "EWS_") %>%
  rename_v("Q44_", "EWS_") %>%
  rename_v("Q999_", "specs_") %>%
  rename_v("Q67_", "FPS_") %>%
  rename(FP_mitigation = Q69) %>%
  rename(FP_mitigation_text = Q71)
#4. Import Q sort data
dat.m_qsort <- read_csv("data/study-3-qsort.csv", col_names = F)
#Assign data column names, based on schema used to create MySQL db, see ../study2/data/schema.txt
colnames(dat.m_qsort) <- c(
  paste0("pref_sta_", 1:30),
  paste0("reason_sta_", 1:30),
  paste0("sort_sta_", 1:30),
  paste0("time_", c("instruct", "pilot", "pref", "reason", "sort")),
  paste0("count_", c("noroom", "incon")),
  paste0("supress_", c("noroom", "incon")),
  paste0("pilot_response", 1:2),
  paste0("place_", c("grid", "avail", "spare", "spare_max")),
  "qs",
  "pass",
  "time_start_qsort"
)
dat.m_qsort <- dat.m_qsort %>% 
  as.tibble()
#Convert passcodes and query strings into CHARACTER
passcodes_to_character <- function(c){
  #Turns code (c, integer) into a 7-digit string
  c <- as.character(c)
  if(is.na(c)){return(NA)}
  short_by <- 7-nchar(c)
  if(short_by > 0){
    rep(0, short_by) %>%
      paste(collapse = "") %>%
      paste0(c) %>%
      return()
  } else{
    return(c)
  }
}
dat.m_qsort <- dat.m_qsort %>%
  rowwise %>%
  mutate_at(vars(qs, pass), passcodes_to_character)
dat.clean <- dat.clean %>%
  rowwise() %>%
  mutate_at(vars(qualtrics_qsort_id, code_entered), passcodes_to_character)

#5. Match Qualtrics issued IDs to Q sort information
match_qsort_ids <- function(q, s, t1, t2, t3, t4, t5){
  #Function to match Q sort data to a unique participant ID (id var in dat.clean)
  #Uses timing information (start time (s) and phase times (t1:t5)) and issued query string (q)
  #Returns a participant ID
  #1. Identify matching query strings
  match <- dat.clean %>%
    filter(qualtrics_qsort_id == q)
  if(nrow(match) != 1){
    #Duplicates found
    #Refine further using timing data
    #Participant must have started survey before starting Q sort
    match <- match %>%
      filter(as.POSIXct(time_start, format = '%Y-%m-%d %H:%M:%S') <= as.POSIXct(s, format = "%Y-%m-%d %H:%M:%S")) #[DFP] line was changed due to formating error
    #Participant must have completed Q sort before completing survey
    match <- match %>%
      filter(as.POSIXct(time_end, format = '%Y-%m-%d %H:%M:%S') >= (as.POSIXct(s, format = "%Y-%m-%d %H:%M:%S")+t1+t2+t3+t4+t5)) #[DFP] line was changed due to formating error
  }
  if(nrow(match) != 1){
    #Then data cannot be successfully matched
    #Assign an ID of -1
    return(-1)
  }
  else{
    #Data has been successfully matched
    #Return matched id
    match %>%
      pull(id) %>%
      return
  }
}
dat.m_qsort <- dat.m_qsort %>%
  rowwise %>%
  mutate(id = match_qsort_ids(qs, time_start_qsort, time_instruct, time_pilot, time_pref, time_reason, time_sort)) %>%
  group_by(id) %>%
  mutate(id_is_diagnostic = !(n() > 1)) %>% 
  rowwise %>% 
  mutate(id = ifelse(id_is_diagnostic, id, -1))  %>% 
  select(-id_is_diagnostic)
#Check for any duplicate IDs
#If any exist, process has not been diagnostic and participants should be removed/replaced

#6. Join Q sort data with Qualtrics data
dat.clean <- dat.clean %>%
  full_join(dat.m_qsort, by = "id")
#7. Remove any excess Q sort data
dat.clean <- dat.clean %>%
  mutate(pcf = as.integer(pcf)) %>%
  filter(pcf == 1)
#8. Export a clean data file
#Export to /out
cleanDir <- "data/study-3-clean.csv"
dat.clean %>%
  write_csv(cleanDir)