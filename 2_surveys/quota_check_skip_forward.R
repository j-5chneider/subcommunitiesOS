################################################################# #
## OLD SCRIPT FOR QUOTA           #################################
################################################################# #

library(formr)
library(dplyr)
library(stringr)

formr_connect(keyring = "juergen")

# extract the session names of peepz that successfully ended the survey
success <- formr_raw_results(survey_name = 'osc') %>%
  dplyr::filter(!is.na(session) & !is.na(ended))

# filter first survey based on "successful" session IDs
sample <- formr_raw_results(survey_name = 'osc_0') %>%
  dplyr::filter(session %in% success$session) %>%
  mutate(disc = str_sub(disc, 1, 3)) %>% # make clusters of disciplines
  dplyr::filter(disc == str_sub(osc_0$disc, 1, 3)) %>% # filter only rows with the same disc
  dplyr::summarize(subsample = nrow(.))
         
sample$subsample >= 50


################################################################# #
## NEW SCRIPT FOR QUOTA           #################################
################################################################# #

library(httr)
library(stringi)
library(dplyr)
library(stringr)

# Login using your client ID and client Secret to get an access token
login <- list( # define login credentials
  client_id = "###",
  client_secret = "###",
  grant_type = "client_credentials"
)

request <- POST( # send POST request
  "https://api.formr.org/oauth/access_token",
  body = login, 
  encode = "form"
)
# parse response to get access token
# If there an error then the response object would contain the details of the error in response$error
response <- content(request)
access_token <- response$access_token

#With a valid access token, call API to get the results of a particular study (run)
query <- list(
  access_token = access_token,
  "run[name]" = "osc",
  "run[session]" = ""
)
request <- GET("https://api.formr.org/get/results", query=query)
results <- content(request) # download results

## transform list to data.frame
# survey osc
osc_res <- as.data.frame(t(stri_list2matrix(results$osc)))
colnames(osc_res) <- unique(unlist(sapply(results$osc, names)))[,1]

# survey osc_0
osc_0_res <- as.data.frame(t(stri_list2matrix(results$osc_0)))
colnames(osc_0_res) <- unique(unlist(sapply(results$osc_0, names)))[,1]

# extract the session names of peepz that successfully ended the survey
success <- osc_res %>%
  dplyr::filter(!str_detect(session, 'XXX') & !is.na(session) & finished == "finished")

# filter first survey based on "successful" session IDs
sample <- osc_0_res %>%
  dplyr::filter(session %in% success$session) %>% # which session IDs from osc_0 are the ones finishing the survey?
  mutate(disc = str_sub(disc, 1, 3)) %>% # make clusters of disciplines
  dplyr::filter(disc == str_sub(osc_0$disc, 1, 3)) %>% # filter only rows with the same disc as participant
  dplyr::summarize(subsample = nrow(.))  # count the number of rows

sample$subsample >= 53 # we had 3 test runs


################################################################# #
## CHECK PARTICIPANT STATUS       #################################
################################################################# #

library(formr)
library(dplyr)
library(stringr)

formr_connect(keyring = "juergen")

# extract the session names of peepz that successfully ended the survey
osc_0 <- formr_raw_results(survey_name = 'osc_0') %>%
  dplyr::filter(!is.na(session) & !is.na(ended)) %>%
  dplyr::select(session, prolific_pid, quality_check)

osc <- formr_raw_results(survey_name = 'osc') %>%
  dplyr::filter(!is.na(session) & !is.na(ended)) %>%
  dplyr::select(session, finished)

quota_full <- formr_raw_results(survey_name = 'quota_full') %>%
  dplyr::filter(!is.na(session) & !is.na(ended)) %>%
  dplyr::select(session, full)

tmp <- left_join(osc_0, osc, by="session")
part_stat <- left_join(tmp, quota_full, by="session") %>%
  dplyr::filter(!str_detect(session, 'XXX') & !is.na(session)) %>%
  dplyr::select(-session)

options(max.print=1000000)
part_stat[19:nrow(part_stat),] # filter away the first 18 entries (test runs). Published: 20 Aug 2021, 12:04



################################################################# #
## CHECK CELLS                    #################################
################################################################# #




library(httr)
library(stringi)
library(dplyr)
library(stringr)

# Login using your client ID and client Secret to get an access token
login <- list( # define login credentials
  client_id = "",
  client_secret = "",
  grant_type = "client_credentials"
)

request <- POST( # send POST request
  "https://api.formr.org/oauth/access_token",
  body = login, 
  encode = "form"
)
# parse response to get access token
# If there an error then the response object would contain the details of the error in response$error
response <- content(request)
access_token <- response$access_token

#With a valid access token, call API to get the results of a particular study (run)
query <- list(
  access_token = access_token,
  "run[name]" = "osc",
  "run[session]" = ""
)
request <- GET("https://api.formr.org/get/results", query=query)
results <- content(request) # download results

## transform list to data.frame
# survey osc
osc_res <- as.data.frame(t(stri_list2matrix(results$osc)))
colnames(osc_res) <- unique(unlist(sapply(results$osc, names)))[,1]

# survey osc_0
osc_0_res <- as.data.frame(t(stri_list2matrix(results$osc_0)))
colnames(osc_0_res) <- unique(unlist(sapply(results$osc_0, names)))[,1]

# extract the session names of peepz that successfully ended the survey
success <- osc_res %>%
  dplyr::filter(!str_detect(session, 'XXX') & !is.na(session) & finished == "finished")

# filter first survey based on "successful" session IDs
sample <- osc_0_res %>%
  dplyr::filter(session %in% success$session) %>% # which session IDs from osc_0 are the ones finishing the survey?
  mutate(disc = str_sub(disc, 1, 3))

sample <- sample[13:nrow(sample),] # filter away first 12 entires (=test runs)

table(sample$disc)




################################################################# #
## EXPORT DATA                    #################################
################################################################# #

library(formr)
library(dplyr)
library(stringr)

# formr_connect(keyring = "formr_juergen")
# 
# survey_osc_0 <- formr_raw_results(survey_name = 'osc_0') %>%  # need to use RAW results because it leaves the data in order
#   dplyr::filter(!is.na(session) & !is.na(ended) & !str_detect(session, 'XXX')) %>%  # filter out dummy test runs
#   dplyr::select(-ended, -modified, -expired, -prolific_pid)
# 
# testruns <- survey_osc_0[1:18,"session"]  # filter out "live" testruns
# 
# 
# survey_osc <- formr_raw_results(survey_name = 'osc') %>%
#   dplyr::filter(!is.na(session) & !is.na(ended) & !str_detect(session, 'XXX')) %>%  # filter out dummy test runs
#   dplyr::select(-created, -expired)
# 
# data_osc <- left_join(survey_osc, survey_osc_0, by="session") %>%
#   dplyr::filter(!(session %in% testruns)) %>% #filter out sessions that are from testruns
#   dplyr::select(-quality_check, -finished)

formr_connect(keyring = "juergen")

osc_survey_osc_0 <- formr_results(survey_name = 'osc_0')%>%
  dplyr::select(session, disc, prolific_pid, created) %>%
  dplyr::mutate(disc = str_sub(disc, 1, 3))
osc_survey_osc <- formr_results(survey_name = 'osc')%>%
  dplyr::select(-created)



osc_data <- left_join(osc_survey_osc, osc_survey_osc_0, by = "session") %>%
  dplyr::filter(nchar(prolific_pid)>20 & !is.na(finished)) %>%
  dplyr::select(-prolific_pid, -finished)


saveRDS(osc_data, "data/osc_data.RData")
rio::export(osc_data, "data/osc_data.csv")
