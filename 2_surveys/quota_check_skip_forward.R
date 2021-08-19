################################################################# #
## OLD SCRIPT FOR QUOTA           #################################
################################################################# #

library(formr)
library(dplyr)
library(stringr)

formr_connect(email = keyring::key_list(service = 'formr_juergen')[, "username"], 
              password = keyring::key_get("formr_juergen", 
                                          keyring::key_list(service = 'formr_juergen')[, "username"]))

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
  client_id = "###",  # get from formr.org
  client_secret = "###", # get from formr.org
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
colnames(osc_res) <- unique(unlist(sapply(results$osc, names)))

# survey osc_0
osc_0_res <- as.data.frame(t(stri_list2matrix(results$osc_0)))
colnames(osc_0_res) <- unique(unlist(sapply(results$osc_0, names)))

# extract the session names of peepz that successfully ended the survey
success <- osc_res %>%
  dplyr::filter(finished == "finished") #dplyr::filter(!str_detect(session, 'XXX') & !is.na(session) & finished == "finished")

# filter first survey based on "successful" session IDs
sample <- osc_0_res %>%
  dplyr::filter(session %in% success$session) %>% # which session IDs from osc_0 are the ones finishing the survey?
  mutate(disc = str_sub(disc, 1, 3)) %>% # make clusters of disciplines
  dplyr::filter(disc == str_sub(osc_0$disc, 1, 3)) %>% # filter only rows with the same disc as participant
  dplyr::summarize(subsample = nrow(.))  # count the number of rows

sample$subsample >= 50