library(formr)
library(tidyverse)
formr_connect(email = 'juergen.schneider@uni-tuebingen.de', 
              rstudioapi::askForPassword("DemJuergenseinSuperpasswort") 
              ) # find password in video https://www.youtube.com/watch?v=dQw4w9WgXcQ


# get surveys
osc_raw <- formr_results(survey_name = 'osc') #%>%
  # filter(!is.na(session) & !is.na(ended) & !str_detect(session, "XXX"))

