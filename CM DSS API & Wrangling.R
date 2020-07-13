library(qualtRics)
qualtrics_api_credentials(api_key = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                          # api key deleted to protect student info
                          base_url = "https://az1.qualtrics.com",
                          install = TRUE,
                          overwrite = TRUE)
readRenviron("~/.Renviron")
surveys <- all_surveys()
mysurvey <- fetch_survey(surveyID = surveys$id[1]) 
View(mysurvey)
mysurvey1 <- mysurvey  %>% 
  select(-c(4,6:8,12,17:44,49:50,58, 60:66, 71:130, 135, 136, 144, 146:208))
jakerice <- mysurvey1 %>%
  unite("workingState", colnames(mysurvey1[c(45:95)])) %>% 
  mutate(workingState = str_remove_all(str_remove_all(str_remove_all(workingState, "NA_"),"NA"),"_")) %>%
  unite("StateIntern", colnames(mysurvey1[c(118:168)])) %>%
  mutate(StateIntern = str_remove_all(str_remove_all(str_remove_all(StateIntern, "NA_"),"NA"),"_"))