#######################################
# DOCCS Covid Reports
# Creates tables from DOCCS COVID Reports 
# by Mari Roberts
# 10/14/2020
#######################################

# load necessary packages
requiredPackages = c('dplyr',
                     'ggplot2',
                     'pdftools',
                     'tidyverse',
                     'readr')
# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

getwd <- function(){
  thisLogin <- Sys.info()['login']
  if(thisLogin=="evan") {
    base <- '/home'
    canyF <- 'Documents/cany/corona/doccs_downloads/rawFiles'
  }
  if(thisLogin=="mr4909"){
    base <- '/Users'
    canyF <- 'doccs_downloads/rawFiles'
  }
  if(thisLogin=="capp") {
    base <- '/home'
    canyF <- 'doccs_downloads/rawFiles'
  }
  wd <- paste(base,thisLogin,canyF,sep="/")
  return(wd)
}

# set working directory
wd <- getwd()
setwd(wd)
# setwd("/Users/mari/doccs_downloads/rawFiles")

# list all pdfs in directory
temp <- list.files(pattern = "*.pdf", full.names = TRUE)

# create an empty data frame for the pdf data
outputs.df <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("facility", 
                                                                 "recovered",
                                                                 "deceased",                               
                                                                 "positive", 
                                                                 "pending", 
                                                                 "negative",
                                                                 "report_date"))

# read all pdfs in directory and create one dataframe
# for loop cycles through each pdf and rbinds to outputs.df
for(i in 1:length(temp)){
  
  # read in each pdf file
  pdf_name <- pdf_text(temp[i]) %>% read_lines() 
  # pdf_name <- pdf_text("/Users/mr4909/doccs_downloads/rawFiles/doccs-covid-19-confirmed-facility-9292020.pdf") %>% read_lines() 
  
  # extract date string
  report_date <- pdf_name[c(2)]
  #report_date <- gsub(',','',report_date)
  
  # select lines with relevant information
  pdf_name <-pdf_name[c(7:58)] 
  
  # clean lines
  pdf_name <- pdf_name[1:52] %>%
    str_squish() %>%
    str_replace_all("%","") %>% # remove % 
    str_replace_all(",","") %>% # remove punctuation
    str_replace_all("  "," ")   # remove extra white space
  
  # remove whitespace in facility names
  pdf_name <- mgsub::mgsub(pdf_name, c("BARE HILL", 
                                       "BEDFORD HILLS",
                                       "CAPE VINCENT",
                                       "FIVE POINTS",
                                       "GREAT MEADOW ",
                                       "GREEN HAVEN",
                                       "HALE CREEK",
                                       "MOHAWK/WALSH RMU",
                                       "SING SING"), 
                           c("BARE.HILL", 
                             "BEDFORD.HILLS",
                             "CAPE.VINCENT",                               
                             "FIVE.POINTS", 
                             "GREAT.MEADOW ", 
                             "GREEN.HAVEN",
                             "HALE.CREEK",
                             "MOHAWK/WALSH.RMU",
                             "SING.SING"))
  
  # create dataframe
  pdf_temp <- Reduce(rbind, strsplit(trimws(pdf_name),"\\s{2,}"))
  rownames(pdf_temp) <- 1:dim(pdf_temp)[1] # give numbers as rownames
  pdf_name <- data.frame(pdf_temp, stringsAsFactors=FALSE)
  
  # separate string by white space
  pdf_name <- data.frame(do.call('rbind', strsplit(as.character(pdf_name$pdf_temp),' ',fixed=TRUE)),stringsAsFactors=FALSE)
  
  # fix date format
  months.regex <- paste(month.name, collapse='|')
  d <- gsub(paste0(".*(", months.regex, ")"), "\\1", 
            report_date[grep(months.regex, report_date, TRUE)], TRUE)
  report_date <- as.Date(d, format = '%B %d, %Y AT %I:%M %p')
  
  # add date column
  pdf_name$report_date <- report_date
  
  # add month and year column
  pdf_name$year = as.numeric(format(pdf_name$report_date, "%Y"))
  pdf_name$month = as.numeric(format(pdf_name$report_date, "%m"))

  # create variable names
  var_lines <- c("facility", 
                 "recovered",
                 "deceased",                               
                 "positive_total", 
                 "pending_test", 
                 "negative_test",
                 "report_date",
                 "year",
                 "month")
  
  # assign variable names
  colnames(pdf_name) <- var_lines
  
  # bind the rows
  outputs.df <- rbind(outputs.df, pdf_name)

}

# fix variable types - change from chr to num
outputs.df$recovered <- as.numeric(outputs.df$recovered)
outputs.df$deceased <- as.numeric(outputs.df$deceased)
outputs.df$positive_total <- as.numeric(outputs.df$positive_total)
outputs.df$pending_test <- as.numeric(outputs.df$pending_test)
outputs.df$negative_test <- as.numeric(outputs.df$negative_test)

##################
# create variables
##################

# active positive cases (not recovered or deceased)
# total tests given (positive + negative)
outputs.df <- outputs.df %>% mutate(positive_active = positive_total-deceased-recovered,
                                    tests_given = positive_total + negative_test + pending_test)

# find positivity rate
outputs.df <- outputs.df %>% mutate(positivity_rate = positive_total/(tests_given-pending_test))

# finds most recent date report
lastReport <- outputs.df[outputs.df$report_date== max(outputs.df$report_date),]
notThisMonthMask <- outputs.df$month!=lastReport$month[1] | outputs.df$year!=lastReport$year[1]
notThisMonth <- outputs.df[notThisMonthMask,]
lastNotThisMonth <- notThisMonth[notThisMonth$report_date==max(notThisMonth$report_date),]

##########
# top 5s
##########

# # top 5 places for positive cases
# top5_positives <- lastReport %>%
#   arrange(-positive_total) %>% # sort
#   slice(1:5) # take top 5 row per subgroup
# top5_positives_names <- top5_positives$facility
# 
# # top 5 places for pending tests
# top5_pending_tests <- lastReport %>%
#   arrange(-pending_test) %>% # sort
#   slice(1:5) # take top 5 row per subgroup
# top5_pending_tests_names <- top5_pending_tests$facility
# 
# # top 5 places for deceased
# top5_deaths <- lastReport %>%
#   arrange(-deceased) %>% # sort
#   slice(1:5) # take top 5 row per subgroup
# top5_deaths_names <- top5_deaths$facility

################################################################################################
# INCREASES FOR CURRENT MONTH
################################################################################################

# rename variables for last day of previous month
df_min <- lastNotThisMonth %>% select(facility, 
                                      recovered_min = recovered,
                                      deceased_min = deceased,
                                      positive_total_min = positive_total,
                                      pending_test_min = pending_test,
                                      negative_test_min = negative_test,
                                      positive_active_min = positive_active,
                                      tests_given_min = tests_given,
                                      positivity_rate_min = positivity_rate)

# rename variables for current report
df_max <- lastReport %>% select(recovered_max = recovered,
                                deceased_max = deceased,
                                positive_total_max = positive_total,
                                pending_test_max = pending_test,
                                negative_test_max = negative_test,
                                positive_active_max = positive_active,
                                tests_given_max = tests_given,
                                positivity_rate_max = positivity_rate)

# cbind df_min with df_max
df_final <- cbind(df_min, df_max)

# find increases
df_final <- df_final %>% mutate(recovered_increase = recovered_max-recovered_min,
                                deceased_increase = deceased_max-deceased_min,
                                positive_total_increase = positive_total_max-positive_total_min,
                                pending_test_increase = pending_test_max-pending_test_min,
                                negative_test_increase = negative_test_max-negative_test_min,
                                positive_active_increase = positive_active_max-positive_active_min,
                                tests_given_increase = tests_given_max-tests_given_min,
                                positivity_rate_increase = positivity_rate_max-positivity_rate_min)

df_final <- df_final %>% select(facility,
                                recovered_increase,
                                deceased_increase,
                                positive_active_increase,
                                positive_total_increase,
                                pending_test_increase,
                                negative_test_increase,
                                tests_given_increase,
                                positivity_rate_increase)

##########
# top 5s increases
##########

# issue with top 5
# doesn't account for zeros
# doesn't account for duplicate values

# create an empty data frame for the pdf data
# top5.df <- data.frame(matrix(ncol = 0, nrow = 0))
# 
# for(i in df_final){
# 
#   df <- c(1)
# 
#   # bind the rows
#   top5.df <- cbind(top5.df, df)
# }

# top 5 places for recovered_increase
top5_recovered_increase <- df_final %>%
  arrange(-recovered_increase) %>% # sort
  slice(1:5) %>% # take top 5 row per subgroup
  select(facility, recovered_increase)

# top 5 places for deceased_increase
top5_deceased_increase <- df_final %>%
  arrange(-deceased_increase) %>% # sort
  slice(1:5) %>% # take top 5 row per subgroup
  select(facility, deceased_increase)

# top 5 places for positive_active_increase
top5_positive_active_increase <- df_final %>%
  arrange(-positive_active_increase) %>% # sort
  slice(1:5) %>% # take top 5 row per subgroup
  select(facility, positive_active_increase)

# top 5 places for positive_total_increase
top5_positive_total_increase <- df_final %>%
  arrange(-positive_total_increase) %>% # sort
  slice(1:5) %>% # take top 5 row per subgroup
  select(facility, positive_total_increase)

# top 5 places for pending_test_increase
top5_pending_test_increase <- df_final %>%
  arrange(-pending_test_increase) %>% # sort
  slice(1:5) %>% # take top 5 row per subgroup
  select(facility, pending_test_increase)

# top 5 places for negative_test_increase
top5_negative_test_increase <- df_final %>%
  arrange(-negative_test_increase) %>% # sort
  slice(1:5) %>% # take top 5 row per subgroup
  select(facility, negative_test_increase)

# top 5 places for tests_given_increase
top5_tests_given_increase <- df_final %>%
  arrange(-tests_given_increase) %>% # sort
  slice(1:5) %>% # take top 5 row per subgroup
  select(facility, tests_given_increase)

# top 5 places for positivity_rate_increase
top5_positivity_rate_increase <- df_final %>%
  arrange(-positivity_rate_increase) %>% # sort
  slice(1:5) %>% # take top 5 row per subgroup
  select(facility, positivity_rate_increase)

# combine everything into one dataframe
top5_all <- data.frame(top5_recovered_increase$facility,
                       top5_deceased_increase$facility,
                       top5_positive_active_increase$facility,
                       top5_positive_total_increase$facility,
                       top5_pending_test_increase$facility,
                       top5_negative_test_increase$facility,
                       top5_tests_given_increase$facility,
                       top5_positivity_rate_increase$facility)