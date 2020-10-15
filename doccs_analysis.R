#######################################
# DOCCS Covid Reports
# Creates df of DOCCS Reports 
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

# list all pdfs in directory
setwd("/Users/mari/doccs_downloads/rawFiles")
temp <- list.files(pattern = "*.pdf", full.names = TRUE)

# create an empty data frame for the data
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

# fix variable types
outputs.df$recovered <- as.numeric(outputs.df$recovered)
outputs.df$deceased <- as.numeric(outputs.df$deceased)
outputs.df$positive_total <- as.numeric(outputs.df$positive_total)
outputs.df$pending_test <- as.numeric(outputs.df$pending_test)
outputs.df$negative_test <- as.numeric(outputs.df$recovered)

################
# top 5s
################

# finds most recent date report
max_date <- max(outputs.df$report_date, na.rm = TRUE)
# finds first date report
min_date <- min(outputs.df$report_date, na.rm = TRUE)

# subsets data to most recent report
df_most_recent <- outputs.df %>% filter(report_date == max_date)

# top 5 places for positive cases
top5_positives <- df_most_recent %>%
  arrange(-positive_total) %>% # sort
  slice(1:5) # take top 5 row per subgroup
top5_positives_names <- top5_positives$facility

# top 5 places for pending tests
top5_pending_tests <- df_most_recent %>%
  arrange(-pending_test) %>% # sort
  slice(1:5) # take top 5 row per subgroup
top5_pending_tests_names <- top5_pending_tests$facility

# top 5 places for deceased
top5_deaths <- df_most_recent %>%
  arrange(-deceased) %>% # sort
  slice(1:5) # take top 5 row per subgroup
top5_deaths_names <- top5_deaths$facility

################
# increases
################

#####
# positive cases
#####

# uses minimum report date and maximum report date

# earliest report information
df_min <- outputs.df %>% filter(report_date == min_date) %>% select(facility, pos_min = positive_total)
# most recent report information
df_max <- df_most_recent %>% select(pos_max = positive_total)
# rbind with df_most_recent
df_pct <- cbind(df_min, df_max)
# find increase in number of pending tests since September 28, 2020
df_pct <- df_pct %>% mutate(pct_change = (pos_max-pos_min)/pos_min*100)

# negative or positive change
df_pct <- df_pct %>%
  mutate(change_positive = pct_change > 0)

# negative or positive change
df_pct <- df_pct %>%
  mutate(change_positive = pct_change > 0)

# graph for percent changes in positive cases by facility
ggplot(data = df_pct,
       aes(x = reorder(facility, pct_change), y = pct_change,
           fill = change_positive))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x = "Facility", y = "Positive Cases Per Facility %",
       title = "Percentage Change in Positive Cases Per Facility",
       subtitles = "DOCCS Reports")+
  theme_minimal()+
  guides(fill = FALSE)

################
# increases for current month
################

# organize date, max - grab latest, look max of what remains , look at 29 as calc increases last day of the last month; working directory 

# most recent month report information
current_month <- format(as.Date(max_date), "%m")

# subset data to most recent month i.e. month of October
df_current_month <- outputs.df %>% filter(month == current_month)

# earliest report information for the month
# finds first date report
min_date <- min(df_current_month$report_date, na.rm = TRUE)
df_min <- df_current_month %>% filter(report_date == min_date) %>% select(facility, 
                                                                          recovered_min = recovered,
                                                                          deceased_min = deceased,
                                                                          positive_total_min = positive_total,
                                                                          pending_test_min = pending_test,
                                                                          negative_test_min = negative_test)

# most recent report information for the month
df_max <- df_current_month %>% filter(report_date == max_date) %>% select(recovered_max = recovered,
                                                                          deceased_max = deceased,
                                                                          positive_total_max = positive_total,
                                                                          pending_test_max = pending_test,
                                                                          negative_test_max = negative_test)
# cbind with df_max
df_pct <- cbind(df_min, df_max)

# find increase in number of pending tests since beginning of the month
df_pct <- df_pct %>% mutate(recovered_increase = recovered_max-recovered_min,
                            deceased_increase = deceased_max-deceased_min,
                            positive_total_increase = positive_total_max-positive_total_min,
                            pending_test_increase = pending_test_max-pending_test_min,
                            negative_test_increase = negative_test_max-negative_test_min)

# find % increase in number of pending tests since beginning of the month
df_pct <- df_pct %>% mutate(recovered_pct_change = (recovered_max-recovered_min)/recovered_min*100,
                            deceased_pct_change = (deceased_max-deceased_min)/deceased_min*100,
                            positive_total_pct_change = (positive_total_max-positive_total_min)/positive_total_min*100,
                            pending_test_pct_change = (pending_test_max-pending_test_min)/pending_test_min*100,
                            negative_test_pct_change = (negative_test_max-negative_test_min)/negative_test_min*100) 

# start writing to an output file
sink('analysis-output.txt')

# get top 5 facilities
cat('The 5 facilities with the most positive cases to date are', top5_positives_names, ".\n", 
    'The 5 facilities with the most deaths  to date are', top5_deaths_names, ".\n",
    'The 5 facilities with the most pending tests  to date are', top5_pending_tests_names, ".\n")

# other stats

             
# stop writing to the file
sink()          