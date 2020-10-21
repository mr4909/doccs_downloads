#######################################
# DOCCS Covid Reports
# Retrieves Data from DOCCS COVID Reports 
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
  #pdf_name <- pdf_text("/Users/mari/cany/doccs_downloads/rawFiles/doccs-covid-19-confirmed-by-facility-5.15.2020.pdf") %>% read_lines() 
  
  # extract date string
  report_date <- pdf_name[c(2)]
  
  # get line with just RMU
  rmu <- grep("^RMU$", pdf_name)
  rmu <- length(rmu)
  
  # IF RMU is on its own line, execute the following (basically removes the RMU line which is empty)
  if (rmu > 0) {
    
    # remove RMU line that's empty
    pdf_name_1 <- pdf_name[c(grep(pattern = "ADIRONDACK", pdf_name)):(grep(pattern = "MOHAWK/WALSH", pdf_name))]
    pdf_name_2 <- pdf_name[c(grep(pattern = "MORIAH", pdf_name)):(grep(pattern = "WYOMING", pdf_name))]
    pdf_name <- c(pdf_name_1, pdf_name_2)
    
    # select lines from adirondack to wyoming 
    pdf_name <- pdf_name[(grep(pattern = "ADIRONDACK", pdf_name)):(grep(pattern = "WYOMING", pdf_name))] %>%
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
                                         "MOHAWK/WALSH",
                                         "SING SING"), 
                             c("BARE.HILL", 
                               "BEDFORD.HILLS",
                               "CAPE.VINCENT",                               
                               "FIVE.POINTS", 
                               "GREAT.MEADOW ", 
                               "GREEN.HAVEN",
                               "HALE.CREEK",
                               "MOHAWK/WALSH.RMU",
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
    
    # if RMU is not on its own line, execute the following 
  } else {
    
    # select lines from adirondack to wyoming 
    pdf_name <- pdf_name[(grep(pattern = "ADIRONDACK", pdf_name)):(grep(pattern = "WYOMING", pdf_name))] %>%
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
                                         "MOHAWK/WALSH",
                                         "SING SING"), 
                             c("BARE.HILL", 
                               "BEDFORD.HILLS",
                               "CAPE.VINCENT",                               
                               "FIVE.POINTS", 
                               "GREAT.MEADOW ", 
                               "GREEN.HAVEN",
                               "HALE.CREEK",
                               "MOHAWK/WALSH.RMU",
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
    
  }
  
  # bind the rows
  outputs.df <- rbind(outputs.df, pdf_name)
  
}

# fix variable types - change from chr to num
outputs.df$recovered <- as.numeric(outputs.df$recovered)
outputs.df$deceased <- as.numeric(outputs.df$deceased)
outputs.df$positive_total <- as.numeric(outputs.df$positive_total)
outputs.df$pending_test <- as.numeric(outputs.df$pending_test)
outputs.df$negative_test <- as.numeric(outputs.df$negative_test)

# check data 
# outputs.df <- outputs.df %>% distinct()
# temp <- outputs.df %>% 
#   group_by(report_date) %>% 
#   summarise_if(is.numeric, funs(sum))

###############################################################################
# issues with 10-13-2020 and 10-14-2020; DOCCS used date = 10-13-2020 for both
###############################################################################

# fix 10-13-2020 and 10-14-2020 dates
outputs.10.13.10.14 <- outputs.df %>% filter(report_date == "2020-10-13")
outputs.10.13.2020 <- outputs.10.13.10.14 %>% slice(1:52) 
outputs.10.13.2020 <- outputs.10.13.2020 %>% mutate(report_date = as.Date("2020-10-12"))
outputs.10.14.2020 <- outputs.10.13.10.14 %>% slice(53:104) 
outputs.10.13.10.14 <- rbind(outputs.10.13.2020,outputs.10.14.2020)
outputs.df <- outputs.df %>% filter(report_date != "2020-10-13")
outputs.df <- rbind(outputs.10.13.10.14,outputs.df)

# sort by date
outputs.df <- outputs.df[order(outputs.df$report_date),]

# check for duplicates
setdiff(outputs.df, outputs.df1)

outputs.df <- outputs.df %>% distinct()