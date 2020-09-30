############################
# download DOCCS confirmed 
# by facility each day at 5pm
# by Mari Roberts
# 9/24/2020
############################

# load necessary packages
requiredPackages = c('rvest',
                     'tidyverse',
                     'stringr',
                     'pdftools',
                     'tm') 
# only installs packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

url <- 'https://doccs.ny.gov/doccs-covid-19-report'
links <- url %>% 
  read_html() %>% 
  html_nodes('a') %>% 
  html_attr('href') %>% 
  grep('facility', ., value = T) # subset links that contain facility, getting an error with "pdf"

# base url
base_url <- 'https://doccs.ny.gov'
# pdf link
link1 <- links[3]
# combine the base url with the pdf url
pdf_url <- paste0(base_url, link1,".pdf")
pdf_url

myBase <- "https://doccs.ny.gov/system/files/documents"
myYear <- format(Sys.Date(),format = "%Y")
myMonth <- format(Sys.Date(),format = "%m")
myDoc <- "doccs-covid-19-confirmed-by-facility-"
myDate <- format(Sys.Date(),format = "%-m.%-d.%Y")
myFtype <- ".pdf"
myFullDoc <- paste0(myDoc,myDate,myFtype)

pdf2_url <- paste(myBase,myYear,myMonth,myFullDoc,sep="/")

pdf1_url <- "https://doccs.ny.gov/system/files/documents/2020/09/doccs-covid-19-confirmed-by-facility-9.29.2020.pdf"

download.file(pdf1_url,paste0("./rawFiles/",myFullDoc))
# data from pdf
doccs_report <- pdf_text(pdf_url)
# remove punction e.g. commas in numbers
doccs_report <- removePunctuation(doccs_report)
# head(print(doccs_report)) # view data

# write file to working directory
wd <- getwd() # get the working directory
file_name <- paste0(wd, "/", "doccs_report.txt")
write(doccs_report, file = file_name, sep = "\t")

https://doccs.ny.gov/system/files/documents/2020/09/doccs-covid-19-confirmed-by-facility-9.29.2020.pdf
