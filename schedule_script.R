############################
# Schedule Script Using Windows TaskscheduleR
# by Mari Roberts
# 9/24/2020
############################

# set working directory
setwd("/Users/mari/CANY/doccs_downloads")

# install packages
library(githubinstall)
install_github("bnosac/taskscheduleR")
library(taskscheduleR)

taskscheduler_create(
  taskname = "r_web_scraping_doccs",
  rscript = "C:\Users\mari\CANY\doccs_downloads\webscraping.R", # use double \\ in windows
  schedule = "DAILY",
  starttime = format(Sys.time() + 62, "%H:%M"),
  startdate = format(Sys.Date(), "%m/%d/%Y"),
  Rexe = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe")
)

# # stop script
# taskscheduler_stop("r_web_scraping_doccs")
# # delete script
# taskscheduler_delete("r_web_scraping_doccs")