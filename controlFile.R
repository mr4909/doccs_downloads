## Not run:
library(cronR)
if(Sys.info()[["login"]]=="evan"){
    cmd <- cron_rscript("/home/evan/Documents/cany/corona/doccs_downloads/webscraping.R")
}
if(Sys.info()[["login"]]=="capp"){
    cmd <- cron_rscript("/home/capp/doccs_downloads/webscraping.R")
}
cmd

cron_add(command = cmd, frequency ='minutely', id ='test1')
cron_njobs()
cron_ls()
cron_clear(ask=FALSE)
cron_ls()
