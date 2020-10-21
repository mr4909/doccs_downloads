#######################################
# DOCCS Covid Reports
# Creates df of DOCCS Reports 
# by Mari Roberts and Evan Misshula
# 10/14/2020
#######################################

# import and clean data using doccs_clean file
source("doccs_clean.R")

####
# This Month
####

lastReport <- outputs.df[outputs.df$report_date== max(outputs.df$report_date),]
notThisMonthMask <- outputs.df$month!=lastReport$month[1] | outputs.df$year!=lastReport$year[1]
notThisMonth <- outputs.df[notThisMonthMask,]
lastNotThisMonth <- notThisMonth[notThisMonth$report_date==max(notThisMonth$report_date),]

## pending
totalPendingNow <- sum(lastReport$pending_test)
lastNotThisMonthPending <- sum(lastNotThisMonth$pending_test)
monthIncreasePending <- totalPendingNow-lastNotThisMonthPending
## deaths
totalDeathsNow <- sum(lastReport$deceased)
lastNotThisMonthDeaths <- sum(lastNotThisMonth$deceased)
monthIncreaseDeaths <- totalDeathsNow-lastNotThisMonthDeaths
monthIncreaseDeathsFac <- lastReport$deceased-lastNotThisMonth$deceased
## recovered
totalRecoveredNow <- sum(lastReport$recovered)
lastNotThisMonthRecovered <- sum(lastNotThisMonth$recovered)
monthIncreaseRecovered <- totalRecoveredNow-lastNotThisMonthRecovered
monthIncreaseRecoveredFac <- lastReport$recovered-lastNotThisMonth$recovered
## total positive
totalPositiveNow <- sum(lastReport$positive_total)
lastNotThisMonthPositive <- sum(lastNotThisMonth$positive_total)
monthIncreasePositive <- totalPositiveNow-lastNotThisMonthPositive
monthIncreasePositiveFac  <- lastReport$positive-lastNotThisMonth$positive
## current cases
totalSickNow <- totalPositiveNow-totalDeathsNow-totalRecoveredNow
totalSickLastNotThisMonth <- lastNotThisMonthPositive-lastNotThisMonthDeaths-lastNotThisMonthRecovered
changeInSick <- totalSickNow-totalSickLastNotThisMonth
totalSickNowFac <- (lastReport$positive_total-lastReport$recovered-lastReport$deceased)
totalSickLastNotThisMonthFac <- (lastNotThisMonth$positive_total-lastNotThisMonth$recovered-lastNotThisMonth$deceased)

changeInSickFac <- totalSickNowFac -totalSickLastNotThisMonthFac

## negative
totalNegativeNow <- sum(lastReport$negative_test)
lastNotThisMonthNegative <- sum(lastNotThisMonth$negative_test)
monthIncreaseNegative <- totalNegativeNow-lastNotThisMonthNegative
monthIncreaseNegativeFac <- lastReport$negative_test-lastNotThisMonth$negative_test
## positivity
monthResolved <- monthIncreasePositive+monthIncreaseNegative
positivity <- round(100*monthIncreasePositive/monthResolved,1)
monthResolvedFac <- monthIncreasePositiveFac+monthIncreaseNegativeFac
positivityFac <- round(100*monthIncreasePositiveFac/monthResolvedFac,1)

###############
# pending cases - Mari
###############
# totalPendingNow <- sum(lastReport$pending_test)
# lastNotThisMonthPending <- sum(lastNotThisMonth$pending_test)
monthIncreasePending <- totalPendingNow-lastNotThisMonthPending
monthIncreasePendingFac <- lastReport$pending_test-lastNotThisMonth$pending_test

incrementalTests <- c(monthIncreasePositive,monthIncreaseNegative,monthIncreasePending)
incrementalTestsM <- matrix(incrementalTests,byrow=T,ncol=3)
incrementalTestsDF <- as.data.frame(incrementalTestsM)
colnames(incrementalTestsDF) <- c("positive","negative","chgInPending")

posNsickM <- matrix(c(totalSickNow,changeInSick,0.01*positivity),ncol = 3)
posNsickDF <- as.data.frame(posNsickM)
colnames(posNsickDF) <- c("active-cases","chg-in-active","positivity")

casesThisMonth <- data.frame(lastReport$facility,
                             monthIncreasePositiveFac,
                             monthIncreaseNegativeFac,
                             positivityFac,
                             monthIncreasePendingFac, # add month increase in pending tests
                             stringsAsFactors = F)
casesThisMonthO <- casesThisMonth[order(casesThisMonth$monthIncreasePositiveFac,decreasing = T),]

# add column names
colnames(casesThisMonthO) <- c("facility","pos","neg","positivity","pending test")
casesThisMonthOgt0  <- casesThisMonthO[casesThisMonthO$pos>0,]

CapStr <- function(y1) {
    y <- tolower(y1)
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
      sep="", collapse=" ")
}

casesThisMonthOgt0$facility <- unlist(lapply(casesThisMonthOgt0$facility,CapStr))
normPos <- function(x){
    if(is.numeric(x) & !is.infinite(x)){
        p <- 0.01*x
    }else{
        p <- NA
    }
    return(p)
}

casesThisMonthOgt0$positivity <- unlist(lapply(casesThisMonthOgt0$positivity,normPos))
#### 
library(openxlsx)
wb <- openxlsx::createWorkbook()
SHEET_NAME=paste0("all_nys_prisons")
STYLE_NUM_W_COMMA <- openxlsx::createStyle(numFmt = "#,###")
STYLE_PCT <- openxlsx::createStyle(numFmt = "0.0%")
STYLE_NUM_CHG_POS <- openxlsx::createStyle(numFmt = "+#,###")
STYLE_NUM_CHG_NEG <- openxlsx::createStyle(numFmt = "-#,###")
addWorksheet(wb, SHEET_NAME)
mergeCells(wb, SHEET_NAME, cols = 1:8, rows = 1)
openxlsx::writeData(wb, SHEET_NAME, "How many tests were done so far, this month in NY State Prisons?",
                    startCol = 1, startRow = 1)
dateFprmat <- "%A, %B %-d, %Y"
timeSpan <- paste0("From ",format(lastNotThisMonth$report_date[1],dateFprmat)," to ",format(lastReport$report_date[1],dateFprmat),".")
mergeCells(wb, SHEET_NAME, cols = 1:8, rows = 3)
openxlsx::writeData(wb, SHEET_NAME, timeSpan,
                    startCol = 1, startRow = 3)
addStyle(wb, SHEET_NAME, STYLE_NUM_W_COMMA,
             rows = 5:6,cols = 2:5,
             gridExpand = T, stack = T)
openxlsx::writeData(wb, SHEET_NAME, incrementalTestsDF,
                    startCol = 2, startRow = 5)
addStyle(wb, SHEET_NAME, STYLE_PCT,
             rows = 8:9,cols = 4:5,
         gridExpand = T, stack = T)
if(changeInSick>=0){
    addStyle(wb, SHEET_NAME, STYLE_NUM_CHG_POS,
             rows = 8:9,cols = 3:3,
             gridExpand = T, stack = T)
}else{
    addStyle(wb, SHEET_NAME, STYLE_NUM_CHG_NEG,
             rows = 8:9,cols = 3:3,
             gridExpand = T, stack = T)
    }
openxlsx::writeData(wb, SHEET_NAME, posNsickDF,
                    startCol = 2, startRow = 8)
mergeCells(wb, SHEET_NAME, cols = 1:8, rows = 11)
openxlsx::writeData(wb, SHEET_NAME, "What prisons are the cases coming from this month?",
                    startCol = 1, startRow = 11)
addStyle(wb, SHEET_NAME, STYLE_PCT,
             rows = 13:(14+nrow(casesThisMonthOgt0)),cols = 5:5,
         gridExpand = T, stack = T)

openxlsx::writeData(wb, SHEET_NAME, casesThisMonthOgt0,
                    startCol = 2, startRow = 13)




openxlsx::saveWorkbook(
              wb=wb,
              file = paste0("../report/report", format(lastReport$report_date[1],"%Y%m%d"),".xlsx"),
              overwrite = T
          )





incFac


IncPending
IncDeaths


###
This Week
####
