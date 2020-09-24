############################
# Schedule Script Using Addin
# by Mari Roberts
# 9/24/2020
############################

# load necessary packages
library(pacman)
p_load_gh("bnosac/cronR")
p_load('miniUI')
p_load('shiny')
p_load('shinyFiles')

# current_time <- Sys.time()
# print(current_time)
# msg <- glue::glue("This is a test I'm running at {current_time}.")
# cat(msg, file = "test.text")
# msg

# "Schedule scripts through Linux/Unix" through Addin Drop Down in R Studio
# Select script, time to start, and frequency
