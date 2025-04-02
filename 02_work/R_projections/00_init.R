############################################################################## #
# Filename
#    00_init.R
#
# Description
#   Initialization file
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      2025-02-25
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #

##############################################################################~#
# Working directories ##########################################################
wd_proj      <- get("project_directory", "RPROJ") # project folder
wd_data      <- file.path(wd_proj, "01_data")
wd_data_orig <- file.path(wd_data, "01_original")
wd_data_work <- file.path(wd_data, "02_work")
wd_work      <- file.path(wd_proj, "02_work")
wd_rproj     <- get("wd_rproj", "RPROJ") # folder of the r-project
wd_res       <- file.path(wd_proj, "03_results", "01_output")
wd_res_tmp   <- file.path(wd_res, "tmp")

# load functions
source(file.path(wd_rproj, "00_functions.R"), encoding = "UTF-8")

##############################################################################~#
# Needed Packages ##############################################################
if (!require("tidyverse")) {
  install.packages("tidyverse")
}
if (!require("rmarkdown")) {
  install.packages("rmarkdown")
}
if (!require("praise")) {
  install.packages("praise")
}
if (!require("data.tree")) {
  install.packages("data.tree")
}
if (!require("rprojroot")) {
  install.packages("rprojroot")
}
if (!require("aTSA")) {
  install.packages("aTSA")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
if (!require("purrr")) {
  install.packages("purrr")
}
if (!require("xtable")) {
  install.packages("xtable")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
if (!require("zoo")) {
  install.packages("zoo")
}
##############################################################################~#
# Initial checks ###############################################################
message("Your project is located at: ", wd_proj)
message("Your code is located at:    ", wd_rproj)

# check for correct R-version
checkRVersion()

# check working directories
wds <- getWDs()
checkWDs(wds = wds, create_missing_wds = FALSE)

##############################################################################~#
# praise #######################################################################

message("\n",
        praise::praise(template = "Do not forget: You are ${adjective}!\n"))
