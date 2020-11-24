# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "shinydashboard" )
usethis::use_package( "DT" )
usethis::use_package( "plotly" )
usethis::use_package( "data.table" )
usethis::use_package( "glue" )
usethis::use_package( "boot" )
usethis::use_package("dplyr")
usethis::use_package("shinipsum")
usethis::use_package("tidyverse", type = "depends")
usethis::use_package("lubridate")
usethis::use_package("RColorBrewer")
usethis::use_package("sjPlot")
usethis::use_package("sjmisc")
usethis::use_package("sjlabelled")
usethis::use_package("R6")




## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "dashboard" ) # Name of the module
golem::add_module( name = "tabDashboardData" ) # Name of the module
golem::add_module( name = "tabDashboardMain" ) # Name of the module
golem::add_module( name = "tabDashboardCohortAnalysis" ) # Name of the module
golem::add_module( name = "tabDashboardReport" ) # Name of the module
golem::add_module( name = "ReportCard" ) # Name of the module


## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("everestInsights")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

