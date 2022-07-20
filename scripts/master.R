#========================================================================
# MASTER SCRIPT
#========================================================================

# Get libraries
library(tidyverse)
library(lfe)
library(miceadds)
library(sandwich)
library(lmtest)
library(haven)
library(stats)
library(margins)
library(stargazer)
library(here)

# Read in data (created in Stata)

benin_data <- read_dta(file.path('data', 'BeninHIV_Analysis.dta'))



