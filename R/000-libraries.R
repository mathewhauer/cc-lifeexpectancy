######################################################
##
## Inaction on climate change could approach one year of life in some European countries
##       Prepare R Session
##
## Authors: Mathew Hauer, mehauer@fsu.edu
##          
## Last updated: 08/22/2018
##
######################################################


#Clean Up Workspace 
rm(list = ls()) # Remove Previous Workspace
gc(reset = TRUE) # Garbage Collection


# R Workspace Options
options(scipen = 12) # Scientific Notation
options(digits = 6) # Specify Digits
options(java.parameters = "-Xmx1000m") # Increase Java Heap Size


#Functions, Libraries, & Parallel Computing 
## Functions 
# Specify Number of Digits (Forward)
numb_digits_F <- function(x,y){
  numb_digits_F <- do.call("paste0", list(paste0(rep(0, y - nchar(x)), collapse = ""), x))
  numb_digits_F <- ifelse(nchar(x) < y, numb_digits_F, x)
}

# Remove Double Space 
numb_spaces <- function(x) gsub("[[:space:]]{2,}", " ", x)


# Install and load pacman for easier package loading and installation
if (!require("pacman", character.only = TRUE)){
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}


# Libraries
pkgs <- c(
  "tidyverse",   # Tidyverse
  "data.table",  # Data Management/Manipulation
  "openxlsx",    # Microsoft Excel Files
  "HMDHFDplus",  # HMD data
  "tmap",        # Mapping
  "tmapptools",  # Mapping tools
  "RColorBrewer",# Colors
  "kableExtra",  # Table making
  "knitr",       # Knitting
  "xtable",      # Table making
  "getPass"      # To get the password/ids
  
)

# Install missing packages
# Will only run if at least one package is missing

if(!sum(!p_isinstalled(pkgs))==0){
  p_install(
    package = pkgs[!p_isinstalled(pkgs)], 
    character.only = TRUE
  )
}

# load the packages
p_load(pkgs, character.only = TRUE)