#' Script to return Block or Tract IDs from Census tables that match
#' user inputs
#' 
#' 
#' Load packages
library("data.table")

#' Preprocessing
#' Load in Data as Data.Table
block_dt <- fread("block_crosswalk.csv")

