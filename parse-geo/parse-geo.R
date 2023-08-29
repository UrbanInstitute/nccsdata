#' Script to return Block or Tract IDs from Census tables that match
#' user inputs
#' 
#' 
#' Load packages
library("data.table")
library("dplyr")
library("purrr")
library("usdata")

#' Preprocessing
#' Load in Data as Data.Table
block_dt <- fread("block_crosswalk.csv")
tract_dt <- fread("tract_crosswalk.csv")
#' Rename states in tract
tract_dt <- tract_dt %>% 
  mutate(geo.state = usdata::state2abbr(state_name))

#' Function to filter block or tract
parse_dt <- function(dat, ...){
  args <- enquos(...)
  ex_args <- unname(
    purrr::imap(
      args,
      function(expr, name) quo(!!sym(name)==!!expr)
    )
  )
  dat %>% 
    filter(!!! ex_args) %>% 
    select("block_geoid")
}


#' Parse-Geo function
#' Takes as input a series of geographic arguments and returns a list
#' of Tract or Block IDs
parse_geo <- function(geo.level, ...){
  
  # Extract arguments
  args <- enquos(...)
  ex_args <- unname(
    purrr::imap(
      args,
      function(expr, name) quo(!!sym(name)==!!expr)
    )
  )
  
  # Evaluate arguments
  
  ifelse(
    geo.level == "TRACT",
    tract_dt %>% 
      filter(!!! ex_args) %>% 
      select("tract"),
    ifelse(
      geo.level == "BLOCK",
      block_dt %>% 
        filter(!!! ex_args) %>% 
        select("block_geoid"),
      stop("Invalid geo.level, select either 'BLOCK' or 'TRACT'")
    )
  )
}



block_dt %>% 
  filter(county_geoid == c("1001", "1002")) %>% 
  select("block_geoid")
