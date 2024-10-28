library(broom)
library(caTools) 
library(randomForest) 
library(caret)
library(lubridate)
library(raster)
library(sf)
library(purrr)
library(rlang)
library(dplyr)

data.frame.lag.lead = function(dataframe, covariates, lag = FALSE, lead = FALSE,
                               nlags = 0, nleads = 0, vlags = c(), vleads = c(),
                               grouping = NA, skips = 1) {
  # INPUTS
  # dataframe: input data
  # covariates: vector of strings that specify covariates to lag in the dataframe
  # lag: determines if function will lag
  # lead: determines if function will lead 
  # nlags: number of times to lag the covariates in the dataframe
  # nleads: number of times to lead the covariates in the dataframe
  # vlags: vector containing integers for more control of how many lags, overrides nlags & skips
  # vleads: vector containing integers for more control of how many leads, overrides nleads & skips
  # grouping: the column name to group data by to ensure that lagging does not go across groups
  # skips: number of entries to skip across each separate lag
  
  # OUTPUTS
  # dataframe : input data with new lagged/lead columns
  # new.covariates : vector of strings with new column names
  
  new.covariates = vector() # holds new column names
  group.count = 1 # number of groupings in the dataframe, such as separate idcollar groups
  
  if (lag & lead) {
    
    # recursively call function to lag and lead separately
    lagging = data.frame.lag.lead(dataframe, covariates, lag, lead = FALSE, nlags, nleads, vlags = vlags, vleads = c(), grouping, skips)
    leading = data.frame.lag.lead(lagging[[1]], covariates, lag = FALSE, lead, nlags, nleads, vlags = c(), vleads = vleads, grouping, skips)
    
    # return final data frame object and combine the lag & lead new.covariates vectors
    return(list(dataframe = leading[[1]], 
                new.covariates = c(lagging[[2]], leading[[2]])))
    
  } else if (lag) {
    
    if (nlags == 0) return(list(dataframe, c())) # do nothing
    
    # give placeholder variables the appropriate lagging values
    func = dplyr::lag
    prefix = "prev"
    n.level = nlags
    vec = vlags
    
  } else if (lead) {
    
    if (nleads == 0) return(list(dataframe, c())) # do nothing
    
    # give placeholder variables the appropriate leading values
    func = dplyr::lead
    prefix = "next"
    n.level = nleads
    vec = vleads
    
  }
  
  if (!is.na(grouping)) {
    # use group_by so that when lagging & leading, groups do not interact with each other
    # in nonsensical ways, such as the last entry from animal 1 being included as a previous
    # entry for the first entry of animal 2
    dataframe = dataframe %>%
      group_by(!!sym(grouping))
    group.count = length(unique(dataframe[[grouping]]))
  }
  
  # prepare names to be used in mutate & lag/lead functions:
  
  if (length(vec) != 0) { # 
    # vector of vlag/lead vec distances, repeated for each covariates
    lag.lead.distance = rep_len(x = vec, 
                                length.out = length(covariates) * length(vec))
    # vector of specified/input covariates, repeated to be same length as lag.lead.distance
    repeated.covariates = rep(covariates, each=length(vec))    
  } else {
    # vector of distance from current row to the lagged or lead row, repeated for each covariate
    lag.lead.distance = rep_len(x = seq(from=skips, to=skips*n.level, 
                                        by=skips), 
                                length.out = length(covariates)*n.level)
    # vector of specified/input covariates, repeated to be same length as lag.lead.distance
    repeated.covariates = rep(covariates, each=n.level)
  }
  
  # vector of new column names using prev/next classification, distance from current row, & covariate name
  new.covariates = paste0(prefix, lag.lead.distance, repeated.covariates)
  
  # iterate over the new vectors to mutate the appropriate lag/lead data & column names
  for (i in 1:length(new.covariates)) {
    dataframe = dataframe %>%
      mutate(!!new.covariates[[i]] := func(!!sym(repeated.covariates[[i]]), 
                                             n=lag.lead.distance[[i]]))
  }
  
  if (!is.na(grouping)) {
    dataframe = dataframe %>%
      ungroup()
  }
  
  # returns new, mutated dataframe and new column names
  return(list(dataframe = dataframe, 
              new.covariates = new.covariates))
  
}

###############################################
################### TESTING ###################
###############################################

dataframe <- read.csv("/Users/johnpaul/Desktop/Texas A&M University - Statistics/DBS Research/RF/rf_predicted_file.csv")

covariates = list("latitude",
                  "longitude",
                  "TempF",
                  "slope",
                  "ruggedness",
                  "cosHour",
                  "sinHour",
                  "cosDay",
                  "sinDay",
                  "cosMon",
                  "sinMon",
                  "distance",
                  "speed",
                  "cosAngle",
                  "sinAngle",
                  "azimuth",
                  "cosAspect",
                  "sinAspect")

grouping = "idcollar"
nlags = 2
nleads = 3
skips = 2

just.lag = data.frame.lag.lead(dataframe = dataframe,
                               covariates = covariates,
                               lag = TRUE,
                               nlags = nlags,
                               grouping = grouping,
                               skips = skips)

# View(just.lag$dataframe) # views lagged & lead dataframe
# print(just.lag$new.covariates) # outputs new column names

just.lead = data.frame.lag.lead(dataframe = dataframe,
                                covariates = covariates,
                                lead = TRUE,
                                nleads = nleads,
                                grouping = grouping,
                                skips = skips)

# View(just.lead$dataframe) # views lagged & lead dataframe
# print(just.lead$new.covariates) # outputs new column names

both.lag.lead = data.frame.lag.lead(dataframe = dataframe,
                        covariates = covariates,
                        lag = TRUE,
                        lead = TRUE,
                        nlags = nlags,
                        nleads = nleads,
                        grouping = grouping,
                        skips = skips)

# View(both.lag.lead$dataframe) # views lagged & lead dataframe
# print(both.lag.lead$new.covariates) # outputs new column names

vector.lag.lead = data.frame.lag.lead(dataframe = dataframe,
                                      covariates = covariates,
                                      lag = TRUE,
                                      lead = TRUE,
                                      nlags = 1299, # gets 'overwritten' in practice
                                      nleads = 123456789, # because of vector input
                                      vlags = c(4,1),
                                      vleads = c(1,9),
                                      grouping = grouping,
                                      skips = 100) # also 'overwritten'

# View(vector.lag.lead$dataframe) # views lagged & lead dataframe
# print(vector.lag.lead$new.covariates) # outputs new column names
