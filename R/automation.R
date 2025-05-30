#' Generate temporal lead or lag embeddings
#'
#' @param dataframe a tidy dataframe
#' @param covariates a character vector of strings that specify covariates to lag in the dataframe
#' @param nlags integer number of times to lag the covariates (default 0)
#' @param nleads integer number of times to lead the covariates (default 0)
#' @param vlags vector of integers for creating multiple lags, overrides nlags & skips (default c())
#' @param vleads vector of integers for creating multiple leads, overrides nleads & skips (default c())
#' @param grouping string specifying column name to group data by to ensure that lagging does not go across groups
#' @param skips integer number of entries to skip across each separate lag (default 1)
#'
#' @return
#' A named list with two components
#'
#'   \item{dataframe}{A dataframe with the original columns of the input dataframe and additional columns}
#'   \item{new.covariates}{A character vector of the additional columns names}
#'
#' The additional column names are created with the following string pattern:
#'
#'
#' @export
#'
#' @examples
#'
#' df <- data.frame(x = 1:20,
#'                  y = 20:1)
#'
#' df_lag <- data.frame_lag_lead(df, covariates = "x")
#' df_lead <- data.frame_lag_lead(df, covariates = "x", nleads = 1)


data.frame_lag_lead = function(dataframe, covariates,
                               nlags = 0, nleads = 0, vlags = c(), vleads = c(),
                               grouping = NA, skips = 1) {


  if (nlags > 0 | !is.null(vlags)) to.lag = TRUE else to.lag = FALSE
  if (nleads > 0 | !is.null(vleads)) to.lead = TRUE else to.lead = FALSE

  new.covariates = vector()

  if (!to.lag & !to.lead) {

    # if user does not specify any lag or lead arguments, default behavior
    # will be one lag

    func = dplyr::lag
    prefix = "prev"
    n.level = 1
    vec = vlags
    skips = 1

    warning("Lagged covariates by 1 as default behavior, specify nlags, vlags, or lead
            equivalents to change behavior")

  }
  else if ((to.lag & to.lead)) {

    # recursively call function to lag and lead separately
    lagging = data.frame_lag_lead(dataframe, covariates, nlags = nlags, vlags = vlags,
                                  grouping = grouping, skips = skips)
    leading = data.frame_lag_lead(lagging[[1]], covariates, nleads = nleads, vleads = vleads,
                                  grouping = grouping, skips = skips)

    # return final data frame object and combine the lag & lead new.covariates vectors
    return(list(dataframe = leading[[1]],
                new.covariates = c(lagging[[2]], leading[[2]])))

  } else if (to.lag) {

    # give placeholder variables the appropriate lagging values
    func = dplyr::lag
    prefix = "prev"
    n.level = nlags
    vec = vlags

  } else if (to.lead) {

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
    dataframe = dataframe |>
      dplyr::group_by(!!rlang::sym(grouping))
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
    dataframe = dataframe |>
      dplyr::mutate(!!new.covariates[[i]] := func(!!rlang::sym(repeated.covariates[[i]]),
                                             n=lag.lead.distance[[i]]))
  }

  if (!is.na(grouping)) {
    dataframe = dataframe |>
      dplyr::ungroup()
  }

  if (to.lag) {
    message(length(new.covariates), " new lag columns in dataframe")
  } else if (to.lead) {
    message(length(new.covariates), " new lead columns in dataframe")
  }

  # returns new, mutated dataframe and new column names
  return(list(dataframe = dataframe,
              new.covariates = new.covariates))

}
