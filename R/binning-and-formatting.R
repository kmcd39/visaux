

# capping and binning -----------------------------------------------------

#' cap.at.quantile
#'
#' Caps values of a vector at a given percentile. Can be helpful for visualizing
#' data with long-tail distributions, especially when color is mapped to the
#' long tail.
#'
#' @param x values
#' @param ptile percentile at which to cap `x`
#'
#'
#' @export cap.at.quantile
cap.at.quantile <- function(x, ptile = .95, na.rm = T) {

  cap <- stats::quantile(x, probs = ptile
                         ,na.rm = na.rm)

  dplyr::if_else(x >= cap
                 ,cap
                 ,x)
}


#' ggcapped.labels
#'
#' Use in a ggplot2 scales for capped labels. Intended to be passed onto
#' `labels` argument. I.e., scale_fill_viridis_c(labels = visaux::ggcapped.labels(x))
#'
#' @export ggcapped.labels
#'
ggcapped.labels <- function(x, tails = 'one'
                                 ) {
  x[length(x)] <- paste0('>', x[length(x)])

  if(tails == 'one')
    return(x)

  x[1] <- paste0('<', x[1])

  return(x)
}



# formating ---------------------------------------------------------------


#' number.to.formatted.string
#'
#' Divides by the largest of thousand, million, billion, trillion and appends
#' string to modify units.
#'
#' @param x A number to format (not a vector)
#' @param format.fcn A formatting function to apply to number part of output.
#' @param pluralize Whether to pluralize; i.e., "million(s)"
#'
#' @export number.to.formatted.string
#'
number.to.formatted.string <- function(
    x
    ,format.fcn =
      #  function(x) format(x, big.mark = ',', digits = 1 scientific = F)
      scales::label_comma(accuracy = .1, drop0trailing = T)
    ,pluralize = F
) {

  #browser()

  if(is.na(x))
    return( as.character(NA) )

  #format.fcn <- function(x) format(x, ..., scientific = F, drop0trailing = T)

  div.factors <- c(1e2, 1e3, 1e6,
                   1e9, 1e12, 1e15)
  div.lbls <-  c("hundred", 'thousand', 'million',
                 'billion', 'trillion', "quadrillion")

  if( abs(x) < min(div.factors) )
    return( format.fcn(x) )

  if(pluralize)
    div.lbls <- paste0(div.lbls, 's')

  # max(div.factors[div.factors < 5e10 ])
  # ?max(div.factors[div.factors < 5 ])
  div.factor <- max(div.factors[div.factors <= abs(x) ])

  fx <-
    paste0(
      format.fcn(x / div.factor),
      ' ',
      div.lbls[which(div.factors == div.factor)]
    )

  return(fx)
}

# number.to.formated.string(-4321713218)
# number.to.formated.string(4)
#
# number.to.formated.string(482319750321894327)
# number.to.formated.string(1.7787134718e3)
# number.to.formated.string(-1.7787134718e10)
# number.to.formated.string(100.210312e6)



#' formated2numeric
#'
#' Turns a formatted number string back into a numeric
#'
#' @export format_as.numeric
#'
format_as.numeric <- function(x) {
  as.numeric(
    gsub(",|%|\\$", "", x)
  )
}


# older fcns --------------------------------------------------------------


#' get_mean_from_interval
#'
#' Gets means from intervals in (xx,yy) or (xx,yy] form (for example, common
#' output of `cut` function); this transforms into numeric for the point histogram
#'
#' @param interval interval in form (xx,yy) or (xx,yy]
#'
#' @importFrom stringr str_extract_all
#' @importFrom purrr map_dbl
#' @importFrom magrittr %>%
#'
#' @export get_mean_from_interval
#'
get_mean_from_interval <- function(interval) {

  mean_list <-
    stringr::str_extract_all(interval, "-?[e+0-9.]+"
                             ) %>%
    purrr::map_dbl(~mean(as.numeric(.), na.rm = T)
                   )

  return(mean_list)
}






