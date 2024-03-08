

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
#' @export cap.at.quantile
cap.at.quantile <- function(x, ptile = .95, na.rm = T) {

  dplyr::if_else(x >= quantile(x, probs = ptile
                               ,na.rm = na.rm)
                 ,quantile(x, probs = ptile
                           , na.rm = na.rm)
                 ,x
  )
}


#' ggcapped.labels
#'
#' Use in a ggplot2 scales for capped labels. Intended to be passed onto
#' `labels` argument. I.e., scale_fill_viridis_c(labels = visaux::ggcapped.labels(x))
#'
#' @export ggcapped.labels
ggcapped.labels <- function(x, tails = 'one'
                                 ) {
  x[length(x)] <- paste0('>', x[length(x)])

  if(tails == 'one')
    return(x)

  x[1] <- paste0('<', x[1])

  return(x)
}






# older fcns --------------------------------------------------------------

# i want to break out the fcn that just transforms i.e., (0,5] default cut label
# into more outward facing versions...

#' get.quantile.breaks
#'
#' bins a sequential variable based on value ranges. It can often
#' make visualizations easier to look at by allowing more striking differences
#' and keeping extremes from dominating the color scale. the highlight_top
#' parameter ensures a separate bin for top 1%. Called from
#' quantile.bin_format.
#'
#' @inheritParams quantile.bin_format
#' @param n_breaks target number of breaks for x
#' @param highlight_top_percent Whether to break out an additional bin for the top
#'   1%. Helpful to break out upper outliers.
#'
get.quantile.breaks <- function(x, n_breaks = 6, highlight_top_percent = F, ...) {

  if (highlight_top_percent)
    probs <- c(seq(0, 0.99, 0.99/n_breaks), 1)
  else
    probs <- seq(0, 1, 1/n_breaks)

  breaks <- quantile(x, probs = probs, na.rm = T)
  breaks <- unique(breaks)
  return(breaks)
}

#' bin_from_breaks
#'
#' From a determined set of breakpoints, put data into bins
#'
#' @inheritParams quantile.bin_format
#' @param format_breaks whether to create "x-y" type character labels for buckets rather
#'   than default labels from `cut`
#' @param digits Digits to pass on to `q.format` if using formatting
#'
#' @export bin_from_breaks
bin_from_breaks <- function(x, breaks, format_breaks = TRUE, digits = 2, ...) {

  if (length(breaks) == 1)
    return(x)

  if( format_breaks )
    .labels <-
      purrr::map_chr(1:(length(breaks) - 1),
                     ~paste0(q.format(breaks[.], digits = digits), " - ",
                             q.format(breaks[. + 1], digits = digits)))
  else
    .labels <- NULL

  cut(x, breaks = breaks,
      labels = .labels,
      include.lowest = T)
}


#' quantile.bin_format
#'
#' Bins a sequential variable based on value ranges. It can often make visualizations
#' easier to look at by allowing more striking differences and keeping outliers from
#' dominating the color scale. Wraps `get_breaks` and `get.quantile.breaks`, which can
#' also be used separately
#'
#' @param x numeric vector
#' @inheritDotParams bin_from_breaks
#' @inheritDotParams get.quantile.breaks
#'
#' @export quantile.bin_format
quantile.bin_format <- function(x, ...) {
  breaks <- get.quantile.breaks(x, ...)
  bin_from_breaks(x, breaks, ...)
}



#' get_mean_from_interval
#'
#' Gets means from intervals in (xx,yy) or (xx,yy] form (for example, common
#' output of `cut` function); this transforms into numeric for the point histogram
#'
#' @param interval interval in form (xx,yy) or (xx,yy]
#'
#' @export get_mean_from_interval
get_mean_from_interval <- function(interval) {
  mean_list <- stringr::str_extract_all(interval, "-?[e+0-9.]+") %>%
    purrr::map_dbl(~mean(as.numeric(.), na.rm = T))
  return(mean_list)
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
number.to.formatted.string <- function(
     x
    ,format.fcn =
      #  function(x) format(x, big.mark = ',', digits = 1 scientific = F)
      scales::label_comma(accuracy = .1, drop0trailing = T)
    ,pluralize = F
) {

  #browser()

  #format.fcn <- function(x) format(x, ..., scientific = F, drop0trailing = T)

  div.factors <- c(1e3, 1e6, 1e9, 1e12)
  div.lbls <-  c('thousand', 'million', 'billion', 'trillion')

  if( abs(x) < min(div.factors) )
    return( format.fcn(x) )

  if(pluralize)
    div.lbls <- paste0(div.lbls, 's')

  # max(div.factors[div.factors < 5e10 ])
  # ?max(div.factors[div.factors < 5 ])
  div.factor <- max(div.factors[div.factors < abs(x) ])

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
#' @export
format_as.numeric <- function(x) {
  as.numeric(
    gsub(",|%|\\$", "", x)
  )
}


