#' ts.labels.geom
#'
#' Returns a ggplot2 geometry that anticipates labeling lines of a multi-group
#' time series plot. Labels will appear at end of the plot.
#'
#' @param tsx time series data
#' @param group.col column indicating group. Each will be labelled.
#' @param date.col date or time column for x axis.
#'
#'
#' @export ts.labels.geom
ts.labels.geom <- function(tsx,
                           group.col,
                           date.col = 'date'
                           ,size = 3
                           ,alpha = .6
                           ,color = 'black') {

  require(tidyverse)
  require(ggrepel)

  .date <- rlang::sym(date.col)
  .group <- rlang::sym(group.col)

  ts.lbls <- tsx %>%
    filter(!!.date == max(!!.date))

  lbls <- ggrepel::geom_label_repel( data = ts.lbls
                                     ,aes(label = !!.group
                                          ,fill = !!.group
                                     )
                                     ,color = color
                                     ,size = size
                                     ,alpha = alpha
  )
  return(lbls)
}



# recessions for time series ----------------------------------------------


#' get.recessions.start.ends
#'
#' Gets recession starts and endpoints; queries from FRED series "JHDUSRGDPBR"
#' and sets up a tibble w start and end dates as two columns.
#'
#' @param recessions as pulled from `fredr::fredr(series_id = 'JHDUSRGDPBR')`,
#'   with additional arguments for month/qtr/annual or observation start.
#'
#' @export get.recessions.start.ends
get.recessions.start.ends <- function(recessions = NULL) {


  if(is.null(recessions))
    recessions <- fredr::fredr(
      series_id = 'JHDUSRGDPBR') %>%
      select(-matches('^realtime'))

  recessions <- recessions %>%
    mutate(start =
             lag(value, order_by = date) %in% c(0, NA) &
             value == 1
           ,end =
             lead(value, order_by = date) %in% c(0, NA) &
             value == 1
    )

  recessions <- tibble(
    starts = filter(recessions, start)$date
    ,ends = filter(recessions, end)$date
  )

  return(recessions)
}



#' gg.recessions.ts
#'
#' Adds colored bars to a (time-series) ggplot, as to indicate recessions. By
#' default queries recessions from FRED series "JHDUSRGDPBR."
#'
#' @param recessions A time series of recessions (or other periodizations) to
#'   add to ggplot. Expects a `date` column.
#' @param rcolor color for bars indicating recessions etc.
#' @param ... passed onto `fredr::fredr`; months or quarters and obs start may
#'   be helpful.
#'
#' @export gg.recessions.ts
gg.recessions.ts <- function(recessions = NULL
                             ,rcolor = 'grey90'
) {

  if(is.null(recessions))
    recessions <- get.recessions.start.ends()

  lyr <- geom_rect(
    data = recessions
    ,aes(
      xmin = starts
      ,xmax = ends
      ,ymin = -Inf
      ,ymax = Inf
    )
    #,ymin = -Inf
    ,fill = rcolor
    ,color = rcolor
    ,inherit.aes = F
    ,show.legend = F
  )

  return(lyr)
}

