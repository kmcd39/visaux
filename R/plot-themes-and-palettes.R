
# log scale helpers -------------------------------------------------------

# originally from ccpDOT project.


#' breaks_log10
#'
#' Manually adds breaks at very power of 10 for log-scale axes.
#'
#' solution adapted from https://r-graphics.org/recipe-axes-axis-log-ticks
#'
#' @param x vector mapped to x or y scales.
#' @param break.intervals A vector of numerics between [1,9] for there to be
#'   major breaks at every power of 10. I.e. every 1e6, 1e7 if left at just one,
#'   but 1e6, 3e6, 1e6, 3e7 if includes `c(1, 3)`.
#'
#'  @export breaks_log10
#'
breaks_log10 <- function(x, break.intervals = c(1)) {

  low <- floor(log10(min(x)))
  high <- ceiling(log10(max(x)))

  #browser()
  breaks <- as.vector(outer(break.intervals, 10 ^ (seq.int(low, high))))

  return(breaks)
}

#' minor.breaks_log10
#'
#'
#' Manually adds minor breaks at very power of 5 for log-scale axes.
#'
#'
minor.breaks_log10 <- function(x) {

  low <- floor(log10(min(x)/5))
  high <- ceiling(log10(max(x)/5))

  # outer(c(2.5, 5, 7.5),
  #       10^(seq.int(low, high))
  #       ) %>%
  #   as.vector()

  5 * 10^(seq.int(low, high))
}


#' ggthme.logscales
#'
#' @param axis.lbl.style whether to apply `visaux::number.to.formatted.string`
#'   to log axis labels
#' @param x.break.intervals,y.break.intervals A vector of numerics between [1,9]
#'   for there to be major breaks at every power of 10. I.e. every 1e6, 1e7 if
#'   left at just one, but 1e6, 3e6, 1e6, 3e7 if includes `c(1, 3)`.
#'
#' @import ggplot2
#'
#' @export ggthme.logscales
#'
ggthme.logscales <- function(
     x.break.intervals = c(1)
    ,y.break.intervals = c(1)
    ,axis.lbl.style = F
    ) {

  if(axis.lbl.style){
    lbl.style <- ~purrr::map(.x, visaux::number.to.formatted.string)
  } else {
    lbl.style <- ggplot2::waiver()
  }

  list(
    "base.theme" =
      hrbrthemes::theme_ipsum()

    ,"log.x.axis" =
      scale_x_log10(
        breaks = breaks_log10
        ,minor_breaks  =  minor.breaks_log10
        ,labels = lbl.style
      )

    ,"log.y.axis" =
      scale_y_log10(
        breaks = breaks_log10
        ,minor_breaks = minor.breaks_log10
        ,labels = lbl.style
      )

    ,"grid.details" =
      theme(panel.grid.major =
              element_line(color = "grey65",
                           linewidth = 0.25)
            ,panel.grid.minor =
              element_line(color = "grey75",
                           linewidth = 0.18)
      )
  )


}

# general ggplot themes ---------------------------------------------------

#' ggemphatic.facet.labels
#'
#' Adds theme
#'
#' @export ggemphatic.facet.labels
#'
ggemphatic.facet.labels <- function(
    strip.color = visaux::jewel.pal()[4]) {

  ggplot2::theme(
    strip.background = element_rect(
      fill = strip.color )
    ,strip.text = element_text(
      face = 'bold'
      ,color = 'white'
      #,size = fontsize - 1
      )
  )

}


#' upper.legend.box
#'
#' Draws a box around a legend and top-justifies
#'
#' @export upper.legend.box
#'
upper.legend.box <- function(
     leg.pos = 'right'
    ,lmargin = margin(4,4,4,4)
    ) {

  ggplot2::theme(
     legend.position = leg.pos
    ,legend.box.background = element_rect(color="gray30"
                                         , size=.05)
    ,legend.box.margin = lmargin
    ,legend.justification = 'top'
  )
}



# palettes ----------------------------------------------------------------


#' jewel.pal
#'
#' Returns some jewel-toney colors that can be nice for discrete data
#' visualization. Colorblindness simulator seems to show decent accessibility.
#'
#' @export jewel.pal
jewel.pal <- function() {

  jewel.tones <- c( '#880088', '#000088'
                   ,'#dd9933', '#008888'
                   ,'#bb1e39', '#88ac7f'
  )
  return(jewel.tones)
}

# data tables -------------------------------------------------------------


#' dt.table.template
#'
#' Wrapped DT::datatable defaults
#'
#' @export dt.table.template
#'
dt.table.template <- function(x) {

  DT::datatable(x
                ,extensions = 'Buttons'
                ,options = list(dom='Bfrtp'
                                ,ordering=F
                                ,pageLength =20
                                ,buttons=c('copy','csv','excel')))
}
