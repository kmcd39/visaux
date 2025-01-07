
# log scale helpers -------------------------------------------------------


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
#' @export breaks_log10
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

  5 * 10 ^ (seq.int(low, high))
}


#' ggthme.logscales
#'
#' @param axis.lbl.style whether to apply `visaux::number.to.formatted.string`
#'   to log axis labels
#' @param break.intervals A vector of numerics between [1,9] for there to be
#'   major breaks at every power of 10. I.e. every 1e6, 1e7 if left at just one,
#'   but 1e6, 3e6, 1e6, 3e7 if includes `c(1, 3)`.
#' @param minor.breaks List with elements for x and y to pass onto
#'   `minor_breaks` in the scale
#'
#' @export ggthme.logscales
#'
ggthme.logscales <- function(
     break.intervals =
       list( x = c(1)
            ,y = c(1))
    ,minor.breaks =
      list( x = minor.breaks_log10
           ,y = minor.breaks_log10)
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
      ggplot2::scale_x_log10(
        breaks = ~breaks_log10(.x, break.intervals$x)
        ,minor_breaks  =  minor.breaks$x
        ,labels = lbl.style
      )

    ,"log.y.axis" =
      ggplot2::scale_y_log10(
        breaks = ~breaks_log10(.x, break.intervals$y)
        ,minor_breaks = minor.breaks$y
        ,labels = lbl.style
      )

    ,"grid.details" =
      ggplot2::theme(
        panel.grid.major =
          ggplot2::element_line(color = "grey65",
                                linewidth = 0.25)
        ,panel.grid.minor =
          ggplot2::element_line(color = "grey75",
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
    strip.background = ggplot2::element_rect(
      fill = strip.color )
    ,strip.text = ggplot2::element_text(
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
    ,legend.box.background =
      ggplot2::element_rect(color="gray30"
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
#'
jewel.pal <- function() {

  jewel.tones <- c( '#880088', '#000088'
                   ,'#dd9933', '#008888'
                   ,'#bb1e39', '#88ac7f'
  )
  return(jewel.tones)
}

#' cci.pal
#'
#'
#' @export cci.pal
#'
cci.pal <- function(n = 5 ) {

  cci.cols <- c(
     "blues" = "#6599A9"
     ,"purples" = "#A54F9B"
    ,"greens" = "#2A845A"
    ,"reds" = "#F1573D"
    ,"yellows" = "#F9E26F"
    ,"oranges" = "#F89B45"
  )

  return(cci.cols[1:n])

}

# data tables -------------------------------------------------------------


#' dt.table.template
#'
#' Wrapped DT::datatable defaults
#'
#'
#' @export dt.table.template
#'
dt.table.template <- function(x) {

  DT::datatable(x
                ,extensions = 'Buttons'
                ,options = list( dom = 'Bfrtp'
                                ,ordering=F
                                ,pageLength =20
                                ,buttons=c('copy','csv','excel')))
}
