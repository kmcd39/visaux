#' add.plot.theme
#'
#' Add base plot theme.
#'
#' @export add.plot.theme
add.plot.theme <- function( fontsize = 10
                            ,leg.pos = 'top'
                            #,font='Arial'
                            #,facet.label.font.size = fontsize
                            ,...
) {
  require(extrafont)

  list(
    theme_minimal() ,
    theme( panel.grid.major.x = element_blank()
           ,legend.position = leg.pos
           #,panel.grid.major.x = element_blank()
           ,panel.grid.minor = element_blank()
           ,text = element_text(
             size = fontsize,
             family = font)
           # for facet warp labels
           ,strip.background = element_rect(
             fill = aevis::aecom.palette()[2] )
           ,strip.text = element_text(
             face = 'bold'
             ,color = 'white'
             ,size = fontsize - 1)
           ,... )
  )
}


#' add.map.theme
#'
#' Add base map theme.
#'
#' @export add.map.theme
add.map.theme <- function(fontsize = 11
                          ,leg.pos = 'right'
                          #,font='Arial'
                          ,lmargin = margin(4,4,4,4)
                          , ...) {
  require(extrafont)

  list(
    theme_void() ,
    theme( legend.position = leg.pos
           ,text = element_text( size = fontsize
                                 ,family=font)
           ,legend.box.margin = lmargin
           ,legend.justification = 'top'
           ,... )
  )
}


#' upper.legend.box
#'
#' Draws a box around a legend and top-justifies
#'
#' @export upper.legend.box
upper.legend.box <- function() {

  theme(
    legend.box.background = element_rect(color="gray30"
                                         , size=.05)
    ,legend.box.margin = margin(4,4,4,4)
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

  jewel.tones <- c('#880088', '#000088'
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
dt.table.template <- function(x) {

  DT::datatable(x
                ,extensions = 'Buttons'
                ,options = list(dom='Bfrtp'
                                ,ordering=F
                                ,pageLength =20
                                ,buttons=c('copy','csv','excel')))
}
