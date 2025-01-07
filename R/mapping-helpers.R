
# setup leaflet tiles for ggplot bkg -------------------------------------------

#' bbox2ggcrop
#'
#' Wraps `coord_sf`, which can be used to crop a map made in ggplot. Uses an sf object
#' or bbox to set crop area.
#'
#' @param sfx bbox or sf object
#' @param clip passed onto `ggplot2::coord_sf`
#'
#'
#' @export bbox2ggcrop
bbox2ggcrop <- function(sfx, crs = 4326, clip = 'on') {

  # turn sf to bbox if needed
  if(! 'bbox' %in% class(sfx))
    sfx <- sfx %>% sf::st_transform(crs) %>% sf::st_bbox()

  # return coord_sf
  ggplot2::coord_sf( xlim = c(sfx[['xmin']], sfx['xmax'])
                     ,ylim = c(sfx[['ymin']], sfx['ymax'])
                     ,clip = clip)
}

