% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping-helpers.R
\name{get.stamen.bkg}
\alias{get.stamen.bkg}
\title{get.stamen.bkg}
\usage{
get.stamen.bkg(
  sfx,
  maptype = c("toner-background", "toner-lines", "watercolor"),
  zoom = 10,
  ...
)
}
\arguments{
\item{sfx}{bbox or sf object to get background for}

\item{maptype}{passed onto \code{ggmap::get_stamenmap}. 'toner-background' and
'toner-lines' are good options for basic black/white background maps with major
features. 'Lines' seems to emphasize small streets more and has less black on
it. 'Watercolor' is pretty but very colorful and bad for chloropleths. Could be
interesting for some other types of visuals.}
}
\description{
Gets stamen base tiles based on supplied bbox of \code{sf} object.
}
\details{
Unfortunately, ggmap has some idiosyncracies with geom_sf. If combining these into
single plot, remember:
\enumerate{
\item include \code{inherit.aes = F} in geom_sf layer
\item Match sf crs to that of stamen tiles. Should be epsg = 4326 (but looks like
google maps uses different)
}
}
