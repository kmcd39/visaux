




#' ragg.wrapper
#'
#' Wraps `ragg::agg_png` to save a plot as .png. An alternative to ggsave. See
#' https://www.tidyverse.org/blog/2019/07/ragg-0-1-0/ for pkg intro.
#'
#' @param fn filename to save to; exclude extension. If NULL, will just saved to i.e., `-1.png`,
#'   incrementing if other plots exist
#' @param plot to save
#' @param sv.dir save directory. Defaults to `visuals/` subdirectory w/in project
#'   dir.
#' @param width,height,res,units passed onto `ragg::agg_png`
#'
#' @export ragg.wrapper
ragg.wrapper <- function(fn = NULL
                         ,plot = ggplot2::last_plot()
                         ,sv.dir = 'visuals/'
                         , width = 6
                         , height = 6
                         , res = 388
                         , units = 'in'
                         ) {

  if(!dir.exists(sv.dir))
    dir.create(sv.dir)

  # gen filename if NULL
  if(is.null(fn)) {

    extant.defaults <- list.files(sv.dir
                                  , pattern = '-[0-9]*\\.png')

    nm <- stringr::str_extract(extant.defaults
                               ,'[0-9]+')
    if(length(nm) == 0)
      nm <- 0

    nm <- max(as.numeric(nm)) + 1

    fn <- glue::glue('-{nm}')
  }

  path <- paste0(sv.dir
                 ,fn, '.png')

  ragg::agg_png(path
                ,width = width
                ,height = height
                ,res = res
                ,units = units)
  print(plot)
  invisible(dev.off())
}




#' vector.ggsave
#'
#' Wraps ggsave with some defaults to save vectorized images.
#'
#' @param dir,fn directory and filename to save to
#'
#' @export vector.ggsave
#'
vector.ggsave <- function(plot,
                         fn,
                         sv.dir = "visuals/",
                         ext = "svg",
                         height = 7,
                         width = height * 1.2,
                         ...) {

  if(!dir.exists(sv.dir))
    dir.create(sv.dir)

  # gen filename if NULL
  if(is.null(fn)) {

    extant.defaults <- list.files(sv.dir
                                  , pattern = '-[0-9]*\\.png')

    nm <- stringr::str_extract(extant.defaults
                               ,'[0-9]+')
    if(length(nm) == 0)
      nm <- 0

    nm <- max(as.numeric(nm)) + 1

    fn <- glue::glue('-{nm}')
  }

  path <- paste0(sv.dir
                 ,fn, '.', ext
                 )

  ggplot2::ggsave(
    filename = path,
    plot = plot,
    height = height,
    width = width,
    ...
  )
}
