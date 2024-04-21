#' Title
#'
#' @param x occurence data - UD object
#' @param DF Rasterize the probability density function "PDF", probability mass function "PMF", or cumulative distribution function "CDF".
#' @param ...
#'
#' @importFrom terra rast
#' @importFrom terra set.values
#' @importFrom terra xmin
#' @importFrom terra xmax
#' @importFrom terra ymin
#' @importFrom terra ymax
#' @importFrom dplyr last
#'
#'
#' @return A terra object
#' @export
#'
#' @examples
#' rast(occurence_data)
rast.UD <- function(x,DF="CDF",...)
{
  proj <- attr(x,"info")$projection
  UD <- x

  dx <- UD$dr[1]
  dy <- UD$dr[2]

  xmn <- UD$r$x[1]-dx/2
  xmx <- dplyr::last(UD$r$x)+dx/2

  ymn <- UD$r$y[1]-dy/2
  ymx <- dplyr::last(UD$r$y)+dy/2

  z <- UD$r$z

  # probability mass for the cells
  if(DF=="PMF")
  { UD <- UD[["PDF"]] * prod(UD$dr) }
  else
  { UD <- UD[[DF]] }

  if(length(dim(UD))==2)
  {
    UD <- t(UD[,dim(UD)[2]:1])
    R <- terra::rast(UD,#xmin=xmn,xmax=xmx,ymin=ymn,ymax=ymx,
                     crs=proj)
    terra::xmin(R) <- xmn
    terra::xmax(R) <- xmx
    terra::ymin(R) <- ymn
    terra::ymax(R) <- ymx
  }
  else
  {
    UD <- aperm(UD[,terra::dim(UD)[2]:1,],c(2,1,3))
    R <- terra::rast(UD,xmin=xmn,xmax=xmx,ymin=ymn,ymax=ymx,crs=proj)
    R <- terra::set.values(R,z,name="height")
  }

  return(R)
}

#' Title
#'
#' @param rast terra object
#' @param maxNA value below which values are to be set to NA
#'
#' @return terra object with values below maxNA set to NA
#' @export
#'
#' @examples
#' rast <- reclassNA(rast, 0.05)
reclassNA <- function(rast, maxNA) {
  rast[rast < maxNA] <- NA
  return(rast)
}
