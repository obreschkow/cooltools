#' Multi-dimensional adaptive kernel density estimation
#'
#' @importFrom pracma meshgrid
#' @importFrom Rcpp sourceCpp
#' @importFrom FNN knn.dist
#' @importFrom graphics rasterImage text par
#'
#' @description Produces a 2D kernel density estimation on a 2D grid from a D-dimensional (D>=2) point set
#'
#' @param x N-by-D vector of x-coordinates or N-by-2 matrix of (x,y)-coordinates
#' @param w optional N-element vector with weights
#' @param s characteristic smoothing length
#' @param nx integer specifying the number of equally space grid cells along the x-axis; the number ny of pixels along the y-axis is determined automatically from xlim and ylim.
#' @param xlim 2-element vector specifying the x-range
#' @param ylim 2-element vector specifying the y-range; if needed, this range is slightly adjusted to allow for an integer number of pixels.
#' @param smoothing positive linear smoothing factor, the larger, the more smoothed the Kernel estimation turns out.
#' @param sigma.min optional value, specifying the minimum blurring of any pixel, expressed in standard deviations in units of pixels
#' @param sigma.max optional value, specifying the maximum blurring of any pixel, expressed in standard deviations in units of pixels
#' @param reflect vector of strings c('left','right','bottom','top') specifying the edges, where the data should be reflected to avoid probability density leaking outside the window
#' @param algorithm character string: "fast" is a purely 2D smoothing method that ignores higher dimensional information and applies a smoothing size to each pixel that depends on the number (or mass, if weights given) of objects in each pixel. "nn" is a more sophisticated Kernel density estimator that uses D-dimensional nearest neighbor separations to smooth each data point individually.
#' @param probability logical flag. If TRUE, the output field is normalised such that sum(field)dpixel^2=1. If FALSE (default), the field is such that sum(field)dpixel^2 equals the effective number of particles (or effective mass, if weights are given) in the range specified by xlim and ylim, including particle fractions that have been smoothed into the field and excluding particle fractions that have been smoothed out of it.
#'
#' @return Returns a list of items
#' \item{field}{2D array of smoothed density field.}
#' \item{x}{nx-element vector of cell-center x-coordinates.}
#' \item{y}{ny-element vector of cell-center y-coordinates.}
#' \item{xbreak}{(nx+1)-element vector of cell-edge x-coordinates.}
#' \item{ybreak}{(ny+1)-element vector of cell-edge y-coordinates.}
#' \item{dpixel}{grid spacing along x-coordinate and y-coordinate.}
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{griddata}}
#'
#' @examples
#' # make a mock sample of n d-dimensional points from
#' # three different components (1D line, 2D square, d-D normal distr)
#' d = 3 # number of dimensions of mock point set; try to choose different values 2, 3, 4, ...
#' n = 1e4 # number of particles per component
#' set.seed(1)
#' x = rbind(cbind(array(rep(runif(n,-1,1),2),c(n,2)),array(0,c(n,d-2))),
#'           cbind(array(runif(2*n),c(n,2)),array(0,c(n,d-2))),
#'           array(rnorm(d*n),c(n,d)))
#'
#' # grid total projected probability density
#' npixels = 500 # number of pixels along a grid side
#' q = midseq(-3,3,npixels)
#' f1 = outer(dnorm(q),dnorm(q),'*')/3+outer(dunif(q),dunif(q),'*')/3
#' q = seq(round(npixels/3),round(npixels*2/3))
#' f1[q+npixels*(q-1)] = f1[q+npixels*(q-1)]+(npixels/6)^2/length(q)/3
#'
#' # grid point sample for display
#' f2 = griddata(x[,1:2], n=npixels, min=c(-3,-3), max=c(3,3), type='probability')$field
#'
#' # recover 2D projected pdf from 3D point sample using two different methods
#' f3 = kde2(x, n=npixels, xlim=c(-3,3), ylim=c(-3,3), algorithm='fast',probability=TRUE)$field
#' f4 = kde2(x, n=npixels, xlim=c(-3,3), ylim=c(-3,3), algorithm='nn', probability=TRUE)$field
#'
#' # plot the 2D fields
#' img = function(f,x,y,title) {
#'   graphics::rasterImage(rasterflip(lim(f)^0.3),x,y,x+0.99,y+0.99)
#'   graphics::text(x+0.05,y+0.9,title,col='orange',pos=4)
#' }
#' graphics::par(mar=rep(0.1,4))
#' nplot(c(0,2),c(0,2),asp=1)
#' img(f1,0,1,'Input pdf')
#' img(f2,1,1,'Random sample')
#' img(f3,0,0,'Recovered pdf (fast)')
#' img(f4,1,0,'Recovered pdf (nn)')
#'
#' @export
#'
kde2 = function(x, w=NULL, s=1, nx=300, xlim=NULL, ylim=NULL,
                smoothing = 1, sigma.min=0, sigma.max=Inf,
                reflect='', algorithm='nn', probability=FALSE) {

  # handle inputs
  if (is.null(dim(x)) || length(dim(x))!=2 || dim(x)[2]<2) stop('x must be a vector or a N-by-D matrix with D>=2.')
  if (is.null(xlim)) xlim=range(x[,1])
  if (is.null(ylim)) ylim=range(x[,2])
  npoints.all = dim(x)[1]

  # make grid spacing and tweak y-range to contain an integer number of pixels
  dpixel = diff(xlim)/nx
  ny = round(diff(ylim)/dpixel)
  deltay = ny*dpixel-diff(ylim)
  ylim = ylim+c(-0.5,0.5)*deltay

  # determine size of embedding frame
  h = max(1,round(min(c(nx,ny))/10)) # [pixels] thickness of additional margin
  xlim.frame = c(xlim[1]-dpixel*h,xlim[2]+dpixel*h)
  ylim.frame = c(ylim[1]-dpixel*h,ylim[2]+dpixel*h)

  # select points inside embedding frame
  s = which(x[,1]>=xlim.frame[1] & x[,1]<=xlim.frame[2] & x[,2]>=ylim.frame[1] & x[,2]<=ylim.frame[2])
  if (length(s)>=1) {

    x = rbind(x[s,])
    if (!is.null(w)) w = w[s]

    if (algorithm=='fast') {

      # parameters fixed by developer
      smoothing.scaling = 0.4 # overall linear smoothing factor
      d = 0.1 # step between standard deviations in pixels
      n.sd = 3 # number of standard deviations considered

      # make smoothing kernels
      n.pix = nx*ny
      sd.max = max(2*d,min(min(c(nx,ny))/10,sigma.max))
      sd = seq(0,sd.max,by=d) # list of standard deviations
      n.kernels = length(sd)
      kernel = {}
      kernel[[1]] = matrix(c(0,0,0,0,1,0,0,0,0),3,3)
      kern.index = rep(1,n.kernels)
      kern.length = rep(9,n.kernels)

      for (i in seq(2,n.kernels)) {
        kern.index[i] = kern.index[i-1]+kern.length[i-1]
        hh = ceiling(sd[i]*n.sd)
        if (hh>h) {
          n.kernels = i-1
          break
        }
        n.side = 2*hh+1 # number of pixels per side
        mesh = pracma::meshgrid(seq(-hh,hh))
        kernel[[i]] = exp(-(mesh$X^2+mesh$Y^2)/2/sd[i]^2)
        kernel[[i]] = kernel[[i]]/sum(kernel[[i]])
        kern.length[i] = n.side^2
      }

      # grid data onto embedding frame
      count = griddata(x[,1:2],n=c(nx,ny)+2*h,min=c(xlim.frame[1],ylim.frame[1]),max=c(xlim.frame[2],ylim.frame[2]),type='counts')$field
      if (is.null(w)) {
        map = count
      } else {
        map = griddata(x[,1:2],w=w,n=c(nx,ny)+2*h,min=c(xlim.frame[1],ylim.frame[1]),max=c(xlim.frame[2],ylim.frame[2]),type='counts')$field
      }

      field = kde2stampxx(map, count, h, smoothing*smoothing.scaling, sigma.min, sd.max, d, n.kernels, unlist(kernel), kern.index, kern.length)[h+(1:(nx+2*h)),h+(1:(ny+2*h))]

    } else if (algorithm=='nn') {

      if (!requireNamespace("EBImage", quietly=TRUE)) {
        stop('Package EBImage is needed to run kde2 in with nn algorithm.')
      }

      # parameters fixed by developer
      npoints = length(s)
      smoothing.scaling = ifelse(dim(x)[2]==2,3.5,1.6) # overall linear smoothing factor
      sub = ifelse(npoints<5e3,2,1) # stepping of smoothing kernel in factors of 2^(1/sub)
      k = max(2,round(log10(npoints+1))) # number of nearest neighbors the smaller the faster

      # determine smoothing kernel size for each particle (in bins)
      nn = FNN::knn.dist(x,k=k)[,k]
      nn = nn/dpixel # mean distance to the k nearest neighbors in pixel
      idist = round(log2(nn)*sub)
      idist.min = log2(max(0.1,sigma.min)/smoothing/smoothing.scaling)*sub
      idist.max = log2(min(min(c(nx,ny))/10,sigma.max)/smoothing/smoothing.scaling)*sub
      idist = pmax(pmin(idist,idist.max),idist.min)

      # smooth particles from smallest to largest kernel
      field = array(0,c(nx,ny)+2*h)
      for (i in unique(idist)) {
        sel = which(idist==i)
        g = griddata(rbind(x[sel,1:2]),w=w,n=c(nx,ny)+2*h,min=c(xlim.frame[1],ylim.frame[1]),max=c(xlim.frame[2],ylim.frame[2]),type='density')
        sigma = 2^(i/sub)*smoothing*smoothing.scaling
        field = field+EBImage::gblur(g$field,sigma)
      }
      field[field<0] = 0 # remove very small negatives due to floating point inaccuracies of gblur

    } else {

      stop('unknown algorithm')

    }

    # reflect boundaries if desired
    if (any(reflect=='left')) field[(h+1):(2*h),] = field[(h+1):(2*h),]+field[h:1,]
    if (any(reflect=='right')) field[(h+nx):(1+nx),] = field[(h+nx):(1+nx),]+field[(h+nx+1):(2*h+nx),]
    if (any(reflect=='bottom')) field[,(h+1):(2*h)] = field[,(h+1):(2*h)]+field[,h:1]
    if (any(reflect=='top')) field[,(h+ny):(1+ny)] = field[,(h+ny):(1+ny)]+field[,(h+ny+1):(2*h+ny)]

    # cut margin of embedding frame
    field = field[h+(1:nx),h+(1:ny)]

    # normalised field
    if (probability) field = field/(sum(field)*dpixel^2)

  } else {

    field = array(0,c(nx,ny))

  }

  # make output data
  out = list(field=field,
             x=midseq(xlim[1],xlim[2],nx),
             y=midseq(ylim[1],ylim[2],ny),
             xbreak=seq(xlim[1],xlim[2],nx+1),
             ybreak=seq(ylim[1],ylim[2],ny+1),
             dpixel=dpixel)

}
