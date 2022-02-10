#' Plot a spherical function or point set
#'
#' @importFrom graphics par lines polygon text points
#' @importFrom grDevices gray.colors
#' @importFrom raster spPolygons
#' @importFrom sp plot
#'
#' @description Plots a spherical function or a point set in a 2D projection using only standard R graphics. This avoids compatibility issues of rgl, e.g. knitting markdown documents.
#'
#' @param f must be either of:
#'
#' (1) NULL to plot just grid without spherical function
#'
#' (2) a vectorized real function f(theta,phi) of the polar angle theta [0,pi] and azimuth angle [0,2pi]
#'
#' (3) an n-by-2 array of values theta and phi
#'
#' @param n number of grid cells in each dimension used in the plot
#' @param theta0 polar angle in radians at the center of the projection
#' @param phi0 azimuth angle in radians at the center of the projection
#' @param angle angle in radians between vertical axis and central longitudinal great circle
#' @param projection type of projection: "globe" (default), "cylindrical", "mollweide"
#' @param col color map
#' @param clim 2-element vector specifying the values of f corresponding to the first and last color in col
#' @param add logical flag specifying whether the sphere is to be added to an existing plot
#' @param center center of the sphere on the plot
#' @param radius radius of the sphere on the plot
#' @param nv number or vertices used for grid lines and border
#' @param show.border logical flag specifying whether to show a circular border around the sphere
#' @param show.grid logical flag specifying whether to show grid lines
#' @param grid.phi vector of phi-values of the longitudinal grid lines
#' @param grid.theta vector of theta-values of the latitudinal grid lines
#' @param pch point type
#' @param pt.col point color
#' @param pt.cex point size
#' @param lwd line width of grid lines and border
#' @param lty line type of grid lines and border
#' @param line.col color of grid lines and border
#' @param background background color
#' @param ... additional arguments to be passed to the function f
#'
#' @return Returns a list containing the vector \code{col} of colors and 2-vector \code{clim} of values corresponding to the first and last color.
#'
#' @author Danail Obreschkow
#'
#' @examples
#'
#' ## Plot random points on the unit sphere in Mollweide projection
#' set.seed(1)
#' f = cbind(acos(runif(5000,-1,1)),runif(5000,0,2*pi))
#' sphereplot(f,theta0=pi/3,projection='mollweide',pt.col='red')
#'
#' ## Plot real spherical harmonics up to third degree
#' oldpar = par(mar=c(0,0,0,0))
#' nplot(xlim=c(-4,3.5),ylim=c(0,4),asp=1)
#' for (l in seq(0,3)) { # degree of spherical harmonic
#'   for (m in seq(-l,l)) { # order of spherical harmonic
#'
#'     # make spherical harmonic function in real-valued convention
#'     f = function(theta,phi) sphericalharmonics(l,m,cbind(theta,phi))
#'
#'     # plot spherical harmonic
#'     sphereplot(f, 50, col=planckcolors(100), phi0=0.1, theta0=pi/3, add=TRUE, clim=c(-0.7,0.7),
#'                center=c(m,3.5-l), radius=0.47)
#'
#'     if (l==3) text(m,-0.15,sprintf('m=%+d',m))
#'   }
#'   text(-l-0.55,3.5-l,sprintf('l=%d',l),pos=2)
#' }
#' par(oldpar)
#'
#' @export

sphereplot = function(f, n = 100, theta0 = pi/2, phi0 = 0, angle = 0, projection='globe',
                      col = gray.colors(256,0,1), clim=NULL,
                      add = FALSE, center = c(0,0), radius = 1, nv = 500,
                      show.border = TRUE,
                      show.grid = TRUE, grid.phi = seq(0,330,30)/180*pi, grid.theta = seq(30,150,30)/180*pi,
                      pch = 16, pt.col='black', pt.cex=0.5, lwd = 0.5, lty = 1, line.col = 'black', background = 'white', ...) {

  # each projection is characterised by:
  # + limits xlim, ylim specifying the ranges if radius=1
  # + a function sph2xy, which takes a data frame with colums theta,phi and outputs a data frame
  #   with the projected coordinates x,y
  # + a function xy2sph, which takes a data frame with columns x,y and outputs a data frame
  #   with the spherical coordinates columns theta,phi
  # + boundary(nv) a function generating the boundary of the projection in xy with nv points

  # safe use of par()
  oldpar = par(no.readonly = TRUE)
  on.exit(par(oldpar))

  # initialize projection
  if (projection=='globe') {

    xlim = c(-1,1)
    ylim = c(-1,1)
    sph2xy = function(p) {
      behind = p$phi>pi/2 & p$phi<3*pi/2
      v = rep(1,length(p$phi))
      v[behind] = NA
      return(data.frame(x = sin(p$theta)*sin(p$phi)*v,
                        y = cos(p$theta))*v)
    }
    xy2sph = function(p) {
      z = sqrt(1-pmin(1,p$x^2+p$y^2))
      d = 2/n
      return(data.frame(theta = acos(pmin(1,pmax(-1,p$y))),
                        phi = atan2(p$x,z)%%(2*pi),
                        out = pmax(0,abs(p$x)-d/2)^2+pmax(0,abs(p$y)-d/2)^2>1))
    }
    boundary = function(nv) {
      a = midseq(0,2*pi,nv)
      return(cbind(cos(a),sin(a)))
    }
    need.frame = TRUE

  } else if (projection=='cylindrical') {

    xlim = c(0,2*pi)
    ylim = c(0,pi)
    sph2xy = function(p) {
      return(data.frame(x=p$phi, y=pi-p$theta))
    }
    xy2sph = function(p) {
      return(data.frame(theta=pi-p$y, phi=p$x))
    }
    boundary = function(nv) {
      a = midseq(0,2*pi,nv)
      return(cbind(c(xlim,rev(xlim)),c(rep(ylim,each=2))))
    }
    need.frame = FALSE

  } else if (projection=='mollweide') {

    xlim = c(-2*sqrt(2),2*sqrt(2))
    ylim = c(-sqrt(2),sqrt(2))
    sph2xy = function(p) {
      m = mollweide(p$phi, pi/2-p$theta)
      return(data.frame(x=m$x, y=m$y))
    }
    xy2sph = function(p) {
      alpha = asin(p$y/sqrt(2))
      d = 2/n
      return(data.frame(theta = acos((2*alpha+sin(2*alpha))/pi),
                        phi = (pi*p$x/(2*sqrt(2)*cos(alpha)))%%(2*pi),
                        out = pmax(0,abs(p$x)/2/sqrt(2)-d/2)^2+pmax(0,abs(p$y)/sqrt(2)-d/2)^2>1))
    }
    boundary = function(nv) {
      a = midseq(0,2*pi,nv)
      return(cbind(2*sqrt(2)*cos(a),sqrt(2)*sin(a)))
    }
    need.frame = TRUE

  } else {

    stop('unknown projection')

  }

  # prepare rotation
  .rot = function(u, angle=NULL) {
    unorm = sqrt(sum(u^2))
    if (unorm==0) {
      R = diag(3)
    } else {
      if (is.null(angle)) angle = unorm
      u = u/unorm
      c = cos(angle)
      s = sin(angle)
      R = rbind(c(c+u[1]^2*(1-c), u[1]*u[2]*(1-c)-u[3]*s, u[1]*u[3]*(1-c)+u[2]*s),
                c(u[2]*u[1]*(1-c)+u[3]*s, c+u[2]^2*(1-c), u[2]*u[3]*(1-c)-u[1]*s),
                c(u[3]*u[1]*(1-c)-u[2]*s, u[3]*u[2]*(1-c)+u[1]*s, c+u[3]^2*(1-c)))
    }
    return(R)
  }
  R = .rot(c(0,0,1),angle)%*%.rot(c(-1,0,0),theta0-pi/2)%*%.rot(c(0,-1,0),phi0)
  rotate = function(p) {
    xyz = cbind(sin(p$theta)*sin(p$phi),cos(p$theta),sin(p$theta)*cos(p$phi))%*%R
    p$theta = acos(pmin(1,pmax(-1,xyz[,2])))
    p$phi = atan2(xyz[,1],xyz[,3])%%(2*pi)
    return(p)
  }

  # initialize plot
  if (!add) nplot(center[1]+radius*xlim,center[2]+radius*ylim,asp=1)

  # plot projection

  if (!is.null(f)) {

    if (is.function(f)) {

      # make xy-coordinates of plot
      wx = diff(xlim)
      wy = diff(ylim)
      nx = round(n*sqrt(wx/wy))
      ny = round(n*sqrt(wy/wx))
      p = expand.grid(x=midseq(xlim[1],xlim[2],nx), y=midseq(ylim[1],ylim[2],ny))

      # compute inverse projection
      p = xy2sph(p)

      # rotate spherical coordinates
      p = rotate(p)

      # evaluate function
      p$f = f(p$theta,p$phi,...)

      # determine color range
      if (is.null(clim)) clim = range(p$f)
      if (clim[2]==clim[1]) clim=clim+c(-1,1)

      # make color array
      ncol = length(col)
      p$img = col[pmax(1,pmin(ncol,round(0.5+ncol*(p$f-clim[1])/(clim[2]-clim[1]))))]
      p$img[p$out] = background

      # plot raster
      rasterImage(rasterflip(array(p$img,c(nx,ny))), interpolate = T,
                    center[1]+radius*xlim[1], center[2]+radius*ylim[1], center[1]+radius*xlim[2], center[2]+radius*ylim[2])

    } else if (is.array(f) && dim(f)[2]==2) {

      p = data.frame(theta=f[,1], phi=f[,2])
      p = rotate(p)
      p = sph2xy(p)
      p = t(t(p)*radius+center)
      points(p, pch=pch, cex=pt.cex, col=pt.col)

      need.frame = FALSE

    } else {

      stop('unknown type of f')

    }
  }

  # grid lines
  if (show.grid) {

    for (i in seq(length(grid.phi)+length(grid.theta))) {

      if (i<=length(grid.phi)) {
        theta = seq(0,pi,length=nv)
        phi = grid.phi[i]
      } else {
        theta = grid.theta[i-length(grid.phi)]
        phi = seq(0,2*pi,length=nv)
      }

      # rotate spherical coordinates
      xyz = t(R%*%rbind(sin(theta)*sin(phi),cos(theta),sin(theta)*cos(phi)))
      g = data.frame(theta=acos(pmin(1,pmax(-1,xyz[,2]))), phi=atan2(xyz[,1],xyz[,3])%%(2*pi))

      # convert to projected xy-coordinates
      xy = sph2xy(g)
      xy = t(t(xy)*radius+center)

      # cut wrapped lines
      dp = (xy[,1]-cshift(xy[,1],1))^2+(xy[,2]-cshift(xy[,2],1))^2 # square distance to previous
      dp[1] = 0
      dn = cshift(dp,-1)
      k = which(dn>dp*10+mean(dp))
      if (length(k)>0) {
        for (j in rev(k)) {
          xy = rbind(xy[1:j,],c(NA,NA),xy[(j+1):nv,])
        }
      }

      # plot lines
      lines(xy,lwd=lwd,lty=lty,col=line.col)
    }
  }

  # overplot frame to truncated pixels outside projection
  bd = t(t(boundary(nv))*radius+center)
  par(xpd=TRUE)

  if (need.frame) {
    d = sqrt(diff(xlim)*diff(ylim))*radius*0.01
    xl = xlim*radius+center[1]
    yl = ylim*radius+center[2]
    rect = cbind(c(xl[1]-d,xl[2]+d,xl[2]+d,xl[1]-d),c(yl[1]-d,yl[1]-d,yl[2]+d,yl[2]+d))
    frame = raster::spPolygons(list(rect,bd))
    sp::plot(frame,col=background,border=NA,add=T)
  }

  # plot border
  if (show.border) lines(rbind(bd,bd[1,]),col=line.col,lwd=lwd)

  # return values
  return(list(col=col, clim=clim))

}
