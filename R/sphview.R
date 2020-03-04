library(cooltools)
library(png)
library(EBImage)

sphview = function(x, col = c('#ff0010', '#0515ff', 'green', 'orange', 'yellow', 'purple'),
                   center = NULL, radius = NULL, radius.scale = 1, screen = TRUE, pngfile = NULL, pdffile = NULL,
                   rotation = 1, kde = TRUE, ngrid = 300, lum = 1, shadows = 1, sigma = 1,
                   arrows = TRUE, side = NULL, xlab = NULL, ylab = NULL,
                   add = FALSE, add.xlim = NULL, add.ylim = NULL) {

  # handle x
  if (is.list(x)) {
    n = length(x)
    if (n>6) stop('The list x must not contain more than 5 arrays.')
  } else {
    x = list(x)
    n = 1
  }

  # concatenate list
  xc = x[[1]]
  if (n>1) {
    for (i in seq(2,n)) xc = rbind(xc,x[[i]])
  }
  x0 = apply(xc,2,mean) # geometric centre
  npoints = dim(xc)[1]

  # plot parameters
  d = 0.05  # distance to margin for arrows ant title
  larrow = 0.1   # length of arrows
  mar = c(0,0,0,0)
  col = t(col2rgb(col)/255)

  # rotation matrix
  if (length(rotation)==3) {
    rot = t(rotation3(rotation))
    if (is.null(xlab)) xlab = expression(e[1])
    if (is.null(ylab)) ylab = expression(e[2])
  } else {
    if (length(rotation)!=1) stop('rotation must be an integer 1,...,6 or a real 3-vector.')
    if (rotation>3) e = eigen(quadrupole(xc))$vectors
    if (rotation==1) {
      rot = diag(3)
      if (is.null(xlab)) xlab = 'x'
      if (is.null(ylab)) ylab = 'y'
    } else if (rotation==2) {
      rot = rbind(c(0,0,1),c(1,0,0),c(0,1,0))
      if (is.null(xlab)) xlab = 'y'
      if (is.null(ylab)) ylab = 'z'
    } else if (rotation==3) {
      rot = rbind(c(0,1,0),c(0,0,1),c(1,0,0))
      if (is.null(xlab)) xlab = 'z'
      if (is.null(ylab)) ylab = 'x'
    } else if (rotation==4) {
      rot = e[,c(1,3,2)]
      if (is.null(xlab)) xlab = expression(lambda ['max'])
      if (is.null(ylab)) ylab = expression(lambda ['min'])
    } else if (rotation==5) {
      rot = e[,c(1,2,3)]
      if (is.null(xlab)) xlab = expression(lambda ['max'])
      if (is.null(ylab)) ylab = expression(lambda ['mid'])
    } else if (rotation==6) {
      rot = e[,c(2,3,1)]
      if (is.null(xlab)) xlab = expression(lambda ['mid'])
      if (is.null(ylab)) ylab = expression(lambda ['min'])
    } else {
      stop('rotation must be an integer 1,...,6 or a real 3-vector.')
    }
  }

  # center points
  if (is.null(center)) {
    center = x0
  } else {
    if (length(center)==2) center=c(center,0)
  }
  for (i in seq(n)) {
    x[[i]] = t(t(x[[i]])-center)
  }

  # determine radius
  if (is.null(radius)) {
    radius = sqrt(max(apply(t(t(xc)-x0)^2,1,sum)))*radius.scale
  }

  # rotate and project coordinates
  for (i in seq(n)) {
    x[[i]] = (x[[i]]%*%rot)[,1:2]
  }

  # raster data
  smoothing = 0.01*sigma*ngrid/radius
  img = array(0,c(ngrid,ngrid,3))

  for (i in seq(n)) {

    if (smoothing==0) {
      g = griddata2(x[[i]][,1],x[[i]][,2],xlim=c(-1,1)*radius,ylim=c(-1,1)*radius,n=ngrid)
    } else {
      if (kde) {
        g = kde2(x[[i]][,1],x[[i]][,2],xlim=c(-1,1)*radius,ylim=c(-1,1)*radius,n=ngrid,s = 0.2*smoothing^0.5, sd.max=smoothing*2)
        g$n = g$d
      } else {
        g = griddata2(x[[i]][,1],x[[i]][,2],xlim=c(-1,1)*radius,ylim=c(-1,1)*radius,n=ngrid)
        g$n = gblur(g$n,smoothing)
      }
    }

    for (k in seq(3)) {
      img[,,k] = img[,,k]+col[i,k]*g$n
    }

  }

  # finalize image
  img = atan(img/npoints*ngrid^2/radius^2*lum)/pi*2
  f = 10^max(0,shadows)*2
  img = log10(f*img+1)/log10(f+1)
  img = lim(img) # just to be sure

  # save raster image as png
  if (!is.null(pngfile)) {
    png::writePNG(rasterflip(img),pngfile)
  }

  # determine margin
  mar = rep(0.25,4)
  for (i in seq(4)) {
    if (i%in%side) mar[i]=3
  }

  # show on screen and save as pdf
  for (mode in seq(2)) {

    make = FALSE

    if (mode==1 & screen) {
      make = TRUE
    }

    if (mode==2 & !is.null(pdffile) & !add) {
      make = TRUE
      pdf(plotfile,width=7,height=7)
    }

    if (make) {

      # initialize plot
      if (!add) nplot(xlim=c(0,1), ylim=c(0,1), pty='s', mar=mar)

      # plot raster
      if (add) {
        xleft = add.xlim[1]
        ybottom = add.ylim[1]
        dx = add.xlim[2]-add.xlim[1]
        dy = add.ylim[2]-add.ylim[1]
      } else {
        xleft = 0
        ybottom = 0
        dx = 1
        dy = 1
      }
      rasterImage(rasterflip(img),xleft,ybottom,xleft+dx,ybottom+dy)

      # arrows
      if (arrows) {
        if (!is.null(xlab)) {
          text(xleft+d+larrow,ybottom+d,xlab,pos=4,col='white')
          arrows(xleft+d,ybottom+d,xleft+d+larrow,ybottom+d,col='white',length = 0.1,angle=20)
        }
        if (!is.null(ylab)) {
          text(xleft+d,ybottom+d+larrow,ylab,pos=3,col='white')
          arrows(xleft+d,ybottom+d,xleft+d,ybottom+d+larrow,col='white',length = 0.1,angle=20)
        }
      }

      # side axes
      par(xpd=T)
      rect(xleft,ybottom,xleft+dx,ybottom+dy)
      par(xpd=F)
      usr = par()$usr
      par(usr=c(-1,1,-1,1)*radius)
      for (i in seq(4)) {
        if (i%in%side) axis(side=i)
      }
      par(usr=usr)

      if (mode==2) {dev.off()}

    }

  }

}

sphview4 = function(x, rotations=c(2,3,4,1), screen = TRUE, pdffile = NULL, ...) {

  for (mode in seq(2)) {

    make = FALSE
    if (mode==1 & screen) {
      make = TRUE
    }
    if (mode==2 & !is.null(pdffile)) {
      make = TRUE
      pdf(plotfile,width=7,height=7)
    }

    if (make) {

      nplot(xlim=c(0,2), ylim=c(0,2), pty='s', mar=c(0,0,0,0))

      ix = iy = 0

      for (i in seq(4)) {

        sphview(x, rotation = rotations[[i]], pngfile = NULL, pdffile = NULL, add = TRUE, add.xlim = c(0,1)+ix, add.ylim = c(0,1)+iy, screen = TRUE, ...)
        ix = (ix+1)%%2
        if (ix==0) iy = iy+1

      }

      # lines between panels
      lines(c(1,1),c(0,2),col='grey')
      lines(c(0,2),c(1,1),col='grey')

      if (mode==2) {dev.off()}

    }

  }

}

set.seed(1)
x.red = runif3(1e5,polarangle = c(0,pi/2), azimuth = c(0,pi*0.75))
x.blue = runif3(1e5,polarangle = c(pi/2,pi), azimuth = c(pi*1.25,2*pi))
x.green = t(t(runif3(2e4,r=0.5))+c(1,1/4,-1))
sphview4(list(x.red,x.blue,x.green), radius.scale=1.2)
