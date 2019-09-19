#' Display merger tree
#'
#' @importFrom grDevices pdf dev.off rainbow
#' @importFrom graphics lines points plot rect
#' @importFrom plotrix draw.circle
#' @importFrom data.table as.data.table
#'
#' @description Visualise rooted directional trees, such as the merger trees of halos and galaxies.
#'
#' @param tree list of three vectors of equal lengths, specifying the merger tree. Each element refers to one vertex (halo) and the root vertex must be listed first. \code{mass} contains the masses of the vertices (halos). \code{descendant} contains the descendant indices; the first element must be 0, indicating that the root has no decendant. \code{value} (optional) contains values, normalized to [0,1], to be displayed in color.
#' @param normalize.mass logical flag specifying whether the masses should be normalized to the root mass
#' @param monotonic.growth optional string. If "down", the masses are forced to increase top-down; if "up", the masses are forced to decrease bottom-up.
#' @param sort optional string. If "left", the masses are ordered from left to right; if "right", the masses are sorted from right to left.
#' @param simplify logical flag. If TRUE, all the halos (other than the root) with only one progenitor are removed for graphical simplicity
#' @param draw.edges logical flag to turn on/off edges
#' @param draw.vertices logical flag to turn on/off vertices (halos)
#' @param draw.leaf.vertices logical flag to turn on/off the vertices of the leaves
#' @param draw.box logical flag to turn on/off a black frame around the box
#' @param root.length length of the root branch, extending down from the root halo
#' @param leaf.length length of the leaf branches, extending up from the leaves
#' @param style integer value to specify the rendering: 1=straight lines, 2=splines, 3=polygon splines with optimal vertex-matching
#' @param scale scaling factor to convert masses into line widths
#' @param gamma scaling exponent to convert masses into line widths
#' @param min minimum linewidth
#' @param vertex.size scaling factor of the vertex size
#' @param margin thickness of margin surrouding the plot
#' @param add logical flag specifying whether the plot should be added to an existing plot
#' @param xlim horizontal range of the tree with margin (only needed if add = FALSE)
#' @param ylim vertical range of the tree with margin (only needed if add = FALSE)
#' @param xrange horizontal range to be displayed
#' @param yrange vertical range to be displayed
#' @param col color of the tree; or a vector of colors if values have been specified in the tree
#' @param pdf.filename optional filename of a PDF file to be saved
#' @param pdf.size optional size of the PDF image
#'
#' @examples
#'
#' ## plot default tree
#' showtree()
#'
#' ## show a simple custom tree without vertices
#' tree = list(mass = c(1,0.6,0.1,0.5,0.4), descendant = c(0,1,2,2,1))
#' showtree(tree)
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{buildtree}}
#'
#' @export

showtree = function(
  tree = NULL,
  normalize.mass = TRUE,
  monotonic.growth = FALSE,
  sort = "",
  simplify = FALSE,
  draw.edges = TRUE,
  draw.vertices = FALSE,
  draw.leaf.vertices = draw.vertices,
  draw.box = TRUE,
  root.length = 0.05,
  leaf.length = 0,
  style = 3,
  scale = 1,
  gamma = 1,
  min = 0.03,
  vertex.size = 1.5,
  margin = 0.05,
  add = FALSE,
  xlim = c(0,1),
  ylim = c(0,1),
  xrange = xlim,
  yrange = ylim,
  col = rainbow(100),
  pdf.filename = NULL,
  pdf.size = 5) {

  # make default tree, if not given
  if (is.null(tree)) {
    tree = data.frame(
      mass = c(2,1/6,3/6,1/18,2/18,6/18,3/18,2/6),
      descendant = c(0,1,1,2,2,3,3,1),
      value = seq(8)/8
    )
  }

  # convert to data table
  tree = as.data.table(tree)

  # check size
  n = dim(tree)[1]
  if (n<2) stop('The tree must have at least two vertices.')

  # normalize masses
  if (normalize.mass) tree$mass = tree$mass/tree$mass[1]

  # determine the y-position of each halo and check descendant links
  if (tree$descendant[1]>0) stop('The descendant of the first vertex must be 0 (= root).')
  if (min(tree$descendant[2:n])<1) stop('Descendant index <1 not allowed, except for root.')
  if (max(tree$descendant[2:n])>n) stop('Descendant index >n not allowed.')
  tree$y = rep(1,n)
  for (i in seq(2,n)) {
    descendant = i
    count = 1
    while (descendant!=1 & count<=n) {
      descendant = tree$descendant[descendant]
      count = count+1
    }
    tree$y[i] = count
    if (count>n) stop('Linking error in tree.')
  }

  # simplify branches
  if (simplify) {
    showhalo = rep(TRUE,n)
    for (i in seq(n)) {
      progenitor.list = which(tree$descendant==i)
      n.progenitors = length(progenitor.list)
      if (n.progenitors==1) {
        showhalo[i] = FALSE
        tree[progenitor.list]$descendant = tree[i]$descendant
      }
    }
    tree = tree[showhalo,]
    n = dim(tree)[1]
    for (i in seq(2,n)) tree$descendant[i] = sum(showhalo[1:tree$descendant[i]])
  }

  # determine which haloes have a progenitor
  tree$hasprogenitor = rep(FALSE,n)
  tree$hasprogenitor[tree$descendant[tree$descendant>=1 & tree$descendant<=n]] = TRUE

  # determine the number of starting branches and number of levels
  nx = sum(!tree$hasprogenitor)
  ny = max(tree$y)

  # enforce monotonic mass growth
  if (monotonic.growth=='up') {
    for (y in seq(ny-1)) {
      levellist = which(tree$y==y)
      for (i in levellist) {
        progenitor.list = which(tree$descendant==i)
        progenitor.mass = sum(tree$mass[progenitor.list])
        mass.rescaling = min(1,tree$mass[i]/progenitor.mass)
        tree$mass[progenitor.list] = tree$mass[progenitor.list]*mass.rescaling
      }
    }
  } else if (monotonic.growth=='down') {
    for (y in seq(ny-1,1)) {
      levellist = which(tree$y==y)
      for (i in levellist) {
        progenitor.list = which(tree$descendant==i)
        progenitor.mass = sum(tree$mass[progenitor.list])
        mass.rescaling = max(1,progenitor.mass/tree$mass[i])
        tree$mass[i] = tree$mass[i]*mass.rescaling
      }
    }
  }

  # count number of branches of each vertex
  tree$nbranches = rep(0,n)
  for (i in seq(n)[!tree$hasprogenitor]) {
    descendant = i
    while (descendant!=1) {
      descendant = tree$descendant[descendant]
      tree$nbranches[descendant] = tree$nbranches[descendant]+1
    }
  }

  # prepare mass sorting
  if (sort=='left' | sort=='right') {
    decreasing = sort=='left'
  } else {
    if (sort!='') stop('Unknown value for "sort".')
  }

  # determine halo x-positions
  set.progenitors = function(i) {
    progenitor.list = which(tree$descendant==i)
    if (sort!='')  progenitor.list = progenitor.list[sort.int(tree$mass[progenitor.list],decreasing=decreasing,index.return=TRUE)$ix]
    for (j in progenitor.list) {
      tree$x0[j] <<- xtmp[i]
      tree$x1[j] <<- xtmp[i]+max(1,tree$nbranches[j])-1
      xtmp[i] <<- xtmp[i]+max(1,tree$nbranches[j])
      xtmp[j] <<- tree$x0[j]
      set.progenitors(j)
    }
  }
  tree$x0 = rep(NA,n)
  tree$x1 = rep(NA,n)
  tree$x0[1] = 1
  tree$x1[1] = nx
  xtmp = rep(NA,n)
  xtmp[1] = 1
  set.progenitors(1)
  tree$x = (tree$x0+tree$x1)/2

  # contract tree
  occupied = array(0,c(nx,ny))


  # make colors
  if (!is.null(tree$value)) {
    color = col[ceiling(lim(tree$value)*(length(col)-1)+0.5)]
  } else {
    color = rep('black',n)
  }

  # plot coordinates
  dx = (xlim[2]-xlim[1]-2*margin)/max(1,(nx-1))
  dy = (ylim[2]-ylim[1]-2*margin-root.length-leaf.length)/(ny-1)
  if (nx>1) {
    x = (tree$x-1)*dx+margin+xlim[1]
  } else {
    x = rep(mean(xlim),n)
  }
  y = (tree$y-1)*dy+margin+ylim[1]+root.length

  # make s-curves
  if (style>1) {

    tree$q = rep(0.5,n)

    scurve = function(q=0.5) {
      a = -8+16*q
      b = 14-32*q
      c = -5+16*q
      y = seq(0,1,length=100)
      x = a*y^4+b*y^3+c*y^2
      dxdy = 4*a*y^3+3*b*y^2+2*c*y
      return(list(x=x,y=y,dxdy=dxdy))
    }

    # sfct = function(y,q) {
    #   a = -8+16*q
    #   b = 14-32*q
    #   c = -5+16*q
    #   return(a*y^4+b*y^3+c*y^2)
    # }

    # crossover = function(i,j) {
    #
    #   ides = tree$descendant[i]
    #   jdes = tree$descendant[j]
    #   yi = seq(y[i],y[ides],length.out = 100)
    #   xi = sfct(seq(0,1,length.out=100),tree$q[i])*(x[ides]-x[i])+x[i]
    #   xj = x[j]+(yi-y[j])*(x[jdes]-x[j])/(y[jdes]-y[j])
    #   dxj = min(abs(xj[2:100]-xj[1:99]))
    #   sel = xj>min(x[j],x[jdes])-dxj & xj<max(x[j],x[jdes])+dxj
    #
    #   if (sum(sel)>0) {
    #     d = xi[sel]-xj[sel]
    #     return(min(d)<0 & max(d)>0)
    #   } else {
    #     return(FALSE)
    #   }
    #
    # }

    # if (simplify) {
    #   for (i in seq(2,n)) {
    #     for (j in seq(2,n)) {
    #       if (y[tree$descendant[i]]<y[tree$descendant[j]]) {
    #         if (crossover(i,j)) {
    #           while (crossover(i,j) & tree$q[i]>0.1) tree$q[i]=tree$q[i]-0.05
    #           tree$q[i]=tree$q[i]-0.05
    #         }
    #       }
    #     }
    #   }
    # }
  }

  if (is.null(pdf.filename)) {
    output.type = c(FALSE)
  } else {
    output.type = c(FALSE,TRUE)
  }

  for (make.pdf in output.type) {

    # open new plot
    asp = (yrange[2]-yrange[1])/(xrange[2]-xrange[1])
    if (make.pdf) pdf(pdf.filename,width=pdf.size/sqrt(asp),height=pdf.size*sqrt(asp))
    par(mar=rep(0.1,4),pty='m')
    if (!add | make.pdf) plot(0,0,type='n',xlim=xrange,ylim=yrange,asp=1,
                              xlab='',ylab='',xaxs='i',yaxs='i',xaxt='n',yaxt='n',bty='n')

    # draw edges
    if (draw.edges) {

      lwd = pmax(min,tree$mass^gamma*scale)

      if (style==3) {
        lwd = lwd*0.01
      } else {
        lwd = lwd*13
      }

      # leaf edges
      if (leaf.length>0) {
        if (style==3) {
          for (i in seq(n)[!tree$hasprogenitor]) {
            deltax = lwd[i]
            rect(x[i]-deltax,y[i],x[i]+deltax,y[i]+leaf.length,lwd=0.2,border=color[i],col=color[i])
          }
        } else {
          for (i in seq(n)[!tree$hasprogenitor]) {
            lines(c(x[i],x[i]),c(y[i],y[i]+leaf.length),lwd=lwd[i],col=color[i])
          }
        }
      }

      # connecting edges
      if (style==1) {
        for (i in seq(2,n)) {
          j = tree$descendant[i]
          lines(c(x[i],x[j]),c(y[i],y[j]),lwd=lwd[i],col=color[i])
        }
      } else if (style==2) {
        for (i in seq(2,n)) {
          s = scurve(tree$q[i])
          j = tree$descendant[i]
          lines(x[i]+s$x*(x[j]-x[i]),y[i]+s$y*(y[j]-y[i]),lwd=lwd[i],col=color[i])
        }
      } else if (style==3) {

        # make polygons
        xp = yp = array(NA,c(n,200))
        xc = yc = rc = rep(NA,n)
        for (j in seq(n)) {
          if (tree$hasprogenitor[j]) {
            progenitor.list = which(tree$descendant==j)
            progenitor.list = progenitor.list[sort.int(tree$x[progenitor.list],decreasing=FALSE,index.return=TRUE)$ix]
            n.progenitors = length(progenitor.list)
            ds1 = lwd[progenitor.list]  # line thicknesses of progenitors in image coordinates
            ds0 = lwd[j]
            ds2 = ds1/sum(ds1)*ds0
            if (n.progenitors==1) {
              offset = c(0)
            } else {
              offset = ds2-ds0+2*c(0,cumsum(ds2))[1:n.progenitors]
            }
            i.progenitor = 0
            for (i in progenitor.list) {
              s = scurve(tree$q[i])
              i.progenitor = i.progenitor+1
              slope.scale = (x[j]+offset[i.progenitor]-x[i])/(y[j]-y[i])
              sgn = sign(slope.scale)
              fl = s$dxdy*slope.scale
              ds = ds1[i.progenitor]#*(1-s$y)+ds2[i.progenitor]*s$y
              deltay = ds/sqrt(1+fl^(-2))
              deltax = ds/sqrt(1+fl^2)
              xl = x[i]+s$x*(x[j]+offset[i.progenitor]-x[i])
              yl = y[i]+s$y*(y[j]-y[i])
              xp[i,] = c(xl+deltax,rev(xl-deltax))
              yp[i,] = c(yl-sgn*deltay,rev(yl+sgn*deltay))
              xc[i] = xl[1]
              yc[i] = yl[1]
              rc[i] = lwd[i]
              #yp[i,] = pmin(y[i],pmax(y[j],c(yl-sgn*deltay,rev(yl+sgn*deltay))))
            }
          }
        }
        xc[1] = x[1]
        yc[1] = y[1]
        rc[1] = lwd[1]

        # plot all, ordered by increasing mass
        index = sort.int(tree$y,decreasing=TRUE,index.return=TRUE)$ix
        for (i in index) {
          # draw edges
          if (i>1) polygon(xp[i,],yp[i,],lwd=0.2,border=color[i],col=color[i])

          # draw round caps
          plotrix::draw.circle(xc[i],yc[i],rc[i],lwd=0.2,border=color[i],col=color[i])
        }

      } else {
        stop('style unknown.')
      }

      # root edge
      if (root.length>0) {
        if (style==3) {
          deltax = lwd[1]
          rect(x[1]-deltax,y[1]-root.length,x[1]+deltax,y[1],lwd=0.2,border=color[1],col=color[1])
        } else {
          lines(c(x[1],x[1]),c(y[1],y[1]-root.length),lwd=lwd[1],col=color[1])
        }
      }

    }

    # draw vertices for styles <3
    if (draw.vertices) {
      sel = rep(TRUE,n)
      if (!draw.leaf.vertices) sel[!tree$hasprogenitor]=FALSE
      cex = pmax(min,tree$mass^gamma*scale)*vertex.size
      if (style<3) {
        cex = cex*5
        points(x[sel],y[sel],cex=cex[sel],pch=20,col=color[sel])
      } else {
        cex = cex*0.01
        for (i in seq(n)[sel]) {
          plotrix::draw.circle(x[i],y[i],cex[i],lwd=0.2,border=color[i],col=color[i])
        }
      }
    }

    # draw box
    par(xpd=TRUE)
    if (draw.box) rect(xlim[1],ylim[1],xlim[2],ylim[2])
    par(xpd=FALSE)

    if (make.pdf) {dev.off()}

  }

}
