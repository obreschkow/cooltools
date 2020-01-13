#' Display merger tree
#'
#' @importFrom grDevices pdf dev.off rainbow
#' @importFrom graphics lines points plot rect
#' @importFrom plotrix draw.circle
#' @importFrom data.table as.data.table
#'
#' @description Visualise rooted directional trees, such as the merger trees of halos and galaxies.
#'
#' @param tree list of vectors of equal lengths (\code{n}), specifying the \code{n} vertices (halos) a tree. These vectors are:
#'
#' \code{descendant} (mandatory) specifies a descendant index for each vertex. This vector must contain one zero, which specifies the root vertex. All other values must be positive integers.
#'
#' \code{mass} (optional) specifies contains the masses of the vertices. If not given, the mass will be chosen equal to the number of leaves in the subtree leading to this node.
#'
#' \code{value} (optional) contains values, normalized to [0,1], to be displayed in color.
#'
#' \code{length} (optional) number of vertical steps ("snapshots") to the descendant of each vertex. If not given, the default values is one.
#'
#' @param normalize.mass logical flag specifying whether the masses should be normalized to the root mass
#'
#' @param sort logical flag, specifying if the branches are ordered such that higher mass branches are more central
#' @param spacetype integer, specifying how much horizontal space is allocated to each progenitor branch: 1=all progenitors get equal space, 2=proportional to number of leaves, 3=proportional to mass
#' @param straightening real between 0 and 1 specifying the maximal mass-ratio up to which it is enforced that the  main progentior is strictly vertically above its descendant.
#' @param simplify logical flag. If TRUE, all the vertices, other than the root, with only one progenitor are removed for graphical simplicity.
#' @param style integer value to specify the rendering: 1=straight lines, 2=splines, 3=polygon splines with optimal vertex-matching, 4=same as (3) but with round connectors
#' @param root.length length of the root branch, extending down from the root halo
#' @param leaf.length length of the leaf branches, extending up from the leaves
#' @param col color of the edges; or a vector of colors if values have been specified in the tree
#' @param margin thickness of margin surrouding the plot
#' @param draw.edges logical flag to turn on/off edges
#' @param draw.vertices logical flag to turn on/off vertices
#' @param draw.box logical flag to turn on/off a black frame around the box
#' @param add logical flag specifying whether the plot should be added to an existing plot
#' @param xlim horizontal range of the tree with margin (expecially needed if add = FALSE)
#' @param ylim vertical range of the tree with margin (only needed if add = FALSE)
#' @param scale scaling factor to convert masses into line widths
#' @param gamma scaling exponent to convert masses into line widths
#' @param min minimum linewidth
#' @param vertex.size scaling factor of the vertex size
#' @param pdf.filename optional filename of a PDF file to be saved
#' @param pdf.size optional size of the PDF image
#'
#' @examples
#'
#' ## plot default tree
#' plottree()
#'
#' ## show a simple custom tree
#' tree = list(mass = c(1,0.6,0.1,0.5,0.4), descendant = c(0,1,2,2,1))
#' plottree(tree)
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{buildtree}}
#'
#' @export

plottree = function(
  tree = NULL,
  normalize.mass = TRUE,
  sort = TRUE,
  spacetype = 3,
  straightening = 0,
  simplify = FALSE,
  style = 3,
  root.length = 0.5,
  leaf.length = 0.5,
  margin = 0.03,
  col = rainbow(100,start=0,end=5/6),
  draw.edges = TRUE,
  draw.vertices = FALSE,
  draw.box = TRUE,
  add = FALSE,
  xlim = c(0,1),
  ylim = c(0,1),
  scale = 1,
  gamma = 1,
  min = 0.0,
  vertex.size = 1,
  pdf.filename = NULL,
  pdf.size = 5) {

  # make default tree, if not given
  if (is.null(tree)) {
    tree = list(
      mass = c(1,1/6,3/6,1/18,2/18,6/18,3/18,2/6,seq(6)/sum(seq(6))*6/18,2/6,2/6),
      descendant = c(0,1,1,2,2,3,3,1,6,6,6,6,6,6,8,15),
      value = seq(0,15)/15
    )
  }

  # check if descendant exists
  if (is.null(tree$descendant)) stop("No 'descendant' vector specified.")

  # check size
  n = length(tree$descendant)
  if (n<2) stop('The tree must have at least two vertices.')

  # check if tree has a root
  root = which(tree$descendant==0)
  if (length(root)!=1) stop('The tree must contain exactly one root vector, specified by a descendant index 0.')

  # check descendant ids and change them to array index
  if (min(tree$descendant)<0 | max(tree$descendant)>n) stop("The 'descendant' vector contains values outside the allowed range.")

  # count progenitors of each vertex
  tree$n.progenitors = rep(0,n)
  for (i in seq(n)[-root]) {
    j = tree$descendant[i]
    tree$n.progenitors[j] = tree$n.progenitors[j]+1
  }

  # make leaf indices
  leaves = seq(n)[tree$n.progenitors==0]
  if (length(leaves)<1) stop("No leaves found in tree.")

  # count leaves of each vertex
  tree$n.leaves = rep(0,n)
  for (leaf in leaves) {
    i = leaf
    while (i>0) {
      tree$n.leaves[i] = tree$n.leaves[i]+1
      i = tree$descendant[i]
    }
  }
  if (any(tree$n.leaves==0)) stop("Leave counting error.")

  # check masses
  if (is.null(tree$mass)) {
    tree$mass = tree$n.leaves
  } else {
    if (length(tree$mass)!=n) stop("The 'mass' vector must have the same length as the 'descendant' vector.")
    if (min(tree$mass)<=0) stop("All values of the 'mass' vector must be positive.")
  }

  # check values & assign colors
  if (is.null(tree$value)) {
    tree$col = rep('black',n)
  } else {
    if (length(tree$value)!=n) stop("The 'value' vector must have the same length as the 'descendant' vector.")
    if (min(tree$value)<0 | max(tree$value)>1) stop("All values of the 'value' vector must be between 0 and 1.")
    tree$col = col[floor(tree$value*0.99999*length(col))+1]
  }

  # make default vertical spacetypes between progenitors and descendants
  if (is.null(tree$length)) {
    tree$length = rep(1,n)
  } else {
    if (length(tree$length)!=n) stop("The 'length' vector must have the same length as the 'descendant' vector.")
    if (min(tree$mass)<=0) stop("All values of the 'length' vector must be positive.")
  }

  # convert to data table
  tree = data.table::as.data.table(tree)

  # normalize masses
  if (normalize.mass) tree$mass = tree$mass/tree$mass[root]

  # assign line widths
  tree$lwd = pmax(min,tree$mass^gamma*scale)*0.01

  # simplify tree
  if (simplify) {
    remove = tree$n.progenitors==1
    keep = !remove
    shift = cumsum(remove)
    for (i in seq(n)[-root]) {
      if (keep[i]) {
        j = tree$descendant[i]
        while (j!=root & remove[j]) {
          j = tree$descendant[j]
          tree$length[i] = tree$length[i]+1
        }
        tree$descendant[i] = j-shift[j]
      }
    }
    tree = tree[keep,]
    n = dim(tree)[1]
    root = which(tree$descendant==0)
    leaves = seq(n)[tree$n.progenitors==0]
  }

  # extend roots and leaves
  n.native = n
  if (root.length>0) {
    tree = rbind(tree,tree[root,])
    tree$n.progenitors[n+1] = 1
    tree$length[root] = root.length
    tree$descendant[root] = n+1
    root = n+1
    n = n+1
  }
  if (leaf.length>0) {
    tree = rbind(tree,tree[leaves,])
    new = n+seq(length(leaves))
    tree$n.progenitors[new] = 0
    tree$descendant[new] = leaves
    tree$n.progenitors[leaves] = 1
    tree$length[new] = leaf.length
    leaves = new
    n = n+length(new)
  }

  # determine (x,y) coordinates
  tree$x = tree$xmin = tree$xmax = tree$y = tree$direction = rep(NA,n)
  tree$direction[root] = 0
  tree$x[root] = 0
  tree$y[root] = 0
  tree$xmin[root] = -1
  tree$xmax[root] = 1
  tree = .make.progenitor.coordinates(tree,root,spacetype,straightening,sort)
  if (any(is.na(tree$x))) stop('Coordinate assingment error.')

  # renormalize coordinates to unit square
  tree$x = (tree$x-min(tree$x-tree$lwd))/(max(tree$x+tree$lwd)-min(tree$x-tree$lwd))
  tree$y = (tree$y-min(tree$y))/(max(tree$y)-min(tree$y))

  # add margin
  if (margin<0 | margin>0.3) stop('margin must be between 0 and 0.3')
  tree$x = tree$x*(1-2*margin)+margin
  tree$y = tree$y*(1-2*margin)+margin

  # rescale to xlim and ylim
  tree$x = tree$x*(xlim[2]-xlim[1])+xlim[1]
  tree$y = tree$y*(ylim[2]-ylim[1])+ylim[1]

  # plot tree
  if (style<3) tree$lwd = tree$lwd*1300
  asp = (ylim[2]-ylim[1])/(xlim[2]-xlim[1])

  if (is.null(pdf.filename)) {
    output.type = c(FALSE)
  } else {
    if (add) stop('It is not possible to add a plot to a pdf.')
    output.type = c(FALSE,TRUE)
  }

  for (make.pdf in output.type) {

    # open new plot
    if (make.pdf) pdf(pdf.filename,width=pdf.size/sqrt(asp),height=pdf.size*sqrt(asp))
    if (!add) nplot(xlim=xlim,ylim=ylim,mar=rep(0.1,4),asp=1)

    # draw edges
    if (draw.edges) {
      edges = sort.int(tree$y, decreasing = TRUE,index.return = TRUE)$ix
      edges = edges[edges!=root]
      s = .scurve()
      for (i in edges) {
        j = tree$descendant[i]
        direction = sign(tree$x[j]-tree$x[i])
        if (direction==0) direction=1
        if (style==1) {
          lines(tree$x[c(i,j)],tree$y[c(i,j)],lwd=tree$lwd[i],col=tree$col[i])
        } else if (style==2) {
          sx = s$x*(tree$x[i]-tree$x[j])+tree$x[j]
          sy = s$y*(tree$y[i]-tree$y[j])+tree$y[j]
          lines(sx,sy,lwd=tree$lwd[i],col=tree$col[i])
        } else if (style==3 | style==4) {
          progenitors = which(tree$descendant==j)
          np = length(progenitors)
          index = sort.int(tree$x[progenitors],index.return = TRUE)$ix
          index.0 = which(progenitors[index]==i)
          if (index.0>1) {
            index.left = seq(1,index.0-1)
          } else {
            index.left = NULL
          }
          f = tree$lwd[progenitors[index]]/sum(tree$lwd[progenitors])
          lwdj = tree$lwd[j]*f[index.0]
          xj = tree$x[j]-tree$lwd[j]+2*tree$lwd[j]*sum(f[index.left])+f[index.0]*tree$lwd[j]
          sx = s$x*(tree$x[i]-xj)+xj
          eps = (tree$y[i]-tree$y[j])*2e-3
          sy = s$y*(tree$y[i]+eps-tree$y[j])+tree$y[j]+eps
          sdxdy = s$dxdy*(tree$x[i]-xj)/(tree$y[i]-tree$y[j])
          f = sdxdy^2+1e-10
          ds = seq(lwdj,tree[i]$lwd,length.out=length(sx))
          dx = ds/sqrt(1+f)*direction
          dy = ds/sqrt(1+1/f)
          px = c(sx+dx,rev(sx-dx))
          py = c(sy+dy,rev(sy-dy))
          polygon(px,py,col=tree$col[i],border=NA)
          if (tree$n.progenitors[i]>1) {
            if (style==4) {
              plotrix::draw.circle(tree$x[i],tree$y[i],tree$lwd[i],border=NA,col=tree$col[i])
            }
          }
        }

      }
    }

    # draw vertices
    if (draw.vertices) {
      vertices = seq(n.native)
      cex = pmax(min,tree$mass^gamma*scale)*3*vertex.size
      points(tree$x[vertices],tree$y[vertices],pch=16,cex=cex[vertices])
    }

    # draw box
    par(xpd=TRUE)
    if (draw.box) rect(xlim[1],ylim[1],xlim[2],ylim[2])
    par(xpd=FALSE)

    # close pdf
    if (make.pdf) {dev.off()}

  }

  # return
  invisible(tree)

}

.make.progenitor.coordinates = function(tree,i,spacetype,straightening,sort) {

  # determine progenitors of i
  progenitors = which(tree$descendant==i)
  np = length(progenitors)

  # determine horizontal space fraction assigned to each progenitor
  if (spacetype==3) {
    f = tree$mass[progenitors]/sum(tree$mass[progenitors]) # evently space by mass
  } else if (spacetype==2) {
    f = tree$n.leaves[progenitors]/sum(tree$n.leaves[progenitors]) # evenly space by leaves
  } else if (spacetype==1) {
    f = rep(1/np,np)
  }

  # order progenitors if requested and divide into left and right or main progenitor
  if (sort) {
    index = sort.int(tree$mass[progenitors],index.return = T)$ix
    if (np>1) {
      index.1 = index[seq(1,np-1,by=2)]
    } else {
      index.1 = NULL
    }
    if (np>2) {
      index.2 = index[seq(2,np-1,by=2)]
    } else {
      index.2 = NULL
    }
    index.0 = index[np]
    if (sign(sum(f[index.2])-sum(f[index.1]))==tree$direction[i]) {
      index.left = index.2
      index.right = index.1
    } else {
      index.left = index.1
      index.right = index.2
    }
  } else {
    index = seq(np)
    index.0 = which.max(tree$mass[progenitors])[1]
    if (index.0>1) {
      index.left = seq(1,index.0-1)
    } else {
      index.left = NULL
    }
    if (index.0<np) {
      index.right = seq(np,index.0+1,by=-1)
    } else {
      index.right = NULL
    }
  }

  # extract total horizontal interval
  xmin = tree$xmin[i]
  xmax = tree$xmax[i]

  # enforce main branch to be exactly vertical
  m1 = max(tree$mass[progenitors])
  m2 = max(tree$mass[progenitors]%%m1)
  if (m2/m1<straightening) {
    space.left = sum(f[index.left])
    space.right = sum(f[index.right])
    s = f[index.0]+2*max(space.left,space.right)
    if (space.left>space.right) {
      xmax = xmin+(xmax-xmin)/s
    } else {
      xmin = xmax-(xmax-xmin)/s
    }
  }

  # assign x-intervals to all progenitors of i
  dx = xmax-xmin
  for (j in index.left) {
    p = progenitors[j]
    tree$xmin[p] = xmin
    xmin = xmin+dx*f[j]
    tree$xmax[p] = xmin
  }
  for (j in c(index.right,index.0)) {
    p = progenitors[j]
    tree$xmax[p] = xmax
    xmax = xmax-dx*f[j]
    tree$xmin[p] = xmax
  }

  # determine x-position and direction of branches
  for (p in progenitors) {
    tree$x[p] = (tree$xmin[p]+tree$xmax[p])/2
    tree$direction[p] = sign(tree$x[i]-tree$x[p])
  }

  # assign y-coordinates to all progenitors of i
  tree$y[progenitors] = tree$y[i]+tree$length[progenitors]

  # recursivel call progentiors
  for (p in progenitors) {
    if (tree$n.progenitors[p]>0) {
      tree = .make.progenitor.coordinates(tree,p,spacetype,straightening,sort)
    }
  }

  # return tree-structure
  invisible(tree)
}

.scurve = function(q=0.5,n=100,dy=1e-2) {
  a = -8+16*q
  b = 14-32*q
  c = -5+16*q
  y = seq(0,1,length=n)
  x = a*y^4+b*y^3+c*y^2
  dxdy = 4*a*y^3+3*b*y^2+2*c*y
  y[1] = y[1]-dy
  y[n] = y[n]+dy
  return(list(x=x,y=y,dxdy=dxdy))
}
