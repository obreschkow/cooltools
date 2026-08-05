#' Highest-density contour levels
#'
#' @importFrom stats approx integrate optim uniroot
#' @importFrom cubature cuhre
#' @importFrom MASS mvrnorm
#'
#' @description
#' Computes density thresholds defining highest-density regions.
#'
#' For each probability \code{p}, the function returns a level \eqn{\ell} such
#' that the region where the density is at least \eqn{\ell} contains a fraction
#' \code{p} of the total probability mass:
#' \deqn{
#'   \frac{\int_{f(\mathbf{x}) \geq \ell}
#'   f(\mathbf{x})\,d\mathbf{x}}
#'   {\int f(\mathbf{x})\,d\mathbf{x}}
#'   = p.
#' }
#'
#' The input may be a numeric vector or array containing sampled density
#' values, or a function representing a density over a bounded rectangular
#' domain. The density does not need to be normalised, but it must be
#' non-negative and have a finite, positive integral or sum.
#'
#' @param f A non-negative numeric vector or array, or a function of a numeric
#' vector. If \code{f} is a function, it must return a single finite,
#' non-negative density value.
#'
#' @param p Numeric vector of enclosed probability masses. Every value must lie
#' strictly between 0 and 1.
#'
#' @param xmin,xmax Numeric vectors giving the lower and upper limits of the
#' integration domain. These arguments are required when \code{f} is a
#' function and must have equal lengths. Outside this rectangular domain,
#' \code{f} is assumed to be zero.
#'
#' @param neval Positive integer giving the maximum number of function
#' evaluations used in each multidimensional numerical integration.
#'
#' @param subdivisions Positive integer giving the maximum number of
#' subintervals used by \code{\link[stats]{integrate}} in one dimension.
#'
#' @param napprox Number of trial density levels used to interpolate the
#' enclosed-mass function when \code{f} is a function. Larger values are
#' generally more accurate but require more numerical integrations. If
#' \code{napprox=0}, each requested contour level is determined directly by
#' root-finding.
#'
#' @param rel.tol Positive relative tolerance used for numerical integration.
#'
#' @param abs.tol Non-negative absolute tolerance used for numerical
#' integration.
#'
#' @param ... Additional arguments passed to \code{f}.
#'
#' @return
#' A numeric vector of density levels with the same length and ordering as
#' \code{p}.
#'
#' For a vector or array, all entries are assumed to represent cells of equal
#' volume or equal statistical weight. Because the enclosed mass changes in
#' discrete steps, the returned level is the sampled density threshold whose
#' superlevel set first contains at least the requested probability mass.
#'
#' @details
#' The returned contours define highest-density regions: points are included
#' in decreasing order of density until the requested probability mass is
#' enclosed. For multimodal densities, the resulting region may consist of
#' several disconnected components.
#'
#' When \code{f} is a function, the enclosed mass above a trial level is
#' evaluated by numerically integrating
#' \deqn{
#'   f(\mathbf{x}) I[f(\mathbf{x})\geq\ell].
#' }
#' This integrand is discontinuous at the contour boundary, so convergence may
#' be slower than for a smooth integrand, particularly in high dimensions or
#' for complicated contours.
#'
#' One-dimensional densities are integrated using
#' \code{\link[stats]{integrate}}. Multidimensional densities are integrated
#' using \code{\link[cubature]{cuhre}}.
#'
#' If \code{napprox>0}, the enclosed-mass function is evaluated on a grid of
#' density levels and inverted by monotonic linear interpolation. If
#' \code{napprox=0}, each requested probability is solved separately using
#' \code{\link[stats]{uniroot}}. The latter is generally slower but avoids the
#' interpolation approximation.
#'
#' @examples
#'
#' ## f(x) is a one-dimensional PDF
#' # Compute the one- and two-sigma contour levels of a normal distribution,
#' # i.e. the values l such that
#' # integral over dnorm(x) >= l of dnorm(x) dx = p,
#' # where p = 68.3% and 95.4%.
#' l = contourlevel(dnorm, xmin = -10, xmax = 10, napprox = 0)
#' print(l)
#'
#' # Compare these values with dnorm(1) and dnorm(2)
#' print(dnorm(c(1, 2)))
#'
#'
#' ## f(x) is a two-dimensional likelihood function
#' # Produce 20%, 40%, 60%, and 80% highest-density contours.
#' f = function(x) {
#'   cos(2*x[1]-x[2]-1)^2*exp(-x[1]^2-x[2]^2-x[1]*x[2])
#' }
#'
#' p = c(0.2, 0.4, 0.6, 0.8)
#'
#' # Values l such that
#' # integral over f(x) >= l of f(x) dx = p * integral f(x) dx
#' l = contourlevel(f, p, c(-5, -5), c(5, 5))
#'
#' # Plot the function and contours at the levels l
#' x = seq(-3, 3, length.out = 200)
#' m = pracma::meshgrid(x)
#' z = array(Vectorize(function(x, y) f(c(x, y)))(m$Y, m$X), dim(m$X))
#'
#' image(x, x, z, col = terrain.colors(100))
#' contour(x, x, z, levels = l, add = TRUE,
#'         labels = sprintf("%.0f%%", p*100), labcex = 0.7)
#'
#'
#' ## f is a 20-by-20 array representing a gridded point set
#' # Produce 1000 points drawn from a two-dimensional normal distribution.
#' set.seed(1)
#' x = MASS::mvrnorm(n = 1000, mu = c(0, 0), Sigma = matrix(c(3, 1, 1, 2), 2, 2))
#'
#' # Grid these points onto a regular 20-by-20 grid
#' g = griddata(x, min = -6, max = 6)
#'
#' # Find one- and two-sigma contour levels and draw the contours
#' l = contourlevel(g$field)
#'
#' plot(x, xlim = g$grid[[1]]$lim, ylim = g$grid[[2]]$lim, pch = 20, cex = 0.5)
#' contour(g$grid[[1]]$mid, g$grid[[2]]$mid, g$field,
#'         levels = l, add = TRUE, col = "red", lwd = c(2, 1), labels = NA)
#'
#' @seealso \code{\link{dpqr}}
#'
#' @author Danail Obreschkow
#'
#' @export
contourlevel = function(
    f,
    p = c(0.6826895, 0.9544997),
    xmin = NULL,
    xmax = NULL,
    neval = 1e4,
    subdivisions = 1000,
    napprox = 30,
    rel.tol = 1e-5,
    abs.tol = 0,
    ...
) {

  # Check probabilities
  if (!is.numeric(p) || length(p) == 0L) {
    stop("'p' must be a non-empty numeric vector.")
  }

  if (any(!is.finite(p))) {
    stop("All values of 'p' must be finite.")
  }

  if (any(p <= 0 | p >= 1)) {
    stop("All values of 'p' must lie strictly between 0 and 1.")
  }

  # Function input
  if (is.function(f)) {
    return(
      .contourlevel_function(
        f = f,
        p = p,
        xmin = xmin,
        xmax = xmax,
        neval = neval,
        subdivisions = subdivisions,
        napprox = napprox,
        rel.tol = rel.tol,
        abs.tol = abs.tol,
        ...
      )
    )
  }

  # Vector or array input
  if (is.numeric(f) && (is.vector(f) || is.array(f))) {
    return(.contourlevel_values(f, p))
  }

  stop("'f' must be a numeric vector, numeric array, or function.")
}


.contourlevel_values = function(f, p) {

  values = as.numeric(f)

  if (length(values) == 0L) {
    stop("'f' must contain at least one value.")
  }

  if (any(!is.finite(values))) {
    stop("'f' must contain only finite values.")
  }

  if (any(values < 0)) {
    stop("'f' must not contain negative values.")
  }

  total = sum(values)

  if (!is.finite(total) || total <= 0) {
    stop("The sum of 'f' must be finite and positive.")
  }

  values = sort(values, decreasing = TRUE)
  cumulative = cumsum(values) / total

  vapply(
    p,
    function(prob) {
      values[which(cumulative >= prob)[1L]]
    },
    numeric(1)
  )
}


.contourlevel_function = function(
    f,
    p,
    xmin,
    xmax,
    neval,
    subdivisions,
    napprox,
    rel.tol,
    abs.tol,
    ...
) {

  # Check domain
  if (is.null(xmin) || is.null(xmax)) {
    stop("'xmin' and 'xmax' must be supplied when 'f' is a function.")
  }

  if (!is.numeric(xmin) || !is.numeric(xmax)) {
    stop("'xmin' and 'xmax' must be numeric vectors.")
  }

  xmin = as.numeric(xmin)
  xmax = as.numeric(xmax)

  if (length(xmin) == 0L || length(xmin) != length(xmax)) {
    stop("'xmin' and 'xmax' must be non-empty vectors of equal length.")
  }

  if (any(!is.finite(xmin)) || any(!is.finite(xmax))) {
    stop("'xmin' and 'xmax' must contain only finite values.")
  }

  if (any(xmin >= xmax)) {
    stop(
      "Every element of 'xmin' must be smaller than the corresponding ",
      "element of 'xmax'."
    )
  }

  # Check density function
  test = f((xmin + xmax)/2, ...)
  if (!is.numeric(test) || length(test) != 1L || !is.finite(test)) {
    stop("'f' must return one finite numeric value.")
  }
  if (test < 0) {
    stop("'f' must return non-negative values.")
  }

  # Check numerical controls
  if (
    length(neval) != 1L ||
    !is.finite(neval) ||
    neval < 1 ||
    neval != floor(neval)
  ) {
    stop("'neval' must be a positive integer.")
  }

  if (
    length(subdivisions) != 1L ||
    !is.finite(subdivisions) ||
    subdivisions < 1 ||
    subdivisions != floor(subdivisions)
  ) {
    stop("'subdivisions' must be a positive integer.")
  }

  if (
    length(napprox) != 1L ||
    !is.finite(napprox) ||
    napprox < 0 ||
    napprox != floor(napprox)
  ) {
    stop("'napprox' must be a non-negative integer.")
  }

  if (napprox == 1L) {
    stop("'napprox' must be 0 or at least 2.")
  }

  if (
    length(rel.tol) != 1L ||
    !is.finite(rel.tol) ||
    rel.tol <= 0
  ) {
    stop("'rel.tol' must be a positive finite number.")
  }

  if (
    length(abs.tol) != 1L ||
    !is.finite(abs.tol) ||
    abs.tol < 0
  ) {
    stop("'abs.tol' must be a non-negative finite number.")
  }

  neval = as.integer(neval)
  subdivisions = as.integer(subdivisions)
  napprox = as.integer(napprox)

  ndim = length(xmin)

  # Checked scalar evaluation of the density
  density_scalar = function(x) f(x, ...)

  # Numerical integration
  integrate_density = function(integrand) {

    if (ndim == 1L) {

      result = stats::integrate(
        f = function(x) {
          vapply(x, function(xi) integrand(xi), numeric(1))
        },
        lower = xmin,
        upper = xmax,
        subdivisions = subdivisions,
        rel.tol = rel.tol,
        abs.tol = abs.tol,
        stop.on.error = FALSE
      )

      value = as.numeric(result$value)

      if (length(value) != 1L || !is.finite(value)) {
        stop("One-dimensional numerical integration failed.")
      }

      return(value)
    }

    result = cubature::cuhre(
      f = integrand,
      nComp = 1L,
      lowerLimit = xmin,
      upperLimit = xmax,
      maxEval = neval,
      relTol = rel.tol,
      absTol = abs.tol
    )

    value = as.numeric(result$integral)

    if (length(value) != 1L || !is.finite(value)) {
      stop("Multidimensional numerical integration failed.")
    }

    value
  }

  # Total normalisation
  total_mass = integrate_density(density_scalar)

  if (total_mass <= 0) {
    stop("The integral of 'f' over the specified domain must be positive.")
  }

  # Enclosed mass above a density level
  mass_above = function(level) {

    integrand = function(x) {

      value = density_scalar(x)

      if (value >= level) {
        value
      } else {
        0
      }
    }

    mass = integrate_density(integrand)

    # Protect against small numerical excursions
    min(total_mass, max(0, mass))
  }

  # Estimate the maximum density
  fit = stats::optim(
    par = (xmin + xmax)/2,
    fn = function(x) -density_scalar(x),
    method = "L-BFGS-B",
    lower = xmin,
    upper = xmax
  )

  if (!is.finite(fit$value)) {
    stop("Failed to estimate the maximum of the density.")
  }

  upper_level = -fit$value * (1 + sqrt(.Machine$double.eps))

  # Direct root-finding
  if (napprox == 0L) {

    return(
      vapply(
        p,
        function(prob) {

          target = prob * total_mass

          stats::uniroot(
            f = function(level) {
              mass_above(level) - target
            },
            interval = c(0, upper_level),
            tol = max(
              .Machine$double.eps^0.5,
              rel.tol * upper_level
            )
          )$root
        },
        numeric(1)
      )
    )
  }

  # Interpolated enclosed-mass curve
  #
  # The quadratic spacing provides greater resolution near zero density,
  # which is useful for large enclosed probabilities.
  u = seq(0, 1, length.out = napprox)
  levels = upper_level * u^2

  masses = vapply(
    levels,
    mass_above,
    numeric(1)
  ) / total_mass

  # Remove small violations of monotonicity caused by integration noise
  masses = cummin(masses)
  masses = pmin(1, pmax(0, masses))

  # Reverse so that probability increases
  masses = rev(masses)
  levels = rev(levels)

  # Remove repeated probability values
  keep = !duplicated(masses)
  masses = masses[keep]
  levels = levels[keep]

  if (length(masses) < 2L) {
    stop(
      paste(
        "The enclosed-mass curve could not be resolved.",
        "Increase 'neval' or set 'napprox=0'."
      )
    )
  }

  stats::approx(
    x = masses,
    y = levels,
    xout = p,
    method = "linear",
    ties = "ordered",
    rule = 2
  )$y
}
