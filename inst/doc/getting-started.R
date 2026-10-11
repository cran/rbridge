## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                     fig.height = 4.2, dpi = 120)
library(rbridge)

## ----penalty-regions, echo=FALSE, fig.width=10, fig.height=7.2, fig.cap="Unit bridge penalty regions, following the exponents in Figure 1 of Yuzbasi et al. (2021). All axes have equal scale; q = 5 and infinity are geometry only."----
penalty_boundary <- function(q) {
  if (is.infinite(q)) {
    return(matrix(c(1, 1, -1, 1, -1, -1, 1, -1, 1, 1),
                  ncol = 2, byrow = TRUE))
  }
  # Each share allocates the unit budget between the two absolute powers.
  share <- seq(0, 1, length.out = 1001)
  a <- share^(1 / q)
  b <- (1 - share)^(1 / q)
  rbind(cbind(a, b), cbind(rev(a), -rev(b)),
        cbind(-a, -b), cbind(-rev(a), rev(b)))
}

draw_penalty_regions <- function() {
  previous <- par(no.readonly = TRUE)
  on.exit({ layout(1); par(previous) })
  q_geometry <- c(0.01, 0.5, 1, 2, 5, Inf)
  colors <- c("#637787", "#148078", "#294E79", "#B97824", "#8C699C", "#637787")
  subtitles <- c("Narrow nonconvex region", "Nonconvex region", "L1 geometry",
                 "L2 geometry", "Geometry only", "Limit; geometry only")
  ink <- "#152B3D"
  layout(matrix(seq_along(q_geometry), nrow = 2, byrow = TRUE))
  par(oma = c(2.8, 0, 4.1, 0), mar = c(3.1, 3.2, 2.8, 1),
      bg = "white", fg = ink, col.axis = ink, col.lab = ink,
      family = "sans", las = 1, mgp = c(1.8, 0.65, 0),
      cex.axis = 0.75, cex.lab = 0.85)
  ticks <- seq(-1, 1, by = 0.5)
  for (i in seq_along(q_geometry)) {
    boundary <- penalty_boundary(q_geometry[i])
    plot(NA_real_, xlim = c(-1.12, 1.12), ylim = c(-1.12, 1.12),
         asp = 1, axes = FALSE, xlab = expression(b[1]), ylab = expression(b[2]))
    abline(h = ticks, v = ticks, col = "#E8EEF1", lwd = 0.7)
    polygon(boundary, col = grDevices::adjustcolor(colors[i], alpha.f = 0.12),
            border = colors[i], lwd = 2.2)
    axis(1, at = ticks, col = "#BCCBD3", col.ticks = "#BCCBD3")
    axis(2, at = ticks, col = "#BCCBD3", col.ticks = "#BCCBD3")
    title(if (is.infinite(q_geometry[i])) expression(q == infinity)
          else bquote(q == .(q_geometry[i])), line = 1.5,
          col.main = ink, cex.main = 1.05)
    mtext(subtitles[i], side = 3, line = 0.25, cex = 0.7, col = "#526B7D")
  }
  mtext("How q shapes the bridge penalty", side = 3, outer = TRUE,
        line = 2.3, adj = 0, font = 2, cex = 1.3, col = ink)
  mtext("Two coefficient coordinates; unit penalty budget", side = 3,
        outer = TRUE, line = 1, adj = 0, cex = 0.85, col = "#526B7D")
  mtext("Analytical geometry; q = 5 and infinity are outside the fitting range",
        side = 1, outer = TRUE, line = 1.2, adj = 0, cex = 0.78, col = "#526B7D")
}
draw_penalty_regions()

## ----penalty-regions-source, echo=TRUE, eval=FALSE, ref.label='penalty-regions'----
# penalty_boundary <- function(q) {
#   if (is.infinite(q)) {
#     return(matrix(c(1, 1, -1, 1, -1, -1, 1, -1, 1, 1),
#                   ncol = 2, byrow = TRUE))
#   }
#   # Each share allocates the unit budget between the two absolute powers.
#   share <- seq(0, 1, length.out = 1001)
#   a <- share^(1 / q)
#   b <- (1 - share)^(1 / q)
#   rbind(cbind(a, b), cbind(rev(a), -rev(b)),
#         cbind(-a, -b), cbind(-rev(a), rev(b)))
# }
# 
# draw_penalty_regions <- function() {
#   previous <- par(no.readonly = TRUE)
#   on.exit({ layout(1); par(previous) })
#   q_geometry <- c(0.01, 0.5, 1, 2, 5, Inf)
#   colors <- c("#637787", "#148078", "#294E79", "#B97824", "#8C699C", "#637787")
#   subtitles <- c("Narrow nonconvex region", "Nonconvex region", "L1 geometry",
#                  "L2 geometry", "Geometry only", "Limit; geometry only")
#   ink <- "#152B3D"
#   layout(matrix(seq_along(q_geometry), nrow = 2, byrow = TRUE))
#   par(oma = c(2.8, 0, 4.1, 0), mar = c(3.1, 3.2, 2.8, 1),
#       bg = "white", fg = ink, col.axis = ink, col.lab = ink,
#       family = "sans", las = 1, mgp = c(1.8, 0.65, 0),
#       cex.axis = 0.75, cex.lab = 0.85)
#   ticks <- seq(-1, 1, by = 0.5)
#   for (i in seq_along(q_geometry)) {
#     boundary <- penalty_boundary(q_geometry[i])
#     plot(NA_real_, xlim = c(-1.12, 1.12), ylim = c(-1.12, 1.12),
#          asp = 1, axes = FALSE, xlab = expression(b[1]), ylab = expression(b[2]))
#     abline(h = ticks, v = ticks, col = "#E8EEF1", lwd = 0.7)
#     polygon(boundary, col = grDevices::adjustcolor(colors[i], alpha.f = 0.12),
#             border = colors[i], lwd = 2.2)
#     axis(1, at = ticks, col = "#BCCBD3", col.ticks = "#BCCBD3")
#     axis(2, at = ticks, col = "#BCCBD3", col.ticks = "#BCCBD3")
#     title(if (is.infinite(q_geometry[i])) expression(q == infinity)
#           else bquote(q == .(q_geometry[i])), line = 1.5,
#           col.main = ink, cex.main = 1.05)
#     mtext(subtitles[i], side = 3, line = 0.25, cex = 0.7, col = "#526B7D")
#   }
#   mtext("How q shapes the bridge penalty", side = 3, outer = TRUE,
#         line = 2.3, adj = 0, font = 2, cex = 1.3, col = ink)
#   mtext("Two coefficient coordinates; unit penalty budget", side = 3,
#         outer = TRUE, line = 1, adj = 0, cex = 0.85, col = "#526B7D")
#   mtext("Analytical geometry; q = 5 and infinity are outside the fitting range",
#         side = 1, outer = TRUE, line = 1.2, adj = 0, cex = 0.78, col = "#526B7D")
# }
# draw_penalty_regions()

## ----data---------------------------------------------------------------------
set.seed(2019)
n <- 80
p <- 30
Sigma <- outer(seq_len(p), seq_len(p), function(i, j) 0.5^abs(i - j))
X <- matrix(rnorm(n * p), n, p) %*% chol(Sigma)
colnames(X) <- paste0("x", seq_len(p))
beta <- c(1, 0.5, -0.2, rep(0, p - 3))
sigma <- 0.75
y <- as.numeric(1 + X %*% beta + sigma * rnorm(n))
lambda <- c(5, 2, 1)

## ----path---------------------------------------------------------------------
path <- bridge(X, y, q = 2, lambda = lambda)
knitr::kable(coef(path)[1:6, , drop = FALSE], digits = 3,
             caption = "Demonstration path: intercept and first five slopes")
predict(path, newx = X[1:3, , drop = FALSE], s = 2)

## ----restricted---------------------------------------------------------------
R <- matrix(0, nrow = 1, ncol = p)
R[1, 4] <- 1
r <- matrix(0, nrow = 1)
restricted <- rbridge(X, y, q = 2, R = R, r = r, lambda = lambda)
scale <- sqrt(colMeans(sweep(X, 2, colMeans(X), "-")^2))
standardized_slopes <- restricted$betas[-1, , drop = FALSE] * scale
max(abs(sweep(R %*% standardized_slopes, 1, as.numeric(r), "-")))

## ----cv-----------------------------------------------------------------------
lambda_cv <- exp(seq(log(100), log(0.001), length.out = 100))
q_values <- c(0.5, 1, 2)
cv_fits <- lapply(q_values, function(q) {
  cv.rbridge(X, y, q = q, R = R, r = r, lambda = lambda_cv,
             nfolds = 4, num_threads = 1)
})
names(cv_fits) <- paste("q =", q_values)
cv_summary <- data.frame(
  q = q_values,
  lambda_min = vapply(cv_fits, function(fit) fit$lambda.min, numeric(1)),
  minimum_cv_error = vapply(cv_fits, function(fit) {
    fit$cve[match(fit$lambda.min, fit$lambda)]
  }, numeric(1))
)
knitr::kable(cv_summary, digits = c(1, 4, 4), row.names = FALSE,
             col.names = c("q", "Selected lambda", "Minimum CV error"),
             caption = "Fitted-grid CV minima on the same data and folds")
cv <- cv_fits[["q = 2"]]
coef(cv, s = "lambda.min")[1:6, , drop = FALSE]
predict(cv, newx = X[1:3, , drop = FALSE])

## ----cv-plot, echo=FALSE, fig.width=10, fig.height=5.2, fig.cap="Synthetic demonstration: CV curves and fitted-grid minima for q = 0.5, 1 and 2; the right panel uses a closer error scale."----
draw_cv_comparison <- function(fits, summary) {
  previous <- par(no.readonly = TRUE)
  on.exit({ layout(1); par(previous) })
  colors <- c("#148078", "#294E79", "#B97824")
  shapes <- c(16, 17, 15)
  styles <- c(1, 2, 3)
  ink <- "#152B3D"
  grid <- "#E8EEF1"
  layout(matrix(1:2, nrow = 1), widths = c(1.8, 1))
  par(oma = c(1.8, 0, 3.8, 0), mar = c(4.2, 4.5, 2.4, 1.1),
      bg = "white", fg = ink, col.axis = ink, col.lab = ink,
      family = "sans", las = 1, cex.axis = 0.85, cex.lab = 0.95)
  errors <- unlist(lapply(fits, function(fit) fit$cve))
  limits <- range(errors)
  limits <- limits + c(-0.04, 0.09) * diff(limits)
  plot(range(log10(fits[[1]]$lambda)), limits, type = "n", axes = FALSE,
       xlab = "Lambda (log scale)", ylab = "Historical CV error")
  ticks <- -3:2
  abline(h = pretty(limits), v = ticks, col = grid, lwd = 0.8)
  axis(1, at = ticks, labels = c("0.001", "0.01", "0.1", "1", "10", "100"),
       col = "#BCCBD3", col.ticks = "#BCCBD3")
  axis(2, col = "#BCCBD3", col.ticks = "#BCCBD3")
  for (i in seq_along(fits)) {
    fit <- fits[[i]]
    lines(log10(fit$lambda), fit$cve, col = colors[i], lwd = 2.3,
          lty = styles[i])
    selected <- match(fit$lambda.min, fit$lambda)
    points(log10(fit$lambda[selected]), fit$cve[selected], pch = shapes[i],
           col = colors[i], cex = 1.25)
  }
  legend("topleft", legend = names(fits), col = colors, pch = shapes,
         lty = styles, lwd = 2.3, ncol = 3, bty = "n", cex = 0.83)
  title("CV curves", adj = 0, col.main = ink, cex.main = 1.1)

  par(mar = c(4.2, 4.8, 2.4, 1.1))
  minimum_limits <- range(summary$minimum_cv_error)
  padding <- max(diff(minimum_limits), 0.002) * 0.4
  minimum_limits <- minimum_limits + c(-padding, padding)
  plot(summary$q, summary$minimum_cv_error, type = "n", axes = FALSE,
       xlim = c(0.25, 2.25), ylim = minimum_limits,
       xlab = "Penalty exponent q", ylab = "Minimum CV error")
  abline(h = pretty(minimum_limits, n = 4), v = summary$q, col = grid, lwd = 0.8)
  axis(1, at = summary$q, col = "#BCCBD3", col.ticks = "#BCCBD3")
  axis(2, at = pretty(minimum_limits, n = 4), col = "#BCCBD3",
       col.ticks = "#BCCBD3")
  points(summary$q, summary$minimum_cv_error, pch = shapes, col = colors,
         cex = 1.45)
  text(summary$q, summary$minimum_cv_error,
       labels = paste0("lambda = ", formatC(summary$lambda_min, digits = 3,
                                            format = "g")),
       pos = c(3, 1, 3), offset = 0.8, cex = 0.68, col = colors)
  title("Minimum by q", adj = 0, col.main = ink, cex.main = 1.1)
  mtext("Choosing the bridge penalty", side = 3, outer = TRUE,
        line = 2, adj = 0, font = 2, cex = 1.3, col = ink)
  mtext("80 observations, 30 predictors; four folds and 100 penalties for each q", side = 3,
        outer = TRUE, line = 0.8, adj = 0, cex = 0.85, col = "#526B7D")
  mtext("Synthetic example selected to illustrate CV shape; fitted-grid minima", side = 1,
        outer = TRUE, line = 0.7, adj = 0, cex = 0.78, col = "#526B7D")
}
draw_cv_comparison(cv_fits, cv_summary)

## ----cv-plot-source, echo=TRUE, eval=FALSE, ref.label='cv-plot'---------------
# draw_cv_comparison <- function(fits, summary) {
#   previous <- par(no.readonly = TRUE)
#   on.exit({ layout(1); par(previous) })
#   colors <- c("#148078", "#294E79", "#B97824")
#   shapes <- c(16, 17, 15)
#   styles <- c(1, 2, 3)
#   ink <- "#152B3D"
#   grid <- "#E8EEF1"
#   layout(matrix(1:2, nrow = 1), widths = c(1.8, 1))
#   par(oma = c(1.8, 0, 3.8, 0), mar = c(4.2, 4.5, 2.4, 1.1),
#       bg = "white", fg = ink, col.axis = ink, col.lab = ink,
#       family = "sans", las = 1, cex.axis = 0.85, cex.lab = 0.95)
#   errors <- unlist(lapply(fits, function(fit) fit$cve))
#   limits <- range(errors)
#   limits <- limits + c(-0.04, 0.09) * diff(limits)
#   plot(range(log10(fits[[1]]$lambda)), limits, type = "n", axes = FALSE,
#        xlab = "Lambda (log scale)", ylab = "Historical CV error")
#   ticks <- -3:2
#   abline(h = pretty(limits), v = ticks, col = grid, lwd = 0.8)
#   axis(1, at = ticks, labels = c("0.001", "0.01", "0.1", "1", "10", "100"),
#        col = "#BCCBD3", col.ticks = "#BCCBD3")
#   axis(2, col = "#BCCBD3", col.ticks = "#BCCBD3")
#   for (i in seq_along(fits)) {
#     fit <- fits[[i]]
#     lines(log10(fit$lambda), fit$cve, col = colors[i], lwd = 2.3,
#           lty = styles[i])
#     selected <- match(fit$lambda.min, fit$lambda)
#     points(log10(fit$lambda[selected]), fit$cve[selected], pch = shapes[i],
#            col = colors[i], cex = 1.25)
#   }
#   legend("topleft", legend = names(fits), col = colors, pch = shapes,
#          lty = styles, lwd = 2.3, ncol = 3, bty = "n", cex = 0.83)
#   title("CV curves", adj = 0, col.main = ink, cex.main = 1.1)
# 
#   par(mar = c(4.2, 4.8, 2.4, 1.1))
#   minimum_limits <- range(summary$minimum_cv_error)
#   padding <- max(diff(minimum_limits), 0.002) * 0.4
#   minimum_limits <- minimum_limits + c(-padding, padding)
#   plot(summary$q, summary$minimum_cv_error, type = "n", axes = FALSE,
#        xlim = c(0.25, 2.25), ylim = minimum_limits,
#        xlab = "Penalty exponent q", ylab = "Minimum CV error")
#   abline(h = pretty(minimum_limits, n = 4), v = summary$q, col = grid, lwd = 0.8)
#   axis(1, at = summary$q, col = "#BCCBD3", col.ticks = "#BCCBD3")
#   axis(2, at = pretty(minimum_limits, n = 4), col = "#BCCBD3",
#        col.ticks = "#BCCBD3")
#   points(summary$q, summary$minimum_cv_error, pch = shapes, col = colors,
#          cex = 1.45)
#   text(summary$q, summary$minimum_cv_error,
#        labels = paste0("lambda = ", formatC(summary$lambda_min, digits = 3,
#                                             format = "g")),
#        pos = c(3, 1, 3), offset = 0.8, cex = 0.68, col = colors)
#   title("Minimum by q", adj = 0, col.main = ink, cex.main = 1.1)
#   mtext("Choosing the bridge penalty", side = 3, outer = TRUE,
#         line = 2, adj = 0, font = 2, cex = 1.3, col = ink)
#   mtext("80 observations, 30 predictors; four folds and 100 penalties for each q", side = 3,
#         outer = TRUE, line = 0.8, adj = 0, cex = 0.85, col = "#526B7D")
#   mtext("Synthetic example selected to illustrate CV shape; fitted-grid minima", side = 1,
#         outer = TRUE, line = 0.7, adj = 0, cex = 0.78, col = "#526B7D")
# }
# draw_cv_comparison(cv_fits, cv_summary)

## ----capabilities-------------------------------------------------------------
rbridge_info()

