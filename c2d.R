# Clean up the center line.  Want it as a sequence of points that correspond to
# dots on the track.  As it is, it's a smoothed curve.  Ditto the start/finish line.
#
n <- length(start_pts$x)
dx <- diff(start_pts$x)
dy <- diff(start_pts$y)
sf <- rep(FALSE, length(dots.df$x))
for (i in 1:(n-1)) {
  for (j in 0:20) {
    lsx <- round(start_pts$x[i] + dx[i] / 20 * j, 0)
    lsy <- round(start_pts$y[i] + dy[i] / 20 * j, 0)
    k <- which(dots.df$x == lsx & dots.df$y == lsy)
    if (length(k) != 0) {
      sf[k] <- TRUE
    }
  }
}
i <- which(sf)
start_finish <- data.frame(x = dots.df$x[i], y = dots.df$y[i])

center_mat <- centerline
centerline <- data.frame(x = center_mat[,1], y = center_mat[,2])
n <- length(centerline$x)
dx <- diff(centerline$x)
dy <- diff(centerline$y)
cl <- rep(FALSE, length(dots.df$x))
for (i in 1:(n-1)) {
  for (j in 0:20) {
    lsx <- round(centerline$x[i] + dx[i] / 20 * j, 0)
    lsy <- round(centerline$y[i] + dy[i] / 20 * j, 0)
    k <- which(dots.df$x == lsx & dots.df$y == lsy)
    if (length(k) != 0) {
      cl[k] <- TRUE
    }
  }
}
i <- which(cl)
center_line <- data.frame(x = dots.df$x[i], y = dots.df$y[i])

raceTrack <- list(dots = dots.df, inner = rt_inner, outer = rt_outer, finish = start_finish, centerline = center_line)

saveRDS(raceTrack, "track.RDS")
