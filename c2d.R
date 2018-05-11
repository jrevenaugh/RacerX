# Clean up the start/finish line.

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
sf <- data.frame(x = rep(start_pts$x, 2), y = start_pts$y)

# Add some extra on to complete track
cl <- rbind(cl, data.frame(x=rep(-105, 3), y = c(-45, -43, -41)))

raceTrack <- list(dots = dots.df, inner = rt_inner, outer = rt_outer, start = sf, finish = start_finish, centerline = cl)

saveRDS(raceTrack, "track.RDS")
