require(sf)
require(smoothr)
source("global.R")

track <- readRDS("Tracks/Autodromo Hermanos Rodriguez Oval.RDS")
dots.df <- track$dots
rt_inner <- track$inner
rt_outer <- track$outer

# Pick centerline
dev.new(noRStudioGD = TRUE)
plot(dots.df$x, dots.df$y, cex = 0.1, asp = 1)
lines(rt_inner)
lines(rt_outer)

# Pick start line crossing track at right angle.  Extend past edges of track.
start_pts <- locator(n = 2, type = "l")

lines(start_pts, col = "blue")

# Pick centerline starting at start line and finishing just short of start line
center_pts <- locator(n = 100, type = "l", col = "red")
n <- length(center_pts$x)
center_pts$x[n + 1] <- center_pts$x[1]
center_pts$y[n + 1] <- center_pts$y[1]
m <- matrix(c(center_pts$x, center_pts$y), ncol = 2)
ctr <- st_linestring(m)
ctr_d <- densify(ctr, n = 10)
centerline <- smooth(ctr_d, method = "ksmooth", smoothness = 10)

plot(dots.df$x, dots.df$y, cex = 0.1, asp = 1)
lines(rt_inner, type = "l")
lines(rt_outer, type = "l")
lines(start_pts, col = "blue")
lines(centerline, col = "red")

cl <- data.frame(x = centerline[,1], y = centerline[,2])
cl_add <- rbind(cl, data.frame(x = cl$x[2], y = cl$y[2]))

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

raceTrack <- list(dots = dots.df, inner = rt_inner, outer = rt_outer, start = sf, finish = start_finish, centerline = cl_add)

saveRDS(raceTrack, "track.RDS")
