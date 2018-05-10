require(sf)
require(smoothr)
source("global.R")

# Set car on start line with no momentum (primary = null)
#trackName <- paste0("Tracks/", track_choices[1], ".RDS")
#track <- readRDS(trackName)
#dots.df <- track$dots
#rt_inner <- track$inner
#rt_outer <- track$outer

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
