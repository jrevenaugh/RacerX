require(png)
require(raster)
library(sf)
library(units)
library(smoothr)

# Read Grayscale PNG
img <- readPNG("Tracks/Gotland Ring North Circuit.png")
n <- length(img)

# Threshold grayscale
for (i in 1:n) {
  img[i] <- ifelse(img[i] < 0.1, 1, 0)
}

# Convert to raster
track_raster <- raster(nrow = dim(img)[1], ncol = dim(img)[2], xmn = 0)
track_raster[] <- img[,,3]

# Smooth raster and apply breaks
n_smooth <- 7 # must be odd!
r3 <- focal(track_raster, w = matrix(1 / n_smooth^2, nrow = n_smooth, ncol = n_smooth))
threshold <- 0.1 # Usually works.  May have to increase for "tight" tracks.
r4 <- cut(r3, breaks = c(-Inf, threshold, Inf)) - 1

# Decimate raster and smooth a second time
r5 <- aggregate(r4, 3, fun = max)
r6 <- focal(r5, w = matrix(1 / 9, nrow = 3, ncol = 3))
track_raster <- cut(r6, breaks = c(-Inf, 0.5, Inf)) - 1

#rm(r3, r4, r5, img)

# Convert raster to simple feature polygons
r_poly <- rasterToPolygons(track_raster, function(x){x == 1}, dissolve = TRUE) %>%
  st_as_sf()

# Smooth the polygons
smoothness <- 15 # Reduce for "tight" tracks.
r_poly_smooth <- smooth(r_poly, method = "ksmooth", smoothness = smoothness)

# Now extract the inner and outer polygons defining the track.  This step requires
# human assistance--different pngs have different numbers of polygons
t2 <- r_poly_smooth$geometry[[1]][[2]]
plot(t2, type = "l")
t1 <- r_poly_smooth$geometry[[1]][[1]]
lines(t1, col = "red")

rt_inner <- t2
rt_outer <- t1

x <- seq(-180,180,1)
y <- seq(-90,90,1)
xygrid <- expand.grid(x = x, y = y)

in_outer <- point.in.polygon(xygrid$x, xygrid$y, rt_outer[,1], rt_outer[,2], mode.checked=FALSE)
out_inner <- point.in.polygon(xygrid$x, xygrid$y, rt_inner[,1], rt_inner[,2], mode.checked=FALSE)

ll <- which(in_outer > 0 & out_inner != 1)

#plot(xygrid$x[ll],xygrid$y[ll],cex = 0.1)
t2 <- t2[seq(1, nrow(t2), 10),]
t2 <- rbind(t2, t2[1,])
t1 <- t1[seq(1, nrow(t1), 10),]
t1 <- rbind(t1, t1[1,])

#lines(t2)
#lines(t1, col = "red")

dots.df <- data.frame(x = xygrid$x[ll], y = xygrid$y[ll])
rt_inner <- as.data.frame(t2)
colnames(rt_inner) <- c("x", "y")
rt_outer <- as.data.frame(t1)
colnames(rt_outer) <- c("x", "y")

rm(t1, t2, ll, xygrid, in_outer, out_inner)

g <- ggplot(dots.df, aes(x, y)) +
     geom_point(size = 0.1) +
     geom_path(data = rt_inner) +
     geom_path(data = rt_outer) +
     theme_void() + theme(legend.position = "none") +
     coord_equal()
