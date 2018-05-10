require(tidyverse)
source("global.R")

plotTrack <- function(rt, racecar) {
  track <- rt$track
  xfoc <- c(racecar[1] - 30, racecar[1] + 30)
  yfoc <- c(racecar[2] - 20, racecar[2] + 20)

  g <- ggplot() +
    geom_point(data = track$dots, aes(x, y), size = 1, color = "gray50", pch = 3) +
    geom_path(data = track$inner, aes(x, y)) +
    geom_path(data = track$outer, aes(x, y)) +
    geom_path(data = track$finish, aes(x, y), color = "black", size = 3, linetype = "dashed") +
    coord_equal() + theme_void() +
    scale_x_continuous(limits = xfoc) +
    scale_y_continuous(limits = yfoc) +
    annotate("point", x = racecar[1], y = racecar[2], size = 7, color = "blue")

  return(g)
}



rt <- list(track = NA)
racecar <- list(x = NA, y = NA, primary = NA)

# Set car on start line with no momentum (primary = null)
trackName <- paste0("Tracks/", track_choices[1], ".RDS")
rt$track <- readRDS(trackName)
track <- rt$track
n <- length(track$finish$x)
n <- sample(1:length(track$finish$x), 1)
racecar$x <- track$finish$x[3]
racecar$y <- track$finish$y[3]
racecar$primary <- data.frame(x = 0, y = 0)
nAhead <- 10

# Loop over moves.  Continue until car crashes or finishes race
while (1) {
  AIcur <- rep(0, nAhead)
  AIprimary <- matrix(c(racecar$primary$x, racecar$primary$y), nrow = nAhead, ncol = 2, byrow = TRUE)
  AItried <- matrix(FALSE, nrow = nAhead, ncol = 9)
  AIcar <- matrix(c(racecar$x, racecar$y), nrow = nAhead, ncol = 2, byrow = TRUE)

  # Look ahead nAhead moves trying to get furthest on centerline index
  dist <- sqrt((track$centerline$x - AIcar[1,1])^2 +
               (track$centerline$y - AIcar[1,2])^2)
  AIcur[1] <- which.min(dist)
  i <- 1
  while(i < nAhead) {
    print(i)
    moveTo <- list(x = AIcar[i,1] + AIprimary[i,1] + gridxy,
                   y = AIcar[i,2] + AIprimary[i,2] + gridxy)
    xyg <-  expand.grid(x = moveTo$x, y = moveTo$y)
    moveToGrid <- data.frame(x = xyg$x, y = xyg$y, onCourse = rep(FALSE, 9))
    for (l in 1:9) {
      xind <- which(moveToGrid$x[l] == track$dots$x)
      yind <- which(moveToGrid$y[l] == track$dots$y)
      if (length(xind) >= 1 & length(yind) >= 1) {
        if (any(xind %in% yind)) moveToGrid$onCourse[l] <- TRUE
      }
    }
    AItried[i,moveToGrid$onCourse == FALSE] <- TRUE # Rule out crashes immediately
    if (sum(AItried[i,]) == 9) {
      i <- i - 1
      if (i == 0) stop("Car crashed")
      AItried[i,AItrying] <- TRUE
      next
    }
    print("Past crash check")
    # Have non-crashing options.  Choose most aggressive first
    jcur <- rep(0, 9)
    for (j in 1:9) {
      dist <- sqrt((track$centerline$x - moveToGrid$x[j])^2 +
                   (track$centerline$y - moveToGrid$y[j])^2)
      jcur[j] <- which.min(dist)
    }
    print(jcur)
    ordering <- order(jcur, decreasing = TRUE)
    for (k in ordering) {
      if (AItried[i,k]) next
      AIprimary[i,1] <- moveToGrid$x[k] - AIcar[i,1]
      AIprimary[i,2] <- moveToGrid$y[k] - AIcar[i,2]
      AItried[i,k] <- TRUE
      i <- i + 1
      AIcar[i,1] <- moveToGrid$x[k]
      AIcar[i,2] <- moveToGrid$y[k]
      break
    }
  }
  print("plotting")
  g <- plotTrack(rt, AIcar[1,])
}


