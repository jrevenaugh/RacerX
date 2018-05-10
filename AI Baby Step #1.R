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
racecar <- list(x = NA, y = NA, current = 1, primary = NA)

# Set car on start line with no momentum (primary = null)
trackName <- paste0("Tracks/", track_choices[1], ".RDS")
rt$track <- readRDS("track.RDS")

track <- rt$track
#track$centerline <- track$centerline %>% slice(1:3000)
track$centerline <- rbind(track$centerline, data.frame(x = rep(140), y = c(-58:(-53))))
n <- length(track$finish$x)
n <- sample(1:length(track$finish$x), 1)
racecar$x <- track$finish$x[3]
racecar$y <- track$finish$y[3]
racecar$primary <- data.frame(x = 0, y = 0)
nAhead <- 8
nMoves <- 1

# Loop over moves.  Continue until car crashes or finishes race
while (nMoves) {
  AIcur <- rep(racecar$current, nAhead)
  AIprimary <- matrix(c(racecar$primary$x, racecar$primary$y), nrow = nAhead, ncol = 2, byrow = TRUE)
  AItried <- matrix(FALSE, nrow = nAhead, ncol = 9)
  AIcar <- matrix(c(racecar$x, racecar$y), nrow = nAhead, ncol = 2, byrow = TRUE)
  g <- plotTrack(rt, AIcar[1,])
#  print(g)

  i <- 1
  while(i < nAhead) {
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
      AItried[(i+1):nAhead,] <- FALSE
      next
    }
    # Have non-crashing options.  Choose most aggressive first, work down from there.
    jcur <- rep(0, 9)
    jdist <- rep(0, 9)
    dx <- track$centerline$x[AIcur[i]:(AIcur[i] + 500)]
    dy <- track$centerline$y[AIcur[i]:(AIcur[i] + 500)]

    jdist <- rep(0, 9)
    for (j in 1:9) {
      dist <- sqrt((dx - moveToGrid$x[j])^2 +
                   (dy - moveToGrid$y[j])^2)
      jcur[j] <- which.min(dist)
      jdist[j] <- dist[jcur[j]]
    }
    ordering <- order((jcur - jdist), decreasing = TRUE)
    for (k in ordering) {
      if (AItried[i,k]) next
      AIprimary[i+1,1] <- moveToGrid$x[k] - AIcar[i,1]
      AIprimary[i+1,2] <- moveToGrid$y[k] - AIcar[i,2]
      if (sum(abs(AIprimary[i+1,1]) + abs(AIprimary[i+1,2])) > maxSpeed) {
        AItried[i,k] <- TRUE
        next
      }
      AItried[i,k] <- TRUE
      i <- i + 1
      AIcur[i] <- jcur[k] + AIcur[i-1] - 1
      AIcar[i,1] <- moveToGrid$x[k]
      AIcar[i,2] <- moveToGrid$y[k]
      break
    }
  }
  # Okay, we have gotten through the look ahead without crashing and attempting
  # to maximize along-track position.  The first car position, is the one we want.
  # we also need it's primary.


  racecar$x <- AIcar[2,1]
  racecar$y <- AIcar[2,2]
  racecar$primary$x <- AIprimary[2,1]
  racecar$primary$y <- AIprimary[2,2]
  racecar$current <- AIcur[2]
  nMoves <- nMoves + 1
  if (racecar$current >= length(track$centerline$x) - 3) {
    print("Finished")
    break
  }
  print(nMoves)
}


