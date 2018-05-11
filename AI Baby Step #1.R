require(tidyverse)
source("global.R")


rt <- list(track = NA)
racecar <- list(x = NA, y = NA, current = 1, primary = NA)

# Set car on start line with no momentum (primary = null)
trackName <- paste0("Tracks/", track_choices[1], ".RDS")
rt$track <- readRDS("track.RDS")

track <- rt$track
n <- length(track$finish$x)
n <- sample(1:length(track$finish$x), 1)
racecar$x <- track$finish$x[1]
racecar$y <- track$finish$y[n]
racecar$primary <- data.frame(x = 0, y = 0)
nMoves <- 0
jcur <- rep(0, 9)
jdist <- rep(0, 9)

nTL <- nrow(track$centerline)

# Loop over moves.  Continue until car crashes or finishes race
while (nMoves < 200) {
  AIcur <- rep(racecar$current, nAhead)
  AIprimary <- matrix(c(racecar$primary$x, racecar$primary$y), nrow = nAhead, ncol = 2, byrow = TRUE)
  AItried <- matrix(FALSE, nrow = nAhead, ncol = 9)
  AIcar <- matrix(c(racecar$x, racecar$y), nrow = nAhead, ncol = 2, byrow = TRUE)
#  g <- plotTrack(rt, AIcar[2,])
#  print(g)
  i <- 1
  while(i < nAhead) {
    moveToGrid <-  expand.grid(x = AIcar[i,1] + AIprimary[i,1] + gridxy,
                               y = AIcar[i,2] + AIprimary[i,2] + gridxy)
    moveToGrid$onCourse <- rep(FALSE, 9)
    for (l in 1:9) {
      xind <- which(moveToGrid$x[l] == track$dots$x)
      if (length(xind) >= 1) {
        if (any(moveToGrid$y[l] == track$dots$y[xind])) moveToGrid$onCourse[l] <- TRUE
      }
    }
    AItried[i,moveToGrid$onCourse == FALSE] <- TRUE # Rule out crashes immediately
    if (sum(AItried[i,]) >= maxTried) {
      i <- i - 1
      if (i == 0) stop("Car crashed")
#      AItried[i,AItrying] <- TRUE
      AItried[(i+1):nAhead,] <- FALSE
      next
    }

    # Have non-crashing options.  Choose most aggressive first, work down from there.

    jrange <- seq(AIcur[i], min(AIcur[i] + 300, nTL))
    dx <- track$centerline$x[jrange]
    dy <- track$centerline$y[jrange]
    jcur <- rep(0, 9)
    for (j in 1:9) {
      if (!AItried[i,j]) {
        dist <- (dx - moveToGrid$x[j])^2 + (dy - moveToGrid$y[j])^2
        jcur[j] <- which.min(dist)
      }
    }
    ordering <- order(jcur, decreasing = TRUE)
    for (k in ordering) {
      if (AItried[i,k]) next
      AIprimary[i+1,1] <- moveToGrid$x[k] - AIcar[i,1]
      AIprimary[i+1,2] <- moveToGrid$y[k] - AIcar[i,2]
      if ((abs(AIprimary[i+1,1]) + abs(AIprimary[i+1,2])) > maxSpeed) {
        AItried[i,k] <- TRUE
        next
      }
      AItried[i,k] <- TRUE
      i <- i + 1
      AIcur[i] <- jcur[k] + AIcur[i - 1] - 1
      AIcar[i,1] <- moveToGrid$x[k]
      AIcar[i,2] <- moveToGrid$y[k]
      break
    }
  }
  # Okay, we have gotten through the look ahead without crashing and attempting
  # to maximize along-track position.  The first transitioned car position,
  # is the one we want.
  # We also need it's primary vector.


  racecar$x <- AIcar[2,1]
  racecar$y <- AIcar[2,2]
  racecar$primary$x <- AIprimary[2,1]
  racecar$primary$y <- AIprimary[2,2]
  racecar$current <- AIcur[2]
  nMoves <- nMoves + 1
  if (racecar$current >= nTL) {
    print("Finished")
    break
  }
  print(paste(nMoves, racecar$x, racecar$y, AIcur[2]))
}


