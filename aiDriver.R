require(tidyverse)
source("global.R")

# Track is rt$track
aiDriver <- function(track, racecar, pcar) {

  # Set up some local variables
  jcur <- rep(0, 9)
  jdist <- rep(0, 9)
  i <- 1
  nTL <- nrow(track$centerline)

  AIcur <- rep(racecar$current, nAhead)
  AIprimary <- matrix(c(racecar$primary$x, racecar$primary$y), nrow = nAhead, ncol = 2, byrow = TRUE)
  AItried <- matrix(FALSE, nrow = nAhead, ncol = 9)
  AIcar <- matrix(c(racecar$x, racecar$y), nrow = nAhead, ncol = 2, byrow = TRUE)

  # Start loop over look-ahead steps
  while(i < nAhead) {
    aiToGrid <-  expand.grid(x = AIcar[i,1] + AIprimary[i,1] + gridxy,
                             y = AIcar[i,2] + AIprimary[i,2] + gridxy)
    aiToGrid$onCourse <- rep(FALSE, 9)

    # Determine which moveTo points are on course and not coincident with player car.
    for (j in 1:9) {
      if (aiToGrid$y[j] >= track$ymin &
          aiToGrid$y[j] <= track$ymax &
          aiToGrid$x[j] >= track$xmin &
          aiToGrid$x[j] <= track$xmax) {
        iR <- aiToGrid$y[j] - track$ymin + 1
        iC <- aiToGrid$x[j] - track$xmin + 1
        if (track$rstr[iR,iC] == 1) aiToGrid$onCourse[j] <- TRUE
      }
      if (i == 1 &
          (aiToGrid$x[j] == pcar$x) &
          (aiToGrid$y[j] == pcar$y)) aiToGrid$onCourse[j] <- FALSE
    }

    # Rule out crashes
    AItried[i,aiToGrid$onCourse == FALSE] <- TRUE

    # If no options remain, car must crash, return immediately, else go back a
    # look-ahead step and try the next option
    if (sum(AItried[i,]) >= maxTried) {
      i <- i - 1
      if (i == 0) return(list(crashed = TRUE, r = racecar))
      AItried[(i+1):nAhead,] <- FALSE
      next
    }

    # Have non-crashing options.  Choose most aggressive first, work down from there.
    jrange <- seq(AIcur[i], min(AIcur[i] + 300, nTL))
    dx <- track$centerline$x[jrange]
    dy <- track$centerline$y[jrange]
    jcur <- rep(0, 9)
    jdist <- rep(0, 9)
    speedCorrection <- (AIprimary[i,1]^2 + AIprimary[i,2]^2)^(1/3)
    for (j in 1:9) {
      if (!AItried[i,j]) {
        dist <- (dx - aiToGrid$x[j])^2 + (dy - aiToGrid$y[j])^2
        jcur[j] <- which.min(dist)
        jdist[j] <- dist[jcur[j]] * speedCorrection
      }
    }

    # Pick options by order of centerline sequence (larger equals better).
    ordering <- order(jcur - ocdPenaltyFactor * jdist, decreasing = TRUE)
    for (k in ordering) {
      if (AItried[i,k]) next
      AIprimary[i+1,1] <- aiToGrid$x[k] - AIcar[i,1]
      AIprimary[i+1,2] <- aiToGrid$y[k] - AIcar[i,2]

      # Limit top-end speed.  This is related to how far we look ahead
      if ((abs(AIprimary[i+1,1]) + abs(AIprimary[i+1,2])) > maxSpeed) {
        AItried[i,k] <- TRUE
        next
      }

      # Take the option and advance to next look-ahead
      AItried[i,k] <- TRUE
      i <- i + 1
      AIcur[i] <- jcur[k] + AIcur[i - 1] - 1
      AIcar[i,1] <- aiToGrid$x[k]
      AIcar[i,2] <- aiToGrid$y[k]
      break
    }
  }

  # Okay, we have gotten through the look ahead without crashing while attempting
  # to maximize along-track distance.  The first transitioned car position,
  # is the one we want.  We also need its primary vector.
  racecar$x <- AIcar[2,1]
  racecar$y <- AIcar[2,2]
  racecar$primary$x <- AIprimary[2,1]
  racecar$primary$y <- AIprimary[2,2]
  racecar$current <- AIcur[2]

  return(list(crashed = FALSE, r = racecar))
}


