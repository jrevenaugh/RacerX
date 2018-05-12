# Determine if race is over (or not) and set finish times.

source("global.R")

raceOver <- function(aicar, racecar, track, nMoves) {
  finished <- FALSE
  nCL <- nrow(track$centerline)
  if (abs(aicar$x - track$finish$x[1]) <= 20) {
    if (abs(aicar$y - track$finish$y[1]) <= 20) {
      jrange <- seq(aicar$current, min(aicar$current + 300, nCL))
      dx <- track$centerline$x[jrange]
      dy <- track$centerline$y[jrange]
      dist <- (dx - aicar$x)^2 + (dy - aicar$y)^2
      iCur <- aicar$current + which.min(dist) - 1
      if ((abs(iCur - nCL) < 3) & (aicar$x <= track$finish$x[1])) {
        aicar$time <- nMoves - (track$finish$x[1] - aicar$x) / aicar$primary$x
        finished <- TRUE
      }
    }
  }
  if (abs(racecar$x - track$finish$x[1]) <= 20) {
    if (abs(racecar$y - track$finish$y[1]) <= 20) {
      jrange <- seq(racecar$current, min(racecar$current + 300, nCL))
      dx <- track$centerline$x[jrange]
      dy <- track$centerline$y[jrange]
      dist <- (dx - racecar$x)^2 + (dy - racecar$y)^2
      iCur <- racecar$current + which.min(dist) - 1
      if ((abs(iCur - nCL) < 3) & (racecar$x <= track$finish$x[1])) {
        racecar$time <- nMoves - (track$finish$x[1] - racecar$x) / racecar$primary$x
        finished <- TRUE
      }
    }
  }
  return(list(finished = finished, aicar = aicar, racecar = racecar, nMoves = nMoves))
}
