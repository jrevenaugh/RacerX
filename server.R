# server
#
# RacerX shiny server

source("aiDriver.R")
source("raceOver.R")

server <- function(input, output, session) {

  # Reactives ------------------------------------------------------------------
  racecar <- reactiveValues(x = NA,
                            y = NA,
                            primary = NA,
                            current = NA,
                            offCourse = 0,
                            time = 1e6)

  aicar <- reactiveValues(x = NA,
                          y = NA,
                          primary = NA,
                          current = NA,
                          time = 1e6)

  prior <- reactiveValues(x = rep(0, nBack),
                          y = rep(0, nBack),
                          primary = matrix(0, nrow = nBack, ncol = 2),
                          current = rep(0, nBack),
                          offCourse = rep(0, nBack),
                          nMoves = 1)

  rt <- reactiveValues(track = NA, nCL = NA)

  moveToGrid <- reactiveValues(x = NA,
                               y = NA,
                               onCourse = NA)

  finished <- reactiveValues(done = FALSE)

  # Event Observers ------------------------------------------------------------
  observeEvent(c(input$reset, input$track), {
    # Load new track
    trackName <- paste0("Tracks/", input$track, ".RDS")
    rt$track <- readRDS(trackName)
    rt$nCL <- nrow(rt$track$centerline)
    finished$done <- FALSE

    # Straighten the finish line (hanging chad from course creation)
    rt$track$finish$x <- rt$track$finish$x[1]
    rt$track$start$x <- rt$track$start$x[1]

    # Create logical raster of track (FALSE = off course).
    xrange <- range(rt$track$dots$x)
    yrange <- range(rt$track$dots$y)
    nr <- diff(yrange) + 1
    rt$track$rstr <- matrix(FALSE, nrow = nr, ncol = diff(xrange) + 1)
    rt$track$xmin <- xrange[1]
    rt$track$xmax <- xrange[2]
    rt$track$ymin <- yrange[1]
    rt$track$ymax <- yrange[2]
    j <- nr * (rt$track$dots$x - rt$track$xmin) + rt$track$dots$y - rt$track$ymin + 1
    rt$track$rstr[j] <- TRUE

    # Get starting positions
    n <- length(rt$track$finish$x)
    ps <- round(mean(rt$track$finish$y) - 1, 0)
    racecar$x <- rt$track$finish$x[1]
    racecar$y <- ps
    racecar$primary <- data.frame(x = 0, y = 0)
    racecar$time <- 1e6
    racecar$current <- 0
    racecar$offCourse <- 0
    aicar$x <- rt$track$finish$x[1]
    aicar$y <- ps + 1
    aicar$primary <- data.frame(x = 0, y = 0)
    aicar$time <- 1e6
    aicar$current <- 0

    # Assure that centerline start is conducive to a proper start for AI car.
    mx <- mean(rt$track$finish$x)
    i <- 1
    while (rt$track$centerline$x[i] > mx - 1) i <- i + 1
    rt$track$centerline <- rbind(data.frame(x = mx, y = ps + 1),
                                 data.frame(x = mx - 1, y = ps + 1),
                                 rt$track$centerline[-i,])

    # Set up undo structure
    prior$x <- rep(racecar$x, nBack)
    prior$y <- rep(racecar$y, nBack)
    prior$primary <- matrix(0, nrow = nBack, ncol = 2)
    prior$offCourse <- rep(0, nBack)
    prior$nMoves <- 1
  })


  # Undo last move (recursive).  Note that this only undoes the player's car.
  # AI is unaffected.  This allows you to correct a fatal error, but at a price.
  observeEvent(input$undo, {
    if (prior$nMoves <= 1) return()
    if (finished$done) return()
    prior$nMoves <- prior$nMoves - 1
    racecar$x <- prior$x[prior$nMoves]
    racecar$y <- prior$y[prior$nMoves]
    racecar$primary$x <- prior$primary[prior$nMoves,1]
    racecar$primary$y <- prior$primary[prior$nMoves,2]
    racecar$offCourse <- prior$offCourse[prior$nMoves]
    racecar$current <- prior$current[prior$nMoves]

    # Move AI car
    dai <- list(x = aicar$x,
                y = aicar$y,
                primary = aicar$primary,
                current = aicar$current)
    pcar <- list(x = racecar$x, y = racecar$y)
    aiR <- aiDriver(rt$track, dai, pcar)
    if (aiR$crashed) {
      print("AI car crashed.  Need to deal with this")
    } else {
      aicar$x <- aiR$r$x
      aicar$y <- aiR$r$y
      aicar$primary <- aiR$r$primary
      aicar$current <- aiR$r$current
    }
    dai <- list(x = aicar$x,
                y = aicar$y,
                primary = aicar$primary,
                current = aicar$current,
                time = aicar$time)
    dpc <- list(x = racecar$x,
                y = racecar$y,
                primary = racecar$primary,
                current = racecar$current,
                time = racecar$time)
    over <- raceOver(dai, dpc, rt$track, prior$nMoves)
    if (over$finished) {
      aicar$time <- over$aicar$time
      racecar$time <- over$racecar$time
      finished$done <- TRUE
    }
  })

  # Pick next position
  observeEvent(input$click, {
    if (finished$done) return()
    # Get click location
    x <- input$click$x
    y <- input$click$y

    # Map to moveToGrid and move player car.
    dist <- sqrt((moveToGrid$x - x)^2 + (moveToGrid$y - y)^2)
    lmove <- which.min(dist)
    racecar$offCourse <- racecar$offCourse - 1

    # Determine primary vector (null if off course or still under penalty).
    if (moveToGrid$onCourse[lmove] == "yes" & racecar$offCourse <= 0) {
      racecar$primary$x <- moveToGrid$x[lmove] - racecar$x
      racecar$primary$y <- moveToGrid$y[lmove] - racecar$y
    } else {
      racecar$primary$x <- 0
      racecar$primary$y <- 0
    }
    if (moveToGrid$onCourse[lmove] == "no") racecar$offCourse <- nCrashSlowDown

    # Set the current move
    racecar$x <- moveToGrid$x[lmove]
    racecar$y <- moveToGrid$y[lmove]
    jrange <- seq(racecar$current, min(racecar$current + 300, rt$nCL))
    dx <- rt$track$centerline$x[jrange]
    dy <- rt$track$centerline$y[jrange]
    dist <- (dx - racecar$x)^2 + (dy - racecar$y)^2
    racecar$current <- racecar$current + which.min(dist) - 1

    # Update undo structure
    prior$nMoves <- prior$nMoves + 1
    prior$x[prior$nMoves] <- racecar$x
    prior$y[prior$nMoves] <- racecar$y
    prior$current[prior$nMoves] <- racecar$current
    prior$primary[prior$nMoves,] <- c(racecar$primary$x, racecar$primary$y)
    prior$offCourse[prior$nMoves] <- racecar$offCourse

    # Move AI car
    dai <- list(x = aicar$x,
                y = aicar$y,
                primary = aicar$primary,
                current = aicar$current)
    pcar <- list(x = racecar$x, y = racecar$y)
    aiR <- aiDriver(rt$track, dai, pcar)
    if (aiR$crashed) {
      aicar$primary$x <- 0
      aicar$primary$y <- 0
    } else {
      aicar$x <- aiR$r$x
      aicar$y <- aiR$r$y
      aicar$primary <- aiR$r$primary
      aicar$current <- aiR$r$current
    }
    dai <- list(x = aicar$x,
                y = aicar$y,
                primary = aicar$primary,
                current = aicar$current,
                time = aicar$time)
    dpc <- list(x = racecar$x,
                y = racecar$y,
                primary = racecar$primary,
                current = racecar$current,
                time = racecar$time)
    over <- raceOver(dai, dpc, rt$track, prior$nMoves)
    if (over$finished) {
      aicar$time <- over$aicar$time
      racecar$time <- over$racecar$time
      finished$done <- TRUE
    }
  })


  # Main Panel -----------------------------------------------------------------
  # Racetrack zoom plot
  output$racetrack <- renderPlot({
    if (finished$done == TRUE) {
      if (aicar$time < racecar$time) {
        label <- paste0("Blue Car\nTime: ", round(aicar$time, 2))
        ballColor <- "blue"
      } else {
        label <- paste0("Red Car\nTime: ", round(racecar$time, 2))
        ballColor <- "red"
      }
      g <- ggplot() +
           annotation_custom(flagGrob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf ) +
           annotate("polygon",
                    x = 250 / 72 * circle$x,
                    y = 250 / 72 * circle$y,
                    color = ballColor,
                    fill = "white",
                    size = 6,
                    alpha = 0.7) +
           annotate("text", x = 0, y = 0,
                    label = label,
                    color = "black",
                    size = 20) +
           coord_equal() +
           xlim(-5, 5) +
           ylim(-5, 5) +
           theme_void()
      return(g)
    }
    track <- rt$track
    xfoc <- c(racecar$x - 40, racecar$x + 40)
    yfoc <- c(racecar$y - 20, racecar$y + 20)

    moveTo$x <- racecar$x + racecar$primary$x + gridxy
    moveTo$y <- racecar$y + racecar$primary$y + gridxy
    xyg <-  expand.grid(x = moveTo$x, y = moveTo$y)
    isolate({
      moveToGrid$x <- xyg$x
      moveToGrid$y <- xyg$y
      moveToGrid$onCourse <- rep("no", 9)
      for (i in 1:9) {
        if (moveToGrid$y[i] >= track$ymin &
            moveToGrid$y[i] <= track$ymax &
            moveToGrid$x[i] >= track$xmin &
            moveToGrid$x[i] <= track$xmax) {
          iR <- moveToGrid$y[i] - track$ymin + 1
          iC <- moveToGrid$x[i] - track$xmin + 1
          if (track$rstr[iR,iC] == 1) moveToGrid$onCourse[i] <- "yes"
        }
      }
      moveToGrid$onCourse <- factor(moveToGrid$onCourse, levels = c("no", "yes"))
      mvt <- data.frame(x = moveToGrid$x, y = moveToGrid$y, onCourse = moveToGrid$onCourse)
    })

    g <- ggplot() +
      geom_point(data = track$dots, aes(x, y), size = 1, color = "gray50", pch = 3) +
      geom_path(data = track$inner, aes(x, y)) +
      geom_path(data = track$outer, aes(x, y)) +
      geom_path(data = track$finish, aes(x, y), color = "black", size = 3) +
      coord_equal() + theme_void() +
      scale_x_continuous(limits = xfoc) +
      scale_y_continuous(limits = yfoc) +
      annotate("point", x = racecar$x, y = racecar$y, size = 7, color = "red") +
      annotate("point", x = aicar$x, y = aicar$y, size = 7, color = "blue") +
      geom_point(data = mvt, aes(x, y, fill = onCourse), size = 4, pch = 21 ) +
      annotate("segment", x = racecar$x, y = racecar$y,
               xend = mvt$x[5], yend = mvt$y[5],
               arrow = arrow(length = unit(0.5, "cm")),
               col = "red") +
      scale_fill_manual(values = c("red", "green"), drop = FALSE) +
      theme(legend.position = "none")

    g
  })

  # Racetrack inset plot
  output$inset <- renderPlot({
    track <- rt$track
    g <- ggplot(track$dots, aes(x,y)) +
      geom_path(data = track$inner, aes(x, y)) +
      geom_path(data = track$outer, aes(x, y)) +
      geom_path(data = track$finish, aes(x, y), color = "black") +
      annotate("point", x = racecar$x, y = racecar$y, size = 4, color = "red") +
      annotate("point", x = aicar$x, y = aicar$y, size = 4, color = "blue") +
      coord_equal() + theme_void()

    g
  })

  # Speed and elapsed time
  output$stats <- renderText({
    paste0("Speed: ",
          round(sqrt(racecar$primary$x^2 + racecar$primary$y^2), 2),
          "\n",
          "Elapsed: ", prior$nMoves - 1)
  })

  # Pop up help panel
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Instructions",
      HTML(paste("Guide the red car around the track in as few moves as possible.",
                 "Your car's momentum dictates where you can go.  Click any",
                 "green circle to move.  Be sure to slow down before turns and avoid",
                 "the barricades (red circles).  If you crash, get back on the track",
                 "as quickly as possible and your car will (slowly) regain full throttle.",
                 tags$br(), tags$br(),
                 "You can undo as many moves as needed if you get in trouble,",
                 "but beware--the blue car keeps going.",
                 tags$br(), tags$br(),
                 "Justin Revenaugh", tags$br(),
                 "Earth Sciences", tags$br(),
                 "University of Minnesota", tags$br(),
                 "justinr@umn.edu", tags$br(),
                 "Code at: github.com/jrevenaugh/RacerX"
                )
      ),
      easyClose = TRUE)
    )
  })
}
