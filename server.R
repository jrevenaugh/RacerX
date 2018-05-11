# server
#
# RacerX shiny server

source("aiDriver.R")

server <- function(input, output, session) {

  # Reactives ------------------------------------------------------------------
  racecar <- reactiveValues(x = NA,
                            y = NA,
                            primary = NA,
                            offCourse = 0)

  aicar <- reactiveValues(x = NA,
                          y = NA,
                          primary = NA,
                          current = NA)

  prior <- reactiveValues(x = rep(0, nBack),
                          y = rep(0, nBack),
                          primary = matrix(0, nrow = nBack, ncol = 2),
                          offCourse = rep(0, nBack),
                          nCurrent = 1)

  rt <- reactiveValues(track = NA)

  moveToGrid <- reactiveValues(x = NA,
                               y = NA,
                               onCourse = NA)

  finished <- reactiveValues(done = FALSE)

  # Event Observers ------------------------------------------------------------
  observeEvent(c(input$reset, input$track), {
    # Load new track
    trackName <- paste0("Tracks/", input$track, ".RDS")
    rt$track <- readRDS(trackName)

    # Straighten the finish line (hanging chad from course creation)
    rt$track$finish$x <- rt$track$finish$x[1]
    rt$track$start$x <- rt$track$start$x[1]
    track <- rt$track

    # Create logical raster of track (FALSE = off course).
    xrange <- range(track$dots$x)
    yrange <- range(track$dots$y)
    rt$track$rstr <- matrix(FALSE, nrow = diff(yrange) + 1, ncol = diff(xrange) + 1)
    rt$track$xmin <- xrange[1]
    rt$track$xmax <- xrange[2]
    rt$track$ymin <- yrange[1]
    rt$track$ymax <- yrange[2]
    for (i in 1:nrow(track$dots)) {
      iR <- track$dots$y[i] - rt$track$ymin + 1
      iC <- track$dots$x[i] - rt$track$xmin + 1
      rt$track$rstr[iR,iC] <- TRUE
    }
    rt$track$centerline <- rbind(data.frame(x = mean(track$finish$x), y = mean(track$finish$y)),
                                 track$centerline)

    # Get starting positions
    n <- length(track$finish$x)
    n <- sample(1:length(track$finish$x), 2)
    racecar$x <- track$finish$x[1]
    racecar$y <- track$finish$y[n[1]]
    racecar$primary <- data.frame(x = 0, y = 0)
    racecar$offCourse <- 0
    aicar$x <- track$finish$x[1]
    aicar$y <- track$finish$y[n[2]]
    aicar$primary <- data.frame(x = 0, y = 0)
    aicar$current <- 0

    # Set up undo structure
    prior$x <- rep(racecar$x, nBack)
    prior$y <- rep(racecar$y, nBack)
    prior$primary <- matrix(0, nrow = nBack, ncol = 2)
    prior$offCourse <- rep(0, nBack)
    prior$nCurrent <- 1
  })


  # Undo last move (recursive).  Note that this only undoes the player's car.
  # AI is unaffected.  This allows you to correct a fatal error, but at a price.
  observeEvent(input$undo, {
    if (prior$nCurrent <= 1) return()
    prior$nCurrent <- prior$nCurrent - 1
    racecar$x <- prior$x[prior$nCurrent]
    racecar$y <- prior$y[prior$nCurrent]
    racecar$primary$x <- prior$primary[prior$nCurrent,1]
    racecar$primary$y <- prior$primary[prior$nCurrent,2]
    racecar$offCourse <- prior$offCourse[prior$nCurrent]
  })

  # Pick next position
  observeEvent(input$click, {
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

    # Update undo structure
    prior$nCurrent <- prior$nCurrent + 1
    prior$x[prior$nCurrent] <- racecar$x
    prior$y[prior$nCurrent] <- racecar$y
    prior$primary[prior$nCurrent,] <- c(racecar$primary$x, racecar$primary$y)
    prior$offCourse[prior$nCurrent] <- racecar$offCourse


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
  })

  # Main Panel -----------------------------------------------------------------
  # Racetrack zoom plot
  output$racetrack <- renderPlot({
    track <- rt$track
    xfoc <- c(racecar$x - 30, racecar$x + 30)
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
               xend = mvt$x[9], yend = mvt$y[9],
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
          "Elapsed: ", prior$nCurrent - 1)
  })

  # Pop up help panel
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Instructions",
      HTML(paste("Guide the red car around the track in as few moves as possible.",
                 "Your car's momentum dictates where you can go.  Click any",
                 "green circle to move.  Be sure to slow down before turns, and avoid",
                 "oil slicks (black circles) and the barricades (red circles).",
                 tags$br(), tags$br(),
                 "You can undo as many moves as needed if you get in trouble,",
                 "but the blue car keeps going.")
      ),
      easyClose = TRUE)
    )
  })
}
