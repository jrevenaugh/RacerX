# server
#
# RacerX shiny server

server <- function(input, output, session) {

  # Reactives ------------------------------------------------------------------
  racecar <- reactiveValues(x = NA,
                            y = NA,
                            primary = NA)

  prior <- reactiveValues(x = rep(0, nBack),
                          y = rep(0, nBack),
                          primary = matrix(0, nrow = nBack, ncol = 2),
                          nCurrent = 1)

  rt <- reactiveValues(track = NA)

  moveToGrid <- reactiveValues(x = NA,
                               y = NA,
                               onCourse = NA)

  finished <- reactiveValues(done = FALSE)

  # Event Observers ------------------------------------------------------------
  observeEvent(c(input$reset, input$track), {
    trackName <- paste0(input$track, ".RDS")
    rt$track <- readRDS(trackName)
    track <- rt$track
    n <- length(track$finish$x)
    n <- sample(1:length(track$finish$x), 1)
    racecar$x <- track$finish$x[n]
    racecar$y <- track$finish$y[n]
    racecar$primary <- data.frame(x = 0, y = 0)
    prior$x <- rep(racecar$x, nBack)
    prior$y <- rep(racecar$y, nBack)
    prior$primary <- matrix(0, nrow = nBack, ncol = 2)
    prior$nCurrent <- 1
  })


  # Undo last move (recursive)
  observeEvent(input$undo, {
    if (prior$nCurrent <= 1) return()
    prior$nCurrent <- prior$nCurrent - 1
    racecar$x <- prior$x[prior$nCurrent]
    racecar$y <- prior$y[prior$nCurrent]
    racecar$primary$x <- prior$primary[prior$nCurrent,1]
    racecar$primary$y <- prior$primary[prior$nCurrent,2]
  })

  # Pick next position
  observeEvent(input$click, {
    x <- input$click$x
    y <- input$click$y

    dist <- sqrt((moveToGrid$x - x)^2 + (moveToGrid$y - y)^2)
    lmove <- which.min(dist)
    racecar$primary$x <- moveToGrid$x[lmove] - racecar$x
    racecar$primary$y <- moveToGrid$y[lmove] - racecar$y
    racecar$x <- moveToGrid$x[lmove]
    racecar$y <- moveToGrid$y[lmove]
    prior$nCurrent <- prior$nCurrent + 1
    prior$x[prior$nCurrent] <- racecar$x
    prior$y[prior$nCurrent] <- racecar$y
    prior$primary[prior$nCurrent,] <- c(racecar$primary$x, racecar$primary$y)
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
        xind <- which(moveToGrid$x[i] == track$dots$x)
        if (length(xind) >= 1) {
          moveToGrid$onCourse[i] <- ifelse(any(moveToGrid$y[i] == track$dots$y[xind]),
                                           "yes", "no")
        }
      }
      moveToGrid$onCourse <- factor(moveToGrid$onCourse, levels = c("no", "yes"))
      mvt <- data.frame(x = moveToGrid$x, y = moveToGrid$y, onCourse = moveToGrid$onCourse)
    })

    g <- ggplot() +
      geom_point(data = track$dots, aes(x, y), size = 1, color = "gray50", pch = 3) +
      geom_path(data = track$inner, aes(x, y)) +
      geom_path(data = track$outer, aes(x, y)) +
      geom_path(data = track$finish, aes(x, y), color = "black", size = 3, linetype = "dashed") +
      coord_equal() + theme_void() +
      scale_x_continuous(limits = xfoc) +
      scale_y_continuous(limits = yfoc) +
      annotate("point", x = racecar$x, y = racecar$y, size = 7, color = "red") +
      annotate("segment", x = racecar$x, y = racecar$y,
               xend = mvt$x[5], yend = mvt$y[5],
               arrow = arrow(length = unit(0.5, "cm")),
               col = "red") +
      geom_point(data = mvt, aes(x, y, fill = onCourse), size = 4, pch = 21 ) +
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
      annotate("point", x = racecar$x, racecar$y, size = 4, color = "red") +
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
      HTML(paste("Guide your car around the track in as few moves as possible.",
                 "Your car's momentum dictates where you can go.  Click any",
                 "green circle to move.  Be sure to slow down before turns, and avoid",
                 "oil slicks (black circles) and the barricades (red circles).",
                 tags$br(), tags$br(),
                 "You can undo as many moves as needed if you get in trouble.")
      ),
      easyClose = TRUE)
    )
  })
}
