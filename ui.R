# UI
#
# Establish a bootstrapPage with input control widgets and track overview on the LHS
# and track zoom illustration filling page.

require(shiny)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  plotOutput(outputId = "racetrack",
             height = "100%",
             width = "100%",
             click = "click"),

  absolutePanel(bottom = 10, left = 10, width = "260px", draggable = FALSE,
                plotOutput(outputId = "inset",
                           height = "250px",
                           width = "250px"),
                verbatimTextOutput(outputId = "stats"),
                style = "opacity: 0.8; background:#FFFFFF;"),

  absolutePanel(top = 10, left = 10, width = "260px", draggable = FALSE,
                wellPanel(h4("RacerX V1.0"),
                          selectInput(inputId = "track",
                                      label = "Track",
                                      choices = track_choices,
                                      width = "100%"),
                          actionButton(inputId = "undo",
                                       label = "Undo",
                                       width = "100%",
                                       style = "margin-bottom: 5px;"),
                          actionButton(inputId = "reset",
                                       label = "New Race",
                                       width = "100%",
                                       style = "margin-bottom: 5px;"),
                          actionButton(inputId = "help",
                                       label = "Instructions",
                                       width = "100%"),
                          style = "opacity: 0.8; background:#FAFAFA;")
  )
)
