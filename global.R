# Are you threatening me?
options(warn = -1)

# Movement parameters ----------------------------------------------------------
gridxy <- c(-1,0,1)
moveTo <- data.frame(x = rep(0, 3),
                     y = rep(0, 3))


# Number of undo steps available -----------------------------------------------
nBack <- 500

# Track collection -------------------------------------------------------------
track_choices = c("Hermanos Rodriguez Oval" = "Autodromo Hermanos Rodriguez Oval",
                  "Lausitz Tri-Oval" = "Lausitz Oval",
                  "Autodromo Juan y Oscar Galvez" = "Autodromo Juan y Oscar Galvez",
                  "F1 Outdoors Kart A&D" = "F1 Outdoors Kart A&D",
                  "Grand Bend Raceway" = "Grand Bend Raceway",
                  "Driveway Austin" = "Driveway Austin",
                  "Aiginio ART Greece" = "Aiginio ART Greece",
                  "Las Vegas Motor Speedway Road" = "Las Vegas Motor Speedway Road",
                  "Gotland Ring North Circuit" = "Gotland Ring North Circuit",
                  "Bridge Hampton Raceway" = "Bridge Hampton Raceway"
                 )

# AI parameters ----------------------------------------------------------------
maxTried <- 8
nAhead <- 10
maxSpeed <- nAhead - 1
ocdPenaltyFactor <- 2.5 # Off centerline distance penalty factor

# Crash parameters -------------------------------------------------------------
nCrashSlowDown <- 3

# Finish line stuff ------------------------------------------------------------
flagGrob <- readRDS(url("https://github.com/jrevenaugh/RacerX/raw/master/flags.RDS"))
angle <- seq(0, 2 * pi, length.out = 60)
circle <- data.frame(x = cos(angle), y = sin(angle))
