# Are you threatening me?
options(warn = -1)

# Movement parameters ----------------------------------------------------------
gridxy <- c(1,-1,0)
moveTo <- data.frame(x = rep(0, 3),
                     y = rep(0, 3))


# Number of undo steps available -----------------------------------------------
nBack <- 5000

# Track collection -------------------------------------------------------------
track_choices = c("Aiginio ART Greece" = "Aiginio ART Greece",
                  "Hermanos Rodriguez Oval" = "Autodromo Hermanos Rodriguez Oval",
                  "Las Vegas Motor Speedway Road" = "Las Vegas Motor Speedway Road",
                  "Bridge Hampton Raceway" = "Bridge Hampton Raceway",
                  "Autodromo Juan y Oscar Galvez" = "Autodromo Juan y Oscar Galvez",
                  "F1 Outdoors Kart A&D" = "F1 Outdoors Kart A&D",
                  "Grand Bend Raceway" = "Grand Bend Raceway",
                  "Gotland Ring North Circuit" = "Gotland Ring North Circuit",
                  "Driveway Austin" = "Driveway Austin"
                 )

# AI parameters ----------------------------------------------------------------
maxTried <- 9
nAhead <- 10
maxSpeed <- nAhead - 1

# Crash parameters -------------------------------------------------------------
nCrashSlowDown <- 3
