rt <- readRDS("aiginio.RDS")
g <- ggplot(rt$dots, aes(x,y)) +
     geom_point(size = 0.4, pch = 3) +
     geom_path(data = rt$inner, aes(x, y)) +
     geom_path(data = rt$outer, aes(x, y)) +
     geom_point(data = rt$finish, aes(x,y), color = "blue", size = 0.4, pch = 3) +
     geom_point(data = rt$centerline, aes(x, y), color = "red", size = 0.4, pch = 3) +
     coord_equal() + theme_void()

print(g)

carx <- rt$finish$x[3]
cary <- rt$finish$y[3]

primaryx <- 0
primaryy <- 0

gridxy <- c(-1,0,1)

while (1) {
  moveTox <- carx + primaryx + gridxy
  moveToy <- cary + primaryy + gridxy
  moveToGrid <- expand.grid(x = moveTox, y = moveToy)
  print(moveToGrid)
  g <- ggplot() +
    geom_point(data = rt$dots, aes(x,y), size = 0.1) +
    geom_point(data = moveToGrid, aes(x, y), pch = 1, color = "red") +
    geom_path(data = rt$inner, aes(x, y)) +
    geom_path(data = rt$outer, aes(x, y)) +
    coord_equal() + theme_void()

  print(g)
  i <- sample(1:3, 1)
  j <- sample(1:3, 1)
  i <- 4
  j <- 4
  primaryx <- moveToGrid$x[i] - carx
  primaryy <- moveToGrid$y[j] - cary
  carx <- moveToGrid$x[i]
  cary <- moveToGrid$y[j]
  if (!(carx %in% rt$dots$x & cary %in% rt$dots$y)) {
    print("Crash")
    break
  }
}

