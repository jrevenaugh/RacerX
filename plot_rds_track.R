rt <- readRDS("aiginio.RDS")
g <- ggplot(rt$dots, aes(x,y)) +
     geom_point(size = 0.4, pch = 3) +
     geom_path(data = rt$inner, aes(x, y)) +
     geom_path(data = rt$outer, aes(x, y)) +
     geom_point(data = rt$finish, aes(x,y), color = "blue", size = 0.4, pch = 3) +
     geom_point(data = rt$centerline, aes(x, y), color = "red", size = 0.4, pch = 3) +
     coord_equal() + theme_void()

print(g)
