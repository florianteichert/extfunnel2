library(extfunnel2)

data <- data.frame(yi = c(-10.0, -16.3, -27.0, -14.0, -15.1, -2.1, -19.0, -8.7),
                   sei = c(3.4, 5.8, 3.4, 5.3, 2.9, 1.1, 7.2, 4.2),
                   ni = c(174.0, 35.0, 25.0, 30.5, 13.5, 34.0, 15.0, 86.0))

extfunnel2(data$yi, data$sei,
           swe = -20,
           method = "DL",
           contour_points = 50,
           x_lim = c(-40, 10), y_lim = c(0, 15),
           x_ticks = seq(from = -40, to = 10, by = 10),
           y_ticks = seq(from = 0, to = 15, by = 3),
           x_lab = "Effect on pain (100 point scale)"
)

extfunnel2(data$yi,
           sd = 15, n = data$ni,
           swe = -20,
           method = "DL",
           contour_points = 500,
           x_lim = c(-40, 10), y_lim = c(10, 1000),
           x_ticks = seq(from = -40, to = 10, by = 10),
           y_ticks = seq(from = 0, to = 1000, by = 200),
           x_lab = "Effect on pain (100 point scale)"
)
