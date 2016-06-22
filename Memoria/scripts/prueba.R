## ---- prueba ----
data <- data.frame(x = 1:500)
ggplot(data, aes(x)) +
  stat_function(fun=cos, n=nrow(data), geom="point")
