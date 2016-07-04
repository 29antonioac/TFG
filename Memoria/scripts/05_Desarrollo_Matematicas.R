## ---- randomLine ----
simmulate.random.line <- function(interval = c(0,1)){
  A <- runif(2, interval[1], interval[length(interval)])
  B <- runif(2, interval[1], interval[length(interval)])
  
  # The scope m is the quotient of the difference of the components
  m <- (A[2] - B[2]) / (A[1] - B[1])
  
  # The bias b is the other coefficient
  b <- A[2] - m * A[1]
  
  # Return in the right order for using with abline
  return(c(b = b,m = m))
}

## ---- signOfPoint ----
sign.of.point <- function(point, line){
  return(sign(point[2] - point[1]*line["m"] - line["b"]))
}

## ---- dataExample ----
line.random <- simmulate.random.line()
example.data <- data.frame(matrix(runif(100), ncol = 2, byrow = TRUE))
example.data$Label <- as.factor(apply(X = example.data, 
                                      MARGIN = 1, FUN = sign.of.point, line=line.random))
g <- ggplot(example.data) + 
      geom_point(aes(X1,X2, colour = Label, size = 2)) + 
      guides(size=FALSE)
g

## ---- classificationExample ----
g <- g + geom_abline(intercept = line.random[1], slope = line.random[2])
g

## ---- breakExample ----
ggplot(data.frame(X1 = runif(3),
                  X2 = runif(3)), aes(X1,X2)) + geom_point(size = 4)

## ---- anotherbreakExample ----
ggplot(data.frame(X1 = runif(4),
                  X2 = runif(4),
                  label = as.factor(c(-1,1,-1,1))), aes(X1,X2, col = label)) +
                  geom_point(size = 4) + guides(col="none")