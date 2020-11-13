# 열의 위치를 원하는 곳으로 변경하는 함수

arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}

# Guide
df <- arrange_vars(df, c("marekt") = value))

# Example
data("iris")

head(iris)
  Sepal.Length Sepal.Width Petal.Length Petal.Width Species
1          5.1         3.5          1.4         0.2  setosa
2          4.9         3.0          1.4         0.2  setosa
3          4.7         3.2          1.3         0.2  setosa
4          4.6         3.1          1.5         0.2  setosa
5          5.0         3.6          1.4         0.2  setosa
6          5.4         3.9          1.7         0.4  setosa

iris <- arrange_vars(iris, c("Sepal.Width" = 5))

head(iris)
  Sepal.Length Petal.Length Petal.Width Species Sepal.Width
1          5.1          1.4         0.2  setosa         3.5
2          4.9          1.4         0.2  setosa         3.0
3          4.7          1.3         0.2  setosa         3.2
4          4.6          1.5         0.2  setosa         3.1
5          5.0          1.4         0.2  setosa         3.6
6          5.4          1.7         0.4  setosa         3.9
