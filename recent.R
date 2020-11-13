recent <- function(x){
  aggregate(data = x, . ~ year, FUN = "max")
}
