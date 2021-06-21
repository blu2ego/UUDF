# creates the folder at any depth and returns the path back:
makepath <- function(path) {dir.create(dirname(path), recursive=TRUE); path}
