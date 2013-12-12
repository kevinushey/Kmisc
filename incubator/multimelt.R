df <- data.frame(
  id1=letters[1:5],
  id2=LETTERS[1:5],
  v1.Mean=1:5,
  v1.SD=6:10,
  z1.Mean=11:15,
  z1.SD=16:20
)

data <- data.table(df)
id.vars <- c("id1", "id2")
measure.vars <- list( c("v1.Mean", "v1.SD"), c("z1.Mean", "z1.SD") )
variable.names <- c("v1", "v2")
value.names <- c("Mean", "SD")

multimelt <- function(data, ...) {
  UseMethod("multimelt")
}

multimelt.data.table <- function(data, id.vars, measure.vars, 
  variable.names, ..., value.names) {
  
  tmp <- lapply(measure.vars, function(x) {
    melt_( data[, c(id.vars, x), with=FALSE], id.vars=id.vars )
  })
  
  output <- tmp[[1]][, c(id.vars, "variable"), with=FALSE]
  for (i in seq_along(tmp)) {
    output[, eval(value.names[i]) := tmp[[i]][["value"]]]
  }
  
  
}
