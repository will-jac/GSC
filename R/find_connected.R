find_connected = function(connect) {
  for (i in 1:nrow(connect)) {
    con = matrix()
    for (j in 1:ncol(connect)) {
      if (connect[i,j] > 0) {
        con = cbind(con, j)
      }
    }
    print(paste(i, con, sep=':'))
  }
}
