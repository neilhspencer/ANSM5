perms <- function(n){
  if(n == 1){
    return(matrix(1))
  }else{
    prev <- perms(n - 1)
    p <- nrow(prev)
    perms_out <- matrix(nrow = n * p, ncol = n)
    for(i in 1:n){
      perms_out[(i - 1) * p + 1:p,] <- cbind(i , prev + (prev >= i))
    }
    return(perms_out)
  }
}
