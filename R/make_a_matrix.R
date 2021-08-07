LD <- c(1,3)
n <- 5

make_a_matrix <- function(N,sus){
  npairs = choose(N,2)
  pair_names = rep(NA,length(npairs))

  mat <- matrix(0,4,npairs)
  mat[1:2,] = combn(1:N,2)
  mat[3,] = 1

  for(j in 1:length(sus)){
    for(i in 1:npairs){
      if (j == 1){
        pair_names[i] = paste(mat[1:2,i], collapse = ",")
      }
      if (mat[1,i] == sus[j] | mat[2,i] == sus[j]){
        mat[4,i] = 1
      }
    }
  }
  out = list(out1 = t(mat),out2 = pair_names)
  return(out)
}

make_a_matrix(N = n, sus = LD)
