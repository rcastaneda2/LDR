n <- 15
LD <- c(4,12,13)

#N <- 15
#sus <- c(4,12,13)

make_a_vector <- function(N,sus,complex = F,pattern="triangle"){
  ##indices for counting
  # i for pairs of all items
  # j for pairs of sus items
  # k for number of items

  #Count pairs
  npairs = choose(N,2)
  nsus = choose(length(sus),2)

  #Make empty objects
  sus_pair_names = rep(NA,nsus)
  mod = rep(0,npairs)

  if(pattern=="triangle"){
    #upper triangle pattern
    all_pair_names <- NULL

    for (col in 2:n){
      for (row in 1:(col-1)){
        new <- paste(c(row,col),collapse=",")
        all_pair_names <- append(all_pair_names,new)
      }
    }

  } else if(pattern == "list"){

    all_pair_names = rep(NA,npairs)
    #Vector listing all possible pairs
    all_pairs = combn(1:N,2)
    for(i in 1:npairs){all_pair_names[i] = paste(all_pairs[,i], collapse = ",")}
  }


  #Vector listing all suspicion pairs
  sus_pairs = combn(sus,2)
  for(j in 1:nsus){sus_pair_names[j] = paste(sus_pairs[,j], collapse = ",")}

  #Mark the position of the suspicious pairs
  for(j in 1:nsus){
    for(i in 1:npairs){
      if (sus_pair_names[j] == all_pair_names[i]){
        mod[i] = 1
      }
    }
  }

  #option to view that sus pairs match with LD mod vector
  mod_names = cbind(as.data.frame(all_pair_names),mod)

  #if complex, make a matrix to pick off each item
  if(complex == T){
    mat = make_a_matrix(N,npairs,all_pairs)
    mod = rbind(mat,mod)
  }

  #Output an object to feed into rma and one to view
  out = list(mod = mod, mod_names = mod_names)
}

make_a_matrix <- function(N,npairs,all_pairs){
  #given the number of items,pairs, and what the pairs are
  #make a matrix to reflect the 'complex' structure
  mat <- matrix(0,N-1,npairs)

  #set up a moderator for each item
  for(k in 1:(N-1)){
    for(i in 1:npairs){
      if (all_pairs[1,i] == k | all_pairs[2,i] == k){
        mat[k,i] = 1
      }
    }
  }

  return(mat)
}

test <- make_a_vector(N = n, sus = LD,complex=F, pattern='triangle')
