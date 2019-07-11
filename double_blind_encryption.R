library(digest)
library(gtools)
library(glue)

all_letters <- c(letters, LETTERS, seq(0, 9, 1))
perm = permutations(n = 26 * 2 + 10, r = 3, v = all_letters, repeats.allowed = F)
for(i in 1:dim(perm)[1]){
  perm[i,] = c(paste(perm[i,], collapse = ""), 0, 0)
}
perm = as.vector(perm[,1])

#This is one of the moving parts of this algorithm
#Different seeds will generate different dictionaries
set.seed(120) 

dict = data.frame(index = seq(1, length(c(seq(0,9,1), letters, LETTERS)), 1), 
                  element = c(seq(0,9,1), letters, LETTERS), 
                  encode = sample(perm, length(c(seq(0,9,1), letters, LETTERS)), 
                                  replace = FALSE))

is.odd <- function(x){
  return(x %% 2 != 0)
}

element_to_hash = function(e){
  return(as.vector(dict[dict$element == e,]$encode))
}

hash_to_element = function(h){
  return(as.vector(dict[dict$encode == h,]$element))
}

string_to_hash = function(s){
  temp = sapply(unlist(strsplit(s, split = "")), element_to_hash)
  temp = paste(temp, collapse = "")
  temp = unlist(strsplit(temp, split = ""))
  if(is.odd(length(temp))){
    templen = length(temp) - 1
  }else{
    templen = length(temp)
  }
  templen = templen / 2
  for(i in 1:templen){
    temp_in_temp = temp[2 * i]
    temp[2 * i] = temp[2 * i - 1]
    temp[2 * i - 1] = temp_in_temp
  }
  return(paste(temp, collapse = ""))
}

string_to_hash("happy")
