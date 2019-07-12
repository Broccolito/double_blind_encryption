library(digest)
library(gtools)
library(glue)

#Defining elements in the dictionary
all_elements = c(letters, LETTERS, seq(0, 9, 1), rep("?", 1))
all_hash_elements = c(letters, LETTERS, seq(0, 9, 1))
perm = permutations(n = 26 * 2 + 10, r = 3, v = all_hash_elements, 
                    repeats.allowed = FALSE)
for(i in 1:dim(perm)[1]){
  perm[i,] = c(paste(perm[i,], collapse = ""), 0, 0)
}
perm = as.vector(perm[,1])

#This is one of the moving parts of this algorithm
#Different seeds will generate different dictionaries

generate_dict = function(seed){
  set.seed(seed) 
  dict <<- data.frame(index = seq(1, length(all_elements), 1), 
                    element = all_elements, 
                    encode = sample(perm, length(all_elements), 
                                    replace = FALSE))
}

is.odd <- function(x){
  return(x %% 2 != 0)
}

element_to_hash = function(e){
  return(as.vector(dict[dict$element == e,]$encode))
}

hash_to_element = function(h){
  return(as.vector(dict[dict$encode == h,]$element))
}

string_to_hash_string = function(s){
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

hash_string_to_string = function(hash_string){
  temp = unlist(strsplit(hash_string, split = ""))
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
  templen = length(temp) / 3
  for(i in 1:templen){
    temp = c(temp, paste(c(temp[i * 3 - 2], 
                           temp[i * 3 - 1], 
                           temp[i * 3]), 
                         collapse = ""))
  }
  temp = temp[sapply(temp, nchar) == 3]
  temp = sapply(temp, hash_to_element)
  #Return NULL if no result can be found
  if(any(sapply(temp, length) == 0)){
    return(NULL)
  }
  names(temp) = NULL
  temp = paste(temp, collapse = "")
  return(temp)
}

passcode_to_hash = function(passcode){
  len_password = nchar(passcode) 
  passcode = unlist(strsplit(passcode, split = ""))
  missing_char = 50 - len_passcode
  pos = sample(1:len_passcode, size = missing_char, replace = TRUE)
  for(i in pos){
    passcode = append(passcode, values = "?", after = i)
  }
  passcode = paste(passcode, collapse = "")
  passcode = string_to_hash_string(passcode)
  return(passcode)
}

hash_to_passcode = function(hash_string){
  temp = hash_string_to_string(hash_string)
  temp = unlist(strsplit(temp, split = ""))
  temp[temp == "?"] = ""
  temp = paste(temp, collapse = "")
  return(temp)
}

#This is an unbreakable encoding
multiple_hash_encoding = function(string){
  mhe1 = function(s){
    return(sha512(sha512(sha512(sha512(sha512(s))))))
  }
  mhe2 = function(s){
    return(mhe1(mhe1(mhe1(mhe1(mhe1(s))))))
  }
  return(mhe2(mhe2(mhe2(mhe2(mhe2(string))))))
}



#Testing
generate_dict(171)
hash_to_passcode(password_to_hash("happy"))
# [1] "happy"
