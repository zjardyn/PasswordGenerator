#
# A simple password generator for sites and apps
# 


# generates the password and writes it to file
simple_password <- function(app, file, n, symbol) {
  x <- sample(LETTERS, 10, replace = T)
  y <- sample(letters, 10, replace = T)
  z <- sample(1:9, 10, replace = T)
  symbols <- unlist(strsplit("@%+#$:~_^*&+=;", ""))
  s <- sample(symbols, 10, replace = T)
  if(symbol==T){
    xyz <- c(x,y,z,s)
  } else {
    xyz <- c(x,y,z)
  }
  xyz <- sample(xyz, n, replace = F )
  xyz <- paste(xyz, collapse = "")
  xyz <- paste(app, xyz, sep = "    " )
  write(xyz, file=file, append=T)
}
 
# file="passwords.txt"
# simple_password("Facebook", file, 15, T)
# file.remove(file)

# takes a list of passwords and appends them to file
password_file <- function(apps, file, n=12, seed=123, symbol=T){
  if (file.exists(file)) {
    file.remove(file)
  }
  for(app in apps){
    set.seed(seed)
    simple_password(app, file, n, symbol)
    seed <- seed + 1
  }
}

apps <- c("Facebook", "Courselink", "Google", "LoL", "Amazon", "ComputeCanada")
file="passwords.txt"

password_file(apps, file, 15, 123, symbol = T)

# Facebook    4&7JKvyyO:7^R8e
# Courselink    +J7A~8G+^#hF6@y
# Google    ^+wSmCwq763#k^X
# LoL    cL3mw_^kWBZt_f1
# Amazon    5SuIr=%z5$usw*f
# ComputeCanada    ~#@wXY8Q3&ft6Vd
  
