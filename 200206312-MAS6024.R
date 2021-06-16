lgrid <- matrix(NA, nrow = 8, ncol = 8)
lgrid[1,] <- c("r", "l", "q", "s", "t", "z", "c", "a")
lgrid[2,] <- c("i", "v", "d", "z", "h", "l", "t", "p")
lgrid[3,] <- c("u", "r", "o", "y", "w", "c", "a", "c")
lgrid[4,] <- c("x", "r", "f", "n", "d", "p", "g", "v")
lgrid[5,] <- c("h", "j", "f", "f", "k", "h", "g", "m")
lgrid[6,] <- c("k", "y", "e", "x", "x", "g", "k", "i")
lgrid[7,] <- c("l", "q", "e", "q", "f", "u", "e", "b")
lgrid[8,] <- c("l", "s", "d", "h", "i", "k", "y", "n")

#part1 不在边缘的初始位置的情况，和在边缘的移动情况
make_move <- function(start){
  x <- start[1]
  y <- start[2]
  if ((x > 1 & x < 8) & (y > 1 & y < 8)){
    adjacent_square <- list(c(x-1, y-1), c(x-1, y), c(x-1, y+1), c(x, y-1), 
                            c(x, y+1), c(x+1, y-1), c(x+1, y), c(x+1, y+1))
    finish <- as.vector(unlist(sample(adjacent_square, 1)))
    finish_x <- finish[1]
    finish_y <- finish[2]
  } else{
    finish_x <- sample(1:8, 1)
    finish_y <- sample(1:8, 1)
  }
  return(c(finish_x, finish_y))
}

start <- c(1, 4)
(next_move <- make_move(start))

#如果令牌落在白色方块上，是否将字母添加到玩家的收藏夹中
Two_same_letters <- function(c){
  if(isTRUE(c[1] == c[2] & c[1] != c[3]) |
     isTRUE(c[1] == c[3] & c[1] != c[2]) |
     isTRUE(c[2] == c[3] & c[2] != c[1])) return(TRUE) else return(FALSE)
}

Three_same_letters <- function(c){
  if(isTRUE(c[1] == c[2] & c[1] == c[3])) return(TRUE) else return(FALSE)
}

Four_same_letters <- function(c){
  if(isTRUE(c[1] == c[2] & c[1] == c[3] & c[1] == c[4])) return(TRUE) else return(FALSE)
}

AAAB_or_AABB <- function(c){
  if(sum(c[1] == c) == 1 | sum(c[1] == c) == 3) return(TRUE)
  if(sum(c[1] == c) == 2) return(FALSE)
}

getmode <- function(c) {
  uniqc <- unique(c)
  uniqc[which.max(tabulate(match(c, uniqc)))]
}

white_square <- function(square_coor, collect){
  square_x <- square_coor[1]
  square_y <- square_coor[2]
  len_col <- length(collect)
  if(isTRUE(len_col == 0)) collect[len_col+1] <- lgrid[square_x, square_y]
  if(isTRUE(len_col == 1)) collect[len_col+1] <- lgrid[square_x, square_y]
  if(isTRUE(len_col == 2)) collect[len_col+1] <- lgrid[square_x, square_y]
  if(isTRUE(len_col == 3)){
    if(isTRUE(Two_same_letters(collect)) | 
       isTRUE(Three_same_letters(collect))) collect[len_col+1] <- lgrid[square_x, square_y]
    if(isFALSE(Two_same_letters(collect)) & 
       isFALSE(Three_same_letters(collect))){
      if(any(collect == lgrid[square_x, square_y])) collect[len_col+1] <- lgrid[square_x, square_y]
    }
  }
  if(isTRUE(len_col == 4)){
    if(isTRUE(Four_same_letters(collect))) collect[len_col+1] <- lgrid[square_x, square_y]
    if(isTRUE(Three_same_letters(collect))){
      if(isTRUE(any(collect == lgrid[square_x, square_y]))) collect[len_col+1] <- lgrid[square_x, square_y]
    }
    if(isFALSE(Two_same_letters(collect)) & isFALSE(Three_same_letters(collect))){
      if(isTRUE(any(collect == lgrid[square_x, square_y])) & 
         isTRUE(collect[4] != lgrid[square_x, square_y])) collect[len_col+1] <- lgrid[square_x, square_y]
    }
    if(isTRUE(Two_same_letters(collect))){
      if(isFALSE(any(collect[1:3] == collect[4]))){
        if(isTRUE(any(collect == lgrid[square_x, square_y])) & 
           isFALSE(getmode(collect) == lgrid[square_x, square_y])) collect[len_col+1] <- lgrid[square_x, square_y]
      }
      if(isTRUE(any(collect[1:3] == collect[4]))){
        if(isTRUE(AAAB_or_AABB(collect))){
          if(isTRUE(any(collect == lgrid[square_x, square_y]))) collect[len_col+1] <- lgrid[square_x, square_y]
        }
        if(isFALSE(AAAB_or_AABB(collect))) collect[len_col+1] <- lgrid[square_x, square_y]
      }
    }
  }
  return(collect)
}

collection <- c() #初始化collection
start <- c(2,2)
collection <- white_square(start, collection)
start <- c(3,3)
collection <- white_square(start, collection)
start <- c(4,4)
collection <- white_square(start, collection)
start <- c(5,5)
collection <- white_square(start, collection)
collection

#跳到绿色方块情况
green_square <- function(square_coor_g, collect_g, prob){
  square_x_g <- square_coor_g[1]
  square_y_g <- square_coor_g[2]
  elements_remove <- lgrid[square_x_g, square_y_g]
  if(isTRUE(rbinom(1, 1, prob) == 1)){
    collect_g <- c("f", "f", "h", "k")
  } else{
    if(isTRUE(any(collect_g == lgrid[square_x_g, square_y_g]))){
      collect_g <- collect_g[!(collect_g %in% elements_remove)]
    }
  }
  return(collect_g)
}
start <- c(7,3)
collection <- c("f", "e", "f", "k")
collection <- green_square(start, collection, 0.5)
collection
dist_of_num_moves <- replicate(100, green_square(start, collection, 0.2))
rbinom(1, 1, 0.1)
on_green_square <- function(square_coor){
  if(isTRUE(square_coor[1] == 6 & square_coor[2] == 2) |
     isTRUE(square_coor[1] == 7 & square_coor[2] == 3) |
     isTRUE(square_coor[1] == 2 & square_coor[2] == 6) |
     isTRUE(square_coor[1] == 3 & square_coor[2] == 7)) return(TRUE) else return(FALSE)
}
start <- c(2,7)
on_green_square(start)


#part3主程序
count_num_moves <- function(start_sq, start_col, p, to_print){
  num_moves <- 0
  if(isTRUE(to_print)) cat("Start at square", start_sq, "\t")
  if(isTRUE(to_print)) cat("Collection is empty\n")
  while(isFALSE(length(start_col) == 5)){
    if(isTRUE(on_green_square(start_sq))){
      next_col <- green_square(start_sq, start_col, p)
      if(isTRUE(to_print)) cat("Collection is [", next_col, "]\n")
    }
    if(isFALSE(on_green_square(start_sq))){
      next_col <- white_square(start_sq, start_col)
      if(isTRUE(to_print)) cat("Collection is [", next_col, "]\n")
    }
    num_moves <- num_moves + 1
    next_sq <- make_move(start_sq)
    if(isTRUE(to_print & length(next_col) != 5)) cat("move to square", next_sq, "\n")
    start_sq <- next_sq
    start_col <- next_col
  }
  return(num_moves)
}
start <- c(4,4)
collection <- c()
count_num_moves(start, collection, 0.8, T)
#part4 p约接近于1，完成游戏所需的步数（时间）越少
record_time <- function(num, p){
  t1=proc.time()
  dist_of_num_moves <- replicate(num, count_num_moves(start, collection, p, F))
  t2=proc.time()
  t = t2 - t1
  return(t[3][[1]])
}
time <- c()
vector <- seq(0, 1, by = 0.1)
for (j in vector) {
  time[j * 10 +1] <- record_time(10000, j)
}
time
plot(vector, time)
hist(dist_of_num_moves, main ="Histogram of the number of moves to complete the game", xlab = "Number of Moves" )

t3=proc.time()
dist_of_num_moves2 <- replicate(10000, count_num_moves(start, collection, 0.2, F))
t4=proc.time()
tt = t4 - t3
mean(dist_of_num_moves2)
tt
tt[3][[1]]
hist(dist_of_num_moves, main ="Histogram of the number of moves to complete the game", xlab = "Number of Moves" )

dist_of_num_moves3 <- replicate(10000, count_num_moves(start, collection, 0.1, F))
mean(dist_of_num_moves3)
median(dist_of_num_moves3)
hist(dist_of_num_moves, main ="Histogram of the number of moves to complete the game", xlab = "Number of Moves" )

dist_of_num_moves4 <- replicate(10000, count_num_moves(start, collection, 0.5, F))
mean(dist_of_num_moves4)
median(dist_of_num_moves4)

dist_of_num_moves5 <- replicate(10000, count_num_moves(start, collection, 0.95, F))
mean(dist_of_num_moves5)
median(dist_of_num_moves5)

#part5 画Cumulative Fraction Function    
startA <- c(4,4)
startB <- c(6,6)
dist_of_num_movesA <- replicate(50, count_num_moves(startA, collection, 0.95, F))
dist_of_num_movesB <- replicate(50, count_num_moves(startB, collection, 0.05, F))
movesa <- as.integer(dist_of_num_movesA)
movesb <- as.integer(dist_of_num_movesB)
kst <- ks.test(movesa, movesb) 
ks.test(dist_of_num_movesA, dist_of_num_movesB) 
hist(dist_of_num_movesA, main ="Histogram of the number of moves to complete the game", xlab = "Number of Moves" )
hist(dist_of_num_movesB, main ="Histogram of the number of moves to complete the game", xlab = "Number of Moves" )
mean(dist_of_num_movesA)
mean(dist_of_num_movesB)
dist_of_num_movesA = sort(dist_of_num_movesA)
dist_of_num_movesB = sort(dist_of_num_movesB)
sapply(movesa, max)
summary(movesa)
coef(movesa)
?coef

x <- rnorm(50)
y <- rnorm(50)

coef(dist_of_num_movesA)
y <- runif(30)
# Do x and y come from the same distribution?
ks.test(x, y)
x <- 1:5
#part6 假设检验
A <- c(25, 13, 16, 24, 11, 12, 24, 26, 15, 19, 34)
B <- c(35, 41, 23, 26, 18, 15, 33, 42, 18, 47, 21, 26)
ks.test(A, B)
