
#라이브러리_실행#
library(devtools)
devtools::install_github("tarakc02/puzzlr")
library(puzzlr)
library(tidyr)
devtools::install_github("tarakc02/lazylist")
library(lazylist)

#퍼즐생성_및_기본기능_실행#
set.seed(9803808)
p <- random_puzzle(size = 3)
plot(p)
neighbors(p)
moved_once <- move(p, source = c(1, 1), dest = c(2, 1))
replay_moves(moved_once)

#최단거리_계산_함수_생성#
all_games_from <- function(pz, weight_function) {
  possible_next_moves <- puzzlr::neighbors(pz)
  
  possible_moves <- purrr::map(
    possible_next_moves,
    function(x) cons_stream(x, all_games_from(x, weight_function) )
  )
  
  purrr::reduce(possible_moves,
                merge_weighted,
                weight = weight_function)
}

#맨하탄거리_계산_및_적용#
mh_cost <- function(x) moves(x) + manhattan(x)
all_games <- all_games_from(p, weight_function = mh_cost)

is_solution <- function(x) manhattan(x) == 0
solutions <- stream_filter(all_games, is_solution)

#최소_숫자_계산#
puzzlr::moves(solutions[1])

#평가#
solution_index <- stream_which(all_games, is_solution)
solution_index[1]
