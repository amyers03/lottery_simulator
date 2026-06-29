
library(tidyverse)
library(dplyr)

###########################################
# Creating the Lottery Pool to draw from. #
###########################################

teams <- c("IND", "WAS", "BRK", "UTA", "SAC", "MEM", "DAL", "ATL", "CHI", 
           "MIL", "GSW", "OKC", "MIA", "CHA", "POR", "ORL")
teams <- tibble(teams)

teams <- teams %>%
  mutate(balls = c(2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1))

teams <- teams %>%
  mutate(order = c(1:16))

lottery_pool <- rep(teams$teams, teams$balls)

final_order <- c()

#############################################
# Creating the function for the simulation. #
#############################################

run_lottery <- function(seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  
  relegated <- c("IND", "WAS", "BRK")
  
  final_order <- character(0)
  
  #Helper for weighted draw from remaining teams
  
  draw_team <- function(pool, drafted, eligible = NULL) {
    
    available <- pool[!(pool %in% drafted)]
    
    if (!is.null(eligible)) {
      available <- available[available %in% eligible]
    }
    
    sample(available, 1)
    
  }
  
  #Draw for Pick 1 (DAL not included)
  
  pick1_pool <- lottery_pool[lottery_pool != "DAL"]
  
  final_order <- c(sample(pick1_pool, 1))
  
  #Picks 2 through 9
  
  while (length(final_order) < 9) {
    
    final_order <- c(
      final_order,
      draw_team(lottery_pool, final_order)
    )
  }
  
  #Picks 10 through 12 (assign to relegated teams who have yet to be drawn)
  
  while (length(final_order) < 12) {
    
    remaining_relegated <-
      relegated[!(relegated %in% final_order)]
    
    remaining_slots <- 12 - length(final_order)
    
    if (length(remaining_relegated) == remaining_slots) {
      draw <- draw_team(
        lottery_pool,
        final_order,
        eligible = remaining_relegated
      )
      
    } else {
      
      draw <- draw_team(
        lottery_pool,
        final_order
      )
      
    }
    
    final_order <- c(final_order, draw)
    
  }
  
  #Picks 13 through 16
  
  while (length(final_order) < 16) {
    
    draw <- draw_team(
      lottery_pool,
      final_order
    )
    
    final_order <- c(final_order, draw)
    
  }
  
  #IND Pick Protection
  
  ind_pos <- which(final_order == "IND")
  
  if (length(ind_pos) == 1 && ind_pos > 4) {
    final_order[ind_pos] <- "LAC"
  }
  
  #ATL/MIL Pick Swap
  
  atl_pos <- which(final_order == "ATL")
  mil_pos <- which(final_order == "MIL")
  
  if (length(atl_pos) == 1 && length(mil_pos) == 1) {
    
    better <- min(atl_pos, mil_pos)
    worse <- max(atl_pos, mil_pos)
    
    final_order[better] <- "ATL"
    final_order[worse] <- "MIL"
    
  }
  
  #Results
  
  data.frame(
    pick = 1:16,
    team = final_order,
    stringsAsFactors = FALSE
  )
}

run_lottery()
