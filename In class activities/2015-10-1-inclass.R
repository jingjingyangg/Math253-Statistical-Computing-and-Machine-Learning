#Jingjing Yang
# Oct 1, 2015 in class poker

poker_deck <- c(outer((10*2:14), 1:4, "+"))

suit <- function(card){
  suit = card %% 10
  suit
} 

rank <- function(card){
  rank = card %/% 10
  rank
}


is_flush <- function(deck){
  deck = sort(deck)
  suit <- suit(deck)
  if (sd(suit) == 0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_straight_flush <- function(deck){
  deck = sort(deck)
  suit <- suit(deck)
  rank <- rank(deck)
  if (is_flush(deck) == TRUE){
    if(rank[5] - rank[1] == length(deck) - 1){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

is_loyal_flush <- function(deck){
  deck = sort(deck)
  suit <- suit(deck)
  rank <- rank(deck)
  if (is_flush(deck) == TRUE){
    if(max(rank)==14 && min(rank)==10){
      return(TRUE)
    } else {
      return(FALSE)
    } 
  } else {
    return(FALSE)
  }
}

is_full_house <- function(deck){
  suit <- suit(deck)
  rank <- rank(deck)
  counts <- table(rank(deck))
  if (min(counts) == 2 && max(counts) == 3){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
    
is_four_of_a_kind <- function(deck){
  suit <- suit(deck)
  rank <- rank(deck)
  counts <- table(rank(deck))
  if (min(counts) == 1 && max(counts) == 4){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_straight <- function(deck){
  deck = sort(deck)
  suit <- suit(deck)
  rank <- rank(deck)
  if(rank[5] - rank[1] == length(deck) - 1){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_three_of_a_kind <- function(deck){
  suit <- suit(deck)
  rank <- rank(deck)
  counts <- table(rank(deck))
  if(length(unique(rank(deck))) == 3){
    if (min(counts) == 1 && max(counts) == 3){
      return(TRUE)
    } else {
      return(FALSE)
    }}
  else{
    return(FALSE)
  }
}

is_two_pair <- function(deck){
  suit <- suit(deck)
  rank <- rank(deck)
  counts <- table(rank(deck))
  if(length(unique(rank(deck))) == 3){
    if (min(counts) == 1 && max(counts) == 2){
      return(TRUE)
    } else {
      return(FALSE)
    }}
  else{
    return(FALSE)
  }
}

is_one_pair <- function(deck){
  suit <- suit(deck)
  rank <- rank(deck)
  counts <- table(rank(deck))
  if(length(unique(rank(deck))) == 4){
    if (min(counts) == 1 && max(counts) == 2){
      return(TRUE)
    } else {
      return(FALSE)
    }}
  else{
    return(FALSE)
  }
}


before_draw <- function(x){
  five_cards <- sample(poker_deck, 5)
  if(is_loyal_flush(five_cards) == TRUE){
    return("loyal flush")
  } else if (is_straight_flush(five_cards) == TRUE){
    return("straight flush")
  } else if (is_four_of_a_kind(five_cards) == TRUE){
    return("four of a kind")
  } else if (is_full_house(five_cards) == TRUE){
    return("full house")
  } else if (is_flush(five_cards) == TRUE){
    return("flush")
  } else if (is_straight(five_cards) == TRUE){
    return("straight")
  } else if (is_three_of_a_kind(five_cards) == TRUE){
    return("three of a kind")
  } else if (is_two_pair(five_cards) == TRUE){
    return("two pairs")
  } else if (is_one_pair(five_cards) == TRUE){
    return("one pair")
  } else {
    return("no pair")
  }
}

table(sapply(1:1e+05, FUN = before_draw))

#test
require(scoreActivity, quietly = TRUE )
score253(day = 9)
