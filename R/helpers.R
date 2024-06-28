
# probs
inv_logit <- function(x) 1 / (1 + exp(-x))

# expect equal
is_equivalent <- function(a, b){
  all(a %in% b) & all(b %in% a)
}

# get last value
last <- function(x) x[length(x)]
