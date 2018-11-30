## David Clancy
## 2018/11/28
## Revised: 

## Load libraries
library(parallel)

## Data reading & set up variables
cities = read.csv("./Santa/cities.csv")
primes = Primes(1, nrow(cities))
tens = seq(10, nrow(cities), by = 10)

grid.size = 15
y.region = seq(0, 3400, length = grid.size+1)
x.region = seq(0, 5100, length = grid.size+1)
cities$x.reg = sapply(cities$X, FUN = function(x) max(which(x.region <= x)))
cities$y.reg = sapply(cities$Y, FUN = function(y) max(which(y.region <= y)))
section.order = matrix(c(sort(rep(1:grid.size, grid.size)),
                         rep(c(1:grid.size,grid.size:1),grid.size/2)), ncol = 2)

initial.path = unlist(sapply(1:nrow(section.order), FUN = function(i) {
  sample(which(cities$x.reg == section.order[i,1] & cities$y.reg == section.order[i,2]))
}))

dists = unlist(mclapply(2:nrow(cities), FUN = function(i) {
  return(sqrt((cities$X[initial.path[i]] - cities$X[initial.path[i-1]])^2 +
                (cities$Y[initial.path[i]] - cities$Y[initial.path[i-1]])^2))
}))

inflate = !(initial.path[tens] %in% primes)
dists[tens[inflate]] = dists[tens[inflate]]*1.1
sum(dists)

res = lapply(1:50, FUN = function(k) {
  initial.path = unlist(sapply(1:nrow(section.order), FUN = function(i) {
    sample(which(cities$x.reg == section.order[i,1] & cities$y.reg == section.order[i,2]))
  }))
  
  dists = unlist(mclapply(2:nrow(cities), FUN = function(i) {
    return(sqrt((cities$X[initial.path[i]] - cities$X[initial.path[i-1]])^2 +
                  (cities$Y[initial.path[i]] - cities$Y[initial.path[i-1]])^2))
  }))
  
  inflate = !(initial.path[tens] %in% primes)
  dists[tens[inflate]] = dists[tens[inflate]]*1.1
  return(list(initial.path, sum(dists)))
})

scores = sapply(res, FUN = function(x) x[[2]])

current.gen = res[sort(scores, ind = TRUE)$ix[1:5]]

n.gen = 5
n.children = 25
probs = c(.36, .28, .2, .12, .04)

for(gen in 1:n.gen) {
  children = lapply(1:n.children, FUN = function(i) {
    p1 = sample(1:5, 1, prob = probs)
    p2 = sample(1:5, 1, prob = probs)
    child = unlist(current.gen[p1][[1]][1])
    prime.inds = which(child %in% primes)
    swaps = sample(1:length(child), rbinom(1, length(child) - length(prime.inds), .001))
    swaps = c(swaps,
              sample(prime.inds[prime.inds %% 10 == 0], 
                     rbinom(1, sum(prime.inds %% 10 == 0), .0001)),
              sample(prime.inds[prime.inds %% 10 != 0], 
                     rbinom(1, sum(prime.inds %% 10 != 0), .05))
    )
    swap.pos = sapply(swaps, FUN = function(ind) {
      proposed = ind + (2*(runif(1) < .5)-1)*rpois(1, 25)
      if(propsed < 1 | proposed > length(child)) {
        while(propsed < 1 | proposed > length(child)) {
          proposed = ind + (2*(runif(1) < .5)-1)*rpois(1, 25)}
        }
      return(proposed)
      })
    for(i in seq_along(swaps)) {
      child[c(swaps[i], swap.pos[i])] = child[c(swap.pos[i], swaps[i])]
    }
    dists = unlist(mclapply(2:nrow(cities), FUN = function(i) {
      return(sqrt((cities$X[child[i]] - cities$X[child[i-1]])^2 +
                    (cities$Y[child[i]] - cities$Y[child[i-1]])^2))
    }))
    
    inflate = !(initial.path[tens] %in% primes)
    dists[tens[inflate]] = dists[tens[inflate]]*1.1
    return(list(child, sum(dists)))
  })
}



