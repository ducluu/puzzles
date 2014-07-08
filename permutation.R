library("e1071")
library("plyr")

invert <- function(perm) {
  n <- perm[1]
  perm[(n+1):length(perm)] -> tail
  perm[n:1] -> head
  append(head, tail)
}


solve <- function (perm, max.iter) {
  num.iter = 0
  while ((perm[1] != 1) & (num.iter <= max.iter)) {
    invert(perm) -> perm
    num.iter + 1 -> num.iter
  }
  if (perm[1] == 1) {
    num.iter
  } else {
    cat("passed maximum number of iterations", max.iter, "\n")
    num.iter
  }
}

permutations(9) -> permutations
sample(1:nrow(permutations), 2*10^5) -> ridx
res <- ldply(ridx, function(row) {
  solve(permutations[row, ], max.iter) -> num.iter
  data.frame("idx" = row, "num.iter" = num.iter)
},.progress="time")

list(perm=permutations[res$idx[which.max(res$num.iter)], ], max.num.iter = max(res$num.iter))

                          
