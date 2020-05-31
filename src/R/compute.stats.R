#' compute the log-likelihood and baseline cumulative hazard
compute.stats <- function(z_hat,D_all,aggregates,betas){
  # Sum the aggregate statistics for each site
  for (k in 1:length(aggregates)) {
    if (k == 1) {
      summed_agg1 <- aggregates[[k]]$agg1
    } else {
      summed_agg1 <- summed_agg1 + aggregates[[k]]$agg1
    }
  }


  first <- z_hat %*% betas
  second <- 0
  third <- c()
  for (i in 1:length(D_all)) {
    s <- D_all[[i]] * log(summed_agg1[i ])
    second <- second + s
    third[i] <- D_all[[i]]/summed_agg1[i ]
  }
  ll <- first - second
  third <- cbind(time=as.numeric(names(D_all)),cumhaz=cumsum(third))

  return(list(loglik=ll,cumhaz=third))
}
