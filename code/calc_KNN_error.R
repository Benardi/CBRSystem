require(FNN)

gen_estimate <- function(indices,
                         train_resp){
  estimates <- c()
  
  for(i in seq(1,nrow(indices),1)) {
    lapply(indices[i,],
           function(x){train_resp[x,]$MSRP}) %>% 
      Reduce("+", .) %>% 
      divide_by(length(indices[i,])) -> result
    estimates <-c(estimates, result)
  }
  return(estimates)
}

calc_KNN_error <- function(k_val,
                           train_pred,
                           test_pred,
                           label,
                           train_resp,
                           test_res) {
  
  k <- FNN::knn(train_pred,
                test_pred, 
                label,
                k = k_val,
                algorithm="cover_tree")
  
  indices <- attr(k, "nn.index")
  
  estimates <- gen_estimate(indices,
                            train_resp)
  
  accum_error <- 0
  
  for(j in seq(1,length(estimates),1)) {
    disturbance <- ((estimates[j] - test_res$MSRP[j]) ^ 2 )
    accum_error <- accum_error + disturbance
  }
  return(accum_error)
}