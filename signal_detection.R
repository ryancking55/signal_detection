





dprime = function(hit, fa, adjustment = 0){
  
  hit = unlist_if(hit)
  fa = unlist_if(fa)
  
  if(adjustment){
    
    corrected = correct(hit, fa, adjustment)
    hit = corrected$hit
    fa = corrected$fa
    
  }
  
  return(qnorm(hit) - qnorm(fa))
  
}


aprime = function(hit, fa){
  
  hit = unlist_if(hit)
  fa = unlist_if(fa)
  
  return(.5 + sign(hit - fa)*(((hit - fa)^2 + 
                                 abs(hit - fa))/(4*max(hit, fa) - 4*hit*fa)))
}


c_val = function(hit, fa, adjustment = 0){
  
  hit = unlist_if(hit)
  fa = unlist_if(fa)
  
  if(adjustment){
    
    corrected = correct(hit, fa, adjustment)
    hit = corrected$hit
    fa = corrected$fa
    
  }
  
  return(-(qnorm(hit) + qnorm(fa))/2)
}


beta_val = function(hit, fa, adjustment = 0){
  
  hit = unlist_if(hit)
  fa = unlist_if(fa)
  
  if(adjustment){
    
    corrected = correct(hit, fa, adjustment)
    hit = corrected$hit
    fa = corrected$fa
    
  }
  
  return(exp((qnorm(fa)^2 - qnorm(hit)^2)/2))
}


correct = function(hit, fa, adjustment){
  
  if(any(hit == 0)){hit[which(hit == 0)] = hit[which(hit == 0)] + adjustment}
  if(any(hit == 1)){hit[which(hit == 1)] = hit[which(hit == 1)] - adjustment}
  if(any(fa == 0)){fa[which(fa == 0)] = fa[which(fa == 0)] + adjustment}
  if(any(fa == 1)){fa[which(fa == 1)] = fa[which(fa == 1)] - adjustment}
  
  return(list("hit" = hit, "fa" = fa))
}


sdt_list = function(hit, fa, adjustment = 0.05){
  
  corrected = correct(hit, fa, adjustment)
  hit_adj = corrected$hit
  fa_adj = corrected$fa
  
  return(list(
    c("dprime" = dprime(hit_adj, fa_adj)),
    c("aprime" = aprime(hit, fa)),
    c("beta" = beta_val(hit_adj, fa_adj)),
    c("c" = c_val(hit_adj, fa_adj))
  )
  )
}


unlist_if = function(d) {
  
  if (is.data.frame(d)) {
    
    return(unlist(d))
    
  } else {
    
    return(d)
  }
}