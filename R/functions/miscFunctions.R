# Utility function: correlation Table
# First part of extracting names seems highly inefficient
corrTable = function(x,rmethod,absolute){
  rr = psych::corr.test(x,method = rmethod)
  v1 = list();  v2 = list();  rn = row.names(rr$r); ctr= 1
  for(i in 1:(length(rn)-1)){
    for(j in (i+1):length(rn)){
      v1[ctr] = rn[i]
      v2[ctr] = rn[j]
      ctr = ctr + 1
    }
  }
  
  X = data.frame(var1 = unlist(v1),var2=unlist(v2),r= rr$r[lower.tri(rr$r)],
                 cl = rr$ci.adj$lower.adj, cu = rr$ci.adj$upper.adj,n = rr$n)
  
  if(absolute==TRUE){
    X = X %>% mutate(sign = ifelse(r<0,'neg','pos')) %>% mutate(sign = as.factor(sign))
    X = X %>% mutate(r = abs(r))
    X = X %>% mutate(cl = ifelse(cl < 0 & cu >0,cl,abs(cl)))
    X = X %>% mutate(cu = abs(cu))
  }
  # Make symmetrical
  tmp = X
  tmp$var1 = X$var2
  tmp$var2 = X$var1
  X = rbind(X,tmp)
  
  return(X)
}