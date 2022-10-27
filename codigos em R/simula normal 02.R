y = c(1:1000)

  for (i in 1:1000){
    S2 = 1.1
    n=30
    x <- rnorm(n,0,S2)
    z <- rchisq(x,(n-1))
    
    mean(z)
    var(z)
    
    gS2 = log(S2)
    y[i] = sqrt(n)*(log(var(x)) - gS2)
  }


round(mean(y))
round(var(y))

