
     ### PART IV
    
    ### (1)

    # Patients arrive

    i <- 1
    p <- NA
    repeat   {    # this makes an infinite loops that breaks only when condition is met
      p[i] <- rpois(1, lambda = 10)
      i <- i +1
      if (cumsum(p)[length(p)] > 420) {break}
      }
    p <- p[-p[length(p)]] # turn back the first person who arrived late
    t.arr <- cumsum(p)    # absolute arrival times for each patient
    
    N <- length(p)  # patients for today, total
    
    
    #  each patient has her own duration for the visit
    visit <- NA
    for (i in 1:N) {
      visit[i] <- runif(1, min = 5, max = 20)
    }
    
    visit = round(visit)
    X <- cbind(t.arr,visit)
    
    
    #  time when each patient walks in with a doctor
      # number of available doctors

    t.out <- 0
    wait <- 0
    inside <- 0
    left <- 0
    X <- cbind(X,t.out,wait,inside,left)

    X <- Y <- as.data.frame(X)
    
    # loop sends a person in,
      # keeps track of all her info,
      # keeps track of first person to get out,
      # sends another person in. Repeat untile Nth patient.
  
    for (i in 1:N) {
      
      X <- X[order(-X$left,-X$inside,-X$t.out),]
      
      if (sum(X$inside)<3) {
        X[i,"t.out"] <- X[i,"t.arr"] + X[i,"visit"]
        X[i,"wait"] <- 0
        X[i,"inside"] <- 1
        next
      }
      if (sum(X$inside)==3) {
        X[i-1,"left"] <- 1
        X[i-1,"inside"] <- 0
        
        X[i,"inside"] <- 1
        X[i,"wait"] <- X[i-1,"t.arr"] + X[i-1,"visit"] + X[i-1,"wait"] - X[i,"t.arr"]
        X[i,"wait"][X[i,"wait"]<0] <- 0
        
        X[i,"t.out"] <- X[i,"t.arr"] + X[i,"visit"] + X[i,"wait"]
        
       next
      }      
      
    }
    
    X