    ### 6.4
    # [for the sake of practice, here I use a different approach than in the previous exercise.
      # this approach is more general as it is not limited to two states as before]
      
    # random walk parameters:
    p <- .3
    q <- .3
    r <- .4
    
    p1<- .2
    q1<- .2
    r1<- .6

    p2<- .4
    q2<- .3
    r2<- .3
    
    p3<- .3
    q3<- .4
    r3<-  .3

      # set initial state by fiat
    s <- t <- u <- v<- 0
    
    # set repetitions
    N <- 500
    for (k in 2:N) {
      # roll the dice see what gets picked
      roll <- rmultinom(1,1,p=c(p,q,r))
      # make the step
      s[k][roll[1] == 1] <- s[k-1] + 1
      s[k][roll[2] == 1] <- s[k-1] - 1
      s[k][roll[3] == 1] <- s[k-1]
      # this flips a coin with probability assigned by P, based on current state
      # to determine successive state. It stores that in s vector.
      
      roll <- rmultinom(1,1,p=c(p1,q1,r1))
      t[k][roll[1] == 1] <- t[k-1] + 1
      t[k][roll[2] == 1] <- t[k-1] - 1
      t[k][roll[3] == 1] <- t[k-1]
    
      roll <- rmultinom(1,1,p=c(p2,q2,r2))
      u[k][roll[1] == 1] <- u[k-1] + 1
      u[k][roll[2] == 1] <- u[k-1] - 1
      u[k][roll[3] == 1] <- u[k-1]
      
      roll <- rmultinom(1,1,p=c(p3,q3,r3))
      v[k][roll[1] == 1] <- v[k-1] + 1
      v[k][roll[2] == 1] <- v[k-1] - 1
      v[k][roll[3] == 1] <- v[k-1]    
      }
    
    # plotting random walks:
    par(mfrow=c(2,2))
    
    plot(s, type = "l",
         main = "p=.3 q=.3, r=.4",
         xlab = "N")
    plot(t, type = "l",
         main = "p=.2, q=.2, r=.6",
         xlab = "N")
    plot(u, type = "l",
         main = "p=.4, q=.3, r=.3",
         xlab = "N")
    plot(v, type = "l",
         main = "p=.3, q=.3, r=.3",
         xlab = "N")