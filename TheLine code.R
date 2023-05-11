
#### code to simulate the mobility in a linear city.
#### this code was created to simulate a person living in a city with 170km long
#### the number of train stations can be adjusted and it is possible to add a second line
### walking speed is 4kmph
### train speed is 300 kmph
#### plus 20 secs per station
### distances are km, time in minutes
### expect to wait one minute 

#### function to estimate the extra cost in seconds to 
#### stop at some station
StationCost <- function(l){ ### l = dist between stations in metres
    v = 300/3.6 #### kmph divide by 3.6 so it is in seconds
    c0 = l/v
    c1 = 2*(l^(1/2)) #### accelerating to mid point and decreasing
    r = (1/2)*(v^2)#### point where accelerating speed is terminal
    c2 = 2*((2*r)^(1/2)) + (l - 2*r)/v
    cost = c1-c0
    if(r < l/2){cost = c2 -c0} #else{cost = c1 - c0}
    return((cost+15)/60)
}

ETime <- function(o, d, N){
    #### function that gets origin, destination and number of stations
    #### returns expected time
    w <- 60/4 #### walking pace
    r <- 60/300 ### train pace
    st <- seq(from = 0, to = 170, length.out= N)
    StCost <- StationCost(1000*(st[2]-st[1]))
    Dost <- min(abs(o - st))
    Ddst <- min(abs(d - st))
    EO <- which.min(abs(o - st))
    ED <- which.min(abs(d - st))
    # walk to station + train time + 30 sec delay at station + waiting train
    PTtime <- w*(Dost + Ddst) + abs(st[EO] - st[ED])*r + abs(EO - ED)*StCost + 1
    WTime <- abs(o - d)*w
    return(min(PTtime, WTime))
}

ETime2 <- function(o, d, N, M){
  #### function that gets origin, destination and number of stations 
  #### assumes multilines, so the first line has N stations and the second has M 
    #### return expected time
    u = min(c(M,N))
    v = max(c(M,N))
    w <- 60/4 #### walking pace
    r <- 60/300 ### train pace
    st <- seq(from = 0, to = 170, length.out= v)
    mst <- seq(from = 0, to = 170, length.out = u)
    StCost <- StationCost(1000*(st[2]-st[1]))
    MstCost <- StationCost(1000*(mst[2]-mst[1]))
    for(i in 1:length(mst)){
        mst[i] <- st[which.min(abs(st - mst[i]))]
    }
    Dost <- min(abs(o - st))
    Ddst <- min(abs(d - st))
    EO <- which.min(abs(o - st))
    ED <- which.min(abs(d - st))
    MO <- which.min(abs(st[EO] - mst)) ## main stations
    MD <- which.min(abs(st[ED] - mst)) ## main stations
    EOMO <- which(st == mst[MO])
    EDMD <- which(st == mst[MD])
    MSR1 <- abs(EO - EOMO) + abs(ED - EDMD)### number of stations to change line 1
    MSR2 <- abs(MO - MD)### number of stations to change line 2
    TSR <-  abs(mst[MO] - mst[MD]) + abs(st[EO]-st[EOMO]) + abs(st[ED]-st[EDMD])
    # walk to station + train time + 30 sec delay at station + waiting train
    PTtime <- w*(Dost + Ddst) + abs(st[EO] - st[ED])*r + abs(EO - ED)*StCost + 1
    # walk to station + train time + 30 sec delay at station + waiting train
    PMtime <- w*(Dost + Ddst) + TSR*r + MSR1*StCost + MSR2*MstCost + 3
    WTime <- abs(o - d)*w
    return(min(PTtime, WTime, PMtime))
}

#### run simulations
{
sims <- 15000
Res <- data.frame(ETime = c(),
                  sdTime = c(),
                  lbTime = c(),
                  ubTime = c(),
                  Above1h = c(),
                  MedianT = c(),
                  ETime2 = c(),
                  ETime3 = c(),
                  ETime4 = c(),
                  N = c())
for(k in 1:490){ #### number of stations
    TempTime <- rep(0,sims)
    TempTime2 <- rep(0,sims)
    TempTime3 <- rep(0,sims)
    TempTime4 <- rep(0,sims)
    N <- 10 + 1*k
    for(i in 1:sims){
        x <- 170*runif(1); y <- 170*runif(1)
        TempTime[i] <- ETime(x, y, N)
        TempTime2[i] <- ETime2(x, y, N, 4)  ### second line with 4 stations
        TempTime3[i] <- ETime2(x, y, N, 12) ### second line with 12 stations
        TempTime4[i] <- ETime2(x, y, N, 36) ### second line with 36 stations
        }
    Res <- rbind(Res, data.frame(ETime = mean(TempTime),
                                 sdTime = sd(TempTime),
                                 lbTime = quantile(TempTime, 0.1),
                                 ubTime = quantile(TempTime, 0.9),
                                 Above1h = mean(TempTime > 60),
                                 MedianT = median(TempTime),
                                 ETime2 = mean(TempTime2),
                                 ETime3 = mean(TempTime3),
                                 ETime4 = mean(TempTime4),
                                 N = N))
    cat(k, "\n")
}
save(Res, file = "CommuteTime20230112.RData")
}


#### run the same simulation strategy in a circle
#### radius = 3.28km
#### walking speed = 1.5 km in 15 minutes
#### so tolerance is 1.5 km
#### simulate distancess inside a circle
{
  R = 3.28
  numP <- 1800
  D <- data.frame(x = 2*R*runif(numP)-R, 
                  y = 2*R*runif(numP)-R)
  D <- D[D$x^2 + D$y^2<R,]
  DSample <- as.vector(dist(D, upper = T))
  res <- c()
  for(k in 1:1000){
  #### simualte dists inside a circle
  {
    R = 3.28
    numP <- 1800
    D <- data.frame(x = 2*R*runif(numP)-R, 
                    y = 2*R*runif(numP)-R)
    D <- D[D$x^2 + D$y^2<R,]
    DSample <- as.vector(dist(D, upper = T))
  }
  res <- c(res, mean(DSample < 1.5))
}
}
  
#### the same for the line
{
  N <- 1800
  x <- 170*runif(N)
  D <- abs(matrix(rep(x, times = N), ncol = N, byrow = T)-matrix(rep(x, times = N), ncol = N, byrow = F))
  
  res <- c()
  for(k in 1:1000){
    #### simualte dists inside a circle
    {
      x <- 170*runif(N)
      D <- abs(matrix(rep(x, times = N), ncol = N, byrow = T)-matrix(rep(x, times = N), ncol = N, byrow = F))
    }
    res <- c(res, mean(D < 1.0))
  }
  
  hist(res, n = 90)
}

#### FIGURES
#### time cost of a station as a function of l
{
SC <- c()
for(k in 1:15000){SC[k] <- StationCost(k)}
plot(1:15000, SC)
}

#### time as a function of N with interval
{
par(mar = c(3,3,.1,.1))
plot(Res$N, Res$ETime, type = "l",
     xlab = "",
     ylab = "",
     col = NA,
     xlim = c(10, 200),
     ylim = c(20, 140))
mtext(side = 1, line = 2, "Number of train stations")
mtext(side = 2, line = 2, "Commuting time")
polygon(c(Res$N, rev(Res$N)),
        c(Res$ETime-Res$sdTime, 
          rev(c(Res$ETime+Res$sdTime))),
        border = NA,
        col = "cyan1")
polygon(c(Res$N, rev(Res$N)),
        c(Res$lbTime, 
          rev(c(Res$ubTime))),
        border = NA,
        col = "cyan3")
points(Res$N, Res$ETime, type = "l",
       lwd = 5,
     col = "tomato")
for(k in 1:100){points(c(0, 3000), c(k,k)*10, 
                       col = "gray60",
                       type = "l", lty = 2)}
}

#### time as a function of N and M lines
{
par(mar = c(3,3,.1,.1))
plot(Res$N, Res$ETime, type = "l",
         xlab = "",
         ylab = "",
         col = NA,
         xlim = c(10, 200),
         ylim = c(20, 140))
mtext(side = 1, line = 2, "Number of train stations")
mtext(side = 2, line = 2, "Commuting time")
points(Res$N, Res$ETime, type = "l",
           lwd = 5,
           col = "tomato")
points(Res$N, Res$ETime2, type = "l",
           lwd = 5,
           col = "gold")
points(Res$N, Res$ETime3, type = "l",
           lwd = 5,
           col = "cyan3")
#points(Res$N, Res$ETime4, type = "l",
#           lwd = 5,
#           col = "green")
for(k in 1:100){points(c(0, 3000), c(k,k)*10, 
                       col = "gray60",
                       type = "l", lty = 2)}
text(50, 125, "Single line", col = "tomato", adj = 0)
text(50, 115, "Double line (stop every 4 stations)", col = "cyan3", adj = 0)
text(50, 105, "Double line (stop every 12 stations)", col = "gold", adj = 0)
}

#### combined two plot
#### time as a function of N with interval
{
    par(mar = 0*c(3,3,.1,.1), mfrow = c(2,1), oma = 1*c(3,3,.1,.1))
    plot(Res$N, Res$ETime, type = "l",
         xlab = "",
         xaxt = "n",
         ylab = "",
         col = NA,
         xlim = c(10, 200),
         ylim = c(20, 140))
    mtext(side = 2, line = 2, "Commuting time")
    polygon(c(Res$N, rev(Res$N)),
            c(Res$ETime-Res$sdTime, 
              rev(c(Res$ETime+Res$sdTime))),
            border = NA,
            col = "gray80")
    points(Res$N, Res$ETime, type = "l",
           lwd = 5,
           col = "tomato")
    for(k in 1:100){points(c(0, 3000), c(k,k)*20, 
                           col = "gray50",
                           type = "l", lty = 2)}

    points(c(62,62), c(0, 200), type = "l", col = 1)
    plot(Res$N, Res$ETime, type = "l",
         xlab = "",
         ylab = "",
         col = NA,
         xlim = c(10, 200),
         ylim = c(20, 140))
    mtext(side = 1, line = 2, "Number of train stations")
    mtext(side = 2, line = 2, "Commuting time")
    points(Res$N, Res$ETime, type = "l",
           lwd = 5,
           col = "tomato")
    points(Res$N, Res$ETime2, type = "l",
           lwd = 5,
           col = "gold")
    points(Res$N, Res$ETime3, type = "l",
           lwd = 5,
           col = "cyan3")
    #points(Res$N, Res$ETime4, type = "l",
    #           lwd = 5,
    #           col = "green")
    for(k in 1:100){points(c(0, 3000), c(k,k)*20, 
                           col = "gray50",
                           type = "l", lty = 2)}
    text(30, 130, "Single line", col = "tomato", adj = 0)
    text(30, 110, "Double line (stop every 4 stations)", col = "cyan3", adj = 0)
    text(30, 90, "Double line (stop every 12 stations)", col = "gold", adj = 0)
}

#### PDF time as a function of N with interval
{
    pdf("EstationTime.PDF", width = 4, height = 4)
    par(mar = 0*c(3,3,.1,.1), mfrow = c(2,1), oma = 0*c(3,3,.1,.1))
    plot(Res$N, Res$ETime, type = "l",
         xlab = "",
         xaxt = "n",
         ylab = "",
         col = NA,
         xlim = c(20, 200),
         ylim = c(20, 140))
    #mtext(side = 2, line = 2, "Commuting time")
    polygon(c(Res$N, rev(Res$N)),
            c(Res$ETime-Res$sdTime, 
              rev(c(Res$ETime+Res$sdTime))),
            border = NA,
            col = "gray80")
    points(Res$N, Res$ETime, type = "l",
           lwd = 5,
           col = "tomato")
    for(k in 1:100){points(c(0, 3000), c(k,k)*20, 
                           col = "gray50",
                           type = "l", lty = 2)}
    
    points(c(62,62), c(0, 200), type = "l", col = 1)
    plot(Res$N, Res$ETime, type = "l",
         xlab = "",
         ylab = "",
         col = NA,
         xlim = c(20, 200),
         ylim = c(20, 140))
    #mtext(side = 1, line = 2, "Number of train stations")
    #mtext(side = 2, line = 2, "Commuting time")
    points(Res$N, Res$ETime, type = "l",
           lwd = 5,
           col = "tomato")
    points(Res$N, Res$ETime2, type = "l",
           lwd = 5,
           col = "gold")
    points(Res$N, Res$ETime3, type = "l",
           lwd = 5,
           col = "cyan3")
    #points(Res$N, Res$ETime4, type = "l",
    #           lwd = 5,
    #           col = "green")
    for(k in 1:100){points(c(0, 3000), c(k,k)*20, 
                           col = "gray50",
                           type = "l", lty = 2)}
    dev.off()
}




