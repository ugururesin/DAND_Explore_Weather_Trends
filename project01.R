#UDACITY - DATA ANALYST NANODEGREE PROGRAM 
#PROJECT-1

#setting the working directory
setwd("/Users/ugur/Desktop/mycodes/myprojects/training/Udacity_DAND/project1/")

#data import
data_global = read.csv("data/global_data.csv")
data_city = read.csv("data/city_data.csv")
data_citynames = read.csv("data/city_list.csv")
data_istanbul = read.csv("data/istanbul.csv")
data_oslo = read.csv("data/oslo.csv")
data_sydney = read.csv("data/sydney.csv")
data_rio = read.csv("data/rio.csv")
data_toronto = read.csv("data/toronto.csv")

#initials
describe(data_global)
describe(data_city)
describe(data_citynames)
#
summary(data_global)
summary(data_city)
summary(data_citynames)

#scatter plot - average global temperature 
(plot_avglobal <- plot(data_global, type="p", col="blue",
     xlab="Year",
     ylab="Average Global Temperature (Celsius)", col.lab="black"))
#
#scatter plot - average global temperature 
(plot_avglobal2 <- plot(data_global[150:175,], type="p", col="blue",
                       xlab="Year",
                       ylab="Average Global Temperature (Celsius)", col.lab="black"))

##comparison of the average temperatures: global vs. istanbul
#setting axis limits
max_x1 <- round(max(max(data_istanbul[,2], na.rm = TRUE),
                    max(data_global$avg_temp, na.rm = TRUE)))+1
min_x1 <- round(min(min(data_istanbul[,2], na.rm = TRUE),
                    min(data_global$avg_temp, na.rm = TRUE)))-1
#plotting
(plot_avglobal <- plot(data_global, type="o", col="blue",
                       xlab="Year", ylab="Average Temperature (Celsius)",
                       col.lab="black", ylim=c(min_x1,max_x1),
                       pch=1, lty=1))
lines(data_istanbul, type="o", col="red",pch=1, lty=1)
legend('topleft',c("Global","Istanbul"), col=c("blue","red"), pch=c(1,4), lty=c(1,2))

#max-min average values
min(data_global$avg_temp[1:214], na.rm=TRUE)
max(data_global$avg_temp[1:214], na.rm=TRUE)
min(data_istanbul[1:221,2], na.rm=TRUE)
max(data_istanbul[1:221,2], na.rm=TRUE)
#
min(data_global[215:264,]$avg_temp, na.rm=T)
max(data_global[215:264,]$avg_temp, na.rm=T)
min(data_istanbul[222:271,2], na.rm=T)
max(data_istanbul[222:271,2], na.rm=T)

#moving average function
#(reference:http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/)
mav <- function(x, n=10, centered=FALSE) {
  if (centered) {
    before <- floor  ((n-1)/2)
    after  <- ceiling((n-1)/2)
  } else {
    before <- n-1
    after  <- 0
  }
  # Track the sum and count of number of non-NA items
  s     <- rep(0, length(x))
  count <- rep(0, length(x))
  # Add the centered data 
  new <- x
  # Add to count list wherever there isn't a 
  count <- count + !is.na(new)
  # Now replace NA_s with 0_s and add to total
  new[is.na(new)] <- 0
  s <- s + new
  # Add the data from before
  i <- 1
  while (i <= before) {
    # This is the vector with offset values to add
    new   <- c(rep(NA, i), x[1:(length(x)-i)])
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    i <- i+1
  }
  # Add the data from after
  i <- 1
  while (i <= after) {
    # This is the vector with offset values to add
    new   <- c(x[(i+1):length(x)], rep(NA, i))
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    i <- i+1
  }
  s/count# return sum divided by count
}
#end of the mav function

#moving averages of the data
mav_global <- cbind(data_global$year, mav(data_global$avg_temp))
mav_istanbul <- cbind(data_istanbul[,1], mav(data_istanbul[,2]))

#plotting of moving averages
max_x1 <- round(max(max(data_istanbul[,2], na.rm = TRUE),
                    max(data_global$avg_temp, na.rm = TRUE)))+2.5
min_x1 <- 6
#
(plot_avglobal <- plot(data_global, type="o", col="blue",
                       xlab="Year", ylab="Average Temperature (Celsius)",
                       col.lab="black", ylim=c(min_x1,max_x1),
                       pch=1, lty=1))
lines(mav_global, type="o", col="purple",pch=8, lty=2)
lines(data_istanbul, type="o", col="red",pch=1, lty=1)
lines(mav_istanbul, type="o", col="green",pch=8, lty=2)
legend('topleft',c("Global","Global MAv.","Istanbul","Istanbul MAv."),
       col=c("blue","purple","red","green"), pch=c(1,4), lty=c(1,2))
#
limits <- cbind(max(data_oslo[,2], na.rm=T),
           min(data_oslo[,2], na.rm=T),
           max(data_sydney[,2], na.rm=T),
           min(data_sydney[,2], na.rm=T),
           max(data_rio[,2], na.rm=T),
           min(data_rio[,2], na.rm=T),
           max(data_toronto[,2], na.rm=T),
           min(data_toronto[,2], na.rm=T))
#
mav_oslo <- cbind(data_oslo[,1], mav(data_oslo[,2]))
mav_sydney <- cbind(data_sydney[,1], mav(data_sydney[,2]))
mav_rio <- cbind(data_rio[,1], mav(data_rio[,2]))
mav_toronto <- cbind(data_toronto[,1], mav(data_toronto[,2]))

#plotting of moving averages: 5 cities & global
max_x1 <- round(max(limits))
min_x1 <- round(min(limits))+6
#
plot(mav_global, type="o", col="blue",
     xlab="Year", ylab="Average Temperature (Celsius)",
     col.lab="black", ylim=c(min_x1,max_x1),
     pch=1, lty=1)
lines(mav_istanbul, type="o", col="red",pch=1, lty=1)
lines(mav_oslo, type="o", col="yellow",pch=1, lty=1)
lines(mav_sydney, type="o", col="purple",pch=1, lty=1)
lines(mav_rio, type="o", col="green",pch=1, lty=1)
lines(mav_toronto, type="o", col="cyan",pch=1, lty=1)
legend('topleft',c("Global","Istanbul","Oslo","Sydney","Rio","Toronto"),
       col=c("blue","red","yellow","purple","green","cyan"), pch=c(1,4), lty=c(1,2))
#
m=10
plot(tail(mav_global,m), type="o", col="blue",
     xlab="Year", ylab="Average Temperature (Celsius)",
     col.lab="black", ylim=c(min_x1,max_x1),
     pch=1, lty=1)
lines(tail(mav_istanbul,m), type="o", col="red",pch=1, lty=1)
lines(tail(mav_oslo,m), type="o", col="yellow",pch=1, lty=1)
lines(tail(mav_sydney,m), type="o", col="purple",pch=1, lty=1)
lines(tail(mav_rio,m), type="o", col="green",pch=1, lty=1)
lines(tail(mav_toronto,m), type="o", col="cyan",pch=1, lty=1)
legend('topleft',c("Global","Istanbul","Oslo","Sydney","Rio","Toronto"),
       col=c("blue","red","yellow","purple","green","cyan"), pch=c(1,4), lty=c(1,2))

#correlations
data_glo_ist <- as.data.frame(cbind(data_global[1:264,2],data_istanbul[8:271,2]))
cor(data_global[1:264,2],data_istanbul[8:271,2])
plot(data_glo_ist, xlim=c(6.5,10), ylim=c(11,16), pch=8,
     xlab="Global", ylab="Istanbul", main="Average Temperatures (Celcius)")
abline(lm(V2~., data=data_glo_ist),col="red")
legend('topleft',c("Correlation: 0.7187"),
       col=c("red"), pch=8, lty=1)
#
#end of the code