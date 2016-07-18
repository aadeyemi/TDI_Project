# load data
uber_raw_data_apr14      <- read.csv("data/uber-tlc-foil-response/uber-trip-data/uber-raw-data-apr14.csv")
uber_raw_data_janjune_15 <- read.csv("data/uber-tlc-foil-response/uber-trip-data/uber-raw-data-janjune-15.csv")

# save RData file for quick data reload at a future time
save(uber_raw_data_apr14,file="uber_raw_data_apr14.RData")
save(uber_raw_data_janjune_15,file="uber_raw_data_janjune_15.RData")

load(file="uber_raw_data_apr14.RData")
load(file="uber_raw_data_janjune_15.RData")

# convert datetime to POSIXct
t1.str <- strptime(uber_raw_data_apr14$Date.Time, "%m/%d/%Y %H:%M:%S")
t2.str <- strptime(uber_raw_data_janjune_15$Pickup_date, "%Y-%m-%d %H:%M:%S")

uber_raw_data_janjune_15$month   <- as.numeric(format(t2.str, "%m"))

uber_raw_data_apr15 <- uber_raw_data_janjune_15[uber_raw_data_janjune_15$month == 4,]

t3.str <- strptime(uber_raw_data_apr15$Pickup_date, "%Y-%m-%d %H:%M:%S")


# create data for analysis
uber_raw_data_apr14$year    <- as.numeric(format(t1.str, "%Y"))
uber_raw_data_apr14$month   <- as.numeric(format(t1.str, "%m"))
uber_raw_data_apr14$day     <- as.numeric(format(t1.str, "%d"))
uber_raw_data_apr14$hour    <- as.numeric(format(t1.str, "%H"))
uber_raw_data_apr14$weekday <- factor(format(t1.str, "%A"))
uber_raw_data_apr14$p       <- rep(1,dim(uber_raw_data_apr14)[1])

uber_raw_data_apr15$year    <- as.numeric(format(t3.str, "%Y"))
uber_raw_data_apr15$day     <- as.numeric(format(t3.str, "%d"))
uber_raw_data_apr15$hour    <- as.numeric(format(t3.str, "%H"))
uber_raw_data_apr15$weekday <- factor(format(t3.str, "%A"))
uber_raw_data_apr15$p       <- rep(1,dim(uber_raw_data_apr15)[1])

weekends2014 <- as.numeric(uber_raw_data_apr14$weekday == "Saturday" | 
                               uber_raw_data_apr14$weekday == "Sunday")
weekends2015 <- as.numeric(uber_raw_data_apr15$weekday == "Saturday" | 
                               uber_raw_data_apr15$weekday == "Sunday")

uber_raw_data_apr14$weekends <- weekends2014
uber_raw_data_apr15$weekends <- weekends2015

uber_raw_data_apr14$weekends <- factor(uber_raw_data_apr14$weekends,
                                       labels = c("Weekday","Weekend"))
uber_raw_data_apr15$weekends <- factor(uber_raw_data_apr15$weekends,
                                       labels = c("Weekday","Weekend"))


# aggregate data

df1 <- aggregate(uber_raw_data_apr14$p,
                 by = list(uber_raw_data_apr14$year,
                           uber_raw_data_apr14$month,
                           uber_raw_data_apr14$hour),
                 FUN = sum)

df2 <- aggregate(uber_raw_data_apr15$p,
                 by = list(uber_raw_data_apr15$year,
                           uber_raw_data_apr15$month,
                           uber_raw_data_apr15$hour),
                 FUN = sum)

names(df1) <- c("year","month","hour","x")
names(df2) <- c("year","month","hour","x")


# make plots

df_all <- rbind(df1,df2) 

library(ggplot2)
g <- ggplot(data=df_all,aes(x=hour,y=x,color=factor(year)))
g <- g + geom_line() + geom_point()
g <- g + labs(x="hour",y="count")
g <- g + labs(title="April hourly Uber taxi usage in NYC")
g <- g + theme(legend.title=element_blank())
plot(g)

png(filename = "april_uber_hourly_usage.png")
plot(g)
dev.off()


# plot 2
df3 <- aggregate(uber_raw_data_apr14$p,
                 by = list(uber_raw_data_apr14$year,
                           uber_raw_data_apr14$month,
                           uber_raw_data_apr14$hour,
                           uber_raw_data_apr14$weekends),
                 FUN = sum)

df4 <- aggregate(uber_raw_data_apr15$p,
                 by = list(uber_raw_data_apr15$year,
                           uber_raw_data_apr15$month,
                           uber_raw_data_apr15$hour,
                           uber_raw_data_apr15$weekends),
                 FUN = sum)

names(df3) <- c("year","month","hour","day","x")
names(df4) <- c("year","month","hour","day","x")


df_all2 <- rbind(df3[df3$day=="Weekday",],df4[df4$day=="Weekday",]) 

library(ggplot2)
g <- ggplot(data=df_all2,aes(x=hour,y=x,color=factor(year)))
g <- g + geom_line() + geom_point()
g <- g + labs(x="hour",y="count")
g <- g + labs(title="April weekday hourly Uber taxi usage in NYC")
g <- g + theme(legend.title=element_blank())
plot(g)

png(filename = "april_uber_weekday_hourly_usage.png")
plot(g)
dev.off()




df_all3 <- rbind(df3[df3$day=="Weekend",],df4[df4$day=="Weekend",]) 

library(ggplot2)
g <- ggplot(data=df_all3,aes(x=hour,y=x,color=factor(year)))
g <- g + geom_line() + geom_point()
g <- g + labs(x="hour",y="count")
g <- g + labs(title="April weekend hourly Uber taxi usage in NYC")
g <- g + theme(legend.title=element_blank())
plot(g)

png(filename = "april_uber_weekend_hourly_usage.png")
plot(g)
dev.off()

write.csv(df_all2,file="apr_weekday.csv")
write.csv(df_all3,file="apr_weekend.csv")




