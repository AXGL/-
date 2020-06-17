# 3M公司股票从1946年2月到2008年12月的月对数收益率， 共有755个观测
d <- read_table2(
  "m-3m4608.txt",
  col_types=cols(.default=col_double(),
                 date=col_date(format="%Y%m%d")))
mmm <- xts(log(1 + d[["rtn"]]), d$date)
rm(d)
tclass(mmm) <- "yearmon"
ts.3m <- ts(coredata(mmm), start=c(1946,2), frequency=12)
head(ts.3m)

# 3M公司月对数收益率
plot(ts.3m, main="3M Monthly Log Return")
abline(h = 0, col = "gray")

# 3M公司月对数收益率的ACF，ACF很接近于白噪声
acf(ts.3m, main="")

# 3M公司月对数收益率的PACF，PACF也比较接近于白噪声但是有比较多的超出界限的值， 尽管超出量不大
pacf(ts.3m, main="")

# 用TSA包提供的eacf()函数辨识模型
TSA::eacf(ts.3m, 6, 12)


# TSA包还提供了armasubsets()函数用来选择ARMA模型阶， 办法是用长阶自回归获得新息的估计， 然后用普通最小二乘估计ARMA系数， 用回归的自变量选择方法进行模型选择。 如：
resr <- TSA::armasubsets(ts.3m, nar = 6, nma = 12)
plot(resr)

# 用forecasts包的auto.arima()函数定阶
forecast::auto.arima(ts.3m, max.p = 6, max.q = 6, max.P = 1, max.Q = 1)

