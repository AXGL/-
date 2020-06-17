library(tidyverse) # Wickham的数据整理的整套工具
library(lubridate) # 日期和日期时间数据处理
library(quantmod)  # 金融数据的整理与作图功能
# 把一组时间序列数据读进来
d <- read_table2(
  "d-ibm-0110.txt", col_types=cols(
    .default=col_double(),
    date=col_date(format="%Y%m%d")))

#美国从1948年1月到2010年9月的经季节调整的月失业率百分数
d <- read_table2(
  "m-unrate.txt", col_types=cols(
    .default=col_double())) %>%
  mutate(date = make_date(Year, mon, dd)) %>%
  select(-Year, -mon, -dd) %>%
  select(date, everything())

# IBM和标普500从1926-01到2011-09的月度简单收益率
d <- read_table2(
  "m-ibmsp-2611.txt",
  col_types=cols(.default=col_double(),
                 date=col_date(format="%Y%m%d")))

# 读入csv
d <- read_csv(
"m-ibmsp-2611.csv",
col_types=cols(
  date=col_date(format="%Y%m%d"),
  .default=col_double()
)
)


# 生成时间序列数据，ts类型用于保存一元或者多元的等间隔时间序列， 如月度、季度、年度数据
#美国从1948年1月到2010年9月的经季节调整的月失业率百分数， 转换成ts类型
d <- read.table(
  "m-unrate.txt", header=TRUE,
  colClasses=rep("numeric", 4))
unrate <- ts(d[["rate"]], start=c(1948,1), frequency=12)

# IBM股票从2001-1-2到2010-12-31的日简单收益率， 读入数据并转换为xts类型,xts也是一种时间序列数据类型
library(xts)
d <- read.table(
  "d-ibm-0110.txt", header=TRUE,
  colClasses=c("character", "numeric"))
ibmrtn <- xts(d[["return"]], lubridate::ymd(d[["date"]]))

# 用函数ts把一个向量转换为时间序列
yd <- ts(
  c(4, 8, 7, 7, 3, 1, 8, 9, 8, 6, 3, 5,  #年度数据，start开始日期
    5, 8, 2, 5, 9, 2, 5, 2, 3, 2, 2, 4), 
  frequency=1, start=2001); yd

ym <- ts(
  c(9, 6, 3, 5, 4, 8, 2, 5, 8, 4, 6, 3,  #月度数据
    2, 2, 6, 4, 1, 4, 2, 1, 8, 9, 4, 6), 
  frequency=12, start=c(2001,1)); ym


# R中已安装的时间列示例数据有美国泛美航空公司1949-1960 的国际航班订票数的月度数据
data(AirPassengers)
attributes(AirPassengers)
AirPassengers

start(AirPassengers)  # 时间序列开始点

end(AirPassengers)    # 时间序列结束点

frequency(AirPassengers)  # 时间序列采样频率

AP.year <- aggregate(AirPassengers); AP.year  #aggregate()函数可以把月度数据加总成年数据，如果加参数FUN=mean可以取均值。

# cycle()函数对月度数据返回序列每个时间点所在的月份
cy.AP <- cycle(AirPassengers); cy.AP

# window()函数取出时间序列的一段， 如果指定frequency=TRUE还可以仅取出某个月（季度）
AP.Jan <- window(AirPassengers, start=c(1949,1), 
                 frequency=TRUE); AP.Jan

# stats::lag()可以计算滞后序列， 对ts类型输入， lag()的作用是序列数值不变， 但是时间标签增加一个单位或者用k=指定的间隔
x1 <- ts(1:5, start=2001); x1
x2 <- stats::lag(x1, k=-1); x2  #滞后一项
cbind(x1, x2)



# 生成zoo类型的时间序列
set.seed(1)
z.1 <- zoo(sample(3:6, size=12, replace=TRUE), 
           make_date(2018, 1, 1) + ddays(0:11)); z.1
set.seed(2)
z.2 <- zoo(cbind(x=sample(5:10, size=12, replace=TRUE),
                 y=sample(8:13, size=12, replace=TRUE)), 
           make_date(2018, 1, 1) + ddays(0:11)); z.2

# zoo类型时间不必是连续的，ts则是需要连续的
set.seed(1)
z.3 <- zoo(101:112, 
           make_date(2018, 1, 1) + 
             ddays(sample(0:30, 12, replace=FALSE))); z.3

# ts、irts（在tseries包中定义）、its等时间序列类型可以用as.zoo()转换为zoo类型
# str(x)显示x的类型和结构信息
str(z.2)

# 类似矩阵取值
z.2[1:3, 1:2]
z.2[1:3, 2] # 只提取第二列

# 用plot()函数对zoo类型数据做图，一元
plot(z.1) 
plot(z.3) # 序列不规则
plot(z.2) #二元
plot(z.2, plot.type="single", col=c("red", "blue")) #二元的图画在同一张画板上

# 序列合并
merge(z.2[3:7], z.3[1:4], all=FALSE)

# 从日数据转换成月数据
z.ap <- as.zooreg(AirPassengers)
z.apy <- aggregate(z.ap, year, sum) #转化为月数据
z.apy


# 把年度数据转为月度数据
z.Nile <- as.zoo(Nile)
head(z.Nile)
z.NileM <- merge(
  z.Nile,
  zoo(, seq(start(z.Nile), end(z.Nile), by=1/4))
)
head(z.NileM, 6)
head(na.locf(z.NileM))

# 如果序列是价格，用如下方法计算简单收益率与对数收益率
simple.return <- function(x) diff(x)/lag(x, k=-1)[-1]
simple.return(z.1)
log.return <- function(x) diff(log(x))
log.return(z.1)




# 金融时间序列
# 例子：可口可乐公司盈利季度数据
# 读入数据并转化为xts类型时间序列
da <- read_table(
  "q-ko-earns8309.txt",
  col_types=cols(
    .default = col_double(),
    pends = col_date("%Y%m%d"),
    anntime = col_date("%Y%m%d")
  ) )
ko.Rqtr <- xts(da[["value"]], da[["pends"]])

# 绘制时间序列图
chartSeries(
  ko.Rqtr, type="line", TA=NULL,
  major.ticks="years", minor.ticks=FALSE,
  theme="white", name="Coca Kola Quarterly Return"
)

# 用ggplot绘制
da_b <- da %>%
  mutate(time = as.yearqtr(pends),
         qtr = factor(quarter(pends),
                      levels = 1:4,
                      labels=c("Spring", "Summer", "Autumn", "Winter")))
ggplot(data = da_b, mapping = aes(
  x = time, y = value)) +
  geom_point(mapping=aes(color=qtr)) +
  geom_line()
