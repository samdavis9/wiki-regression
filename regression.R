library(xts)

# Read and organize wiki data
wiki = read.table('mainpage.txt')
wiki$t = as.POSIXct(strptime(substr(wiki$V1, 12, 22), "%Y%m%d-%H"))
wikits = xts(wiki$V3, order.by=wiki$t)

# Plot November data
nov = wikits["2008-11"]
xlim = range(index(nov))
dev.new()
plot(index(nov) + 24*3600*7, nov/1000, xlim=xlim, ylim=c(0,500), 
	type='l', col='green', xlab='Date', ylab='Thousands of hits',
	main='Wikipedia main page traffic, Nov 08')
lines(index(nov), nov/1000)
legend(index(nov)[1], 100, c('Main page traffic', 'Week-delayed data'), col=c('black', 'green'), lty=1)

# Create training  and testing sets
hrs = 49:length(wikits)
wiki2 = xts(data.frame(y=wikits[hrs], x1=wikits[hrs-1],
	x2=wikits[hrs-2], x3=wikits[hrs-3], x4=wikits[hrs-24],
	x5=wikits[hrs-48]), order.by=index(wikits)[hrs])
train = wiki2["2008-11-22::2008-12-17"]
test = wiki2["2008-12:18::2008-12-31"]

# Use linear regression
mod = lm(log(y) ~ log(x1) + log(x2) + log(x3) + log(x4) + log(x5),
	data=train)
yp = exp(predict(mod, test[,2:6]))

yweekago = wiki2$y["2008-12-11::2008-12-24"]

# Plot
plot(index(test), yweekago/1000, col='green', type='l',
	ylim=c(0,450), xlab='date', ylab='Hits/1000', 
	main='Wikipedia main page traffic, Dec 2008')
legend(index(test)[1], 100, 
	c('Week-delayed data','Regression projection',
	'Actual data'), col=c('green','red','black'), lty=1)
lines(index(test), yp/1000, col='red')
lines(test$y/1000)

# Regression model for November data
test2 = wiki2["2008-11-03::2008-11-30"]
yp2 = exp(predict(mod, test2[, 2:6]))
lines(index(test2), yp2/1000, col='red')

# Read Christmas page data
xmas = read.table('christmas.txt')
xmas$t = as.POSIXct(strptime(substr(xmas$V1, 12, 22), "%Y%m%d-%H"))
xmasts = xts(xmas$V3, order.by=xmas$t)
plot(xmasts, ylab="Hits",
	main="Hits on Wikipedia's Christmas page, Nov/Dec '08")
lines(index(xmasts) + 24*7*3600, xmasts, col='green')
legend(index(xmasts)[1], 8000, 
	c('Main page traffic', 'Week-delayed data'), 
	col=c('black', 'green'), lty=1)

# Create training and testing sets
hrs = 49:length(xmasts)
xmas2 = xts(data.frame(y=xmasts[hrs], x1=xmasts[hrs-1],
	x2=xmasts[hrs-2], x3=xmasts[hrs-3], x4=xmasts[hrs-24],
	x5=xmasts[hrs-48]), order.by=index(xmasts)[hrs])
trainx = xmas2["2008-11-22::2008-12-17"]
testx = xmas2["2008-12:18::2008-12-31"]

# Linear regression
modx = lm(log(y) ~ log(x1) + log(x2) + log(x3) + log(x4) + log(x5),
	data=trainx)
ypx = exp(predict(modx, testx[,2:6]))

xmasweekago = xmas2$y["2008-12-11::2008-12-24"]

# Plot
plot(index(testx), xmasweekago, col='green', type='l',
	ylim=c(0,8000), 
	main='Wikipedia Christmas page traffic, Dec 2008')
legend(index(testx)[1], 8000, 
	c('Week-delayed data','Regression projection',
	'Actual data'), col=c('green','red','black'), lty=1)
lines(index(testx), ypx, col='red')
lines(testx$y)
