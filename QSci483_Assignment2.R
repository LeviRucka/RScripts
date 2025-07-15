### Assignment 2

data(swiss)
head(swiss)

# Outlier methods

# In this assignment, we use Cooks distance, DFFITS, Standardised residuals
# and Studentised residuals to analyse outliers in the swiss dataset

# Lets use the iris dataset to illustrate those methods

# All of the methods take a model as the input
lm3 <- lm(Agriculture ~ Education, data=swiss)

# Cooks distance
CD.swiss <- cooks.distance(lm3)

# We can then plot cooks distance 
par(mfrow=c(1,1))
plot(CD.swiss, type="h", lend=1, ylab="Cooks D", xlab="Observation number", lwd=1)

# We want to look at observations that are above 3*mean
mean.CD <- mean(CD.swiss)

# add this limit to the plot
abline(h=3*mean.CD, col=2, lty=2)

# It's a little difficult to identify which points are above using this plot alone
# We can use dynamic subsetting
# Remember the [] can be used to query data using a logical argument
# We want observations where CD.iris > 3*mean
swiss[CD.swiss > 3*mean.CD,]

# Lets now plot which points those are
cd.col <- rep(1,nrow(swiss))
cd.col[CD.swiss > 3*mean.CD] <- 2

plot(Agriculture ~ Education, data=swiss, type="p", pch=16, col=cd.col)

# We can repeat this for each of the outlier methods

# DFFITS
DFF.swiss <- dffits(lm3)
plot(DFF.swiss, type="h", lend=1, ylab="DFFITS", xlab="Observation number", lwd=1)

DFF.crit <- 2*sqrt(2/nrow(swiss))
abline(h=c(DFF.crit,-DFF.crit), col=2, lty=2)

df.col <- rep(1,nrow(swiss))
df.col[abs(DFF.swiss) > DFF.crit] <- 2

plot(Agriculture ~ Education, data=swiss, type="p", pch=16, col=df.col)

# Standardised residuals
rsta.swiss <- rstandard(lm3)
plot(rsta.swiss, type="h", lend=1, ylab="Standardised residuals", xlab="Observation number", lwd=1)

# Define an outlier as outside the 95% range of observations
qnorm(0.975)
abline(h=c(1.96,-1.96), col=2, lty=2)

rsta.col <- rep(1,nrow(swiss))
rsta.col[abs(rsta.swiss) > 1.96] <- 2

plot(Agriculture ~ Education, data=swiss, type="p", pch=16, col=rsta.col)

# Studentised residuals
rstu.swiss <- rstudent(lm3)
plot(rstu.swiss, type="h", lend=1, ylab="Studentised residuals", xlab="Observation number", lwd=1)

# Define an outlier as outside the 95% range of observations
qt(0.975, df=nrow(swiss)-2-1)
abline(h=c(1.98,-1.98), col=2, lty=2)

rstu.col <- rep(1,nrow(swiss))
rstu.col[abs(rstu.swiss) > 1.98] <- 2

plot(Agriculture ~ Education, data=swiss, type="p", pch=16, col=rstu.col)

# Plot all side-by side
par(mfrow=c(2,2))
plot(Agriculture ~ Education, data=swiss, type="p", pch=16, col=cd.col, main="Cooks")
plot(Agriculture ~ Education, data=swiss, type="p", pch=16, col=df.col, main="DFFITS")
plot(Agriculture ~ Education, data=swiss, type="p", pch=16, col=rsta.col, main="Standardised")
plot(Agriculture ~ Education, data=swiss, type="p", pch=16, col=rstu.col, main="Studentised")
