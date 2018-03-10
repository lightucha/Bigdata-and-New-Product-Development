#####################################################################################
### Project : Data Cleansing
### Script : Data cleansing.R
### Description : Exercises using HBAT, AI, and MI data
#####################################################################################

#####################################################################################
### Setting up environment
#####################################################################################

# Load data
load(url("http://idisk.unist.ac.kr:8081/api.link/3d_baLILE6bCR-QL.RData")) # HBAT 
load(url("http://idisk.unist.ac.kr:8081/api.link/3d_baLILHabHReEO.RData")) # AI diff 
load(url("http://idisk.unist.ac.kr:8081/api.link/3d_baLILHKbEQecL.RData")) # MI all

# Load library  
pkgs <- c("DJL", "MASS")
sapply(pkgs, require, character.only = T)


#####################################################################################
### Mahalanobis Distance
#####################################################################################

# Mahalanobis contour map
n <- 1000
v <- matrix(c(1, .8, .8, 1), 2)
d <- mvrnorm(n, mu = c(0, 0), Sigma = v)
plot(d, xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5),
     axes = F,
     xlab = "", ylab = "",
     main = "Contour plot of the Mahalanobis distance to the origin")
box()
abline(h = 0, v = 0, lty = 3)
x <- seq(min(d[, 1]), max(d[, 1]), length = 100)
y <- seq(min(d[, 2]), max(d[, 2]), length = 100)
z <- outer(x, y, function (x, y) sqrt(apply(rbind(x, y) * solve(v, rbind(x, y)), 2, sum)))
contour(x, y, z, add = T, col = "blue", lwd = 1)

# Identify outliers
id.suspect <- dm.mahalanobis(d, p = 1)$suspect
points(d[id.suspect,], pch = 16, cex = 2, col = "red")


#####################################################################################
### Statistical Assumptions
#####################################################################################

# Normality test (Q-Q plot)
i <- 7
par(mfrow = c(1, 2))
qqnorm(hbat[, i], main = paste(hbat_nm$Name[i-1], ": Q-Q plot")); qqline(hbat[, i], col = "red")
plot(density(hbat[, i]), main = paste(hbat_nm$Name[i-1], ": Density plot"))
par(mfrow = c(1, 1))

# Homoscedasticity test (scatter plot)
par(mfrow = c(1, 2))
plot(hbat[, 19], hbat[, 10], xlab = hbat_nm$Name[18], ylab = hbat_nm$Name[9], main = "Homoscedasticity")
plot(hbat[, 16], hbat[, 20], xlab = hbat_nm$Name[15], ylab = hbat_nm$Name[19], main = "Heteroscedasticity")
par(mfrow = c(1, 1))

# Linearity test (residual plot)
fit    <- lm(V62 ~ V2 + V30 + V35, data = mi_data)
label  <- row.names(mi_data)
st.rsd <- residuals(fit)/sd(residuals(fit))
plot(fitted(fit), st.rsd, main="Std. Residual Plot",
     xlab = "Y_hat", ylab = "Std. Residual",
     ylim = c(-3, 3), col="blue", 
     pch  = 16, cex.main = 1.5, cex.lab = 1)
abline(a = 0,     b = 0, col = "red")
abline(a = -1.96, b = 0, col = "pink")
abline(a = 1.96,  b = 0, col = "pink")
text(fitted(fit), st.rsd, label, cex = 0.6, pos = 4, col = "blue")


#####################################################################################
### Data Transformation
#####################################################################################

# Generate right skewed sample
n <- 10000
x <- rnbinom(n, 10, .5)
par(mfrow = c(1, 2))
hist(x, probability = T, col = "lightblue", main = "Raw Data")

# Sqroot transformation
x_t <- x^0.5
hist(x_t, probability = T, col = "lightblue", main = "Transformed (Sqrt) Data")
par(mfrow = c(1, 1))


#####################################################################################
### Binning
#####################################################################################

# Reconfiguration using SEM
# Key performance change
bio.index <- c(1:4, 15:17)
str.index <- -3
saq.index <- c(1:9, 14, 19)
kpc       <- data.frame(BIO = diff.bio.eff[, bio.index],
                        STR = diff.str.eff[, str.index],
                        SAQ = diff.saq.eff[, saq.index])
kpc

# Std Error of Measurement
sem        <- c(1.34, 1.34, 6.4, 7.3, 14.5, 14.5, 0.0214,
                2.88, 2.88, 50, 0.11, 0.09,
                2.0, 2.7, 3.5, 2.0, 2.7, 3.5, 0.95, 0.95, 1.35, 0.13, 0.035)
names(sem) <- names(kpc)  

# How many significant changes?
# class function
class.f <- function(x, y){
  list(nochanged   = sum(which(abs(x) < 2 * y) > 0),
       increased   = sum(which(abs(x) >= 2 * y & x > 0) > 0),
       decreased   = sum(which(abs(x) >= 2 * y & x < 0) > 0),
       nochanged.r = which(abs(x) < 2 * y),
       increased.r = which(abs(x) >= 2 * y & x > 0),
       decreased.r = which(abs(x) >= 2 * y & x < 0))
}

stat <- matrix(NA, 23, 3, 
               dimnames = list(c(names(kpc)), c("Decreased", "Nochange", "Increased")))
for(i in 1:23){
  stat[i, 1] <- class.f(kpc[, i], sem[i])$decreased
  stat[i, 2] <- class.f(kpc[, i], sem[i])$nochanged
  stat[i, 3] <- class.f(kpc[, i], sem[i])$increased
}
stat
