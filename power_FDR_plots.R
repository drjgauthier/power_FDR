prev = seq(0,1,by=0.5) ; names(prev) <- prev
power = seq(0,1,by=0.01); names(power) <- power
alpha <- 0.05
r <- as.vector((1-prev)*alpha)/((prev*power)+(1-prev)*alpha)
grid<-expand.grid(prev=prev,power=power)
grid$z <- r
grid
levelplot(z~prev*power,grid,cuts = 1000, xlab="",
          ylab="", main="FDR",
          colorkey = FALSE, region = TRUE)


fdr <- as.matrix(outer(prev,power,function(prev,power) ((1-prev)*alpha)/(prev*power+(1-prev)*alpha)))
df<-expand.grid(prev,power)
df$fdr <- ((1-prev)*0.05)/((prev*power)+(1-prev)*0.05)
dimnames(fdr) <- list(c("prev","power"))
# This forms a length(p1)*length(n) matrix of power estimates
contour(prev,power,fdr,
        xlab = "Prevalence",
        ylab = "Power",
        lty=2,
        labcex = 1)

fdr %>% pivot_longer(
  cols=0:1)
library(reshape2)
melt_df <- melt(fdr, id.vars = c("prev", "power"), measure.vars = "fdr")

mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2", "C.3")))
mdat

x <- seq(pi/4, 5 * pi, length.out = 100)
y <- seq(pi/4, 5 * pi, length.out = 100)
r <- as.vector(sqrt(outer(x^2, y^2, "+")))
grid <- expand.grid(x=x, y=y)
grid$z <- cos(r^2) * exp(-r/(pi^3))
lattice::levelplot(z~x*y, grid, cuts = 50, scales=list(log="e"), xlab="",
          ylab="", main="Weird Function", sub="with log scales",
          colorkey = FALSE, region = TRUE)

lattice::(levelplot(fdr~prev*power,))
library(lattice)
require(stats)
attach(environmental)
ozo.m <- loess((ozone^(1/3)) ~ wind * temperature * radiation,
               parametric = c("radiation", "wind"), span = 1, degree = 2)
w.marginal <- seq(min(wind), max(wind), length.out = 50)
t.marginal <- seq(min(temperature), max(temperature), length.out = 50)
r.marginal <- seq(min(radiation), max(radiation), length.out = 4)
wtr.marginal <- list(wind = w.marginal, temperature = t.marginal,
                     radiation = r.marginal)
grid <- expand.grid(wtr.marginal)
grid[, "fit"] <- c(predict(ozo.m, grid))
contourplot(fit ~ wind * temperature | radiation, data = grid,
            cuts = 10, region = TRUE,
            xlab = "Wind Speed (mph)",
            ylab = "Temperature (F)",
            main = "Cube Root Ozone (cube root ppb)")
detach()