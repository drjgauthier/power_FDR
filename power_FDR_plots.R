prev = seq(0,0.5,by=0.1) ; names(prev) <- prev
power = seq(0,1,by=0.01); names(power) <- power
alpha <- 0.05
grid<-expand.grid(prev=prev,power=power)
grid$fdr <- ((1-grid$prev)*0.05)/((grid$prev*grid$power)+(1-grid$prev)*0.05)
grid
x <- list(
  title = "Study Power = P(Reject H0|H1 is true)"
)
y <- list(
  title = "Assumed Probability of Positive Study in the Field of Interest")
z <- list(
  title = "False Discovery Rate")
library(plotly)
p<-plot_ly(
  x=grid$power,
  y=grid$prev,
  z=grid$fdr,
  type="contour",
  colorscale = 'Jet'
)

p %>% 
  layout(xaxis=x,yaxis=y) %>% 
  colorbar(title = "False Discovery Rate (P(False Positive Study))")

#Power calculation 
pwr.2p.test(h=0.25,n=80,sig.level=0.05,alternative="greater")
