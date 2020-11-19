library(ggplot2)
library(graphics)
library(lattice)
dat <- ggplot2::mpg
dat <- transform(dat,
                 cyl = factor(cyl),
                 drv = factor(drv),
                 fl = factor(fl),
                 year = factor(year),
                 class = factor(class)
)


# Scatter plot
ggplot(dat) + # data
  aes(x = displ, y = hwy) # variables
ggplot(dat) + # data
  aes(x = displ, y = hwy) + # variables
  geom_point() # type of plot

#or
ggplot(mpg,aes(x = displ,y = hwy))+ 
  geom_point()


# Histogram
ggplot(dat) +
  aes(x = hwy) +
  geom_histogram(bins = sqrt(nrow(dat)))


ggplot(dat) +
  aes(x = hwy, y = ..density..,color=dat$year) +
  geom_histogram() +
  geom_density()

# Boxplot
ggplot(dat) +
  aes(x = drv, y = hwy) +
  geom_boxplot(varwidth = TRUE) + # vary boxes width according to n obs.
  geom_jitter(alpha = 0.25, width = 0.2) # adds random noise and limit its width

# Sublimation and improvement
ggplot(dat) +
  aes(x = drv, y = hwy, fill = drv) + # add color to boxes with fill
  geom_boxplot(varwidth = TRUE) + # vary boxes width according to n obs.
  geom_jitter(alpha = 0.25, width = 0.2) + # adds random noise and limit its width
  facet_wrap(~year) + # divide into 2 panels
  theme(legend.position = "none") + # remove legend
  scale_fill_manual(values = c("darkred", "darkgreen", "steelblue")) # change fill color manually



# Put it together and show it
p_a <- ggplot(dat) +
  aes(x = displ, y = hwy) +
  geom_point()
p_b <- ggplot(dat) +
  aes(x = hwy) +
  geom_histogram()
p_c <- ggplot(dat) +
  aes(x = drv, y = hwy) +
  geom_boxplot()
library(patchwork)
p_a + p_b / p_c


#Time series
dat$date <- as.Date("2020-08-21") - 0:(nrow(dat) - 1)
p <- ggplot(dat) +
  aes(x = date, y = hwy) +
  geom_line()
p


p + scale_x_date(date_labels = "%B %Y")


p + scale_x_date(date_breaks = "10 days", date_labels = "%d %b") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



# Image plot 
library(fileds)
library(spam)
library(maps)
library(RNetCDF)
library(png)
library(ggplot2)
library(knitr)

sample = cor(matrix(rnorm(400),nrow = 20))
image(cor(matrix(rnorm(400),nrow = 20)),axes = F)
mtext(text = c(paste("country",1:21)),side = 2,line = 0.3,at = seq(0,1,0.05),las = 1,cex = 0.8)
mtext(text = c(paste("country",1:21)),side = 1,line = 0.3,at = seq(0,1,0.05),las = 2,cex = 0.8)
image.plot(sample,legend.only = T)
