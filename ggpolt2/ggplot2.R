library(ggplot2)
##vignette
vignette(package='ggplot2')
vignette(package='ggplot2','ggplot2-specs')
vignette(package='ggplot2','extending-ggplot2')
vignette(package='ggplot2','profiling')
#------------------------------stat_function--------------------------------------------
#1正态曲线
m<-data.frame(a=rnorm(1000))
ggplot(data=m,aes(x=a))+geom_histogram(aes(y=..density..),fill='lightblue',bins=40,color='black')+
  stat_function(fun=dnorm,args=list(mean=mean(m$a),sd=sd(m$a)),color='red')+theme_classic()
#2
ggplot(data=m,aes(x=a))+geom_histogram(aes(y=..density..),fill='lightblue',bins=40,color='black')+
  stat_function(fun=dnorm,args=list(mean=mean(m$a),sd=sd(m$a)),color='red',geom='point')+theme_classic()
#3
stat_overlay_normal_density(color = "red", linetype = "dashed")

##stat_function更多是用来在原有图像的基础上画线
#--------------------------------------------base theme---------------------------------------------------
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V-shaped", "Straight"))
  am <- factor(am, labels = c("Automatic", "Manual"))
  cyl  <- factor(cyl)
  gear <- factor(gear)
})
p1<-ggplot(mtcars2) +
  geom_point(aes(x = wt, y = mpg, colour = gear)) +
  labs(title = "Fuel economy declines as weight increases",
       subtitle = "(1973-74)",
       caption = "Data from the 1974 Motor Trend US magazine.",
       tag = "Figure 1",
       x = "Weight (1000 lbs)",
       y = "Fuel economy (mpg)",
       colour = "Gears")
p1 + theme_gray() # the default
p1 + theme_bw()
p1 + theme_linedraw()
p1 + theme_light()
p1 + theme_dark()
p1 + theme_minimal()
p1 + theme_classic()
p1 + theme_void()
#--------------------------------------------------------------------------------------------
#散点图+边际直方图
library(ggExtra)
p<-gggplot(aes(x=,y=))+geom_point()
ggMarginal(p,type='histogram')








