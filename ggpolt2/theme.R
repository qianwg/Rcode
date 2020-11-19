#base theme
theme(plot.title=element_text(hjust=0.5,face="bold"),
      axis.title=element_text(face="bold",size=10),
      axis.text=element_text(face="bold",size=9),
      panel.grid.major = element_line(colour=NA),
      panel.grid.minor = element_blank(),
      panel.background=element_rect(fill=NA))
#2. 删除X轴标签、标题、
axis.text.x=element_blank()
axis.title.x=element_blank()
#3.删除lengend
legend.position="none"
#
strip.background = element_rect(
   color = "white", fill = "white")
#分面设置
panel.spacing.y=unit(0.05, "lines")
#增加X,Y轴
axis.line = element_line(color='black')
#
legend.key=element_rect(color='white')
#X、Y轴网格线
panel.grid.major.x = element_line(color='grey')
panel.grid.major.y = element_blank()
#分面标题
strip.text=element_text(face='blod',color='red')
#
