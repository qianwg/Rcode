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