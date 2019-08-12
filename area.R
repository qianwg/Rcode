##示范区街道命名
library(rio)
screening<-import('C:/Users/dell/Desktop/PAD8月9日.xlsx')
screening2<-within(screening,{
  area<-vector()
  item<-vector()
  area[id<=31033000 & id>=31030001]<-'尖山街'
  area[id<=31063000 & id>=31060001]<-'下瓦房街'
  area[id<=31083000 & id>=31080001]<-'友谊路街'
  area[id<=31093000 & id>=31090001]<-'桃园街'
  area[id<=31073000 & id>=31070001]<-'越秀街'
  area[id<=31113000 & id>=31110001]<-'陈塘庄街'
  item[id>=31030001 & id<=31113000]<-'示范区'
  area[id<=33033000 & id>=33030001]<-'南营门街'
  area[id<=34043000 & id>=34040001]<-'万兴街(东南)'
  area[id<=34053000 & id>=34050001]<-'水上公园街'
  area[id<=35033000 & id>=35030001]<-'二号桥街'
  area[id<=35053000 & id>=35050001]<-'大王庄街'
  area[id<=32023000 & id>=32020001]<-'别山镇'
  area[id<=32043000 & id>=32040001]<-'马伸桥镇'
  item[id>=32020001 & id<=35053000]<-'早诊早治'
})
export(screening2,'C:/Users/dell/Desktop/PAD8月9日(街道赋值).xlsx')
