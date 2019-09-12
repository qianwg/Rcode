##两个函数
risk_fuc<-function(x){
  x2<-factor(x,levels = c(0,1,2,3,4,5,6),labels=c('正常','超出截值1-2倍','超出截值2-3倍','超出截值3-4倍','超出截值4倍-200','200-500','>=500'))
  return(x2)
}

marker_func<-function(data){
   biomark2<-within(data,{
    CEA.risk<-vector()
    AFP.risk<-vector()
    CA199.risk<-vector()
    CA153.risk<-vector()
    CA125.risk<-vector()
    #CEA
    CEA.risk[CEA<=5]<-0
    CEA.risk[CEA>5 & CEA<=10]<-1
    CEA.risk[CEA>10 & CEA<=15]<-2
    CEA.risk[CEA>15 & CEA<=20]<-3
    CEA.risk[CEA>20 & CEA<200]<-4
    CEA.risk[CEA>=200 & CEA<500]<-5
    CEA.risk[CEA>=500]<-6
    
    #AFP
    AFP.risk[AFP<=7]<-0
    AFP.risk[AFP>7 & AFP<=14]<-1
    AFP.risk[AFP>14 & AFP<=21]<-2
    AFP.risk[AFP>21 & AFP<=28]<-3
    AFP.risk[AFP>28 & AFP<200]<-4
    AFP.risk[AFP>=200 & AFP<500]<-5
    AFP.risk[AFP>=500]<-6
    
    #CA199
    CA199.risk[CA199<=27]<-0
    CA199.risk[CA199>27 & CA199<=27*2]<-1
    CA199.risk[CA199>27*2 & CA199<=27*3]<-2
    CA199.risk[CA199>27*3 & CA199<=27*4]<-3
    CA199.risk[CA199>27*4 & CA199<200]<-4
    CA199.risk[CA199>=200 & CA199<500]<-5
    CA199.risk[CA199>=500]<-6
    
    #CA153
    CA153.risk[CA153<=30]<-0
    CA153.risk[CA153>30 & CA153<=60]<-1
    CA153.risk[CA153>60 & CA153<=90]<-2
    CA153.risk[CA153>90 & CA153<=120]<-3
    CA153.risk[CA153>120 & CA153<200]<-4
    CA153.risk[CA153>=200 & CA153<500]<-5
    CA153.risk[CA153>=500]<-6
    #CA125
    CA125.risk[CA125<=35]<-0
    CA125.risk[CA125>35 & CA125<=70]<-1
    CA125.risk[CA125>70 & CA125<=105]<-2
    CA125.risk[CA125>105 & CA125<=140]<-3
    CA125.risk[CA125>140 & CA125<200]<-4
    CA125.risk[CA125>=200 & CA125<500]<-5
    CA125.risk[CA125>=500]<-6
   }
   )
}