#年份分类
biomarker$Year[biomarker$ID_BLAST>10000000 & biomarker$ID_BLAST<20000000]<-2017
biomarker$Year[biomarker$ID_BLAST>20000000 & biomarker$ID_BLAST<30000000]<-2018
biomarker$Year[biomarker$ID_BLAST>30000000 ]<-2019
table(biomarker$Year)
##---------------------------------------------2017年
#CEA(一倍标准)
biomarker$CEA_pos[biomarker$CEA>5 & biomarker$Year==2017]<-2
biomarker$CEA_pos[biomarker$CEA<=5 & biomarker$Year==2017]<-1
#NSE(一倍标准)
biomarker$NSE_pos[biomarker$NSE>14.3 & biomarker$Year==2017]<-2
biomarker$NSE_pos[biomarker$NSE<=14.3 & biomarker$Year==2017]<-1
#SCC(一倍标准)
biomarker$SCC_pos[biomarker$SCC>1.5 & biomarker$Year==2017]<-2
biomarker$SCC_pos[biomarker$SCC<=1.5 & biomarker$Year==2017]<-1
#Fer(一倍标准)
biomarker$Fer_pos[biomarker$Fer>200 & biomarker$Year==2017]<-2
biomarker$Fer_pos[biomarker$Fer<13 & biomarker$Year==2017]<-2
biomarker$Fer_pos[biomarker$Fer<=200 &  biomarker$Fer>=13 &biomarker$Year==2017]<-1
#CA153(一倍标准)
biomarker$CA153_pos[biomarker$CA153>35 & biomarker$Year==2017]<-2
biomarker$CA153_pos[biomarker$CA153<=35 & biomarker$Year==2017]<-1
##---------------------------------------------2018年
#CEA(超过四倍上限为阳性)
biomarker$CEA_pos[biomarker$CEA>5*4 & biomarker$Year==2018]<-2
biomarker$CEA_pos[biomarker$CEA<=5*4 & biomarker$Year==2018]<-1
#CA199(超过四倍上限为阳性)
biomarker$CA199_pos[biomarker$CA199>27*4 & biomarker$Year==2018]<-2
biomarker$CA199_pos[biomarker$CA199<=27*4 & biomarker$Year==2018]<-1
#CA125(超过四倍上限为阳性)
biomarker$CA125_pos[biomarker$CA125>35*4 & biomarker$Year==2018]<-2
biomarker$CA125_pos[biomarker$CA125<=35*4 & biomarker$Year==2018]<-1
#CA153(超过四倍上限为阳性)
biomarker$CA153_pos[biomarker$CA153>25*4 & biomarker$Year==2018]<-2
biomarker$CA153_pos[biomarker$CA153<=25*4 & biomarker$Year==2018]<-1
#AFP(超过四倍上限为阳性)
biomarker$AFP_pos[biomarker$AFP>7*4 & biomarker$Year==2018]<-2
biomarker$AFP_pos[biomarker$AFP<=7*4 & biomarker$Year==2018]<-1
##---------------------------------------------2019年
#CEA(超过四倍上限为阳性)
biomarker$CEA_pos[biomarker$CEA>5*4 & biomarker$Year==2019]<-2
biomarker$CEA_pos[biomarker$CEA<=5*4 & biomarker$Year==2019]<-1
#CA199(超过四倍上限为阳性)
biomarker$CA199_pos[biomarker$CA199>27*4 & biomarker$Year==2019]<-2
biomarker$CA199_pos[biomarker$CA199<=27*4 & biomarker$Year==2019]<-1
#CA125(超过四倍上限为阳性)
biomarker$CA125_pos[biomarker$CA125>35*4 & biomarker$Year==2019]<-2
biomarker$CA125_pos[biomarker$CA125<=35*4 & biomarker$Year==2019]<-1
#CA153(超过四倍上限为阳性)
biomarker$CA153_pos[biomarker$CA153>25*4 & biomarker$Year==2019]<-2
biomarker$CA153_pos[biomarker$CA153<=25*4 & biomarker$Year==2019]<-1
#AFP(超过四倍上限为阳性)
biomarker$AFP_pos[biomarker$AFP>7*4 & biomarker$Year==2019]<-2
biomarker$AFP_pos[biomarker$AFP<=7*4 & biomarker$Year==2019]<-1
#PG(PG1<=70 & PGR<=3  |  PG1>200)
biomarker$PG_pos[biomarker$PGI<=70 & biomarker$PG_ratio<=3 & biomarker$Year==2019]<-2
biomarker$PG_pos[biomarker$PGI==200 & biomarker$Year==2019]<-2
biomarker$PG_pos[is.na(biomarker$PG_pos) & biomarker$Year==2019]<-1
#HBsAg(阳性)
biomarker$HBsAg_pos[biomarker$HBsAg>0 & biomarker$Year==2019]<-2
biomarker$HBsAg_pos[biomarker$HBsAg==0 & biomarker$Year==2019]<-1
