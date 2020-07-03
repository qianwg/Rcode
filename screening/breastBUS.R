#读取数据
screening<-import('~/data/女性生理与生育年龄趋势.sav')
#备注：LBultraso or RBultraso 为1(未见异常)的话，肿块="无"、钙化="无"、周围组织="无"、腋窝淋巴结="正常"
#数据处理
breastBUS<-screening%>%filter(CASelf==1)%>%transmute(
  ID_BLAST=ID_BLAST,
  筛查年份=Year,
  左乳超声描述=factor(LBultraso,levels=c(1,2,3),labels=c('未见异常','增生','乳导管扩张')),
  右乳超声描述=factor(RBultraso,levels=c(1,2,3),labels=c('未见异常','增生','乳导管扩张')),
  左乳肿块=factor(ifelse(LBultraso==1 & !is.na(LBultraso),1,LBlump),levels = c(1,2),labels = c('无','有')),
  右乳肿块=factor(ifelse(RBultraso==1 & !is.na(RBultraso),1,RBlump),levels = c(1,2),labels = c('无','有')),
  左乳钙化=factor(ifelse(LBultraso==1 & !is.na(LBultraso),1,LBcalcifi),levels = c(1,2),labels=c('无','有')),
  右乳钙化=factor(ifelse(RBultraso==1 & !is.na(RBultraso),1,RBcalcifi),levels = c(1,2),labels=c('无','有')),
  左乳特殊情况=factor(ifelse(LBultraso==1 & !is.na(LBultraso),1,LBspecial),levels=c(1,2),labels=c('无','有')),
  右乳特殊情况=factor(ifelse(RBultraso==1 & !is.na(RBultraso),1,RBspecial),levels=c(1,2),labels=c('无','有')),
  左乳周围组织异常=factor(ifelse(LBultraso==1 & !is.na(LBultraso),1,LBperitiss),levels = c(1,2),labels=c('无','有')),
  右乳周围组织异常=factor(ifelse(RBultraso==1 & !is.na(RBultraso),1,RBperitiss),levels = c(1,2),labels=c('无','有')),
  左乳淋巴结肿大=factor(ifelse(LBultraso==1 & !is.na(LBultraso),1,LBaxilymno),levels = c(1,2),labels=c('无','有')),
  右乳淋巴结肿大=factor(ifelse(RBultraso==1 & !is.na(RBultraso),1,RBaxilymno),levels = c(1,2),labels=c('无','有')),
  乳腺超声描述=factor(ifelse(左乳超声描述=="未见异常" & 右乳超声描述=="未见异常",1,2),levels=c(1,2),labels=c('未见异常','增生/乳导管扩张')),
  肿块=factor(ifelse(左乳肿块=="有" | 右乳肿块=="有",1,0),levels=c(0,1),labels=c('无','有')),
  钙化=factor(ifelse(左乳钙化=="有" | 右乳钙化=="有",2,1),levels=c(1,2),labels=c('无','有')),
  特殊情况=factor(ifelse(左乳特殊情况=="有" | 右乳特殊情况=="有",2,1),levels=c(1,2),labels=c('无','有')),
  周围组织异常=factor(ifelse(左乳周围组织异常=="有" | 右乳周围组织异常=="有",2,1),levels=c(1,2),labels=c('无','有')),
  淋巴结肿大=factor(ifelse(左乳淋巴结肿大=="有" | 右乳淋巴结肿大=="有",2,1),levels=c(1,2),labels=c('无','有')),
  乳腺组织构成=factor(ultrBRTDEN,levels = c(1,2,3),labels=c('均质-脂肪','均质-纤维腺体','不均质')),
  BIRADS=case_when(
    ultrBIRADS<=3 ~ 1,
    ultrBIRADS==4 ~ 2,
    ultrBIRADS>4 ~ 3,
  ),
  BIRADS=factor(BIRADS,levels = c(1,2,3),labels=c('<=3','==4','>=5')),
  #左侧肿块数量=factor(LBLumpnum,levels=c(1,2),labels=c('单发','多发')),
#  左侧形状=factor(LBlumpshap,levels=c(1,2,3),labels=c('圆形','卵圆形','不规则')),
#  左侧边缘特征=factor(LBlumpedtp,levels=c(1,2,3,4,5),labels=c('光整','不光整','成角','微笑分叶','毛刺状')),
  
)%>%transmute(ID_BLAST=ID_BLAST,筛查年份,
              BIRADS,乳腺组织构成,左乳超声描述,右乳超声描述,乳腺超声描述,左乳肿块,右乳肿块,肿块,左乳钙化,右乳钙化,钙化,左乳周围组织异常,
              右乳周围组织异常,周围组织异常,左乳特殊情况,右乳特殊情况,特殊情况,左乳淋巴结肿大,右乳淋巴结肿大,淋巴结肿大,
              特征影像异常=case_when(
                肿块=="有" ~ 1,
                钙化=="有" ~ 1,
                周围组织异常=='有' ~ 1,
                特殊情况=='有' ~ 1,
                淋巴结肿大=="有" ~ 1
              ),
              特征影像异常=factor(ifelse(is.na(特征影像异常),0,特征影像异常),levels = c(0,1),labels=c('否','是'))
              
)
rm(screening)
