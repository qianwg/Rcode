Charls<-import('~/data/CHARLS/analysis20210316/CHARLS20210316.sav')%>%
  transmute(ID,urban_nbs=factor(urban_nbs,levels=c(0,1),labels=c('Rural','Urban')),
            ragender=factor(ragender,levels=c(1,2),labels=c('Male','Female')),
            Agew1,Agew1_group=factor(ifelse(Agew1<65,1,2),levels=c(1,2),labels=c('<65','>=65')),
            rw1mnev=factor(rw1mnev,levels=c(1,2),labels=c('Married','Never Married')),
            rw1educa=factor(rw1educa,levels=c(1,2,3),labels=c('<=Primary school','Middled school','high school and above')),
            rw1educa2=factor(ifelse(rw1educa=='<=MIiddled school',1,2),levels=c(1,2),labels=c('<=MIiddled school','>=high school')),
            rw1height,rw1weight,rw1bmi,rw1bmi1_group=factor(case_when(
              rw1bmi<18.5 ~ 2, #偏瘦
              rw1bmi<24 & rw1bmi>=18.5 ~ 1,#正常
              rw1bmi<28 & rw1bmi>=24 ~ 3,#超重
              rw1bmi>=28 ~ 4#肥胖
            ),levels=seq(4),labels=c('Normal','Thin','Overweight','Obesity')),
            rw1bmi1_group2=factor(ifelse(rw1bmi<24,0,1),levels=c(0,1),labels=c('<24','>=24')),
            rw1smoke=factor(rw1smoke,levels=c(1,2,3),labels=c('Never','Former','Current')),
            rw1drinke=factor(rw1drinke,levels=c(1,2,3),labels=c('Never','Former','Current')),
            rw1hibpe=factor(ifelse(rw1hibpe<9,rw1hibpe,NA),levels=c(0,1),labels=c('No','Yes')),
            rw1dyslipe=factor(ifelse(rw1dyslipe<9,rw1dyslipe,NA),levels=c(0,1),labels=c('No','Yes')),
            rw1diabe=factor(ifelse(rw1diabe<9,rw1diabe,NA),levels=c(0,1),labels=c('No','Yes')),
            rw1kidneye=factor(ifelse(rw1kidneye<9,rw1kidneye,NA),levels=c(0,1),labels=c('No','Yes')),
            #女性初潮年龄、绝经、绝经年龄
            Menarche,AMenopause,Menopause=factor(ifelse(da027==1 & !is.na(da027) & ragender=="Female",1,0),levels = c(0,1),labels=c('No','Yes')),
            Menarche_group=factor(ifelse(Menarche<16,1,2),levels=c(1,2),labels=c('<16','>=16')),
            scales=AMenopause-Menarche,scales_group=factor(ifelse(scales<=33,1,2),levels=c(1,2),labels=c('<=33','>33')),
            #抑郁量表
            rw1depresl_scale,rw1effortl_scale,   
            rw1sleeprl_scale,rw1whappyl_scale,rw1flonel_scale,rw1goingl_scale,    
            rw1botherl_scale,rw1mindtsl_scale,rw1fhopel_scale,rw1fearll_scale,    
            rw1cesd10,depressive_symptoms=factor(depressive_symptoms,levels=c(1,2),labels=c('No symptoms','Symptoms')),
            depressive_group1=factor(depressive_group1,levels=1:5,labels=c('<=3','4-6','7-9','10-14','>=15')),
            depressive_group2=factor(depressive_group2,levels=,labels=c('<=3','4-6','7-9','9-12','>12')), 
            depressive_group3=factor(depressive_group3,levels=,labels=c('<=6','7-12','>12')),
            depressive_group4=factor(
              case_when(
                rw1cesd10<6 ~ 1,
                rw1cesd10<9 & rw1cesd10>=6 ~ 2,
                rw1cesd10<13  & rw1cesd10>=9~ 3,
                rw1cesd10>=13 ~ 4
              ),levels=c(1,2,3,4),labels=c('<6','6-8','9-12','>=13')
            ),
            #具体抑郁特征
            rw1depresl_scale2=factor(ifelse(rw1depresl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1effortl_scale2=factor(ifelse(rw1effortl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1sleeprl_scale2=factor(ifelse(rw1sleeprl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1whappyl_scale2=factor(ifelse(rw1whappyl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1flonel_scale2=factor(ifelse(rw1flonel_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1goingl_scale2=factor(ifelse(rw1goingl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1botherl_scale2=factor(ifelse(rw1botherl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1mindtsl_scale2=factor(ifelse(rw1mindtsl_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1fhopel_scale2=factor(ifelse(rw1fhopel_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            rw1fearll_scale2=factor(ifelse(rw1fearll_scale>=2,1,0),levels=c(0,1),labels=c('No','Yes')),
            
            #血检数据
            WBC=qc1_vb002,qc1_vb006,qc1_vb009,newbun,FBG=newglu,newcrea,TC=newcho,TG=newtg,             
            HDL_c=newhdl,LDL_c=newldl,Hs_CRP=newcrp,newhba1c,newua,qc1_vb005,qc1_vb004,cystatinc,
            #权重
            bloodweight,HH_weight,HH_weight_ad1,ind_weight,ind_weight_ad1,
            ind_weight_ad2,bio_weight1,bio_weight2,
            #结局
            status=preload,cancre2011,
            T2_2=T2-2011,T1_2=T1-2011,
            Time=ifelse(status==1,(T2_2+T1_2)/2,T1_2),
            #癌症类型
            Brain=factor(ifelse(is.na(Brain),1,Brain)),
            Oral=factor(ifelse(is.na(Oral),1,Oral)),Larynx=factor(ifelse(is.na(Larynx),1,Larynx)),
            Pharynx=factor(ifelse(is.na(Pharynx),1,Pharynx)),Thyroid=factor(ifelse(is.na(Thyroid),1,Thyroid)),
            Lung=factor(ifelse(is.na(Lung),1,Lung)),Breast=factor(ifelse(is.na(Breast),1,Breast)),
            Oesophagus=factor(ifelse(is.na(Oesophagus),1,Oesophagus)),Stomach=factor(ifelse(is.na(Stomach),1,Stomach)),
            Liver=factor(ifelse(is.na(Liver),1,Liver)),Pancreas=factor(ifelse(is.na(Pancreas),1,Pancreas)),
            Kidney=factor(ifelse(is.na(Kidney),1,Kidney)),Prostate=factor(ifelse(is.na(Prostate),1,Prostate)),
            Testicle=factor(ifelse(is.na(Testicle),1,Testicle)),Ovary=factor(ifelse(is.na(Ovary),1,Ovary)),
            Cervix=factor(ifelse(is.na(Cervix),1,Cervix)),Endometrium=factor(ifelse(is.na(Endometrium),1,Endometrium)),
            ColonRectum=factor(ifelse(is.na(ColonRectum),1,ColonRectum)),
            Bladder=factor(ifelse(is.na(Bladder),1,Bladder)),Skin=factor(ifelse(is.na(Skin),1,Skin)),
            Lymphoma=factor(ifelse(is.na(Lymphoma),1,Lymphoma)),Leukemia=factor(ifelse(is.na(Leukemia),1,Leukemia)),
            cancer_other=factor(ifelse(is.na(cancer_other),1,cancer_other)),
            cancer_Hemato=factor(ifelse(is.na(cancer_Hemato),1,cancer_Hemato)),
            cancer_Respir=factor(ifelse(is.na(cancer_Respir),1,cancer_Respir)),
            cancer_Gyneco=factor(ifelse(is.na(cancer_Gyneco),1,cancer_Gyneco)),
            cancer_Urologic=factor(ifelse(is.na(cancer_Urologic),1,cancer_Urologic)),
            cancer_Gastrointest=factor(ifelse(is.na(cancer_Gastrointest),1,cancer_Gastrointest)),
            cancer_Breast=factor(ifelse(is.na(cancer_Breast),1,cancer_Breast))
            
  )
