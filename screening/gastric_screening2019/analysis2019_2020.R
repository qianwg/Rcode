source('~/Rcode/statistics/Table1.R')
source('~/Rcode/statistics/OR.R')
variables<-c("胃癌家族史","性别",'年龄分组5',"就业状况2","家庭收入2","教育",        
             "血型1", "运动", "BMI_group2",  "吸烟1",  
             "被动吸烟1","饮酒",   
             "喝茶",    "鲜奶",    "酸奶",    "咖啡","碳酸饮料",        
             "果味饮料","蔬菜",    "水果",    "谷类",   
             "鸡蛋",    "杂粮",    "豆类",     "坚果",   
             "大蒜",    "菌类",    "油炸",   
             "烧烤",    "熏制",    "酱制",    "偏咸",    "腌制",   
             "偏辣",    "偏烫",    "偏酸",    "偏甜",   
             "偏硬",     "糖尿病",  "高血压",  "高血脂", 
             "冠心病","重度精神问题")


#########以PG定义的轻重度胃萎缩

make.table(dat=pepsinogen2019_2020,
           strat        = "PG_pos4",
           cat.rmstat   = c("col"),
           cat.varlist  = c("包年分组",variables),
           cat.ptype    = c("chisq"),
           output       = "html")
##Normal vs Mild
make.table(dat=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'),
           strat        = "PG_pos",
           cat.rmstat   = c("col"),
           cat.varlist  = c("包年分组",variables),
           cat.ptype    = c("chisq"),
           output       = "html")
##Normal vs Serve
make.table(dat=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'),
           strat        = "PG_pos",
           cat.rmstat   = c("col"),
           cat.varlist  = c("包年分组",variables),
           cat.ptype    = c("chisq"),
           output       = "html")

####Normal vs Mild
logit(x=c('教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('血型2','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('运动','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('被动吸烟1','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('喝茶','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('鲜奶','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('碳酸饮料','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('果味饮料','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('酸奶','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('咖啡','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('蔬菜','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('水果','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('谷类','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('鸡蛋','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('杂粮','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('豆类','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('坚果','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('大蒜','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('菌类','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('油炸','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('烧烤','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('熏制','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('酱制','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('偏咸','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('腌制','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('偏辣','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('偏烫','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('偏酸','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('偏甜','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
logit(x=c('偏硬','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='重度萎缩'))
###重度萎缩性胃炎
logit(x=c('喝茶','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('鲜奶','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('碳酸饮料','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('果味饮料','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('酸奶','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('咖啡','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('蔬菜','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('水果','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('谷类','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('鸡蛋','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('杂粮','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('豆类','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('坚果','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('大蒜','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('菌类','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('油炸','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('烧烤','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('熏制','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('酱制','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('偏咸','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('腌制','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('偏辣','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('偏烫','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('偏酸','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('偏甜','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))
logit(x=c('偏硬','教育','性别','年龄分组5','就业状况2','家庭收入2','吸烟1','饮酒','糖尿病'),y='PG_pos',data=subset(pepsinogen2019_2020,PG_pos4!='一般萎缩'))



##分为中-青年和老年人

#中青年者
#人口学特征
model1<-glm(PG_pos~性别+教育+家庭收入2+吸烟2+饮酒+BMI_group,data=subset(pepsinogen2019_2020,年龄<60),family='binomial')
summary(model1)
#饮食相关因素
logit(x=c('喝茶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group','就业状况2'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('鲜奶','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('酸奶','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('咖啡','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('碳酸饮料','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('蔬菜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('水果','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('谷类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('鸡蛋','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('杂粮','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('豆类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('肉类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('坚果','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('薯类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('大蒜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('菌类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('油炸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('烧烤','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
#饮食习惯
logit(x=c('熏制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('酱制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('腌制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('晒制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
##饮食偏好
logit(x=c('偏咸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('偏辣','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('偏烫','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('偏甜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('偏硬','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
#生活习惯
logit(x=c('每天早餐','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('准点吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('睡眠时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('睡眠质量','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('吃饭速度','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('外出吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('静态时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))
logit(x=c('夜班','性别','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60))

###将胃萎缩分为一般萎缩和重度萎缩
logit(x=c('喝茶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('喝茶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('鲜奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('鲜奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('酸奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('酸奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('咖啡','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('咖啡','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('碳酸饮料','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('碳酸饮料','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('蔬菜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('蔬菜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('水果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('水果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('谷类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('谷类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('鸡蛋','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('鸡蛋','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('杂粮','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('杂粮','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('豆类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('豆类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('肉类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('肉类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('坚果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('坚果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('薯类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('薯类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('大蒜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('大蒜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('菌类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('菌类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('油炸','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('油炸','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('烧烤','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('烧烤','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('熏制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('熏制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('酱制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('酱制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('腌制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('腌制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


logit(x=c('晒制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('晒制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

#饮食偏好
logit(x=c('偏咸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('偏咸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('偏辣','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('偏辣','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('偏烫','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('偏烫','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('偏甜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('偏甜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('偏硬','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('偏硬','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

#生活习惯
logit(x=c('每天早餐','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('每天早餐','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('准点吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('准点吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('睡眠时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('睡眠时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('睡眠质量','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('睡眠质量','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('吃饭速度','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('吃饭速度','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('外出吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('外出吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('静态时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('静态时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))

logit(x=c('夜班','性别','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='重度萎缩'))
logit(x=c('夜班','性别','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))


#####老年人

#饮食相关因素
logit(x=c('喝茶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('鲜奶','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('酸奶','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('咖啡','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('碳酸饮料','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('蔬菜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('水果','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('谷类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('鸡蛋','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('杂粮','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('豆类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('肉类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('坚果','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('薯类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('大蒜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('菌类','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('油炸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('烧烤','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
#饮食习惯
logit(x=c('熏制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('酱制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('腌制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('晒制','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
##饮食偏好
logit(x=c('偏咸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('偏辣','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('偏烫','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('偏甜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('偏硬','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
#生活习惯
logit(x=c('每天早餐','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('准点吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('睡眠时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('睡眠质量','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('吃饭速度','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('外出吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('静态时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))
logit(x=c('夜班','性别','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60))

###将胃萎缩分为一般萎缩和重度萎缩
make.table(dat=pepsinogen2019_2020,
           strat        = "PG_pos10",
           cat.rmstat   = c("row"),
           cat.varlist  = c("包年分组",variables),
           cat.ptype    = c("chisq"),
           output       = "html")
logit(x=c('喝茶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('喝茶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('鲜奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('鲜奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('酸奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('酸奶','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('咖啡','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('咖啡','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('碳酸饮料','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('碳酸饮料','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('蔬菜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('蔬菜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('水果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('水果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('谷类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('谷类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('鸡蛋','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('鸡蛋','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('杂粮','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('杂粮','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('豆类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('豆类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('肉类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('肉类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('坚果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('坚果','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('薯类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('薯类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('大蒜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('大蒜','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('菌类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('菌类','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('油炸','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('油炸','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('烧烤','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('烧烤','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('熏制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('熏制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('酱制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('酱制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('腌制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('腌制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


logit(x=c('晒制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('晒制','性别','教育','家庭收入2','吸烟2','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

#饮食偏好
logit(x=c('偏咸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('偏咸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('偏辣','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('偏辣','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('偏酸','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('偏烫','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('偏烫','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('偏甜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('偏甜','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('偏硬','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄<60 & PG_pos4!='一般萎缩'))
logit(x=c('偏硬','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

#生活习惯
logit(x=c('每天早餐','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('每天早餐','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('准点吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('准点吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('睡眠时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('睡眠时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('睡眠质量','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('睡眠质量','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('吃饭速度','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('吃饭速度','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('外出吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('外出吃饭','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('静态时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('静态时间','性别','教育','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))

logit(x=c('夜班','性别','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='重度萎缩'))
logit(x=c('夜班','性别','家庭收入2','吸烟1','饮酒','BMI_group'),y='PG_pos',data=subset(pepsinogen2019_2020,年龄>=60 & PG_pos4!='一般萎缩'))


#######对女性生理生育与胃萎缩的相关性研究<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

###女性人群中的分布
#基线分布
make.table(dat=subset(pepsinogen2019_2020,性别=='Female'),
           strat        = "PG_pos",
           cat.varlist  = c('胃癌家族史','就业状况2','家庭收入2','教育','BMI_group2','BMI_group','吸烟1','年龄分组','年龄分组2',
                            '饮酒','喝茶','鲜奶','碳酸饮料','果味饮料','酸奶','咖啡','蔬菜','水果',
                            '谷类','鸡蛋','杂粮','豆类','坚果','大蒜','菌类','油炸','烧烤','熏制','酱制','偏咸','腌制','偏辣','偏烫','偏酸','偏甜','偏硬'),
           cat.rmstat   = c("col"),
           cat.ptype    = c("chisq"),
           output       = "html")

##生理生育因素
make.table(dat=subset(pepsinogen2019_2020,性别=='Female'),
           strat        = "PG1_range4",
           cat.varlist  = c("初潮年龄分组",'生育','生育次数分组','首次生育年龄分组','绝经年龄分组',
                            '绝经','口服避孕药','雌激素代替治疗','子宫摘除术','卵巢摘除术','妇科手术史',
                            '雌激素影响时间分组'),
           cat.rmstat   = c("col"),
           cat.ptype    = c("chisq"),
           output       = "html")




make.table(dat=subset(pepsinogen2019_2020,性别=='Female'),
           strat        = "PG_pos",
           cat.varlist  = c("初潮年龄分组",'生育','生育次数分组','首次生育年龄分组','绝经年龄分组',
                            '绝经','口服避孕药','雌激素代替治疗','妇科手术史',
                            '雌激素影响时间分组'),
           cat.rmstat   = c("col"),
           cat.ptype    = c("chisq"),
           output       = "html")


##多因素矫正
#model1
logit(x=c('绝经','年龄'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('生育','年龄'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('初潮年龄分组','年龄'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('首次生育年龄分组','年龄'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('绝经年龄分组','年龄'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('口服避孕药','年龄'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('雌激素代替治疗','年龄'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('雌激素影响时间分组','年龄','BMI','教育','吸烟1','饮酒','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('妇科手术史','年龄'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))

#model2
logit(x=c('初潮年龄分组','年龄','BMI_group','教育','油炸','咖啡','就业状况2'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('首次生育年龄分组','年龄','BMI_group','教育','油炸','咖啡','就业状况2'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('绝经','年龄','BMI_group','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('绝经年龄分组','年龄','BMI_group','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('口服避孕药','年龄','BMI_group','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('雌激素代替治疗','年龄','BMI_group','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('雌激素影响时间分组','年龄','BMI','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('妇科手术史','年龄','BMI_group','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))

###将萎缩分为一般萎缩和重度萎缩
make.table(dat=subset(pepsinogen2019_2020,性别=='Female'),
           strat        = "PG_pos4",
           cat.varlist  = c("初潮年龄分组",'生育','生育次数分组','首次生育年龄分组','绝经年龄分组',
                            '绝经','口服避孕药','雌激素代替治疗','妇科手术史',
                            '雌激素影响时间分组'),
           cat.rmstat   = c("row"),
           cat.ptype    = c("chisq"),
           output       = "html")



#model2
logit(x=c('初潮年龄分组','年龄','BMI','教育','油炸','咖啡','就业状况2'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='重度萎缩'))
logit(x=c('初潮年龄分组','年龄','BMI','教育','油炸','咖啡','就业状况2'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='一般萎缩'))

logit(x=c('首次生育年龄分组','年龄','BMI','教育','油炸','咖啡','就业状况2'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='重度萎缩'))
logit(x=c('首次生育年龄分组','年龄','BMI','教育','油炸','咖啡','就业状况2'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='一般萎缩'))

logit(x=c('绝经','年龄','BMI_group','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='重度萎缩'))
logit(x=c('绝经','年龄','BMI_group','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='一般萎缩'))

logit(x=c('绝经年龄分组','年龄','BMI','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='重度萎缩'))
logit(x=c('绝经年龄分组','年龄','BMI','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='一般萎缩'))

logit(x=c('口服避孕药','年龄','BMI','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='重度萎缩'))
logit(x=c('口服避孕药','年龄','BMI','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='一般萎缩'))

logit(x=c('雌激素代替治疗','年龄','BMI','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='重度萎缩'))
logit(x=c('雌激素代替治疗','年龄','BMI','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='一般萎缩'))

logit(x=c('雌激素影响时间分组','年龄','BMI','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='重度萎缩'))
logit(x=c('雌激素影响时间分组','年龄','BMI','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='一般萎缩'))

logit(x=c('妇科手术史','年龄','BMI','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='重度萎缩'))
logit(x=c('妇科手术史','年龄','BMI','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female' & PG_pos4!='一般萎缩'))



##2020幽门螺旋杆菌
make.table(dat=subset(pepsinogen2020,性别=='Female'),
           strat        = "PG_pos",
           cat.varlist  = c("初潮年龄分组",'生育','生育次数分组','首次生育年龄分组','绝经年龄分组',
                            '绝经','口服避孕药','雌激素代替治疗','妇科手术史',
                            '雌激素影响时间分组'),
           cat.rmstat   = c("col"),
           cat.ptype    = c("chisq"),
           output       = "html")


##多因素矫正
#model1
logit(x=c('绝经','年龄'),y='PG_pos',data=subset(pepsinogen2020,性别=='Female' & Hp_pos!='阴性'))
logit(x=c('生育','年龄'),y='PG_pos',data=subset(pepsinogen2020,性别=='Female' & Hp_pos!='阴性'))
logit(x=c('初潮年龄分组','年龄'),y='PG_pos',data=subset(pepsinogen2020,性别=='Female' & Hp_pos!='阴性'))
logit(x=c('首次生育年龄分组','年龄'),y='PG_pos',data=subset(pepsinogen2020,性别=='Female' & Hp_pos!='阴性'))
logit(x=c('绝经年龄分组','年龄'),y='PG_pos',data=subset(pepsinogen2020,性别=='Female' & Hp_pos!='阴性'))
logit(x=c('口服避孕药','年龄'),y='PG_pos',data=subset(pepsinogen2020,性别=='Female' & Hp_pos!='阴性'))
logit(x=c('雌激素代替治疗','年龄'),y='PG_pos',data=subset(pepsinogen2020,性别=='Female' & Hp_pos!='阴性'))
logit(x=c('雌激素影响时间分组','年龄','BMI','教育','吸烟1','饮酒','咖啡'),y='PG_pos',data=subset(pepsinogen2020,性别=='Female' & Hp_pos!='阴性'))
logit(x=c('妇科手术史','年龄'),y='PG_pos',data=subset(pepsinogen2020,性别=='Female' & Hp_pos!='阴性'))

#model2
logit(x=c('初潮年龄分组','年龄','BMI_group','教育','油炸','咖啡','就业状况2'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('首次生育年龄分组','年龄','BMI_group','教育','油炸','咖啡','就业状况2'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('绝经','年龄','BMI_group','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('绝经年龄分组','年龄','BMI_group','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('口服避孕药','年龄','BMI_group','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('雌激素代替治疗','年龄','BMI_group','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('雌激素影响时间分组','年龄','BMI','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))
logit(x=c('妇科手术史','年龄','BMI_group','教育','油炸','咖啡'),y='PG_pos',data=subset(pepsinogen2019_2020,性别=='Female'))




















####BMI与胃萎缩的相关性分析
###基本分布情况
table1(~年龄+性别+吸烟1+家庭收入2+PG1+PG2+PGR+PG_pos+咖啡+饮酒+PG1_range4+PG_pos4+PG_pos5+PG_pos9 | BMI_group4,data=pepsinogen2019_2020,render.categorical=my.render.cat)
##对于总体
######low:PGI≤70 & PGII≤3
#model1:
model1<-glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+年龄,data=pepsinogen2019_2020,family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=pepsinogen2019_2020,family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤50 & PGII≤3
#model1:
model1<-glm(PG_pos1~relevel(BMI_group2,ref='正常')+性别+年龄,data=pepsinogen2019_2020,family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos1~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=pepsinogen2019_2020,family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤30 & PGII≤2
model1<-glm(PG_pos2~relevel(BMI_group2,ref='正常')+性别+年龄,data=pepsinogen2019_2020,family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos2~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=pepsinogen2019_2020,family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤70
#model1:
model1<-glm(PG_pos7~relevel(BMI_group2,ref='正常')+性别+年龄,data=pepsinogen2019_2020,family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos7~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=pepsinogen2019_2020,family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI<30
#model1:
model1<-glm(PG_pos6~relevel(BMI_group2,ref='正常')+性别+年龄,data=pepsinogen2019_2020,family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos6~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=pepsinogen2019_2020,family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))

######low:PGR≤3
#model1:
model1<-glm(PG_pos11~relevel(BMI_group2,ref='正常')+性别+年龄,data=pepsinogen2019_2020,family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos11~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=pepsinogen2019_2020,family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))


######中-青年人

######low:PGI≤70 & PGII≤3
#model1:
model1<-glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen2019_2020,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,subset(pepsinogen2019_2020,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤50 & PGII≤3
#model1:
model1<-glm(PG_pos1~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen2019_2020,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos1~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen2019_2020,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤30 & PGII≤2
model1<-glm(PG_pos2~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen2019_2020,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos2~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen2019_2020,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤70
#model1:
model1<-glm(PG_pos7~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen2019_2020,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos7~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen2019_2020,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI<30
#model1:
model1<-glm(PG_pos6~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen2019_2020,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos6~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen2019_2020,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))

######low:PGR≤3
#model1:
model1<-glm(PG_pos11~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen2019_2020,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos11~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen2019_2020,年龄<60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))


#######老年人

######low:PGI≤70 & PGII≤3
#model1:
model1<-glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen2019_2020,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen2019_2020,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤50 & PGII≤3
#model1:
model1<-glm(PG_pos1~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen2019_2020,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos1~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen2019_2020,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤30 & PGII≤2
model1<-glm(PG_pos2~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen2019_2020,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos2~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen2019_2020,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI≤70
#model1:
model1<-glm(PG_pos7~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen2019_2020,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos7~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen2019_2020,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))
######low:PGI<30
#model1:
model1<-glm(PG_pos6~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen2019_2020,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos6~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen2019_2020,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))

######low:PGR≤3
#model1:
model1<-glm(PG_pos11~relevel(BMI_group2,ref='正常')+性别+年龄,data=subset(pepsinogen2019_2020,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model1$coefficients),2)),data.frame(round(exp(confint(model1)),2)))
#model2：
model2<-glm(PG_pos11~relevel(BMI_group2,ref='正常')+性别+年龄+吸烟1+饮酒+家庭收入2,data=subset(pepsinogen2019_2020,年龄>=60),family='binomial')
cbind(data.frame(aOR=round(exp(model2$coefficients),2)),data.frame(round(exp(confint(model2)),2)))










