###-----------------------------------------------------------####
###-----------------------------------------------------------####
####-------------------ETI: 城级数据构建----------------------####
###-----------------------------------------------------------####
###-----------------------------------------------------------####


#remove all the things
rm(list=ls(all=TRUE)) 

#读取阶段一结果
source("C:/Users/TJSEM/Dropbox/00000_1_Idea List/Idea_7_Energy_transition_Index/3_Code/1_城市数据_9_27/01. Summary Statistics_数据处理.R", encoding = 'GB18030')
library(zoo)

#读取城市名称列表
city_name_list <- read_excel("C:/Users/TJSEM/Dropbox/00000_1_Idea List/Idea_7_Energy_transition_Index/3_Code/1_城市数据_9_27/city_name_list.xlsx")

###-----------------------------
#指数列表： 
###-----------------------------

#---------------
#01. System Performance score
#---------------

####A. Energy System Structure：
#A_1_能源结构: 煤炭占比
#A_2_电力结构: 发电能耗/用电量
#A_3_能源强度: 能耗/GDP
#A_4_能源消费：1. 人均电力消费； 2. 人均能源消费； 

####B. Environmental Sustainability：
#B_1_碳强度: 
#B_2_人均碳排放: 
#B_3_空气污染: PM 2.5

#---------------
#02. System Performance score
#---------------

####C. Economic Development：  
#C_1_Economic_Growth: 1. 人均GDP； 2. GDP Growth
#C_2_Economic_Structure: 1. 第二产业采矿比例; 2. 第三产业;

####D. Capital & investment：  
#D_1_资本存量: 1. 人均固定资产净值年平均余额_全市_万元 2. 城市建设用地占城市建设用地占市辖区面积比重_百分比；3. 人均存款余额;
#D_2_投资: 1. 人均固定资产投资总额_全市_万元; 2. 人均当年实际使用外资；
#D_3_财政能力: 人均财政收入；

####E. Technology capability：
#E_1_创新能力: 1. 创新创业指数； 2. 新经济：互联网接入比例; 3. 人均绿色发明+绿色专利
#E_2_科学支出: 人均科技支出
#E_3_适应性技术能力: 1. 二氧化硫去除率； 2 工业废水去除率;  3 生活垃圾无害化处理率;  4 一般工业固体废物综合利用率;

####F. Human Capital（Quality of Education）：
#F_1_教育水平: 大学人口占比
#F_2_师资培训水平:  1 教育从业比例； 2 中职； 3 普高教师；
#F_3_科研能力：1. R&D人员比例 2. 信息技术； 3. 人均科学教育支出； 


###-------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------------------------------------------------------------------###
############################################### Step_2: 数据处理################################################### 
###-------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------------------------------------------------------------------###

#------------------------------------
#------------------------------------
####A. 初步查验数据
#------------------------------------
#------------------------------------

# #初始城市数量---------------282个
# length(unique(Data$城市))
# 
# #Check the number of values
# Data_unit = cbind(Data,temp=matrix(1, nrow(temp), 1)) #构建为1的标记序列
# names(Data_unit)[37] <- "Units"                       #命名为Units
# Data_unit <- subset(Data_unit, !is.na(Data_unit$A_1_能源结构)) #以Carbon_Intensity为基准，查看available的数据
# Data_select<-Data_unit %>% group_by(城市)%>% filter(sum(Units)>1)     #遴选大于阈值10的城市集为180，大于阈值5（1）的城市集为234（252）；2010年有数据的为228；总共有282个城市
# #Number of cities
# length(unique(Data_select$城市)) #大于阈值（10）的城市集城市数量-----------------------------------------------数据良好，10为阈值尚有3/4的数据。
# 
# 
# rm(Data_select,Data_unit) 

#------------------------------------
#------------------------------------
####B. 指数构建
#------------------------------------
#------------------------------------

#------------------------------------
####Step B_0. 指数的选择 --------------------09_27版本之后，不需要这一步骤了
#------------------------------------

# #移除没有数据的列（即该列的所值均为NA）
# Data_temp <- Data[,colSums(is.na(Data))<nrow(Data)]
# 
# sapply(Data_temp, class)
# 
# # #Change to numeric
# # for (i in 4:length(Data_temp)){
# #   Data_temp[,i]<-as.numeric(Data_temp[,i])
# # }
# 
# ##暂时不用到最后三个数据
# Data_temp=Data_temp[,1:(ncol(Data_temp)-3)] 
# 
# #定义新变量
# Data_2=Data_temp
# 
# #去除冗余变量
# rm(Data_temp) 

#------------------------------------
####Step B_1. 时间范围的选择 
#------------------------------------

Data_2=Data

#01. 时间变量的选择

Data_2=filter(Data_2,年份<2020) 

#------------------------------------
####Step B_2. 数据填充；暂时使用最简单的方式（如果有一个数据，就用这个数据填充；如果没有数据，就放均值；未来可以用同省数据填充等方式）
#------------------------------------

Data_select=Data_2

#填充数据，标准为线性填充，边界值由边界值延展；注意其中na.approx函数对只有一个数据的列只会return一个数值，所以这边要写loop调整；
n_country=length(unique(Data_select$城市))
n_year=length(unique(Data_select$年份))
n_column=ncol(Data_select)


#步骤一：线性填充
for (i in 1:n_country) {
  for (j in 4:n_column) {
    if (sum(is.na(Data_select[(i*n_year-n_year+1):(i*n_year),j]))==(n_year-1)) { Data_select[(i*n_year-n_year+1):(i*n_year),j]=mean(as.numeric(unlist(Data_select[(i*n_year-n_year+1):(i*n_year),j])),na.rm = T) } 
    else { if (sum(is.na(Data_select[(i*n_year-n_year+1):(i*n_year),j])) < (n_year-1)) { Data_select[(i*n_year-n_year+1):(i*n_year),j]=na.approx(Data_select[(i*n_year-n_year+1):(i*n_year),j],rule=2) }  } 
  }
}


#步骤二：将某一指标全为NA的，用均值代替
Data_temp=Data_select

Data_mean=Data_temp %>% group_by(年份) %>% summarise_all("mean",na.rm = TRUE) 


#填充数据
n_country=length(unique(Data_temp$城市))
n_year=length(unique(Data_temp$年份))
n_column=ncol(Data_temp)


for (i in 1:n_country) {
  for (j in 4:n_column) {
    if (sum(is.na(Data_temp[(i*n_year-n_year+1):(i*n_year),j]))==(n_year)) {Data_temp[(i*n_year-n_year+1):(i*n_year),j]=as.numeric(unlist(Data_mean[,j])) } 
  }
}

Data_2=Data_temp


#------------------------------------
####Step B_3. 上下限的选择（bound selection）
#------------------------------------
#注：暂时我们用最简单的选法，即最高的2.5%和最低的2.5%；稳健性检验汇报其他结果

Quan_mat=matrix(0,2,length(Data_2))

#97.5% and 2.5%
for (i in 4:length(Data_2)){
  Quan_mat[1,i]<-quantile(na.omit(Data_2[,i]), 0.975)
  Quan_mat[2,i]<-quantile(na.omit(Data_2[,i]), 0.025)
}

colnames(Quan_mat)=colnames(Data_2)



#------------------------------------
####Step B_4. 数据标准化（normalization of indicator values）------------------------------------------------------------------------------------调整这里来调整数据覆盖城市范围（#遴选大于阈值10的城市集为180，大于阈值5的城市集为234；2010年有数据的为228；总城市数量包含282个）
#------------------------------------

#选择正向指标，并提取编号
Positive_index=match(c("C_1_Economic_Devlelopment_人均GDP","C_1_Economic_Devlelopment_GDP增速" ,"C_2_Economic_Stucture_第二产业_采矿业人员占比","C_2_Economic_Stucture_第三产业" ,  "D_1_资本存量_人均固定资产净余额" ,"D_1_资本存量_城市建设用地占比","D_1_资本存量_人均存款余额"  ,"D_2_投资_人均固定资产投资", "D_2_投资_人均当年使用外资", "D_3_财政能力_人均GDP", "E_1_创新能力_创新创业指数" , "E_1_创新能力_新经济"   ,"E_1_创新能力_发明专利" , "E_2_科学支出","E_3_适应性技术能力_二氧化硫去除率",  "E_3_适应性技术能力_工业废水处理率"  ,"E_3_适应性技术能力_生活垃圾无害化处理率"   ,    "E_3_适应性技术能力_工业固体废物综合利用率"   ,  "F_1_科研能力_科研人员比例" ,  "F_1_科研能力_信息技术人员比例" , "F_2_师资培训水平_教育人员比例" , "F_2_师资培训水平_中职" ,   "F_2_师资培训水平_普高" , "F_3_教育水平_人均教育事业费用支出", "F_3_教育水平_大学生比例" ) ,names(Data_2))

#选择负向指标，并提取编号
Negative_index=match(c( "A_1_能源结构" ,"A_2_电力结构" ,"A_3_能源强度","A_4_能源消费_化石能源","A_4_能源消费_电力","B_1_碳强度" ,"B_2_人均碳排放"  ,"B_3_空气污染" ,"C_2_Economic_Stucture_第二产业_采矿业人员占比") ,names(Data_2))


Data_Normal = Data_2;

#正向指标
for (i in Positive_index){
  for (j in 1:nrow(Data_2)){
    Data_Normal[j,i]<-(( Data_2[j,i]-Quan_mat[2,i])/(Quan_mat[1,i]-Quan_mat[2,i]))*100
  }
}

#负向指标
for (i in Negative_index){
  for (j in 1:nrow(Data_2)){
    Data_Normal[j,i]<-(( Data_2[j,i]-Quan_mat[1,i])/(Quan_mat[2,i]-Quan_mat[1,i]))*100
  }
}


#top coding，阈值处理（超过100的为100，小于0的为0）

Data_Normal[Data_Normal<0] <- 0
Data_Normal[Data_Normal>100] <- 100

#加回表头
Data_Normal[,1:3]=Data_2[,1:3]

#去除冗余变量
rm(Data_2,Data_temp,Data_mean,Data_select)

#------------------------------------
####Step B_5. 国家&城市指数的构建
#------------------------------------

##################----------------------------------------------------------------###############
##################----------------------------------------------------------------###############
##################-------------------------选择城市范围---------------------------###############
##################------------------基准结果，大样本(282个城市)-------------------###############
##################----------------------------------------------------------------###############
##################----------------------------------------------------------------###############

#初始城市数量---------------282个
length(unique(Data_Normal$城市))

Data_select=Data_Normal

##################----------------------------------------------------------------###############
##################--------------------1.国家层面指数构建--------------------------###############
##################----------------------------------------------------------------###############


#----------国家指数的构建------------#------------------------------------（每1年一个指数）

#计算年度加总数据
Data_temp<-Data_select%>% group_by(年份)%>% summarise_all("mean",na.rm = TRUE)   

# #-----A 简单平均-----
# Results_National_1_detail=mutate(Data_temp, National_1 = rowMeans(Data_temp[,3:ncol(Data_temp)]))
# 
# #构建精简国家层面结果
# Results_National_1=cbind(Results_National_1_detail[,1],Results_National_1_detail[,length(Results_National_1_detail)])
# 
# # #保存数据
# # write.csv(Results_National_1_detail, "Results_National_1_detail.csv", fileEncoding = "GBK",row.names = F)


#-----B 理论框架加总----- （根据具体情况，调整）

#生成子指标
Results_National_2_A=(rowMeans(select(Data_temp[,-1], starts_with("A_1")))+rowMeans(select(Data_temp[,-1], starts_with("A_2")))+rowMeans(select(Data_temp[,-1], starts_with("A_3")))+rowMeans(select(Data_temp[,-1], starts_with("A_4"))))/4
Results_National_2_B=(rowMeans(select(Data_temp[,-1], starts_with("B_1")))+rowMeans(select(Data_temp[,-1], starts_with("B_2")))+rowMeans(select(Data_temp[,-1], starts_with("B_3"))))/3
Results_National_2_C=(rowMeans(select(Data_temp[,-1], starts_with("C_1")))+rowMeans(select(Data_temp[,-1], starts_with("C_2"))))/2
Results_National_2_D=(rowMeans(select(Data_temp[,-1], starts_with("D_1")))+rowMeans(select(Data_temp[,-1], starts_with("D_2")))+rowMeans(select(Data_temp[,-1], starts_with("D_3"))))/3
Results_National_2_E=(rowMeans(select(Data_temp[,-1], starts_with("E_1")))+rowMeans(select(Data_temp[,-1], starts_with("E_2")))+rowMeans(select(Data_temp[,-1], starts_with("E_3"))))/3
Results_National_2_F=(rowMeans(select(Data_temp[,-1], starts_with("F_1")))+rowMeans(select(Data_temp[,-1], starts_with("F_2")))+rowMeans(select(Data_temp[,-1], starts_with("F_3"))))/3

#计算子指数
Results_National_2_Energy=(Results_National_2_A+Results_National_2_B)/2
Results_National_2_Readiness=(Results_National_2_C+Results_National_2_D+Results_National_2_E+Results_National_2_F)/4

#计算总指数
Results_National_2_ETI=(Results_National_2_Energy+Results_National_2_Readiness)/2

#合并数据
Results_National_2_detail=cbind(Data_temp,Results_National_2_A,Results_National_2_B,Results_National_2_C,Results_National_2_D,Results_National_2_E,Results_National_2_F,Results_National_2_Energy,Results_National_2_Readiness,Results_National_2_ETI)


#修改名称
colnames(Results_National_2_detail)[(ncol(Results_National_2_detail)-8):ncol(Results_National_2_detail)]=c("National_2_A","National_2_B","National_2_C","National_2_D","National_2_E","National_2_F","National_2_Energy","National_2_Readiness","National_2_ETI")

# #构建精简国家层面结果
# Results_National_2=cbind(Results_National_2_detail[,1],Results_National_2_detail[,length(Results_National_2_detail)])

#去除冗余变量
rm(Results_National_2_A,Results_National_2_B,Results_National_2_C,Results_National_2_D,Results_National_2_E,Results_National_2_F,Results_National_2_Energy,Results_National_2_Readiness,Results_National_2_ETI,Data_temp)

#-----------------
#保存数据
#-----------------
write.csv(Results_National_2_detail, "Results_National_2_detail.csv", fileEncoding = "GBK",row.names = F)


##################----------------------------------------------------------------###############
##################--------------------2.城市层面指数构建--------------------------###############
##################----------------------------------------------------------------###############

#----------城市指数的构建------------#------------------------------------（每5年一个指数）

#定义城市数据，方便操作
Data_select_城市 = Data_select

#标识数据，以便计算中位数
Data_select_城市=Data_select_城市 %>% 
  mutate(mark = case_when(年份 %in% c(2003,2004,2005,2006,2007) ~ 1, 
                            年份 %in% c(2008,2009,2010,2011,2012,2013,2014) ~ 2,
                            年份 %in% c(2015,2016,2017,2018,2019) ~ 3
  ))

#计算中位数
Data_temp<-Data_select_城市%>% group_by(城市,mark)%>% summarise_all("median",na.rm = TRUE)   


# #-----A 简单平均-----
# 
# #保留表头
# Data_temp_title=Data_temp[,1:4]
# 
# #计算城市层面指数
# Data_temp=Data_temp[,5:ncol(Data_temp)] %>% mutate(city_1 = rowMeans(.,na.rm = TRUE))
# Results_城市_1_detail=cbind(Data_temp_title,Data_temp)
# 
# #构建精简城市层面结果
# Results_城市_1=cbind(Results_城市_1_detail[,1:4],Results_城市_1_detail[,length(Results_城市_1_detail)])
# 
# #去除冗余变量
# rm(Data_temp_title,Data_temp)



#-----B 理论框架加总----- （根据具体情况，调整）
#计算中位数
Data_temp<-Data_select_城市%>% group_by(城市,mark)%>% summarise_all("median",na.rm = TRUE)   

#生成子指标
Results_City_2_A= (rowMeans(select(Data_temp[,-1], starts_with("A_1")))+rowMeans(select(Data_temp[,-1], starts_with("A_2")))+rowMeans(select(Data_temp[,-1], starts_with("A_3")))+rowMeans(select(Data_temp[,-1], starts_with("A_4"))))/4 #########---------------------------负一是为了遴选变量的方便；不包含负一，会选到地方这个变量
Results_City_2_B= (rowMeans(select(Data_temp[,-1], starts_with("B_1")))+rowMeans(select(Data_temp[,-1], starts_with("B_2")))+rowMeans(select(Data_temp[,-1], starts_with("B_3"))))/3
Results_City_2_C= (rowMeans(select(Data_temp[,-1], starts_with("C_1")))+rowMeans(select(Data_temp[,-1], starts_with("C_2"))))/2
Results_City_2_D= (rowMeans(select(Data_temp[,-1], starts_with("D_1")))+rowMeans(select(Data_temp[,-1], starts_with("D_2")))+rowMeans(select(Data_temp[,-1], starts_with("D_3"))))/3
Results_City_2_E= (rowMeans(select(Data_temp[,-1], starts_with("E_1")))+rowMeans(select(Data_temp[,-1], starts_with("E_2")))+rowMeans(select(Data_temp[,-1], starts_with("E_3"))))/3
Results_City_2_F= (rowMeans(select(Data_temp[,-1], starts_with("F_1")))+rowMeans(select(Data_temp[,-1], starts_with("F_2")))+rowMeans(select(Data_temp[,-1], starts_with("F_3"))))/3

#计算子指数
Results_City_2_Energy=(Results_City_2_A+Results_City_2_B)/2
Results_City_2_Readiness=(Results_City_2_C+Results_City_2_D+Results_City_2_E+Results_City_2_F)/4

#计算指数
Results_City_2_ETI=(Results_City_2_Energy+Results_City_2_Readiness)/2

#合并数据
Results_City_2_detail=cbind(Data_temp,Results_City_2_A,Results_City_2_B,Results_City_2_C,Results_City_2_D,Results_City_2_E,Results_City_2_F,Results_City_2_Energy,Results_City_2_Readiness,Results_City_2_ETI)

#修改名称
colnames(Results_City_2_detail)[(ncol(Results_City_2_detail)-8):ncol(Results_City_2_detail)]=c("City_2_A","City_2_B","City_2_C","City_2_D","City_2_E","City_2_F","City_2_Energy","City_2_Readiness","City_2_ETI")

# #构建精简城市层面结果
# Results_City_2=cbind(Results_City_2_detail[,1:4],Results_City_2_detail[,length(Results_City_2_detail)])

# #-----------------
# #将中文城市名称转化为英文
# #-----------------
# 
# for (i in 1:dim(Results_City_2_detail)[1]) {
#   Results_City_2_detail$城市[i]<-city_name_list$English_Name[city_name_list$Chinese_Name==Results_City_2_detail$城市[i]]
# }

#保存数据
write.csv(Results_City_2_detail, "Results_City_2_detail.csv", fileEncoding = "GBK",row.names = F)
