###-----------------------------------------------------------####
###-----------------------------------------------------------####
####-------------------ETI: 城级数据构建----------------------####
###-----------------------------------------------------------####
###-----------------------------------------------------------####


#remove all the things

rm(list=ls(all=TRUE)) 

#Set workspace
getwd()
setwd("C:/Users/TJSEM/Dropbox/00000_1_Idea List/Idea_7_Energy_transition_Index/3_Code/1_城市数据_9_27")
#Sys.setlocale(category="LC_ALL",locale="en_US.UTF-8")
#Packages
library(readxl)
library(moments)
library(dplyr)
library(ggplot2)
library(poweRlaw)
#load the data

#Sys.setlocale(category = "LC_ALL", locale = "zh_cn.utf-8") #-------------------------------------------------------Read the Data

#读取ETI指数数据
Data_raw<-read_excel("C:/Users/TJSEM/Dropbox/00000_1_Idea List/Idea_7_Energy_transition_Index/3_Code/1_城市数据_9_27/9_13_城市能源转型数据.xlsx",col_types = "numeric")
Data_raw_title<-read_excel("C:/Users/TJSEM/Dropbox/00000_1_Idea List/Idea_7_Energy_transition_Index/3_Code/1_城市数据_9_27/9_13_城市能源转型数据.xlsx")
Data_raw=cbind(Data_raw_title[,1:4],Data_raw[,-c(1,2,3,4)]) ######-----------------通过这两步，将数据转化为数字格式

#按城市排序
Data_raw_1=arrange(Data_raw,城市)

rm(Data_raw,Data_raw_title)

###--------------------
#指数列表： 
###--------------------

#---------------
#01. System Performance score
#---------------

####A. Energy System Structure：
#A_1_能源结构: 煤炭占比 (%)
#A_2_电力结构: 发电能耗/用电量 (吨标准煤/千瓦时)
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
############################################### Step_1: 遴选变量################################################### 
###-------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------------------------------------------------------------------###

Data=Data_raw_1[,1:3];

#构建一个替代变量，为早期调试数据做准备(当没有是数据的时候 先填入以下值，temp)
temp=Data[,1]
temp=matrix(NA, length(temp), 1)

#人口变量（万人）
Pop_全市_总人口_万=select(Data_raw_1,年末总人口_全市_万人);
Pop_市辖区_总人口_万=select(Data_raw_1,年末总人口_市辖区_万人);
Pop_全市_非农人口_万=select(Data_raw_1,非农业人口_全市_万人);
Pop_市辖区_非农人口_万=select(Data_raw_1,非农业人口_市辖区_万人);

#GDP变量(当年价格，还未转为真实价格)
GDP_全市=select(Data_raw_1,地区生产总值_当年价格_全市_万元);
GDP_市辖区=select(Data_raw_1,地区生产总值_当年价格_市辖区_万元);

#-----------------------------------------------------
####A. Energy System Structure：
#A_1_能源结构: 煤炭占比（煤炭总燃烧量/能源燃烧量）
#A_2_电力结构: 发电能耗/用电量 
#A_3_能源强度: 能耗/GDP
#A_4_能源消费：1. 人均电力消费； 2. 人均能源消费； 
#-----------------------------------------------------

Data=cbind(Data,select(Data_raw_1,能源结构)) #煤炭占能源消费的比重(北海市有问题)
Data=cbind(Data,select(Data_raw_1,`发电能耗/全社会用电量（吨标准煤/千瓦时）`)) #发电能耗/用电量 
Data=cbind(Data,select(Data_raw_1,`Scope 1 Total能源燃烧量（NCV)`)/GDP_市辖区) #能耗/GDP
Data=cbind(Data,select(Data_raw_1,`Scope 1 Total能源燃烧量（NCV)`)/Pop_市辖区_总人口_万) #能源消费： 2. 人均能源消费； 
Data=cbind(Data,select(Data_raw_1,`全年用电总量_市辖区_万千瓦时...34`)/Pop_市辖区_总人口_万) #能源消费：1. 人均电力消费；

#重命名
names(Data)[4] <- "A_1_能源结构"
names(Data)[5] <- "A_2_电力结构"
names(Data)[6] <- "A_3_能源强度"
names(Data)[7] <- "A_4_能源消费_化石能源"
names(Data)[8] <- "A_4_能源消费_电力"


#-----------------------------------------------------
####B. Environmental Sustainability：
#B_1_碳强度: 
#B_2_人均碳排放: 
#B_3_空气污染: PM 2.5
#-----------------------------------------------------

Carbon_Intensity=select(Data_raw_1,各城市排放量)/GDP_全市
Carbon_emission_per_capita =select(Data_raw_1,各城市排放量)/Pop_全市_总人口_万
PM2.5=select(Data_raw_1,可吸入细颗粒物年平均浓度_全市_微克每立方米) 

Data=cbind(Data,Carbon_Intensity) #Carbon Intensity
Data=cbind(Data,Carbon_emission_per_capita) #Carbon emission per capita
Data=cbind(Data,PM2.5) #PM2.5

#重命名
names(Data)[9] <- "B_1_碳强度"
names(Data)[10] <- "B_2_人均碳排放"
names(Data)[11] <- "B_3_空气污染"

#去除冗余变量
rm(Carbon_Intensity,Carbon_emission_per_capita, PM2.5) 


#-----------------------------------------------------
####C. Economic Development：  
#C_1_Economic_Growth: 1. 人均GDP； 2. GDP Growth
#C_2_Economic_Structure: 1. 第二产业采矿比例; 2. 第三产业;
#-----------------------------------------------------

#1_经济增长
#人均GDP
GDP_人均=GDP_全市/Pop_全市_总人口_万
Data=cbind(Data,GDP_人均) #人均GDP-----------------------------------------------（稳健性检验，职工平均工资_全市）

#Data=cbind(Data,select(Data_raw_1,人均地区生产总值_全市_元))    #-----------不直接用该Data_raw_1中已有数据，2013年很多城市有问题，总体数据与以上计算数据一致；

#人均GDP增长
Data=cbind(Data,select(Data_raw_1,地区生产总值增长率_全市_百分比)) #人均GDP-----------------------------------------------（稳健性检验，职工平均工资_全市）

#2_经济结构
#1. 采矿业人员比例；
Mine_pop=select(Data_raw_1,第二产业_采矿业_全市_人)/Pop_全市_总人口_万;
#2. 三产比例；
Data=cbind(Data,Mine_pop,select(Data_raw_1,第三产业占GDP的比重_全市_百分比)) #第二产业采矿比例，第三产业GDP占比

#重命名
names(Data)[12] <- "C_1_Economic_Devlelopment_人均GDP"
names(Data)[13] <- "C_1_Economic_Devlelopment_GDP增速"
names(Data)[14] <- "C_2_Economic_Stucture_第二产业_采矿业人员占比"
names(Data)[15] <- "C_2_Economic_Stucture_第三产业"


#-----------------------------------------------------
####D. Capital & investment：  
#D_1_资本存量: 1. 人均固定资产净值年平均余额_全市_万元 2. 城市建设用地占城市建设用地占市辖区面积比重_百分比；3. 人均存款余额;
#D_2_投资: 1. 人均固定资产投资总额_全市_万元; 2. 人均当年实际使用外资；
#D_3_财政能力: 人均财政收入
#-----------------------------------------------------

#D_1_资本存量:
Data=cbind(Data,select(Data_raw_1,固定资产净值年平均余额_全市_万元)/Pop_全市_总人口_万,select(Data_raw_1,城市建设用地占市辖区面积比重_百分比),select(Data_raw_1,年末金融机构人民币各项存款余额_全市_万元)/Pop_全市_总人口_万) #1. 固定资产净值年平均余额_全市_万元；2. 城市建设用地占市辖区面积比重_百分比; 3.年末金融机构人民币各项存款余额_全市_万元 

#D_2_投资:
Data=cbind(Data,select(Data_raw_1,固定资产投资总额_全市_万元)/Pop_全市_总人口_万,select(Data_raw_1,外商实际投资额_全市_万美元)/Pop_全市_总人口_万) #1.人均固定资产投资总额_全市_万元 ; 2. 当年实际使用外资额；

#D_3_财政能力: 
Data=cbind(Data,select(Data_raw_1,地方一般公共预算收入_全市_万元)/Pop_全市_总人口_万) #财政收入

#重命名
names(Data)[16] <- "D_1_资本存量_人均固定资产净余额"
names(Data)[17] <- "D_1_资本存量_城市建设用地占比"
names(Data)[18] <- "D_1_资本存量_人均存款余额"
names(Data)[19] <- "D_2_投资_人均固定资产投资"
names(Data)[20] <- "D_2_投资_人均当年使用外资"
names(Data)[21] <- "D_3_财政能力_人均GDP"


#去除冗余变量
rm(Mine_pop) 

#-----------------------------------------------------
####E. Technology capability：
#E_1_创新能力: 1. 创新创业指数； 2. 新经济：互联网接入比例; 3. 人均绿色发明+绿色专利
#E_2_科学支出: 人均科技支出
#E_3_适应性技术能力: 1. 二氧化硫去除率； 2 工业废水去除率;  3 生活垃圾无害化处理率;  4 一般工业固体废物综合利用率;
#-----------------------------------------------------

#E_1_创新能力
Data=cbind(Data,select(Data_raw_1,`各地市创新创业创业指数-人均得分`)) #创新创业指数
Data=cbind(Data,select(Data_raw_1,互联网宽带接入用户数_全市_万户)/Pop_全市_总人口_万) #互联网接入比例 (这个数据尚可，但多多少少有点问题，比如上海市是下降的)
Data=cbind(Data,(select(Data_raw_1,当年申请的绿色发明数量)/Pop_全市_总人口_万+select(Data_raw_1,当年申请的绿色实用新型数量)/Pop_全市_总人口_万)/2) #1. 绿色发明； 2 绿色实用发明


#E_2_人均科学技术支出
Data=cbind(Data,select(Data_raw_1,`地方一般公共预算收支状况(全市)-科学技术支出（万元）`)/Pop_全市_总人口_万) #人均科学技术支出


#E_3_适应性技术能力:
#1. 二氧化硫去除；
SO2= select(Data_raw_1,工业二氧化硫去除量_全市_吨)/(select(Data_raw_1,工业二氧化硫去除量_全市_吨)+select(Data_raw_1,工业二氧化硫排放量_全市_吨))

#2 工业废水处理率；
Water= select(Data_raw_1,污水处理厂集中处理率_全市_百分比)

#3 生活垃圾无害化处理
Trash= select(Data_raw_1,生活垃圾无害化处理率_全市_百分比)

#4 工业固体废物综合利用率
Solid= select(Data_raw_1,一般工业固体废物综合利用率_全市_百分比)

Data=cbind(Data,SO2,Water,Trash,Solid) #1. 二氧化硫去除； 2 工业废水处理率； 3 生活垃圾无害化处理； 4 工业固体废物综合利用率

names(Data)[22] <- "E_1_创新能力_创新创业指数"
names(Data)[23] <- "E_1_创新能力_新经济"
names(Data)[24] <- "E_1_创新能力_发明专利"
names(Data)[25] <- "E_2_科学支出"
names(Data)[26] <- "E_3_适应性技术能力_二氧化硫去除率"
names(Data)[27] <- "E_3_适应性技术能力_工业废水处理率"
names(Data)[28] <- "E_3_适应性技术能力_生活垃圾无害化处理率"
names(Data)[29] <- "E_3_适应性技术能力_工业固体废物综合利用率"

#去除冗余变量
rm(SO2,Water,Trash,Solid) 


#-----------------------------------------------------
####F. Human Capital（Quality of Education）：
#F_1_教育水平: 大学人口占比
#F_2_师资培训水平:  1 教育从业比例； 2 中职； 3 普高教师；
#F_3_科研能力：1. R&D人员比例 2. 信息技术； 3. 人均科学教育支出； 
#-----------------------------------------------------


#F_1_科研能力
#1. R&D人数；
R_and_D_pop=select(Data_raw_1,科研综合技术服务业从业人员数_全市_人)/Pop_全市_总人口_万; 
#2. 第三产业信息产业人数
IT_pop=select(Data_raw_1,第三产业_信息传输计算机服务和软件业_全市_人)/Pop_全市_总人口_万; 

#F_2_师资培训水平；
Edu_1=select(Data_raw_1,第三产业_教育_全市_人)/Pop_全市_总人口_万; 
Edu_2=select(Data_raw_1,中等职业教育学校专任教师数_全市_人)/Pop_全市_总人口_万; 
Edu_3=select(Data_raw_1,普通高等学校专任教师数_全市_人)/Pop_全市_总人口_万; 

#F_3_教育水平；
Edu_level_1=select(Data_raw_1,`地方一般公共预算收支状况(全市)-教（万元）育支出`)/Pop_全市_总人口_万; 
Edu_level_2=select(Data_raw_1,普通本专科在校学生数_全市_人)/Pop_全市_总人口_万; 


Data=cbind(Data,R_and_D_pop,IT_pop,Edu_1,Edu_2,Edu_3,Edu_level_1,Edu_level_2) 

names(Data)[30] <- "F_1_科研能力_科研人员比例"
names(Data)[31] <- "F_1_科研能力_信息技术人员比例"
names(Data)[32] <- "F_2_师资培训水平_教育人员比例"
names(Data)[33] <- "F_2_师资培训水平_中职"
names(Data)[34] <- "F_2_师资培训水平_普高"
names(Data)[35] <- "F_3_教育水平_人均教育事业费用支出"
names(Data)[36] <- "F_3_教育水平_大学生比例"

#去除冗余变量
rm(R_and_D_pop,IT_pop,Edu_1,Edu_2,Edu_3,Edu_level_1,Edu_level_2) 



#------------------------------------
####save the data
#------------------------------------

# write.csv(Data, "Data_select.csv", fileEncoding = "GBK",row.names = F)

