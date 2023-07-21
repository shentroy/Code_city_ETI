###-----------------------------------------------------------####
###-----------------------------------------------------------####
####-------------------ETI: �Ǽ����ݹ���----------------------####
###-----------------------------------------------------------####
###-----------------------------------------------------------####


#remove all the things
rm(list=ls(all=TRUE)) 

#��ȡ�׶�һ���
source("C:/Users/TJSEM/Dropbox/00000_1_Idea List/Idea_7_Energy_transition_Index/3_Code/1_��������_9_27/01. Summary Statistics_���ݴ���.R", encoding = 'GB18030')
library(zoo)

#��ȡ���������б�
city_name_list <- read_excel("C:/Users/TJSEM/Dropbox/00000_1_Idea List/Idea_7_Energy_transition_Index/3_Code/1_��������_9_27/city_name_list.xlsx")

###-----------------------------
#ָ���б��� 
###-----------------------------

#---------------
#01. System Performance score
#---------------

####A. Energy System Structure��
#A_1_��Դ�ṹ: ú̿ռ��
#A_2_�����ṹ: �����ܺ�/�õ���
#A_3_��Դǿ��: �ܺ�/GDP
#A_4_��Դ���ѣ�1. �˾��������ѣ� 2. �˾���Դ���ѣ� 

####B. Environmental Sustainability��
#B_1_̼ǿ��: 
#B_2_�˾�̼�ŷ�: 
#B_3_������Ⱦ: PM 2.5

#---------------
#02. System Performance score
#---------------

####C. Economic Development��  
#C_1_Economic_Growth: 1. �˾�GDP�� 2. GDP Growth
#C_2_Economic_Structure: 1. �ڶ���ҵ�ɿ����; 2. ������ҵ;

####D. Capital & investment��  
#D_1_�ʱ�����: 1. �˾��̶��ʲ���ֵ��ƽ�����_ȫ��_��Ԫ 2. ���н����õ�ռ���н����õ�ռ��Ͻ���������_�ٷֱȣ�3. �˾�������;
#D_2_Ͷ��: 1. �˾��̶��ʲ�Ͷ���ܶ�_ȫ��_��Ԫ; 2. �˾�����ʵ��ʹ�����ʣ�
#D_3_��������: �˾��������룻

####E. Technology capability��
#E_1_��������: 1. ���´�ҵָ���� 2. �¾��ã��������������; 3. �˾���ɫ����+��ɫר��
#E_2_��ѧ֧��: �˾��Ƽ�֧��
#E_3_��Ӧ�Լ�������: 1. ��������ȥ���ʣ� 2 ��ҵ��ˮȥ����;  3 ���������޺���������;  4 һ�㹤ҵ��������ۺ�������;

####F. Human Capital��Quality of Education����
#F_1_����ˮƽ: ��ѧ�˿�ռ��
#F_2_ʦ����ѵˮƽ:  1 ������ҵ������ 2 ��ְ�� 3 �ո߽�ʦ��
#F_3_����������1. R&D��Ա���� 2. ��Ϣ������ 3. �˾���ѧ����֧���� 


###-------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------------------------------------------------------------------###
############################################### Step_2: ���ݴ���################################################### 
###-------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------------------------------------------------------------------###

#------------------------------------
#------------------------------------
####A. ������������
#------------------------------------
#------------------------------------

# #��ʼ��������---------------282��
# length(unique(Data$����))
# 
# #Check the number of values
# Data_unit = cbind(Data,temp=matrix(1, nrow(temp), 1)) #����Ϊ1�ı������
# names(Data_unit)[37] <- "Units"                       #����ΪUnits
# Data_unit <- subset(Data_unit, !is.na(Data_unit$A_1_��Դ�ṹ)) #��Carbon_IntensityΪ��׼���鿴available������
# Data_select<-Data_unit %>% group_by(����)%>% filter(sum(Units)>1)     #��ѡ������ֵ10�ĳ��м�Ϊ180��������ֵ5��1���ĳ��м�Ϊ234��252����2010�������ݵ�Ϊ228���ܹ���282������
# #Number of cities
# length(unique(Data_select$����)) #������ֵ��10���ĳ��м���������-----------------------------------------------�������ã�10Ϊ��ֵ����3/4�����ݡ�
# 
# 
# rm(Data_select,Data_unit) 

#------------------------------------
#------------------------------------
####B. ָ������
#------------------------------------
#------------------------------------

#------------------------------------
####Step B_0. ָ����ѡ�� --------------------09_27�汾֮�󣬲���Ҫ��һ������
#------------------------------------

# #�Ƴ�û�����ݵ��У������е���ֵ��ΪNA��
# Data_temp <- Data[,colSums(is.na(Data))<nrow(Data)]
# 
# sapply(Data_temp, class)
# 
# # #Change to numeric
# # for (i in 4:length(Data_temp)){
# #   Data_temp[,i]<-as.numeric(Data_temp[,i])
# # }
# 
# ##��ʱ���õ������������
# Data_temp=Data_temp[,1:(ncol(Data_temp)-3)] 
# 
# #�����±���
# Data_2=Data_temp
# 
# #ȥ���������
# rm(Data_temp) 

#------------------------------------
####Step B_1. ʱ�䷶Χ��ѡ�� 
#------------------------------------

Data_2=Data

#01. ʱ�������ѡ��

Data_2=filter(Data_2,���<2020) 

#------------------------------------
####Step B_2. ������䣻��ʱʹ����򵥵ķ�ʽ�������һ�����ݣ��������������䣻���û�����ݣ��ͷž�ֵ��δ��������ͬʡ�������ȷ�ʽ��
#------------------------------------

Data_select=Data_2

#������ݣ���׼Ϊ������䣬�߽�ֵ�ɱ߽�ֵ��չ��ע������na.approx������ֻ��һ�����ݵ���ֻ��returnһ����ֵ���������Ҫдloop������
n_country=length(unique(Data_select$����))
n_year=length(unique(Data_select$���))
n_column=ncol(Data_select)


#����һ���������
for (i in 1:n_country) {
  for (j in 4:n_column) {
    if (sum(is.na(Data_select[(i*n_year-n_year+1):(i*n_year),j]))==(n_year-1)) { Data_select[(i*n_year-n_year+1):(i*n_year),j]=mean(as.numeric(unlist(Data_select[(i*n_year-n_year+1):(i*n_year),j])),na.rm = T) } 
    else { if (sum(is.na(Data_select[(i*n_year-n_year+1):(i*n_year),j])) < (n_year-1)) { Data_select[(i*n_year-n_year+1):(i*n_year),j]=na.approx(Data_select[(i*n_year-n_year+1):(i*n_year),j],rule=2) }  } 
  }
}


#���������ĳһָ��ȫΪNA�ģ��þ�ֵ����
Data_temp=Data_select

Data_mean=Data_temp %>% group_by(���) %>% summarise_all("mean",na.rm = TRUE) 


#�������
n_country=length(unique(Data_temp$����))
n_year=length(unique(Data_temp$���))
n_column=ncol(Data_temp)


for (i in 1:n_country) {
  for (j in 4:n_column) {
    if (sum(is.na(Data_temp[(i*n_year-n_year+1):(i*n_year),j]))==(n_year)) {Data_temp[(i*n_year-n_year+1):(i*n_year),j]=as.numeric(unlist(Data_mean[,j])) } 
  }
}

Data_2=Data_temp


#------------------------------------
####Step B_3. �����޵�ѡ��bound selection��
#------------------------------------
#ע����ʱ��������򵥵�ѡ��������ߵ�2.5%����͵�2.5%���Ƚ��Լ���㱨�������

Quan_mat=matrix(0,2,length(Data_2))

#97.5% and 2.5%
for (i in 4:length(Data_2)){
  Quan_mat[1,i]<-quantile(na.omit(Data_2[,i]), 0.975)
  Quan_mat[2,i]<-quantile(na.omit(Data_2[,i]), 0.025)
}

colnames(Quan_mat)=colnames(Data_2)



#------------------------------------
####Step B_4. ���ݱ�׼����normalization of indicator values��------------------------------------------------------------------------------------�����������������ݸ��ǳ��з�Χ��#��ѡ������ֵ10�ĳ��м�Ϊ180��������ֵ5�ĳ��м�Ϊ234��2010�������ݵ�Ϊ228���ܳ�����������282����
#------------------------------------

#ѡ������ָ�꣬����ȡ���
Positive_index=match(c("C_1_Economic_Devlelopment_�˾�GDP","C_1_Economic_Devlelopment_GDP����" ,"C_2_Economic_Stucture_�ڶ���ҵ_�ɿ�ҵ��Առ��","C_2_Economic_Stucture_������ҵ" ,  "D_1_�ʱ�����_�˾��̶��ʲ������" ,"D_1_�ʱ�����_���н����õ�ռ��","D_1_�ʱ�����_�˾�������"  ,"D_2_Ͷ��_�˾��̶��ʲ�Ͷ��", "D_2_Ͷ��_�˾�����ʹ������", "D_3_��������_�˾�GDP", "E_1_��������_���´�ҵָ��" , "E_1_��������_�¾���"   ,"E_1_��������_����ר��" , "E_2_��ѧ֧��","E_3_��Ӧ�Լ�������_��������ȥ����",  "E_3_��Ӧ�Լ�������_��ҵ��ˮ������"  ,"E_3_��Ӧ�Լ�������_���������޺���������"   ,    "E_3_��Ӧ�Լ�������_��ҵ��������ۺ�������"   ,  "F_1_��������_������Ա����" ,  "F_1_��������_��Ϣ������Ա����" , "F_2_ʦ����ѵˮƽ_������Ա����" , "F_2_ʦ����ѵˮƽ_��ְ" ,   "F_2_ʦ����ѵˮƽ_�ո�" , "F_3_����ˮƽ_�˾�������ҵ����֧��", "F_3_����ˮƽ_��ѧ������" ) ,names(Data_2))

#ѡ����ָ�꣬����ȡ���
Negative_index=match(c( "A_1_��Դ�ṹ" ,"A_2_�����ṹ" ,"A_3_��Դǿ��","A_4_��Դ����_��ʯ��Դ","A_4_��Դ����_����","B_1_̼ǿ��" ,"B_2_�˾�̼�ŷ�"  ,"B_3_������Ⱦ" ,"C_2_Economic_Stucture_�ڶ���ҵ_�ɿ�ҵ��Առ��") ,names(Data_2))


Data_Normal = Data_2;

#����ָ��
for (i in Positive_index){
  for (j in 1:nrow(Data_2)){
    Data_Normal[j,i]<-(( Data_2[j,i]-Quan_mat[2,i])/(Quan_mat[1,i]-Quan_mat[2,i]))*100
  }
}

#����ָ��
for (i in Negative_index){
  for (j in 1:nrow(Data_2)){
    Data_Normal[j,i]<-(( Data_2[j,i]-Quan_mat[1,i])/(Quan_mat[2,i]-Quan_mat[1,i]))*100
  }
}


#top coding����ֵ����������100��Ϊ100��С��0��Ϊ0��

Data_Normal[Data_Normal<0] <- 0
Data_Normal[Data_Normal>100] <- 100

#�ӻر�ͷ
Data_Normal[,1:3]=Data_2[,1:3]

#ȥ���������
rm(Data_2,Data_temp,Data_mean,Data_select)

#------------------------------------
####Step B_5. ����&����ָ���Ĺ���
#------------------------------------

##################----------------------------------------------------------------###############
##################----------------------------------------------------------------###############
##################-------------------------ѡ����з�Χ---------------------------###############
##################------------------��׼�����������(282������)-------------------###############
##################----------------------------------------------------------------###############
##################----------------------------------------------------------------###############

#��ʼ��������---------------282��
length(unique(Data_Normal$����))

Data_select=Data_Normal

##################----------------------------------------------------------------###############
##################--------------------1.���Ҳ���ָ������--------------------------###############
##################----------------------------------------------------------------###############


#----------����ָ���Ĺ���------------#------------------------------------��ÿ1��һ��ָ����

#������ȼ�������
Data_temp<-Data_select%>% group_by(���)%>% summarise_all("mean",na.rm = TRUE)   

# #-----A ��ƽ��-----
# Results_National_1_detail=mutate(Data_temp, National_1 = rowMeans(Data_temp[,3:ncol(Data_temp)]))
# 
# #����������Ҳ�����
# Results_National_1=cbind(Results_National_1_detail[,1],Results_National_1_detail[,length(Results_National_1_detail)])
# 
# # #��������
# # write.csv(Results_National_1_detail, "Results_National_1_detail.csv", fileEncoding = "GBK",row.names = F)


#-----B ���ۿ�ܼ���----- �����ݾ��������������

#������ָ��
Results_National_2_A=(rowMeans(select(Data_temp[,-1], starts_with("A_1")))+rowMeans(select(Data_temp[,-1], starts_with("A_2")))+rowMeans(select(Data_temp[,-1], starts_with("A_3")))+rowMeans(select(Data_temp[,-1], starts_with("A_4"))))/4
Results_National_2_B=(rowMeans(select(Data_temp[,-1], starts_with("B_1")))+rowMeans(select(Data_temp[,-1], starts_with("B_2")))+rowMeans(select(Data_temp[,-1], starts_with("B_3"))))/3
Results_National_2_C=(rowMeans(select(Data_temp[,-1], starts_with("C_1")))+rowMeans(select(Data_temp[,-1], starts_with("C_2"))))/2
Results_National_2_D=(rowMeans(select(Data_temp[,-1], starts_with("D_1")))+rowMeans(select(Data_temp[,-1], starts_with("D_2")))+rowMeans(select(Data_temp[,-1], starts_with("D_3"))))/3
Results_National_2_E=(rowMeans(select(Data_temp[,-1], starts_with("E_1")))+rowMeans(select(Data_temp[,-1], starts_with("E_2")))+rowMeans(select(Data_temp[,-1], starts_with("E_3"))))/3
Results_National_2_F=(rowMeans(select(Data_temp[,-1], starts_with("F_1")))+rowMeans(select(Data_temp[,-1], starts_with("F_2")))+rowMeans(select(Data_temp[,-1], starts_with("F_3"))))/3

#������ָ��
Results_National_2_Energy=(Results_National_2_A+Results_National_2_B)/2
Results_National_2_Readiness=(Results_National_2_C+Results_National_2_D+Results_National_2_E+Results_National_2_F)/4

#������ָ��
Results_National_2_ETI=(Results_National_2_Energy+Results_National_2_Readiness)/2

#�ϲ�����
Results_National_2_detail=cbind(Data_temp,Results_National_2_A,Results_National_2_B,Results_National_2_C,Results_National_2_D,Results_National_2_E,Results_National_2_F,Results_National_2_Energy,Results_National_2_Readiness,Results_National_2_ETI)


#�޸�����
colnames(Results_National_2_detail)[(ncol(Results_National_2_detail)-8):ncol(Results_National_2_detail)]=c("National_2_A","National_2_B","National_2_C","National_2_D","National_2_E","National_2_F","National_2_Energy","National_2_Readiness","National_2_ETI")

# #����������Ҳ�����
# Results_National_2=cbind(Results_National_2_detail[,1],Results_National_2_detail[,length(Results_National_2_detail)])

#ȥ���������
rm(Results_National_2_A,Results_National_2_B,Results_National_2_C,Results_National_2_D,Results_National_2_E,Results_National_2_F,Results_National_2_Energy,Results_National_2_Readiness,Results_National_2_ETI,Data_temp)

#-----------------
#��������
#-----------------
write.csv(Results_National_2_detail, "Results_National_2_detail.csv", fileEncoding = "GBK",row.names = F)


##################----------------------------------------------------------------###############
##################--------------------2.���в���ָ������--------------------------###############
##################----------------------------------------------------------------###############

#----------����ָ���Ĺ���------------#------------------------------------��ÿ5��һ��ָ����

#����������ݣ��������
Data_select_���� = Data_select

#��ʶ���ݣ��Ա������λ��
Data_select_����=Data_select_���� %>% 
  mutate(mark = case_when(��� %in% c(2003,2004,2005,2006,2007) ~ 1, 
                            ��� %in% c(2008,2009,2010,2011,2012,2013,2014) ~ 2,
                            ��� %in% c(2015,2016,2017,2018,2019) ~ 3
  ))

#������λ��
Data_temp<-Data_select_����%>% group_by(����,mark)%>% summarise_all("median",na.rm = TRUE)   


# #-----A ��ƽ��-----
# 
# #������ͷ
# Data_temp_title=Data_temp[,1:4]
# 
# #������в���ָ��
# Data_temp=Data_temp[,5:ncol(Data_temp)] %>% mutate(city_1 = rowMeans(.,na.rm = TRUE))
# Results_����_1_detail=cbind(Data_temp_title,Data_temp)
# 
# #����������в�����
# Results_����_1=cbind(Results_����_1_detail[,1:4],Results_����_1_detail[,length(Results_����_1_detail)])
# 
# #ȥ���������
# rm(Data_temp_title,Data_temp)



#-----B ���ۿ�ܼ���----- �����ݾ��������������
#������λ��
Data_temp<-Data_select_����%>% group_by(����,mark)%>% summarise_all("median",na.rm = TRUE)   

#������ָ��
Results_City_2_A= (rowMeans(select(Data_temp[,-1], starts_with("A_1")))+rowMeans(select(Data_temp[,-1], starts_with("A_2")))+rowMeans(select(Data_temp[,-1], starts_with("A_3")))+rowMeans(select(Data_temp[,-1], starts_with("A_4"))))/4 #########---------------------------��һ��Ϊ����ѡ�����ķ��㣻��������һ����ѡ���ط��������
Results_City_2_B= (rowMeans(select(Data_temp[,-1], starts_with("B_1")))+rowMeans(select(Data_temp[,-1], starts_with("B_2")))+rowMeans(select(Data_temp[,-1], starts_with("B_3"))))/3
Results_City_2_C= (rowMeans(select(Data_temp[,-1], starts_with("C_1")))+rowMeans(select(Data_temp[,-1], starts_with("C_2"))))/2
Results_City_2_D= (rowMeans(select(Data_temp[,-1], starts_with("D_1")))+rowMeans(select(Data_temp[,-1], starts_with("D_2")))+rowMeans(select(Data_temp[,-1], starts_with("D_3"))))/3
Results_City_2_E= (rowMeans(select(Data_temp[,-1], starts_with("E_1")))+rowMeans(select(Data_temp[,-1], starts_with("E_2")))+rowMeans(select(Data_temp[,-1], starts_with("E_3"))))/3
Results_City_2_F= (rowMeans(select(Data_temp[,-1], starts_with("F_1")))+rowMeans(select(Data_temp[,-1], starts_with("F_2")))+rowMeans(select(Data_temp[,-1], starts_with("F_3"))))/3

#������ָ��
Results_City_2_Energy=(Results_City_2_A+Results_City_2_B)/2
Results_City_2_Readiness=(Results_City_2_C+Results_City_2_D+Results_City_2_E+Results_City_2_F)/4

#����ָ��
Results_City_2_ETI=(Results_City_2_Energy+Results_City_2_Readiness)/2

#�ϲ�����
Results_City_2_detail=cbind(Data_temp,Results_City_2_A,Results_City_2_B,Results_City_2_C,Results_City_2_D,Results_City_2_E,Results_City_2_F,Results_City_2_Energy,Results_City_2_Readiness,Results_City_2_ETI)

#�޸�����
colnames(Results_City_2_detail)[(ncol(Results_City_2_detail)-8):ncol(Results_City_2_detail)]=c("City_2_A","City_2_B","City_2_C","City_2_D","City_2_E","City_2_F","City_2_Energy","City_2_Readiness","City_2_ETI")

# #����������в�����
# Results_City_2=cbind(Results_City_2_detail[,1:4],Results_City_2_detail[,length(Results_City_2_detail)])

# #-----------------
# #�����ĳ�������ת��ΪӢ��
# #-----------------
# 
# for (i in 1:dim(Results_City_2_detail)[1]) {
#   Results_City_2_detail$����[i]<-city_name_list$English_Name[city_name_list$Chinese_Name==Results_City_2_detail$����[i]]
# }

#��������
write.csv(Results_City_2_detail, "Results_City_2_detail.csv", fileEncoding = "GBK",row.names = F)