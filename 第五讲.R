#####第五讲#####

#5.1 了解数据
data1<- read.csv("penguin_rawdata.csv")
gender <- c("男","女","女","男")
gender1 <- factor(gender,levels = c("男","女"))
sort(gender1)
score <- c(97,99,95,96)
letter <- c("A","A","B","B")
data2 <- data.frame()  

colnames(data1)
stress_data <- data1[,c()]
#"Nanjing" %in% data1$Site
#"Tsinghua" %in% data1$Site

#1）区分量表，如何把stress单独分出来
selected_cols <- c('Site',colnames(data1)[base::startsWith(colnames(data1),"stress")])
print(selected_cols)
stress_data <- data1[ ,selected_cols]

#alternative，注意，grep返回的是列的序号
stress_data <- data1[,c(1 ,grep("^stress",names(data1)))]
c(1,grep("^stress",names(data1)))#列的序号

#2）如何挑出行，只选清华
stress_data_thu <- stress_data[stress_data$Site=="Tsinghua",]


#3）计算每个被试“stress”的总分
stress_data_thu$stress_sum <- 
  rowSums(stress_data_thu[,grep("^stress",names(stress_data_thu))])

#4）stress总分的均值和标准差
mean(stress_data_thu$stress_sum, na.rm = TRUE)
sd(stress_data_thu$stress_sum, na.rm = TRUE)

#5.2数据类型的判断与转换
#判断
is.numeric(1.0)

#转换数值型
h <- as.numeric(c("1","2","3"))
j <- as.numeric(c("1","2","a"))
k <- as.numeric(c(T,F))
#如果强行NA该如何拆解，如何看转换之前结果是什么，避免盲目转换
#选择出相应的行，把它提出来

#转换字符型
l <- as.character(c("B","C","D"))
m <- as.character(c(T,F))
as.character(as.numeric(c("1","3","2")))

#####第六讲#########

#用tidyverse的反向分操作

#小技巧：假设需找到data中age大于30的所有行，并按照年龄排序
filtered_data <- dplyr::filter(data1,age>30)
filtered_sorted_data <- dplyr::arrange(filtered_data,age)

#使用管道操作符后，代码变为：
filtered_sorted_data <- data1%>%
  dplyr::filter(age>30)%>%
  dplyr::arrange(age)
# %>% 向右操作符，把左侧的数据或结果传递给右侧的函数或表达式进行运行
# %T>% 向左操作符，接受前一行的输出结果，但不会把自己的输出结果传入下一行
#传递的是%T>%前一行的输出结果
# %$% 解释操作符，传递输出数据框列名，可以允许后一行代码直接根据列名调用相应的数据
# %<>% 复合赋值操作符，在整段代码运行完后，将运行结果直接返回给%<>%前面的变量并保存下来
#省去了再次命名的步骤

#小练习

#创建dataframe
data <- data.frame(
  "grammer"=c('R',"SPSS","Python","R",NA,"Matlab","Python",'R'),
  "score"=c(4,2,5,4.5,5,4,2,5),
  "popularity"=c(1,2,NA,4,5,6,7,10)
  )
#提取前两列
select_data1 <- data[,1:2]
#提取含字符串“R”的行
select_data2 <- select_data1[select_data1$grammer=="R",]
#用管道提取R
library(tidyverse)
select_data3 <- data%>%
  dplyr::select(grammer,score)%>%
  dplyr::filter(grammer =='R')


#反向积分关键函数
#mutate,case_when
#将4、12、14、16题反向计分，计算ALEX,保存为ALEX
data2 <- data1 %>%
  dplyr::mutate(data1,
             ALEX4=case_when(TRUE~ 6 - ALEX4),
             ALEX12 = case_when(TRUE~ 6 - ALEX12),
             ALEX14 = case_when(TRUE~ 6 - ALEX14),
             ALEX16 = case_when(TRUE~ 6 - ALEX16))
#操作步骤：
#step1:选择变量[select]
#step2:检查数据类型[glimpse，as族函数]
#step3:处理缺失值[filter，is.na]
#反向计分[mutate,case_when]
#计算所需变量[mutate]
#分组求统计量[group_by,summarise]

#6.2用tidyverse处理问卷数据
#6.2.1 操作步骤
#6.2.1.1选择变量
#选择我们需要的变量：Temperature_t1,Temperature_t2,SNI28-32,DEQ,romantic,ALEX1-16
df2 <- data1%>%
  dplyr::select(Temperature_t1,Temperature_t2,
                socialdiversity,Site,DEQ,
                romantic,ALEX1:ALEX16)
#检查变量的数据类型
base::summary(df2)
#转换数据类型
#这里的数据类型是正确的，只是示例
df2 <- df2 %>%
  dplyr::mutate(Temperature_t1_new = as.numeric(Temperature_t1),
                Temperature_t2 = as.numeric(Temperature_t2))

#6.2.1.2，step3:处理缺失值
#按照Temperature，DEQ处理缺失值
df2 <- df2 %>%
  dplyr::filter(!is.na(Temperature_t1)&!is.na(Temperature_t2)& !is.na(DEQ))
#filter意味着选中，！是否定符号，表示如果是na就不选中。

#6.2.1.3，step4:计算所需变量
#计算两次核心温度的均值，保存为Temperature
df2 <- df2 %>%
  dplyr::mutate(Temperature = rowMeans(select(df2,starts_with("Temperature"))))
#计算ALEX
df2 <- df2 %>%
  dplyr::mutate(ALEX=rowMeans(select(df2,starts_with("ALEX"))))

#6.2.1.3，step6:分组描述
#按site计算Temperature的平均值
df2 <- dplyr::group_by(df2,Site)
df2 <- dplyr::summarise(df2,mean_Temperature = mean(Temperature),n = n())
df2 <- dplyr::ungroup(df2)

# 6.2.2小结
#已截图

#6.2.3 练习
#按照langfamily进行分组计算DEQ,CSI的均值
df4 <- data1%>%
  dplyr::select(Site,langfamily,DEQ,socialdiversity) %>%
  dplyr::filter(!is.na(DEQ)&!is.na(socialdiversity)& !is.na(langfamily))%>%
  #dplyr::mutate(meanDEQ = rowMeans(select(df4,starts_with("DEQ"))))%>%
  #dplyr::mutate(meansocialdiversity = rowMeans(select(df4,starts_with("socialdiversity"))))%>%
  dplyr::group_by(langfamily)%>%
  dplyr::summarise(mean_DEQ = mean(DEQ), mean_socialdiversity = mean(socialdiversity))%>%
  dplyr::ungroup()

#6.3反应时数据
#6.3.1 问题&数据
#数据以被试编号分为txt，都是单个被试
#6.3.2操作步骤
#1.批量读取并合并数据[for loop]
#2.选择变量[select]
#3.处理缺失值[drop_na,filter]
#4.分实验条件计算变量[group_by,summarise]
#5.拆分变量[extract,filter]
#6.将长数据转为宽数据[pivot_wide]
#7.计算实验条件为Match-Moral时RT的自我优势效应spe[mutate,select]

#










