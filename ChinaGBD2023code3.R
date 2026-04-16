library(dplyr)
library(reshape2)
library(tidyverse)
library(scales)
library(ggplot2)
library(ggsci)


########  原始数据 #######
data <- readRDS("GBD2023.rds")
colnames(data)
colnames(data) <- c("measure_name","location_name",
                    "sex_name","age_name","cause_name","rei_name","metric_name","year",
                    "val","upper","lower")
data <- data[,c("measure_name","location_name","sex_name","age_name","cause_name","rei_name","metric_name","year","val","upper","lower")]


data$measure_name[data$measure_name=="DALYs (Disability-Adjusted Life Years)"] <- "DALYs"
data$location_name[data$location_name=="People's Republic of China"] <- "China"

#################################不同省份-性别差异条图####################################
data3 <- data %>%
  dcast(year + location_name  + sex_name + rei_name + cause_name ~ measure_name + metric_name + age_name,
        value.var = "val")

data3 <- data3%>%
  filter(year=="2023"&cause_name=="Non-communicable diseases"&sex_name%in%c("Male","Female"))%>%
  select("year","location_name","sex_name","rei_name","cause_name",
         "DALYs_Number_All ages","DALYs_Rate_Age-standardized",
         "Deaths_Number_All ages","Deaths_Rate_Age-standardized")

colnames(data3) <- c("year","location_name","sex_name","rei_name","cause_name",
                     "DALYs_Number_All_ages","DALYs_Rate_Age_standardized","Deaths_Number_All_ages", "Deaths_Rate_Age_standardized")

data3 <- data3%>%
  mutate(location_name=case_when(location_name=="Macao Special Administrative Region of China"~"Macao",
                                 location_name=="Hong Kong Special Administrative Region of China"~"Hong kong",
                                 TRUE~location_name))


data3_pro <- data3%>%
  filter(rei_name=="Dietary risks")%>%
  select("year","location_name","sex_name","rei_name","cause_name",
         "DALYs_Number_All_ages","DALYs_Rate_Age_standardized",
         "Deaths_Number_All_ages","Deaths_Rate_Age_standardized")

##############################################DALY

barplot <- data3_pro%>%
  filter(location_name!="China")%>%
  group_by(location_name)%>%
  mutate(sumval=sum(DALYs_Rate_Age_standardized))%>%
  ungroup()%>%
  arrange(sumval)%>%
  mutate(location_name=factor(location_name,level=rev(unique(location_name))))%>%
  mutate(sex_name=factor(sex_name,level=c("Male","Female")))%>%
  ggplot(aes(x=location_name, y=DALYs_Rate_Age_standardized, fill=sex_name, group=sex_name)) +
  geom_bar(stat="identity", position="dodge") +  # 用 "identity" 让 y 值直接映射
  scale_fill_manual(values = c("Male"="#4387B5","Female"="#BD6263"))+
  theme_minimal() +  
  labs(x="Province", y="Age-standardized DALYs Rate (per 100,000 population)", 
       #title="Diet attributable NCDs DALYs Rate by sex among provinces ",
       fill="Sex"
  ) +  # 添加轴标签和标题
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=18,family = "Arial"),
        panel.background = element_rect(fill = "white", colour = NA),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA))  


ggsave("result/DALYs burden by  provinces and Sex.png",barplot,dpi = 300,width =20,height=10,limitsize = FALSE,bg = "white")

##########################################################Death
barplot <- data3_pro%>%
  filter(location_name!="China")%>%
  group_by(location_name)%>%
  mutate(sumval=sum(Deaths_Rate_Age_standardized))%>%
  ungroup()%>%
  arrange(sumval)%>%
  mutate(location_name=factor(location_name,level=rev(unique(location_name))))%>%
  mutate(sex_name=factor(sex_name,level=c("Male","Female")))%>%
  ggplot(aes(x=location_name, y=Deaths_Rate_Age_standardized, fill=sex_name, group=sex_name)) +
  geom_bar(stat="identity", position="dodge") +  # 用 "identity" 让 y 值直接映射
  scale_fill_manual(values = c("Male"="#4387B5","Female"="#BD6263"))+
  theme_minimal() +  
  labs(x="Province", y="Age-standardized Death Rate (per 100,000 population)", 
       #title="Diet attributable NCDs Death Rate by sex among provinces ",
       fill="Sex"
  ) +  # 添加轴标签和标题
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=18,family = "Arial"),
        panel.background = element_rect(fill = "white", colour = NA),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA))  


ggsave("result/Death burden by  provinces and Sex.png",barplot,dpi = 300,width =20,height=10,limitsize = FALSE,bg = "white")


#################################不同因素-性别差异条图####################################
data3_rei <- data3%>%
  filter(location_name=="China")%>%
  select("year","location_name","sex_name","rei_name","cause_name",
         "DALYs_Number_All_ages","DALYs_Rate_Age_standardized",
         "Deaths_Number_All_ages","Deaths_Rate_Age_standardized")

##############################################DALY
plot_by_rei_scaled <- function(data) {
  scale_factor <- 1e4  # 定义缩放因子
  
  # 数据转换并按照 Number 排序
  data_long <- data %>%
    select(rei_name, sex_name, DALYs_Number_All_ages, DALYs_Rate_Age_standardized) %>%
    group_by(rei_name) %>%
    mutate(total_number = sum(DALYs_Number_All_ages, na.rm = TRUE)) %>%  # 计算排序依据
    ungroup() %>%
    arrange(total_number) %>%  # 按总数降序排列
    mutate(rei_name = factor(rei_name, levels = unique(rei_name))) %>%  # 调整因子顺序
    mutate(scaled_rate = DALYs_Rate_Age_standardized * scale_factor,  # 缩放
           scaled_rate = -scaled_rate) %>%  # 设置为负值
    pivot_longer(cols = c(DALYs_Number_All_ages, scaled_rate),
                 names_to = "variable", values_to = "value")
  
  # 找到轴范围
  y_limits <- range(c(data_long$value), na.rm = TRUE)
  
  # 绘图
  ggplot(data_long, aes(x = rei_name, y = value, fill = sex_name)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    coord_flip() +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    labs(#title = "DALYs burden of dietary risk  by Sex",
      x = "Risk Name",
      y = "DALYs Rate(left)        Number (right)") +
    scale_y_continuous(
      # breaks = seq(-y_limits[2], y_limits[2], length.out = 6),
      labels = function(x) comma(abs(x) / ifelse(x < 0, scale_factor, 1))  # 标签对应原始值
    ) +
    scale_fill_manual(values = setNames(c("skyblue", "lightcoral"),c("Male","Female")),
                      name = "Sex") +
    theme_minimal() +
    theme(text = element_text(size = 15),
          axis.title.y = element_text(angle = 90,hjust = 0.5,vjust = 0.5),
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "right")
}

# 执行函数
bibar2 <- plot_by_rei_scaled(data3_rei)
ggsave("result/DALYs burden by Dietary risk and Sex.png",bibar2,dpi = 300,width =20,height=10,limitsize = FALSE,bg = "white")

##########################################################Death
plot_by_rei_scaled <- function(data) {
  scale_factor <- 1e4  # 定义缩放因子
  
  # 数据转换并按照 Number 排序
  data_long <- data %>%
    select(rei_name, sex_name, Deaths_Number_All_ages, Deaths_Rate_Age_standardized) %>%
    group_by(rei_name) %>%
    mutate(total_number = sum(Deaths_Number_All_ages, na.rm = TRUE)) %>%  # 计算排序依据
    ungroup() %>%
    arrange(total_number) %>%  # 按总数降序排列
    mutate(rei_name = factor(rei_name, levels = unique(rei_name))) %>%  # 调整因子顺序
    mutate(scaled_rate = Deaths_Rate_Age_standardized * scale_factor,  # 缩放
           scaled_rate = -scaled_rate) %>%  # 设置为负值
    pivot_longer(cols = c(Deaths_Number_All_ages, scaled_rate),
                 names_to = "variable", values_to = "value")
  
  # 找到轴范围
  y_limits <- range(c(data_long$value), na.rm = TRUE)
  
  # 绘图
  ggplot(data_long, aes(x = rei_name, y = value, fill = sex_name)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    coord_flip() +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    labs(#title = "Deaths burden of dietary risk  by Sex",
      x = "Risk Name",
      y = "Death Rate(left)                                Number (right)") +
    scale_y_continuous(
      # breaks = seq(-y_limits[2], y_limits[2], length.out = 6),
      labels = function(x)comma(abs(x) / ifelse(x < 0, scale_factor, 1))  # 标签对应原始值
    ) +
    scale_fill_manual(values = setNames(c("skyblue", "lightcoral"),c("Male","Female")),
                      name = "Sex") +
    theme_minimal() +
    theme(text = element_text(size = 15),
          axis.title.y = element_text(angle = 90,hjust = 0.5,vjust = 0.5),
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "right")
}

# 执行函数
bibar2 <- plot_by_rei_scaled(data3_rei)
ggsave("result/Deaths burden by Dietary risk and Sex.png",bibar2,dpi = 300,width =20,height=10,limitsize = FALSE,bg = "white")




###############################不同年龄组条图+线图############################################
data1val <- data %>%
  dcast(year + location_name + age_name + rei_name + cause_name ~ measure_name + metric_name + sex_name,
        value.var = "val")

data1L <- data %>%
  dcast(year + location_name + age_name + rei_name + cause_name ~ measure_name + metric_name + sex_name,
        value.var = "lower")
colnames(data1L)[6:ncol(data1L)] <- paste0(colnames(data1L)[6:ncol(data1L)], "_L")

data1U <- data %>%
  dcast(year + location_name + age_name + rei_name + cause_name ~ measure_name + metric_name + sex_name,
        value.var = "upper")
colnames(data1U)[6:ncol(data1U)] <- paste0(colnames(data1U)[6:ncol(data1U)], "_U")

# 合并数据框
data1 <- merge(merge(data1val, data1L, by = c("year", "location_name", "age_name", "rei_name", "cause_name")),
               data1U, by = c("year", "location_name", "age_name", "rei_name", "cause_name"))

#saveRDS(data1,"data1.rds")

rm(data1L,data1U,data1val)


############作图
datafig_age <- data1%>%
  filter(age_name%in%agegroup3&location_name=="China"&cause_name=="Non-communicable diseases"&rei_name=="Dietary risks")%>%
  select(year,location_name,age_name,rei_name,cause_name,
         DALYs_Number_Both,DALYs_Rate_Both,Deaths_Number_Both,Deaths_Rate_Both)



theme1 <- function(){
  theme_minimal()+
    theme(
      text = element_text(size = 26, family = "Arial"),
      title = element_text(size = 28, hjust = 0.5),
      axis.title = element_text(size = 28),
      axis.text = element_text(size = 26, hjust = 1, vjust = 1),
      legend.title = element_text(size = 28),
      legend.text = element_text(size = 26),
      panel.background = element_rect(fill = "white", colour = NA),  
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      axis.line = element_line(color = "black", linewidth = 0.7),  
      strip.background =  element_rect(fill = "white", colour = NA),
      strip.text = element_text(size = 28, family = "Arial"),
      legend.key.width = unit(5, "line")      # 增加图例线条的宽度
    )
}

#install.packages("patchwork")
library(patchwork)
library(scales)

# 创建第一个图（死亡数条形图）
p1 <- ggplot(datafig_age, aes(x = year, group = age_name, fill = age_name)) +
  geom_bar(aes(y = Deaths_Number_Both), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Death Number",
                     labels = label_comma()) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020,2023)) +
  scale_fill_lancet(name = "Age Group") +
  #labs(title = "NCDs Death burden from 1990 to 2023 by age group in China") +
  theme1() +
  theme(
    legend.key.height = unit(1.0, 'cm'),
    legend.spacing.y = unit(0.5, 'cm')
  )

# 创建第二个图（死亡率折线图）
p2 <- ggplot(datafig_age, aes(x = year, group = age_name, color = age_name)) +
  geom_line(aes(y = Deaths_Rate_Both), linewidth = 1.5) +  # 使用死亡率
  scale_y_continuous(name = "Age-standardized Death Rate (per 100,000 population)",
                     labels = label_comma()) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020,2023)) +
  scale_color_lancet(name = "Age Group") +
  theme1() +
  theme(
    legend.key.height = unit(1.0, 'cm'),
    legend.spacing.y = unit(0.5, 'cm')
  )

# 使用patchwork将两个图拼接
fig3_deaths <- p1 / p2



ggsave("result/fig3_deaths3段年龄.png",fig3_deaths,dpi = 300,width =20,height=20)

######DALYs
# 创建第一个图（死亡数条形图）
p1 <- ggplot(datafig_age, aes(x = year, group = age_name, fill = age_name)) +
  geom_bar(aes(y = DALYs_Number_Both), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "DALYs Number",
                     labels = label_comma()) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020,2023)) +
  scale_fill_lancet(name = "Age Group") +
  #labs(title = "NCDs DALYs burden from 1990 to 2023 by age group in China") +
  theme1() +
  theme(
    legend.key.height = unit(1.0, 'cm'),
    legend.spacing.y = unit(0.5, 'cm')
  )

# 创建第二个图（死亡率折线图）
p2 <- ggplot(datafig_age, aes(x = year, group = age_name, color = age_name)) +
  geom_line(aes(y = DALYs_Rate_Both), linewidth = 1.5) +  # 使用死亡率
  scale_y_continuous(name = "Age-standardized DALYs Rate (per 100,000 population)",
                     labels = label_comma()) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020,2023)) +
  scale_color_lancet(name = "Age Group") +
  theme1() +
  theme(
    legend.key.height = unit(1.0, 'cm'),
    legend.spacing.y = unit(0.5, 'cm')
  )

# 使用patchwork将两个图拼接
fig3_DALYs <- p1 / p2
ggsave("result/fig3_DALYs3段年龄.png",fig3_DALYs,dpi = 300,width =20,height=20)



#########################生成23年负担和负担change数据框，用于绘制地图#####################

############合并地图的change数据框
data6 <- data%>%
  filter(sex_name=="Both"&
           year%in%c("1990","2023")&age_name%in%c("Age-standardized","All ages"))

data7 <- dcast(data6,location_name+sex_name
               +age_name+cause_name+rei_name+metric_name~measure_name+year,
               value.var = "val")
data7 <- data7%>%
  transmute(location_name,
            sex_name,
            age_name,
            cause_name,
            rei_name,
            metric_name,
            DALYs_change=(DALYs_2023-DALYs_1990)*100/abs(DALYs_1990),
            Deaths_change=(Deaths_2023-Deaths_1990)*100/abs(Deaths_1990))

####Number
level1change_num <- data7%>%
  filter(age_name=="All ages"&metric_name=="Number")

write.csv(level1change_num,"result/level1change/level1change_num.csv")

####Rate
level1change_rat <- data7%>%
  filter(age_name=="Age-standardized"&metric_name=="Rate")

write.csv(level1change_rat,"result/level1change/level1change_rat.csv")


####红肉
level1change_rat_redmeat <- level1change_rat%>%
  filter(rei_name=="Diet high in red meat")
write.csv(level1change_rat_redmeat,"result/level1change/level1change_rat_redmeat.csv")



############合并地图的2023疾病负担数据框
#####Number
data_mapNumber <- data%>%
  filter(location_name!="China"&age_name%in%c("All ages")&sex_name=="Both"&year%in%c("1990","2023")&metric_name%in%c("Number"))%>%
  select(location_name,year,age_name,rei_name,cause_name,measure_name,metric_name,val)

write.csv(data_mapNumber,"result/data_mapNumber.csv")

####Rate
data_mapRate <- data%>%
  filter(location_name!="China"&age_name%in%c("Age-standardized")&sex_name=="Both"&year%in%c("1990","2023")&metric_name%in%c("Rate"))%>%
  select(location_name,year,age_name,rei_name,cause_name,measure_name,metric_name,val)

write.csv(data_mapRate,"result/data_mapRate.csv")





















######change数据框
data21 <- data%>%filter(year%in%c("1990","2023")
                        &age_name=="Age-standardized"&metric_name=="Rate"
                        &sex_name=="Both")%>%
  select("location_name",
         "sex_name","age_name","cause_name","rei_name","metric_name","measure_name","year",
         "val")

data21change <- data21%>%
  dcast(location_name+sex_name+age_name+cause_name+rei_name+metric_name~measure_name+year,value.var = "val")

data21change <- data21change%>%
  mutate(DALYs_change=DALYs2023-DALYs1990,
         Deaths_change=Deaths2023-Deaths1990)%>%
  select("location_name",
         "sex_name","age_name","cause_name","rei_name","metric_name","DALYs_change","Deaths_change")


data22 <- data%>%filter(year%in%c(1990,2023)
                        &age_name%in%c("All ages")&metric_name%in%c("Number")
                        &sex_name=="Both")%>%
  select("location_name",
         "sex_name","age_name","cause_name","rei_name","metric_name","measure_name","year",
         "val")








#######################男女性别差异表格###########################


########分地区 

data1d2 <- data1%>%
  mutate_at(vars(6:ncol(.)), ~round(., 2)) 
  
data4 <- data1d2%>%
  filter(year=="2023"&age_name%in%c("Age-standardized")&rei_name=="Dietary risks"&cause_name=="Non-communicable diseases")%>%
  transmute(
    location_name,
    Deaths_Rate_Male, Deaths_Rate_Male_L, Deaths_Rate_Male_U,
    Deaths_Rate_Female, Deaths_Rate_Female_L,Deaths_Rate_Female_U,
    
    DALYs_Rate_Male, DALYs_Rate_Male_L, DALYs_Rate_Male_U,
    DALYs_Rate_Female, DALYs_Rate_Female_L, DALYs_Rate_Female_U,
      
    Deaths_Rate_dif=Deaths_Rate_Male-Deaths_Rate_Female,
    DALYs_Rate_dif=DALYs_Rate_Male-DALYs_Rate_Female,
    
    Deaths_Rate_dif_rank = rank(if_else(location_name == "China", NA_real_, Deaths_Rate_dif), na.last = "keep"),
    DALYs_Rate_dif_rank = rank(if_else(location_name == "China", NA_real_,DALYs_Rate_dif), na.last = "keep")
    )%>%
  mutate(
    Deaths_Rate_MALE=paste0(Deaths_Rate_Male,"(",Deaths_Rate_Male_L,",", Deaths_Rate_Male_U,")"),
    Deaths_Rate_FEMALE=paste0(Deaths_Rate_Female,"(",Deaths_Rate_Female_L,",", Deaths_Rate_Female_U,")"),
    DALYs_Rate_MALE=paste0(DALYs_Rate_Male,"(",DALYs_Rate_Male_L,",", DALYs_Rate_Male_U,")"),
    DALYs_Rate_FEMALE=paste0(DALYs_Rate_Female,"(",DALYs_Rate_Female_L,",", DALYs_Rate_Female_U,")")
    
  )%>%
  dplyr::select(location_name,Deaths_Rate_MALE,Deaths_Rate_FEMALE,Deaths_Rate_dif,Deaths_Rate_dif_rank,
                DALYs_Rate_MALE,DALYs_Rate_FEMALE,DALYs_Rate_dif,DALYs_Rate_dif_rank)
    
    
write.csv(data4,"result/性别差异全因分省份.csv")

#####分因素
data5 <- data1d2%>%
  filter(location_name=="China"&year=="2023"&age_name%in%c("Age-standardized")&cause_name=="Non-communicable diseases")%>%
  transmute(
    rei_name,
    Deaths_Rate_Male, Deaths_Rate_Male_L, Deaths_Rate_Male_U,
    Deaths_Rate_Female, Deaths_Rate_Female_L,Deaths_Rate_Female_U,
    
    DALYs_Rate_Male, DALYs_Rate_Male_L, DALYs_Rate_Male_U,
    DALYs_Rate_Female, DALYs_Rate_Female_L, DALYs_Rate_Female_U,
    
    Deaths_Rate_dif=Deaths_Rate_Male-Deaths_Rate_Female,
    DALYs_Rate_dif=DALYs_Rate_Male-DALYs_Rate_Female,
    
    Deaths_Rate_dif_rank = rank(Deaths_Rate_dif),
    DALYs_Rate_dif_rank = rank(DALYs_Rate_dif)
  )%>%
  mutate(
    Deaths_Rate_MALE=paste0(Deaths_Rate_Male,"(",Deaths_Rate_Male_L,",", Deaths_Rate_Male_U,")"),
    Deaths_Rate_FEMALE=paste0(Deaths_Rate_Female,"(",Deaths_Rate_Female_L,",", Deaths_Rate_Female_U,")"),
    DALYs_Rate_MALE=paste0(DALYs_Rate_Male,"(",DALYs_Rate_Male_L,",", DALYs_Rate_Male_U,")"),
    DALYs_Rate_FEMALE=paste0(DALYs_Rate_Female,"(",DALYs_Rate_Female_L,",", DALYs_Rate_Female_U,")")
    
  )%>%
  dplyr::select( rei_name,Deaths_Rate_MALE,Deaths_Rate_FEMALE,Deaths_Rate_dif,Deaths_Rate_dif_rank,
                DALYs_Rate_MALE,DALYs_Rate_FEMALE,DALYs_Rate_dif,DALYs_Rate_dif_rank)

write.csv(data5,"result/性别差异全国分因素.csv")
