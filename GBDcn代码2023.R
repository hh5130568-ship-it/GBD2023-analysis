library(ggplot2)
library(ggsci)
library(dplyr)
library(extrafont)
library(patchwork)
library(cowplot)
loadfonts(device = "win")




##########################################15种因素合并地图#####################
################################ 1.导入地图数据###########################

#####外源地图shp文件
library(sf)



china_sf1 <-  st_read("data/mapdata/中国_省shp/中国_省1.shp")
china_sf2 <-  st_read("data/mapdata/中国_省shp/中国_省2.shp")
china_sf <- rbind(china_sf1,china_sf2)

china_sf <- china_sf %>%
  mutate(name = case_when(
    name == "安徽省" ~ "Anhui",
    name == "北京市" ~ "Beijing",
    name == "重庆市" ~ "Chongqing",
    name == "福建省" ~ "Fujian",
    name == "甘肃省" ~ "Gansu",
    name == "广东省" ~ "Guangdong",
    name == "广西壮族自治区" ~ "Guangxi",
    name == "贵州省" ~ "Guizhou",
    name == "海南省" ~ "Hainan",
    name == "河北省" ~ "Hebei",
    name == "黑龙江省" ~ "Heilongjiang",
    name == "河南省" ~ "Henan",
    name == "湖北省" ~ "Hubei",
    name == "湖南省" ~ "Hunan",
    name == "江苏省" ~ "Jiangsu",
    name == "江西省" ~ "Jiangxi",
    name == "吉林省" ~ "Jilin",
    name == "辽宁省" ~ "Liaoning",
    name == "内蒙古自治区" ~ "Inner Mongolia",
    name == "宁夏回族自治区" ~ "Ningxia",
    name == "青海省" ~ "Qinghai",
    name == "山东省" ~ "Shandong",
    name == "山西省" ~ "Shanxi",
    name == "陕西省" ~ "Shaanxi",
    name == "上海市" ~ "Shanghai",
    name == "四川省" ~ "Sichuan",
    name == "天津市" ~ "Tianjin",
    name == "西藏自治区" ~ "Tibet",
    name == "新疆维吾尔自治区" ~ "Xinjiang",
    name == "云南省" ~ "Yunnan",
    name == "浙江省" ~ "Zhejiang",
    name == "澳门特别行政区" ~ "Macao",
    name == "香港特别行政区" ~ "Hong Kong",
    name == "台湾省" ~ "Taiwan",
    TRUE ~ name  # 保留原始名称以防有未覆盖的情况
  ))


################################2.疾病负担数据90 21 number rate  change############

data_mapPer0 <- read.csv("result/level1change/level1change_num.csv")
colnames(data_mapPer0)
data_mapPer0 <- data_mapPer0[,c("location_name","rei_name","cause_name",
                                "metric_name","DALYs_change","Deaths_change")]

data_mapPer <- read.csv("result/level1change/level1change_rat.csv")
colnames(data_mapPer)
data_mapPer <- data_mapPer[,c("location_name","rei_name","cause_name",
                              "metric_name","DALYs_change","Deaths_change")]

data_mapPer1 <- rbind(data_mapPer0,data_mapPer)
data_mapPer1 <- left_join(china_sf,data_mapPer1,by=c("name"="location_name"))


#########3. 定义绘图函数并绘制地图##############

#因素（无红肉）
rei_name <- c("Diet low in fiber","Diet low in whole grains"
              ,"Diet high in processed meat","Diet high in sugar-sweetened beverages"
              ,"Diet high in sodium","Dietary risks","Diet low in calcium","Diet low in fruits"
              ,"Diet low in vegetables","Diet low in omega-6 polyunsaturated fatty acids"
              ,"Diet low in legumes","Diet low in seafood omega-3 fatty acids"
              ,"Diet high in trans fatty acids","Diet low in nuts and seeds","Diet low in milk"
)
cause_name <- c("Non-communicable diseases")


data_process <- function(Metric_name,Year){
  for (reiname in rei_name) {
    for (causename in cause_name) {
      ################################疾病负担图
      data_map0 <- read.csv(ifelse(Metric_name=="Number","result/data_mapNumber.csv",
                                   "result/data_mapRate.csv"))
      data_map <- data_map0%>%
        mutate(location_name=case_when(
          location_name=="Hong Kong Special Administrative Region of China"~"Hong Kong",
          location_name=="Macao Special Administrative Region of China"~"Macao",
          TRUE~location_name
        ))%>%
        filter(cause_name==causename&rei_name==reiname&year==Year&metric_name==Metric_name)
      
      data_map1 <-data_map %>% filter(measure_name == "DALYs")
      if(nrow(data_map1) > 0){
        data_map1 <- left_join(china_sf,data_map1,by=c("name"="location_name"))
        
        main_map1<- ggplot(data = data_map1) +
          geom_sf(aes(fill = val), color = "black", size = 0.1) +  # 使用 fill 映射 DALYs_All 到颜色
          coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
          scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "grey80")+    
          labs(fill =paste0("DALYs"," ",Metric_name)
               , title = paste0(reiname)
          ) +  # 添加图例标题和图表标题
          theme_minimal() +  # 使用简洁主题
          theme(legend.position = "right",
                legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 50),
                legend.text = element_text(size = 45),
                plot.title = element_text(size = 60),
                panel.background = element_rect(fill = "white", colour = NA), 
                plot.background = element_rect(fill = "white", colour = NA),
                panel.grid = element_blank(),
                axis.title = element_blank(),  # 移除轴标题
                axis.text = element_blank(),   # 移除轴文本
                axis.ticks = element_blank(),text = element_text(family = "Arial"),
                legend.key.height = unit(2, "cm"))
        
        southsea_map1 <- ggplot(data = data_map1)+
          geom_sf(aes(fill=val),color="black",size=0.1)+
          scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "gray80")+
          coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
          theme_void()+
          theme(legend.position = "none") 
        
        
        fig_DALYs <- main_map1+inset_element(southsea_map1,left = 0.8,right = 1.0,bottom = 0.1,top = 0.3)
        assign(paste0("fig_DALYs", reiname), fig_DALYs, envir = .GlobalEnv)
      }
      
      data_map2 <- data_map %>% filter(measure_name == "Deaths")
      if(nrow(data_map2) > 0){
        data_map2 <- left_join(china_sf,data_map2,by=c("name"="location_name"))
        
        main_map2<- ggplot(data = data_map2) +
          geom_sf(aes(fill = val), color = "black", size = 0.1) +  # 使用 fill 映射 DALYs_All 到颜色
          coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
          scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "grey80")+    
          labs(fill = paste0("Death"," ",Metric_name)
               , title = paste0(reiname)
          ) +  # 添加图例标题和图表标题
          theme_minimal() +  # 使用简洁主题
          theme(legend.position = "right",
                legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 50),
                legend.text = element_text(size = 45),
                plot.title = element_text(size = 60),
                panel.background = element_rect(fill = "white", colour = NA), 
                plot.background = element_rect(fill = "white", colour = NA),
                panel.grid = element_blank(),
                axis.title = element_blank(),  # 移除轴标题
                axis.text = element_blank(),   # 移除轴文本
                axis.ticks = element_blank(),text = element_text(family = "Arial"),
                legend.key.height = unit(2, "cm"))
        
        southsea_map2 <- ggplot(data = data_map2) +
          geom_sf(aes(fill = val), color = "black", size = 0.1) +  # 使用 fill 映射 DALYs_All 到颜色
          scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "gray80")+
          coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
          theme_void()+
          theme(legend.position = "none")
        
        fig_Deaths <- main_map2+inset_element(southsea_map2,left = 0.8,right = 1.0,top = 0.3,bottom = 0.1)
        assign(paste0("fig_Deaths", reiname), fig_Deaths, envir = .GlobalEnv)
      }
      
      
      #################################变化图
      
      data_mapPer2 <- data_mapPer1 %>%
        filter(name=="境界线"|name=="Taiwan"|(rei_name == reiname &cause_name==causename
                                           &metric_name==Metric_name))
      if(nrow(data_mapPer2) > 8){
        ##DALYS 
        
        quantiles <- seq(0, 1, length.out = 11)  
        quantile_values <- quantile(data_mapPer2$DALYs_change, probs = quantiles,na.rm=TRUE)
        
        # 根据分位数将数据进行分段
        data_mapPer2$DALYs_change_cut <- cut(data_mapPer2$DALYs_change, 
                                             breaks = quantile_values, 
                                             include.lowest = TRUE, 
                                             labels = FALSE)
        
        # 创建每个区间的原始值标签
        labels <- sapply(1:(length(quantile_values) - 1), function(i) {
          if (i == 1) {
            # 第一个标签显示完整区间范围
            paste0(round(quantile_values[i], 1), " ~ ", round(quantile_values[i + 1], 1))
          } else if (i == length(quantile_values) - 1) {
            # 最后一个标签显示完整区间范围
            paste0(round(quantile_values[i], 1), " ~ ", round(quantile_values[i + 1], 1))
          } else {
            # 中间的标签只显示最小值
            paste0(round(quantile_values[i], 1), "~")
          }
        })
        
        # 设置自定义颜色
        colors <- colorRampPalette(c("#5eaaf5", "#f4d963", "red"))(10)  # 生成33个颜色
        
        
        main_map3<- ggplot(data = data_mapPer2) +
          geom_sf(aes(fill = as.factor(DALYs_change_cut)), color = "black", size = 0.1) +  # 使用分段数据映射颜色
          coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
          scale_fill_manual(values = colors, labels = labels,na.value = "grey80") +  # 自定义颜色和标签
          labs(fill = "Percentage change (%)"
               ,title = paste0(reiname)
          ) +
          theme_minimal() +  # 使用简洁主题
          theme(legend.position = "right",
                legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 50),
                legend.text = element_text(size = 45),
                plot.title = element_text(size = 60),
                panel.background = element_rect(fill = "white", colour = NA), 
                plot.background = element_rect(fill = "white", colour = NA),
                panel.grid = element_blank(),
                axis.title = element_blank(),  # 移除轴标题
                axis.text = element_blank(),   # 移除轴文本
                axis.ticks = element_blank(),text = element_text(family = "Arial"))
        
        southsea_map3 <- ggplot(data = data_mapPer2)+
          geom_sf(aes(fill = as.factor(DALYs_change_cut)),color="black",size=0.1)+
          scale_fill_manual(values = colors, labels = labels,na.value = "grey80") +  # 自定义颜色和标签
          coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
          theme_void()+
          theme(legend.position = "none") 
        
        
        fig_DALYs_Per <- main_map3+inset_element(southsea_map3,left = 0.8,right = 1.0,bottom = 0.1,top = 0.3)
        
        assign(paste0("fig_DALYs_Per", reiname), fig_DALYs_Per, envir = .GlobalEnv)
        
        
        ##DeathS 
        
        quantiles <- seq(0, 1, length.out = 11)  
        quantile_values1 <- quantile(data_mapPer2$Deaths_change, probs = quantiles,na.rm=TRUE)
        
        # 根据分位数将数据进行分段
        data_mapPer2$Deaths_change_cut <- cut(data_mapPer2$Deaths_change, 
                                              breaks = quantile_values1, 
                                              include.lowest = TRUE, 
                                              labels = FALSE)
        
        # 创建每个区间的原始值标签
        labels1 <- sapply(1:(length(quantile_values1) - 1), function(i) {
          if (i == 1) {
            # 第一个标签显示完整区间范围
            paste0(round(quantile_values1[i], 1), " ~ ", round(quantile_values1[i + 1], 1))
          } else if (i == length(quantile_values1) - 1) {
            # 最后一个标签显示完整区间范围
            paste0(round(quantile_values1[i], 1), " ~ ", round(quantile_values1[i + 1], 1))
          } else {
            # 中间的标签只显示最小值
            paste0(round(quantile_values1[i], 1), "~")
          }
        })
        
        # 设置自定义颜色
        colors <- colorRampPalette(c("#5eaaf5", "#f4d963", "red"))(10)  # 生成33个颜色
        
        
        main_map4<- ggplot(data = data_mapPer2) +
          geom_sf(aes(fill = as.factor(Deaths_change_cut)), color = "black", size = 0.1) +  # 使用 fill 映射 Deaths_All 到颜色
          coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
          scale_fill_manual(values = colors, labels = labels1,na.value = "grey80") +  # 自定义颜色和标签
          labs(fill = "Percentage change (%)"
               ,title = paste0(reiname)
          ) +
          theme_minimal() +  # 使用简洁主题
          theme(legend.position = "right",
                legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 50),
                legend.text = element_text(size = 45),
                plot.title = element_text(size = 60),
                panel.background = element_rect(fill = "white", colour = NA), 
                plot.background = element_rect(fill = "white", colour = NA),
                panel.grid = element_blank(),
                axis.title = element_blank(),  # 移除轴标题
                axis.text = element_blank(),   # 移除轴文本
                axis.ticks = element_blank(),text = element_text(family = "Arial"))
        
        southsea_map4 <- ggplot(data = data_mapPer2)+
          geom_sf(aes(fill = as.factor(Deaths_change_cut)),color="black",size=0.1)+
          scale_fill_manual(values = colors, labels = labels1,na.value = "grey80") +  # 自定义颜色和标签
          coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
          theme_void()+
          theme(legend.position = "none") 
        
        
        fig_Deaths_Per <- main_map4+inset_element(southsea_map4,left = 0.8,right = 1.0,bottom = 0.1,top = 0.3)
        assign(paste0("fig_Deaths_Per", reiname), fig_Deaths_Per, envir = .GlobalEnv)
        
      }
      
    }
    
  }
}


# #####Number
# data_process(Metric_name = "Number",Year = "2023")

#####Rate
data_process(Metric_name = "Rate",Year = "2023")


#################################4.红肉（存在极端值，需手动调整图例，因此单独处理）######################
reiname <- c("Diet high in red meat")
causename <- c("Non-communicable diseases")
Metric_name  <-  "Rate"
Year <-  "2023"


data_map0 <- read.csv(ifelse(Metric_name=="Number","result/data_mapNumber.csv",
                             "result/data_mapRate.csv"))
data_map <- data_map0%>%
  mutate(location_name=case_when(
    location_name=="Hong Kong Special Administrative Region of China"~"Hong Kong",
    location_name=="Macao Special Administrative Region of China"~"Macao",
    TRUE~location_name
  ))%>%
  filter(cause_name==causename&rei_name==reiname&year==Year&metric_name==Metric_name)


####
data_map1 <-data_map %>% filter(measure_name == "DALYs")

data_map1 <- left_join(china_sf,data_map1,by=c("name"="location_name"))

main_map1<- ggplot(data = data_map1) +
  geom_sf(aes(fill = val), color = "black", size = 0.1) +  # 使用 fill 映射 DALYs_All 到颜色
  coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
  scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "grey80")+    
  labs(fill =paste0("DALYs"," ",Metric_name)
       , title = paste0(reiname)
  ) +  # 添加图例标题和图表标题
  theme_minimal() +  # 使用简洁主题
  theme(legend.position = "right",
        legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 50),
        legend.text = element_text(size = 45),
        plot.title = element_text(size = 60),
        panel.background = element_rect(fill = "white", colour = NA), 
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),  # 移除轴标题
        axis.text = element_blank(),   # 移除轴文本
        axis.ticks = element_blank(),text = element_text(family = "Arial"),
        legend.key.height = unit(2, "cm"))

southsea_map1 <- ggplot(data = data_map1)+
  geom_sf(aes(fill=val),color="black",size=0.1)+
  scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "gray80")+
  coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
  theme_void()+
  theme(legend.position = "none") 


fig_DALYsredmeat <- main_map1+inset_element(southsea_map1,left = 0.8,right = 1.0,bottom = 0.1,top = 0.3)





####
data_map2 <- data_map %>% filter(measure_name == "Deaths")

data_map2 <- left_join(china_sf,data_map2,by=c("name"="location_name"))

main_map2<- ggplot(data = data_map2) +
  geom_sf(aes(fill = val), color = "black", size = 0.1) +  # 使用 fill 映射 DALYs_All 到颜色
  coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
  scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "grey80")+    
  labs(fill = paste0("Death"," ",Metric_name)
       , title = paste0(reiname)
  ) +  # 添加图例标题和图表标题
  theme_minimal() +  # 使用简洁主题
  theme(legend.position = "right",
        legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 50),
        legend.text = element_text(size = 45),
        plot.title = element_text(size = 60),
        panel.background = element_rect(fill = "white", colour = NA), 
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),  # 移除轴标题
        axis.text = element_blank(),   # 移除轴文本
        axis.ticks = element_blank(),text = element_text(family = "Arial"),
        legend.key.height = unit(2, "cm"))

southsea_map2 <- ggplot(data = data_map2) +
  geom_sf(aes(fill = val), color = "black", size = 0.1) +  # 使用 fill 映射 DALYs_All 到颜色
  scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "gray80")+
  coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
  theme_void()+
  theme(legend.position = "none")

fig_Deathsredmeat <- main_map2+inset_element(southsea_map2,left = 0.8,right = 1.0,top = 0.3,bottom = 0.1)


#################################变化图
library(openxlsx)
data_mapPer <- read.csv("result/level1change/level1change_rat_redmeat.csv")
colnames(data_mapPer)
data_mapPer <- data_mapPer[,c("location_name","rei_name","cause_name",
                              "metric_name","DALYs_change","Deaths_change")]



data_mapPer1 <- left_join(china_sf,data_mapPer,by=c("name"="location_name"))

data_mapPer2 <- data_mapPer1 %>%
  filter(name=="境界线"|name=="Taiwan"|(rei_name == reiname &cause_name==causename
                                     &metric_name==Metric_name))

data_mapPer2 <- data_mapPer2 %>%
  mutate(
    DALYs_change_factor = case_when(
      DALYs_change >= 0 & DALYs_change < 50 ~ "0 ~ 50",
      DALYs_change >= 50 & DALYs_change < 100 ~ "50 ~ 100",
      DALYs_change >= 100 & DALYs_change < 150 ~ "100 ~ 150",
      DALYs_change >= 150 & DALYs_change < 200 ~ "150 ~ 200",
      DALYs_change >= 200 & DALYs_change < 250 ~ "200 ~ 250",
      DALYs_change >= 250 & DALYs_change < 300 ~ "250 ~ 300",
      DALYs_change >= 300 & DALYs_change < 350 ~ "300 ~ 350",
      DALYs_change >= 350 & DALYs_change < 400 ~ "350 ~ 400",
      DALYs_change >= 450 & DALYs_change < 500 ~ "450 ~ 500",
      DALYs_change >= 700 & DALYs_change < 750 ~ "700 ~ 750",
      DALYs_change >= 800 & DALYs_change < 850 ~ "800 ~ 850",
      DALYs_change >= 1000 & DALYs_change < 1050 ~ "1000 ~ 1050",
      DALYs_change >= 1100 & DALYs_change < 1150 ~ "1100 ~ 1150",
      DALYs_change >= 1450 & DALYs_change < 1500 ~ "1450 ~ 1500",
      DALYs_change >= 9150 & DALYs_change < 9200 ~ "9150 ~ 9200",
      TRUE ~ NA  # 添加默认的标签
    ),
    Deaths_change_factor = case_when(
      Deaths_change >= -50 & Deaths_change < 0 ~ "-50 ~ 0",
      Deaths_change >= 0 & Deaths_change < 50 ~ "0 ~ 50",
      Deaths_change >= 50 & Deaths_change < 100 ~ "50 ~ 100",
      Deaths_change >= 100 & Deaths_change < 150 ~ "100 ~ 150",
      Deaths_change >= 150 & Deaths_change < 200 ~ "150 ~ 200",
      Deaths_change >= 200 & Deaths_change < 250 ~ "200 ~ 250",
      Deaths_change >= 250 & Deaths_change < 300 ~ "250 ~ 300",
      Deaths_change >= 300 & Deaths_change < 350 ~ "300 ~ 350",
      Deaths_change >= 350 & Deaths_change < 400 ~ "350 ~ 400",
      Deaths_change >= 400 & Deaths_change < 450 ~ "400 ~ 450",
      Deaths_change >= 450 & Deaths_change < 500 ~ "450 ~ 500",
      Deaths_change >= 700 & Deaths_change < 750 ~ "700 ~ 750",
      Deaths_change >= 2900 & Deaths_change < 2950 ~ "2900 ~ 2950",
      Deaths_change >= 3200 & Deaths_change < 3250 ~ "3200 ~ 3250",
      Deaths_change >= 3850 & Deaths_change < 3900 ~ "3850 ~ 3900",
      TRUE ~ NA # 添加默认的标签
    )
  ) %>%
  mutate(
    DALYs_change_factor = factor(DALYs_change_factor, 
                                 levels = c("0 ~ 50","50 ~ 100","100 ~ 150","150 ~ 200",
                                            "200 ~ 250","250 ~ 300","300 ~ 350","350 ~ 400",
                                            "450 ~ 500","700 ~ 750","800 ~ 850","1000 ~ 1050",
                                            "1100 ~ 1150","1450 ~ 1500","9150 ~ 9200")),
    Deaths_change_factor = factor(Deaths_change_factor, 
                                  levels = c("-50 ~ 0","0 ~ 50","50 ~ 100","100 ~ 150",
                                             "150 ~ 200","200 ~ 250","250 ~ 300","300 ~ 350",
                                             "350 ~ 400","400 ~ 450","450 ~ 500","700 ~ 750",
                                             "2900 ~ 2950","3200 ~ 3250","3850 ~ 3900"))
  )




# 创建自定义的颜色
colors <- colorRampPalette(c("#5eaaf5", "#f4d963", "red"))(17)  # 生成33个颜色



####
# 绘制主地图
main_map3 <- ggplot(data = data_mapPer2) +
  geom_sf(aes(fill = DALYs_change_factor), color = "black", size = 0.1) +  # 映射所有颜色到因子
  coord_sf(xlim = c(73, 136), ylim = c(15, 54)) +  # 设置非南海区域的坐标范围
  scale_fill_manual(values = colors, na.value = "grey80") +  # 自定义颜色和标签
  labs(fill = "Percentage change (%)"
       ,title = paste0(reiname)
  ) +
  theme_minimal() +  # 使用简洁主题
  theme(legend.position = "right",
        legend.title = element_text(hjust = 0.5, vjust = 1.5, size = 50),
        legend.text = element_text(size = 45),
        plot.title = element_text(size = 60),
        panel.background = element_rect(fill = "white", colour = NA), 
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),  # 移除轴标题
        axis.text = element_blank(),   # 移除轴文本
        axis.ticks = element_blank(),
        text = element_text(family = "Arial"))+
  guides(fill = guide_legend(ncol = 1))  # 设置图例为两列

# 绘制南海地图
southsea_map3 <- ggplot(data = data_mapPer2) +
  geom_sf(aes(fill = DALYs_change_factor), color = "black", size = 0.1) +  # 映射所有颜色到因子
  scale_fill_manual(values = colors, labels = labels, na.value = "grey80") +  # 自定义颜色和标签
  coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
  theme_void() +
  theme(legend.position = "none")  # 不显示图例

# 显示地图
main_map3
southsea_map3

fig_DALYs_Perredmeat <- main_map3+inset_element(southsea_map3,left = 0.8,right = 1.0,bottom = 0.1,top = 0.3)







####
main_map4<- ggplot(data = data_mapPer2) +
  geom_sf(aes(fill = Deaths_change_factor), color = "black", size = 0.1) +  # 使用 fill 映射 Deaths_All 到颜色
  coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
  scale_fill_manual(values = colors, na.value = "grey80") +  # 自定义颜色和标签
  labs(fill = "Percentage change (%)"
       ,title = paste0(reiname)
  ) +
  theme_minimal() +  # 使用简洁主题
  theme(legend.position = "right",
        legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 50),
        legend.text = element_text(size = 45),
        plot.title = element_text(size = 60),
        panel.background = element_rect(fill = "white", colour = NA), 
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),  # 移除轴标题
        axis.text = element_blank(),   # 移除轴文本
        axis.ticks = element_blank(),text = element_text(family = "Arial"))+
  guides(fill = guide_legend(ncol = 1))  # 设置图例为两列

southsea_map4 <- ggplot(data = data_mapPer2)+
  geom_sf(aes(fill = Deaths_change_factor),color="black",size=0.1)+
  scale_fill_manual(values = colors,na.value = "grey80") +  # 自定义颜色和标签
  coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
  theme_void()+
  theme(legend.position = "none") 


fig_Deaths_Perredmeat <- main_map4+inset_element(southsea_map4,left = 0.8,right = 1.0,bottom = 0.1,top = 0.3)



###############5.所有因素大合并#############
####
plot_Deaths <- (`fig_DeathsDiet high in sodium`|`fig_DeathsDiet low in whole grains`|`fig_DeathsDiet low in fruits`)/
  (`fig_DeathsDiet low in omega-6 polyunsaturated fatty acids`|`fig_DeathsDiet low in nuts and seeds`|`fig_DeathsDiet low in fiber`)/
  (`fig_DeathsDiet low in seafood omega-3 fatty acids`|`fig_DeathsDiet low in legumes`|fig_Deathsredmeat)/
  (`fig_DeathsDiet low in vegetables`|`fig_DeathsDiet low in milk`|`fig_DeathsDiet low in calcium`)/
  (`fig_DeathsDiet high in processed meat`|`fig_DeathsDiet high in trans fatty acids`|`fig_DeathsDiet high in sugar-sweetened beverages`)


ggsave("result/plot_Deaths1.png",plot_Deaths,width = 60,height = 60,dpi = 300,limitsize = FALSE)

#### 
plot_DALYs <- (`fig_DALYsDiet high in sodium`|`fig_DALYsDiet low in whole grains`|`fig_DALYsDiet low in fruits`)/
  (`fig_DALYsDiet low in omega-6 polyunsaturated fatty acids`|`fig_DALYsDiet low in nuts and seeds`|`fig_DALYsDiet low in fiber`)/
  (`fig_DALYsDiet low in seafood omega-3 fatty acids`|`fig_DALYsDiet low in legumes`|fig_DALYsredmeat)/
  (`fig_DALYsDiet low in vegetables`|`fig_DALYsDiet low in milk`|`fig_DALYsDiet low in calcium`)/
  (`fig_DALYsDiet high in processed meat`|`fig_DALYsDiet high in trans fatty acids`|`fig_DALYsDiet high in sugar-sweetened beverages`)

ggsave("result/plot_DALYs1.png",plot_DALYs,width = 60,height = 60,dpi = 300,limitsize = FALSE)


####
plot_Deaths_Per <- (`fig_Deaths_PerDiet high in sodium`|`fig_Deaths_PerDiet low in whole grains`|`fig_Deaths_PerDiet low in fruits`)/
  (`fig_Deaths_PerDiet low in omega-6 polyunsaturated fatty acids`|`fig_Deaths_PerDiet low in nuts and seeds`|`fig_Deaths_PerDiet low in fiber`)/
  (`fig_Deaths_PerDiet low in seafood omega-3 fatty acids`|`fig_Deaths_PerDiet low in legumes`|fig_Deaths_Perredmeat)/
  (`fig_Deaths_PerDiet low in vegetables`|`fig_Deaths_PerDiet low in milk`|`fig_Deaths_PerDiet low in calcium`)/
  (`fig_Deaths_PerDiet high in processed meat`|`fig_Deaths_PerDiet high in trans fatty acids`|`fig_Deaths_PerDiet high in sugar-sweetened beverages`)


ggsave("result/plot_Deaths_Per1.png",plot_Deaths_Per,width = 60,height = 60,dpi = 300,limitsize = FALSE)

#### 
plot_DALYs_Per <- (`fig_DALYs_PerDiet high in sodium`|`fig_DALYs_PerDiet low in whole grains`|`fig_DALYs_PerDiet low in fruits`)/
  (`fig_DALYs_PerDiet low in omega-6 polyunsaturated fatty acids`|`fig_DALYs_PerDiet low in nuts and seeds`|`fig_DALYs_PerDiet low in fiber`)/
  (`fig_DALYs_PerDiet low in seafood omega-3 fatty acids`|`fig_DALYs_PerDiet low in legumes`|fig_DALYs_Perredmeat)/
  (`fig_DALYs_PerDiet low in vegetables`|`fig_DALYs_PerDiet low in milk`|`fig_DALYs_PerDiet low in calcium`)/
  (`fig_DALYs_PerDiet high in processed meat`|`fig_DALYs_PerDiet high in trans fatty acids`|`fig_DALYs_PerDiet high in sugar-sweetened beverages`)

ggsave("result/plot_DALYs_Per1.png",plot_DALYs_Per,width = 60,height = 60,dpi = 300,limitsize = FALSE)







##########################################左burden右change合并地图#####################
###### 1.导入地图数据###########################

#####外源地图shp文件
library(sf)



china_sf1 <-  st_read("data/mapdata/中国_省shp/中国_省1.shp")
china_sf2 <-  st_read("data/mapdata/中国_省shp/中国_省2.shp")
china_sf <- rbind(china_sf1,china_sf2)

china_sf <- china_sf %>%
  mutate(name = case_when(
    name == "安徽省" ~ "Anhui",
    name == "北京市" ~ "Beijing",
    name == "重庆市" ~ "Chongqing",
    name == "福建省" ~ "Fujian",
    name == "甘肃省" ~ "Gansu",
    name == "广东省" ~ "Guangdong",
    name == "广西壮族自治区" ~ "Guangxi",
    name == "贵州省" ~ "Guizhou",
    name == "海南省" ~ "Hainan",
    name == "河北省" ~ "Hebei",
    name == "黑龙江省" ~ "Heilongjiang",
    name == "河南省" ~ "Henan",
    name == "湖北省" ~ "Hubei",
    name == "湖南省" ~ "Hunan",
    name == "江苏省" ~ "Jiangsu",
    name == "江西省" ~ "Jiangxi",
    name == "吉林省" ~ "Jilin",
    name == "辽宁省" ~ "Liaoning",
    name == "内蒙古自治区" ~ "Inner Mongolia",
    name == "宁夏回族自治区" ~ "Ningxia",
    name == "青海省" ~ "Qinghai",
    name == "山东省" ~ "Shandong",
    name == "山西省" ~ "Shanxi",
    name == "陕西省" ~ "Shaanxi",
    name == "上海市" ~ "Shanghai",
    name == "四川省" ~ "Sichuan",
    name == "天津市" ~ "Tianjin",
    name == "西藏自治区" ~ "Tibet",
    name == "新疆维吾尔自治区" ~ "Xinjiang",
    name == "云南省" ~ "Yunnan",
    name == "浙江省" ~ "Zhejiang",
    name == "澳门特别行政区" ~ "Macao",
    name == "香港特别行政区" ~ "Hong Kong",
    name == "台湾省" ~ "Taiwan",
    TRUE ~ name  # 保留原始名称以防有未覆盖的情况
  ))


#####2.疾病负担数据90 21 number rate  change############

data_mapPer0 <- read.csv("result/level1change/level1change_num.csv")
colnames(data_mapPer0)
data_mapPer0 <- data_mapPer0[,c("location_name","rei_name","cause_name",
                                "metric_name","DALYs_change","Deaths_change")]

data_mapPer <- read.csv("result/level1change/level1change_rat.csv")
colnames(data_mapPer)
data_mapPer <- data_mapPer[,c("location_name","rei_name","cause_name",
                              "metric_name","DALYs_change","Deaths_change")]

data_mapPer1 <- rbind(data_mapPer0,data_mapPer)
data_mapPer1 <- left_join(china_sf,data_mapPer1,by=c("name"="location_name"))




#########3. 定义绘图函数并绘制地图##############
#level2疾病
cause_name <- c("Neoplasms","Cardiovascular diseases","Diabetes and kidney diseases")


# cause_name <- c("Non-communicable diseases")

# rei_name <- c("Diet low in fiber","Diet low in whole grains","Diet high in red meat"
#               ,"Diet high in processed meat","Diet high in sugar-sweetened beverages"
#               ,"Diet high in sodium","Dietary risks","Diet low in calcium","Diet low in fruits"
#               ,"Diet low in vegetables","Diet low in omega-6 polyunsaturated fatty acids"
#               ,"Diet low in legumes","Diet low in seafood omega-3 fatty acids"
#               ,"Diet high in trans fatty acids","Diet low in nuts and seeds","Diet low in milk"
# )
# rei_name <- c("Diet high in red meat")


rei_name <- c("Dietary risks")


data_process <- function(Metric_name,Year){
  for (reiname in rei_name) {
    for (causename in cause_name) {
      ################################疾病负担图
      data_map0 <- read.csv(ifelse(Metric_name=="Number","result/data_mapNumber.csv",
                                   "result/data_mapRate.csv"))
      data_map <- data_map0%>%
        mutate(location_name=case_when(
          location_name=="Hong Kong Special Administrative Region of China"~"Hong Kong",
          location_name=="Macao Special Administrative Region of China"~"Macao",
          TRUE~location_name
        ))%>%
        filter(cause_name==causename&rei_name==reiname&year==Year&metric_name==Metric_name)
      
      data_map1 <-data_map %>% filter(measure_name == "DALYs")
      if(nrow(data_map1) > 0){
        data_map1 <- left_join(china_sf,data_map1,by=c("name"="location_name"))
        
        main_map1<- ggplot(data = data_map1) +
          geom_sf(aes(fill = val), color = "black", size = 0.1) +  # 使用 fill 映射 DALYs_All 到颜色
          coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
          scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "grey80")+    
          labs(fill =paste0("DALYs"," ",Metric_name)
               #, title = paste0(reiname," ","attributable"," ",causename," ","DALYs"," ",Metric_name," ","in"," ",Year)
          ) +  # 添加图例标题和图表标题
          theme_minimal() +  # 使用简洁主题
          theme(legend.position = "right",
                legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 15),
                legend.text = element_text(size = 12),
                panel.background = element_rect(fill = "white", colour = NA), 
                plot.background = element_rect(fill = "white", colour = NA),
                panel.grid = element_blank(),
                axis.title = element_blank(),  # 移除轴标题
                axis.text = element_blank(),   # 移除轴文本
                axis.ticks = element_blank(),text = element_text(family = "Arial"))
        
        southsea_map1 <- ggplot(data = data_map1)+
          geom_sf(aes(fill=val),color="black",size=0.1)+
          scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "gray80")+
          coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
          theme_void()+
          theme(legend.position = "none") 
        
        
        fig_DALYs <- main_map1+inset_element(southsea_map1,left = 0.8,right = 1.0,bottom = 0.1,top = 0.3)
        
      }
      
      data_map2 <- data_map %>% filter(measure_name == "Deaths")
      if(nrow(data_map2) > 0){
        data_map2 <- left_join(china_sf,data_map2,by=c("name"="location_name"))
        
        main_map2<- ggplot(data = data_map2) +
          geom_sf(aes(fill = val), color = "black", size = 0.1) +  # 使用 fill 映射 DALYs_All 到颜色
          coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
          scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "grey80")+    
          labs(fill = paste0("Death"," ",Metric_name)
               #, title = paste0(reiname," ","attributable"," ",causename," ","Deaths"," ",Metric_name," ","in"," ",Year)
          ) +  # 添加图例标题和图表标题
          theme_minimal() +  # 使用简洁主题
          theme(legend.position = "right",
                legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 15),
                legend.text = element_text(size = 12),
                panel.background = element_rect(fill = "white", colour = NA), 
                plot.background = element_rect(fill = "white", colour = NA),
                panel.grid = element_blank(),
                axis.title = element_blank(),  # 移除轴标题
                axis.text = element_blank(),   # 移除轴文本
                axis.ticks = element_blank(),text = element_text(family = "Arial"))
        
        southsea_map2 <- ggplot(data = data_map2) +
          geom_sf(aes(fill = val), color = "black", size = 0.1) +  # 使用 fill 映射 DALYs_All 到颜色
          scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "gray80")+
          coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
          theme_void()+
          theme(legend.position = "none")
        
        fig_Deaths <- main_map2+inset_element(southsea_map2,left = 0.8,right = 1.0,top = 0.3,bottom = 0.1)
      }
      
      
      #################################变化图
      
      data_mapPer2 <- data_mapPer1 %>%
        filter(name=="境界线"|name=="Taiwan"|(rei_name == reiname &cause_name==causename
                                           &metric_name==Metric_name))
      if(nrow(data_mapPer2) > 8){
        ##DALYS 
        
        quantiles <- seq(0, 1, length.out = 11)  
        quantile_values <- quantile(data_mapPer2$DALYs_change, probs = quantiles,na.rm=TRUE)
        
        # 根据分位数将数据进行分段
        data_mapPer2$DALYs_change_cut <- cut(data_mapPer2$DALYs_change, 
                                             breaks = quantile_values, 
                                             include.lowest = TRUE, 
                                             labels = FALSE)
        
        # 创建每个区间的原始值标签
        labels <- sapply(1:(length(quantile_values) - 1), function(i) {
          if (i == 1) {
            # 第一个标签显示完整区间范围
            paste0(round(quantile_values[i], 1), " ~ ", round(quantile_values[i + 1], 1))
          } else if (i == length(quantile_values) - 1) {
            # 最后一个标签显示完整区间范围
            paste0(round(quantile_values[i], 1), " ~ ", round(quantile_values[i + 1], 1))
          } else {
            # 中间的标签只显示最小值
            paste0(round(quantile_values[i], 1), "~")
          }
        })
        
        # 设置自定义颜色
        colors <- colorRampPalette(c("#5eaaf5", "#f4d963", "red"))(10)  # 生成33个颜色
        
        
        main_map3<- ggplot(data = data_mapPer2) +
          geom_sf(aes(fill = as.factor(DALYs_change_cut)), color = "black", size = 0.1) +  # 使用分段数据映射颜色
          coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
          scale_fill_manual(values = colors, labels = labels,na.value = "grey80") +  # 自定义颜色和标签
          labs(fill = "Percentage change (%)"
               #,title = paste0(reiname," ","attributable"," ",causename," ","DALYs"," ", Metric_name," ","change")
          ) +
          theme_minimal() +  # 使用简洁主题
          theme(legend.position = "right",
                legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 15),
                legend.text = element_text(size = 12),
                panel.background = element_rect(fill = "white", colour = NA), 
                plot.background = element_rect(fill = "white", colour = NA),
                panel.grid = element_blank(),
                axis.title = element_blank(),  # 移除轴标题
                axis.text = element_blank(),   # 移除轴文本
                axis.ticks = element_blank(),text = element_text(family = "Arial"))
        
        southsea_map3 <- ggplot(data = data_mapPer2)+
          geom_sf(aes(fill = as.factor(DALYs_change_cut)),color="black",size=0.1)+
          scale_fill_manual(values = colors, labels = labels,na.value = "grey80") +  # 自定义颜色和标签
          coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
          theme_void()+
          theme(legend.position = "none") 
        
        
        fig_DALYs_Per <- main_map3+inset_element(southsea_map3,left = 0.8,right = 1.0,bottom = 0.1,top = 0.3)
        
        fig_DALYs_Per1 <- fig_DALYs|fig_DALYs_Per
        
        # ggsave(paste0("D:/MetLAB/GBD_cn/result/map result/DALYs_",Metric_name,"/",reiname,"_",causename,".png"),
        #        fig_DALYs_Per1,width = 20,height = 10,units = "in",dpi = 300)
        
        ggsave(paste0("result/map result//DALYs_",Metric_name,"/",reiname,"_",causename,".png"),
               fig_DALYs_Per1,width = 20,height = 6,units = "in",dpi = 300)
        
        ##DeathS 
        
        quantiles <- seq(0, 1, length.out = 11)  
        quantile_values1 <- quantile(data_mapPer2$Deaths_change, probs = quantiles,na.rm=TRUE)
        
        # 根据分位数将数据进行分段
        data_mapPer2$Deaths_change_cut <- cut(data_mapPer2$Deaths_change, 
                                              breaks = quantile_values1, 
                                              include.lowest = TRUE, 
                                              labels = FALSE)
        
        # 创建每个区间的原始值标签
        labels1 <- sapply(1:(length(quantile_values1) - 1), function(i) {
          if (i == 1) {
            # 第一个标签显示完整区间范围
            paste0(round(quantile_values1[i], 1), " ~ ", round(quantile_values1[i + 1], 1))
          } else if (i == length(quantile_values1) - 1) {
            # 最后一个标签显示完整区间范围
            paste0(round(quantile_values1[i], 1), " ~ ", round(quantile_values1[i + 1], 1))
          } else {
            # 中间的标签只显示最小值
            paste0(round(quantile_values1[i], 1), "~")
          }
        })
        
        # 设置自定义颜色
        colors <- colorRampPalette(c("#5eaaf5", "#f4d963", "red"))(10)  # 生成33个颜色
        
        
        main_map4<- ggplot(data = data_mapPer2) +
          geom_sf(aes(fill = as.factor(Deaths_change_cut)), color = "black", size = 0.1) +  # 使用 fill 映射 Deaths_All 到颜色
          coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
          scale_fill_manual(values = colors, labels = labels1,na.value = "grey80") +  # 自定义颜色和标签
          labs(fill = "Percentage change (%)"
               #,title = paste0(reiname," ","attributable"," ",causename," ","Deaths"," ", Metric_name," ","change")
          ) +
          theme_minimal() +  # 使用简洁主题
          theme(legend.position = "right",
                legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 15),
                legend.text = element_text(size = 12),
                panel.background = element_rect(fill = "white", colour = NA), 
                plot.background = element_rect(fill = "white", colour = NA),
                panel.grid = element_blank(),
                axis.title = element_blank(),  # 移除轴标题
                axis.text = element_blank(),   # 移除轴文本
                axis.ticks = element_blank(),text = element_text(family = "Arial"))
        
        southsea_map4 <- ggplot(data = data_mapPer2)+
          geom_sf(aes(fill = as.factor(Deaths_change_cut)),color="black",size=0.1)+
          scale_fill_manual(values = colors, labels = labels1,na.value = "grey80") +  # 自定义颜色和标签
          coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
          theme_void()+
          theme(legend.position = "none") 
        
        
        fig_Deaths_Per <- main_map4+inset_element(southsea_map4,left = 0.8,right = 1.0,bottom = 0.1,top = 0.3)
        
        fig_Deaths_Per1 <- fig_Deaths|fig_Deaths_Per
        
        # ggsave(paste0("result/map result/Death_",Metric_name,"/",reiname,"_",causename,".png"),
        #        fig_Deaths_Per1,width = 20,height = 10,units = "in",dpi = 300)
        ggsave(paste0("result/map result//Death_",Metric_name,"/",reiname,"_",causename,".png"),
               fig_Deaths_Per1,width = 20,height = 6,units = "in",dpi = 300)
        
      }
      
    }
    
  }
}

######################运行函数，绘制地图
# #####Number
# data_process(Metric_name = "Number",Year = "2023")

#####Rate
data_process(Metric_name = "Rate",Year = "2023")

# #################################4.红肉（存在极端值，需手动调整图例，因此单独处理############
# reiname <- c("Diet high in red meat")
# causename <- c("Non-communicable diseases")
# Metric_name  <-  "Rate"
# Year <-  "2023"
# 
# 
# data_map0 <- read.csv(ifelse(Metric_name=="Number","result/data_mapNumber.csv",
#                              "result/data_mapRate.csv"))
# data_map <- data_map0%>%
#   mutate(location_name=case_when(
#     location_name=="Hong Kong Special Administrative Region of China"~"Hong Kong",
#     location_name=="Macao Special Administrative Region of China"~"Macao",
#     TRUE~location_name
#   ))%>%
#   filter(cause_name==causename&rei_name==reiname&year==Year&metric_name==Metric_name)
# 
# data_map1 <-data_map %>% filter(measure_name == "DALYs")
# 
# data_map1 <- left_join(china_sf,data_map1,by=c("name"="location_name"))
# 
# main_map1<- ggplot(data = data_map1) +
#   geom_sf(aes(fill = val), color = "black", size = 0.1) +  # 使用 fill 映射 DALYs_All 到颜色
#   coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
#   scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "grey80")+    
#   labs(fill =paste0("DALYs"," ",Metric_name)
#        #, title = paste0(reiname," ","attributable"," ",causename," ","DALYs"," ",Metric_name," ","in"," ",Year)
#   ) +  # 添加图例标题和图表标题
#   theme_minimal() +  # 使用简洁主题
#   theme(legend.position = "right",
#         legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 15),
#         legend.text = element_text(size = 12),
#         panel.background = element_rect(fill = "white", colour = NA), 
#         plot.background = element_rect(fill = "white", colour = NA),
#         panel.grid = element_blank(),
#         axis.title = element_blank(),  # 移除轴标题
#         axis.text = element_blank(),   # 移除轴文本
#         axis.ticks = element_blank(),text = element_text(family = "Arial"))
# 
# southsea_map1 <- ggplot(data = data_map1)+
#   geom_sf(aes(fill=val),color="black",size=0.1)+
#   scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "gray80")+
#   coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
#   theme_void()+
#   theme(legend.position = "none") 
# 
# 
# fig_DALYs <- main_map1+inset_element(southsea_map1,left = 0.8,right = 1.0,bottom = 0.1,top = 0.3)
# 
# data_map2 <- data_map %>% filter(measure_name == "Deaths")
# 
# data_map2 <- left_join(china_sf,data_map2,by=c("name"="location_name"))
# 
# main_map2<- ggplot(data = data_map2) +
#   geom_sf(aes(fill = val), color = "black", size = 0.1) +  # 使用 fill 映射 DALYs_All 到颜色
#   coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
#   scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "grey80")+    
#   labs(fill = paste0("Death"," ",Metric_name)
#        #, title = paste0(reiname," ","attributable"," ",causename," ","Deaths"," ",Metric_name," ","in"," ",Year)
#   ) +  # 添加图例标题和图表标题
#   theme_minimal() +  # 使用简洁主题
#   theme(legend.position = "right",
#         legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 15),
#         legend.text = element_text(size = 12),
#         panel.background = element_rect(fill = "white", colour = NA), 
#         plot.background = element_rect(fill = "white", colour = NA),
#         panel.grid = element_blank(),
#         axis.title = element_blank(),  # 移除轴标题
#         axis.text = element_blank(),   # 移除轴文本
#         axis.ticks = element_blank(),text = element_text(family = "Arial"))
# 
# southsea_map2 <- ggplot(data = data_map2) +
#   geom_sf(aes(fill = val), color = "black", size = 0.1) +  # 使用 fill 映射 DALYs_All 到颜色
#   scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5","#f4d963","red"))(33),na.value = "gray80")+
#   coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
#   theme_void()+
#   theme(legend.position = "none")
# 
# fig_Deaths <- main_map2+inset_element(southsea_map2,left = 0.8,right = 1.0,top = 0.3,bottom = 0.1)
# 
# 
# #################################变化图
# library(openxlsx)
# data_mapPer <- read.xlsx("result/level1change/level1change_rat_redmeat.xlsx")
# colnames(data_mapPer)
# data_mapPer <- data_mapPer[,c("location_name","rei_name","cause_name",
#                               "metric_name","DALYs_change","Deaths_change")]
# 
# 
# 
# data_mapPer1 <- left_join(china_sf,data_mapPer,by=c("name"="location_name"))
# 
# data_mapPer2 <- data_mapPer1 %>%
#   filter(name=="境界线"|name=="Taiwan"|(rei_name == reiname &cause_name==causename
#                                      &metric_name==Metric_name))
# 
# 
# data_mapPer2 <- data_mapPer2 %>%
#   mutate(
#     DALYs_change_factor = case_when(
#       DALYs_change >= 0 & DALYs_change < 50 ~ "0 ~ 50",
#       DALYs_change >= 50 & DALYs_change < 100 ~ "50 ~ 100",
#       DALYs_change >= 100 & DALYs_change < 150 ~ "100 ~ 150",
#       DALYs_change >= 150 & DALYs_change < 200 ~ "150 ~ 200",
#       DALYs_change >= 200 & DALYs_change < 250 ~ "200 ~ 250",
#       DALYs_change >= 250 & DALYs_change < 300 ~ "250 ~ 300",
#       DALYs_change >= 300 & DALYs_change < 350 ~ "300 ~ 350",
#       DALYs_change >= 350 & DALYs_change < 400 ~ "350 ~ 400",
#       DALYs_change >= 450 & DALYs_change < 500 ~ "450 ~ 500",
#       DALYs_change >= 700 & DALYs_change < 750 ~ "700 ~ 750",
#       DALYs_change >= 800 & DALYs_change < 850 ~ "800 ~ 850",
#       DALYs_change >= 1000 & DALYs_change < 1050 ~ "1000 ~ 1050",
#       DALYs_change >= 1100 & DALYs_change < 1150 ~ "1100 ~ 1150",
#       DALYs_change >= 1450 & DALYs_change < 1500 ~ "1450 ~ 1500",
#       DALYs_change >= 9150 & DALYs_change < 9200 ~ "9150 ~ 9200",
#       TRUE ~ NA  # 添加默认的标签
#     ),
#     Deaths_change_factor = case_when(
#       Deaths_change >= -50 & Deaths_change < 0 ~ "-50 ~ 0",
#       Deaths_change >= 0 & Deaths_change < 50 ~ "0 ~ 50",
#       Deaths_change >= 50 & Deaths_change < 100 ~ "50 ~ 100",
#       Deaths_change >= 100 & Deaths_change < 150 ~ "100 ~ 150",
#       Deaths_change >= 150 & Deaths_change < 200 ~ "150 ~ 200",
#       Deaths_change >= 200 & Deaths_change < 250 ~ "200 ~ 250",
#       Deaths_change >= 250 & Deaths_change < 300 ~ "250 ~ 300",
#       Deaths_change >= 300 & Deaths_change < 350 ~ "300 ~ 350",
#       Deaths_change >= 350 & Deaths_change < 400 ~ "350 ~ 400",
#       Deaths_change >= 400 & Deaths_change < 450 ~ "400 ~ 450",
#       Deaths_change >= 450 & Deaths_change < 500 ~ "450 ~ 500",
#       Deaths_change >= 700 & Deaths_change < 750 ~ "700 ~ 750",
#       Deaths_change >= 2900 & Deaths_change < 2950 ~ "2900 ~ 2950",
#       Deaths_change >= 3200 & Deaths_change < 3250 ~ "3200 ~ 3250",
#       Deaths_change >= 3850 & Deaths_change < 3900 ~ "3850 ~ 3900",
#       TRUE ~ NA # 添加默认的标签
#     )
#   ) %>%
#   mutate(
#     DALYs_change_factor = factor(DALYs_change_factor, 
#                                  levels = c("0 ~ 50","50 ~ 100","100 ~ 150","150 ~ 200",
#                                             "200 ~ 250","250 ~ 300","300 ~ 350","350 ~ 400",
#                                             "450 ~ 500","700 ~ 750","800 ~ 850","1000 ~ 1050",
#                                             "1100 ~ 1150","1450 ~ 1500","9150 ~ 9200")),
#     Deaths_change_factor = factor(Deaths_change_factor, 
#                                   levels = c("-50 ~ 0","0 ~ 50","50 ~ 100","100 ~ 150",
#                                              "150 ~ 200","200 ~ 250","250 ~ 300","300 ~ 350",
#                                              "350 ~ 400","400 ~ 450","450 ~ 500","700 ~ 750",
#                                              "2900 ~ 2950","3200 ~ 3250","3850 ~ 3900"))
#   )
# 
# 
# 
# 
# # 创建自定义的颜色
# colors <- colorRampPalette(c("#5eaaf5", "#f4d963", "red"))(17)  # 生成33个颜色
# 
# # 绘制主地图
# main_map3 <- ggplot(data = data_mapPer2) +
#   geom_sf(aes(fill = DALYs_change_factor), color = "black", size = 0.1) +  # 映射所有颜色到因子
#   coord_sf(xlim = c(73, 136), ylim = c(15, 54)) +  # 设置非南海区域的坐标范围
#   scale_fill_manual(values = colors, na.value = "grey80") +  # 自定义颜色和标签
#   labs(fill = "Percentage change (%)"
#        #,title = paste0(reiname, " ", "attributable", " ", causename, " ", "DALYs", " ", Metric_name, " ", "change")
#   ) +
#   theme_minimal() +  # 使用简洁主题
#   theme(legend.position = "right",
#         legend.title = element_text(hjust = 0.5, vjust = 1.5, size = 15),
#         legend.text = element_text(size = 12),
#         panel.background = element_rect(fill = "white", colour = NA), 
#         plot.background = element_rect(fill = "white", colour = NA),
#         panel.grid = element_blank(),
#         axis.title = element_blank(),  # 移除轴标题
#         axis.text = element_blank(),   # 移除轴文本
#         axis.ticks = element_blank(),
#         text = element_text(family = "Arial"))+
#   guides(fill = guide_legend(ncol = 2))  # 设置图例为两列
# 
# # 绘制南海地图
# southsea_map3 <- ggplot(data = data_mapPer2) +
#   geom_sf(aes(fill = DALYs_change_factor), color = "black", size = 0.1) +  # 映射所有颜色到因子
#   scale_fill_manual(values = colors, labels = labels, na.value = "grey80") +  # 自定义颜色和标签
#   coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
#   theme_void() +
#   theme(legend.position = "none")  # 不显示图例
# 
# # 显示地图
# main_map3
# southsea_map3
# 
# 
# 
# 
# fig_DALYs_Per <- main_map3+inset_element(southsea_map3,left = 0.8,right = 1.0,bottom = 0.1,top = 0.3)
# 
# fig_DALYs_Per1 <- fig_DALYs|fig_DALYs_Per
# 
# # ggsave(paste0("result/map result/DALYs_",Metric_name,"/",reiname,"_",causename,".png"),
# #        fig_DALYs_Per1,width = 20,height = 10,units = "in",dpi = 300)
# 
# ggsave(paste0("result/map result//DALYs_",Metric_name,"/",reiname,"_",causename,".png"),
#        fig_DALYs_Per1,width = 20,height = 6,units = "in",dpi = 300)
# 
# 
# 
# 
# main_map4<- ggplot(data = data_mapPer2) +
#   geom_sf(aes(fill = Deaths_change_factor), color = "black", size = 0.1) +  # 使用 fill 映射 Deaths_All 到颜色
#   coord_sf(xlim = c(73,136),ylim = c(15,54))+ # 设置非南海区域的坐标范围
#   scale_fill_manual(values = colors, na.value = "grey80") +  # 自定义颜色和标签
#   labs(fill = "Percentage change (%)"
#        #,title = paste0(reiname," ","attributable"," ",causename," ","Deaths"," ", Metric_name," ","change")
#   ) +
#   theme_minimal() +  # 使用简洁主题
#   theme(legend.position = "right",
#         legend.title = element_text(hjust = 0.5,vjust = 1.5, size = 15),
#         legend.text = element_text(size = 12),
#         panel.background = element_rect(fill = "white", colour = NA), 
#         plot.background = element_rect(fill = "white", colour = NA),
#         panel.grid = element_blank(),
#         axis.title = element_blank(),  # 移除轴标题
#         axis.text = element_blank(),   # 移除轴文本
#         axis.ticks = element_blank(),text = element_text(family = "Arial"))+
#   guides(fill = guide_legend(ncol = 2))  # 设置图例为两列
# 
# southsea_map4 <- ggplot(data = data_mapPer2)+
#   geom_sf(aes(fill = Deaths_change_factor),color="black",size=0.1)+
#   scale_fill_manual(values = colors,na.value = "grey80") +  # 自定义颜色和标签
#   coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
#   theme_void()+
#   theme(legend.position = "none") 
# 
# 
# fig_Deaths_Per <- main_map4+inset_element(southsea_map4,left = 0.8,right = 1.0,bottom = 0.1,top = 0.3)
# 
# fig_Deaths_Per1 <- fig_Deaths|fig_Deaths_Per
# 
# # ggsave(paste0("result/map result/Death_",Metric_name,"/",reiname,"_",causename,".png"),
# #        fig_Deaths_Per1,width = 20,height = 10,units = "in",dpi = 300)
# ggsave(paste0("result/map result/Death_",Metric_name,"/",reiname,"_",causename,".png"),
#        fig_Deaths_Per1,width = 20,height = 6,units = "in",dpi = 300)
# 
# 


############中国省份位置介绍地图##################
#####外源地图shp文件
library(sf)

china_sf1 <-  st_read("data/mapdata/中国_省shp/中国_省1.shp")
china_sf2 <-  st_read("data/mapdata/中国_省shp/中国_省2.shp")
china_sf <- rbind(china_sf1,china_sf2)

china_sf <- china_sf %>%
  mutate(name = case_when(
    name == "安徽省" ~ "Anhui",
    name == "北京市" ~ "Beijing",
    name == "重庆市" ~ "Chongqing",
    name == "福建省" ~ "Fujian",
    name == "甘肃省" ~ "Gansu",
    name == "广东省" ~ "Guangdong",
    name == "广西壮族自治区" ~ "Guangxi",
    name == "贵州省" ~ "Guizhou",
    name == "海南省" ~ "Hainan",
    name == "河北省" ~ "Hebei",
    name == "黑龙江省" ~ "Heilongjiang",
    name == "河南省" ~ "Henan",
    name == "湖北省" ~ "Hubei",
    name == "湖南省" ~ "Hunan",
    name == "江苏省" ~ "Jiangsu",
    name == "江西省" ~ "Jiangxi",
    name == "吉林省" ~ "Jilin",
    name == "辽宁省" ~ "Liaoning",
    name == "内蒙古自治区" ~ "Inner Mongolia",
    name == "宁夏回族自治区" ~ "Ningxia",
    name == "青海省" ~ "Qinghai",
    name == "山东省" ~ "Shandong",
    name == "山西省" ~ "Shanxi",
    name == "陕西省" ~ "Shaanxi",
    name == "上海市" ~ "Shanghai",
    name == "四川省" ~ "Sichuan",
    name == "天津市" ~ "Tianjin",
    name == "西藏自治区" ~ "Tibet",
    name == "新疆维吾尔自治区" ~ "Xinjiang",
    name == "云南省" ~ "Yunnan",
    name == "浙江省" ~ "Zhejiang",
    name == "澳门特别行政区" ~ "Macao",
    name == "香港特别行政区" ~ "Hong Kong",
    name == "台湾省" ~ "Taiwan",
    TRUE ~ name  # 保留原始名称以防有未覆盖的情况
  ))

####
# 绘制主地图
main_map <- ggplot(data = china_sf) +
  geom_sf(color = "black", size = 0.1) +  # 绘制省份边界
  # 在地图上标注省份名称，排除指定省份
  geom_sf_text(
    data = china_sf %>% filter(!name %in% c("Beijing", "Tianjin", "Hong Kong", "Macao","境界线")),
    aes(label = name), 
    size = 2.5, 
    color = "red"
  ) +
  coord_sf(xlim = c(73, 136), ylim = c(15, 54)) +  # 设置地图显示范围
  theme_minimal() +  # 使用简洁主题
  theme(
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),  # 移除轴标题
    axis.text = element_blank(),   # 移除轴文本
    axis.ticks = element_blank(),
    text = element_text(family = "Arial")
  )


# 绘制南海地图
southsea_map <- ggplot(data = china_sf) +
  geom_sf(color = "black", size = 0.1) +  # 绘制省份边界
  coord_sf(xlim = c(105, 125), ylim = c(3, 25)) +  # 设置南海区域的坐标范围
  theme_minimal() +  # 使用简洁主题
  theme(
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),  # 移除轴标题
    axis.text = element_blank(),   # 移除轴文本
    axis.ticks = element_blank(),
    text = element_text(family = "Arial")
  )

# 显示地图
main_map
southsea_map

plot_china <- main_map+inset_element(southsea_map,left = 0.8,right = 1.0,bottom = 0.1,top = 0.3)

ggsave("result/中国地图/plot_china.png",plot_china,width = 15,height = 10,units = "in",dpi = 300)







####################  国际比较条图 #########
library(ggplot2)
library(ggsci)
library(dplyr)
library(extrafont)
library(patchwork)
library(cowplot)
library(openxlsx)
library(purrr)
loadfonts(device = "win")

##### 各省份地理分区
location_order <- c("Heilongjiang", "Jilin", "Liaoning", #东北
                    "Hebei", "Shanxi", "Beijing", "Tianjin", "Inner Mongolia", #华北
                    "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi", "Shandong", "Shanghai",#华东
                    "Hubei", "Hunan", "Henan", #华中
                    "Guangdong", "Guangxi", "Hainan", "Hong Kong","Macao", #华南
                    "Chongqing","Sichuan", "Guizhou", "Yunnan", "Tibet",#西南
                    "Shaanxi", "Gansu", "Qinghai", "Ningxia", "Xinjiang",#西北
                    "Global","France"
)

################疾病负担数据 rate   
data_map0 <- read.csv("result/data_mapRate.csv",row.names = 1)
data_map0 <- data_map0%>%
  filter(year=="2023"&cause_name=="Non-communicable diseases")

###加入国际对比数据
data_map00 <- read.csv("data/IHME-GBD_2021_DATA-a4900157-1/国际地中海data_mapRate.csv")
data_map00[data_map00$rei_name == "Diet low in polyunsaturated fatty acids", "rei_name"] <- "Diet low in omega-6 polyunsaturated fatty acids"
data_map0 <- rbind(data_map0,data_map00)


data_map <- data_map0%>%
  mutate(location_name=case_when(
    location_name=="Hong Kong Special Administrative Region of China"~"Hong Kong",
    location_name=="Macao Special Administrative Region of China"~"Macao",
    TRUE~location_name
  ))%>%
  mutate(Region=case_when(location_name%in%c("Heilongjiang", "Jilin", "Liaoning")~"Northeast",
                          location_name %in% c("Hebei", "Shanxi", "Beijing", "Tianjin", "Inner Mongolia") ~ "North China",
                          location_name %in% c("Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi", "Shandong") ~ "East China",
                          location_name %in% c("Henan", "Hubei", "Hunan") ~ "Central China",
                          location_name %in% c("Guangdong", "Guangxi", "Hainan", "Hong Kong", "Macao") ~ "South China",
                          location_name %in% c("Sichuan", "Guizhou", "Yunnan", "Tibet", "Chongqing") ~ "Southwest",
                          location_name %in% c("Shaanxi", "Gansu", "Qinghai", "Ningxia", "Xinjiang") ~ "Northwest",
                          TRUE ~ location_name))
data_map$location_name <- factor(data_map$location_name, levels = rev(location_order))
data_map$Region <- factor(data_map$Region, levels = c("Northeast","North China","East China","Central China","South China","Southwest","Northwest","Global","France"))

data_map1 <-data_map %>% filter(measure_name == "DALYs")


data_map2 <- data_map %>% filter(measure_name == "Deaths")      

##################################################################Deaths
# 遍历所有独特的rei_name并为每个绘制一个图
unique_rei_names <- unique(data_map2$rei_name)

for (rei in unique_rei_names) {
  # 筛选特定rei_name的数据
  subset_data <- data_map2[data_map2$rei_name == rei,]
  global_value <- subset_data %>% filter(location_name == "Global") %>% pull(val)
  france_value <- subset_data %>% filter(location_name == "France") %>% pull(val)
  
  # 生成条形图
  p <- ggplot(subset_data, aes(x = location_name, y = val, fill = Region)) +
    geom_bar(stat = "identity", width = 0.7) +  # 使用identity统计，数据已经是汇总的
    coord_flip() +  # 翻转坐标轴，使条形水平
    geom_hline(yintercept = global_value, linetype = "dashed", color = "red", size = 2) +  # 添加 global 参考线
    geom_hline(yintercept = france_value, linetype = "dashed", color = "blue", size = 2) +  # 添加 France 参考线
    labs(title = paste0(rei), 
         x = "Province", 
         y = "Age-standardized Death Rate (per 100,000 population)") +
    theme_minimal() +  # 使用简洁主题
    theme(legend.position = "None",
          # legend.title = element_text(hjust = 0.5, vjust = 1.5, size = 50),
          # legend.text = element_text(size = 40),
          plot.title = element_text(size = 38),
          panel.background = element_rect(fill = "white", colour = NA), 
          plot.background = element_rect(fill = "white", colour = NA),
          panel.grid.minor = element_blank(),       axis.line = element_line(size = 1, color = "black"),
          axis.title  = element_blank(),  # 移除轴标题
          axis.text.x = element_text(size = 40),
          axis.text.y =  element_blank(),   # 移除轴文
          text = element_text(family = "Arial"),
          aspect.ratio = 2/1  ) + 
    scale_fill_lancet()
  
  # 将图形保存在环境中，命名为 'bar_Deaths' 加上 rei_name
  assign(paste0("bar_Deaths_",rei), p, envir = .GlobalEnv)
}

#################第一列######################

global_value <- data_map2[data_map2$rei_name == "Diet high in sodium",] %>% filter(location_name == "Global") %>% pull(val)
france_value <- data_map2[data_map2$rei_name == "Diet high in sodium",] %>% filter(location_name == "France") %>% pull(val)

`bar_Deaths_Diet high in sodium` <- ggplot(data_map2[data_map2$rei_name == "Diet high in sodium",], aes(x = location_name, y = val, fill = Region)) +
  geom_bar(stat = "identity", width = 0.7) +  # 使用identity统计，数据已经是汇总的
  coord_flip() +  # 翻转坐标轴，使条形水平
  geom_hline(yintercept = global_value, linetype = "dashed", color = "red", size = 2) +  # 添加 global 参考线
  geom_hline(yintercept = france_value, linetype = "dashed", color = "blue", size = 2) +  # 添加 France 参考线
  labs(title = "Diet high in sodium", 
       x = "Province", 
       y = "Age-standardized Death Rate (per 100,000 population)") +
  theme_minimal() +  # 使用简洁主题
  theme(legend.position = "None",
        # legend.title = element_text(hjust = 0.5, vjust = 1.5, size = 50),
        # legend.text = element_text(size = 40),
        plot.title = element_text(size = 38),
        panel.background = element_rect(fill = "white", colour = NA), 
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.minor = element_blank(),       axis.line = element_line(size = 1, color = "black"),
        axis.title = element_blank(),
        axis.text = element_text(size = 40),
        text = element_text(family = "Arial"),
        aspect.ratio = 2/1  ) +     scale_fill_lancet() 


global_value <- data_map2[data_map2$rei_name == "Diet low in fiber",] %>% filter(location_name == "Global") %>% pull(val)
france_value <- data_map2[data_map2$rei_name == "Diet low in fiber",] %>% filter(location_name == "France") %>% pull(val)

`bar_Deaths_Diet low in fiber` <- ggplot(data_map2[data_map2$rei_name == "Diet low in fiber",], aes(x = location_name, y = val, fill = Region)) +
  geom_bar(stat = "identity", width = 0.7) +  # 使用identity统计，数据已经是汇总的
  coord_flip() +  # 翻转坐标轴，使条形水平
  geom_hline(yintercept = global_value, linetype = "dashed", color = "red", size = 2) +  # 添加 global 参考线
  geom_hline(yintercept = france_value, linetype = "dashed", color = "blue", size = 2) +  # 添加 France 参考线
  labs(title = "Diet low in fiber", 
       x = "Province", 
       y = "Age-standardized Death Rate (per 100,000 population)") +
  theme_minimal() +  # 使用简洁主题
  theme(legend.position = "None",
        # legend.title = element_text(hjust = 0.5, vjust = 1.5, size = 50),
        # legend.text = element_text(size = 40),
        plot.title = element_text(size = 38),
        panel.background = element_rect(fill = "white", colour = NA), 
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.minor = element_blank(),       axis.line = element_line(size = 1, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 50),
        axis.text = element_text(size = 40),
        text = element_text(family = "Arial"),
        aspect.ratio = 2/1  ) +     scale_fill_lancet() 

global_value <- data_map2[data_map2$rei_name == "Diet low in milk",] %>% filter(location_name == "Global") %>% pull(val)
france_value <- data_map2[data_map2$rei_name == "Diet low in milk",] %>% filter(location_name == "France") %>% pull(val)
`bar_Deaths_Diet low in milk` <- ggplot(data_map2[data_map2$rei_name == "Diet low in milk",], aes(x = location_name, y = val, fill = Region)) +
  geom_bar(stat = "identity", width = 0.7) +  # 使用identity统计，数据已经是汇总的
  coord_flip() +  # 翻转坐标轴，使条形水平
  geom_hline(yintercept = global_value, linetype = "dashed", color = "red", size = 2) +  # 添加 global 参考线
  geom_hline(yintercept = france_value, linetype = "dashed", color = "blue", size = 2) +  # 添加 France 参考线
  labs(title = "Diet low in milk", 
       x = "Province", 
       y = "Age-standardized Death Rate (per 100,000 population)") +
  theme_minimal() +  # 使用简洁主题
  theme(legend.position = "None",
        # legend.title = element_text(hjust = 0.5, vjust = 1.5, size = 50),
        # legend.text = element_text(size = 40),
        plot.title = element_text(size = 38),
        panel.background = element_rect(fill = "white", colour = NA), 
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.minor = element_blank(),       axis.line = element_line(size = 1, color = "black"),
        axis.title = element_blank(),
        axis.text = element_text(size = 40),
        text = element_text(family = "Arial"),
        aspect.ratio = 2/1  ) +     scale_fill_lancet() 


###########第三行第三个###########
global_value <- data_map2[data_map2$rei_name == "Diet high in processed meat",] %>% filter(location_name == "Global") %>% pull(val)
france_value <- data_map2[data_map2$rei_name == "Diet high in processed meat",] %>% filter(location_name == "France") %>% pull(val)
`bar_Deaths_Diet high in processed meat` <- ggplot(data_map2[data_map2$rei_name == "Diet high in processed meat",], aes(x = location_name, y = val, fill = Region)) +
  geom_bar(stat = "identity", width = 0.7) +  # 使用identity统计，数据已经是汇总的
  coord_flip() +  # 翻转坐标轴，使条形水平
  geom_hline(yintercept = global_value, linetype = "dashed", color = "red", size = 2) +  # 添加 global 参考线
  geom_hline(yintercept = france_value, linetype = "dashed", color = "blue", size = 2) +  # 添加 France 参考线
  labs(title = "Diet high in processed meat", 
       x = "Province", 
       y = "Age-standardized Death Rate") +
  theme_minimal() +  # 使用简洁主题
  theme(legend.position = "None",
        # legend.title = element_text(hjust = 0.5, vjust = 1.5, size = 50),
        # legend.text = element_text(size = 40),
        plot.title = element_text(size = 38),
        panel.background = element_rect(fill = "white", colour = NA), 
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.minor = element_blank(),       axis.line = element_line(size = 1, color = "black"),
        axis.title.x = element_text(size = 50),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 40),
        axis.text.y = element_blank(),
        text = element_text(family = "Arial"),
        aspect.ratio = 2/1  ) +     scale_fill_lancet() 



barplot_Deaths <- (`bar_Deaths_Diet high in sodium` | `bar_Deaths_Diet low in whole grains` | `bar_Deaths_Diet low in fruits`|`bar_Deaths_Diet low in omega-6 polyunsaturated fatty acids` | `bar_Deaths_Diet low in nuts and seeds` )/
  (`bar_Deaths_Diet low in fiber`|`bar_Deaths_Diet low in seafood omega-3 fatty acids` | `bar_Deaths_Diet low in legumes` | `bar_Deaths_Diet high in red meat`|`bar_Deaths_Diet low in vegetables`)/
  (`bar_Deaths_Diet low in milk` | `bar_Deaths_Diet low in calcium`|`bar_Deaths_Diet high in processed meat` | `bar_Deaths_Diet high in trans fatty acids` | `bar_Deaths_Diet high in sugar-sweetened beverages`)

ggsave("result/国际比较条图/barplot_Deaths.png",barplot_Deaths,width = 60,height = 60,dpi = 300,limitsize = FALSE)



#####################DALYs#################     
# 遍历所有独特的rei_name并为每个绘制一个图
unique_rei_names <- unique(data_map1$rei_name)

for (rei in unique_rei_names) {
  # 筛选特定rei_name的数据
  subset_data <- data_map1[data_map1$rei_name == rei,]
  global_value <- subset_data %>% filter(location_name == "Global") %>% pull(val)
  france_value <- subset_data %>% filter(location_name == "France") %>% pull(val)
  # 生成条形图
  p <- ggplot(subset_data, aes(x = location_name, y = val, fill = Region)) +
    geom_bar(stat = "identity", width = 0.7) +  # 使用identity统计，数据已经是汇总的
    coord_flip() +  # 翻转坐标轴，使条形水平
    geom_hline(yintercept = global_value, linetype = "dashed", color = "red", size = 2) +  # 添加 global 参考线
    geom_hline(yintercept = france_value, linetype = "dashed", color = "blue", size = 2) +  # 添加 France 参考线
    labs(title = paste0(rei), 
         x = "Province", 
         y = "Age-standardized DALYs Rate (per 100,000 population)") +
    theme_minimal() +  # 使用简洁主题
    theme(legend.position = "None",
          # legend.title = element_text(hjust = 0.5, vjust = 1.5, size = 50),
          # legend.text = element_text(size = 40),
          plot.title = element_text(size = 38),
          panel.background = element_rect(fill = "white", colour = NA), 
          plot.background = element_rect(fill = "white", colour = NA),
          panel.grid.minor = element_blank(),       axis.line = element_line(size = 1, color = "black"),
          axis.title  = element_blank(),  # 移除轴标题
          axis.text.x = element_text(size = 40),
          axis.text.y =  element_blank(),   # 移除轴文
          text = element_text(family = "Arial"),
          aspect.ratio = 2/1  ) + 
    scale_fill_lancet()  
  
  # 将图形保存在环境中，命名为 'bar_DALYs' 加上 rei_name
  assign(paste0("bar_DALYs_",rei), p, envir = .GlobalEnv)
}

#################第一列######################
global_value <- data_map1[data_map1$rei_name == "Diet high in sodium",] %>% filter(location_name == "Global") %>% pull(val)
france_value <- data_map1[data_map1$rei_name == "Diet high in sodium",] %>% filter(location_name == "France") %>% pull(val)
`bar_DALYs_Diet high in sodium` <- ggplot(data_map1[data_map1$rei_name == "Diet high in sodium",], aes(x = location_name, y = val, fill = Region)) +
  geom_bar(stat = "identity", width = 0.7) +  # 使用identity统计，数据已经是汇总的
  coord_flip() +  # 翻转坐标轴，使条形水平
  geom_hline(yintercept = global_value, linetype = "dashed", color = "red", size = 2) +  # 添加 global 参考线
  geom_hline(yintercept = france_value, linetype = "dashed", color = "blue", size = 2) +  # 添加 France 参考线
  labs(title = "Diet high in sodium", 
       x = "Province", 
       y = "Age-standardized DALYs Rate (per 100,000 population)") +
  theme_minimal() +  # 使用简洁主题
  theme(legend.position = "None",
        # legend.title = element_text(hjust = 0.5, vjust = 1.5, size = 50),
        # legend.text = element_text(size = 40),
        plot.title = element_text(size = 38),
        panel.background = element_rect(fill = "white", colour = NA), 
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.minor = element_blank(),       axis.line = element_line(size = 1, color = "black"),
        axis.title = element_blank(),
        axis.text = element_text(size = 40),
        text = element_text(family = "Arial"),
        aspect.ratio = 2/1  ) +     scale_fill_lancet() 

global_value <- data_map1[data_map1$rei_name == "Diet low in fiber",] %>% filter(location_name == "Global") %>% pull(val)
france_value <- data_map1[data_map1$rei_name == "Diet low in fiber",] %>% filter(location_name == "France") %>% pull(val)
`bar_DALYs_Diet low in fiber` <- ggplot(data_map1[data_map1$rei_name == "Diet low in fiber",], aes(x = location_name, y = val, fill = Region)) +
  geom_bar(stat = "identity", width = 0.7) +  # 使用identity统计，数据已经是汇总的
  coord_flip() +  # 翻转坐标轴，使条形水平
  geom_hline(yintercept = global_value, linetype = "dashed", color = "red", size = 2) +  # 添加 global 参考线
  geom_hline(yintercept = france_value, linetype = "dashed", color = "blue", size = 2) +  # 添加 France 参考线
  labs(title = "Diet low in fiber", 
       x = "Province", 
       y = "Age-standardized DALYs Rate (per 100,000 population)") +
  theme_minimal() +  # 使用简洁主题
  theme(legend.position = "None",
        # legend.title = element_text(hjust = 0.5, vjust = 1.5, size = 50),
        # legend.text = element_text(size = 40),
        plot.title = element_text(size = 38),
        panel.background = element_rect(fill = "white", colour = NA), 
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.minor = element_blank(),       axis.line = element_line(size = 1, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 50),
        axis.text = element_text(size = 40),
        text = element_text(family = "Arial"),
        aspect.ratio = 2/1  ) +     scale_fill_lancet() 

global_value <- data_map1[data_map1$rei_name == "Diet low in milk",] %>% filter(location_name == "Global") %>% pull(val)
france_value <- data_map1[data_map1$rei_name == "Diet low in milk",] %>% filter(location_name == "France") %>% pull(val)
`bar_DALYs_Diet low in milk` <- ggplot(data_map1[data_map1$rei_name == "Diet low in milk",], aes(x = location_name, y = val, fill = Region)) +
  geom_bar(stat = "identity", width = 0.7) +  # 使用identity统计，数据已经是汇总的
  coord_flip() +  # 翻转坐标轴，使条形水平
  geom_hline(yintercept = global_value, linetype = "dashed", color = "red", size = 2) +  # 添加 global 参考线
  geom_hline(yintercept = france_value, linetype = "dashed", color = "blue", size = 2) +  # 添加 France 参考线
  labs(title = "Diet low in milk", 
       x = "Province", 
       y = "Age-standardized DALYs Rate (per 100,000 population)") +
  theme_minimal() +  # 使用简洁主题
  theme(legend.position = "None",
        # legend.title = element_text(hjust = 0.5, vjust = 1.5, size = 50),
        # legend.text = element_text(size = 40),
        plot.title = element_text(size = 38),
        panel.background = element_rect(fill = "white", colour = NA), 
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.minor = element_blank(),       axis.line = element_line(size = 1, color = "black"),
        axis.title = element_blank(),
        axis.text = element_text(size = 40),
        text = element_text(family = "Arial"),
        aspect.ratio = 2/1  ) +     scale_fill_lancet() 


###########第三行第三个###########
global_value <- data_map1[data_map1$rei_name == "Diet high in processed meat",] %>% filter(location_name == "Global") %>% pull(val)
france_value <- data_map1[data_map1$rei_name == "Diet high in processed meat",] %>% filter(location_name == "France") %>% pull(val)
`bar_DALYs_Diet high in processed meat` <- ggplot(data_map1[data_map1$rei_name == "Diet high in processed meat",], aes(x = location_name, y = val, fill = Region)) +
  geom_bar(stat = "identity", width = 0.7) +  # 使用identity统计，数据已经是汇总的
  coord_flip() +  # 翻转坐标轴，使条形水平
  geom_hline(yintercept = global_value, linetype = "dashed", color = "red", size = 2) +  # 添加 global 参考线
  geom_hline(yintercept = france_value, linetype = "dashed", color = "blue", size = 2) +  # 添加 France 参考线
  labs(title = "Diet high in processed meat", 
       x = "Province", 
       y = "Age-standardized DALYs Rate") +
  theme_minimal() +  # 使用简洁主题
  theme(legend.position = "None",
        # legend.title = element_text(hjust = 0.5, vjust = 1.5, size = 50),
        # legend.text = element_text(size = 40),
        plot.title = element_text(size = 38),
        panel.background = element_rect(fill = "white", colour = NA), 
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.minor = element_blank(),       axis.line = element_line(size = 1, color = "black"),
        axis.title.x = element_text(size = 50),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 40),
        axis.text.y = element_blank(),
        text = element_text(family = "Arial"),
        aspect.ratio = 2/1  ) +     scale_fill_lancet() 



barplot_DALYs <- (`bar_DALYs_Diet high in sodium` | `bar_DALYs_Diet low in whole grains` | `bar_DALYs_Diet low in fruits`|`bar_DALYs_Diet low in omega-6 polyunsaturated fatty acids` | `bar_DALYs_Diet low in nuts and seeds` )/
  (`bar_DALYs_Diet low in fiber`|`bar_DALYs_Diet low in seafood omega-3 fatty acids` | `bar_DALYs_Diet low in legumes` | `bar_DALYs_Diet high in red meat`|`bar_DALYs_Diet low in vegetables`)/
  (`bar_DALYs_Diet low in milk` | `bar_DALYs_Diet low in calcium`|`bar_DALYs_Diet high in processed meat` | `bar_DALYs_Diet high in trans fatty acids` | `bar_DALYs_Diet high in sugar-sweetened beverages`)

ggsave("result/国际比较条图/barplot_DALYs.png",barplot_DALYs,width = 60,height = 60,dpi = 300,limitsize = FALSE)



####################################单独生成图例

ggplot(data_map1[data_map1$rei_name == "Diet high in processed meat",], aes(x = location_name, y = val, fill = Region)) +
  geom_bar(stat = "identity", width = 0.7) +  # 使用identity统计，数据已经是汇总的
  coord_flip() +  # 翻转坐标轴，使条形水平
  geom_hline(yintercept = global_value, linetype = "dashed", color = "red", size = 2) +  # 添加 global 参考线
  geom_hline(yintercept = france_value, linetype = "dashed", color = "blue", size = 2) +  # 添加 France 参考线
  labs(title = "Diet high in processed meat", 
       x = "Province", 
       y = "Age-standardized DALYs Rate",fill="Region") +
  theme_minimal() +  # 使用简洁主题
  theme(legend.position = "right",
        # legend.title = element_text(hjust = 0.5, vjust = 1.5, size = 50),
        # legend.text = element_text(size = 40),
        plot.title = element_text(size = 38),
        panel.background = element_rect(fill = "white", colour = NA), 
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.minor = element_blank(),       axis.line = element_line(size = 1, color = "black"),
        axis.title.x = element_text(size = 50),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 40),
        axis.text.y = element_blank(),
        text = element_text(family = "Arial"),
        aspect.ratio = 2/1  ) +     scale_fill_lancet() 



ggplot(data_map1[data_map1$rei_name == "Diet high in processed meat",], aes(x = location_name, y = val, fill = Region)) +
  # 添加条形图
  geom_bar(stat = "identity", width = 0.7) +  
  # 翻转坐标轴
  coord_flip() +  
  # 添加 Global 参考线
  geom_hline(aes(yintercept = global_value, color = "Global"), linetype = "dashed", size = 2) +  
  # 添加 France 参考线
  geom_hline(aes(yintercept = france_value, color = "France"), linetype = "dashed", size = 2) +  
  # 图标题和轴标签
  labs(
    title = "Diet high in processed meat", 
    x = "Province", 
    y = "Age-standardized DALYs Rate"
  ) +
  # 简洁主题
  theme_minimal() +
  theme(
    legend.position = "right",  # 图例位置
    plot.title = element_text(size = 38),
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_blank(),       axis.line = element_line(size = 1, color = "black"),
    axis.title.x = element_text(size = 50),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 40),
    axis.text.y = element_blank(),
    text = element_text(family = "Arial"),
    aspect.ratio = 2/1  
  ) +
  # 隐藏 Region 的图例并添加自定义颜色图例
  scale_fill_lancet(guide = "none") +  
  scale_color_manual(
    name = "Reference Lines",  # 图例标题
    values = c("Global" = "red", "France" = "blue"),
    guide = guide_legend(
      keywidth = unit(2, "cm"),  # 调整图例宽度
      keyheight = unit(0.1, "cm")  # 调整图例高度
    )
  )







