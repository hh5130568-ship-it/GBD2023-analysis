############################################################China GBD####
########change####
file_numbers <- c(1:37)
file_names <- paste0("IHME-GBD_2023_DATA-99d33947-", file_numbers, ".csv")

file_paths <- file.path("~/China GBD/", file_names)

change <- file_paths %>% 
  lapply(read_csv) %>% 
  bind_rows()


unique(change$measure_name)
unique(change$location_name)
unique(change$sex_name)
unique(change$year_start)
unique(change$year_end)
unique(change$metric_name)
unique(change$rei_name)
unique(change$cause_name)
unique(change$age_name)
file_numbers <- c(1:46,48:53)
file_names <- paste0("IHME-GBD_2023_DATA-53fb9a00-", file_numbers, ".csv")

#########annual####
file_paths <- file.path("~/China GBD/", file_names)
膳食annual <- file_paths %>% 
  lapply(read_csv) %>% 
  bind_rows()

names(膳食annual)

unique(膳食annual$measure_name)
unique(膳食annual$location_name)
unique(膳食annual$sex_name)
unique(膳食annual$year)
unique(膳食annual$metric_name)
unique(膳食annual$rei_name)
unique(膳食annual$cause_name)
unique(膳食annual$age_name)

rm(膳食annual)
############dietary_annual1.0####
file_numbers1 <- c("processed meat","red meat","sodium","sugar-sweetened beverages",
                   "trans fatty acids")  
file_names1 <- paste0("Diet high in ", file_numbers1, ".csv")

file_paths1 <- file.path("~/China GBD/GBD2023/合并数据", file_names1)

GBD2023_1 <- file_paths1 %>% 
  lapply(read_csv) %>% 
  bind_rows()

#处理fruits
fruits <- read.csv("~/China GBD/GBD2023/合并数据/Diet low in fruits.csv", 
                   col.names = c(c("measure","location","sex","age","cause","rei","metric","year",
                                   "val","upper","lower"), "Extra1", "Extra2"))

fruits1 <- fruits %>% 
  filter(year == "Diet low in fruits") %>%
  mutate(
    cause = if_else(cause == "Tracheal", "Tracheal, bronchus, and lung cancer", cause),
    rei = if_else(rei == " bronchus", as.character(year), rei),
    metric = if_else(metric == " and lung cancer", as.character(val), metric)) %>% 
  mutate(
    year = as.numeric(upper),
    val = lower,
    upper = ifelse(rep(TRUE, n()), Extra1, NA_real_), 
    lower = ifelse(rep(TRUE, n()), Extra2, NA_real_)) %>% 
  select(measure,location,sex,age,cause,rei,metric,year,val,upper,lower)

fruits <- as.data.frame(fruits)
fruits2 <- fruits %>% 
  filter(cause != "Tracheal") %>% 
  filter(sex != "Fem") %>% 
  mutate(val = as.numeric(val),  
         year = as.numeric(year)) %>%  
  select(measure,location,sex,age,cause,rei,metric,year,val,upper,lower) %>% 
  bind_rows(fruits1)

write.csv(fruits2,file = "~/China GBD/GBD2023/合并数据/Diet low in fruits correct.csv")

#处理总膳食
allrisk <- read.csv("~/China GBD/GBD2023/合并数据/Diet risk factors.csv", 
                    col.names = c("measure", "location", "sex", "age", "cause", "rei",
                                  "metric", "year", "val", "upper", "lower",
                                  "Extra1", "Extra2"))

allrisk1 <- allrisk %>% 
  filter(year == "Dietary risks") %>%
  mutate(
    cause = if_else(cause == "Tracheal", "Tracheal, bronchus, and lung cancer", cause),
    rei = if_else(rei == " bronchus", as.character(year), rei),
    metric = if_else(metric == " and lung cancer", as.character(val), metric)) %>% 
  mutate(
    year = as.numeric(upper),
    val = lower,
    upper = ifelse(rep(TRUE, n()), Extra1, NA_real_), 
    lower = ifelse(rep(TRUE, n()), Extra2, NA_real_)) %>% 
  select(measure,location,sex,age,cause,rei,metric,year,val,upper,lower)

allrisk <- as.data.frame(allrisk)
allrisk2 <- allrisk %>% 
  filter(cause != "Tracheal") %>% 
  filter(sex != "Fem") %>% 
  mutate(val = as.numeric(val),  
         year = as.numeric(year)) %>%  
  select(measure,location,sex,age,cause,rei,metric,year,val,upper,lower) %>% 
  bind_rows(allrisk1)

write.csv(allrisk2,file = "~/China GBD/GBD2023/合并数据/Diet risk factors correct.csv")

file_numbers2 <- c("calcium","fiber","legumes","milk","nuts and seeds",
                   "omege 6 polyunsaturated fatty acids","seafood omega 3 fatty acid",
                   "vegetables", "whole grains")
file_names2 <- paste0("Diet low in ", file_numbers2, ".csv")

file_paths2 <- file.path("~/China GBD/GBD2023/合并数据", file_names2)

GBD2023_2 <- file_paths2 %>% 
  lapply(read_csv) %>% 
  bind_rows()

GBD2023 <- GBD2023_1 %>% 
  bind_rows(GBD2023_2) %>% 
  bind_rows(fruits2) %>% 
  bind_rows(allrisk2)

names(GBD2023)

saveRDS(GBD2023,"~/China GBD/GBD2023/合并数据/GBD2023.rds")

dietary_annual1.0 <- GBD2023
colnames(dietary_annual1.0) <- c("measure_name","location_name","sex_name","age_name","cause_name","rei_name",     
                                 "metric_name","year","val","upper","lower")
names(dietary_annual1.0)
unique(dietary_annual1.0$measure_name)
unique(dietary_annual1.0$location_name)
unique(dietary_annual1.0$sex_name)
unique(dietary_annual1.0$year)
unique(dietary_annual1.0$metric_name)
unique(dietary_annual1.0$rei_name)
unique(dietary_annual1.0$cause_name)
unique(dietary_annual1.0$age_name)

dietary_annual1.0$measure_name[dietary_annual1.0$measure_name=="DALYs (Disability-Adjusted Life Years)"] <- "DALYs"
dietary_annual1.0 <- dietary_annual1.0 %>% 
  mutate(location_name=case_when(
    location_name=="Hong Kong Special Administrative Region of China" ~ "Hong Kong",
    location_name=="Macao Special Administrative Region of China" ~ "Macao",
    location_name=="People's Republic of China" ~ "China",
    TRUE ~ location_name
  ))

############################################################ level1 全因####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&dietary_annual1.0$cause_name=="Non-communicable diseases")
allrisk_cause_1 <- allrisk_cause %>% 
  select(measure_name,location_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name %in% c("Number","Rate")) %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("All ages","Age-standardized")) %>% 
  filter(!(metric_name=="Rate" & age_name=="All ages")) %>% 
  filter(sex_name=="Both")

val_wide <- dcast(allrisk_cause_1, location_name ~ 
                    measure_name+metric_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause_1, location_name ~ 
                      measure_name+metric_name+year, value.var = "upper")
colnames(upper_wide)[2:9] <- paste(colnames(upper_wide)[2:9],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause_1, location_name ~ 
                      measure_name+metric_name+year, value.var = "lower")
colnames(lower_wide)[2:9] <- paste(colnames(lower_wide)[2:9],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name")),
              lower_wide, by = c("location_name"))

data0 <- data0[,c("location_name",
                  "Deaths_Number_1990","Deaths_Number_1990_lower","Deaths_Number_1990_upper",
                  "Deaths_Number_2023","Deaths_Number_2023_lower","Deaths_Number_2023_upper",
                  "Deaths_Rate_1990","Deaths_Rate_1990_lower","Deaths_Rate_1990_upper",
                  "Deaths_Rate_2023","Deaths_Rate_2023_lower","Deaths_Rate_2023_upper",
                  
                  "DALYs_Number_1990","DALYs_Number_1990_lower","DALYs_Number_1990_upper",
                  "DALYs_Number_2023","DALYs_Number_2023_lower","DALYs_Number_2023_upper",
                  "DALYs_Rate_1990","DALYs_Rate_1990_lower","DALYs_Rate_1990_upper",
                  "DALYs_Rate_2023","DALYs_Rate_2023_lower","DALYs_Rate_2023_upper"
)] %>% 
  mutate(
    # 对 Number 列保留 0 位小数
    across(contains("Number"), ~ round(.x, 0)),
    # 对 Rate 列保留 2 位小数
    across(contains("Rate"), ~ round(.x, 2))
  )
data0$location_name <- factor(data0$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data0 <- data0 %>% 
  arrange(location_name)
# Apply rounding to all numeric columns except the first one
# data0[-(1:2)] <- lapply(data0[-(1:2)], function(x) if(is.numeric(x)) round(x, 2) else x)

########性别
allrisk_cause_2 <- allrisk_cause %>% 
  select(measure_name,location_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name %in% c("Number","Rate")) %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("All ages","Age-standardized")) %>% 
  filter(!(metric_name=="Rate" & age_name=="All ages")) %>% 
  filter(!(sex_name=="Both")) %>% #不分男女
  filter(location_name=="China")


val_wide <- dcast(allrisk_cause_2, location_name+sex_name ~ 
                    measure_name+metric_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause_2, location_name+sex_name ~ 
                      measure_name+metric_name+year, value.var = "upper")
colnames(upper_wide)[3:10] <- paste(colnames(upper_wide)[3:10],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause_2, location_name+sex_name ~ 
                      measure_name+metric_name+year, value.var = "lower")
colnames(lower_wide)[3:10] <- paste(colnames(lower_wide)[3:10],"_lower",sep = "")

data1 <-merge(merge(val_wide, upper_wide, by = c("location_name","sex_name")),
              lower_wide, by = c("location_name","sex_name"))

data1 <- data1[,c("location_name","sex_name",
                  "Deaths_Number_1990","Deaths_Number_1990_lower","Deaths_Number_1990_upper",
                  "Deaths_Number_2023","Deaths_Number_2023_lower","Deaths_Number_2023_upper",
                  "Deaths_Rate_1990","Deaths_Rate_1990_lower","Deaths_Rate_1990_upper",
                  "Deaths_Rate_2023","Deaths_Rate_2023_lower","Deaths_Rate_2023_upper",
                  
                  "DALYs_Number_1990","DALYs_Number_1990_lower","DALYs_Number_1990_upper",
                  "DALYs_Number_2023","DALYs_Number_2023_lower","DALYs_Number_2023_upper",
                  "DALYs_Rate_1990","DALYs_Rate_1990_lower","DALYs_Rate_1990_upper",
                  "DALYs_Rate_2023","DALYs_Rate_2023_lower","DALYs_Rate_2023_upper"
)] %>% 
  mutate(
    # 对 Number 列保留 0 位小数
    across(contains("Number"), ~ round(.x, 0)),
    # 对 Rate 列保留 2 位小数
    across(contains("Rate"), ~ round(.x, 2))
  )
data1 <- data1[-1]
names(data1)[1] <- "location_name"
##########年龄组
allrisk_cause_3 <- allrisk_cause %>% 
  select(measure_name,location_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name %in% c("Number","Rate")) %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("15-49 years","50-69 years","70+ years")) %>% 
  filter(sex_name=="Both") %>% 
  filter(location_name=="China")


val_wide <- dcast(allrisk_cause_3, location_name+age_name ~ 
                    measure_name+metric_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause_3, location_name+age_name ~ 
                      measure_name+metric_name+year, value.var = "upper")
colnames(upper_wide)[3:10] <- paste(colnames(upper_wide)[3:10],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause_3, location_name+age_name ~ 
                      measure_name+metric_name+year, value.var = "lower")
colnames(lower_wide)[3:10] <- paste(colnames(lower_wide)[3:10],"_lower",sep = "")

data2 <-merge(merge(val_wide, upper_wide, by = c("location_name","age_name")),
              lower_wide, by = c("location_name","age_name"))

data2 <- data2[,c("location_name","age_name",
                  "Deaths_Number_1990","Deaths_Number_1990_lower","Deaths_Number_1990_upper",
                  "Deaths_Number_2023","Deaths_Number_2023_lower","Deaths_Number_2023_upper",
                  "Deaths_Rate_1990","Deaths_Rate_1990_lower","Deaths_Rate_1990_upper",
                  "Deaths_Rate_2023","Deaths_Rate_2023_lower","Deaths_Rate_2023_upper",
                  
                  "DALYs_Number_1990","DALYs_Number_1990_lower","DALYs_Number_1990_upper",
                  "DALYs_Number_2023","DALYs_Number_2023_lower","DALYs_Number_2023_upper",
                  "DALYs_Rate_1990","DALYs_Rate_1990_lower","DALYs_Rate_1990_upper",
                  "DALYs_Rate_2023","DALYs_Rate_2023_lower","DALYs_Rate_2023_upper"
)] %>% 
  mutate(
    # 对 Number 列保留 0 位小数
    across(contains("Number"), ~ round(.x, 0)),
    # 对 Rate 列保留 2 位小数
    across(contains("Rate"), ~ round(.x, 2))
  )
data2 <- data2[-1]
names(data2)[1] <- "location_name"

####合并
data_all <- data0[1,] %>% 
  rbind(data1) %>% 
  rbind(data2) %>% 
  rbind(data0[-1,])

data_all <- data_all %>% 
  mutate(
    Deaths_num_change = round(((Deaths_Number_2023 - Deaths_Number_1990) / Deaths_Number_1990) * 100, 2),
    Deaths_rate_change = round(((Deaths_Rate_2023 - Deaths_Rate_1990) / Deaths_Rate_1990) * 100, 2),
    
    DALYs_num_change = round(((DALYs_Number_2023 - DALYs_Number_1990) / DALYs_Number_1990) * 100, 2),
    DALYs_rate_change = round(((DALYs_Rate_2023 - DALYs_Rate_1990) / DALYs_Rate_1990) * 100, 2)
  ) %>% 
  mutate(Deaths_num_1990_95UI=paste(Deaths_Number_1990,"(",Deaths_Number_1990_lower,",",Deaths_Number_1990_upper,")"),
         Deaths_num_2023_95UI=paste(Deaths_Number_2023,"(",Deaths_Number_2023_lower,",",Deaths_Number_2023_upper,")"),
         Deaths_rate_1990_95UI=paste(Deaths_Rate_1990,"(",Deaths_Rate_1990_lower,",",Deaths_Rate_1990_upper,")"),
         Deaths_rate_2023_95UI=paste(Deaths_Rate_2023,"(",Deaths_Rate_2023_lower,",",Deaths_Rate_2023_upper,")"),
         
         DALYs_num_1990_95UI=paste(DALYs_Number_1990,"(",DALYs_Number_1990_lower,",",DALYs_Number_1990_upper,")"),
         DALYs_num_2023_95UI=paste(DALYs_Number_2023,"(",DALYs_Number_2023_lower,",",DALYs_Number_2023_upper,")"),
         DALYs_rate_1990_95UI=paste(DALYs_Rate_1990,"(",DALYs_Rate_1990_lower,",",DALYs_Rate_1990_upper,")"),
         DALYs_rate_2023_95UI=paste(DALYs_Rate_2023,"(",DALYs_Rate_2023_lower,",",DALYs_Rate_2023_upper,")")
  ) %>% 
  select(location_name,
         Deaths_num_1990_95UI, Deaths_num_2023_95UI, Deaths_num_change,
         Deaths_rate_1990_95UI, Deaths_rate_2023_95UI, Deaths_rate_change,
         
         DALYs_num_1990_95UI, DALYs_num_2023_95UI, DALYs_num_change,
         DALYs_rate_1990_95UI, DALYs_rate_2023_95UI, DALYs_rate_change)
write.csv(data_all,file = "~/China GBD/GBD2023/Table/allrisk-level1.csv")
############################################################ level1 every risk factor####
# 定义饮食因素

dietary_factors <- c("Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", 
                     "Diet low in omega-6 polyunsaturated fatty acids", "Diet low in nuts and seeds", 
                     "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids", 
                     "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", 
                     "Diet low in vegetables", "Diet low in calcium", "Diet high in sugar-sweetened beverages", 
                     "Diet high in trans fatty acids")

# 循环处理每个饮食因素
for (rei in dietary_factors) {
  
  # 筛选数据
  allrisk_cause_temp <- filter(dietary_annual1.0, dietary_annual1.0$rei_name == rei & dietary_annual1.0$cause_name == "Non-communicable diseases")
  
  # 处理全体性别
  allrisk_cause_1 <- allrisk_cause_temp %>% 
    select(measure_name, location_name, sex_name, age_name, metric_name, year, val, upper, lower) %>% 
    filter(metric_name %in% c("Number", "Rate")) %>% 
    filter(year %in% c(1990, 2023)) %>% 
    filter(age_name %in% c("All ages", "Age-standardized")) %>% 
    filter(!(metric_name == "Rate" & age_name == "All ages")) %>% 
    filter(sex_name == "Both")
  
  val_wide <- dcast(allrisk_cause_1, location_name ~ measure_name + metric_name + year, value.var = "val")
  upper_wide <- dcast(allrisk_cause_1, location_name ~ measure_name + metric_name + year, value.var = "upper")
  colnames(upper_wide)[2:9] <- paste(colnames(upper_wide)[2:9], "_upper", sep = "")
  lower_wide <- dcast(allrisk_cause_1, location_name ~ measure_name + metric_name + year, value.var = "lower")
  colnames(lower_wide)[2:9] <- paste(colnames(lower_wide)[2:9], "_lower", sep = "")
  
  data0 <- merge(merge(val_wide, upper_wide, by = c("location_name")),
                 lower_wide, by = c("location_name"))
  
  data0 <- data0[, c("location_name", 
                     "Deaths_Number_1990", "Deaths_Number_1990_lower", "Deaths_Number_1990_upper", 
                     "Deaths_Number_2023", "Deaths_Number_2023_lower", "Deaths_Number_2023_upper",
                     "Deaths_Rate_1990", "Deaths_Rate_1990_lower", "Deaths_Rate_1990_upper", 
                     "Deaths_Rate_2023", "Deaths_Rate_2023_lower", "Deaths_Rate_2023_upper",
                     "DALYs_Number_1990", "DALYs_Number_1990_lower", "DALYs_Number_1990_upper", 
                     "DALYs_Number_2023", "DALYs_Number_2023_lower", "DALYs_Number_2023_upper", 
                     "DALYs_Rate_1990", "DALYs_Rate_1990_lower", "DALYs_Rate_1990_upper", 
                     "DALYs_Rate_2023", "DALYs_Rate_2023_lower", "DALYs_Rate_2023_upper")] %>% 
    mutate(across(contains("Number"), ~ round(.x, 0)),
           across(contains("Rate"), ~ round(.x, 2)))
  data0$location_name <- factor(data0$location_name, 
                                levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                           "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                           "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                           "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                           "Shaanxi", "Gansu", "Hong Kong", "Macao"))
  data0 <- data0 %>% 
    arrange(location_name)
  # 处理性别
  allrisk_cause_2 <- allrisk_cause_temp %>% 
    select(measure_name, location_name, sex_name, age_name, metric_name, year, val, upper, lower) %>% 
    filter(metric_name %in% c("Number", "Rate")) %>% 
    filter(year %in% c(1990, 2023)) %>% 
    filter(age_name %in% c("All ages", "Age-standardized")) %>% 
    filter(!(metric_name == "Rate" & age_name == "All ages")) %>% 
    filter(!(sex_name == "Both")) %>% 
    filter(location_name == "China")
  
  val_wide <- dcast(allrisk_cause_2, location_name + sex_name ~ measure_name + metric_name + year, value.var = "val")
  upper_wide <- dcast(allrisk_cause_2, location_name + sex_name ~ measure_name + metric_name + year, value.var = "upper")
  colnames(upper_wide)[3:10] <- paste(colnames(upper_wide)[3:10], "_upper", sep = "")
  lower_wide <- dcast(allrisk_cause_2, location_name + sex_name ~ measure_name + metric_name + year, value.var = "lower")
  colnames(lower_wide)[3:10] <- paste(colnames(lower_wide)[3:10], "_lower", sep = "")
  
  data1 <- merge(merge(val_wide, upper_wide, by = c("location_name", "sex_name")),
                 lower_wide, by = c("location_name", "sex_name"))
  
  data1 <- data1[, c("location_name", "sex_name", 
                     "Deaths_Number_1990", "Deaths_Number_1990_lower", "Deaths_Number_1990_upper", 
                     "Deaths_Number_2023", "Deaths_Number_2023_lower", "Deaths_Number_2023_upper",
                     "Deaths_Rate_1990", "Deaths_Rate_1990_lower", "Deaths_Rate_1990_upper", 
                     "Deaths_Rate_2023", "Deaths_Rate_2023_lower", "Deaths_Rate_2023_upper",
                     "DALYs_Number_1990", "DALYs_Number_1990_lower", "DALYs_Number_1990_upper", 
                     "DALYs_Number_2023", "DALYs_Number_2023_lower", "DALYs_Number_2023_upper", 
                     "DALYs_Rate_1990", "DALYs_Rate_1990_lower", "DALYs_Rate_1990_upper", 
                     "DALYs_Rate_2023", "DALYs_Rate_2023_lower", "DALYs_Rate_2023_upper")] %>% 
    mutate(across(contains("Number"), ~ round(.x, 0)),
           across(contains("Rate"), ~ round(.x, 2)))
  data1 <- data1[-1]
  names(data1)[1] <- "location_name"
  
  # 处理年龄组
  allrisk_cause_3 <- allrisk_cause_temp %>% 
    select(measure_name, location_name, sex_name, age_name, metric_name, year, val, upper, lower) %>% 
    filter(metric_name %in% c("Number", "Rate")) %>% 
    filter(year %in% c(1990, 2023)) %>% 
    filter(age_name %in% c("15-49 years", "50-69 years", "70+ years")) %>% 
    filter(sex_name == "Both") %>% 
    filter(location_name == "China")
  
  val_wide <- dcast(allrisk_cause_3, location_name + age_name ~ measure_name + metric_name + year, value.var = "val")
  upper_wide <- dcast(allrisk_cause_3, location_name + age_name ~ measure_name + metric_name + year, value.var = "upper")
  colnames(upper_wide)[3:10] <- paste(colnames(upper_wide)[3:10], "_upper", sep = "")
  lower_wide <- dcast(allrisk_cause_3, location_name + age_name ~ measure_name + metric_name + year, value.var = "lower")
  colnames(lower_wide)[3:10] <- paste(colnames(lower_wide)[3:10], "_lower", sep = "")
  
  data2 <- merge(merge(val_wide, upper_wide, by = c("location_name", "age_name")),
                 lower_wide, by = c("location_name", "age_name"))
  
  data2 <- data2[, c("location_name", "age_name", 
                     "Deaths_Number_1990", "Deaths_Number_1990_lower", "Deaths_Number_1990_upper", 
                     "Deaths_Number_2023", "Deaths_Number_2023_lower", "Deaths_Number_2023_upper",
                     "Deaths_Rate_1990", "Deaths_Rate_1990_lower", "Deaths_Rate_1990_upper", 
                     "Deaths_Rate_2023", "Deaths_Rate_2023_lower", "Deaths_Rate_2023_upper",
                     "DALYs_Number_1990", "DALYs_Number_1990_lower", "DALYs_Number_1990_upper", 
                     "DALYs_Number_2023", "DALYs_Number_2023_lower", "DALYs_Number_2023_upper", 
                     "DALYs_Rate_1990", "DALYs_Rate_1990_lower", "DALYs_Rate_1990_upper", 
                     "DALYs_Rate_2023", "DALYs_Rate_2023_lower", "DALYs_Rate_2023_upper")] %>% 
    mutate(across(contains("Number"), ~ round(.x, 0)),
           across(contains("Rate"), ~ round(.x, 2)))
  data2 <- data2[-1]
  names(data2)[1] <- "location_name"
  
  # 合并
  data_all <- data0[1, ] %>% 
    rbind(data1) %>% 
    rbind(data2) %>% 
    rbind(data0[-1, ])
  
  data_all <- data_all %>% 
    mutate(
      Deaths_num_change = round(((Deaths_Number_2023 - Deaths_Number_1990) / Deaths_Number_1990) * 100, 2),
      Deaths_rate_change = round(((Deaths_Rate_2023 - Deaths_Rate_1990) / Deaths_Rate_1990) * 100, 2),
      
      DALYs_num_change = round(((DALYs_Number_2023 - DALYs_Number_1990) / DALYs_Number_1990) * 100, 2),
      DALYs_rate_change = round(((DALYs_Rate_2023 - DALYs_Rate_1990) / DALYs_Rate_1990) * 100, 2)
    ) %>% 
    mutate(Deaths_num_1990_95UI = paste(Deaths_Number_1990, "(", Deaths_Number_1990_lower, ",", Deaths_Number_1990_upper, ")"),
           Deaths_num_2023_95UI = paste(Deaths_Number_2023, "(", Deaths_Number_2023_lower, ",", Deaths_Number_2023_upper, ")"),
           Deaths_rate_1990_95UI = paste(Deaths_Rate_1990, "(", Deaths_Rate_1990_lower, ",", Deaths_Rate_1990_upper, ")"),
           Deaths_rate_2023_95UI = paste(Deaths_Rate_2023, "(", Deaths_Rate_2023_lower, ",", Deaths_Rate_2023_upper, ")"),
           DALYs_num_1990_95UI = paste(DALYs_Number_1990, "(", DALYs_Number_1990_lower, ",", DALYs_Number_1990_upper, ")"),
           DALYs_num_2023_95UI = paste(DALYs_Number_2023, "(", DALYs_Number_2023_lower, ",", DALYs_Number_2023_upper, ")"),
           DALYs_rate_1990_95UI = paste(DALYs_Rate_1990, "(", DALYs_Rate_1990_lower, ",", DALYs_Rate_1990_upper, ")"),
           DALYs_rate_2023_95UI = paste(DALYs_Rate_2023, "(", DALYs_Rate_2023_lower, ",", DALYs_Rate_2023_upper, ")")
    ) %>% 
    select(location_name,
           Deaths_num_1990_95UI, Deaths_num_2023_95UI, Deaths_num_change,
           Deaths_rate_1990_95UI, Deaths_rate_2023_95UI, Deaths_rate_change,
           
           DALYs_num_1990_95UI, DALYs_num_2023_95UI, DALYs_num_change,
           DALYs_rate_1990_95UI, DALYs_rate_2023_95UI, DALYs_rate_change)
  
  # 写出结果到 CSV
  write.csv(data_all, file = paste0("~/China GBD/GBD2023/Table/", rei, "-level1.csv"))
}




############################################################ level2 全因 ####
Causes <- c("Neoplasms",
            "Cardiovascular diseases",
            "Diabetes and kidney diseases")
for (Cause in Causes) {
  allrisk_level2 <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks" & dietary_annual1.0$cause_name==Cause)
  allrisk_level2_1 <- allrisk_level2 %>% 
    select(measure_name, location_name, sex_name, age_name, metric_name, year, val, upper, lower) %>% 
    filter(metric_name %in% c("Number", "Rate")) %>% 
    filter(year %in% c(1990, 2023)) %>% 
    filter(age_name %in% c("All ages", "Age-standardized")) %>% 
    filter(!(metric_name == "Rate" & age_name == "All ages")) %>% 
    filter(sex_name == "Both")
  
  val_wide <- dcast(allrisk_level2_1, location_name ~ measure_name + metric_name + year, value.var = "val")
  upper_wide <- dcast(allrisk_level2_1, location_name ~ measure_name + metric_name + year, value.var = "upper")
  colnames(upper_wide)[2:9] <- paste(colnames(upper_wide)[2:9], "_upper", sep = "")
  lower_wide <- dcast(allrisk_level2_1, location_name ~ measure_name + metric_name + year, value.var = "lower")
  colnames(lower_wide)[2:9] <- paste(colnames(lower_wide)[2:9], "_lower", sep = "")
  
  data0 <- merge(merge(val_wide, upper_wide, by = c("location_name")),
                 lower_wide, by = c("location_name"))
  
  data0 <- data0[, c("location_name", 
                     "Deaths_Number_1990", "Deaths_Number_1990_lower", "Deaths_Number_1990_upper", 
                     "Deaths_Number_2023", "Deaths_Number_2023_lower", "Deaths_Number_2023_upper",
                     "Deaths_Rate_1990", "Deaths_Rate_1990_lower", "Deaths_Rate_1990_upper", 
                     "Deaths_Rate_2023", "Deaths_Rate_2023_lower", "Deaths_Rate_2023_upper",
                     "DALYs_Number_1990", "DALYs_Number_1990_lower", "DALYs_Number_1990_upper", 
                     "DALYs_Number_2023", "DALYs_Number_2023_lower", "DALYs_Number_2023_upper", 
                     "DALYs_Rate_1990", "DALYs_Rate_1990_lower", "DALYs_Rate_1990_upper", 
                     "DALYs_Rate_2023", "DALYs_Rate_2023_lower", "DALYs_Rate_2023_upper")] %>% 
    mutate(across(contains("Number"), ~ round(.x, 0)),
           across(contains("Rate"), ~ round(.x, 2)))
  data0$location_name <- factor(data0$location_name, 
                                levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                           "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                           "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                           "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                           "Shaanxi", "Gansu", "Hong Kong", "Macao"))
  data0 <- data0 %>% 
    arrange(location_name)
  # 处理性别
  allrisk_level2_2 <- allrisk_level2 %>% 
    select(measure_name, location_name, sex_name, age_name, metric_name, year, val, upper, lower) %>% 
    filter(metric_name %in% c("Number", "Rate")) %>% 
    filter(year %in% c(1990, 2023)) %>% 
    filter(age_name %in% c("All ages", "Age-standardized")) %>% 
    filter(!(metric_name == "Rate" & age_name == "All ages")) %>% 
    filter(!(sex_name == "Both")) %>% 
    filter(location_name == "China")
  
  val_wide <- dcast(allrisk_level2_2, location_name + sex_name ~ measure_name + metric_name + year, value.var = "val")
  upper_wide <- dcast(allrisk_level2_2, location_name + sex_name ~ measure_name + metric_name + year, value.var = "upper")
  colnames(upper_wide)[3:10] <- paste(colnames(upper_wide)[3:10], "_upper", sep = "")
  lower_wide <- dcast(allrisk_level2_2, location_name + sex_name ~ measure_name + metric_name + year, value.var = "lower")
  colnames(lower_wide)[3:10] <- paste(colnames(lower_wide)[3:10], "_lower", sep = "")
  
  data1 <- merge(merge(val_wide, upper_wide, by = c("location_name", "sex_name")),
                 lower_wide, by = c("location_name", "sex_name"))
  
  data1 <- data1[, c("location_name", "sex_name", 
                     "Deaths_Number_1990", "Deaths_Number_1990_lower", "Deaths_Number_1990_upper", 
                     "Deaths_Number_2023", "Deaths_Number_2023_lower", "Deaths_Number_2023_upper",
                     "Deaths_Rate_1990", "Deaths_Rate_1990_lower", "Deaths_Rate_1990_upper", 
                     "Deaths_Rate_2023", "Deaths_Rate_2023_lower", "Deaths_Rate_2023_upper",
                     "DALYs_Number_1990", "DALYs_Number_1990_lower", "DALYs_Number_1990_upper", 
                     "DALYs_Number_2023", "DALYs_Number_2023_lower", "DALYs_Number_2023_upper", 
                     "DALYs_Rate_1990", "DALYs_Rate_1990_lower", "DALYs_Rate_1990_upper", 
                     "DALYs_Rate_2023", "DALYs_Rate_2023_lower", "DALYs_Rate_2023_upper")] %>% 
    mutate(across(contains("Number"), ~ round(.x, 0)),
           across(contains("Rate"), ~ round(.x, 2)))
  data1 <- data1[-1]
  names(data1)[1] <- "location_name"
  
  # 处理年龄组
  allrisk_level2_3 <- allrisk_level2 %>% 
    select(measure_name, location_name, sex_name, age_name, metric_name, year, val, upper, lower) %>% 
    filter(metric_name %in% c("Number", "Rate")) %>% 
    filter(year %in% c(1990, 2023)) %>% 
    filter(age_name %in% c("15-49 years", "50-69 years", "70+ years")) %>% 
    filter(sex_name == "Both") %>% 
    filter(location_name == "China")
  
  val_wide <- dcast(allrisk_level2_3, location_name + age_name ~ measure_name + metric_name + year, value.var = "val")
  upper_wide <- dcast(allrisk_level2_3, location_name + age_name ~ measure_name + metric_name + year, value.var = "upper")
  colnames(upper_wide)[3:10] <- paste(colnames(upper_wide)[3:10], "_upper", sep = "")
  lower_wide <- dcast(allrisk_level2_3, location_name + age_name ~ measure_name + metric_name + year, value.var = "lower")
  colnames(lower_wide)[3:10] <- paste(colnames(lower_wide)[3:10], "_lower", sep = "")
  
  data2 <- merge(merge(val_wide, upper_wide, by = c("location_name", "age_name")),
                 lower_wide, by = c("location_name", "age_name"))
  
  data2 <- data2[, c("location_name", "age_name", 
                     "Deaths_Number_1990", "Deaths_Number_1990_lower", "Deaths_Number_1990_upper", 
                     "Deaths_Number_2023", "Deaths_Number_2023_lower", "Deaths_Number_2023_upper",
                     "Deaths_Rate_1990", "Deaths_Rate_1990_lower", "Deaths_Rate_1990_upper", 
                     "Deaths_Rate_2023", "Deaths_Rate_2023_lower", "Deaths_Rate_2023_upper",
                     "DALYs_Number_1990", "DALYs_Number_1990_lower", "DALYs_Number_1990_upper", 
                     "DALYs_Number_2023", "DALYs_Number_2023_lower", "DALYs_Number_2023_upper", 
                     "DALYs_Rate_1990", "DALYs_Rate_1990_lower", "DALYs_Rate_1990_upper", 
                     "DALYs_Rate_2023", "DALYs_Rate_2023_lower", "DALYs_Rate_2023_upper")] %>% 
    mutate(across(contains("Number"), ~ round(.x, 0)),
           across(contains("Rate"), ~ round(.x, 2)))
  data2 <- data2[-1]
  names(data2)[1] <- "location_name"
  
  # 合并
  data_all <- data0[1, ] %>% 
    rbind(data1) %>% 
    rbind(data2) %>% 
    rbind(data0[-1, ])
  
  data_all <- data_all %>% 
    mutate(
      Deaths_num_change = round(((Deaths_Number_2023 - Deaths_Number_1990) / Deaths_Number_1990) * 100, 2),
      Deaths_rate_change = round(((Deaths_Rate_2023 - Deaths_Rate_1990) / Deaths_Rate_1990) * 100, 2),
      
      DALYs_num_change = round(((DALYs_Number_2023 - DALYs_Number_1990) / DALYs_Number_1990) * 100, 2),
      DALYs_rate_change = round(((DALYs_Rate_2023 - DALYs_Rate_1990) / DALYs_Rate_1990) * 100, 2)
    ) %>% 
    mutate(Deaths_num_1990_95UI = paste(Deaths_Number_1990, "(", Deaths_Number_1990_lower, ",", Deaths_Number_1990_upper, ")"),
           Deaths_num_2023_95UI = paste(Deaths_Number_2023, "(", Deaths_Number_2023_lower, ",", Deaths_Number_2023_upper, ")"),
           Deaths_rate_1990_95UI = paste(Deaths_Rate_1990, "(", Deaths_Rate_1990_lower, ",", Deaths_Rate_1990_upper, ")"),
           Deaths_rate_2023_95UI = paste(Deaths_Rate_2023, "(", Deaths_Rate_2023_lower, ",", Deaths_Rate_2023_upper, ")"),
           DALYs_num_1990_95UI = paste(DALYs_Number_1990, "(", DALYs_Number_1990_lower, ",", DALYs_Number_1990_upper, ")"),
           DALYs_num_2023_95UI = paste(DALYs_Number_2023, "(", DALYs_Number_2023_lower, ",", DALYs_Number_2023_upper, ")"),
           DALYs_rate_1990_95UI = paste(DALYs_Rate_1990, "(", DALYs_Rate_1990_lower, ",", DALYs_Rate_1990_upper, ")"),
           DALYs_rate_2023_95UI = paste(DALYs_Rate_2023, "(", DALYs_Rate_2023_lower, ",", DALYs_Rate_2023_upper, ")")
    ) %>% 
    select(location_name,
           Deaths_num_1990_95UI, Deaths_num_2023_95UI, Deaths_num_change,
           Deaths_rate_1990_95UI, Deaths_rate_2023_95UI, Deaths_rate_change,
           
           DALYs_num_1990_95UI, DALYs_num_2023_95UI, DALYs_num_change,
           DALYs_rate_1990_95UI, DALYs_rate_2023_95UI, DALYs_rate_change)
  
  # 写出结果到 CSV
  write.csv(data_all, file = paste0("~/China GBD/GBD2023/Table/allrisk-level2-",Cause, ".csv"))
}


############################################################年龄分组 level1 全因####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&dietary_annual1.0$cause_name=="Non-communicable diseases")
allrisk_cause <- allrisk_cause %>% 
  select(measure_name,location_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name %in% c("Number","Rate")) %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))

allrisk_cause$age_name <- factor(allrisk_cause$age_name, levels = c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))

val_wide <- dcast(allrisk_cause, location_name+age_name ~ 
                    measure_name+sex_name+metric_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause, location_name+age_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "upper")
colnames(upper_wide)[3:26] <- paste(colnames(upper_wide)[3:26],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause, location_name+age_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "lower")
colnames(lower_wide)[3:26] <- paste(colnames(lower_wide)[3:26],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","age_name")),
              lower_wide, by = c("location_name","age_name"))

data0 <- data0[,c("location_name","age_name",
                  "Deaths_Both_Number_1990","Deaths_Both_Number_1990_lower","Deaths_Both_Number_1990_upper",
                  "Deaths_Both_Number_2023","Deaths_Both_Number_2023_lower","Deaths_Both_Number_2023_upper",
                  "Deaths_Both_Rate_1990","Deaths_Both_Rate_1990_lower","Deaths_Both_Rate_1990_upper",
                  "Deaths_Both_Rate_2023","Deaths_Both_Rate_2023_lower","Deaths_Both_Rate_2023_upper",
                  
                  "Deaths_Male_Number_1990","Deaths_Male_Number_1990_lower","Deaths_Male_Number_1990_upper",
                  "Deaths_Male_Number_2023","Deaths_Male_Number_2023_lower","Deaths_Male_Number_2023_upper",
                  "Deaths_Male_Rate_1990","Deaths_Male_Rate_1990_lower","Deaths_Male_Rate_1990_upper",
                  "Deaths_Male_Rate_2023","Deaths_Male_Rate_2023_lower","Deaths_Male_Rate_2023_upper",
                  
                  "Deaths_Female_Number_1990","Deaths_Female_Number_1990_lower","Deaths_Female_Number_1990_upper",
                  "Deaths_Female_Number_2023","Deaths_Female_Number_2023_lower","Deaths_Female_Number_2023_upper",
                  "Deaths_Female_Rate_1990","Deaths_Female_Rate_1990_lower","Deaths_Female_Rate_1990_upper",
                  "Deaths_Female_Rate_2023","Deaths_Female_Rate_2023_lower","Deaths_Female_Rate_2023_upper",
                  
                  
                  "DALYs_Both_Number_1990","DALYs_Both_Number_1990_lower","DALYs_Both_Number_1990_upper",
                  "DALYs_Both_Number_2023","DALYs_Both_Number_2023_lower","DALYs_Both_Number_2023_upper",
                  "DALYs_Both_Rate_1990","DALYs_Both_Rate_1990_lower","DALYs_Both_Rate_1990_upper",
                  "DALYs_Both_Rate_2023","DALYs_Both_Rate_2023_lower","DALYs_Both_Rate_2023_upper",
                  
                  "DALYs_Male_Number_1990","DALYs_Male_Number_1990_lower","DALYs_Male_Number_1990_upper",
                  "DALYs_Male_Number_2023","DALYs_Male_Number_2023_lower","DALYs_Male_Number_2023_upper",
                  "DALYs_Male_Rate_1990","DALYs_Male_Rate_1990_lower","DALYs_Male_Rate_1990_upper",
                  "DALYs_Male_Rate_2023","DALYs_Male_Rate_2023_lower","DALYs_Male_Rate_2023_upper",
                  
                  "DALYs_Female_Number_1990","DALYs_Female_Number_1990_lower","DALYs_Female_Number_1990_upper",
                  "DALYs_Female_Number_2023","DALYs_Female_Number_2023_lower","DALYs_Female_Number_2023_upper",
                  "DALYs_Female_Rate_1990","DALYs_Female_Rate_1990_lower","DALYs_Female_Rate_1990_upper",
                  "DALYs_Female_Rate_2023","DALYs_Female_Rate_2023_lower","DALYs_Female_Rate_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data0 <- data0 %>% 
  arrange(location_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:2)] <- lapply(data0[-(1:2)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_num_Both_change = round(((Deaths_Both_Number_2023 - Deaths_Both_Number_1990) / Deaths_Both_Number_1990) * 100, 2),
    Deaths_rate_Both_change = round(((Deaths_Both_Rate_2023 - Deaths_Both_Rate_1990) / Deaths_Both_Rate_1990) * 100, 2),
    Deaths_num_Male_change = round(((Deaths_Male_Number_2023 - Deaths_Male_Number_1990) / Deaths_Male_Number_1990) * 100, 2),
    Deaths_rate_Male_change = round(((Deaths_Male_Rate_2023 - Deaths_Male_Rate_1990) / Deaths_Male_Rate_1990) * 100, 2),
    Deaths_num_Female_change = round(((Deaths_Female_Number_2023 - Deaths_Female_Number_1990) / Deaths_Female_Number_1990) * 100, 2),
    Deaths_rate_Female_change = round(((Deaths_Female_Rate_2023 - Deaths_Female_Rate_1990) / Deaths_Female_Rate_1990) * 100, 2),
    
    DALYs_num_Both_change = round(((DALYs_Both_Number_2023 - DALYs_Both_Number_1990) / DALYs_Both_Number_1990) * 100, 2),
    DALYs_rate_Both_change = round(((DALYs_Both_Rate_2023 - DALYs_Both_Rate_1990) / DALYs_Both_Rate_1990) * 100, 2),
    DALYs_num_Male_change = round(((DALYs_Male_Number_2023 - DALYs_Male_Number_1990) / DALYs_Male_Number_1990) * 100, 2),
    DALYs_rate_Male_change = round(((DALYs_Male_Rate_2023 - DALYs_Male_Rate_1990) / DALYs_Male_Rate_1990) * 100, 2),
    DALYs_num_Female_change = round(((DALYs_Female_Number_2023 - DALYs_Female_Number_1990) / DALYs_Female_Number_1990) * 100, 2),
    DALYs_rate_Female_change = round(((DALYs_Female_Rate_2023 - DALYs_Female_Rate_1990) / DALYs_Female_Rate_1990) * 100, 2)
  ) %>% 
  mutate(Deaths_num_Both_1990_95UI=paste(Deaths_Both_Number_1990,"(",Deaths_Both_Number_1990_lower,",",Deaths_Both_Number_1990_upper,")"),
         Deaths_num_Both_2023_95UI=paste(Deaths_Both_Number_2023,"(",Deaths_Both_Number_2023_lower,",",Deaths_Both_Number_2023_upper,")"),
         Deaths_rate_Both_1990_95UI=paste(Deaths_Both_Rate_1990,"(",Deaths_Both_Rate_1990_lower,",",Deaths_Both_Rate_1990_upper,")"),
         Deaths_rate_Both_2023_95UI=paste(Deaths_Both_Rate_2023,"(",Deaths_Both_Rate_2023_lower,",",Deaths_Both_Rate_2023_upper,")"),
         
         Deaths_num_Male_1990_95UI=paste(Deaths_Male_Number_1990,"(",Deaths_Male_Number_1990_lower,",",Deaths_Male_Number_1990_upper,")"),
         Deaths_num_Male_2023_95UI=paste(Deaths_Male_Number_2023,"(",Deaths_Male_Number_2023_lower,",",Deaths_Male_Number_2023_upper,")"),
         Deaths_rate_Male_1990_95UI=paste(Deaths_Male_Rate_1990,"(",Deaths_Male_Rate_1990_lower,",",Deaths_Male_Rate_1990_upper,")"),
         Deaths_rate_Male_2023_95UI=paste(Deaths_Male_Rate_2023,"(",Deaths_Male_Rate_2023_lower,",",Deaths_Male_Rate_2023_upper,")"),
         
         Deaths_num_Female_1990_95UI=paste(Deaths_Female_Number_1990,"(",Deaths_Female_Number_1990_lower,",",Deaths_Female_Number_1990_upper,")"),
         Deaths_num_Female_2023_95UI=paste(Deaths_Female_Number_2023,"(",Deaths_Female_Number_2023_lower,",",Deaths_Female_Number_2023_upper,")"),
         Deaths_rate_Female_1990_95UI=paste(Deaths_Female_Rate_1990,"(",Deaths_Female_Rate_1990_lower,",",Deaths_Female_Rate_1990_upper,")"),
         Deaths_rate_Female_2023_95UI=paste(Deaths_Female_Rate_2023,"(",Deaths_Female_Rate_2023_lower,",",Deaths_Female_Rate_2023_upper,")"),
         
         DALYs_num_Both_1990_95UI=paste(DALYs_Both_Number_1990,"(",DALYs_Both_Number_1990_lower,",",DALYs_Both_Number_1990_upper,")"),
         DALYs_num_Both_2023_95UI=paste(DALYs_Both_Number_2023,"(",DALYs_Both_Number_2023_lower,",",DALYs_Both_Number_2023_upper,")"),
         DALYs_rate_Both_1990_95UI=paste(DALYs_Both_Rate_1990,"(",DALYs_Both_Rate_1990_lower,",",DALYs_Both_Rate_1990_upper,")"),
         DALYs_rate_Both_2023_95UI=paste(DALYs_Both_Rate_2023,"(",DALYs_Both_Rate_2023_lower,",",DALYs_Both_Rate_2023_upper,")"),
         
         DALYs_num_Male_1990_95UI=paste(DALYs_Male_Number_1990,"(",DALYs_Male_Number_1990_lower,",",DALYs_Male_Number_1990_upper,")"),
         DALYs_num_Male_2023_95UI=paste(DALYs_Male_Number_2023,"(",DALYs_Male_Number_2023_lower,",",DALYs_Male_Number_2023_upper,")"),
         DALYs_rate_Male_1990_95UI=paste(DALYs_Male_Rate_1990,"(",DALYs_Male_Rate_1990_lower,",",DALYs_Male_Rate_1990_upper,")"),
         DALYs_rate_Male_2023_95UI=paste(DALYs_Male_Rate_2023,"(",DALYs_Male_Rate_2023_lower,",",DALYs_Male_Rate_2023_upper,")"),
         
         DALYs_num_Female_1990_95UI=paste(DALYs_Female_Number_1990,"(",DALYs_Female_Number_1990_lower,",",DALYs_Female_Number_1990_upper,")"),
         DALYs_num_Female_2023_95UI=paste(DALYs_Female_Number_2023,"(",DALYs_Female_Number_2023_lower,",",DALYs_Female_Number_2023_upper,")"),
         DALYs_rate_Female_1990_95UI=paste(DALYs_Female_Rate_1990,"(",DALYs_Female_Rate_1990_lower,",",DALYs_Female_Rate_1990_upper,")"),
         DALYs_rate_Female_2023_95UI=paste(DALYs_Female_Rate_2023,"(",DALYs_Female_Rate_2023_lower,",",DALYs_Female_Rate_2023_upper,")")) %>% 
  select(location_name,age_name,
         Deaths_num_Both_1990_95UI, Deaths_num_Both_2023_95UI, Deaths_num_Both_change,
         Deaths_rate_Both_1990_95UI, Deaths_rate_Both_2023_95UI, Deaths_rate_Both_change,
         Deaths_num_Male_1990_95UI, Deaths_num_Male_2023_95UI, Deaths_num_Male_change,
         Deaths_rate_Male_1990_95UI, Deaths_rate_Male_2023_95UI, Deaths_rate_Male_change,
         Deaths_num_Female_1990_95UI, Deaths_num_Female_2023_95UI, Deaths_num_Female_change,
         Deaths_rate_Female_1990_95UI, Deaths_rate_Female_2023_95UI, Deaths_rate_Female_change,
         DALYs_num_Both_1990_95UI, DALYs_num_Both_2023_95UI, DALYs_num_Both_change,
         DALYs_rate_Both_1990_95UI, DALYs_rate_Both_2023_95UI, DALYs_rate_Both_change,
         DALYs_num_Male_1990_95UI, DALYs_num_Male_2023_95UI, DALYs_num_Male_change,
         DALYs_rate_Male_1990_95UI, DALYs_rate_Male_2023_95UI, DALYs_rate_Male_change,
         DALYs_num_Female_1990_95UI, DALYs_num_Female_2023_95UI, DALYs_num_Female_change,
         DALYs_rate_Female_1990_95UI, DALYs_rate_Female_2023_95UI, DALYs_rate_Female_change)

data0 <- data0 %>% 
  arrange(age_name,location_name)


write.csv(data0,file = "~/China GBD/GBD2023/Table/allrisk-level1-agegroup.csv")


############################################################年龄分组 level1 every risk factor####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name%in%c("Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", "Diet low in omega-6 polyunsaturated fatty acids",
                                                                          "Diet low in nuts and seeds", "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids",
                                                                          "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", "Diet low in vegetables", "Diet low in calcium",
                                                                          "Diet high in sugar-sweetened beverages", "Diet high in trans fatty acids")&dietary_annual1.0$cause_name=="Non-communicable diseases")
allrisk_cause <- allrisk_cause %>% 
  select(measure_name,location_name,rei_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name %in% c("Number","Rate")) %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))

allrisk_cause$age_name <- factor(allrisk_cause$age_name, levels = c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))
allrisk_cause$rei_name <- factor(allrisk_cause$rei_name, levels = c("Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", "Diet low in omega-6 polyunsaturated fatty acids",
                                                                    "Diet low in nuts and seeds", "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids",
                                                                    "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", "Diet low in vegetables", "Diet low in calcium",
                                                                    "Diet high in sugar-sweetened beverages", "Diet high in trans fatty acids"))



val_wide <- dcast(allrisk_cause, location_name+age_name+rei_name ~ 
                    measure_name+sex_name+metric_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause, location_name+age_name+rei_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "upper")
colnames(upper_wide)[4:27] <- paste(colnames(upper_wide)[4:27],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause, location_name+age_name+rei_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "lower")
colnames(lower_wide)[4:27] <- paste(colnames(lower_wide)[4:27],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","age_name","rei_name")),
              lower_wide, by = c("location_name","age_name","rei_name"))

data0 <- data0[,c("location_name","age_name","rei_name",
                  "Deaths_Both_Number_1990","Deaths_Both_Number_1990_lower","Deaths_Both_Number_1990_upper",
                  "Deaths_Both_Number_2023","Deaths_Both_Number_2023_lower","Deaths_Both_Number_2023_upper",
                  "Deaths_Both_Rate_1990","Deaths_Both_Rate_1990_lower","Deaths_Both_Rate_1990_upper",
                  "Deaths_Both_Rate_2023","Deaths_Both_Rate_2023_lower","Deaths_Both_Rate_2023_upper",
                  
                  "Deaths_Male_Number_1990","Deaths_Male_Number_1990_lower","Deaths_Male_Number_1990_upper",
                  "Deaths_Male_Number_2023","Deaths_Male_Number_2023_lower","Deaths_Male_Number_2023_upper",
                  "Deaths_Male_Rate_1990","Deaths_Male_Rate_1990_lower","Deaths_Male_Rate_1990_upper",
                  "Deaths_Male_Rate_2023","Deaths_Male_Rate_2023_lower","Deaths_Male_Rate_2023_upper",
                  
                  "Deaths_Female_Number_1990","Deaths_Female_Number_1990_lower","Deaths_Female_Number_1990_upper",
                  "Deaths_Female_Number_2023","Deaths_Female_Number_2023_lower","Deaths_Female_Number_2023_upper",
                  "Deaths_Female_Rate_1990","Deaths_Female_Rate_1990_lower","Deaths_Female_Rate_1990_upper",
                  "Deaths_Female_Rate_2023","Deaths_Female_Rate_2023_lower","Deaths_Female_Rate_2023_upper",
                  
                  
                  "DALYs_Both_Number_1990","DALYs_Both_Number_1990_lower","DALYs_Both_Number_1990_upper",
                  "DALYs_Both_Number_2023","DALYs_Both_Number_2023_lower","DALYs_Both_Number_2023_upper",
                  "DALYs_Both_Rate_1990","DALYs_Both_Rate_1990_lower","DALYs_Both_Rate_1990_upper",
                  "DALYs_Both_Rate_2023","DALYs_Both_Rate_2023_lower","DALYs_Both_Rate_2023_upper",
                  
                  "DALYs_Male_Number_1990","DALYs_Male_Number_1990_lower","DALYs_Male_Number_1990_upper",
                  "DALYs_Male_Number_2023","DALYs_Male_Number_2023_lower","DALYs_Male_Number_2023_upper",
                  "DALYs_Male_Rate_1990","DALYs_Male_Rate_1990_lower","DALYs_Male_Rate_1990_upper",
                  "DALYs_Male_Rate_2023","DALYs_Male_Rate_2023_lower","DALYs_Male_Rate_2023_upper",
                  
                  "DALYs_Female_Number_1990","DALYs_Female_Number_1990_lower","DALYs_Female_Number_1990_upper",
                  "DALYs_Female_Number_2023","DALYs_Female_Number_2023_lower","DALYs_Female_Number_2023_upper",
                  "DALYs_Female_Rate_1990","DALYs_Female_Rate_1990_lower","DALYs_Female_Rate_1990_upper",
                  "DALYs_Female_Rate_2023","DALYs_Female_Rate_2023_lower","DALYs_Female_Rate_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data0 <- data0 %>% 
  arrange(location_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:3)] <- lapply(data0[-(1:3)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_num_Both_change = round(((Deaths_Both_Number_2023 - Deaths_Both_Number_1990) / Deaths_Both_Number_1990) * 100, 2),
    Deaths_rate_Both_change = round(((Deaths_Both_Rate_2023 - Deaths_Both_Rate_1990) / Deaths_Both_Rate_1990) * 100, 2),
    Deaths_num_Male_change = round(((Deaths_Male_Number_2023 - Deaths_Male_Number_1990) / Deaths_Male_Number_1990) * 100, 2),
    Deaths_rate_Male_change = round(((Deaths_Male_Rate_2023 - Deaths_Male_Rate_1990) / Deaths_Male_Rate_1990) * 100, 2),
    Deaths_num_Female_change = round(((Deaths_Female_Number_2023 - Deaths_Female_Number_1990) / Deaths_Female_Number_1990) * 100, 2),
    Deaths_rate_Female_change = round(((Deaths_Female_Rate_2023 - Deaths_Female_Rate_1990) / Deaths_Female_Rate_1990) * 100, 2),
    
    DALYs_num_Both_change = round(((DALYs_Both_Number_2023 - DALYs_Both_Number_1990) / DALYs_Both_Number_1990) * 100, 2),
    DALYs_rate_Both_change = round(((DALYs_Both_Rate_2023 - DALYs_Both_Rate_1990) / DALYs_Both_Rate_1990) * 100, 2),
    DALYs_num_Male_change = round(((DALYs_Male_Number_2023 - DALYs_Male_Number_1990) / DALYs_Male_Number_1990) * 100, 2),
    DALYs_rate_Male_change = round(((DALYs_Male_Rate_2023 - DALYs_Male_Rate_1990) / DALYs_Male_Rate_1990) * 100, 2),
    DALYs_num_Female_change = round(((DALYs_Female_Number_2023 - DALYs_Female_Number_1990) / DALYs_Female_Number_1990) * 100, 2),
    DALYs_rate_Female_change = round(((DALYs_Female_Rate_2023 - DALYs_Female_Rate_1990) / DALYs_Female_Rate_1990) * 100, 2)
  ) %>% 
  mutate(Deaths_num_Both_1990_95UI=paste(Deaths_Both_Number_1990,"(",Deaths_Both_Number_1990_lower,",",Deaths_Both_Number_1990_upper,")"),
         Deaths_num_Both_2023_95UI=paste(Deaths_Both_Number_2023,"(",Deaths_Both_Number_2023_lower,",",Deaths_Both_Number_2023_upper,")"),
         Deaths_rate_Both_1990_95UI=paste(Deaths_Both_Rate_1990,"(",Deaths_Both_Rate_1990_lower,",",Deaths_Both_Rate_1990_upper,")"),
         Deaths_rate_Both_2023_95UI=paste(Deaths_Both_Rate_2023,"(",Deaths_Both_Rate_2023_lower,",",Deaths_Both_Rate_2023_upper,")"),
         
         Deaths_num_Male_1990_95UI=paste(Deaths_Male_Number_1990,"(",Deaths_Male_Number_1990_lower,",",Deaths_Male_Number_1990_upper,")"),
         Deaths_num_Male_2023_95UI=paste(Deaths_Male_Number_2023,"(",Deaths_Male_Number_2023_lower,",",Deaths_Male_Number_2023_upper,")"),
         Deaths_rate_Male_1990_95UI=paste(Deaths_Male_Rate_1990,"(",Deaths_Male_Rate_1990_lower,",",Deaths_Male_Rate_1990_upper,")"),
         Deaths_rate_Male_2023_95UI=paste(Deaths_Male_Rate_2023,"(",Deaths_Male_Rate_2023_lower,",",Deaths_Male_Rate_2023_upper,")"),
         
         Deaths_num_Female_1990_95UI=paste(Deaths_Female_Number_1990,"(",Deaths_Female_Number_1990_lower,",",Deaths_Female_Number_1990_upper,")"),
         Deaths_num_Female_2023_95UI=paste(Deaths_Female_Number_2023,"(",Deaths_Female_Number_2023_lower,",",Deaths_Female_Number_2023_upper,")"),
         Deaths_rate_Female_1990_95UI=paste(Deaths_Female_Rate_1990,"(",Deaths_Female_Rate_1990_lower,",",Deaths_Female_Rate_1990_upper,")"),
         Deaths_rate_Female_2023_95UI=paste(Deaths_Female_Rate_2023,"(",Deaths_Female_Rate_2023_lower,",",Deaths_Female_Rate_2023_upper,")"),
         
         DALYs_num_Both_1990_95UI=paste(DALYs_Both_Number_1990,"(",DALYs_Both_Number_1990_lower,",",DALYs_Both_Number_1990_upper,")"),
         DALYs_num_Both_2023_95UI=paste(DALYs_Both_Number_2023,"(",DALYs_Both_Number_2023_lower,",",DALYs_Both_Number_2023_upper,")"),
         DALYs_rate_Both_1990_95UI=paste(DALYs_Both_Rate_1990,"(",DALYs_Both_Rate_1990_lower,",",DALYs_Both_Rate_1990_upper,")"),
         DALYs_rate_Both_2023_95UI=paste(DALYs_Both_Rate_2023,"(",DALYs_Both_Rate_2023_lower,",",DALYs_Both_Rate_2023_upper,")"),
         
         DALYs_num_Male_1990_95UI=paste(DALYs_Male_Number_1990,"(",DALYs_Male_Number_1990_lower,",",DALYs_Male_Number_1990_upper,")"),
         DALYs_num_Male_2023_95UI=paste(DALYs_Male_Number_2023,"(",DALYs_Male_Number_2023_lower,",",DALYs_Male_Number_2023_upper,")"),
         DALYs_rate_Male_1990_95UI=paste(DALYs_Male_Rate_1990,"(",DALYs_Male_Rate_1990_lower,",",DALYs_Male_Rate_1990_upper,")"),
         DALYs_rate_Male_2023_95UI=paste(DALYs_Male_Rate_2023,"(",DALYs_Male_Rate_2023_lower,",",DALYs_Male_Rate_2023_upper,")"),
         
         DALYs_num_Female_1990_95UI=paste(DALYs_Female_Number_1990,"(",DALYs_Female_Number_1990_lower,",",DALYs_Female_Number_1990_upper,")"),
         DALYs_num_Female_2023_95UI=paste(DALYs_Female_Number_2023,"(",DALYs_Female_Number_2023_lower,",",DALYs_Female_Number_2023_upper,")"),
         DALYs_rate_Female_1990_95UI=paste(DALYs_Female_Rate_1990,"(",DALYs_Female_Rate_1990_lower,",",DALYs_Female_Rate_1990_upper,")"),
         DALYs_rate_Female_2023_95UI=paste(DALYs_Female_Rate_2023,"(",DALYs_Female_Rate_2023_lower,",",DALYs_Female_Rate_2023_upper,")")) %>% 
  select(location_name,age_name,rei_name,
         Deaths_num_Both_1990_95UI, Deaths_num_Both_2023_95UI, Deaths_num_Both_change,
         Deaths_rate_Both_1990_95UI, Deaths_rate_Both_2023_95UI, Deaths_rate_Both_change,
         Deaths_num_Male_1990_95UI, Deaths_num_Male_2023_95UI, Deaths_num_Male_change,
         Deaths_rate_Male_1990_95UI, Deaths_rate_Male_2023_95UI, Deaths_rate_Male_change,
         Deaths_num_Female_1990_95UI, Deaths_num_Female_2023_95UI, Deaths_num_Female_change,
         Deaths_rate_Female_1990_95UI, Deaths_rate_Female_2023_95UI, Deaths_rate_Female_change,
         DALYs_num_Both_1990_95UI, DALYs_num_Both_2023_95UI, DALYs_num_Both_change,
         DALYs_rate_Both_1990_95UI, DALYs_rate_Both_2023_95UI, DALYs_rate_Both_change,
         DALYs_num_Male_1990_95UI, DALYs_num_Male_2023_95UI, DALYs_num_Male_change,
         DALYs_rate_Male_1990_95UI, DALYs_rate_Male_2023_95UI, DALYs_rate_Male_change,
         DALYs_num_Female_1990_95UI, DALYs_num_Female_2023_95UI, DALYs_num_Female_change,
         DALYs_rate_Female_1990_95UI, DALYs_rate_Female_2023_95UI, DALYs_rate_Female_change)

data0 <- data0 %>% 
  group_by(age_name,rei_name) %>% 
  arrange(age_name,rei_name,location_name) %>% 
  ungroup()

write.csv(data0,file = "~/China GBD/GBD2023/Table/everyrisk-level1-agegroup.csv")



############################################################年龄分组 level2 全因####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&dietary_annual1.0$cause_name%in% c("Neoplasms",
                                                                                                                         "Cardiovascular diseases", 
                                                                                                                         "Diabetes and kidney diseases"))
allrisk_cause <- allrisk_cause %>% 
  select(measure_name,location_name,sex_name,age_name,cause_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name %in% c("Number","Rate")) %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))

allrisk_cause$age_name <- factor(allrisk_cause$age_name, levels = c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))
allrisk_cause$cause_name <- factor(allrisk_cause$cause_name, levels = c("Cardiovascular diseases",
                                                                        "Neoplasms",
                                                                        "Diabetes and kidney diseases"))


val_wide <- dcast(allrisk_cause, location_name+age_name+cause_name ~ 
                    measure_name+sex_name+metric_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause, location_name+age_name+cause_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "upper")
colnames(upper_wide)[4:27] <- paste(colnames(upper_wide)[4:27],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause, location_name+age_name+cause_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "lower")
colnames(lower_wide)[4:27] <- paste(colnames(lower_wide)[4:27],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","age_name","cause_name")),
              lower_wide, by = c("location_name","age_name","cause_name"))

data0 <- data0[,c("location_name","age_name","cause_name",
                  "Deaths_Both_Number_1990","Deaths_Both_Number_1990_lower","Deaths_Both_Number_1990_upper",
                  "Deaths_Both_Number_2023","Deaths_Both_Number_2023_lower","Deaths_Both_Number_2023_upper",
                  "Deaths_Both_Rate_1990","Deaths_Both_Rate_1990_lower","Deaths_Both_Rate_1990_upper",
                  "Deaths_Both_Rate_2023","Deaths_Both_Rate_2023_lower","Deaths_Both_Rate_2023_upper",
                  
                  "Deaths_Male_Number_1990","Deaths_Male_Number_1990_lower","Deaths_Male_Number_1990_upper",
                  "Deaths_Male_Number_2023","Deaths_Male_Number_2023_lower","Deaths_Male_Number_2023_upper",
                  "Deaths_Male_Rate_1990","Deaths_Male_Rate_1990_lower","Deaths_Male_Rate_1990_upper",
                  "Deaths_Male_Rate_2023","Deaths_Male_Rate_2023_lower","Deaths_Male_Rate_2023_upper",
                  
                  "Deaths_Female_Number_1990","Deaths_Female_Number_1990_lower","Deaths_Female_Number_1990_upper",
                  "Deaths_Female_Number_2023","Deaths_Female_Number_2023_lower","Deaths_Female_Number_2023_upper",
                  "Deaths_Female_Rate_1990","Deaths_Female_Rate_1990_lower","Deaths_Female_Rate_1990_upper",
                  "Deaths_Female_Rate_2023","Deaths_Female_Rate_2023_lower","Deaths_Female_Rate_2023_upper",
                  
                  
                  "DALYs_Both_Number_1990","DALYs_Both_Number_1990_lower","DALYs_Both_Number_1990_upper",
                  "DALYs_Both_Number_2023","DALYs_Both_Number_2023_lower","DALYs_Both_Number_2023_upper",
                  "DALYs_Both_Rate_1990","DALYs_Both_Rate_1990_lower","DALYs_Both_Rate_1990_upper",
                  "DALYs_Both_Rate_2023","DALYs_Both_Rate_2023_lower","DALYs_Both_Rate_2023_upper",
                  
                  "DALYs_Male_Number_1990","DALYs_Male_Number_1990_lower","DALYs_Male_Number_1990_upper",
                  "DALYs_Male_Number_2023","DALYs_Male_Number_2023_lower","DALYs_Male_Number_2023_upper",
                  "DALYs_Male_Rate_1990","DALYs_Male_Rate_1990_lower","DALYs_Male_Rate_1990_upper",
                  "DALYs_Male_Rate_2023","DALYs_Male_Rate_2023_lower","DALYs_Male_Rate_2023_upper",
                  
                  "DALYs_Female_Number_1990","DALYs_Female_Number_1990_lower","DALYs_Female_Number_1990_upper",
                  "DALYs_Female_Number_2023","DALYs_Female_Number_2023_lower","DALYs_Female_Number_2023_upper",
                  "DALYs_Female_Rate_1990","DALYs_Female_Rate_1990_lower","DALYs_Female_Rate_1990_upper",
                  "DALYs_Female_Rate_2023","DALYs_Female_Rate_2023_lower","DALYs_Female_Rate_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data0 <- data0 %>% 
  arrange(location_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:3)] <- lapply(data0[-(1:3)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_num_Both_change = round(((Deaths_Both_Number_2023 - Deaths_Both_Number_1990) / Deaths_Both_Number_1990) * 100, 2),
    Deaths_rate_Both_change = round(((Deaths_Both_Rate_2023 - Deaths_Both_Rate_1990) / Deaths_Both_Rate_1990) * 100, 2),
    Deaths_num_Male_change = round(((Deaths_Male_Number_2023 - Deaths_Male_Number_1990) / Deaths_Male_Number_1990) * 100, 2),
    Deaths_rate_Male_change = round(((Deaths_Male_Rate_2023 - Deaths_Male_Rate_1990) / Deaths_Male_Rate_1990) * 100, 2),
    Deaths_num_Female_change = round(((Deaths_Female_Number_2023 - Deaths_Female_Number_1990) / Deaths_Female_Number_1990) * 100, 2),
    Deaths_rate_Female_change = round(((Deaths_Female_Rate_2023 - Deaths_Female_Rate_1990) / Deaths_Female_Rate_1990) * 100, 2),
    
    DALYs_num_Both_change = round(((DALYs_Both_Number_2023 - DALYs_Both_Number_1990) / DALYs_Both_Number_1990) * 100, 2),
    DALYs_rate_Both_change = round(((DALYs_Both_Rate_2023 - DALYs_Both_Rate_1990) / DALYs_Both_Rate_1990) * 100, 2),
    DALYs_num_Male_change = round(((DALYs_Male_Number_2023 - DALYs_Male_Number_1990) / DALYs_Male_Number_1990) * 100, 2),
    DALYs_rate_Male_change = round(((DALYs_Male_Rate_2023 - DALYs_Male_Rate_1990) / DALYs_Male_Rate_1990) * 100, 2),
    DALYs_num_Female_change = round(((DALYs_Female_Number_2023 - DALYs_Female_Number_1990) / DALYs_Female_Number_1990) * 100, 2),
    DALYs_rate_Female_change = round(((DALYs_Female_Rate_2023 - DALYs_Female_Rate_1990) / DALYs_Female_Rate_1990) * 100, 2)
  ) %>% 
  mutate(Deaths_num_Both_1990_95UI=paste(Deaths_Both_Number_1990,"(",Deaths_Both_Number_1990_lower,",",Deaths_Both_Number_1990_upper,")"),
         Deaths_num_Both_2023_95UI=paste(Deaths_Both_Number_2023,"(",Deaths_Both_Number_2023_lower,",",Deaths_Both_Number_2023_upper,")"),
         Deaths_rate_Both_1990_95UI=paste(Deaths_Both_Rate_1990,"(",Deaths_Both_Rate_1990_lower,",",Deaths_Both_Rate_1990_upper,")"),
         Deaths_rate_Both_2023_95UI=paste(Deaths_Both_Rate_2023,"(",Deaths_Both_Rate_2023_lower,",",Deaths_Both_Rate_2023_upper,")"),
         
         Deaths_num_Male_1990_95UI=paste(Deaths_Male_Number_1990,"(",Deaths_Male_Number_1990_lower,",",Deaths_Male_Number_1990_upper,")"),
         Deaths_num_Male_2023_95UI=paste(Deaths_Male_Number_2023,"(",Deaths_Male_Number_2023_lower,",",Deaths_Male_Number_2023_upper,")"),
         Deaths_rate_Male_1990_95UI=paste(Deaths_Male_Rate_1990,"(",Deaths_Male_Rate_1990_lower,",",Deaths_Male_Rate_1990_upper,")"),
         Deaths_rate_Male_2023_95UI=paste(Deaths_Male_Rate_2023,"(",Deaths_Male_Rate_2023_lower,",",Deaths_Male_Rate_2023_upper,")"),
         
         Deaths_num_Female_1990_95UI=paste(Deaths_Female_Number_1990,"(",Deaths_Female_Number_1990_lower,",",Deaths_Female_Number_1990_upper,")"),
         Deaths_num_Female_2023_95UI=paste(Deaths_Female_Number_2023,"(",Deaths_Female_Number_2023_lower,",",Deaths_Female_Number_2023_upper,")"),
         Deaths_rate_Female_1990_95UI=paste(Deaths_Female_Rate_1990,"(",Deaths_Female_Rate_1990_lower,",",Deaths_Female_Rate_1990_upper,")"),
         Deaths_rate_Female_2023_95UI=paste(Deaths_Female_Rate_2023,"(",Deaths_Female_Rate_2023_lower,",",Deaths_Female_Rate_2023_upper,")"),
         
         DALYs_num_Both_1990_95UI=paste(DALYs_Both_Number_1990,"(",DALYs_Both_Number_1990_lower,",",DALYs_Both_Number_1990_upper,")"),
         DALYs_num_Both_2023_95UI=paste(DALYs_Both_Number_2023,"(",DALYs_Both_Number_2023_lower,",",DALYs_Both_Number_2023_upper,")"),
         DALYs_rate_Both_1990_95UI=paste(DALYs_Both_Rate_1990,"(",DALYs_Both_Rate_1990_lower,",",DALYs_Both_Rate_1990_upper,")"),
         DALYs_rate_Both_2023_95UI=paste(DALYs_Both_Rate_2023,"(",DALYs_Both_Rate_2023_lower,",",DALYs_Both_Rate_2023_upper,")"),
         
         DALYs_num_Male_1990_95UI=paste(DALYs_Male_Number_1990,"(",DALYs_Male_Number_1990_lower,",",DALYs_Male_Number_1990_upper,")"),
         DALYs_num_Male_2023_95UI=paste(DALYs_Male_Number_2023,"(",DALYs_Male_Number_2023_lower,",",DALYs_Male_Number_2023_upper,")"),
         DALYs_rate_Male_1990_95UI=paste(DALYs_Male_Rate_1990,"(",DALYs_Male_Rate_1990_lower,",",DALYs_Male_Rate_1990_upper,")"),
         DALYs_rate_Male_2023_95UI=paste(DALYs_Male_Rate_2023,"(",DALYs_Male_Rate_2023_lower,",",DALYs_Male_Rate_2023_upper,")"),
         
         DALYs_num_Female_1990_95UI=paste(DALYs_Female_Number_1990,"(",DALYs_Female_Number_1990_lower,",",DALYs_Female_Number_1990_upper,")"),
         DALYs_num_Female_2023_95UI=paste(DALYs_Female_Number_2023,"(",DALYs_Female_Number_2023_lower,",",DALYs_Female_Number_2023_upper,")"),
         DALYs_rate_Female_1990_95UI=paste(DALYs_Female_Rate_1990,"(",DALYs_Female_Rate_1990_lower,",",DALYs_Female_Rate_1990_upper,")"),
         DALYs_rate_Female_2023_95UI=paste(DALYs_Female_Rate_2023,"(",DALYs_Female_Rate_2023_lower,",",DALYs_Female_Rate_2023_upper,")")) %>% 
  select(location_name,age_name,cause_name,
         Deaths_num_Both_1990_95UI, Deaths_num_Both_2023_95UI, Deaths_num_Both_change,
         Deaths_rate_Both_1990_95UI, Deaths_rate_Both_2023_95UI, Deaths_rate_Both_change,
         Deaths_num_Male_1990_95UI, Deaths_num_Male_2023_95UI, Deaths_num_Male_change,
         Deaths_rate_Male_1990_95UI, Deaths_rate_Male_2023_95UI, Deaths_rate_Male_change,
         Deaths_num_Female_1990_95UI, Deaths_num_Female_2023_95UI, Deaths_num_Female_change,
         Deaths_rate_Female_1990_95UI, Deaths_rate_Female_2023_95UI, Deaths_rate_Female_change,
         DALYs_num_Both_1990_95UI, DALYs_num_Both_2023_95UI, DALYs_num_Both_change,
         DALYs_rate_Both_1990_95UI, DALYs_rate_Both_2023_95UI, DALYs_rate_Both_change,
         DALYs_num_Male_1990_95UI, DALYs_num_Male_2023_95UI, DALYs_num_Male_change,
         DALYs_rate_Male_1990_95UI, DALYs_rate_Male_2023_95UI, DALYs_rate_Male_change,
         DALYs_num_Female_1990_95UI, DALYs_num_Female_2023_95UI, DALYs_num_Female_change,
         DALYs_rate_Female_1990_95UI, DALYs_rate_Female_2023_95UI, DALYs_rate_Female_change)

data0 <- data0 %>% 
  group_by(cause_name) %>% 
  arrange(cause_name,age_name,location_name) %>% 
  ungroup()

write.csv(data0,file = "~/China GBD/GBD2023/Table/allrisk-level2-agegroup.csv")
############################################################年龄分组 level2 every risk factor####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name%in%c("Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", "Diet low in omega-6 polyunsaturated fatty acids",
                                                                          "Diet low in nuts and seeds", "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids",
                                                                          "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", "Diet low in vegetables", "Diet low in calcium",
                                                                          "Diet high in sugar-sweetened beverages", "Diet high in trans fatty acids")&dietary_annual1.0$cause_name %in% c("Neoplasms",
                                                                                                                                                                                          "Cardiovascular diseases", 
                                                                                                                                                                                          "Diabetes and kidney diseases"))
allrisk_cause <- allrisk_cause %>% 
  select(measure_name,location_name,rei_name,sex_name,age_name,rei_name,cause_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name %in% c("Number","Rate")) %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))

allrisk_cause$age_name <- factor(allrisk_cause$age_name, levels = c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))
allrisk_cause$cause_name <- factor(allrisk_cause$cause_name, levels = c("Cardiovascular diseases",
                                                                        "Neoplasms",
                                                                        "Diabetes and kidney diseases"))
allrisk_cause$rei_name <- factor(allrisk_cause$rei_name, levels = c("Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", "Diet low in omega-6 polyunsaturated fatty acids",
                                                                    "Diet low in nuts and seeds", "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids",
                                                                    "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", "Diet low in vegetables", "Diet low in calcium",
                                                                    "Diet high in sugar-sweetened beverages", "Diet high in trans fatty acids"))


val_wide <- dcast(allrisk_cause, location_name+age_name+rei_name+cause_name ~ 
                    measure_name+sex_name+metric_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause, location_name+age_name+rei_name+cause_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "upper")
colnames(upper_wide)[5:28] <- paste(colnames(upper_wide)[5:28],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause, location_name+age_name+rei_name+cause_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "lower")
colnames(lower_wide)[5:28] <- paste(colnames(lower_wide)[5:28],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","age_name","rei_name","cause_name")),
              lower_wide, by = c("location_name","age_name","rei_name","cause_name"))

data0 <- data0[,c("location_name","age_name","rei_name","cause_name",
                  "Deaths_Both_Number_1990","Deaths_Both_Number_1990_lower","Deaths_Both_Number_1990_upper",
                  "Deaths_Both_Number_2023","Deaths_Both_Number_2023_lower","Deaths_Both_Number_2023_upper",
                  "Deaths_Both_Rate_1990","Deaths_Both_Rate_1990_lower","Deaths_Both_Rate_1990_upper",
                  "Deaths_Both_Rate_2023","Deaths_Both_Rate_2023_lower","Deaths_Both_Rate_2023_upper",
                  
                  "Deaths_Male_Number_1990","Deaths_Male_Number_1990_lower","Deaths_Male_Number_1990_upper",
                  "Deaths_Male_Number_2023","Deaths_Male_Number_2023_lower","Deaths_Male_Number_2023_upper",
                  "Deaths_Male_Rate_1990","Deaths_Male_Rate_1990_lower","Deaths_Male_Rate_1990_upper",
                  "Deaths_Male_Rate_2023","Deaths_Male_Rate_2023_lower","Deaths_Male_Rate_2023_upper",
                  
                  "Deaths_Female_Number_1990","Deaths_Female_Number_1990_lower","Deaths_Female_Number_1990_upper",
                  "Deaths_Female_Number_2023","Deaths_Female_Number_2023_lower","Deaths_Female_Number_2023_upper",
                  "Deaths_Female_Rate_1990","Deaths_Female_Rate_1990_lower","Deaths_Female_Rate_1990_upper",
                  "Deaths_Female_Rate_2023","Deaths_Female_Rate_2023_lower","Deaths_Female_Rate_2023_upper",
                  
                  
                  "DALYs_Both_Number_1990","DALYs_Both_Number_1990_lower","DALYs_Both_Number_1990_upper",
                  "DALYs_Both_Number_2023","DALYs_Both_Number_2023_lower","DALYs_Both_Number_2023_upper",
                  "DALYs_Both_Rate_1990","DALYs_Both_Rate_1990_lower","DALYs_Both_Rate_1990_upper",
                  "DALYs_Both_Rate_2023","DALYs_Both_Rate_2023_lower","DALYs_Both_Rate_2023_upper",
                  
                  "DALYs_Male_Number_1990","DALYs_Male_Number_1990_lower","DALYs_Male_Number_1990_upper",
                  "DALYs_Male_Number_2023","DALYs_Male_Number_2023_lower","DALYs_Male_Number_2023_upper",
                  "DALYs_Male_Rate_1990","DALYs_Male_Rate_1990_lower","DALYs_Male_Rate_1990_upper",
                  "DALYs_Male_Rate_2023","DALYs_Male_Rate_2023_lower","DALYs_Male_Rate_2023_upper",
                  
                  "DALYs_Female_Number_1990","DALYs_Female_Number_1990_lower","DALYs_Female_Number_1990_upper",
                  "DALYs_Female_Number_2023","DALYs_Female_Number_2023_lower","DALYs_Female_Number_2023_upper",
                  "DALYs_Female_Rate_1990","DALYs_Female_Rate_1990_lower","DALYs_Female_Rate_1990_upper",
                  "DALYs_Female_Rate_2023","DALYs_Female_Rate_2023_lower","DALYs_Female_Rate_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data0 <- data0 %>% 
  arrange(location_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:4)] <- lapply(data0[-(1:4)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_num_Both_change = round(((Deaths_Both_Number_2023 - Deaths_Both_Number_1990) / Deaths_Both_Number_1990) * 100, 2),
    Deaths_rate_Both_change = round(((Deaths_Both_Rate_2023 - Deaths_Both_Rate_1990) / Deaths_Both_Rate_1990) * 100, 2),
    Deaths_num_Male_change = round(((Deaths_Male_Number_2023 - Deaths_Male_Number_1990) / Deaths_Male_Number_1990) * 100, 2),
    Deaths_rate_Male_change = round(((Deaths_Male_Rate_2023 - Deaths_Male_Rate_1990) / Deaths_Male_Rate_1990) * 100, 2),
    Deaths_num_Female_change = round(((Deaths_Female_Number_2023 - Deaths_Female_Number_1990) / Deaths_Female_Number_1990) * 100, 2),
    Deaths_rate_Female_change = round(((Deaths_Female_Rate_2023 - Deaths_Female_Rate_1990) / Deaths_Female_Rate_1990) * 100, 2),
    
    DALYs_num_Both_change = round(((DALYs_Both_Number_2023 - DALYs_Both_Number_1990) / DALYs_Both_Number_1990) * 100, 2),
    DALYs_rate_Both_change = round(((DALYs_Both_Rate_2023 - DALYs_Both_Rate_1990) / DALYs_Both_Rate_1990) * 100, 2),
    DALYs_num_Male_change = round(((DALYs_Male_Number_2023 - DALYs_Male_Number_1990) / DALYs_Male_Number_1990) * 100, 2),
    DALYs_rate_Male_change = round(((DALYs_Male_Rate_2023 - DALYs_Male_Rate_1990) / DALYs_Male_Rate_1990) * 100, 2),
    DALYs_num_Female_change = round(((DALYs_Female_Number_2023 - DALYs_Female_Number_1990) / DALYs_Female_Number_1990) * 100, 2),
    DALYs_rate_Female_change = round(((DALYs_Female_Rate_2023 - DALYs_Female_Rate_1990) / DALYs_Female_Rate_1990) * 100, 2)
  ) %>% 
  mutate(Deaths_num_Both_1990_95UI=paste(Deaths_Both_Number_1990,"(",Deaths_Both_Number_1990_lower,",",Deaths_Both_Number_1990_upper,")"),
         Deaths_num_Both_2023_95UI=paste(Deaths_Both_Number_2023,"(",Deaths_Both_Number_2023_lower,",",Deaths_Both_Number_2023_upper,")"),
         Deaths_rate_Both_1990_95UI=paste(Deaths_Both_Rate_1990,"(",Deaths_Both_Rate_1990_lower,",",Deaths_Both_Rate_1990_upper,")"),
         Deaths_rate_Both_2023_95UI=paste(Deaths_Both_Rate_2023,"(",Deaths_Both_Rate_2023_lower,",",Deaths_Both_Rate_2023_upper,")"),
         
         Deaths_num_Male_1990_95UI=paste(Deaths_Male_Number_1990,"(",Deaths_Male_Number_1990_lower,",",Deaths_Male_Number_1990_upper,")"),
         Deaths_num_Male_2023_95UI=paste(Deaths_Male_Number_2023,"(",Deaths_Male_Number_2023_lower,",",Deaths_Male_Number_2023_upper,")"),
         Deaths_rate_Male_1990_95UI=paste(Deaths_Male_Rate_1990,"(",Deaths_Male_Rate_1990_lower,",",Deaths_Male_Rate_1990_upper,")"),
         Deaths_rate_Male_2023_95UI=paste(Deaths_Male_Rate_2023,"(",Deaths_Male_Rate_2023_lower,",",Deaths_Male_Rate_2023_upper,")"),
         
         Deaths_num_Female_1990_95UI=paste(Deaths_Female_Number_1990,"(",Deaths_Female_Number_1990_lower,",",Deaths_Female_Number_1990_upper,")"),
         Deaths_num_Female_2023_95UI=paste(Deaths_Female_Number_2023,"(",Deaths_Female_Number_2023_lower,",",Deaths_Female_Number_2023_upper,")"),
         Deaths_rate_Female_1990_95UI=paste(Deaths_Female_Rate_1990,"(",Deaths_Female_Rate_1990_lower,",",Deaths_Female_Rate_1990_upper,")"),
         Deaths_rate_Female_2023_95UI=paste(Deaths_Female_Rate_2023,"(",Deaths_Female_Rate_2023_lower,",",Deaths_Female_Rate_2023_upper,")"),
         
         DALYs_num_Both_1990_95UI=paste(DALYs_Both_Number_1990,"(",DALYs_Both_Number_1990_lower,",",DALYs_Both_Number_1990_upper,")"),
         DALYs_num_Both_2023_95UI=paste(DALYs_Both_Number_2023,"(",DALYs_Both_Number_2023_lower,",",DALYs_Both_Number_2023_upper,")"),
         DALYs_rate_Both_1990_95UI=paste(DALYs_Both_Rate_1990,"(",DALYs_Both_Rate_1990_lower,",",DALYs_Both_Rate_1990_upper,")"),
         DALYs_rate_Both_2023_95UI=paste(DALYs_Both_Rate_2023,"(",DALYs_Both_Rate_2023_lower,",",DALYs_Both_Rate_2023_upper,")"),
         
         DALYs_num_Male_1990_95UI=paste(DALYs_Male_Number_1990,"(",DALYs_Male_Number_1990_lower,",",DALYs_Male_Number_1990_upper,")"),
         DALYs_num_Male_2023_95UI=paste(DALYs_Male_Number_2023,"(",DALYs_Male_Number_2023_lower,",",DALYs_Male_Number_2023_upper,")"),
         DALYs_rate_Male_1990_95UI=paste(DALYs_Male_Rate_1990,"(",DALYs_Male_Rate_1990_lower,",",DALYs_Male_Rate_1990_upper,")"),
         DALYs_rate_Male_2023_95UI=paste(DALYs_Male_Rate_2023,"(",DALYs_Male_Rate_2023_lower,",",DALYs_Male_Rate_2023_upper,")"),
         
         DALYs_num_Female_1990_95UI=paste(DALYs_Female_Number_1990,"(",DALYs_Female_Number_1990_lower,",",DALYs_Female_Number_1990_upper,")"),
         DALYs_num_Female_2023_95UI=paste(DALYs_Female_Number_2023,"(",DALYs_Female_Number_2023_lower,",",DALYs_Female_Number_2023_upper,")"),
         DALYs_rate_Female_1990_95UI=paste(DALYs_Female_Rate_1990,"(",DALYs_Female_Rate_1990_lower,",",DALYs_Female_Rate_1990_upper,")"),
         DALYs_rate_Female_2023_95UI=paste(DALYs_Female_Rate_2023,"(",DALYs_Female_Rate_2023_lower,",",DALYs_Female_Rate_2023_upper,")")) %>% 
  select(location_name,age_name,rei_name,cause_name,
         Deaths_num_Both_1990_95UI, Deaths_num_Both_2023_95UI, Deaths_num_Both_change,
         Deaths_rate_Both_1990_95UI, Deaths_rate_Both_2023_95UI, Deaths_rate_Both_change,
         Deaths_num_Male_1990_95UI, Deaths_num_Male_2023_95UI, Deaths_num_Male_change,
         Deaths_rate_Male_1990_95UI, Deaths_rate_Male_2023_95UI, Deaths_rate_Male_change,
         Deaths_num_Female_1990_95UI, Deaths_num_Female_2023_95UI, Deaths_num_Female_change,
         Deaths_rate_Female_1990_95UI, Deaths_rate_Female_2023_95UI, Deaths_rate_Female_change,
         DALYs_num_Both_1990_95UI, DALYs_num_Both_2023_95UI, DALYs_num_Both_change,
         DALYs_rate_Both_1990_95UI, DALYs_rate_Both_2023_95UI, DALYs_rate_Both_change,
         DALYs_num_Male_1990_95UI, DALYs_num_Male_2023_95UI, DALYs_num_Male_change,
         DALYs_rate_Male_1990_95UI, DALYs_rate_Male_2023_95UI, DALYs_rate_Male_change,
         DALYs_num_Female_1990_95UI, DALYs_num_Female_2023_95UI, DALYs_num_Female_change,
         DALYs_rate_Female_1990_95UI, DALYs_rate_Female_2023_95UI, DALYs_rate_Female_change)

data0 <- data0 %>% 
  group_by(age_name,rei_name,cause_name) %>% 
  arrange(age_name,rei_name,cause_name,location_name) %>% 
  ungroup()

write.csv(data0,file = "~/China GBD/GBD2023/Table/everyrisk-level2-agegroup.csv")










############################################################年龄分组 level3 全因####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&dietary_annual1.0$cause_name%in% c("Diabetes mellitus",
                                                                                                                         "Chronic kidney disease",
                                                                                                                         "Ischemic heart disease",
                                                                                                                         "Aortic aneurysm",
                                                                                                                         "Lower extremity peripheral arterial disease",
                                                                                                                         "Hypertensive heart disease",
                                                                                                                         "Stroke",
                                                                                                                         "Atrial fibrillation and flutter",
                                                                                                                         "Tracheal, bronchus, and lung cancer",
                                                                                                                         "Breast cancer",
                                                                                                                         "Prostate cancer",
                                                                                                                         "Colon and rectum cancer",
                                                                                                                         "Esophageal cancer",
                                                                                                                         "Stomach cancer"))
allrisk_cause <- allrisk_cause %>% 
  select(measure_name,location_name,sex_name,age_name,cause_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name %in% c("Number","Rate")) %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))

allrisk_cause$age_name <- factor(allrisk_cause$age_name, levels = c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))
allrisk_cause$cause_name <- factor(allrisk_cause$cause_name, levels = c("Ischemic heart disease", "Stroke", "Hypertensive heart disease", "Colon and rectum cancer",
                                                                        "Chronic kidney disease", "Diabetes mellitus", "Stomach cancer", "Tracheal, bronchus, and lung cancer", 
                                                                        "Breast cancer", "Esophageal cancer", "Atrial fibrillation and flutter", "Aortic aneurysm", 
                                                                        "Lower extremity peripheral arterial disease", "Prostate cancer"))


val_wide <- dcast(allrisk_cause, location_name+age_name+cause_name ~ 
                    measure_name+sex_name+metric_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause, location_name+age_name+cause_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "upper")
colnames(upper_wide)[4:27] <- paste(colnames(upper_wide)[4:27],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause, location_name+age_name+cause_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "lower")
colnames(lower_wide)[4:27] <- paste(colnames(lower_wide)[4:27],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","age_name","cause_name")),
              lower_wide, by = c("location_name","age_name","cause_name"))

data0 <- data0[,c("location_name","age_name","cause_name",
                  "Deaths_Both_Number_1990","Deaths_Both_Number_1990_lower","Deaths_Both_Number_1990_upper",
                  "Deaths_Both_Number_2023","Deaths_Both_Number_2023_lower","Deaths_Both_Number_2023_upper",
                  "Deaths_Both_Rate_1990","Deaths_Both_Rate_1990_lower","Deaths_Both_Rate_1990_upper",
                  "Deaths_Both_Rate_2023","Deaths_Both_Rate_2023_lower","Deaths_Both_Rate_2023_upper",
                  
                  "Deaths_Male_Number_1990","Deaths_Male_Number_1990_lower","Deaths_Male_Number_1990_upper",
                  "Deaths_Male_Number_2023","Deaths_Male_Number_2023_lower","Deaths_Male_Number_2023_upper",
                  "Deaths_Male_Rate_1990","Deaths_Male_Rate_1990_lower","Deaths_Male_Rate_1990_upper",
                  "Deaths_Male_Rate_2023","Deaths_Male_Rate_2023_lower","Deaths_Male_Rate_2023_upper",
                  
                  "Deaths_Female_Number_1990","Deaths_Female_Number_1990_lower","Deaths_Female_Number_1990_upper",
                  "Deaths_Female_Number_2023","Deaths_Female_Number_2023_lower","Deaths_Female_Number_2023_upper",
                  "Deaths_Female_Rate_1990","Deaths_Female_Rate_1990_lower","Deaths_Female_Rate_1990_upper",
                  "Deaths_Female_Rate_2023","Deaths_Female_Rate_2023_lower","Deaths_Female_Rate_2023_upper",
                  
                  
                  "DALYs_Both_Number_1990","DALYs_Both_Number_1990_lower","DALYs_Both_Number_1990_upper",
                  "DALYs_Both_Number_2023","DALYs_Both_Number_2023_lower","DALYs_Both_Number_2023_upper",
                  "DALYs_Both_Rate_1990","DALYs_Both_Rate_1990_lower","DALYs_Both_Rate_1990_upper",
                  "DALYs_Both_Rate_2023","DALYs_Both_Rate_2023_lower","DALYs_Both_Rate_2023_upper",
                  
                  "DALYs_Male_Number_1990","DALYs_Male_Number_1990_lower","DALYs_Male_Number_1990_upper",
                  "DALYs_Male_Number_2023","DALYs_Male_Number_2023_lower","DALYs_Male_Number_2023_upper",
                  "DALYs_Male_Rate_1990","DALYs_Male_Rate_1990_lower","DALYs_Male_Rate_1990_upper",
                  "DALYs_Male_Rate_2023","DALYs_Male_Rate_2023_lower","DALYs_Male_Rate_2023_upper",
                  
                  "DALYs_Female_Number_1990","DALYs_Female_Number_1990_lower","DALYs_Female_Number_1990_upper",
                  "DALYs_Female_Number_2023","DALYs_Female_Number_2023_lower","DALYs_Female_Number_2023_upper",
                  "DALYs_Female_Rate_1990","DALYs_Female_Rate_1990_lower","DALYs_Female_Rate_1990_upper",
                  "DALYs_Female_Rate_2023","DALYs_Female_Rate_2023_lower","DALYs_Female_Rate_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data0 <- data0 %>% 
  arrange(location_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:3)] <- lapply(data0[-(1:3)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_num_Both_change = round(((Deaths_Both_Number_2023 - Deaths_Both_Number_1990) / Deaths_Both_Number_1990) * 100, 2),
    Deaths_rate_Both_change = round(((Deaths_Both_Rate_2023 - Deaths_Both_Rate_1990) / Deaths_Both_Rate_1990) * 100, 2),
    Deaths_num_Male_change = round(((Deaths_Male_Number_2023 - Deaths_Male_Number_1990) / Deaths_Male_Number_1990) * 100, 2),
    Deaths_rate_Male_change = round(((Deaths_Male_Rate_2023 - Deaths_Male_Rate_1990) / Deaths_Male_Rate_1990) * 100, 2),
    Deaths_num_Female_change = round(((Deaths_Female_Number_2023 - Deaths_Female_Number_1990) / Deaths_Female_Number_1990) * 100, 2),
    Deaths_rate_Female_change = round(((Deaths_Female_Rate_2023 - Deaths_Female_Rate_1990) / Deaths_Female_Rate_1990) * 100, 2),
    
    DALYs_num_Both_change = round(((DALYs_Both_Number_2023 - DALYs_Both_Number_1990) / DALYs_Both_Number_1990) * 100, 2),
    DALYs_rate_Both_change = round(((DALYs_Both_Rate_2023 - DALYs_Both_Rate_1990) / DALYs_Both_Rate_1990) * 100, 2),
    DALYs_num_Male_change = round(((DALYs_Male_Number_2023 - DALYs_Male_Number_1990) / DALYs_Male_Number_1990) * 100, 2),
    DALYs_rate_Male_change = round(((DALYs_Male_Rate_2023 - DALYs_Male_Rate_1990) / DALYs_Male_Rate_1990) * 100, 2),
    DALYs_num_Female_change = round(((DALYs_Female_Number_2023 - DALYs_Female_Number_1990) / DALYs_Female_Number_1990) * 100, 2),
    DALYs_rate_Female_change = round(((DALYs_Female_Rate_2023 - DALYs_Female_Rate_1990) / DALYs_Female_Rate_1990) * 100, 2)
  ) %>% 
  mutate(Deaths_num_Both_1990_95UI=paste(Deaths_Both_Number_1990,"(",Deaths_Both_Number_1990_lower,",",Deaths_Both_Number_1990_upper,")"),
         Deaths_num_Both_2023_95UI=paste(Deaths_Both_Number_2023,"(",Deaths_Both_Number_2023_lower,",",Deaths_Both_Number_2023_upper,")"),
         Deaths_rate_Both_1990_95UI=paste(Deaths_Both_Rate_1990,"(",Deaths_Both_Rate_1990_lower,",",Deaths_Both_Rate_1990_upper,")"),
         Deaths_rate_Both_2023_95UI=paste(Deaths_Both_Rate_2023,"(",Deaths_Both_Rate_2023_lower,",",Deaths_Both_Rate_2023_upper,")"),
         
         Deaths_num_Male_1990_95UI=paste(Deaths_Male_Number_1990,"(",Deaths_Male_Number_1990_lower,",",Deaths_Male_Number_1990_upper,")"),
         Deaths_num_Male_2023_95UI=paste(Deaths_Male_Number_2023,"(",Deaths_Male_Number_2023_lower,",",Deaths_Male_Number_2023_upper,")"),
         Deaths_rate_Male_1990_95UI=paste(Deaths_Male_Rate_1990,"(",Deaths_Male_Rate_1990_lower,",",Deaths_Male_Rate_1990_upper,")"),
         Deaths_rate_Male_2023_95UI=paste(Deaths_Male_Rate_2023,"(",Deaths_Male_Rate_2023_lower,",",Deaths_Male_Rate_2023_upper,")"),
         
         Deaths_num_Female_1990_95UI=paste(Deaths_Female_Number_1990,"(",Deaths_Female_Number_1990_lower,",",Deaths_Female_Number_1990_upper,")"),
         Deaths_num_Female_2023_95UI=paste(Deaths_Female_Number_2023,"(",Deaths_Female_Number_2023_lower,",",Deaths_Female_Number_2023_upper,")"),
         Deaths_rate_Female_1990_95UI=paste(Deaths_Female_Rate_1990,"(",Deaths_Female_Rate_1990_lower,",",Deaths_Female_Rate_1990_upper,")"),
         Deaths_rate_Female_2023_95UI=paste(Deaths_Female_Rate_2023,"(",Deaths_Female_Rate_2023_lower,",",Deaths_Female_Rate_2023_upper,")"),
         
         DALYs_num_Both_1990_95UI=paste(DALYs_Both_Number_1990,"(",DALYs_Both_Number_1990_lower,",",DALYs_Both_Number_1990_upper,")"),
         DALYs_num_Both_2023_95UI=paste(DALYs_Both_Number_2023,"(",DALYs_Both_Number_2023_lower,",",DALYs_Both_Number_2023_upper,")"),
         DALYs_rate_Both_1990_95UI=paste(DALYs_Both_Rate_1990,"(",DALYs_Both_Rate_1990_lower,",",DALYs_Both_Rate_1990_upper,")"),
         DALYs_rate_Both_2023_95UI=paste(DALYs_Both_Rate_2023,"(",DALYs_Both_Rate_2023_lower,",",DALYs_Both_Rate_2023_upper,")"),
         
         DALYs_num_Male_1990_95UI=paste(DALYs_Male_Number_1990,"(",DALYs_Male_Number_1990_lower,",",DALYs_Male_Number_1990_upper,")"),
         DALYs_num_Male_2023_95UI=paste(DALYs_Male_Number_2023,"(",DALYs_Male_Number_2023_lower,",",DALYs_Male_Number_2023_upper,")"),
         DALYs_rate_Male_1990_95UI=paste(DALYs_Male_Rate_1990,"(",DALYs_Male_Rate_1990_lower,",",DALYs_Male_Rate_1990_upper,")"),
         DALYs_rate_Male_2023_95UI=paste(DALYs_Male_Rate_2023,"(",DALYs_Male_Rate_2023_lower,",",DALYs_Male_Rate_2023_upper,")"),
         
         DALYs_num_Female_1990_95UI=paste(DALYs_Female_Number_1990,"(",DALYs_Female_Number_1990_lower,",",DALYs_Female_Number_1990_upper,")"),
         DALYs_num_Female_2023_95UI=paste(DALYs_Female_Number_2023,"(",DALYs_Female_Number_2023_lower,",",DALYs_Female_Number_2023_upper,")"),
         DALYs_rate_Female_1990_95UI=paste(DALYs_Female_Rate_1990,"(",DALYs_Female_Rate_1990_lower,",",DALYs_Female_Rate_1990_upper,")"),
         DALYs_rate_Female_2023_95UI=paste(DALYs_Female_Rate_2023,"(",DALYs_Female_Rate_2023_lower,",",DALYs_Female_Rate_2023_upper,")")) %>% 
  select(location_name,age_name,cause_name,
         Deaths_num_Both_1990_95UI, Deaths_num_Both_2023_95UI, Deaths_num_Both_change,
         Deaths_rate_Both_1990_95UI, Deaths_rate_Both_2023_95UI, Deaths_rate_Both_change,
         Deaths_num_Male_1990_95UI, Deaths_num_Male_2023_95UI, Deaths_num_Male_change,
         Deaths_rate_Male_1990_95UI, Deaths_rate_Male_2023_95UI, Deaths_rate_Male_change,
         Deaths_num_Female_1990_95UI, Deaths_num_Female_2023_95UI, Deaths_num_Female_change,
         Deaths_rate_Female_1990_95UI, Deaths_rate_Female_2023_95UI, Deaths_rate_Female_change,
         DALYs_num_Both_1990_95UI, DALYs_num_Both_2023_95UI, DALYs_num_Both_change,
         DALYs_rate_Both_1990_95UI, DALYs_rate_Both_2023_95UI, DALYs_rate_Both_change,
         DALYs_num_Male_1990_95UI, DALYs_num_Male_2023_95UI, DALYs_num_Male_change,
         DALYs_rate_Male_1990_95UI, DALYs_rate_Male_2023_95UI, DALYs_rate_Male_change,
         DALYs_num_Female_1990_95UI, DALYs_num_Female_2023_95UI, DALYs_num_Female_change,
         DALYs_rate_Female_1990_95UI, DALYs_rate_Female_2023_95UI, DALYs_rate_Female_change)
data0 <- data0 %>% 
  group_by(cause_name,age_name) %>% 
  arrange(cause_name,age_name,location_name) %>% 
  ungroup()

write.csv(data0,file = "~/China GBD/GBD2023/Table/allrisk-level3-agegroup.csv")


############################################################年龄分组 level3 every risk factor####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name%in%c("Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", "Diet low in omega-6 polyunsaturated fatty acids",
                                                                          "Diet low in nuts and seeds", "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids",
                                                                          "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", "Diet low in vegetables", "Diet low in calcium",
                                                                          "Diet high in sugar-sweetened beverages", "Diet high in trans fatty acids")&dietary_annual1.0$cause_name %in% c("Diabetes mellitus",
                                                                                                                                                                                          "Chronic kidney disease",
                                                                                                                                                                                          "Ischemic heart disease",
                                                                                                                                                                                          "Aortic aneurysm",
                                                                                                                                                                                          "Lower extremity peripheral arterial disease",
                                                                                                                                                                                          "Hypertensive heart disease",
                                                                                                                                                                                          "Stroke",
                                                                                                                                                                                          "Atrial fibrillation and flutter",
                                                                                                                                                                                          "Tracheal, bronchus, and lung cancer",
                                                                                                                                                                                          "Breast cancer",
                                                                                                                                                                                          "Prostate cancer",
                                                                                                                                                                                          "Colon and rectum cancer",
                                                                                                                                                                                          "Esophageal cancer",
                                                                                                                                                                                          "Stomach cancer"))
allrisk_cause <- allrisk_cause %>% 
  select(measure_name,location_name,rei_name,sex_name,age_name,rei_name,cause_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name %in% c("Number","Rate")) %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))

allrisk_cause$age_name <- factor(allrisk_cause$age_name, levels = c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))
allrisk_cause$cause_name <- factor(allrisk_cause$cause_name, levels = c("Ischemic heart disease", "Stroke", "Hypertensive heart disease", "Colon and rectum cancer",
                                                                        "Chronic kidney disease", "Diabetes mellitus", "Stomach cancer", "Tracheal, bronchus, and lung cancer", 
                                                                        "Breast cancer", "Esophageal cancer", "Atrial fibrillation and flutter", "Aortic aneurysm", 
                                                                        "Lower extremity peripheral arterial disease", "Prostate cancer"))
allrisk_cause$rei_name <- factor(allrisk_cause$rei_name, levels = c("Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", "Diet low in omega-6 polyunsaturated fatty acids",
                                                                    "Diet low in nuts and seeds", "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids",
                                                                    "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", "Diet low in vegetables", "Diet low in calcium",
                                                                    "Diet high in sugar-sweetened beverages", "Diet high in trans fatty acids"))


val_wide <- dcast(allrisk_cause, location_name+age_name+rei_name+cause_name ~ 
                    measure_name+sex_name+metric_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause, location_name+age_name+rei_name+cause_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "upper")
colnames(upper_wide)[5:28] <- paste(colnames(upper_wide)[5:28],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause, location_name+age_name+rei_name+cause_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "lower")
colnames(lower_wide)[5:28] <- paste(colnames(lower_wide)[5:28],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","age_name","rei_name","cause_name")),
              lower_wide, by = c("location_name","age_name","rei_name","cause_name"))

data0 <- data0[,c("location_name","age_name","rei_name","cause_name",
                  "Deaths_Both_Number_1990","Deaths_Both_Number_1990_lower","Deaths_Both_Number_1990_upper",
                  "Deaths_Both_Number_2023","Deaths_Both_Number_2023_lower","Deaths_Both_Number_2023_upper",
                  "Deaths_Both_Rate_1990","Deaths_Both_Rate_1990_lower","Deaths_Both_Rate_1990_upper",
                  "Deaths_Both_Rate_2023","Deaths_Both_Rate_2023_lower","Deaths_Both_Rate_2023_upper",
                  
                  "Deaths_Male_Number_1990","Deaths_Male_Number_1990_lower","Deaths_Male_Number_1990_upper",
                  "Deaths_Male_Number_2023","Deaths_Male_Number_2023_lower","Deaths_Male_Number_2023_upper",
                  "Deaths_Male_Rate_1990","Deaths_Male_Rate_1990_lower","Deaths_Male_Rate_1990_upper",
                  "Deaths_Male_Rate_2023","Deaths_Male_Rate_2023_lower","Deaths_Male_Rate_2023_upper",
                  
                  "Deaths_Female_Number_1990","Deaths_Female_Number_1990_lower","Deaths_Female_Number_1990_upper",
                  "Deaths_Female_Number_2023","Deaths_Female_Number_2023_lower","Deaths_Female_Number_2023_upper",
                  "Deaths_Female_Rate_1990","Deaths_Female_Rate_1990_lower","Deaths_Female_Rate_1990_upper",
                  "Deaths_Female_Rate_2023","Deaths_Female_Rate_2023_lower","Deaths_Female_Rate_2023_upper",
                  
                  
                  "DALYs_Both_Number_1990","DALYs_Both_Number_1990_lower","DALYs_Both_Number_1990_upper",
                  "DALYs_Both_Number_2023","DALYs_Both_Number_2023_lower","DALYs_Both_Number_2023_upper",
                  "DALYs_Both_Rate_1990","DALYs_Both_Rate_1990_lower","DALYs_Both_Rate_1990_upper",
                  "DALYs_Both_Rate_2023","DALYs_Both_Rate_2023_lower","DALYs_Both_Rate_2023_upper",
                  
                  "DALYs_Male_Number_1990","DALYs_Male_Number_1990_lower","DALYs_Male_Number_1990_upper",
                  "DALYs_Male_Number_2023","DALYs_Male_Number_2023_lower","DALYs_Male_Number_2023_upper",
                  "DALYs_Male_Rate_1990","DALYs_Male_Rate_1990_lower","DALYs_Male_Rate_1990_upper",
                  "DALYs_Male_Rate_2023","DALYs_Male_Rate_2023_lower","DALYs_Male_Rate_2023_upper",
                  
                  "DALYs_Female_Number_1990","DALYs_Female_Number_1990_lower","DALYs_Female_Number_1990_upper",
                  "DALYs_Female_Number_2023","DALYs_Female_Number_2023_lower","DALYs_Female_Number_2023_upper",
                  "DALYs_Female_Rate_1990","DALYs_Female_Rate_1990_lower","DALYs_Female_Rate_1990_upper",
                  "DALYs_Female_Rate_2023","DALYs_Female_Rate_2023_lower","DALYs_Female_Rate_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data0 <- data0 %>% 
  arrange(location_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:4)] <- lapply(data0[-(1:4)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_num_Both_change = round(((Deaths_Both_Number_2023 - Deaths_Both_Number_1990) / Deaths_Both_Number_1990) * 100, 2),
    Deaths_rate_Both_change = round(((Deaths_Both_Rate_2023 - Deaths_Both_Rate_1990) / Deaths_Both_Rate_1990) * 100, 2),
    Deaths_num_Male_change = round(((Deaths_Male_Number_2023 - Deaths_Male_Number_1990) / Deaths_Male_Number_1990) * 100, 2),
    Deaths_rate_Male_change = round(((Deaths_Male_Rate_2023 - Deaths_Male_Rate_1990) / Deaths_Male_Rate_1990) * 100, 2),
    Deaths_num_Female_change = round(((Deaths_Female_Number_2023 - Deaths_Female_Number_1990) / Deaths_Female_Number_1990) * 100, 2),
    Deaths_rate_Female_change = round(((Deaths_Female_Rate_2023 - Deaths_Female_Rate_1990) / Deaths_Female_Rate_1990) * 100, 2),
    
    DALYs_num_Both_change = round(((DALYs_Both_Number_2023 - DALYs_Both_Number_1990) / DALYs_Both_Number_1990) * 100, 2),
    DALYs_rate_Both_change = round(((DALYs_Both_Rate_2023 - DALYs_Both_Rate_1990) / DALYs_Both_Rate_1990) * 100, 2),
    DALYs_num_Male_change = round(((DALYs_Male_Number_2023 - DALYs_Male_Number_1990) / DALYs_Male_Number_1990) * 100, 2),
    DALYs_rate_Male_change = round(((DALYs_Male_Rate_2023 - DALYs_Male_Rate_1990) / DALYs_Male_Rate_1990) * 100, 2),
    DALYs_num_Female_change = round(((DALYs_Female_Number_2023 - DALYs_Female_Number_1990) / DALYs_Female_Number_1990) * 100, 2),
    DALYs_rate_Female_change = round(((DALYs_Female_Rate_2023 - DALYs_Female_Rate_1990) / DALYs_Female_Rate_1990) * 100, 2)
  ) %>% 
  mutate(Deaths_num_Both_1990_95UI=paste(Deaths_Both_Number_1990,"(",Deaths_Both_Number_1990_lower,",",Deaths_Both_Number_1990_upper,")"),
         Deaths_num_Both_2023_95UI=paste(Deaths_Both_Number_2023,"(",Deaths_Both_Number_2023_lower,",",Deaths_Both_Number_2023_upper,")"),
         Deaths_rate_Both_1990_95UI=paste(Deaths_Both_Rate_1990,"(",Deaths_Both_Rate_1990_lower,",",Deaths_Both_Rate_1990_upper,")"),
         Deaths_rate_Both_2023_95UI=paste(Deaths_Both_Rate_2023,"(",Deaths_Both_Rate_2023_lower,",",Deaths_Both_Rate_2023_upper,")"),
         
         Deaths_num_Male_1990_95UI=paste(Deaths_Male_Number_1990,"(",Deaths_Male_Number_1990_lower,",",Deaths_Male_Number_1990_upper,")"),
         Deaths_num_Male_2023_95UI=paste(Deaths_Male_Number_2023,"(",Deaths_Male_Number_2023_lower,",",Deaths_Male_Number_2023_upper,")"),
         Deaths_rate_Male_1990_95UI=paste(Deaths_Male_Rate_1990,"(",Deaths_Male_Rate_1990_lower,",",Deaths_Male_Rate_1990_upper,")"),
         Deaths_rate_Male_2023_95UI=paste(Deaths_Male_Rate_2023,"(",Deaths_Male_Rate_2023_lower,",",Deaths_Male_Rate_2023_upper,")"),
         
         Deaths_num_Female_1990_95UI=paste(Deaths_Female_Number_1990,"(",Deaths_Female_Number_1990_lower,",",Deaths_Female_Number_1990_upper,")"),
         Deaths_num_Female_2023_95UI=paste(Deaths_Female_Number_2023,"(",Deaths_Female_Number_2023_lower,",",Deaths_Female_Number_2023_upper,")"),
         Deaths_rate_Female_1990_95UI=paste(Deaths_Female_Rate_1990,"(",Deaths_Female_Rate_1990_lower,",",Deaths_Female_Rate_1990_upper,")"),
         Deaths_rate_Female_2023_95UI=paste(Deaths_Female_Rate_2023,"(",Deaths_Female_Rate_2023_lower,",",Deaths_Female_Rate_2023_upper,")"),
         
         DALYs_num_Both_1990_95UI=paste(DALYs_Both_Number_1990,"(",DALYs_Both_Number_1990_lower,",",DALYs_Both_Number_1990_upper,")"),
         DALYs_num_Both_2023_95UI=paste(DALYs_Both_Number_2023,"(",DALYs_Both_Number_2023_lower,",",DALYs_Both_Number_2023_upper,")"),
         DALYs_rate_Both_1990_95UI=paste(DALYs_Both_Rate_1990,"(",DALYs_Both_Rate_1990_lower,",",DALYs_Both_Rate_1990_upper,")"),
         DALYs_rate_Both_2023_95UI=paste(DALYs_Both_Rate_2023,"(",DALYs_Both_Rate_2023_lower,",",DALYs_Both_Rate_2023_upper,")"),
         
         DALYs_num_Male_1990_95UI=paste(DALYs_Male_Number_1990,"(",DALYs_Male_Number_1990_lower,",",DALYs_Male_Number_1990_upper,")"),
         DALYs_num_Male_2023_95UI=paste(DALYs_Male_Number_2023,"(",DALYs_Male_Number_2023_lower,",",DALYs_Male_Number_2023_upper,")"),
         DALYs_rate_Male_1990_95UI=paste(DALYs_Male_Rate_1990,"(",DALYs_Male_Rate_1990_lower,",",DALYs_Male_Rate_1990_upper,")"),
         DALYs_rate_Male_2023_95UI=paste(DALYs_Male_Rate_2023,"(",DALYs_Male_Rate_2023_lower,",",DALYs_Male_Rate_2023_upper,")"),
         
         DALYs_num_Female_1990_95UI=paste(DALYs_Female_Number_1990,"(",DALYs_Female_Number_1990_lower,",",DALYs_Female_Number_1990_upper,")"),
         DALYs_num_Female_2023_95UI=paste(DALYs_Female_Number_2023,"(",DALYs_Female_Number_2023_lower,",",DALYs_Female_Number_2023_upper,")"),
         DALYs_rate_Female_1990_95UI=paste(DALYs_Female_Rate_1990,"(",DALYs_Female_Rate_1990_lower,",",DALYs_Female_Rate_1990_upper,")"),
         DALYs_rate_Female_2023_95UI=paste(DALYs_Female_Rate_2023,"(",DALYs_Female_Rate_2023_lower,",",DALYs_Female_Rate_2023_upper,")")) %>% 
  select(location_name,age_name,rei_name,cause_name,
         Deaths_num_Both_1990_95UI, Deaths_num_Both_2023_95UI, Deaths_num_Both_change,
         Deaths_rate_Both_1990_95UI, Deaths_rate_Both_2023_95UI, Deaths_rate_Both_change,
         Deaths_num_Male_1990_95UI, Deaths_num_Male_2023_95UI, Deaths_num_Male_change,
         Deaths_rate_Male_1990_95UI, Deaths_rate_Male_2023_95UI, Deaths_rate_Male_change,
         Deaths_num_Female_1990_95UI, Deaths_num_Female_2023_95UI, Deaths_num_Female_change,
         Deaths_rate_Female_1990_95UI, Deaths_rate_Female_2023_95UI, Deaths_rate_Female_change,
         DALYs_num_Both_1990_95UI, DALYs_num_Both_2023_95UI, DALYs_num_Both_change,
         DALYs_rate_Both_1990_95UI, DALYs_rate_Both_2023_95UI, DALYs_rate_Both_change,
         DALYs_num_Male_1990_95UI, DALYs_num_Male_2023_95UI, DALYs_num_Male_change,
         DALYs_rate_Male_1990_95UI, DALYs_rate_Male_2023_95UI, DALYs_rate_Male_change,
         DALYs_num_Female_1990_95UI, DALYs_num_Female_2023_95UI, DALYs_num_Female_change,
         DALYs_rate_Female_1990_95UI, DALYs_rate_Female_2023_95UI, DALYs_rate_Female_change)

data0 <- data0 %>% 
  group_by(age_name,rei_name,cause_name) %>% 
  arrange(age_name,rei_name,cause_name,location_name) %>% 
  ungroup()

write.csv(data0,file = "~/China GBD/GBD2023/Table/everyrisk-level3-agegroup.csv")

############################################################ level1 全因 proportion####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&dietary_annual1.0$cause_name=="Non-communicable diseases")
allrisk_cause_1 <- allrisk_cause %>% 
  select(measure_name,location_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name == "Percent") %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name =="All ages") %>% 
  filter(sex_name=="Both") %>% 
  mutate(val=val*100,
         upper=upper*100,
         lower=lower*100)

val_wide <- dcast(allrisk_cause_1, location_name ~ 
                    measure_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause_1, location_name ~ 
                      measure_name+year, value.var = "upper")
colnames(upper_wide)[2:5] <- paste(colnames(upper_wide)[2:5],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause_1, location_name ~ 
                      measure_name+year, value.var = "lower")
colnames(lower_wide)[2:5] <- paste(colnames(lower_wide)[2:5],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name")),
              lower_wide, by = c("location_name"))

data0 <- data0[,c("location_name",
                  "Deaths_2023","Deaths_2023_lower","Deaths_2023_upper",
                  "Deaths_1990","Deaths_1990_lower","Deaths_1990_upper",
                  
                  "DALYs_1990","DALYs_1990_lower","DALYs_1990_upper",
                  "DALYs_2023","DALYs_2023_lower","DALYs_2023_upper"
                  
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data0 <- data0 %>% 
  arrange(location_name)
# Apply rounding to all numeric columns except the first one
# data0[-(1:2)] <- lapply(data0[-(1:2)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_change = round(((Deaths_2023 - Deaths_1990) / Deaths_1990) * 100, 2),
    DALYs_change = round(((DALYs_2023 - DALYs_1990) / DALYs_1990) * 100, 2)
  ) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
  mutate(Deaths_1990_95UI=paste(Deaths_1990,"(",Deaths_1990_lower,",",Deaths_1990_upper,")"),
         Deaths_2023_95UI=paste(Deaths_2023,"(",Deaths_2023_lower,",",Deaths_2023_upper,")"),
         
         DALYs_1990_95UI=paste(DALYs_1990,"(",DALYs_1990_lower,",",DALYs_1990_upper,")"),
         DALYs_2023_95UI=paste(DALYs_2023,"(",DALYs_2023_lower,",",DALYs_2023_upper,")"),
  ) %>% 
  select(location_name,
         Deaths_1990_95UI, Deaths_2023_95UI, Deaths_change,
         DALYs_1990_95UI, DALYs_2023_95UI, DALYs_change
  ) 

write.csv(data0,file = "allrisk-level1-agegroup-proportion.csv")

#####性别
allrisk_cause_2 <- allrisk_cause %>% 
  select(measure_name,location_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name == "Percent") %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name =="All ages") %>% 
  filter(!(sex_name=="Both")) %>% 
  filter(location_name=="China") %>% 
  mutate(val=val*100,
         upper=upper*100,
         lower=lower*100)

val_wide <- dcast(allrisk_cause_2, location_name+sex_name ~ 
                    measure_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause_2, location_name+sex_name ~ 
                      measure_name+year, value.var = "upper")
colnames(upper_wide)[3:6] <- paste(colnames(upper_wide)[3:6],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause_2, location_name+sex_name ~ 
                      measure_name+year, value.var = "lower")
colnames(lower_wide)[3:6] <- paste(colnames(lower_wide)[3:6],"_lower",sep = "")

data1 <-merge(merge(val_wide, upper_wide, by = c("location_name","sex_name")),
              lower_wide, by = c("location_name","sex_name"))

data1 <- data1[,c("location_name","sex_name",
                  "Deaths_2023","Deaths_2023_lower","Deaths_2023_upper",
                  "Deaths_1990","Deaths_1990_lower","Deaths_1990_upper",
                  
                  "DALYs_1990","DALYs_1990_lower","DALYs_1990_upper",
                  "DALYs_2023","DALYs_2023_lower","DALYs_2023_upper"
                  
)]
data1$location_name <- factor(data1$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data1 <- data1 %>% 
  arrange(location_name)
# Apply rounding to all numeric columns except the first one
# data0[-(1:2)] <- lapply(data0[-(1:2)], function(x) if(is.numeric(x)) round(x, 2) else x)

data1 <- data1 %>% 
  mutate(
    Deaths_change = round(((Deaths_2023 - Deaths_1990) / Deaths_1990) * 100, 2),
    DALYs_change = round(((DALYs_2023 - DALYs_1990) / DALYs_1990) * 100, 2)
  ) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
  mutate(Deaths_1990_95UI=paste(Deaths_1990,"(",Deaths_1990_lower,",",Deaths_1990_upper,")"),
         Deaths_2023_95UI=paste(Deaths_2023,"(",Deaths_2023_lower,",",Deaths_2023_upper,")"),
         
         DALYs_1990_95UI=paste(DALYs_1990,"(",DALYs_1990_lower,",",DALYs_1990_upper,")"),
         DALYs_2023_95UI=paste(DALYs_2023,"(",DALYs_2023_lower,",",DALYs_2023_upper,")"),
  ) %>% 
  select(location_name,sex_name,
         Deaths_1990_95UI, Deaths_2023_95UI, Deaths_change,
         DALYs_1990_95UI, DALYs_2023_95UI, DALYs_change
  ) 

data1 <- data1[-1]
names(data1)[1] <- "location_name"
#####年龄组
allrisk_cause_3 <- allrisk_cause %>% 
  select(measure_name,location_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name == "Percent") %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(location_name=="China") %>% 
  filter(age_name %in% c("15-49 years","50-69 years","70+ years")) %>% 
  filter(sex_name=="Both") %>% 
  filter(location_name=="China") %>% 
  mutate(val=val*100,
         upper=upper*100,
         lower=lower*100)

val_wide <- dcast(allrisk_cause_3, location_name+age_name ~ 
                    measure_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause_3, location_name+age_name ~ 
                      measure_name+year, value.var = "upper")
colnames(upper_wide)[3:6] <- paste(colnames(upper_wide)[3:6],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause_3, location_name+age_name ~ 
                      measure_name+year, value.var = "lower")
colnames(lower_wide)[3:6] <- paste(colnames(lower_wide)[3:6],"_lower",sep = "")

data2 <-merge(merge(val_wide, upper_wide, by = c("location_name","age_name")),
              lower_wide, by = c("location_name","age_name"))

data2 <- data2[,c("location_name","age_name",
                  "Deaths_2023","Deaths_2023_lower","Deaths_2023_upper",
                  "Deaths_1990","Deaths_1990_lower","Deaths_1990_upper",
                  
                  "DALYs_1990","DALYs_1990_lower","DALYs_1990_upper",
                  "DALYs_2023","DALYs_2023_lower","DALYs_2023_upper"
                  
)]
data2$location_name <- factor(data2$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data2 <- data2 %>% 
  arrange(location_name)
# Apply rounding to all numeric columns except the first one
# data0[-(1:2)] <- lapply(data0[-(1:2)], function(x) if(is.numeric(x)) round(x, 2) else x)

data2 <- data2 %>% 
  mutate(
    Deaths_change = round(((Deaths_2023 - Deaths_1990) / Deaths_1990) * 100, 2),
    DALYs_change = round(((DALYs_2023 - DALYs_1990) / DALYs_1990) * 100, 2)
  ) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
  mutate(Deaths_1990_95UI=paste(Deaths_1990,"(",Deaths_1990_lower,",",Deaths_1990_upper,")"),
         Deaths_2023_95UI=paste(Deaths_2023,"(",Deaths_2023_lower,",",Deaths_2023_upper,")"),
         
         DALYs_1990_95UI=paste(DALYs_1990,"(",DALYs_1990_lower,",",DALYs_1990_upper,")"),
         DALYs_2023_95UI=paste(DALYs_2023,"(",DALYs_2023_lower,",",DALYs_2023_upper,")"),
  ) %>% 
  select(location_name,age_name,
         Deaths_1990_95UI, Deaths_2023_95UI, Deaths_change,
         DALYs_1990_95UI, DALYs_2023_95UI, DALYs_change
  ) 

data2 <- data2[-1]
names(data2)[1] <- "location_name"
############合并
data_all <- data0[1,] %>% 
  rbind(data1) %>% 
  rbind(data2) %>% 
  rbind(data0[-1,])

write.csv(data_all,file = "~/China GBD/GBD2023/Table/allrisk-level1-proportion.csv")
############################################################年龄分组 level1 every risk factor proportion####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name %in% c("Diet low in fruits", "Diet low in legumes", "Diet low in vegetables", "Diet high in red meat",
                                                                            "Diet low in whole grains", "Diet high in sodium", "Diet low in milk", "Diet high in processed meat",
                                                                            "Diet low in seafood omega-3 fatty acids", "Diet low in omega-6 polyunsaturated fatty acids", "Diet low in fiber",
                                                                            "Diet low in nuts and seeds", "Diet high in sugar-sweetened beverages", "Diet low in calcium", 
                                                                            "Diet high in trans fatty acids") & dietary_annual1.0$cause_name=="Non-communicable diseases")
allrisk_cause <- allrisk_cause %>% 
  select(measure_name,location_name,rei_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name == "Percent") %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("All ages","15-49 years","50-74 years","75-84 years","85+ years")) %>% 
  mutate(val=val*100,
         upper=upper*100,
         lower=lower*100)
allrisk_cause$age_name <- factor(allrisk_cause$age_name, levels = c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))
allrisk_cause$rei_name <- factor(allrisk_cause$rei_name, levels = c("Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", "Diet low in omega-6 polyunsaturated fatty acids",
                                                                    "Diet low in nuts and seeds", "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids",
                                                                    "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", "Diet low in vegetables", "Diet low in calcium",
                                                                    "Diet high in sugar-sweetened beverages", "Diet high in trans fatty acids"))

val_wide <- dcast(allrisk_cause, location_name+rei_name+age_name ~ 
                    measure_name+sex_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause, location_name+rei_name+age_name ~ 
                      measure_name+sex_name+year, value.var = "upper")
colnames(upper_wide)[4:15] <- paste(colnames(upper_wide)[4:15],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause, location_name+rei_name+age_name ~ 
                      measure_name+sex_name+year, value.var = "lower")
colnames(lower_wide)[4:15] <- paste(colnames(lower_wide)[4:15],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","rei_name","age_name")),
              lower_wide, by = c("location_name","rei_name","age_name"))

data0 <- data0[,c("location_name","rei_name","age_name",
                  "Deaths_Both_2023","Deaths_Both_2023_lower","Deaths_Both_2023_upper",
                  "Deaths_Both_1990","Deaths_Both_1990_lower","Deaths_Both_1990_upper",
                  
                  "Deaths_Male_2023","Deaths_Male_2023_lower","Deaths_Male_2023_upper",
                  "Deaths_Male_1990","Deaths_Male_1990_lower","Deaths_Male_1990_upper",
                  
                  "Deaths_Female_1990","Deaths_Female_1990_lower","Deaths_Female_1990_upper",
                  "Deaths_Female_2023","Deaths_Female_2023_lower","Deaths_Female_2023_upper",
                  
                  "DALYs_Both_1990","DALYs_Both_1990_lower","DALYs_Both_1990_upper",
                  "DALYs_Both_2023","DALYs_Both_2023_lower","DALYs_Both_2023_upper",
                  
                  "DALYs_Male_1990","DALYs_Male_1990_lower","DALYs_Male_1990_upper",
                  "DALYs_Male_2023","DALYs_Male_2023_lower","DALYs_Male_2023_upper",
                  
                  "DALYs_Female_1990","DALYs_Female_1990_lower","DALYs_Female_1990_upper",
                  "DALYs_Female_2023","DALYs_Female_2023_lower","DALYs_Female_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data0 <- data0 %>% 
  arrange(location_name,rei_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:3)] <- lapply(data0[-(1:3)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_Both_change = round(((Deaths_Both_2023 - Deaths_Both_1990) / Deaths_Both_1990) * 100, 2),
    Deaths_Male_change = round(((Deaths_Male_2023 - Deaths_Male_1990) / Deaths_Male_1990) * 100, 2),
    Deaths_Female_change = round(((Deaths_Female_2023 - Deaths_Female_1990) / Deaths_Female_1990) * 100, 2),
    
    DALYs_Both_change = round(((DALYs_Both_2023 - DALYs_Both_1990) / DALYs_Both_1990) * 100, 2),
    DALYs_Male_change = round(((DALYs_Male_2023 - DALYs_Male_1990) / DALYs_Male_1990) * 100, 2),
    DALYs_Female_change = round(((DALYs_Female_2023 - DALYs_Female_1990) / DALYs_Female_1990) * 100, 2),
  ) %>% 
  mutate(Deaths_Both_1990_95UI=paste(Deaths_Both_1990,"(",Deaths_Both_1990_lower,",",Deaths_Both_1990_upper,")"),
         Deaths_Both_2023_95UI=paste(Deaths_Both_2023,"(",Deaths_Both_2023_lower,",",Deaths_Both_2023_upper,")"),
         
         Deaths_Male_1990_95UI=paste(Deaths_Male_1990,"(",Deaths_Male_1990_lower,",",Deaths_Male_1990_upper,")"),
         Deaths_Male_2023_95UI=paste(Deaths_Male_2023,"(",Deaths_Male_2023_lower,",",Deaths_Male_2023_upper,")"),
         
         Deaths_Female_1990_95UI=paste(Deaths_Female_1990,"(",Deaths_Female_1990_lower,",",Deaths_Female_1990_upper,")"),
         Deaths_Female_2023_95UI=paste(Deaths_Female_2023,"(",Deaths_Female_2023_lower,",",Deaths_Female_2023_upper,")"),
         
         DALYs_Both_1990_95UI=paste(DALYs_Both_1990,"(",DALYs_Both_1990_lower,",",DALYs_Both_1990_upper,")"),
         DALYs_Both_2023_95UI=paste(DALYs_Both_2023,"(",DALYs_Both_2023_lower,",",DALYs_Both_2023_upper,")"),
         
         DALYs_Male_1990_95UI=paste(DALYs_Male_1990,"(",DALYs_Male_1990_lower,",",DALYs_Male_1990_upper,")"),
         DALYs_Male_2023_95UI=paste(DALYs_Male_2023,"(",DALYs_Male_2023_lower,",",DALYs_Male_2023_upper,")"),
         
         DALYs_Female_1990_95UI=paste(DALYs_Female_1990,"(",DALYs_Female_1990_lower,",",DALYs_Female_1990_upper,")"),
         DALYs_Female_2023_95UI=paste(DALYs_Female_2023,"(",DALYs_Female_2023_lower,",",DALYs_Female_2023_upper,")")) %>% 
  select(location_name,rei_name,age_name,
         Deaths_Both_1990_95UI, Deaths_Both_2023_95UI, Deaths_Both_change,
         Deaths_Male_1990_95UI, Deaths_Male_2023_95UI, Deaths_Male_change,
         Deaths_Female_1990_95UI, Deaths_Female_2023_95UI, Deaths_Female_change,
         DALYs_Both_1990_95UI, DALYs_Both_2023_95UI, DALYs_Both_change,
         DALYs_Male_1990_95UI, DALYs_Male_2023_95UI, DALYs_Male_change,
         DALYs_Female_1990_95UI, DALYs_Female_2023_95UI, DALYs_Female_change)

data0 <- data0 %>% 
  group_by(age_name,rei_name) %>% 
  arrange(age_name,rei_name,location_name) %>% 
  ungroup()

write.csv(data0,file = "~/China GBD/GBD2023/Table/level1_every_riskfac_agegroup_proportion.csv")
############################################################年龄分组 level1 every risk factor proportion 2####
dietary_factors <- c("Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", 
                     "Diet low in omega-6 polyunsaturated fatty acids", "Diet low in nuts and seeds", 
                     "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids", 
                     "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", 
                     "Diet low in vegetables", "Diet low in calcium", "Diet high in sugar-sweetened beverages", 
                     "Diet high in trans fatty acids")
data_list <- list()

for (rei in dietary_factors) {
  allrisk_level1 <- filter(dietary_annual1.0,dietary_annual1.0$rei_name==rei & dietary_annual1.0$cause_name=="Non-communicable diseases")
  
  allrisk_level1_1 <- allrisk_level1 %>% 
    select(measure_name,location_name,rei_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
    filter(metric_name == "Percent") %>% 
    filter(year %in% c(1990,2023)) %>% 
    filter(age_name =="All ages") %>% 
    filter(sex_name=="Both") %>% 
    mutate(val=val*100,
           upper=upper*100,
           lower=lower*100)
  
  val_wide <- dcast(allrisk_level1_1, location_name+rei_name ~ 
                      measure_name+year,value.var = "val")
  
  upper_wide <- dcast(allrisk_level1_1, location_name+rei_name ~ 
                        measure_name+year, value.var = "upper")
  colnames(upper_wide)[3:6] <- paste(colnames(upper_wide)[3:6],"_upper",sep = "")
  
  lower_wide <- dcast(allrisk_level1_1, location_name+rei_name ~ 
                        measure_name+year, value.var = "lower")
  colnames(lower_wide)[3:6] <- paste(colnames(lower_wide)[3:6],"_lower",sep = "")
  
  data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","rei_name")),
                lower_wide, by = c("location_name","rei_name"))
  
  data0 <- data0[,c("location_name","rei_name",
                    "Deaths_2023","Deaths_2023_lower","Deaths_2023_upper",
                    "Deaths_1990","Deaths_1990_lower","Deaths_1990_upper",
                    
                    "DALYs_1990","DALYs_1990_lower","DALYs_1990_upper",
                    "DALYs_2023","DALYs_2023_lower","DALYs_2023_upper"
                    
  )]
  data0$location_name <- factor(data0$location_name, 
                                levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                           "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                           "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                           "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                           "Shaanxi", "Gansu", "Hong Kong", "Macao"))
  data0 <- data0 %>% 
    arrange(location_name)
  # Apply rounding to all numeric columns except the first one
  # data0[-(1:2)] <- lapply(data0[-(1:2)], function(x) if(is.numeric(x)) round(x, 2) else x)
  
  data0 <- data0 %>% 
    mutate(
      Deaths_change = round(((Deaths_2023 - Deaths_1990) / Deaths_1990) * 100, 2),
      DALYs_change = round(((DALYs_2023 - DALYs_1990) / DALYs_1990) * 100, 2)
    ) %>% 
    mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
    mutate(Deaths_1990_95UI=paste(Deaths_1990,"(",Deaths_1990_lower,",",Deaths_1990_upper,")"),
           Deaths_2023_95UI=paste(Deaths_2023,"(",Deaths_2023_lower,",",Deaths_2023_upper,")"),
           
           DALYs_1990_95UI=paste(DALYs_1990,"(",DALYs_1990_lower,",",DALYs_1990_upper,")"),
           DALYs_2023_95UI=paste(DALYs_2023,"(",DALYs_2023_lower,",",DALYs_2023_upper,")"),
    ) %>% 
    select(location_name,rei_name,
           Deaths_1990_95UI, Deaths_2023_95UI, Deaths_change,
           DALYs_1990_95UI, DALYs_2023_95UI, DALYs_change
    ) 
  
  write.csv(data0,file = "~/China GBD/GBD2023/Table/level1_allrisk_agegroup_proportion.csv")
  
  #####性别
  allrisk_level1_2 <- allrisk_level1 %>% 
    select(measure_name,location_name,rei_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
    filter(metric_name == "Percent") %>% 
    filter(year %in% c(1990,2023)) %>% 
    filter(age_name =="All ages") %>% 
    filter(!(sex_name=="Both")) %>% 
    filter(location_name=="China") %>% 
    mutate(val=val*100,
           upper=upper*100,
           lower=lower*100)
  
  val_wide <- dcast(allrisk_level1_2, location_name+sex_name+rei_name ~ 
                      measure_name+year,value.var = "val")
  
  upper_wide <- dcast(allrisk_level1_2, location_name+sex_name+rei_name ~ 
                        measure_name+year, value.var = "upper")
  colnames(upper_wide)[4:7] <- paste(colnames(upper_wide)[4:7],"_upper",sep = "")
  
  lower_wide <- dcast(allrisk_level1_2, location_name+sex_name+rei_name ~ 
                        measure_name+year, value.var = "lower")
  colnames(lower_wide)[4:7] <- paste(colnames(lower_wide)[4:7],"_lower",sep = "")
  
  data1 <-merge(merge(val_wide, upper_wide, by = c("location_name","sex_name","rei_name")),
                lower_wide, by = c("location_name","sex_name","rei_name"))
  
  data1 <- data1[,c("location_name","sex_name","rei_name",
                    "Deaths_2023","Deaths_2023_lower","Deaths_2023_upper",
                    "Deaths_1990","Deaths_1990_lower","Deaths_1990_upper",
                    
                    "DALYs_1990","DALYs_1990_lower","DALYs_1990_upper",
                    "DALYs_2023","DALYs_2023_lower","DALYs_2023_upper"
                    
  )]
  data1$location_name <- factor(data1$location_name, 
                                levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                           "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                           "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                           "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                           "Shaanxi", "Gansu", "Hong Kong", "Macao"))
  data1 <- data1 %>% 
    arrange(location_name)
  # Apply rounding to all numeric columns except the first one
  # data0[-(1:2)] <- lapply(data0[-(1:2)], function(x) if(is.numeric(x)) round(x, 2) else x)
  
  data1 <- data1 %>% 
    mutate(
      Deaths_change = round(((Deaths_2023 - Deaths_1990) / Deaths_1990) * 100, 2),
      DALYs_change = round(((DALYs_2023 - DALYs_1990) / DALYs_1990) * 100, 2)
    ) %>% 
    mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
    mutate(Deaths_1990_95UI=paste(Deaths_1990,"(",Deaths_1990_lower,",",Deaths_1990_upper,")"),
           Deaths_2023_95UI=paste(Deaths_2023,"(",Deaths_2023_lower,",",Deaths_2023_upper,")"),
           
           DALYs_1990_95UI=paste(DALYs_1990,"(",DALYs_1990_lower,",",DALYs_1990_upper,")"),
           DALYs_2023_95UI=paste(DALYs_2023,"(",DALYs_2023_lower,",",DALYs_2023_upper,")"),
    ) %>% 
    select(location_name,sex_name,rei_name,
           Deaths_1990_95UI, Deaths_2023_95UI, Deaths_change,
           DALYs_1990_95UI, DALYs_2023_95UI, DALYs_change
    ) 
  
  data1 <- data1[-1]
  names(data1)[1] <- "location_name"
  #####年龄组
  allrisk_level1_3 <- allrisk_level1 %>% 
    select(measure_name,location_name,rei_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
    filter(metric_name == "Percent") %>% 
    filter(year %in% c(1990,2023)) %>% 
    filter(location_name=="China") %>% 
    filter(age_name %in% c("15-49 years","50-69 years","70+ years")) %>% 
    filter(sex_name=="Both") %>% 
    filter(location_name=="China") %>% 
    mutate(val=val*100,
           upper=upper*100,
           lower=lower*100)
  
  val_wide <- dcast(allrisk_level1_3, location_name+age_name+rei_name ~ 
                      measure_name+year,value.var = "val")
  
  upper_wide <- dcast(allrisk_level1_3, location_name+age_name+rei_name ~ 
                        measure_name+year, value.var = "upper")
  colnames(upper_wide)[4:7] <- paste(colnames(upper_wide)[4:7],"_upper",sep = "")
  
  lower_wide <- dcast(allrisk_level1_3, location_name+age_name+rei_name ~ 
                        measure_name+year, value.var = "lower")
  colnames(lower_wide)[4:7] <- paste(colnames(lower_wide)[4:7],"_lower",sep = "")
  
  data2 <-merge(merge(val_wide, upper_wide, by = c("location_name","age_name","rei_name")),
                lower_wide, by = c("location_name","age_name","rei_name"))
  
  data2 <- data2[,c("location_name","age_name","rei_name",
                    "Deaths_2023","Deaths_2023_lower","Deaths_2023_upper",
                    "Deaths_1990","Deaths_1990_lower","Deaths_1990_upper",
                    
                    "DALYs_1990","DALYs_1990_lower","DALYs_1990_upper",
                    "DALYs_2023","DALYs_2023_lower","DALYs_2023_upper"
                    
  )]
  data2$location_name <- factor(data2$location_name, 
                                levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                           "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                           "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                           "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                           "Shaanxi", "Gansu", "Hong Kong", "Macao"))
  data2 <- data2 %>% 
    arrange(location_name)
  # Apply rounding to all numeric columns except the first one
  # data0[-(1:2)] <- lapply(data0[-(1:2)], function(x) if(is.numeric(x)) round(x, 2) else x)
  
  data2 <- data2 %>% 
    mutate(
      Deaths_change = round(((Deaths_2023 - Deaths_1990) / Deaths_1990) * 100, 2),
      DALYs_change = round(((DALYs_2023 - DALYs_1990) / DALYs_1990) * 100, 2)
    ) %>% 
    mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
    mutate(Deaths_1990_95UI=paste(Deaths_1990,"(",Deaths_1990_lower,",",Deaths_1990_upper,")"),
           Deaths_2023_95UI=paste(Deaths_2023,"(",Deaths_2023_lower,",",Deaths_2023_upper,")"),
           
           DALYs_1990_95UI=paste(DALYs_1990,"(",DALYs_1990_lower,",",DALYs_1990_upper,")"),
           DALYs_2023_95UI=paste(DALYs_2023,"(",DALYs_2023_lower,",",DALYs_2023_upper,")"),
    ) %>% 
    select(location_name,age_name,rei_name,
           Deaths_1990_95UI, Deaths_2023_95UI, Deaths_change,
           DALYs_1990_95UI, DALYs_2023_95UI, DALYs_change
    ) 
  
  data2 <- data2[-1]
  names(data2)[1] <- "location_name"
  ############合并
  data_all <- data0[1,] %>% 
    rbind(data1) %>% 
    rbind(data2) %>% 
    rbind(data0[-1,])
  
  data_list[[rei]] <- data_all
}

data_combined <- bind_rows(data_list)
write.csv(data_combined,file = "~/China GBD/GBD2023/Table/everyrisk-level1_proportion.csv")
############################################################年龄分组 level2 全因 proportion####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&dietary_annual1.0$cause_name %in% c("Neoplasms",
                                                                                                                          "Cardiovascular diseases", 
                                                                                                                          "Diabetes and kidney diseases"))
allrisk_cause <- allrisk_cause %>% 
  select(measure_name,location_name,cause_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name == "Percent") %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("All ages","15-49 years","50-74 years","75-84 years","85+ years")) %>% 
  mutate(val=val*100,
         upper=upper*100,
         lower=lower*100)

allrisk_cause$age_name <- factor(allrisk_cause$age_name, levels = c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))
allrisk_cause$cause_name <- factor(allrisk_cause$cause_name, levels = c("Cardiovascular diseases",
                                                                        "Neoplasms",
                                                                        "Diabetes and kidney diseases"))


val_wide <- dcast(allrisk_cause, location_name+cause_name+age_name ~ 
                    measure_name+sex_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause, location_name+cause_name+age_name ~ 
                      measure_name+sex_name+year, value.var = "upper")
colnames(upper_wide)[4:15] <- paste(colnames(upper_wide)[4:15],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause, location_name+cause_name+age_name ~ 
                      measure_name+sex_name+year, value.var = "lower")
colnames(lower_wide)[4:15] <- paste(colnames(lower_wide)[4:15],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","cause_name","age_name")),
              lower_wide, by = c("location_name","cause_name","age_name"))

data0 <- data0[,c("location_name","cause_name","age_name",
                  "Deaths_Both_2023","Deaths_Both_2023_lower","Deaths_Both_2023_upper",
                  "Deaths_Both_1990","Deaths_Both_1990_lower","Deaths_Both_1990_upper",
                  
                  "Deaths_Male_2023","Deaths_Male_2023_lower","Deaths_Male_2023_upper",
                  "Deaths_Male_1990","Deaths_Male_1990_lower","Deaths_Male_1990_upper",
                  
                  "Deaths_Female_1990","Deaths_Female_1990_lower","Deaths_Female_1990_upper",
                  "Deaths_Female_2023","Deaths_Female_2023_lower","Deaths_Female_2023_upper",
                  
                  "DALYs_Both_1990","DALYs_Both_1990_lower","DALYs_Both_1990_upper",
                  "DALYs_Both_2023","DALYs_Both_2023_lower","DALYs_Both_2023_upper",
                  
                  "DALYs_Male_1990","DALYs_Male_1990_lower","DALYs_Male_1990_upper",
                  "DALYs_Male_2023","DALYs_Male_2023_lower","DALYs_Male_2023_upper",
                  
                  "DALYs_Female_1990","DALYs_Female_1990_lower","DALYs_Female_1990_upper",
                  "DALYs_Female_2023","DALYs_Female_2023_lower","DALYs_Female_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data0 <- data0 %>% 
  arrange(location_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:3)] <- lapply(data0[-(1:3)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_Both_change = round(((Deaths_Both_2023 - Deaths_Both_1990) / Deaths_Both_1990) * 100, 2),
    Deaths_Male_change = round(((Deaths_Male_2023 - Deaths_Male_1990) / Deaths_Male_1990) * 100, 2),
    Deaths_Female_change = round(((Deaths_Female_2023 - Deaths_Female_1990) / Deaths_Female_1990) * 100, 2),
    
    DALYs_Both_change = round(((DALYs_Both_2023 - DALYs_Both_1990) / DALYs_Both_1990) * 100, 2),
    DALYs_Male_change = round(((DALYs_Male_2023 - DALYs_Male_1990) / DALYs_Male_1990) * 100, 2),
    DALYs_Female_change = round(((DALYs_Female_2023 - DALYs_Female_1990) / DALYs_Female_1990) * 100, 2),
  ) %>% 
  mutate(Deaths_Both_1990_95UI=paste(Deaths_Both_1990,"(",Deaths_Both_1990_lower,",",Deaths_Both_1990_upper,")"),
         Deaths_Both_2023_95UI=paste(Deaths_Both_2023,"(",Deaths_Both_2023_lower,",",Deaths_Both_2023_upper,")"),
         
         Deaths_Male_1990_95UI=paste(Deaths_Male_1990,"(",Deaths_Male_1990_lower,",",Deaths_Male_1990_upper,")"),
         Deaths_Male_2023_95UI=paste(Deaths_Male_2023,"(",Deaths_Male_2023_lower,",",Deaths_Male_2023_upper,")"),
         
         Deaths_Female_1990_95UI=paste(Deaths_Female_1990,"(",Deaths_Female_1990_lower,",",Deaths_Female_1990_upper,")"),
         Deaths_Female_2023_95UI=paste(Deaths_Female_2023,"(",Deaths_Female_2023_lower,",",Deaths_Female_2023_upper,")"),
         
         DALYs_Both_1990_95UI=paste(DALYs_Both_1990,"(",DALYs_Both_1990_lower,",",DALYs_Both_1990_upper,")"),
         DALYs_Both_2023_95UI=paste(DALYs_Both_2023,"(",DALYs_Both_2023_lower,",",DALYs_Both_2023_upper,")"),
         
         DALYs_Male_1990_95UI=paste(DALYs_Male_1990,"(",DALYs_Male_1990_lower,",",DALYs_Male_1990_upper,")"),
         DALYs_Male_2023_95UI=paste(DALYs_Male_2023,"(",DALYs_Male_2023_lower,",",DALYs_Male_2023_upper,")"),
         
         DALYs_Female_1990_95UI=paste(DALYs_Female_1990,"(",DALYs_Female_1990_lower,",",DALYs_Female_1990_upper,")"),
         DALYs_Female_2023_95UI=paste(DALYs_Female_2023,"(",DALYs_Female_2023_lower,",",DALYs_Female_2023_upper,")")) %>% 
  select(location_name,cause_name,age_name,
         Deaths_Both_1990_95UI, Deaths_Both_2023_95UI, Deaths_Both_change,
         Deaths_Male_1990_95UI, Deaths_Male_2023_95UI, Deaths_Male_change,
         Deaths_Female_1990_95UI, Deaths_Female_2023_95UI, Deaths_Female_change,
         DALYs_Both_1990_95UI, DALYs_Both_2023_95UI, DALYs_Both_change,
         DALYs_Male_1990_95UI, DALYs_Male_2023_95UI, DALYs_Male_change,
         DALYs_Female_1990_95UI, DALYs_Female_2023_95UI, DALYs_Female_change)

data0 <- data0 %>% 
  group_by(cause_name,age_name) %>% 
  arrange(cause_name,age_name,location_name) %>% 
  ungroup()

write.csv(data0,file = "~/China GBD/GBD2023/Table/level2_allrisk_agegroup_proportion.csv")
############################################################年龄分组 level2 every risk factor proportion####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name %in% c("Diet low in fruits", "Diet low in legumes", "Diet low in vegetables", "Diet high in red meat",
                                                                            "Diet low in whole grains", "Diet high in sodium", "Diet low in milk", "Diet high in processed meat",
                                                                            "Diet low in seafood omega-3 fatty acids", "Diet low in polyunsaturated fatty acids", "Diet low in fiber",
                                                                            "Diet low in nuts and seeds", "Diet high in sugar-sweetened beverages", "Diet low in calcium", 
                                                                            "Diet high in trans fatty acids") & dietary_annual1.0$cause_name %in% c("Neoplasms",
                                                                                                                                                    "Cardiovascular diseases", 
                                                                                                                                                    "Diabetes and kidney diseases"))
allrisk_cause <- allrisk_cause %>% 
  select(measure_name,location_name, rei_name, cause_name, sex_name, age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name == "Percent") %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("All ages","15-49 years","50-74 years","75-84 years","85+ years")) %>% 
  mutate(val=val*100,
         upper=upper*100,
         lower=lower*100)

allrisk_cause$age_name <- factor(allrisk_cause$age_name, levels = c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))
allrisk_cause$cause_name <- factor(allrisk_cause$cause_name, levels = c("Cardiovascular diseases",
                                                                        "Neoplasms",
                                                                        "Diabetes and kidney diseases"))
allrisk_cause$rei_name <- factor(allrisk_cause$rei_name, levels = c("Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", "Diet low in polyunsaturated fatty acids",
                                                                    "Diet low in nuts and seeds", "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids",
                                                                    "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", "Diet low in vegetables", "Diet low in calcium",
                                                                    "Diet high in sugar-sweetened beverages", "Diet high in trans fatty acids"))

val_wide <- dcast(allrisk_cause, location_name+rei_name+cause_name+age_name ~ 
                    measure_name+sex_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause, location_name+rei_name+cause_name+age_name ~ 
                      measure_name+sex_name+year, value.var = "upper")
colnames(upper_wide)[5:16] <- paste(colnames(upper_wide)[5:16],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause, location_name+rei_name+cause_name+age_name ~ 
                      measure_name+sex_name+year, value.var = "lower")
colnames(lower_wide)[5:16] <- paste(colnames(lower_wide)[5:16],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","rei_name","cause_name","age_name")),
              lower_wide, by = c("location_name","rei_name","cause_name","age_name"))

data0 <- data0[,c("location_name","rei_name","cause_name","age_name",
                  "Deaths_Both_2023","Deaths_Both_2023_lower","Deaths_Both_2023_upper",
                  "Deaths_Both_1990","Deaths_Both_1990_lower","Deaths_Both_1990_upper",
                  
                  "Deaths_Male_2023","Deaths_Male_2023_lower","Deaths_Male_2023_upper",
                  "Deaths_Male_1990","Deaths_Male_1990_lower","Deaths_Male_1990_upper",
                  
                  "Deaths_Female_1990","Deaths_Female_1990_lower","Deaths_Female_1990_upper",
                  "Deaths_Female_2023","Deaths_Female_2023_lower","Deaths_Female_2023_upper",
                  
                  "DALYs_Both_1990","DALYs_Both_1990_lower","DALYs_Both_1990_upper",
                  "DALYs_Both_2023","DALYs_Both_2023_lower","DALYs_Both_2023_upper",
                  
                  "DALYs_Male_1990","DALYs_Male_1990_lower","DALYs_Male_1990_upper",
                  "DALYs_Male_2023","DALYs_Male_2023_lower","DALYs_Male_2023_upper",
                  
                  "DALYs_Female_1990","DALYs_Female_1990_lower","DALYs_Female_1990_upper",
                  "DALYs_Female_2023","DALYs_Female_2023_lower","DALYs_Female_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data0 <- data0 %>% 
  arrange(location_name,rei_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:4)] <- lapply(data0[-(1:4)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_Both_change = round(((Deaths_Both_2023 - Deaths_Both_1990) / Deaths_Both_1990) * 100, 2),
    Deaths_Male_change = round(((Deaths_Male_2023 - Deaths_Male_1990) / Deaths_Male_1990) * 100, 2),
    Deaths_Female_change = round(((Deaths_Female_2023 - Deaths_Female_1990) / Deaths_Female_1990) * 100, 2),
    
    DALYs_Both_change = round(((DALYs_Both_2023 - DALYs_Both_1990) / DALYs_Both_1990) * 100, 2),
    DALYs_Male_change = round(((DALYs_Male_2023 - DALYs_Male_1990) / DALYs_Male_1990) * 100, 2),
    DALYs_Female_change = round(((DALYs_Female_2023 - DALYs_Female_1990) / DALYs_Female_1990) * 100, 2),
  ) %>% 
  mutate(Deaths_Both_1990_95UI=paste(Deaths_Both_1990,"(",Deaths_Both_1990_lower,",",Deaths_Both_1990_upper,")"),
         Deaths_Both_2023_95UI=paste(Deaths_Both_2023,"(",Deaths_Both_2023_lower,",",Deaths_Both_2023_upper,")"),
         
         Deaths_Male_1990_95UI=paste(Deaths_Male_1990,"(",Deaths_Male_1990_lower,",",Deaths_Male_1990_upper,")"),
         Deaths_Male_2023_95UI=paste(Deaths_Male_2023,"(",Deaths_Male_2023_lower,",",Deaths_Male_2023_upper,")"),
         
         Deaths_Female_1990_95UI=paste(Deaths_Female_1990,"(",Deaths_Female_1990_lower,",",Deaths_Female_1990_upper,")"),
         Deaths_Female_2023_95UI=paste(Deaths_Female_2023,"(",Deaths_Female_2023_lower,",",Deaths_Female_2023_upper,")"),
         
         DALYs_Both_1990_95UI=paste(DALYs_Both_1990,"(",DALYs_Both_1990_lower,",",DALYs_Both_1990_upper,")"),
         DALYs_Both_2023_95UI=paste(DALYs_Both_2023,"(",DALYs_Both_2023_lower,",",DALYs_Both_2023_upper,")"),
         
         DALYs_Male_1990_95UI=paste(DALYs_Male_1990,"(",DALYs_Male_1990_lower,",",DALYs_Male_1990_upper,")"),
         DALYs_Male_2023_95UI=paste(DALYs_Male_2023,"(",DALYs_Male_2023_lower,",",DALYs_Male_2023_upper,")"),
         
         DALYs_Female_1990_95UI=paste(DALYs_Female_1990,"(",DALYs_Female_1990_lower,",",DALYs_Female_1990_upper,")"),
         DALYs_Female_2023_95UI=paste(DALYs_Female_2023,"(",DALYs_Female_2023_lower,",",DALYs_Female_2023_upper,")")) %>% 
  select(location_name,rei_name,cause_name,age_name,
         Deaths_Both_1990_95UI, Deaths_Both_2023_95UI, Deaths_Both_change,
         Deaths_Male_1990_95UI, Deaths_Male_2023_95UI, Deaths_Male_change,
         Deaths_Female_1990_95UI, Deaths_Female_2023_95UI, Deaths_Female_change,
         DALYs_Both_1990_95UI, DALYs_Both_2023_95UI, DALYs_Both_change,
         DALYs_Male_1990_95UI, DALYs_Male_2023_95UI, DALYs_Male_change,
         DALYs_Female_1990_95UI, DALYs_Female_2023_95UI, DALYs_Female_change)

data0 <- data0 %>% 
  group_by(age_name,rei_name,cause_name) %>% 
  arrange(age_name,rei_name,cause_name,location_name) %>% 
  ungroup()

write.csv(data0,file = "~/China GBD/GBD2023/Table/level2_every_riskfac_agegroup_proportion.csv")






############################################################年龄分组 level3 全因 proportion####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&dietary_annual1.0$cause_name %in% c("Diabetes mellitus",
                                                                                                                          "Chronic kidney disease",
                                                                                                                          "Ischemic heart disease",
                                                                                                                          "Aortic aneurysm",
                                                                                                                          "Lower extremity peripheral arterial disease",
                                                                                                                          "Hypertensive heart disease",
                                                                                                                          "Stroke",
                                                                                                                          "Atrial fibrillation and flutter",
                                                                                                                          "Tracheal, bronchus, and lung cancer",
                                                                                                                          "Breast cancer",
                                                                                                                          "Prostate cancer",
                                                                                                                          "Colon and rectum cancer",
                                                                                                                          "Esophageal cancer",
                                                                                                                          "Stomach cancer"))
allrisk_cause <- allrisk_cause %>% 
  select(measure_name,location_name,cause_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name == "Percent") %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("All ages","15-49 years","50-74 years","75-84 years","85+ years")) %>% 
  mutate(val=val*100,
         upper=upper*100,
         lower=lower*100)

allrisk_cause$age_name <- factor(allrisk_cause$age_name, levels = c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))
allrisk_cause$cause_name <- factor(allrisk_cause$cause_name, levels = c("Ischemic heart disease", "Stroke", "Hypertensive heart disease", "Colon and rectum cancer",
                                                                        "Chronic kidney disease", "Diabetes mellitus", "Stomach cancer", "Tracheal, bronchus, and lung cancer", 
                                                                        "Breast cancer", "Esophageal cancer", "Atrial fibrillation and flutter", "Aortic aneurysm", 
                                                                        "Lower extremity peripheral arterial disease", "Prostate cancer"))

val_wide <- dcast(allrisk_cause, location_name+cause_name+age_name ~ 
                    measure_name+sex_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause, location_name+cause_name+age_name ~ 
                      measure_name+sex_name+year, value.var = "upper")
colnames(upper_wide)[4:15] <- paste(colnames(upper_wide)[4:15],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause, location_name+cause_name+age_name ~ 
                      measure_name+sex_name+year, value.var = "lower")
colnames(lower_wide)[4:15] <- paste(colnames(lower_wide)[4:15],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","cause_name","age_name")),
              lower_wide, by = c("location_name","cause_name","age_name"))

data0 <- data0[,c("location_name","cause_name","age_name",
                  "Deaths_Both_2023","Deaths_Both_2023_lower","Deaths_Both_2023_upper",
                  "Deaths_Both_1990","Deaths_Both_1990_lower","Deaths_Both_1990_upper",
                  
                  "Deaths_Male_2023","Deaths_Male_2023_lower","Deaths_Male_2023_upper",
                  "Deaths_Male_1990","Deaths_Male_1990_lower","Deaths_Male_1990_upper",
                  
                  "Deaths_Female_1990","Deaths_Female_1990_lower","Deaths_Female_1990_upper",
                  "Deaths_Female_2023","Deaths_Female_2023_lower","Deaths_Female_2023_upper",
                  
                  "DALYs_Both_1990","DALYs_Both_1990_lower","DALYs_Both_1990_upper",
                  "DALYs_Both_2023","DALYs_Both_2023_lower","DALYs_Both_2023_upper",
                  
                  "DALYs_Male_1990","DALYs_Male_1990_lower","DALYs_Male_1990_upper",
                  "DALYs_Male_2023","DALYs_Male_2023_lower","DALYs_Male_2023_upper",
                  
                  "DALYs_Female_1990","DALYs_Female_1990_lower","DALYs_Female_1990_upper",
                  "DALYs_Female_2023","DALYs_Female_2023_lower","DALYs_Female_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data0 <- data0 %>% 
  arrange(location_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:3)] <- lapply(data0[-(1:3)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_Both_change = round(((Deaths_Both_2023 - Deaths_Both_1990) / Deaths_Both_1990) * 100, 2),
    Deaths_Male_change = round(((Deaths_Male_2023 - Deaths_Male_1990) / Deaths_Male_1990) * 100, 2),
    Deaths_Female_change = round(((Deaths_Female_2023 - Deaths_Female_1990) / Deaths_Female_1990) * 100, 2),
    
    DALYs_Both_change = round(((DALYs_Both_2023 - DALYs_Both_1990) / DALYs_Both_1990) * 100, 2),
    DALYs_Male_change = round(((DALYs_Male_2023 - DALYs_Male_1990) / DALYs_Male_1990) * 100, 2),
    DALYs_Female_change = round(((DALYs_Female_2023 - DALYs_Female_1990) / DALYs_Female_1990) * 100, 2),
  ) %>% 
  mutate(Deaths_Both_1990_95UI=paste(Deaths_Both_1990,"(",Deaths_Both_1990_lower,",",Deaths_Both_1990_upper,")"),
         Deaths_Both_2023_95UI=paste(Deaths_Both_2023,"(",Deaths_Both_2023_lower,",",Deaths_Both_2023_upper,")"),
         
         Deaths_Male_1990_95UI=paste(Deaths_Male_1990,"(",Deaths_Male_1990_lower,",",Deaths_Male_1990_upper,")"),
         Deaths_Male_2023_95UI=paste(Deaths_Male_2023,"(",Deaths_Male_2023_lower,",",Deaths_Male_2023_upper,")"),
         
         Deaths_Female_1990_95UI=paste(Deaths_Female_1990,"(",Deaths_Female_1990_lower,",",Deaths_Female_1990_upper,")"),
         Deaths_Female_2023_95UI=paste(Deaths_Female_2023,"(",Deaths_Female_2023_lower,",",Deaths_Female_2023_upper,")"),
         
         DALYs_Both_1990_95UI=paste(DALYs_Both_1990,"(",DALYs_Both_1990_lower,",",DALYs_Both_1990_upper,")"),
         DALYs_Both_2023_95UI=paste(DALYs_Both_2023,"(",DALYs_Both_2023_lower,",",DALYs_Both_2023_upper,")"),
         
         DALYs_Male_1990_95UI=paste(DALYs_Male_1990,"(",DALYs_Male_1990_lower,",",DALYs_Male_1990_upper,")"),
         DALYs_Male_2023_95UI=paste(DALYs_Male_2023,"(",DALYs_Male_2023_lower,",",DALYs_Male_2023_upper,")"),
         
         DALYs_Female_1990_95UI=paste(DALYs_Female_1990,"(",DALYs_Female_1990_lower,",",DALYs_Female_1990_upper,")"),
         DALYs_Female_2023_95UI=paste(DALYs_Female_2023,"(",DALYs_Female_2023_lower,",",DALYs_Female_2023_upper,")")) %>% 
  select(location_name,cause_name,age_name,
         Deaths_Both_1990_95UI, Deaths_Both_2023_95UI, Deaths_Both_change,
         Deaths_Male_1990_95UI, Deaths_Male_2023_95UI, Deaths_Male_change,
         Deaths_Female_1990_95UI, Deaths_Female_2023_95UI, Deaths_Female_change,
         DALYs_Both_1990_95UI, DALYs_Both_2023_95UI, DALYs_Both_change,
         DALYs_Male_1990_95UI, DALYs_Male_2023_95UI, DALYs_Male_change,
         DALYs_Female_1990_95UI, DALYs_Female_2023_95UI, DALYs_Female_change)

data0 <- data0 %>% 
  group_by(age_name,cause_name) %>% 
  arrange(age_name,cause_name,location_name) %>% 
  ungroup()

write.csv(data0,file = "~/China GBD/GBD2023/Table/level3_allrisk_agegroup_proportion.csv")
############################################################年龄分组 level3 every risk factor proportion####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name %in% c("Diet low in fruits", "Diet low in legumes", "Diet low in vegetables", "Diet high in red meat",
                                                                            "Diet low in whole grains", "Diet high in sodium", "Diet low in milk", "Diet high in processed meat",
                                                                            "Diet low in seafood omega-3 fatty acids", "Diet low in polyunsaturated fatty acids", "Diet low in fiber",
                                                                            "Diet low in nuts and seeds", "Diet high in sugar-sweetened beverages", "Diet low in calcium", 
                                                                            "Diet high in trans fatty acids") & dietary_annual1.0$cause_name %in% c("Diabetes mellitus",
                                                                                                                                                    "Chronic kidney disease",
                                                                                                                                                    "Ischemic heart disease",
                                                                                                                                                    "Aortic aneurysm",
                                                                                                                                                    "Lower extremity peripheral arterial disease",
                                                                                                                                                    "Hypertensive heart disease",
                                                                                                                                                    "Stroke",
                                                                                                                                                    "Atrial fibrillation and flutter",
                                                                                                                                                    "Tracheal, bronchus, and lung cancer",
                                                                                                                                                    "Breast cancer",
                                                                                                                                                    "Prostate cancer",
                                                                                                                                                    "Colon and rectum cancer",
                                                                                                                                                    "Esophageal cancer",
                                                                                                                                                    "Stomach cancer"))
allrisk_cause <- allrisk_cause %>% 
  select(measure_name,location_name, rei_name, cause_name, sex_name, age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name == "Percent") %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in% c("All ages","15-49 years","50-74 years","75-84 years","85+ years")) %>% 
  mutate(val=val*100,
         upper=upper*100,
         lower=lower*100)

allrisk_cause$age_name <- factor(allrisk_cause$age_name, levels = c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))
allrisk_cause$cause_name <- factor(allrisk_cause$cause_name, levels = c("Ischemic heart disease", "Stroke", "Hypertensive heart disease", "Colon and rectum cancer",
                                                                        "Chronic kidney disease", "Diabetes mellitus", "Stomach cancer", "Tracheal, bronchus, and lung cancer", 
                                                                        "Breast cancer", "Esophageal cancer", "Atrial fibrillation and flutter", "Aortic aneurysm", 
                                                                        "Lower extremity peripheral arterial disease", "Prostate cancer"))
allrisk_cause$rei_name <- factor(allrisk_cause$rei_name, levels = c("Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", "Diet low in polyunsaturated fatty acids",
                                                                    "Diet low in nuts and seeds", "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids",
                                                                    "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", "Diet low in vegetables", "Diet low in calcium",
                                                                    "Diet high in sugar-sweetened beverages", "Diet high in trans fatty acids"))
val_wide <- dcast(allrisk_cause, location_name+rei_name+cause_name+age_name ~ 
                    measure_name+sex_name+year,value.var = "val")

upper_wide <- dcast(allrisk_cause, location_name+rei_name+cause_name+age_name ~ 
                      measure_name+sex_name+year, value.var = "upper")
colnames(upper_wide)[5:16] <- paste(colnames(upper_wide)[5:16],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause, location_name+rei_name+cause_name+age_name ~ 
                      measure_name+sex_name+year, value.var = "lower")
colnames(lower_wide)[5:16] <- paste(colnames(lower_wide)[5:16],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","rei_name","cause_name","age_name")),
              lower_wide, by = c("location_name","rei_name","cause_name","age_name"))

data0 <- data0[,c("location_name","rei_name","cause_name","age_name",
                  "Deaths_Both_2023","Deaths_Both_2023_lower","Deaths_Both_2023_upper",
                  "Deaths_Both_1990","Deaths_Both_1990_lower","Deaths_Both_1990_upper",
                  
                  "Deaths_Male_2023","Deaths_Male_2023_lower","Deaths_Male_2023_upper",
                  "Deaths_Male_1990","Deaths_Male_1990_lower","Deaths_Male_1990_upper",
                  
                  "Deaths_Female_1990","Deaths_Female_1990_lower","Deaths_Female_1990_upper",
                  "Deaths_Female_2023","Deaths_Female_2023_lower","Deaths_Female_2023_upper",
                  
                  "DALYs_Both_1990","DALYs_Both_1990_lower","DALYs_Both_1990_upper",
                  "DALYs_Both_2023","DALYs_Both_2023_lower","DALYs_Both_2023_upper",
                  
                  "DALYs_Male_1990","DALYs_Male_1990_lower","DALYs_Male_1990_upper",
                  "DALYs_Male_2023","DALYs_Male_2023_lower","DALYs_Male_2023_upper",
                  
                  "DALYs_Female_1990","DALYs_Female_1990_lower","DALYs_Female_1990_upper",
                  "DALYs_Female_2023","DALYs_Female_2023_lower","DALYs_Female_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", 
                                         "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", 
                                         "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", 
                                         "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Xinjiang", "Qinghai", "Ningxia", 
                                         "Shaanxi", "Gansu", "Hong Kong", "Macao"))
data0 <- data0 %>% 
  arrange(location_name,rei_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:4)] <- lapply(data0[-(1:4)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_Both_change = round(((Deaths_Both_2023 - Deaths_Both_1990) / Deaths_Both_1990) * 100, 2),
    Deaths_Male_change = round(((Deaths_Male_2023 - Deaths_Male_1990) / Deaths_Male_1990) * 100, 2),
    Deaths_Female_change = round(((Deaths_Female_2023 - Deaths_Female_1990) / Deaths_Female_1990) * 100, 2),
    
    DALYs_Both_change = round(((DALYs_Both_2023 - DALYs_Both_1990) / DALYs_Both_1990) * 100, 2),
    DALYs_Male_change = round(((DALYs_Male_2023 - DALYs_Male_1990) / DALYs_Male_1990) * 100, 2),
    DALYs_Female_change = round(((DALYs_Female_2023 - DALYs_Female_1990) / DALYs_Female_1990) * 100, 2),
  ) %>% 
  mutate(Deaths_Both_1990_95UI=paste(Deaths_Both_1990,"(",Deaths_Both_1990_lower,",",Deaths_Both_1990_upper,")"),
         Deaths_Both_2023_95UI=paste(Deaths_Both_2023,"(",Deaths_Both_2023_lower,",",Deaths_Both_2023_upper,")"),
         
         Deaths_Male_1990_95UI=paste(Deaths_Male_1990,"(",Deaths_Male_1990_lower,",",Deaths_Male_1990_upper,")"),
         Deaths_Male_2023_95UI=paste(Deaths_Male_2023,"(",Deaths_Male_2023_lower,",",Deaths_Male_2023_upper,")"),
         
         Deaths_Female_1990_95UI=paste(Deaths_Female_1990,"(",Deaths_Female_1990_lower,",",Deaths_Female_1990_upper,")"),
         Deaths_Female_2023_95UI=paste(Deaths_Female_2023,"(",Deaths_Female_2023_lower,",",Deaths_Female_2023_upper,")"),
         
         DALYs_Both_1990_95UI=paste(DALYs_Both_1990,"(",DALYs_Both_1990_lower,",",DALYs_Both_1990_upper,")"),
         DALYs_Both_2023_95UI=paste(DALYs_Both_2023,"(",DALYs_Both_2023_lower,",",DALYs_Both_2023_upper,")"),
         
         DALYs_Male_1990_95UI=paste(DALYs_Male_1990,"(",DALYs_Male_1990_lower,",",DALYs_Male_1990_upper,")"),
         DALYs_Male_2023_95UI=paste(DALYs_Male_2023,"(",DALYs_Male_2023_lower,",",DALYs_Male_2023_upper,")"),
         
         DALYs_Female_1990_95UI=paste(DALYs_Female_1990,"(",DALYs_Female_1990_lower,",",DALYs_Female_1990_upper,")"),
         DALYs_Female_2023_95UI=paste(DALYs_Female_2023,"(",DALYs_Female_2023_lower,",",DALYs_Female_2023_upper,")")) %>% 
  select(location_name,rei_name,cause_name,age_name,
         Deaths_Both_1990_95UI, Deaths_Both_2023_95UI, Deaths_Both_change,
         Deaths_Male_1990_95UI, Deaths_Male_2023_95UI, Deaths_Male_change,
         Deaths_Female_1990_95UI, Deaths_Female_2023_95UI, Deaths_Female_change,
         DALYs_Both_1990_95UI, DALYs_Both_2023_95UI, DALYs_Both_change,
         DALYs_Male_1990_95UI, DALYs_Male_2023_95UI, DALYs_Male_change,
         DALYs_Female_1990_95UI, DALYs_Female_2023_95UI, DALYs_Female_change)

data0 <- data0 %>% 
  group_by(age_name,rei_name,cause_name) %>% 
  arrange(age_name,rei_name,cause_name,location_name) %>% 
  ungroup()

write.csv(data0,file = "~/China GBD/GBD2023/Table/level3_every_riskfac_agegroup_proportion.csv")

############################################################性别差异 level1 全因 figure####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&dietary_annual1.0$cause_name=="Non-communicable diseases")
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&dietary_annual1.0$cause_name=="Cardiovascular diseases")
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&dietary_annual1.0$cause_name=="Neoplasms")
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&dietary_annual1.0$cause_name=="Diabetes and kidney diseases")

allrisk_cause <- allrisk_cause %>% 
  select(measure_name,location_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(location_name=="China") %>% 
  filter(metric_name %in% c("Number","Rate")) %>% 
  filter(age_name %in% c("All ages", "Age-standardized")) %>% 
  filter(!(age_name=="All ages" & metric_name=="Rate"))

val_wide <- dcast(allrisk_cause, location_name + year + sex_name ~ 
                    measure_name+metric_name+ age_name,value.var = "val")

upper_wide <- dcast(allrisk_cause, location_name + year + sex_name  ~ 
                      measure_name+metric_name+ age_name, value.var = "upper")
colnames(upper_wide)[4:7] <- paste(colnames(upper_wide)[4:7],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause, location_name + year + sex_name  ~ 
                      measure_name+metric_name+ age_name, value.var = "lower")
colnames(lower_wide)[4:7] <- paste(colnames(lower_wide)[4:7],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name", "year", "sex_name")),
              lower_wide, by = c("location_name", "year", "sex_name"))

### 总慢病
number_rate_death <- ggplot(data0, aes(x = year, fill = sex_name)) +
  geom_bar(aes(y = `Deaths_Number_All ages`), stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymin = `Deaths_Number_All ages_lower`, ymax = `Deaths_Number_All ages_upper`), 
  #               position = position_dodge(width = 0.9), width = 0.25) +
  geom_line(aes(y = `Deaths_Rate_Age-standardized` * 11000, color = sex_name), size = 1.0) +
  geom_point(aes(y = `Deaths_Rate_Age-standardized` * 11000, color = sex_name), size = 2) +  # 添加点
  # geom_ribbon(aes(ymin = `Deaths_Rate_Age-standardized_lower` * 10000, ymax = `Deaths_Rate_Age-standardized_upper` * 10000, fill = sex_name), alpha = 0.2) +
  scale_x_continuous(
    name = "Year",
    breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023), # 关键时间点
    labels = c("1990", "1995", "2000", "2005", "2010", "2015", "2020", "2023"),
    expand = expansion(mult = 0.02) # 两边留一点空间
  ) +
  scale_y_continuous(
    name = "Total Death Cases",
    labels = scales::label_comma(),
    limits = c(0, 2500000),
    breaks = c(0, 2500000, seq(0, 2500000, by = 500000)), 
    sec.axis = sec_axis(~. / 11000, name = "Age-standardized Death Rate (per 100,000)")
  ) +
  labs(
    x = "Year"
  ) +
  scale_fill_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  scale_color_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
    axis.title.y = element_text(size = 12, family = "Arial", color = "black"),  
    axis.text = element_text(size = 12, family = "Arial", color = "black"),
    plot.title = element_text(size = 12, family = "Arial", face = "bold", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, family = "Arial", color = "black"),
    legend.title = element_text(size = 12, family = "Arial", color = "black"),
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    panel.grid = element_blank()  # 移除网格线
  )
ggsave(number_rate_death,file="~/China GBD/GBD2023/Figure/number_rate_death_Non-communicable_diseases.tif",width = 14,height = 8,dpi = 300)


number_rate_DALYs <- ggplot(data0, aes(x = year, fill = sex_name)) +
  geom_bar(aes(y = `DALYs_Number_All ages`/1000000), stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymin = `DALYs_Number_All ages_lower`, ymax = `DALYs_Number_All ages_upper`), 
  #               position = position_dodge(width = 0.9), width = 0.25) +
  geom_line(aes(y = `DALYs_Rate_Age-standardized` /100, color = sex_name), size = 1.0) +
  geom_point(aes(y = `DALYs_Rate_Age-standardized` /100, color = sex_name), size = 2) +  # 添加点
  # geom_ribbon(aes(ymin = `DALYs_Rate_Age-standardized_lower` * 10000, ymax = `DALYs_Rate_Age-standardized_upper` * 10000, fill = sex_name), alpha = 0.2) +
  scale_x_continuous(
    name = "Year",
    breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023), # 关键时间点
    labels = c("1990", "1995", "2000", "2005", "2010", "2015", "2020", "2023"),
    expand = expansion(mult = 0.02) # 两边留一点空间
  ) +
  scale_y_continuous(
    name = "Total DALYs Cases (million)",
    labels = scales::label_comma(),
    limits = c(0, 50),
    sec.axis = sec_axis(~. *100, name = "Age-standardized DALYs Rate (per 100,000)")
  ) +
  labs(x = "Year"
  ) +
  scale_fill_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  scale_color_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
    axis.title.y = element_text(size = 12, family = "Arial", color = "black"),  
    axis.text = element_text(size = 12, family = "Arial", color = "black"),
    plot.title = element_text(size = 12, family = "Arial", face = "bold", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, family = "Arial", color = "black"),
    legend.title = element_text(size = 12, family = "Arial", color = "black"),
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    panel.grid = element_blank()  # 移除网格线
  )
ggsave(number_rate_DALYs,file="~/China GBD/GBD2023/Figure/number_rate_DALYs_Non-communicable_diseases.tif",width = 14,height = 8,dpi = 300)

### CVD
number_rate_death <- ggplot(data0, aes(x = year, fill = sex_name)) +
  geom_bar(aes(y = `Deaths_Number_All ages`), stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymin = `Deaths_Number_All ages_lower`, ymax = `Deaths_Number_All ages_upper`), 
  #               position = position_dodge(width = 0.9), width = 0.25) +
  geom_line(aes(y = `Deaths_Rate_Age-standardized` * 8000, color = sex_name), size = 1.0) +
  geom_point(aes(y = `Deaths_Rate_Age-standardized` * 8000, color = sex_name), size = 2) +  # 添加点
  # geom_ribbon(aes(ymin = `Deaths_Rate_Age-standardized_lower` * 10000, ymax = `Deaths_Rate_Age-standardized_upper` * 10000, fill = sex_name), alpha = 0.2) +
  scale_x_continuous(
    name = "Year",
    breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023), # 关键时间点
    labels = c("1990", "1995", "2000", "2005", "2010", "2015", "2020", "2023"),
    expand = expansion(mult = 0.02) # 两边留一点空间
  ) +
  scale_y_continuous(
    name = "Total Death Cases",
    labels = scales::label_comma(),
    limits = c(0, 1600000),
    breaks = c(0, 1600000, seq(0, 1600000, by = 400000)), 
    sec.axis = sec_axis(~. / 8000, name = "Age-standardized Death Rate (per 100,000)")
  ) +
  labs(
    x = "Year"
  ) +
  scale_fill_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  scale_color_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
    axis.title.y = element_text(size = 12, family = "Arial", color = "black"),  
    axis.text = element_text(size = 12, family = "Arial", color = "black"),
    plot.title = element_text(size = 12, family = "Arial", face = "bold", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, family = "Arial", color = "black"),
    legend.title = element_text(size = 12, family = "Arial", color = "black"),
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    panel.grid = element_blank()  # 移除网格线
  )
ggsave(number_rate_death,file="~/China GBD/GBD2023/Figure/number_rate_death_CVD.tif",width = 14,height = 8,dpi = 300)


number_rate_DALYs <- ggplot(data0, aes(x = year, fill = sex_name)) +
  geom_bar(aes(y = `DALYs_Number_All ages`/1000000), stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymin = `DALYs_Number_All ages_lower`, ymax = `DALYs_Number_All ages_upper`), 
  #               position = position_dodge(width = 0.9), width = 0.25) +
  geom_line(aes(y = `DALYs_Rate_Age-standardized` /100, color = sex_name), size = 1.0) +
  geom_point(aes(y = `DALYs_Rate_Age-standardized` /100, color = sex_name), size = 2) +  # 添加点
  # geom_ribbon(aes(ymin = `DALYs_Rate_Age-standardized_lower` * 10000, ymax = `DALYs_Rate_Age-standardized_upper` * 10000, fill = sex_name), alpha = 0.2) +
  scale_x_continuous(
    name = "Year",
    breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023), # 关键时间点
    labels = c("1990", "1995", "2000", "2005", "2010", "2015", "2020", "2023"),
    expand = expansion(mult = 0.02) # 两边留一点空间
  ) +
  scale_y_continuous(
    name = "Total DALYs Cases (million)",
    labels = scales::label_comma(),
    limits = c(0, 40),
    sec.axis = sec_axis(~. *100, name = "Age-standardized DALYs Rate (per 100,000)")
  ) +
  labs(x = "Year"
  ) +
  scale_fill_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  scale_color_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
    axis.title.y = element_text(size = 12, family = "Arial", color = "black"),  
    axis.text = element_text(size = 12, family = "Arial", color = "black"),
    plot.title = element_text(size = 12, family = "Arial", face = "bold", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, family = "Arial", color = "black"),
    legend.title = element_text(size = 12, family = "Arial", color = "black"),
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    panel.grid = element_blank()  # 移除网格线
  )
ggsave(number_rate_DALYs,file="~/China GBD/GBD2023/Figure/number_rate_DALYs_CVD.tif",width = 14,height = 8,dpi = 300)

### Neoplasms
number_rate_death <- ggplot(data0, aes(x = year, fill = sex_name)) +
  geom_bar(aes(y = `Deaths_Number_All ages`), stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymin = `Deaths_Number_All ages_lower`, ymax = `Deaths_Number_All ages_upper`), 
  #               position = position_dodge(width = 0.9), width = 0.25) +
  geom_line(aes(y = `Deaths_Rate_Age-standardized` * 9000, color = sex_name), size = 1.0) +
  geom_point(aes(y = `Deaths_Rate_Age-standardized` * 9000, color = sex_name), size = 2) +  # 添加点
  # geom_ribbon(aes(ymin = `Deaths_Rate_Age-standardized_lower` * 10000, ymax = `Deaths_Rate_Age-standardized_upper` * 10000, fill = sex_name), alpha = 0.2) +
  scale_x_continuous(
    name = "Year",
    breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023), # 关键时间点
    labels = c("1990", "1995", "2000", "2005", "2010", "2015", "2020", "2023"),
    expand = expansion(mult = 0.02) # 两边留一点空间
  ) +
  scale_y_continuous(
    name = "Total Death Cases",
    labels = scales::label_comma(),
    limits = c(0, 200000),
    breaks = c(0, 200000, seq(0, 200000, by = 50000)), 
    sec.axis = sec_axis(~. / 9000, name = "Age-standardized Death Rate (per 100,000)")
  ) +
  labs(
    x = "Year"
  ) +
  scale_fill_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  scale_color_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
    axis.title.y = element_text(size = 12, family = "Arial", color = "black"),  
    axis.text = element_text(size = 12, family = "Arial", color = "black"),
    plot.title = element_text(size = 12, family = "Arial", face = "bold", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, family = "Arial", color = "black"),
    legend.title = element_text(size = 12, family = "Arial", color = "black"),
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    panel.grid = element_blank()  # 移除网格线
  )
ggsave(number_rate_death,file="~/China GBD/GBD2023/Figure/number_rate_death_Neoplasms.tif",width = 14,height = 8,dpi = 300)


number_rate_DALYs <- ggplot(data0, aes(x = year, fill = sex_name)) +
  geom_bar(aes(y = `DALYs_Number_All ages`/1000000), stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymin = `DALYs_Number_All ages_lower`, ymax = `DALYs_Number_All ages_upper`), 
  #               position = position_dodge(width = 0.9), width = 0.25) +
  geom_line(aes(y = `DALYs_Rate_Age-standardized` /100, color = sex_name), size = 1.0) +
  geom_point(aes(y = `DALYs_Rate_Age-standardized` /100, color = sex_name), size = 2) +  # 添加点
  # geom_ribbon(aes(ymin = `DALYs_Rate_Age-standardized_lower` * 10000, ymax = `DALYs_Rate_Age-standardized_upper` * 10000, fill = sex_name), alpha = 0.2) +
  scale_x_continuous(
    name = "Year",
    breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023), # 关键时间点
    labels = c("1990", "1995", "2000", "2005", "2010", "2015", "2020", "2023"),
    expand = expansion(mult = 0.02) # 两边留一点空间
  ) +
  scale_y_continuous(
    name = "Total DALYs Cases (million)",
    labels = scales::label_comma(),
    limits = c(0, 6),
    sec.axis = sec_axis(~. *100, name = "Age-standardized DALYs Rate (per 100,000)")
  ) +
  labs(x = "Year"
  ) +
  scale_fill_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  scale_color_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
    axis.title.y = element_text(size = 12, family = "Arial", color = "black"),  
    axis.text = element_text(size = 12, family = "Arial", color = "black"),
    plot.title = element_text(size = 12, family = "Arial", face = "bold", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, family = "Arial", color = "black"),
    legend.title = element_text(size = 12, family = "Arial", color = "black"),
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    panel.grid = element_blank()  # 移除网格线
  )
ggsave(number_rate_DALYs,file="~/China GBD/GBD2023/Figure/number_rate_DALYs_Neoplasms.tif",width = 14,height = 8,dpi = 300)

### Diabetes and kidney diseases
number_rate_death <- ggplot(data0, aes(x = year, fill = sex_name)) +
  geom_bar(aes(y = `Deaths_Number_All ages`), stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymin = `Deaths_Number_All ages_lower`, ymax = `Deaths_Number_All ages_upper`), 
  #               position = position_dodge(width = 0.9), width = 0.25) +
  geom_line(aes(y = `Deaths_Rate_Age-standardized` * 16000, color = sex_name), size = 1.0) +
  geom_point(aes(y = `Deaths_Rate_Age-standardized` * 16000, color = sex_name), size = 2) +  # 添加点
  # geom_ribbon(aes(ymin = `Deaths_Rate_Age-standardized_lower` * 10000, ymax = `Deaths_Rate_Age-standardized_upper` * 10000, fill = sex_name), alpha = 0.2) +
  scale_x_continuous(
    name = "Year",
    breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023), # 关键时间点
    labels = c("1990", "1995", "2000", "2005", "2010", "2015", "2020", "2023"),
    expand = expansion(mult = 0.02) # 两边留一点空间
  ) +
  scale_y_continuous(
    name = "Total Death Cases",
    labels = scales::label_comma(),
    limits = c(0, 80000),
    breaks = c(0, 80000, seq(0, 80000, by = 20000)), 
    sec.axis = sec_axis(~. / 16000, name = "Age-standardized Death Rate (per 100,000)")
  ) +
  labs(
    x = "Year"
  ) +
  scale_fill_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  scale_color_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
    axis.title.y = element_text(size = 12, family = "Arial", color = "black"),  
    axis.text = element_text(size = 12, family = "Arial", color = "black"),
    plot.title = element_text(size = 12, family = "Arial", face = "bold", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, family = "Arial", color = "black"),
    legend.title = element_text(size = 12, family = "Arial", color = "black"),
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    panel.grid = element_blank()  # 移除网格线
  )
ggsave(number_rate_death,file="~/China GBD/GBD2023/Figure/number_rate_death_Diabetes_and_kidney_diseases.tif",width = 14,height = 8,dpi = 300)


number_rate_DALYs <- ggplot(data0, aes(x = year, fill = sex_name)) +
  geom_bar(aes(y = `DALYs_Number_All ages`/1000000), stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymin = `DALYs_Number_All ages_lower`, ymax = `DALYs_Number_All ages_upper`), 
  #               position = position_dodge(width = 0.9), width = 0.25) +
  geom_line(aes(y = `DALYs_Rate_Age-standardized` /60, color = sex_name), size = 1.0) +
  geom_point(aes(y = `DALYs_Rate_Age-standardized` /60, color = sex_name), size = 2) +  # 添加点
  # geom_ribbon(aes(ymin = `DALYs_Rate_Age-standardized_lower` * 10000, ymax = `DALYs_Rate_Age-standardized_upper` * 10000, fill = sex_name), alpha = 0.2) +
  scale_x_continuous(
    name = "Year",
    breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023), # 关键时间点
    labels = c("1990", "1995", "2000", "2005", "2010", "2015", "2020", "2023"),
    expand = expansion(mult = 0.02) # 两边留一点空间
  ) +
  scale_y_continuous(
    name = "Total DALYs Cases (million)",
    labels = scales::label_comma(),
    limits = c(0, 4),
    sec.axis = sec_axis(~. *60, name = "Age-standardized DALYs Rate (per 100,000)")
  ) +
  labs(x = "Year"
  ) +
  scale_fill_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  scale_color_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
    axis.title.y = element_text(size = 12, family = "Arial", color = "black"),  
    axis.text = element_text(size = 12, family = "Arial", color = "black"),
    plot.title = element_text(size = 12, family = "Arial", face = "bold", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, family = "Arial", color = "black"),
    legend.title = element_text(size = 12, family = "Arial", color = "black"),
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    panel.grid = element_blank()  # 移除网格线
  )
ggsave(number_rate_DALYs,file="~/China GBD/GBD2023/Figure/number_rate_DALYs_Diabetes_and_kidney_diseases.tif",width = 14,height = 8,dpi = 300)

############################################################性别差异 level1 every risk factor figure####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name %in% c("Diet low in fruits", "Diet low in legumes", "Diet low in vegetables", "Diet high in red meat",
                                                                            "Diet low in whole grains", "Diet high in sodium", "Diet low in milk", "Diet high in processed meat",
                                                                            "Diet low in seafood omega-3 fatty acids", "Diet low in polyunsaturated fatty acids", "Diet low in fiber",
                                                                            "Diet low in nuts and seeds", "Diet high in sugar-sweetened beverages", "Diet low in calcium", 
                                                                            "Diet high in trans fatty acids") & dietary_annual1.0$cause_name=="Non-communicable diseases")
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name %in% c("Diet low in fruits", "Diet low in legumes", "Diet low in vegetables", "Diet high in red meat",
                                                                            "Diet low in whole grains", "Diet high in sodium", "Diet low in milk", "Diet high in processed meat",
                                                                            "Diet low in seafood omega-3 fatty acids", "Diet low in polyunsaturated fatty acids", "Diet low in fiber",
                                                                            "Diet low in nuts and seeds", "Diet high in sugar-sweetened beverages", "Diet low in calcium", 
                                                                            "Diet high in trans fatty acids") & dietary_annual1.0$cause_name %in% c("Tracheal, bronchus, and lung cancer",
                                                                                                                                                    "Breast cancer",
                                                                                                                                                    "Prostate cancer",
                                                                                                                                                    "Colon and rectum cancer",
                                                                                                                                                    "Esophageal cancer",
                                                                                                                                                    "Stomach cancer",
                                                                                                                                                    "Diabetes mellitus",
                                                                                                                                                    "Chronic kidney disease",
                                                                                                                                                    "Ischemic heart disease",
                                                                                                                                                    "Aortic aneurysm",
                                                                                                                                                    "Lower extremity peripheral arterial disease",
                                                                                                                                                    "Hypertensive heart disease",
                                                                                                                                                    "Stroke",
                                                                                                                                                    "Atrial fibrillation and flutter"))


allrisk_cause <- allrisk_cause %>% 
  select(measure_name,location_name,rei_name,cause_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(location_name=="China") %>% 
  filter(metric_name %in% c("Number","Rate")) %>% 
  filter(age_name %in% c("All ages", "Age-standardized")) %>% 
  filter(!(age_name=="All ages" & metric_name=="Rate"))

val_wide <- dcast(allrisk_cause, location_name + year + sex_name+rei_name+cause_name ~ 
                    measure_name+metric_name+ age_name,value.var = "val")

upper_wide <- dcast(allrisk_cause, location_name + year + sex_name+rei_name+cause_name  ~ 
                      measure_name+metric_name+ age_name, value.var = "upper")
colnames(upper_wide)[6:9] <- paste(colnames(upper_wide)[6:9],"_upper",sep = "")

lower_wide <- dcast(allrisk_cause, location_name + year + sex_name+rei_name+cause_name  ~ 
                      measure_name+metric_name+ age_name, value.var = "lower")
colnames(lower_wide)[6:9] <- paste(colnames(lower_wide)[6:9],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name", "year", "sex_name","rei_name","cause_name")),
              lower_wide, by = c("location_name", "year", "sex_name","rei_name","cause_name"))


#####红肉
red_meat <- filter(data0,rei_name=="Diet high in red meat"&sex_name=="Both")

library(dplyr)
library(ggplot2)

# 获取所有独特的 Cause 名称
causes <- unique(red_meat$cause_name)

# 用来存储每个 Cause 的图
plot_list <- list()

# 为每个 Cause 创建单独的图
for (cause in causes) {
  # 筛选出当前 Cause 的数据
  cause_data <- filter(red_meat, cause_name == cause)
  
  # 检查最大值和最小值
  max_deaths <- max(cause_data$`Deaths_Number_All ages`, na.rm = TRUE)
  min_deaths <- min(cause_data$`Deaths_Number_All ages`, na.rm = TRUE)
  
  # 设置合理的默认最大值和最小值
  if (max_deaths <= 0) {
    max_deaths <- 10000  # 默认最大值
  }
  
  # 如果是 'stroke' 相关的，设置特定的 y 轴范围
  if (cause == "Stroke") {
    y_limits <- c(-100000, 2000)  # Stroke 设置为负值到2000的范围
  } else {
    y_limits <- c(0, max_deaths)  # 其他情况使用正值范围
  }
  
  # 创建图表
  p <- ggplot(cause_data, aes(x = year, y = `Deaths_Number_All ages`, fill = cause_name)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_y_continuous(
      name = "Total Death Cases",
      labels = scales::label_comma(),
      limits = y_limits,  # 使用动态设置的 y 轴范围
      breaks = seq(0, max_deaths, by = 10000)  # 使用适当的步长
    ) +
    labs(
      title = paste("Total Death Cases of", cause, "Attributable to Red Meat"),
      x = "Year"
    ) +
    scale_fill_manual(values = c("Breast cancer" = "#D84B16", 
                                 "Chronic kidney disease" = "#F5B800", 
                                 "Colon and rectum cancer" = "#00A4B8", 
                                 "Diabetes mellitus" = "#39A6A4", 
                                 "Ischemic heart disease" = "#004B8D", 
                                 "Lower extremity peripheral arterial disease" = "#B50000", 
                                 "Stroke" = "#FF7F4F"), 
                      name = "Cause") +
    theme_minimal(base_size = 15) +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      axis.line = element_line(color = "black", linewidth = 0.7),
      axis.title.x = element_text(size = 12, family = "Arial", color = "black"),
      axis.title.y = element_text(size = 12, family = "Arial", color = "black"),
      axis.text = element_text(size = 12, family = "Arial", color = "black"),
      plot.title = element_text(size = 12, family = "Arial", face = "bold", color = "black"),
      strip.background = element_blank(),
      strip.text = element_text(size = 12, family = "Arial", color = "black"),
      legend.title = element_text(size = 12, family = "Arial", color = "black"),
      legend.text = element_text(size = 12, family = "Arial", color = "black"),
      panel.grid = element_blank()
    )
  
  # 将当前 Cause 图添加到 plot_list 中
  plot_list[[cause]] <- p
}

# 你可以选择查看或保存每个图表，或者使用 patchwork 等拼接它们


# 你可以选择查看或保存每个图表，或者使用 patchwork 等拼接它们
library(patchwork)

# 将所有绘制的图表拼接在一起
combined_plot <- plot_list[["Breast cancer"]] +
  plot_list[["Chronic kidney disease"]] +
  plot_list[["Colon and rectum cancer"]] +
  plot_list[["Diabetes mellitus"]] +
  plot_list[["Ischemic heart disease"]] +
  plot_list[["Lower extremity peripheral arterial disease"]] +
  plot_list[["Stroke"]] +
  plot_layout(ncol = 2)  # 设置每行显示2个图表，可以根据需要调整

# 显示拼接后的图表
print(combined_plot)


number_rate_death <- ggplot(red_meat, aes(x = year, fill = cause_name)) +
  geom_bar(aes(y = `Deaths_Number_All ages`), stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymin = `Deaths_Number_All ages_lower`, ymax = `Deaths_Number_All ages_upper`), 
  #               position = position_dodge(width = 0.9), width = 0.25) +
  #geom_line(aes(y = `Deaths_Rate_Age-standardized` * 10000, color = sex_name), size = 1.0) +
  #geom_point(aes(y = `Deaths_Rate_Age-standardized` * 10000, color = sex_name), size = 2) +  # 添加点
  # geom_ribbon(aes(ymin = `Deaths_Rate_Age-standardized_lower` * 10000, ymax = `Deaths_Rate_Age-standardized_upper` * 10000, fill = sex_name), alpha = 0.2) +
  scale_y_continuous(
    name = "Total Death Cases",
    labels = scales::label_comma(),
    limits = c(-100000, 100000),
    breaks = seq(-100000, 100000, by = 10000)
    #sec.axis = sec_axis(~. / 10000, name = "Age-standardized Death Rate (per 100,000)")
  ) +
  labs(
    title = "Total Death Cases of NCDs attributable to red meat by Gender",
    x = "Year"
  ) +
  scale_fill_manual(values = c("Breast cancer" = "#D84B16", 
                               "Chronic kidney disease" = "#F5B800", 
                               "Colon and rectum cancer" = "#00A4B8", 
                               "Diabetes mellitus" = "#39A6A4", 
                               "Ischemic heart disease" = "#004B8D", 
                               "Lower extremity peripheral arterial disease" = "#B50000", 
                               "Stroke" = "#FF7F4F"), 
                    name = "Cause") +
  scale_color_manual(values = c("Breast cancer" = "#D84B16", 
                                "Chronic kidney disease" = "#F5B800", 
                                "Colon and rectum cancer" = "#00A4B8", 
                                "Diabetes mellitus" = "#39A6A4", 
                                "Ischemic heart disease" = "#004B8D", 
                                "Lower extremity peripheral arterial disease" = "#B50000", 
                                "Stroke" = "#FF7F4F"), 
                     name = "Cause")+
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
    axis.title.y = element_text(size = 12, family = "Arial", color = "black"),  
    axis.text = element_text(size = 12, family = "Arial", color = "black"),
    plot.title = element_text(size = 12, family = "Arial", face = "bold", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, family = "Arial", color = "black"),
    legend.title = element_text(size = 12, family = "Arial", color = "black"),
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    panel.grid = element_blank()  # 移除网格线
  )

number_rate_death <- ggplot(red_meat, aes(x = year, y = `Deaths_Number_All ages`, fill = cause_name)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~cause_name, scales = "free_y") +  # 使用facet_wrap按疾病类型分面
  scale_y_continuous(
    name = "Total Death Cases",
    labels = scales::label_comma(),
    limits = c(0, max(red_meat$`Deaths_Number_All ages`, na.rm = TRUE)),
    breaks = seq(0, max(red_meat$`Deaths_Number_All ages`, na.rm = TRUE), by = 10000)
  ) +
  labs(
    title = "Total Death Cases of NCDs Attributable to Red Meat by Cause",
    x = "Year"
  ) +
  scale_fill_manual(values = c("Breast cancer" = "#D84B16", 
                               "Chronic kidney disease" = "#F5B800", 
                               "Colon and rectum cancer" = "#00A4B8", 
                               "Diabetes mellitus" = "#39A6A4", 
                               "Ischemic heart disease" = "#004B8D", 
                               "Lower extremity peripheral arterial disease" = "#B50000", 
                               "Stroke" = "#FF7F4F"), 
                    name = "Cause") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.title.x = element_text(size = 12, family = "Arial", color = "black"),
    axis.title.y = element_text(size = 12, family = "Arial", color = "black"),
    axis.text = element_text(size = 12, family = "Arial", color = "black"),
    plot.title = element_text(size = 12, family = "Arial", face = "bold", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, family = "Arial", color = "black"),
    legend.title = element_text(size = 12, family = "Arial", color = "black"),
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    panel.grid = element_blank()
  )


ggsave(number_rate_death,file="number_rate_death_redmeat.png",width = 14,height = 8,dpi = 300)

number_rate_DALY <- ggplot(red_meat, aes(x = year, fill = sex_name)) +
  geom_bar(aes(y = `DALYs_Number_All ages`), stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymin = `Deaths_Number_All ages_lower`, ymax = `Deaths_Number_All ages_upper`), 
  #               position = position_dodge(width = 0.9), width = 0.25) +
  #geom_line(aes(y = `Deaths_Rate_Age-standardized` * 10000, color = sex_name), size = 1.0) +
  #geom_point(aes(y = `Deaths_Rate_Age-standardized` * 10000, color = sex_name), size = 2) +  # 添加点
  # geom_ribbon(aes(ymin = `Deaths_Rate_Age-standardized_lower` * 10000, ymax = `Deaths_Rate_Age-standardized_upper` * 10000, fill = sex_name), alpha = 0.2) +
  scale_y_continuous(
    name = "Total DALYs Cases",
    labels = scales::label_comma(),
    limits = c(-500000, 2000000),
    breaks = seq(-500000, 2000000, by = 500000)
    #sec.axis = sec_axis(~. / 10000, name = "Age-standardized Death Rate (per 100,000)")
  ) +
  labs(
    title = "Total DALYs Cases of NCDs attributable to red meat by Gender",
    x = "Year"
  ) +
  scale_fill_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  scale_color_manual(values = c("Both" = "#ED0000FF", "Male" = "#00468BFF", "Female" = "#FDAF91FF"), name = "Sex") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
    axis.title.y = element_text(size = 12, family = "Arial", color = "black"),  
    axis.text = element_text(size = 12, family = "Arial", color = "black"),
    plot.title = element_text(size = 12, family = "Arial", face = "bold", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, family = "Arial", color = "black"),
    legend.title = element_text(size = 12, family = "Arial", color = "black"),
    legend.text = element_text(size = 12, family = "Arial", color = "black"),
    panel.grid = element_blank()  # 移除网格线
  )
ggsave(number_rate_DALY,file="number_rate_DALY_redmeat.png",width = 14,height = 8,dpi = 300)

############################################################国内国际对比(全年龄)####
compare <- read.csv("国际.csv")
compare$measure_name[compare$measure_name=="DALYs (Disability-Adjusted Life Years)"] <- "DALYs"
compare <- compare %>% 
  select(measure_name,location_name,sex_name,age_name,rei_name,metric_name,year,val,upper,lower) %>% 
  filter(location_name %in% c("Global",
                              "China",
                              "Brazil", "Russia", "India", "South Africa", 
                              "Argentina", "Australia", "Canada", "France", "Germany",  "Indonesia", 
                              "Italy", "Japan", "Mexico",  "Saudi Arabia", "Turkey",
                              "United Kingdom", "United States of America",
                              "South Korea", "Mongolia", "North Korea")) %>% 
  filter(metric_name%in%c("Number","Rate")) %>% 
  filter(age_name %in% c("All ages","Age-standardized")) %>% 
  filter(!(age_name=="All ages" & metric_name=="Rate"))

compare$rei_name <- factor(compare$rei_name,levels = c("Dietary risks","Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", "Diet low in polyunsaturated fatty acids",
                                                       "Diet low in nuts and seeds", "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids",
                                                       "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", "Diet low in vegetables", "Diet low in calcium",
                                                       "Diet high in sugar-sweetened beverages", "Diet high in trans fatty acids"))

val_wide <- dcast(compare, location_name+rei_name ~ 
                    measure_name+sex_name+metric_name+year,value.var = "val")

upper_wide <- dcast(compare, location_name+rei_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "upper")
colnames(upper_wide)[3:26] <- paste(colnames(upper_wide)[3:26],"_upper",sep = "")

lower_wide <- dcast(compare, location_name+rei_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "lower")
colnames(lower_wide)[3:26] <- paste(colnames(lower_wide)[3:26],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","rei_name")),
              lower_wide, by = c("location_name","rei_name"))

data0 <- data0[,c("location_name","rei_name",
                  "Deaths_Both_Number_1990","Deaths_Both_Number_1990_lower","Deaths_Both_Number_1990_upper",
                  "Deaths_Both_Number_2023","Deaths_Both_Number_2023_lower","Deaths_Both_Number_2023_upper",
                  "Deaths_Both_Rate_1990","Deaths_Both_Rate_1990_lower","Deaths_Both_Rate_1990_upper",
                  "Deaths_Both_Rate_2023","Deaths_Both_Rate_2023_lower","Deaths_Both_Rate_2023_upper",
                  
                  "Deaths_Male_Number_1990","Deaths_Male_Number_1990_lower","Deaths_Male_Number_1990_upper",
                  "Deaths_Male_Number_2023","Deaths_Male_Number_2023_lower","Deaths_Male_Number_2023_upper",
                  "Deaths_Male_Rate_1990","Deaths_Male_Rate_1990_lower","Deaths_Male_Rate_1990_upper",
                  "Deaths_Male_Rate_2023","Deaths_Male_Rate_2023_lower","Deaths_Male_Rate_2023_upper",
                  
                  "Deaths_Female_Number_1990","Deaths_Female_Number_1990_lower","Deaths_Female_Number_1990_upper",
                  "Deaths_Female_Number_2023","Deaths_Female_Number_2023_lower","Deaths_Female_Number_2023_upper",
                  "Deaths_Female_Rate_1990","Deaths_Female_Rate_1990_lower","Deaths_Female_Rate_1990_upper",
                  "Deaths_Female_Rate_2023","Deaths_Female_Rate_2023_lower","Deaths_Female_Rate_2023_upper",
                  
                  
                  "DALYs_Both_Number_1990","DALYs_Both_Number_1990_lower","DALYs_Both_Number_1990_upper",
                  "DALYs_Both_Number_2023","DALYs_Both_Number_2023_lower","DALYs_Both_Number_2023_upper",
                  "DALYs_Both_Rate_1990","DALYs_Both_Rate_1990_lower","DALYs_Both_Rate_1990_upper",
                  "DALYs_Both_Rate_2023","DALYs_Both_Rate_2023_lower","DALYs_Both_Rate_2023_upper",
                  
                  "DALYs_Male_Number_1990","DALYs_Male_Number_1990_lower","DALYs_Male_Number_1990_upper",
                  "DALYs_Male_Number_2023","DALYs_Male_Number_2023_lower","DALYs_Male_Number_2023_upper",
                  "DALYs_Male_Rate_1990","DALYs_Male_Rate_1990_lower","DALYs_Male_Rate_1990_upper",
                  "DALYs_Male_Rate_2023","DALYs_Male_Rate_2023_lower","DALYs_Male_Rate_2023_upper",
                  
                  "DALYs_Female_Number_1990","DALYs_Female_Number_1990_lower","DALYs_Female_Number_1990_upper",
                  "DALYs_Female_Number_2023","DALYs_Female_Number_2023_lower","DALYs_Female_Number_2023_upper",
                  "DALYs_Female_Rate_1990","DALYs_Female_Rate_1990_lower","DALYs_Female_Rate_1990_upper",
                  "DALYs_Female_Rate_2023","DALYs_Female_Rate_2023_lower","DALYs_Female_Rate_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("Global",
                                         "China",
                                         "Brazil", "Russia", "India", "South Africa", 
                                         "Argentina", "Australia", "Canada", "France", "Germany",  "Indonesia", 
                                         "Italy", "Japan", "Mexico",  "Saudi Arabia",  "Turkey",
                                         "United Kingdom", "United States of America",
                                         "South Korea", "Mongolia", "North Korea"))
data0 <- data0 %>% 
  arrange(location_name,rei_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:2)] <- lapply(data0[-(1:2)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_num_Both_change = round(((Deaths_Both_Number_2023 - Deaths_Both_Number_1990) / Deaths_Both_Number_1990) * 100, 2),
    Deaths_rate_Both_change = round(((Deaths_Both_Rate_2023 - Deaths_Both_Rate_1990) / Deaths_Both_Rate_1990) * 100, 2),
    Deaths_num_Male_change = round(((Deaths_Male_Number_2023 - Deaths_Male_Number_1990) / Deaths_Male_Number_1990) * 100, 2),
    Deaths_rate_Male_change = round(((Deaths_Male_Rate_2023 - Deaths_Male_Rate_1990) / Deaths_Male_Rate_1990) * 100, 2),
    Deaths_num_Female_change = round(((Deaths_Female_Number_2023 - Deaths_Female_Number_1990) / Deaths_Female_Number_1990) * 100, 2),
    Deaths_rate_Female_change = round(((Deaths_Female_Rate_2023 - Deaths_Female_Rate_1990) / Deaths_Female_Rate_1990) * 100, 2),
    
    DALYs_num_Both_change = round(((DALYs_Both_Number_2023 - DALYs_Both_Number_1990) / DALYs_Both_Number_1990) * 100, 2),
    DALYs_rate_Both_change = round(((DALYs_Both_Rate_2023 - DALYs_Both_Rate_1990) / DALYs_Both_Rate_1990) * 100, 2),
    DALYs_num_Male_change = round(((DALYs_Male_Number_2023 - DALYs_Male_Number_1990) / DALYs_Male_Number_1990) * 100, 2),
    DALYs_rate_Male_change = round(((DALYs_Male_Rate_2023 - DALYs_Male_Rate_1990) / DALYs_Male_Rate_1990) * 100, 2),
    DALYs_num_Female_change = round(((DALYs_Female_Number_2023 - DALYs_Female_Number_1990) / DALYs_Female_Number_1990) * 100, 2),
    DALYs_rate_Female_change = round(((DALYs_Female_Rate_2023 - DALYs_Female_Rate_1990) / DALYs_Female_Rate_1990) * 100, 2)
  ) %>% 
  mutate(Deaths_num_Both_1990_95UI=paste(Deaths_Both_Number_1990,"(",Deaths_Both_Number_1990_lower,",",Deaths_Both_Number_1990_upper,")"),
         Deaths_num_Both_2023_95UI=paste(Deaths_Both_Number_2023,"(",Deaths_Both_Number_2023_lower,",",Deaths_Both_Number_2023_upper,")"),
         Deaths_rate_Both_1990_95UI=paste(Deaths_Both_Rate_1990,"(",Deaths_Both_Rate_1990_lower,",",Deaths_Both_Rate_1990_upper,")"),
         Deaths_rate_Both_2023_95UI=paste(Deaths_Both_Rate_2023,"(",Deaths_Both_Rate_2023_lower,",",Deaths_Both_Rate_2023_upper,")"),
         
         Deaths_num_Male_1990_95UI=paste(Deaths_Male_Number_1990,"(",Deaths_Male_Number_1990_lower,",",Deaths_Male_Number_1990_upper,")"),
         Deaths_num_Male_2023_95UI=paste(Deaths_Male_Number_2023,"(",Deaths_Male_Number_2023_lower,",",Deaths_Male_Number_2023_upper,")"),
         Deaths_rate_Male_1990_95UI=paste(Deaths_Male_Rate_1990,"(",Deaths_Male_Rate_1990_lower,",",Deaths_Male_Rate_1990_upper,")"),
         Deaths_rate_Male_2023_95UI=paste(Deaths_Male_Rate_2023,"(",Deaths_Male_Rate_2023_lower,",",Deaths_Male_Rate_2023_upper,")"),
         
         Deaths_num_Female_1990_95UI=paste(Deaths_Female_Number_1990,"(",Deaths_Female_Number_1990_lower,",",Deaths_Female_Number_1990_upper,")"),
         Deaths_num_Female_2023_95UI=paste(Deaths_Female_Number_2023,"(",Deaths_Female_Number_2023_lower,",",Deaths_Female_Number_2023_upper,")"),
         Deaths_rate_Female_1990_95UI=paste(Deaths_Female_Rate_1990,"(",Deaths_Female_Rate_1990_lower,",",Deaths_Female_Rate_1990_upper,")"),
         Deaths_rate_Female_2023_95UI=paste(Deaths_Female_Rate_2023,"(",Deaths_Female_Rate_2023_lower,",",Deaths_Female_Rate_2023_upper,")"),
         
         DALYs_num_Both_1990_95UI=paste(DALYs_Both_Number_1990,"(",DALYs_Both_Number_1990_lower,",",DALYs_Both_Number_1990_upper,")"),
         DALYs_num_Both_2023_95UI=paste(DALYs_Both_Number_2023,"(",DALYs_Both_Number_2023_lower,",",DALYs_Both_Number_2023_upper,")"),
         DALYs_rate_Both_1990_95UI=paste(DALYs_Both_Rate_1990,"(",DALYs_Both_Rate_1990_lower,",",DALYs_Both_Rate_1990_upper,")"),
         DALYs_rate_Both_2023_95UI=paste(DALYs_Both_Rate_2023,"(",DALYs_Both_Rate_2023_lower,",",DALYs_Both_Rate_2023_upper,")"),
         
         DALYs_num_Male_1990_95UI=paste(DALYs_Male_Number_1990,"(",DALYs_Male_Number_1990_lower,",",DALYs_Male_Number_1990_upper,")"),
         DALYs_num_Male_2023_95UI=paste(DALYs_Male_Number_2023,"(",DALYs_Male_Number_2023_lower,",",DALYs_Male_Number_2023_upper,")"),
         DALYs_rate_Male_1990_95UI=paste(DALYs_Male_Rate_1990,"(",DALYs_Male_Rate_1990_lower,",",DALYs_Male_Rate_1990_upper,")"),
         DALYs_rate_Male_2023_95UI=paste(DALYs_Male_Rate_2023,"(",DALYs_Male_Rate_2023_lower,",",DALYs_Male_Rate_2023_upper,")"),
         
         DALYs_num_Female_1990_95UI=paste(DALYs_Female_Number_1990,"(",DALYs_Female_Number_1990_lower,",",DALYs_Female_Number_1990_upper,")"),
         DALYs_num_Female_2023_95UI=paste(DALYs_Female_Number_2023,"(",DALYs_Female_Number_2023_lower,",",DALYs_Female_Number_2023_upper,")"),
         DALYs_rate_Female_1990_95UI=paste(DALYs_Female_Rate_1990,"(",DALYs_Female_Rate_1990_lower,",",DALYs_Female_Rate_1990_upper,")"),
         DALYs_rate_Female_2023_95UI=paste(DALYs_Female_Rate_2023,"(",DALYs_Female_Rate_2023_lower,",",DALYs_Female_Rate_2023_upper,")")) %>% 
  select(location_name,rei_name,
         Deaths_num_Both_1990_95UI, Deaths_num_Both_2023_95UI, Deaths_num_Both_change,
         Deaths_rate_Both_1990_95UI, Deaths_rate_Both_2023_95UI, Deaths_rate_Both_change,
         Deaths_num_Male_1990_95UI, Deaths_num_Male_2023_95UI, Deaths_num_Male_change,
         Deaths_rate_Male_1990_95UI, Deaths_rate_Male_2023_95UI, Deaths_rate_Male_change,
         Deaths_num_Female_1990_95UI, Deaths_num_Female_2023_95UI, Deaths_num_Female_change,
         Deaths_rate_Female_1990_95UI, Deaths_rate_Female_2023_95UI, Deaths_rate_Female_change,
         DALYs_num_Both_1990_95UI, DALYs_num_Both_2023_95UI, DALYs_num_Both_change,
         DALYs_rate_Both_1990_95UI, DALYs_rate_Both_2023_95UI, DALYs_rate_Both_change,
         DALYs_num_Male_1990_95UI, DALYs_num_Male_2023_95UI, DALYs_num_Male_change,
         DALYs_rate_Male_1990_95UI, DALYs_rate_Male_2023_95UI, DALYs_rate_Male_change,
         DALYs_num_Female_1990_95UI, DALYs_num_Female_2023_95UI, DALYs_num_Female_change,
         DALYs_rate_Female_1990_95UI, DALYs_rate_Female_2023_95UI, DALYs_rate_Female_change)
#################################################allrisks
data0 <- data0 %>% 
  mutate(
    Deaths_rate_Both_change = round(((Deaths_Both_Rate_2023 - Deaths_Both_Rate_1990) / Deaths_Both_Rate_1990) * 100, 2),
    DALYs_rate_Both_change = round(((DALYs_Both_Rate_2023 - DALYs_Both_Rate_1990) / DALYs_Both_Rate_1990) * 100, 2),
  ) %>% 
  select(location_name,rei_name,Deaths_Both_Rate_2023,Deaths_rate_Both_change,DALYs_Both_Rate_2023,DALYs_rate_Both_change)
allrisks <- filter(data0,rei_name=="Dietary risks")

write.csv(allrisks,"allrisks_international.csv")
#################################################every risk factor
every_riskfac <- data0 %>% 
  filter(!(rei_name=="Dietary risks"))

write.csv(every_riskfac,"every_riskfac_international.csv")

############################################################国内国际对比(分年龄组)####
compare <- read.csv("国际.csv")
compare$measure_name[compare$measure_name=="DALYs (Disability-Adjusted Life Years)"] <- "DALYs"
compare <- compare %>% 
  select(measure_name,location_name,sex_name,age_name,rei_name,metric_name,year,val,upper,lower) %>% 
  filter(location_name %in% c("Global",
                              "China",
                              "Brazil", "Russia", "India", "South Africa", 
                              "Argentina", "Australia", "Canada", "France", "Germany",  "Indonesia", 
                              "Italy", "Japan", "Mexico",  "Saudi Arabia", "Turkey",
                              "United Kingdom", "United States",
                              "South Korea", "Mongolia", "North Korea")) %>% 
  filter(metric_name%in%c("Number","Rate")) %>% 
  filter(age_name %in% c("15-49 years","50-74 years","75-84 years","85+ years"))

compare$rei_name <- factor(compare$rei_name,levels = c("Dietary risks","Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", "Diet low in polyunsaturated fatty acids",
                                                       "Diet low in nuts and seeds", "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids",
                                                       "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", "Diet low in vegetables", "Diet low in calcium",
                                                       "Diet high in sugar-sweetened beverages", "Diet high in trans fatty acids"))

val_wide <- dcast(compare, location_name+age_name+rei_name ~ 
                    measure_name+sex_name+metric_name+year,value.var = "val")

upper_wide <- dcast(compare, location_name+age_name+rei_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "upper")
colnames(upper_wide)[4:27] <- paste(colnames(upper_wide)[4:27],"_upper",sep = "")

lower_wide <- dcast(compare, location_name+age_name+rei_name ~ 
                      measure_name+sex_name+metric_name+year, value.var = "lower")
colnames(lower_wide)[4:27] <- paste(colnames(lower_wide)[4:27],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","age_name","rei_name")),
              lower_wide, by = c("location_name","age_name","rei_name"))

data0 <- data0[,c("location_name","age_name","rei_name",
                  "Deaths_Both_Number_1990","Deaths_Both_Number_1990_lower","Deaths_Both_Number_1990_upper",
                  "Deaths_Both_Number_2023","Deaths_Both_Number_2023_lower","Deaths_Both_Number_2023_upper",
                  "Deaths_Both_Rate_1990","Deaths_Both_Rate_1990_lower","Deaths_Both_Rate_1990_upper",
                  "Deaths_Both_Rate_2023","Deaths_Both_Rate_2023_lower","Deaths_Both_Rate_2023_upper",
                  
                  "Deaths_Male_Number_1990","Deaths_Male_Number_1990_lower","Deaths_Male_Number_1990_upper",
                  "Deaths_Male_Number_2023","Deaths_Male_Number_2023_lower","Deaths_Male_Number_2023_upper",
                  "Deaths_Male_Rate_1990","Deaths_Male_Rate_1990_lower","Deaths_Male_Rate_1990_upper",
                  "Deaths_Male_Rate_2023","Deaths_Male_Rate_2023_lower","Deaths_Male_Rate_2023_upper",
                  
                  "Deaths_Female_Number_1990","Deaths_Female_Number_1990_lower","Deaths_Female_Number_1990_upper",
                  "Deaths_Female_Number_2023","Deaths_Female_Number_2023_lower","Deaths_Female_Number_2023_upper",
                  "Deaths_Female_Rate_1990","Deaths_Female_Rate_1990_lower","Deaths_Female_Rate_1990_upper",
                  "Deaths_Female_Rate_2023","Deaths_Female_Rate_2023_lower","Deaths_Female_Rate_2023_upper",
                  
                  
                  "DALYs_Both_Number_1990","DALYs_Both_Number_1990_lower","DALYs_Both_Number_1990_upper",
                  "DALYs_Both_Number_2023","DALYs_Both_Number_2023_lower","DALYs_Both_Number_2023_upper",
                  "DALYs_Both_Rate_1990","DALYs_Both_Rate_1990_lower","DALYs_Both_Rate_1990_upper",
                  "DALYs_Both_Rate_2023","DALYs_Both_Rate_2023_lower","DALYs_Both_Rate_2023_upper",
                  
                  "DALYs_Male_Number_1990","DALYs_Male_Number_1990_lower","DALYs_Male_Number_1990_upper",
                  "DALYs_Male_Number_2023","DALYs_Male_Number_2023_lower","DALYs_Male_Number_2023_upper",
                  "DALYs_Male_Rate_1990","DALYs_Male_Rate_1990_lower","DALYs_Male_Rate_1990_upper",
                  "DALYs_Male_Rate_2023","DALYs_Male_Rate_2023_lower","DALYs_Male_Rate_2023_upper",
                  
                  "DALYs_Female_Number_1990","DALYs_Female_Number_1990_lower","DALYs_Female_Number_1990_upper",
                  "DALYs_Female_Number_2023","DALYs_Female_Number_2023_lower","DALYs_Female_Number_2023_upper",
                  "DALYs_Female_Rate_1990","DALYs_Female_Rate_1990_lower","DALYs_Female_Rate_1990_upper",
                  "DALYs_Female_Rate_2023","DALYs_Female_Rate_2023_lower","DALYs_Female_Rate_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("Global",
                                         "China",
                                         "Brazil", "Russia", "India", "South Africa", 
                                         "Argentina", "Australia", "Canada", "France", "Germany",  "Indonesia", 
                                         "Italy", "Japan", "Mexico",  "Saudi Arabia",  "Turkey",
                                         "United Kingdom", "United States",
                                         "South Korea", "Mongolia", "North Korea"))
data0 <- data0 %>% 
  arrange(location_name,rei_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:2)] <- lapply(data0[-(1:2)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_num_Both_change = round(((Deaths_Both_Number_2023 - Deaths_Both_Number_1990) / Deaths_Both_Number_1990) * 100, 2),
    Deaths_rate_Both_change = round(((Deaths_Both_Rate_2023 - Deaths_Both_Rate_1990) / Deaths_Both_Rate_1990) * 100, 2),
    Deaths_num_Male_change = round(((Deaths_Male_Number_2023 - Deaths_Male_Number_1990) / Deaths_Male_Number_1990) * 100, 2),
    Deaths_rate_Male_change = round(((Deaths_Male_Rate_2023 - Deaths_Male_Rate_1990) / Deaths_Male_Rate_1990) * 100, 2),
    Deaths_num_Female_change = round(((Deaths_Female_Number_2023 - Deaths_Female_Number_1990) / Deaths_Female_Number_1990) * 100, 2),
    Deaths_rate_Female_change = round(((Deaths_Female_Rate_2023 - Deaths_Female_Rate_1990) / Deaths_Female_Rate_1990) * 100, 2),
    
    DALYs_num_Both_change = round(((DALYs_Both_Number_2023 - DALYs_Both_Number_1990) / DALYs_Both_Number_1990) * 100, 2),
    DALYs_rate_Both_change = round(((DALYs_Both_Rate_2023 - DALYs_Both_Rate_1990) / DALYs_Both_Rate_1990) * 100, 2),
    DALYs_num_Male_change = round(((DALYs_Male_Number_2023 - DALYs_Male_Number_1990) / DALYs_Male_Number_1990) * 100, 2),
    DALYs_rate_Male_change = round(((DALYs_Male_Rate_2023 - DALYs_Male_Rate_1990) / DALYs_Male_Rate_1990) * 100, 2),
    DALYs_num_Female_change = round(((DALYs_Female_Number_2023 - DALYs_Female_Number_1990) / DALYs_Female_Number_1990) * 100, 2),
    DALYs_rate_Female_change = round(((DALYs_Female_Rate_2023 - DALYs_Female_Rate_1990) / DALYs_Female_Rate_1990) * 100, 2)
  ) %>% 
  mutate(Deaths_num_Both_1990_95UI=paste(Deaths_Both_Number_1990,"(",Deaths_Both_Number_1990_lower,",",Deaths_Both_Number_1990_upper,")"),
         Deaths_num_Both_2023_95UI=paste(Deaths_Both_Number_2023,"(",Deaths_Both_Number_2023_lower,",",Deaths_Both_Number_2023_upper,")"),
         Deaths_rate_Both_1990_95UI=paste(Deaths_Both_Rate_1990,"(",Deaths_Both_Rate_1990_lower,",",Deaths_Both_Rate_1990_upper,")"),
         Deaths_rate_Both_2023_95UI=paste(Deaths_Both_Rate_2023,"(",Deaths_Both_Rate_2023_lower,",",Deaths_Both_Rate_2023_upper,")"),
         
         Deaths_num_Male_1990_95UI=paste(Deaths_Male_Number_1990,"(",Deaths_Male_Number_1990_lower,",",Deaths_Male_Number_1990_upper,")"),
         Deaths_num_Male_2023_95UI=paste(Deaths_Male_Number_2023,"(",Deaths_Male_Number_2023_lower,",",Deaths_Male_Number_2023_upper,")"),
         Deaths_rate_Male_1990_95UI=paste(Deaths_Male_Rate_1990,"(",Deaths_Male_Rate_1990_lower,",",Deaths_Male_Rate_1990_upper,")"),
         Deaths_rate_Male_2023_95UI=paste(Deaths_Male_Rate_2023,"(",Deaths_Male_Rate_2023_lower,",",Deaths_Male_Rate_2023_upper,")"),
         
         Deaths_num_Female_1990_95UI=paste(Deaths_Female_Number_1990,"(",Deaths_Female_Number_1990_lower,",",Deaths_Female_Number_1990_upper,")"),
         Deaths_num_Female_2023_95UI=paste(Deaths_Female_Number_2023,"(",Deaths_Female_Number_2023_lower,",",Deaths_Female_Number_2023_upper,")"),
         Deaths_rate_Female_1990_95UI=paste(Deaths_Female_Rate_1990,"(",Deaths_Female_Rate_1990_lower,",",Deaths_Female_Rate_1990_upper,")"),
         Deaths_rate_Female_2023_95UI=paste(Deaths_Female_Rate_2023,"(",Deaths_Female_Rate_2023_lower,",",Deaths_Female_Rate_2023_upper,")"),
         
         DALYs_num_Both_1990_95UI=paste(DALYs_Both_Number_1990,"(",DALYs_Both_Number_1990_lower,",",DALYs_Both_Number_1990_upper,")"),
         DALYs_num_Both_2023_95UI=paste(DALYs_Both_Number_2023,"(",DALYs_Both_Number_2023_lower,",",DALYs_Both_Number_2023_upper,")"),
         DALYs_rate_Both_1990_95UI=paste(DALYs_Both_Rate_1990,"(",DALYs_Both_Rate_1990_lower,",",DALYs_Both_Rate_1990_upper,")"),
         DALYs_rate_Both_2023_95UI=paste(DALYs_Both_Rate_2023,"(",DALYs_Both_Rate_2023_lower,",",DALYs_Both_Rate_2023_upper,")"),
         
         DALYs_num_Male_1990_95UI=paste(DALYs_Male_Number_1990,"(",DALYs_Male_Number_1990_lower,",",DALYs_Male_Number_1990_upper,")"),
         DALYs_num_Male_2023_95UI=paste(DALYs_Male_Number_2023,"(",DALYs_Male_Number_2023_lower,",",DALYs_Male_Number_2023_upper,")"),
         DALYs_rate_Male_1990_95UI=paste(DALYs_Male_Rate_1990,"(",DALYs_Male_Rate_1990_lower,",",DALYs_Male_Rate_1990_upper,")"),
         DALYs_rate_Male_2023_95UI=paste(DALYs_Male_Rate_2023,"(",DALYs_Male_Rate_2023_lower,",",DALYs_Male_Rate_2023_upper,")"),
         
         DALYs_num_Female_1990_95UI=paste(DALYs_Female_Number_1990,"(",DALYs_Female_Number_1990_lower,",",DALYs_Female_Number_1990_upper,")"),
         DALYs_num_Female_2023_95UI=paste(DALYs_Female_Number_2023,"(",DALYs_Female_Number_2023_lower,",",DALYs_Female_Number_2023_upper,")"),
         DALYs_rate_Female_1990_95UI=paste(DALYs_Female_Rate_1990,"(",DALYs_Female_Rate_1990_lower,",",DALYs_Female_Rate_1990_upper,")"),
         DALYs_rate_Female_2023_95UI=paste(DALYs_Female_Rate_2023,"(",DALYs_Female_Rate_2023_lower,",",DALYs_Female_Rate_2023_upper,")")) %>% 
  select(location_name,age_name,rei_name,
         Deaths_num_Both_1990_95UI, Deaths_num_Both_2023_95UI, Deaths_num_Both_change,
         Deaths_rate_Both_1990_95UI, Deaths_rate_Both_2023_95UI, Deaths_rate_Both_change,
         Deaths_num_Male_1990_95UI, Deaths_num_Male_2023_95UI, Deaths_num_Male_change,
         Deaths_rate_Male_1990_95UI, Deaths_rate_Male_2023_95UI, Deaths_rate_Male_change,
         Deaths_num_Female_1990_95UI, Deaths_num_Female_2023_95UI, Deaths_num_Female_change,
         Deaths_rate_Female_1990_95UI, Deaths_rate_Female_2023_95UI, Deaths_rate_Female_change,
         DALYs_num_Both_1990_95UI, DALYs_num_Both_2023_95UI, DALYs_num_Both_change,
         DALYs_rate_Both_1990_95UI, DALYs_rate_Both_2023_95UI, DALYs_rate_Both_change,
         DALYs_num_Male_1990_95UI, DALYs_num_Male_2023_95UI, DALYs_num_Male_change,
         DALYs_rate_Male_1990_95UI, DALYs_rate_Male_2023_95UI, DALYs_rate_Male_change,
         DALYs_num_Female_1990_95UI, DALYs_num_Female_2023_95UI, DALYs_num_Female_change,
         DALYs_rate_Female_1990_95UI, DALYs_rate_Female_2023_95UI, DALYs_rate_Female_change)
#################################################allrisks
allrisks <- filter(data0,rei_name=="Dietary risks")
allrisks$age_name <- factor(allrisks$age_name,levels = c("15-49 years","50-74 years","75-84 years","85+ years"))
allrisks <- allrisks %>% 
  group_by(age_name) %>% 
  arrange(age_name,location_name) %>% 
  ungroup()

write.csv(allrisks,"allrisks_international_agegroup.csv")
#################################################every risk factor
every_riskfac <- data0 %>% 
  filter(!(rei_name=="Dietary risks"))
every_riskfac$age_name <- factor(every_riskfac$age_name,levels = c("15-49 years","50-74 years","75-84 years","85+ years"))
every_riskfac <- every_riskfac %>% 
  group_by(rei_name,age_name) %>% 
  arrange(rei_name,age_name,location_name) %>% 
  ungroup()

write.csv(every_riskfac,"every_riskfac_international_agegroup.csv")
############################################################proportion 国内国际对比(全年龄组和分年龄组)####
compare <- read.csv("国际.csv")
compare$measure_name[compare$measure_name=="DALYs (Disability-Adjusted Life Years)"] <- "DALYs"
compare <- compare %>% 
  select(measure_name,location_name,rei_name,sex_name,age_name,metric_name,year,val,upper,lower) %>% 
  filter(metric_name == "Percent") %>% 
  filter(year %in% c(1990,2023)) %>% 
  filter(age_name %in%c("All ages","15-49 years","50-74 years","75-84 years","85+ years")) %>% 
  mutate(val=val*100,
         upper=upper*100,
         lower=lower*100) %>% 
  filter(location_name %in% c("Global",
                              "China",
                              "Brazil", "Russia", "India", "South Africa", 
                              "Argentina", "Australia", "Canada", "France", "Germany",  "Indonesia", 
                              "Italy", "Japan", "Mexico",  "Saudi Arabia", "Turkey",
                              "United Kingdom", "United States",
                              "South Korea", "Mongolia", "North Korea"))
compare$age_name <- factor(compare$age_name,levels = c("All ages","15-49 years","50-74 years","75-84 years","85+ years"))

compare$rei_name <- factor(compare$rei_name,levels = c("Dietary risks","Diet high in sodium", "Diet low in whole grains", "Diet low in fruits", "Diet low in polyunsaturated fatty acids",
                                                       "Diet low in nuts and seeds", "Diet low in fiber", "Diet high in red meat", "Diet low in seafood omega-3 fatty acids",
                                                       "Diet low in legumes", "Diet low in milk", "Diet high in processed meat", "Diet low in vegetables", "Diet low in calcium",
                                                       "Diet high in sugar-sweetened beverages", "Diet high in trans fatty acids"))
val_wide <- dcast(compare, location_name+age_name+rei_name ~ 
                    measure_name+sex_name+year,value.var = "val")

upper_wide <- dcast(compare, location_name+age_name+rei_name ~ 
                      measure_name+sex_name+year, value.var = "upper")
colnames(upper_wide)[4:15] <- paste(colnames(upper_wide)[4:15],"_upper",sep = "")

lower_wide <- dcast(compare, location_name+age_name+rei_name ~ 
                      measure_name+sex_name+year, value.var = "lower")
colnames(lower_wide)[4:15] <- paste(colnames(lower_wide)[4:15],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("location_name","age_name","rei_name")),
              lower_wide, by = c("location_name","age_name","rei_name"))

data0 <- data0[,c("location_name","age_name","rei_name",
                  "Deaths_Both_2023","Deaths_Both_2023_lower","Deaths_Both_2023_upper",
                  "Deaths_Both_1990","Deaths_Both_1990_lower","Deaths_Both_1990_upper",
                  
                  "Deaths_Male_2023","Deaths_Male_2023_lower","Deaths_Male_2023_upper",
                  "Deaths_Male_1990","Deaths_Male_1990_lower","Deaths_Male_1990_upper",
                  
                  "Deaths_Female_1990","Deaths_Female_1990_lower","Deaths_Female_1990_upper",
                  "Deaths_Female_2023","Deaths_Female_2023_lower","Deaths_Female_2023_upper",
                  
                  "DALYs_Both_1990","DALYs_Both_1990_lower","DALYs_Both_1990_upper",
                  "DALYs_Both_2023","DALYs_Both_2023_lower","DALYs_Both_2023_upper",
                  
                  "DALYs_Male_1990","DALYs_Male_1990_lower","DALYs_Male_1990_upper",
                  "DALYs_Male_2023","DALYs_Male_2023_lower","DALYs_Male_2023_upper",
                  
                  "DALYs_Female_1990","DALYs_Female_1990_lower","DALYs_Female_1990_upper",
                  "DALYs_Female_2023","DALYs_Female_2023_lower","DALYs_Female_2023_upper"
)]
data0$location_name <- factor(data0$location_name, 
                              levels = c("Global",
                                         "China",
                                         "Brazil", "Russia", "India", "South Africa", 
                                         "Argentina", "Australia", "Canada", "France", "Germany",  "Indonesia", 
                                         "Italy", "Japan", "Mexico",  "Saudi Arabia", "Turkey",
                                         "United Kingdom", "United States",
                                         "South Korea", "Mongolia", "North Korea"))
data0 <- data0 %>% 
  arrange(location_name,age_name,rei_name)
# Apply rounding to all numeric columns except the first one
data0[-(1:3)] <- lapply(data0[-(1:3)], function(x) if(is.numeric(x)) round(x, 2) else x)

data0 <- data0 %>% 
  mutate(
    Deaths_Both_change = round(((Deaths_Both_2023 - Deaths_Both_1990) / Deaths_Both_1990) * 100, 2),
    Deaths_Male_change = round(((Deaths_Male_2023 - Deaths_Male_1990) / Deaths_Male_1990) * 100, 2),
    Deaths_Female_change = round(((Deaths_Female_2023 - Deaths_Female_1990) / Deaths_Female_1990) * 100, 2),
    
    DALYs_Both_change = round(((DALYs_Both_2023 - DALYs_Both_1990) / DALYs_Both_1990) * 100, 2),
    DALYs_Male_change = round(((DALYs_Male_2023 - DALYs_Male_1990) / DALYs_Male_1990) * 100, 2),
    DALYs_Female_change = round(((DALYs_Female_2023 - DALYs_Female_1990) / DALYs_Female_1990) * 100, 2),
  ) %>% 
  mutate(Deaths_Both_1990_95UI=paste(Deaths_Both_1990,"(",Deaths_Both_1990_lower,",",Deaths_Both_1990_upper,")"),
         Deaths_Both_2023_95UI=paste(Deaths_Both_2023,"(",Deaths_Both_2023_lower,",",Deaths_Both_2023_upper,")"),
         
         Deaths_Male_1990_95UI=paste(Deaths_Male_1990,"(",Deaths_Male_1990_lower,",",Deaths_Male_1990_upper,")"),
         Deaths_Male_2023_95UI=paste(Deaths_Male_2023,"(",Deaths_Male_2023_lower,",",Deaths_Male_2023_upper,")"),
         
         Deaths_Female_1990_95UI=paste(Deaths_Female_1990,"(",Deaths_Female_1990_lower,",",Deaths_Female_1990_upper,")"),
         Deaths_Female_2023_95UI=paste(Deaths_Female_2023,"(",Deaths_Female_2023_lower,",",Deaths_Female_2023_upper,")"),
         
         DALYs_Both_1990_95UI=paste(DALYs_Both_1990,"(",DALYs_Both_1990_lower,",",DALYs_Both_1990_upper,")"),
         DALYs_Both_2023_95UI=paste(DALYs_Both_2023,"(",DALYs_Both_2023_lower,",",DALYs_Both_2023_upper,")"),
         
         DALYs_Male_1990_95UI=paste(DALYs_Male_1990,"(",DALYs_Male_1990_lower,",",DALYs_Male_1990_upper,")"),
         DALYs_Male_2023_95UI=paste(DALYs_Male_2023,"(",DALYs_Male_2023_lower,",",DALYs_Male_2023_upper,")"),
         
         DALYs_Female_1990_95UI=paste(DALYs_Female_1990,"(",DALYs_Female_1990_lower,",",DALYs_Female_1990_upper,")"),
         DALYs_Female_2023_95UI=paste(DALYs_Female_2023,"(",DALYs_Female_2023_lower,",",DALYs_Female_2023_upper,")")) %>% 
  select(location_name,rei_name,age_name,
         Deaths_Both_1990_95UI, Deaths_Both_2023_95UI, Deaths_Both_change,
         Deaths_Male_1990_95UI, Deaths_Male_2023_95UI, Deaths_Male_change,
         Deaths_Female_1990_95UI, Deaths_Female_2023_95UI, Deaths_Female_change,
         DALYs_Both_1990_95UI, DALYs_Both_2023_95UI, DALYs_Both_change,
         DALYs_Male_1990_95UI, DALYs_Male_2023_95UI, DALYs_Male_change,
         DALYs_Female_1990_95UI, DALYs_Female_2023_95UI, DALYs_Female_change)

#############################allrisks
allrisks <- filter(data0,rei_name=="Dietary risks")
allrisks <- allrisks %>% 
  group_by(age_name) %>% 
  arrange(age_name,rei_name) %>% 
  ungroup()
write.csv(allrisks,file = "allrisks_international_proportion.csv")
#############################everyrisks
every_riskfac <- filter(data0,!(rei_name=="Dietary risks"))
every_riskfac <- every_riskfac %>% 
  group_by(age_name,rei_name) %>% 
  arrange(age_name,rei_name,location_name) %>% 
  ungroup()
write.csv(every_riskfac,file = "every_riskfac_international_proportion.csv")
##################################################level 3####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&
                          dietary_annual1.0$cause_name %in% c("Diabetes mellitus",
                                                              "Chronic kidney disease",
                                                              "Ischemic heart disease",
                                                              "Aortic aneurysm",
                                                              "Lower extremity peripheral arterial disease",
                                                              "Hypertensive heart disease",
                                                              "Stroke",
                                                              "Atrial fibrillation and flutter",
                                                              "Tracheal, bronchus, and lung cancer",
                                                              "Breast cancer",
                                                              "Prostate cancer",
                                                              "Colon and rectum cancer",
                                                              "Esophageal cancer",
                                                              "Stomach cancer"))

########################death####
### rate及percentage change
death_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="2023"&
                          allrisk_cause$location_name=="China"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="All ages")
death_cn_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="1990"&
                          allrisk_cause$location_name=="China"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="All ages")

death_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="2023"&
                          allrisk_cause$location_name=="China"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="Age-standardized")
death_cn_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="1990"&
                          allrisk_cause$location_name=="China"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="Age-standardized")



death_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="2023"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="All ages")

death_cn_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="1990"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="All ages")
death_cn_2023_1 <- death_cn_2023 %>%
  select(location_name,val,lower,upper) %>%
  mutate(death_allages_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>%
  select(location_name,death_allages_2023)

death_cn_1990_1 <- death_cn_1990 %>%
  select(location_name,val,lower,upper) %>%
  mutate(death_allages_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>%
  select(location_name,death_allages_1990)

death <- left_join(death_cn_2023_1,death_cn_1990_1,by="location_name")
death <- death %>%
  select(location_name,death_allages_1990,death_allages_2023)


death_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="2023"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="Age-standardized")
death_cn_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="1990"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="Age-standardized")
death_cn_2023_1 <- death_cn_2023 %>%
  select(location_name,val,lower,upper) %>%
  mutate(death_Age_standardised_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>%
  select(location_name,death_Age_standardised_2023)

death_cn_1990_1 <- death_cn_1990 %>%
  select(location_name,val,lower,upper) %>%
  mutate(death_Age_standardised_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>%
  select(location_name,death_Age_standardised_1990)

death_Age_standardised <- left_join(death_cn_2023_1,death_cn_1990_1,by="location_name")
diet_related_NCDs_death <- left_join(death,death_Age_standardised,by="location_name")
diet_related_NCDs_death$location_name <- factor(diet_related_NCDs_death$location_name,levels = c('China',
                                                                                                 'Beijing',
                                                                                                 'Tianjin',
                                                                                                 'Hebei',
                                                                                                 'Shanxi',
                                                                                                 'Inner Mongolia',
                                                                                                 'Liaoning',
                                                                                                 'Jilin',
                                                                                                 'Heilongjiang',
                                                                                                 'Shanghai',
                                                                                                 'Jiangsu',
                                                                                                 'Zhejiang',
                                                                                                 'Anhui',
                                                                                                 'Fujian',
                                                                                                 'Jiangxi',
                                                                                                 'Shandong',
                                                                                                 'Henan',
                                                                                                 'Hubei',
                                                                                                 'Hunan',
                                                                                                 'Guangdong',
                                                                                                 'Guangxi',
                                                                                                 'Hainan',
                                                                                                 'Chongqing',
                                                                                                 'Sichuan',
                                                                                                 'Guizhou',
                                                                                                 'Yunnan',
                                                                                                 'Tibet',
                                                                                                 'Shaanxi',
                                                                                                 'Gansu',
                                                                                                 'Qinghai',
                                                                                                 'Ningxia',
                                                                                                 'Xinjiang',
                                                                                                 'Hong Kong Special Administrative Region of China',
                                                                                                 'Macao Special Administrative Region of China'))
diet_related_NCDs_death <- diet_related_NCDs_death %>% 
  arrange(location_name)

write.csv(diet_related_NCDs_DALY,file = "diet_related_NCDs_DALY.csv")
write.csv(diet_related_NCDs_death,file = "diet_related_NCDs_death.csv")

#allages rate
death_cn_2023_1 <- death_cn_2023 %>% 
  select(cause_name,val,lower,upper) %>% 
  arrange(desc(val)) %>% 
  mutate(rank_2023 = row_number()) %>% 
  mutate(death_allages_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,rank_2023,death_allages_2023)

death_cn_1990_1 <- death_cn_1990 %>% 
  select(cause_name,val,lower,upper) %>% 
  arrange(desc(val)) %>% 
  mutate(rank_1990 = row_number()) %>% 
  mutate(death_allages_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,rank_1990,death_allages_1990)

death <- left_join(death_cn_2023_1,death_cn_1990_1,by="cause_name")
death <- death %>% 
  select(cause_name,rank_1990,rank_2023,death_allages_1990,death_allages_2023)
#Age standardised rate
death_cn_2023_1 <- death_cn_2023 %>% 
  select(cause_name,val,lower,upper) %>%
  mutate(death_Age_standardised_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,death_Age_standardised_2023)

death_cn_1990_1 <- death_cn_1990 %>% 
  select(cause_name,val,lower,upper) %>% 
  mutate(death_Age_standardised_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,death_Age_standardised_1990)

death_Age_standardised <- left_join(death_cn_2023_1,death_cn_1990_1,by="cause_name")
death_Age_standardised <- death_Age_standardised %>% 
  select(cause_name,death_Age_standardised_1990,death_Age_standardised_2023)

#all ages percentage
death_cn_2023_2 <- death_cn_2023 %>% 
  select(cause_name,val) %>% 
  rename(death_2023= val)

death_cn_1990_2 <- death_cn_1990 %>% 
  select(cause_name,val) %>% 
  rename(death_1990=val)

death_per <- merge(death_cn_1990_2,death_cn_2023_2,by="cause_name")
death_per_allages <- death_per %>% 
  mutate(death_per_change_allages=round(((death_2023-death_1990)/death_1990)*100,digits = 2)) %>% 
  select(cause_name,death_per_change_allages)
#Age standardised percentage
death_cn_2023_2 <- death_cn_2023 %>% 
  select(cause_name,val) %>% 
  rename(death_2023= val)

death_cn_1990_2 <- death_cn_1990 %>% 
  select(cause_name,val) %>% 
  rename(death_1990=val)

death_per <- merge(death_cn_1990_2,death_cn_2023_2,by="cause_name")
death_per_Agestand <- death_per %>% 
  mutate(death_per_change_Agestand=round(((death_2023-death_1990)/death_1990)*100,digits = 2)) %>% 
  select(cause_name,death_per_change_Agestand)

death_final <- death %>% 
  left_join(death_Age_standardised, by="cause_name") %>% 
  left_join(death_per_allages, by="cause_name") %>% 
  left_join(death_per_Agestand, by="cause_name")
write.csv(death_final,file="death.csv")
####allages 线图
death_cn_allages <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                             allrisk_cause$location_name=="China"&
                             allrisk_cause$sex_name=="Both"&
                             allrisk_cause$metric_name=="Rate"&
                             allrisk_cause$age_name=="All ages")

plot_death_allages <- ggplot(death_cn_allages, aes(x = year, y = val, group = cause_name, colour = cause_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "All Ages Rate of Death from 1990-2023 in China",
       x = "Year",
       y = "All Ages Death Rate (per 100,000 population)") +
  scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
                                "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
                                "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
                     name = "Cause of Death") +
  scale_y_continuous(limits = c(0, 60)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial"),  
        axis.title.y = element_text(size = 20, family = "Arial"),  
        axis.text = element_text(size = 20, family = "Arial"),
        plot.title = element_text(size = 20, family = "Arial"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial"),
        legend.title = element_text(size = 20, family = "Arial"),
        legend.text = element_text(size = 20, family = "Arial")) +
  geom_point()
ggsave(plot_death_allages,file="plot_death_allages.tif",width = 20,height = 10,dpi = 300)
####Age standardised 线图
death_cn_Age_standardised <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                                      allrisk_cause$location_name=="China"&
                                      allrisk_cause$sex_name=="Both"&
                                      allrisk_cause$metric_name=="Rate"&
                                      allrisk_cause$age_name=="Age-standardized")
plot_death_Age_standardised <- ggplot(death_cn_Age_standardised, aes(x = year, y = val, group = cause_name, colour = cause_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "Age-standardized Rate of Death from 1990-2023 in China",
       x = "Year",
       y = "Age-standardized Death Rate (per 100,000 population)") +
  scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
                                "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
                                "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
                     name = "Cause of Death") +
  scale_y_continuous(limits = c(0, 60)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  geom_point()

ggsave(plot_death_Age_standardised,file="plot_death_Age_standardised.tif",width = 20,height = 10,dpi = 300)
########################DALY####
DALY_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                         allrisk_cause$year=="2023"&
                         allrisk_cause$location_name=="China"&
                         allrisk_cause$sex_name=="Both"&
                         allrisk_cause$metric_name=="Rate"&
                         allrisk_cause$age_name=="All ages")
DALY_cn_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                         allrisk_cause$year=="1990"&
                         allrisk_cause$location_name=="China"&
                         allrisk_cause$sex_name=="Both"&
                         allrisk_cause$metric_name=="Rate"&
                         allrisk_cause$age_name=="All ages")

DALY_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                         allrisk_cause$year=="2023"&
                         allrisk_cause$location_name=="China"&
                         allrisk_cause$sex_name=="Both"&
                         allrisk_cause$metric_name=="Rate"&
                         allrisk_cause$age_name=="Age-standardized")
DALY_cn_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                         allrisk_cause$year=="1990"&
                         allrisk_cause$location_name=="China"&
                         allrisk_cause$sex_name=="Both"&
                         allrisk_cause$metric_name=="Rate"&
                         allrisk_cause$age_name=="Age-standardized")
#allages rate
DALY_cn_2023_1 <- DALY_cn_2023 %>% 
  select(cause_name,val,lower,upper) %>% 
  arrange(desc(val)) %>% 
  mutate(rank_2023 = row_number()) %>% 
  mutate(DALY_allages_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,rank_2023,DALY_allages_2023)

DALY_cn_1990_1 <- DALY_cn_1990 %>% 
  select(cause_name,val,lower,upper) %>% 
  arrange(desc(val)) %>% 
  mutate(rank_1990 = row_number()) %>% 
  mutate(DALY_allages_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,rank_1990,DALY_allages_1990)

DALY <- left_join(DALY_cn_2023_1,DALY_cn_1990_1,by="cause_name")
DALY <- DALY %>% 
  select(cause_name,rank_1990,rank_2023,DALY_allages_1990,DALY_allages_2023)
#Age standardised rate
DALY_cn_2023_1 <- DALY_cn_2023 %>% 
  select(cause_name,val,lower,upper) %>%
  mutate(DALY_Age_standardised_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,DALY_Age_standardised_2023)

DALY_cn_1990_1 <- DALY_cn_1990 %>% 
  select(cause_name,val,lower,upper) %>% 
  mutate(DALY_Age_standardised_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,DALY_Age_standardised_1990)

DALY_Age_standardised <- left_join(DALY_cn_2023_1,DALY_cn_1990_1,by="cause_name")
DALY_Age_standardised <- DALY_Age_standardised %>% 
  select(cause_name,DALY_Age_standardised_1990,DALY_Age_standardised_2023)

#all ages percentage
DALY_cn_2023_2 <- DALY_cn_2023 %>% 
  select(cause_name,val) %>% 
  rename(DALY_2023= val)

DALY_cn_1990_2 <- DALY_cn_1990 %>% 
  select(cause_name,val) %>% 
  rename(DALY_1990=val)

DALY_per <- merge(DALY_cn_1990_2,DALY_cn_2023_2,by="cause_name")
DALY_per_allages <- DALY_per %>% 
  mutate(DALY_per_change_allages=round(((DALY_2023-DALY_1990)/DALY_1990)*100,digits = 2)) %>% 
  select(cause_name,DALY_per_change_allages)
#Age standardised percentage
DALY_cn_2023_2 <- DALY_cn_2023 %>% 
  select(cause_name,val) %>% 
  rename(DALY_2023= val)

DALY_cn_1990_2 <- DALY_cn_1990 %>% 
  select(cause_name,val) %>% 
  rename(DALY_1990=val)

DALY_per <- merge(DALY_cn_1990_2,DALY_cn_2023_2,by="cause_name")
DALY_per_Agestand <- DALY_per %>% 
  mutate(DALY_per_change_Agestand=round(((DALY_2023-DALY_1990)/DALY_1990)*100,digits = 2)) %>% 
  select(cause_name,DALY_per_change_Agestand)

DALY_final <- DALY %>% 
  left_join(DALY_Age_standardised, by="cause_name") %>% 
  left_join(DALY_per_allages, by="cause_name") %>% 
  left_join(DALY_per_Agestand, by="cause_name")
write.csv(DALY_final,file="DALY.csv")
####allages 线图
DALY_cn_allages <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                            allrisk_cause$location_name=="China"&
                            allrisk_cause$sex_name=="Both"&
                            allrisk_cause$metric_name=="Rate"&
                            allrisk_cause$age_name=="All ages")

plot_DALY_allages <- ggplot(DALY_cn_allages, aes(x = year, y = val, group = cause_name, colour = cause_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "All Ages Rate of DALYs from 1990-2023 in China",
       x = "Year",
       y = "All Ages DALYs Rate (per 100,000 population)") +
  scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
                                "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
                                "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
                     name = "Cause of DALY") +
  scale_y_continuous(limits = c(0, 1200),breaks = c(0,300,600,900,1200)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  geom_point()
ggsave(plot_DALY_allages,file="plot_DALY_allages.tif",width = 20,height = 10,dpi = 300)
####Age standardised 线图
DALY_cn_Age_standardised <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                                     allrisk_cause$location_name=="China"&
                                     allrisk_cause$sex_name=="Both"&
                                     allrisk_cause$metric_name=="Rate"&
                                     allrisk_cause$age_name=="Age-standardized")
plot_DALY_Age_standardised <- ggplot(DALY_cn_Age_standardised, aes(x = year, y = val, group = cause_name, colour = cause_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "Age-standardized Rate of DALYs from 1990-2023 in China",
       x = "Year",
       y = "Age-standardized DALYs Rate (per 100,000 population)") +
  scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
                                "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
                                "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
                     name = "Cause of DALY") +
  scale_y_continuous(limits = c(0, 1200),breaks = c(0,300,600,900,1200)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  geom_point()

ggsave(plot_DALY_Age_standardised,file="plot_DALY_Age_standardised.tif",width = 20,height = 10,dpi = 300)


##################################################level 4####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&
                          dietary_annual1.0$cause_name %in% c("Subarachnoid hemorrhage",    
                                                              "Ischemic stroke",                                            
                                                              "Diabetes mellitus type 2",    
                                                              "Intracerebral hemorrhage",                       
                                                              "Chronic kidney disease due to hypertension",                                   
                                                              "Chronic kidney disease due to other and unspecified causes",                                     
                                                              "Chronic kidney disease due to diabetes mellitus type 2",                                         
                                                              "Chronic kidney disease due to glomerulonephritis"))
########################death####
death_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="2023"&
                          allrisk_cause$location_name=="China"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="All ages")
death_cn_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="1990"&
                          allrisk_cause$location_name=="China"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="All ages")

death_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="2023"&
                          allrisk_cause$location_name=="China"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="Age-standardized")
death_cn_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="1990"&
                          allrisk_cause$location_name=="China"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="Age-standardized")
#allages rate
death_cn_2023_1 <- death_cn_2023 %>% 
  select(cause_name,val,lower,upper) %>% 
  arrange(desc(val)) %>% 
  mutate(rank_2023 = row_number()) %>% 
  mutate(death_allages_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,rank_2023,death_allages_2023)

death_cn_1990_1 <- death_cn_1990 %>% 
  select(cause_name,val,lower,upper) %>% 
  arrange(desc(val)) %>% 
  mutate(rank_1990 = row_number()) %>% 
  mutate(death_allages_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,rank_1990,death_allages_1990)

death <- left_join(death_cn_2023_1,death_cn_1990_1,by="cause_name")
death <- death %>% 
  select(cause_name,rank_1990,rank_2023,death_allages_1990,death_allages_2023)
#Age standardised rate
death_cn_2023_1 <- death_cn_2023 %>% 
  select(cause_name,val,lower,upper) %>%
  mutate(death_Age_standardised_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,death_Age_standardised_2023)

death_cn_1990_1 <- death_cn_1990 %>% 
  select(cause_name,val,lower,upper) %>% 
  mutate(death_Age_standardised_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,death_Age_standardised_1990)

death_Age_standardised <- left_join(death_cn_2023_1,death_cn_1990_1,by="cause_name")
death_Age_standardised <- death_Age_standardised %>% 
  select(cause_name,death_Age_standardised_1990,death_Age_standardised_2023)

#all ages percentage
death_cn_2023_2 <- death_cn_2023 %>% 
  select(cause_name,val) %>% 
  rename(death_2023= val)

death_cn_1990_2 <- death_cn_1990 %>% 
  select(cause_name,val) %>% 
  rename(death_1990=val)

death_per <- merge(death_cn_1990_2,death_cn_2023_2,by="cause_name")
death_per_allages <- death_per %>% 
  mutate(death_per_change_allages=round(((death_2023-death_1990)/death_1990)*100,digits = 2)) %>% 
  select(cause_name,death_per_change_allages)
#Age standardised percentage
death_cn_2023_2 <- death_cn_2023 %>% 
  select(cause_name,val) %>% 
  rename(death_2023= val)

death_cn_1990_2 <- death_cn_1990 %>% 
  select(cause_name,val) %>% 
  rename(death_1990=val)

death_per <- merge(death_cn_1990_2,death_cn_2023_2,by="cause_name")
death_per_Agestand <- death_per %>% 
  mutate(death_per_change_Agestand=round(((death_2023-death_1990)/death_1990)*100,digits = 2)) %>% 
  select(cause_name,death_per_change_Agestand)

death_final <- death %>% 
  left_join(death_Age_standardised, by="cause_name") %>% 
  left_join(death_per_allages, by="cause_name") %>% 
  left_join(death_per_Agestand, by="cause_name")
write.csv(death_final,file="death1.csv")
####allages 线图
death_cn_allages <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                             allrisk_cause$location_name=="China"&
                             allrisk_cause$sex_name=="Both"&
                             allrisk_cause$metric_name=="Rate"&
                             allrisk_cause$age_name=="All ages")

plot_death_allages <- ggplot(death_cn_allages, aes(x = year, y = val, group = cause_name, colour = cause_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "All Ages Rate of Death from 1990-2023 in China",
       x = "Year",
       y = "All Ages Death Rate (per 100,000 population)") +
  scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
                                "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
                                "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
                     name = "Cause of Death") +
  scale_y_continuous(limits = c(0, 20)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  geom_point()
ggsave(plot_death_allages,file="plot_death_allages_level4.tif",width = 20,height = 10,dpi = 300)
####Age standardised 线图
death_cn_Age_standardised <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                                      allrisk_cause$location_name=="China"&
                                      allrisk_cause$sex_name=="Both"&
                                      allrisk_cause$metric_name=="Rate"&
                                      allrisk_cause$age_name=="Age-standardized")
plot_death_Age_standardised <- ggplot(death_cn_Age_standardised, aes(x = year, y = val, group = cause_name, colour = cause_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "Age-standardized Rate of Death from 1990-2023 in China",
       x = "Year",
       y = "Age-standardized Death Rate (per 100,000 population)") +
  scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
                                "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
                                "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
                     name = "Cause of Death") +
  scale_y_continuous(limits = c(0, 30)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  geom_point()

ggsave(plot_death_Age_standardised,file="plot_death_Age_standardised_level4.tif",width = 20,height = 10,dpi = 300)


########################DALY####
DALY_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                         allrisk_cause$year=="2023"&
                         allrisk_cause$location_name=="China"&
                         allrisk_cause$sex_name=="Both"&
                         allrisk_cause$metric_name=="Rate"&
                         allrisk_cause$age_name=="All ages")
DALY_cn_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                         allrisk_cause$year=="1990"&
                         allrisk_cause$location_name=="China"&
                         allrisk_cause$sex_name=="Both"&
                         allrisk_cause$metric_name=="Rate"&
                         allrisk_cause$age_name=="All ages")

DALY_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                         allrisk_cause$year=="2023"&
                         allrisk_cause$location_name=="China"&
                         allrisk_cause$sex_name=="Both"&
                         allrisk_cause$metric_name=="Rate"&
                         allrisk_cause$age_name=="Age-standardized")
DALY_cn_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                         allrisk_cause$year=="1990"&
                         allrisk_cause$location_name=="China"&
                         allrisk_cause$sex_name=="Both"&
                         allrisk_cause$metric_name=="Rate"&
                         allrisk_cause$age_name=="Age-standardized")
#allages rate
DALY_cn_2023_1 <- DALY_cn_2023 %>% 
  select(cause_name,val,lower,upper) %>% 
  arrange(desc(val)) %>% 
  mutate(rank_2023 = row_number()) %>% 
  mutate(DALY_allages_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,rank_2023,DALY_allages_2023)

DALY_cn_1990_1 <- DALY_cn_1990 %>% 
  select(cause_name,val,lower,upper) %>% 
  arrange(desc(val)) %>% 
  mutate(rank_1990 = row_number()) %>% 
  mutate(DALY_allages_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,rank_1990,DALY_allages_1990)

DALY <- left_join(DALY_cn_2023_1,DALY_cn_1990_1,by="cause_name")
DALY <- DALY %>% 
  select(cause_name,rank_1990,rank_2023,DALY_allages_1990,DALY_allages_2023)
#Age standardised rate
DALY_cn_2023_1 <- DALY_cn_2023 %>% 
  select(cause_name,val,lower,upper) %>%
  mutate(DALY_Age_standardised_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,DALY_Age_standardised_2023)

DALY_cn_1990_1 <- DALY_cn_1990 %>% 
  select(cause_name,val,lower,upper) %>% 
  mutate(DALY_Age_standardised_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(cause_name,DALY_Age_standardised_1990)

DALY_Age_standardised <- left_join(DALY_cn_2023_1,DALY_cn_1990_1,by="cause_name")
DALY_Age_standardised <- DALY_Age_standardised %>% 
  select(cause_name,DALY_Age_standardised_1990,DALY_Age_standardised_2023)

#all ages percentage
DALY_cn_2023_2 <- DALY_cn_2023 %>% 
  select(cause_name,val) %>% 
  rename(DALY_2023= val)

DALY_cn_1990_2 <- DALY_cn_1990 %>% 
  select(cause_name,val) %>% 
  rename(DALY_1990=val)

DALY_per <- merge(DALY_cn_1990_2,DALY_cn_2023_2,by="cause_name")
DALY_per_allages <- DALY_per %>% 
  mutate(DALY_per_change_allages=round(((DALY_2023-DALY_1990)/DALY_1990)*100,digits = 2)) %>% 
  select(cause_name,DALY_per_change_allages)
#Age standardised percentage
DALY_cn_2023_2 <- DALY_cn_2023 %>% 
  select(cause_name,val) %>% 
  rename(DALY_2023= val)

DALY_cn_1990_2 <- DALY_cn_1990 %>% 
  select(cause_name,val) %>% 
  rename(DALY_1990=val)

DALY_per <- merge(DALY_cn_1990_2,DALY_cn_2023_2,by="cause_name")
DALY_per_Agestand <- DALY_per %>% 
  mutate(DALY_per_change_Agestand=round(((DALY_2023-DALY_1990)/DALY_1990)*100,digits = 2)) %>% 
  select(cause_name,DALY_per_change_Agestand)

DALY_final <- DALY %>% 
  left_join(DALY_Age_standardised, by="cause_name") %>% 
  left_join(DALY_per_allages, by="cause_name") %>% 
  left_join(DALY_per_Agestand, by="cause_name")
write.csv(DALY_final,file="DALY1.csv")

####allages 线图
DALY_cn_allages <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                            allrisk_cause$location_name=="China"&
                            allrisk_cause$sex_name=="Both"&
                            allrisk_cause$metric_name=="Rate"&
                            allrisk_cause$age_name=="All ages")

plot_DALY_allages <- ggplot(DALY_cn_allages, aes(x = year, y = val, group = cause_name, colour = cause_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "All Ages Rate of DALYs from 1990-2023 in China",
       x = "Year",
       y = "All Ages DALYs Rate (per 100,000 population)") +
  scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
                                "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
                                "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
                     name = "Cause of DALY") +
  scale_y_continuous(limits = c(0, 500)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  geom_point()
ggsave(plot_DALY_allages,file="plot_DALY_allages_level4.tif",width = 20,height = 10,dpi = 300)
####Age standardised 线图
DALY_cn_Age_standardised <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                                     allrisk_cause$location_name=="China"&
                                     allrisk_cause$sex_name=="Both"&
                                     allrisk_cause$metric_name=="Rate"&
                                     allrisk_cause$age_name=="Age-standardized")
plot_DALY_Age_standardised <- ggplot(DALY_cn_Age_standardised, aes(x = year, y = val, group = cause_name, colour = cause_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "Age-standardized Rate of DALYs from 1990-2023 in China",
       x = "Year",
       y = "Age-standardized DALYs Rate (per 100,000 population)") +
  scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
                                "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
                                "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
                     name = "Cause of DALY") +
  scale_y_continuous(limits =c(0,700) , breaks = seq(0, 700, by = 100)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  geom_point()

ggsave(plot_DALY_Age_standardised,file="plot_DALY_Age_standardised_level4.tif",width = 20,height = 10,dpi = 300)


##################################################level 3 and 4####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&
                          dietary_annual1.0$cause_name %in% c("Diabetes mellitus",
                                                              "Chronic kidney disease",
                                                              "Ischemic heart disease",
                                                              "Aortic aneurysm",
                                                              "Lower extremity peripheral arterial disease",
                                                              "Hypertensive heart disease",
                                                              "Stroke",
                                                              "Atrial fibrillation and flutter",
                                                              "Tracheal, bronchus, and lung cancer",
                                                              "Breast cancer",
                                                              "Prostate cancer",
                                                              "Colon and rectum cancer",
                                                              "Esophageal cancer",
                                                              "Stomach cancer",
                                                              "Subarachnoid hemorrhage",    
                                                              "Ischemic stroke",                                            
                                                              "Diabetes mellitus type 2",    
                                                              "Intracerebral hemorrhage",                       
                                                              "Chronic kidney disease due to hypertension",                                   
                                                              "Chronic kidney disease due to other and unspecified causes",                                     
                                                              "Chronic kidney disease due to diabetes mellitus type 2",                                         
                                                              "Chronic kidney disease due to glomerulonephritis"))
####################death
####allages 线图
death_cn_allages <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                             allrisk_cause$location_name=="China"&
                             allrisk_cause$sex_name=="Both"&
                             allrisk_cause$metric_name=="Rate"&
                             allrisk_cause$age_name=="All ages")

plot_death_allages <- ggplot(death_cn_allages, aes(x = year, y = val, group = cause_name, colour = cause_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "All Ages Death Rate from 1990-2023 in China",
       x = "Year",
       y = "All Ages Death Rate (per 100,000 population)") +
  scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
                                "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
                                "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                                "#D55E00", "#CC79A7", "#F0E", "#009", "#56B",
                                "#E69", "#0072B2", "#D55E00", "#CC79A7"),
                     name = "NCDs") +
  scale_y_continuous(limits = c(0, 60)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
        axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
        axis.text = element_text(size = 20, family = "Arial",color = "black"),
        plot.title = element_text(size = 20, family = "Arial",color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.title = element_text(size = 20, family = "Arial",color = "black"),
        legend.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.key.height = unit(1, 'cm')) +
  guides(colour = guide_legend(ncol = 1)) +
  geom_point()
ggsave(plot_death_allages,file="plot_death_allages_level34.tif",width = 20,height = 10,dpi = 300)
####Age standardised 线图
death_cn_Age_standardised <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                                      allrisk_cause$location_name=="China"&
                                      allrisk_cause$sex_name=="Both"&
                                      allrisk_cause$metric_name=="Rate"&
                                      allrisk_cause$age_name=="Age-standardized")
plot_death_Age_standardised <- ggplot(death_cn_Age_standardised, aes(x = year, y = val, group = cause_name, colour = cause_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "Age-standardized Death Rate from 1990-2023 in China",
       x = "Year",
       y = "Age-standardized Death Rate (per 100,000 population)") +
  scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
                                "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
                                "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                                "#D55E00", "#CC79A7", "#F0E", "#009", "#56B",
                                "#E69", "#0072B2", "#D55E00", "#CC79A7"),
                     name = "NCDs") +
  scale_y_continuous(limits = c(0, 60)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
        axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
        axis.text = element_text(size = 20, family = "Arial",color = "black"),
        plot.title = element_text(size = 20, family = "Arial",color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.title = element_text(size = 20, family = "Arial",color = "black"),
        legend.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.key.height = unit(1, 'cm')) +
  guides(colour = guide_legend(ncol = 1)) +
  geom_point()

ggsave(plot_death_Age_standardised,file="plot_death_Age_standardised_level34.tif",width = 20,height = 10,dpi = 300)
#######################DALY
####allages 线图
DALY_cn_allages <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                            allrisk_cause$location_name=="China"&
                            allrisk_cause$sex_name=="Both"&
                            allrisk_cause$metric_name=="Rate"&
                            allrisk_cause$age_name=="All ages")

plot_DALY_allages <- ggplot(DALY_cn_allages, aes(x = year, y = val, group = cause_name, colour = cause_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "All Ages DALYs Rate from 1990-2023 in China",
       x = "Year",
       y = "All Ages DALYs Rate (per 100,000 population)") +
  scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
                                "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
                                "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                                "#D55E00", "#CC79A7", "#F0E", "#009", "#56B",
                                "#E69", "#0072B2", "#D55E00", "#CC79A7"),
                     name = "NCDs") +
  scale_y_continuous(limits = c(0, 1200),breaks = c(0,300,600,900,1200)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
        axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
        axis.text = element_text(size = 20, family = "Arial",color = "black"),
        plot.title = element_text(size = 20, family = "Arial",color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.title = element_text(size = 20, family = "Arial",color = "black"),
        legend.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.key.height = unit(1, 'cm')) +
  guides(colour = guide_legend(ncol = 1)) +
  geom_point()
ggsave(plot_DALY_allages,file="plot_DALY_allages_level34.tif",width = 20,height = 10,dpi = 300)
####Age standardised 线图
DALY_cn_Age_standardised <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                                     allrisk_cause$location_name=="China"&
                                     allrisk_cause$sex_name=="Both"&
                                     allrisk_cause$metric_name=="Rate"&
                                     allrisk_cause$age_name=="Age-standardized")
plot_DALY_Age_standardised <- ggplot(DALY_cn_Age_standardised, aes(x = year, y = val, group = cause_name, colour = cause_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "Age-standardized DALYs Rate from 1990-2023 in China",
       x = "Year",
       y = "Age-standardized DALYs Rate (per 100,000 population)") +
  scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
                                "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
                                "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                                "#D55E00", "#CC79A7", "#F0E", "#009", "#56B",
                                "#E69", "#0072B2", "#D55E00", "#CC79A7"),
                     name = "NCDs") +
  scale_y_continuous(limits = c(0, 1200),breaks = c(0,300,600,900,1200)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
        axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
        axis.text = element_text(size = 20, family = "Arial",color = "black"),
        plot.title = element_text(size = 20, family = "Arial",color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.title = element_text(size = 20, family = "Arial",color = "black"),
        legend.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.key.height = unit(1, 'cm')) +
  guides(colour = guide_legend(ncol = 1)) +
  geom_point()

ggsave(plot_DALY_Age_standardised,file="plot_DALY_Age_standardised_level34.tif",width = 20,height = 10,dpi = 300)


# barplot----
allrisk_allcause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name %in% c("Diet low in fiber", "Diet low in whole grains", "Diet high in red meat", 
                                                                               "Diet high in processed meat", "Diet high in sugar-sweetened beverages", 
                                                                               "Diet high in sodium", "Diet low in calcium", 
                                                                               "Diet low in fruits", "Diet low in vegetables", 
                                                                               "Diet low in omega-6 polyunsaturated fatty acids", "Diet low in legumes", 
                                                                               "Diet low in seafood omega-3 fatty acids", "Diet high in trans fatty acids", 
                                                                               "Diet low in nuts and seeds", "Diet low in milk")&
                             dietary_annual1.0$cause_name %in% c("Neoplasms",
                                                                 "Cardiovascular diseases", 
                                                                 "Diabetes and kidney diseases"))
allrisk_allcause1 <- filter(dietary_annual1.0,dietary_annual1.0$rei_name %in% c("Diet low in fiber", "Diet low in whole grains", "Diet high in red meat", 
                                                                                "Diet high in processed meat", "Diet high in sugar-sweetened beverages", 
                                                                                "Diet high in sodium", "Diet low in calcium", 
                                                                                "Diet low in fruits", "Diet low in vegetables", 
                                                                                "Diet low in omega-6 polyunsaturated fatty acids", "Diet low in legumes", 
                                                                                "Diet low in seafood omega-3 fatty acids", "Diet high in trans fatty acids", 
                                                                                "Diet low in nuts and seeds", "Diet low in milk")&
                              dietary_annual1.0$cause_name %in% c("Non-communicable diseases"))
## death barplot ----
death_cn_2023 <- filter(allrisk_allcause,allrisk_allcause$measure_name=="Deaths"&
                          allrisk_allcause$year=="2023"&
                          allrisk_allcause$location_name=="China"&
                          allrisk_allcause$sex_name=="Both"&
                          allrisk_allcause$metric_name=="Number"&
                          allrisk_allcause$age_name=="All ages")
death_cn_1990 <- filter(allrisk_allcause,allrisk_allcause$measure_name=="Deaths"&
                          allrisk_allcause$year=="1990"&
                          allrisk_allcause$location_name=="China"&
                          allrisk_allcause$sex_name=="Both"&
                          allrisk_allcause$metric_name=="Number"&
                          allrisk_allcause$age_name=="All ages")
death_cn_20231 <- filter(allrisk_allcause1,allrisk_allcause1$measure_name=="Deaths"&
                           allrisk_allcause1$year=="2023"&
                           allrisk_allcause1$location_name=="China"&
                           allrisk_allcause1$sex_name=="Both"&
                           allrisk_allcause1$metric_name=="Number"&
                           allrisk_allcause1$age_name=="All ages")
death_cn_19901 <- filter(allrisk_allcause1,allrisk_allcause1$measure_name=="Deaths"&
                           allrisk_allcause1$year=="1990"&
                           allrisk_allcause1$location_name=="China"&
                           allrisk_allcause1$sex_name=="Both"&
                           allrisk_allcause1$metric_name=="Number"&
                           allrisk_allcause1$age_name=="All ages")
############################################~
death_cn_2023_sum <- death_cn_2023 %>%
  group_by(rei_name) %>%
  summarize(total_val = sum(val))
death_cn_1990_sum <- death_cn_1990 %>%
  group_by(rei_name) %>%
  summarize(total_val = sum(val))

death_cn_2023 <- death_cn_2023 %>%
  left_join(death_cn_2023_sum, by = "rei_name")
death_cn_1990 <- death_cn_1990 %>%
  left_join(death_cn_1990_sum, by = "rei_name")

death_cn <- rbind(death_cn_1990, death_cn_2023)
death_cn$year <- as.factor(death_cn$year)
############################################~
death_cn_20231_sum <- death_cn_20231 %>%
  group_by(rei_name) %>%
  summarize(total_val = sum(val))
death_cn_19901_sum <- death_cn_19901 %>%
  group_by(rei_name) %>%
  summarize(total_val = sum(val))

death_cn_20231 <- death_cn_20231 %>%
  left_join(death_cn_20231_sum, by = "rei_name")
death_cn_19901 <- death_cn_19901 %>%
  left_join(death_cn_19901_sum, by = "rei_name")

death_cn <- rbind(death_cn_19901,death_cn_20231)
death_cn$year <- as.factor(death_cn$year)

death_bar_allages <- ggplot(death_cn, aes(y = reorder(rei_name, total_val), x = val)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +  # 添加黑色边框
  scale_fill_manual(values = c("1990" = "#FF0000", "2023" = "#00468BFF"), name = "Year") +
  labs(x = "Death Number of All Ages", y = "Dietary Risks", title = "Death Number of All Ages attributable to Dietary Risks") +
  scale_x_continuous(labels = scales::comma, limits = c(-150000, 900000), breaks = seq(-150000, 900000, by = 150000)) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 20, family = "Arial"),  
    axis.title.y = element_text(size = 20, family = "Arial"),  
    axis.text.x = element_text(size = 15, family = "Arial", colour = "black"), 
    axis.text.y = element_text(size = 20, family = "Arial", colour = "black"), 
    axis.text = element_text(size = 20, family = "Arial"),
    plot.title = element_text(size = 20, family = "Arial"),
    strip.background = element_blank(),
    strip.text = element_text(size = 20, family = "Arial"),
    legend.title = element_text(size = 20, family = "Arial"),
    legend.text = element_text(size = 20, family = "Arial")
  )
ggsave(death_bar_allages,file="death_bar_allages_level1.tif",width = 20,height = 12,dpi = 300)
# sodium <- death_cn_2023 %>% 
#   filter(rei_name=="Diet high in sodium")
# sodium1 <- death_cn_20231 %>% 
#   filter(rei_name=="Diet high in sodium")
# CVD <- sodium %>% 
#   filter(cause_name %in% c("Intracerebral hemorrhage",                                  
#                            "Ischemic heart disease",                                    
#                            "Hypertensive heart disease",                                
#                            "Subarachnoid hemorrhage",                                   
#                            "Atrial fibrillation and flutter",                                    
#                            "Aortic aneurysm",                                          
#                            "Ischemic stroke",                                           
#                            "Lower extremity peripheral arterial disease")) %>% 
#   summarise(cvd=sum(val))

death_bar_allages <- ggplot(death_cn_2023, aes(y = reorder(rei_name, total_val), x = val, fill = cause_name)) +
  geom_bar(stat = "identity", position = "dodge", aes(group = year), colour = "black") +  # 添加黑色边框
  scale_fill_manual(values = c("#FF0000", "#00468BFF", "#0099B4FF"), name = "Cause") +
  labs(x = "Death Number of All Ages", y = "Dietary Risks", title = "Death Number of All Ages attributable to Dietary Risks") +
  scale_x_continuous(labels = scales::comma, limits = c(-150000, 900000), breaks = seq(-150000, 900000, by = 150000)) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 20, family = "Arial"),  
    axis.title.y = element_text(size = 20, family = "Arial"),  
    axis.text.x = element_text(size = 15, family = "Arial", colour = "black"), 
    axis.text.y = element_text(size = 20, family = "Arial", colour = "black"), 
    axis.text = element_text(size = 20, family = "Arial"),
    plot.title = element_text(size = 20, family = "Arial"),
    strip.background = element_blank(),
    strip.text = element_text(size = 20, family = "Arial"),
    legend.title = element_text(size = 20, family = "Arial"),
    legend.text = element_text(size = 20, family = "Arial")
  )
ggsave(death_bar_allages,file="death_bar_allages.png",width = 20,height = 12,dpi = 300)

death_bar_allages <- ggplot(death_cn, aes(y = reorder(rei_name, total_val), x = val, fill = cause_name)) +
  geom_bar(stat = "identity", position = "dodge", aes(group = year, color = as.factor(year)), size = 0.7) +  # 使用不同的颜色区分年份
  scale_fill_manual(values = c("#FF0000","#00468BFF","#0099B4FF"),name = "Cause") +
  scale_color_manual(values = c("1990" = "black", "2023" = "#925E9FFF"), name = "Year") +  # 自定义不同年份的边框颜色
  labs(x = "Death Number of All Ages", y = "Dietary Risks", title = "Death Number of All Ages by Dietary Risks") +
  scale_x_continuous(labels = scales::comma, limits = c(-50000, 900000), breaks = seq(-150000, 900000, by = 150000)) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial"),  
        axis.title.y = element_text(size = 20, family = "Arial"),  
        axis.text.x = element_text(size = 15, family = "Arial", colour = "black"), 
        axis.text.y = element_text(size = 20, family = "Arial", colour = "black"), 
        axis.text = element_text(size = 20, family = "Arial"),
        plot.title = element_text(size = 20, family = "Arial"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial"),
        legend.title = element_text(size = 20, family = "Arial"),
        legend.text = element_text(size = 20, family = "Arial"))








death_bar_allages <- ggplot(death_cn, aes(y = reorder(rei_name, total_val), x = val, fill = cause_name)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = c("#CC0000","#FF6600","#FF9966"),
                    name = "Cause",
                    guide = guide_legend(direction = "vertical")) +
  labs(x = "Death Number of All Ages", y = "Dietary Risks", title = "Death Number of All Ages by Dietary Risks") +
  scale_x_continuous(labels = scales::comma, limits = c(-50000, 900000), breaks = seq(-150000, 900000, by=150000)) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial"),  
        axis.title.y = element_text(size = 20, family = "Arial"),  
        axis.text.x = element_text(size = 15, family = "Arial", colour = "black"), 
        axis.text.y = element_text(size = 20, family = "Arial", colour = "black"), 
        axis.text = element_text(size = 20, family = "Arial"),
        plot.title = element_text(size = 20, family = "Arial"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial"),
        legend.title = element_text(size = 20, family = "Arial"),
        legend.text = element_text(size = 20, family = "Arial")) +
  facet_wrap(~ year)
death_bar_allages <- ggplot(death_cn_2023, aes(y = reorder(rei_name, total_val), x = val, fill = cause_name)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = c("#CC0000","#FF6600","#FF9966"),
                    name = "Cause",
                    guide = guide_legend(direction = "vertical")) +
  labs(x = "Death Number of All Ages", y = "Dietary Risks", title = "Death Number of All Ages by Dietary Risks") +
  # theme_minimal() +
  scale_x_continuous(labels = scales::comma, limits = c(-50000, 900000),breaks = seq(-150000,900000,by=150000)) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),  # 去掉主网格线
        panel.grid.minor = element_blank(),  # 去掉次网格线
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial"),  
        axis.title.y = element_text(size = 20, family = "Arial"),  
        axis.text.x = element_text(size = 15, family = "Arial", colour = "black"), 
        axis.text.y = element_text(size = 20, family = "Arial", colour = "black"), 
        axis.text = element_text(size = 20, family = "Arial"),
        plot.title = element_text(size = 20, family = "Arial"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial"),
        legend.title = element_text(size = 20, family = "Arial"),
        legend.text = element_text(size = 20, family = "Arial"))

# death_bar_allages <- ggplot(death_cn_2023, aes(y = reorder(rei_name, total_val), x = val, fill = cause_name)) +
#   geom_bar(stat = "identity", position = "stack") +
#   scale_fill_manual(values = c("#1B9", "#F0E", "#42B540", "#0099B4", "#925E9F", "#FDAF91", "#AD000A", "#ADB6B6", "#00468B", "#E69F00", "#56B4E9", "#009E73", "#ED0000", "#0072B2", "#D55E00", "#CC79A7", "#E49", "#377EB8", "#4DA111"), 
#                     name = "Cause of Death") +
#   labs(x = "Death Number of All Ages", y = "Dietary Risks", title = "Death Number of All Ages by Dietary Risks") +
#   theme_minimal() +
#   scale_x_continuous(labels = scales::comma, limits = c(-150000, 900000),breaks = seq(-150000,900000,by=150000)) +
#   theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8)) +
#   theme(panel.background = element_rect(fill = "white", colour = NA),  
#         plot.background = element_rect(fill = "white", colour = NA),
#         axis.line = element_line(color = "black", linewidth = 0.7),  
#         axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
#         axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
#         axis.text.x = element_text(size = 15, family = "Arial", face = "bold"), 
#         axis.text.y = element_text(size = 20, family = "Arial", face = "bold"), 
#         axis.text = element_text(size = 20, family = "Arial", face = "bold"),
#         plot.title = element_text(size = 20, family = "Arial", face = "bold"),
#         strip.background = element_blank(),
#         strip.text = element_text(size = 20, family = "Arial", face = "bold"),
#         legend.title = element_text(size = 20, family = "Arial", face = "bold"),
#         legend.text = element_text(size = 20, family = "Arial", face = "bold"))
ggsave(death_bar_allages,file="death_bar_allages_number.tif",width = 20,height = 10,dpi = 300)

## DALY barplot ----
DALY_cn_2023 <- filter(allrisk_allcause,allrisk_allcause$measure_name=="DALYs"&
                         allrisk_allcause$year=="2023"&
                         allrisk_allcause$location_name=="China"&
                         allrisk_allcause$sex_name=="Both"&
                         allrisk_allcause$metric_name=="Number"&
                         allrisk_allcause$age_name=="All ages")
DALY_cn_1990 <- filter(allrisk_allcause,allrisk_allcause$measure_name=="DALYs"&
                         allrisk_allcause$year=="1990"&
                         allrisk_allcause$location_name=="China"&
                         allrisk_allcause$sex_name=="Both"&
                         allrisk_allcause$metric_name=="Rate"&
                         allrisk_allcause$age_name=="All ages")

DALY_cn_20231 <- filter(allrisk_allcause1,allrisk_allcause1$measure_name=="DALYs"&
                          allrisk_allcause1$year=="2023"&
                          allrisk_allcause1$location_name=="China"&
                          allrisk_allcause1$sex_name=="Both"&
                          allrisk_allcause1$metric_name=="Rate"&
                          allrisk_allcause1$age_name=="All ages")
DALY_cn_19901 <- filter(allrisk_allcause1,allrisk_allcause1$measure_name=="DALYs"&
                          allrisk_allcause1$year=="1990"&
                          allrisk_allcause1$location_name=="China"&
                          allrisk_allcause1$sex_name=="Both"&
                          allrisk_allcause1$metric_name=="Rate"&
                          allrisk_allcause1$age_name=="All ages")
#######################################~
DALY_cn_2023_sum <- DALY_cn_2023 %>%
  group_by(rei_name) %>%
  summarize(total_val = sum(val))
DALY_cn_1990_sum <- DALY_cn_1990 %>%
  group_by(rei_name) %>%
  summarize(total_val = sum(val))

DALY_cn_2023 <- DALY_cn_2023 %>%
  left_join(DALY_cn_2023_sum, by = "rei_name")
DALY_cn_1990 <- DALY_cn_1990 %>%
  left_join(DALY_cn_1990_sum, by = "rei_name")

DALY_cn <- rbind(DALY_cn_1990, DALY_cn_2023)
DALY_cn$year <- as.factor(DALY_cn$year)

#######################################~
DALY_cn_20231_sum <- DALY_cn_20231 %>%
  group_by(rei_name) %>%
  summarize(total_val = sum(val))
DALY_cn_19901_sum <- DALY_cn_19901 %>%
  group_by(rei_name) %>%
  summarize(total_val = sum(val))

DALY_cn_20231 <- DALY_cn_20231 %>%
  left_join(DALY_cn_20231_sum, by = "rei_name")
DALY_cn_19901 <- DALY_cn_19901 %>%
  left_join(DALY_cn_19901_sum, by = "rei_name")

DALY_cn <- rbind(DALY_cn_19901, DALY_cn_20231)
DALY_cn$year <- as.factor(DALY_cn$year)

DALY_bar_allages <- ggplot(DALY_cn, aes(y = reorder(rei_name, total_val), x = val, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +  # 添加黑色边框
  scale_fill_manual(values = c("1990" = "#FF0000", "2023" = "#00468BFF"), name = "Year") +
  labs(x = "DALY Rate of All Ages", y = "Dietary Risks", title = "DALY Rate of All Ages attributable to Dietary Risks") +
  scale_x_continuous(labels = scales::comma, limits = c(-100, 1500), breaks = seq(-100, 1500, by = 400)) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 20, family = "Arial"),  
    axis.title.y = element_text(size = 20, family = "Arial"),  
    axis.text.x = element_text(size = 15, family = "Arial", colour = "black"), 
    axis.text.y = element_text(size = 20, family = "Arial", colour = "black"), 
    axis.text = element_text(size = 20, family = "Arial"),
    plot.title = element_text(size = 20, family = "Arial"),
    strip.background = element_blank(),
    strip.text = element_text(size = 20, family = "Arial"),
    legend.title = element_text(size = 20, family = "Arial"),
    legend.text = element_text(size = 20, family = "Arial")
  )
ggsave(DALY_bar_allages,file="DALY_bar_allages_level1.tif",width = 20,height = 12,dpi = 300)
# sodium_DALY <- DALY_cn_2023 %>% 
#   filter(rei_name=="Diet high in sodium")
# sodium1_DALY <- DALY_cn_20231 %>% 
#   filter(rei_name=="Diet high in sodium")
# CVD_DALY <- sodium_DALY %>% 
#   filter(cause_name %in% c("Intracerebral hemorrhage",                                  
#                            "Ischemic heart disease",                                    
#                            "Hypertensive heart disease",                                
#                            "Subarachnoid hemorrhage",                                   
#                            "Atrial fibrillation and flutter",                                    
#                            "Aortic aneurysm",                                          
#                            "Ischemic stroke",                                           
#                            "Lower extremity peripheral arterial disease")) %>% 
#   summarise(cvd=sum(val))

DALY_bar_allages <- ggplot(DALY_cn_2023, aes(y = reorder(rei_name, total_val), x = val, fill = cause_name)) +
  geom_bar(stat = "identity", position = "dodge", aes(group = year), colour = "black") +  # 添加黑色边框
  scale_fill_manual(values = c("#FF0000", "#00468BFF", "#0099B4FF"), name = "Cause") +
  labs(x = "DALY Number of All Ages", y = "Dietary Risks", title = "DALY Number of All Ages attributable to Dietary Risks") +
  scale_x_continuous(labels = scales::comma) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 20, family = "Arial"),  
    axis.title.y = element_text(size = 20, family = "Arial"),  
    axis.text.x = element_text(size = 15, family = "Arial", colour = "black"), 
    axis.text.y = element_text(size = 20, family = "Arial", colour = "black"), 
    axis.text = element_text(size = 20, family = "Arial"),
    plot.title = element_text(size = 20, family = "Arial"),
    strip.background = element_blank(),
    strip.text = element_text(size = 20, family = "Arial"),
    legend.title = element_text(size = 20, family = "Arial"),
    legend.text = element_text(size = 20, family = "Arial")
  )
ggsave(DALY_bar_allages,file="DALY_bar_allages.png",width = 20,height = 12,dpi = 300)

DALY_bar_allages <- ggplot(DALY_cn_2023, aes(y = reorder(rei_name, total_val), x = val, fill = cause_name)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#1B9", "#F0E", "#42B540", "#0099B4", "#925E9F", "#FDAF91", "#AD002A", "#ADB6B6", "#00468B", "#E69F00", "#56B4E9", "#009E73", "#ED0000", "#0072B2", "#D55E00", "#CC79A7", "#E49", "#377EB8", "#4DA111"), name = "Cause of DALY") +
  labs(x = "DALY Rate of All Ages", y = "Dietary Risks", title = "DALY Rate of All Ages by Dietary Risks") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma, limits = c(-250, 1500),breaks = seq(-500, 1500,by=250)) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text.x = element_text(size = 20, family = "Arial", face = "bold"), 
        axis.text.y = element_text(size = 20, family = "Arial", face = "bold"), 
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold"))
ggsave(DALY_bar_allages,file="DALY_bar_allages_number.tif",width = 20,height = 10,dpi = 300)


# 周脉耕老师文章中热图 ----
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&
                          dietary_annual1.0$cause_name %in% c("Diabetes mellitus",
                                                              "Chronic kidney disease",
                                                              "Ischemic heart disease",
                                                              "Aortic aneurysm",
                                                              "Lower extremity peripheral arterial disease",
                                                              "Hypertensive heart disease",
                                                              "Stroke",
                                                              "Atrial fibrillation and flutter",
                                                              "Tracheal, bronchus, and lung cancer",
                                                              "Breast cancer",
                                                              "Prostate cancer",
                                                              "Colon and rectum cancer",
                                                              "Esophageal cancer",
                                                              "Stomach cancer"))
## death heatmap ----
death_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="2023"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="Age-standardized")

z_test <- function(province_rate, national_rate, province_sd,national_sd, n_province, n_national) {
  SE <- sqrt((province_sd^2 / n_province) + (national_sd^2 / n_national))
  z <- (province_rate - national_rate) / SE
  p_value <- 2 * pnorm(-abs(z))
  return(p_value)
}

#全国和各省份人口
n_national <- 14126
location <- c("Hong Kong Special Administrative Region of China",
              "Inner Mongolia",              
              "Shandong",                                        
              "Macao Special Administrative Region of China",    
              "Tianjin",                                         
              "Shaanxi",                                         
              "Beijing",                                         
              "Zhejiang",                                       
              "Xinjiang",                                        
              "Heilongjiang",                                    
              "Yunnan",                                          
              "Guangdong",                                       
              "Guangxi",                                         
              "Guizhou",                                         
              "Jiangxi",                                         
              "Hubei",                                           
              "Sichuan",                                         
              "Anhui",                                           
              "Hainan",                                          
              "Qinghai",                                         
              "Henan" ,                                          
              "Jiangsu",                                         
              "Jilin" ,                                          
              "Shanghai",                                           
              "Ningxia",                                         
              "Fujian",                                          
              "Tibet" ,                                          
              "Chongqing",                                       
              "Gansu",                                           
              "Liaoning" ,                                       
              "Hunan" ,                                          
              "Shanxi" ,                                         
              "Hebei")
province_number <- c(74.1307,240,1017,6.821,137.3,395.4,218.9,654,258.9,
                     312.5,469,1268.4,503.7,385.2,451.7,583,837.2,611.3,
                     102,59.4,988.3,850.5,237.5,248.9,72.5,418.7,36.6,
                     321.2,249,422.9,662.2,348,744.8)
province_population <- data.frame(location,province_number)

national_data <- death_cn_2023 %>% 
  filter(location_name == "China") %>%
  select(cause_name, national_rate = val,national_rate_lower=lower,national_rate_upper=upper) %>% 
  mutate(national_sd = (national_rate_upper - national_rate_lower) / (2 * 1.96)) %>% 
  select(cause_name, national_rate, national_sd)

province_data <- death_cn_2023 %>%
  filter(location_name != "China") %>%
  left_join(national_data, by = "cause_name") %>% 
  left_join(province_population, by= c("location_name"="location")) %>% 
  mutate(n_national=14126) %>% 
  mutate(province_sd = (upper - lower) / (2 * 1.96))


province_data <- province_data %>%
  group_by(location_name, cause_name) %>%
  mutate(
    p_value = z_test(val, national_rate, province_sd, national_sd, province_number, n_national),
    significance = case_when(
      p_value < 0.05 & val > national_rate ~ "Significantly Higher",
      p_value < 0.05 & val < national_rate ~ "Significantly Lower",
      TRUE ~ "Not Significant"
    )
  )

national_data <- death_cn_2023 %>% 
  filter(location_name == "China") %>% 
  mutate(significance = "Not Significant")

province_data <- full_join(province_data, national_data, 
                           by = c("measure_id", "measure_name", "location_id", 
                                  "location_name", "sex_id", "sex_name", 
                                  "age_id", "age_name", "cause_id", 
                                  "cause_name", "rei_id", "rei_name", 
                                  "metric_id", "metric_name", "year","val",
                                  "upper", "lower", "significance"))
province_data <- province_data %>% 
  mutate(location_name=case_when(
    location_name=="Macao Special Administrative Region of China" ~ "Macao",
    location_name=="Hong Kong Special Administrative Region of China" ~ "Hong Kong",
    TRUE ~ location_name
  ))
province_data$location_name <- factor(province_data$location_name,levels = c("Macao", 
                                                                             "Hong Kong", 
                                                                             "Xinjiang", 
                                                                             "Ningxia", 
                                                                             "Qinghai", 
                                                                             "Gansu", 
                                                                             "Shaanxi", 
                                                                             "Tibet", 
                                                                             "Yunnan", 
                                                                             "Guizhou", 
                                                                             "Sichuan", 
                                                                             "Chongqing", 
                                                                             "Hainan", 
                                                                             "Guangxi", 
                                                                             "Guangdong", 
                                                                             "Hunan", 
                                                                             "Hubei", 
                                                                             "Henan", 
                                                                             "Shandong", 
                                                                             "Jiangxi", 
                                                                             "Fujian", 
                                                                             "Anhui", 
                                                                             "Zhejiang", 
                                                                             "Jiangsu", 
                                                                             "Shanghai", 
                                                                             "Heilongjiang", 
                                                                             "Jilin", 
                                                                             "Liaoning", 
                                                                             "Inner Mongolia", 
                                                                             "Shanxi", 
                                                                             "Hebei", 
                                                                             "Tianjin", 
                                                                             "Beijing", 
                                                                             "China"))

# 绘制热图
death_heatmap_plot <- ggplot(province_data, aes(x = cause_name, y = location_name, fill = significance)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(val, 3)), size = 5, family = "Arial", color = "black") + 
  scale_fill_manual(values = c("Significantly Higher" = "#ed000099", "Significantly Lower" = "#42b54099", "Not Significant" = "#FF9933")) +
  labs(x = "Disease", y = "Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
        axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
        axis.text = element_text(size = 20, family = "Arial",color = "black"),
        plot.title = element_text(size = 20, family = "Arial",color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.title = element_text(size = 20, family = "Arial",color = "black"),
        legend.text = element_text(size = 20, family = "Arial",color = "black"))

ggsave(death_heatmap_plot,file="death_heatmap_plot.tif",dpi = 300,width = 20,height = 15)

## DALY heatmap ----
DALY_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                         allrisk_cause$year=="2023"&
                         allrisk_cause$sex_name=="Both"&
                         allrisk_cause$metric_name=="Rate"&
                         allrisk_cause$age_name=="Age-standardized")

z_test <- function(province_rate, national_rate, province_sd,national_sd, n_province, n_national) {
  SE <- sqrt((province_sd^2 / n_province) + (national_sd^2 / n_national))
  z <- (province_rate - national_rate) / SE
  p_value <- 2 * pnorm(-abs(z))
  return(p_value)
}

#全国和各省份人口
n_national <- 14126
location <- c("Hong Kong Special Administrative Region of China",
              "Inner Mongolia",              
              "Shandong",                                        
              "Macao Special Administrative Region of China",    
              "Tianjin",                                         
              "Shaanxi",                                         
              "Beijing",                                         
              "Zhejiang",                                       
              "Xinjiang",                                        
              "Heilongjiang",                                    
              "Yunnan",                                          
              "Guangdong",                                       
              "Guangxi",                                         
              "Guizhou",                                         
              "Jiangxi",                                         
              "Hubei",                                           
              "Sichuan",                                         
              "Anhui",                                           
              "Hainan",                                          
              "Qinghai",                                         
              "Henan" ,                                          
              "Jiangsu",                                         
              "Jilin" ,                                          
              "Shanghai",                                           
              "Ningxia",                                         
              "Fujian",                                          
              "Tibet" ,                                          
              "Chongqing",                                       
              "Gansu",                                           
              "Liaoning" ,                                       
              "Hunan" ,                                          
              "Shanxi" ,                                         
              "Hebei")
province_number <- c(74.1307,240,1017,6.821,137.3,395.4,218.9,654,258.9,
                     312.5,469,1268.4,503.7,385.2,451.7,583,837.2,611.3,
                     102,59.4,988.3,850.5,237.5,248.9,72.5,418.7,36.6,
                     321.2,249,422.9,662.2,348,744.8)
province_population <- data.frame(location,province_number)

national_data <- DALY_cn_2023 %>% 
  filter(location_name == "China") %>%
  select(cause_name, national_rate = val,national_rate_lower=lower,national_rate_upper=upper) %>% 
  mutate(national_sd = (national_rate_upper - national_rate_lower) / (2 * 1.96)) %>% 
  select(cause_name, national_rate, national_sd)

province_data <- DALY_cn_2023 %>%
  filter(location_name != "China") %>%
  left_join(national_data, by = "cause_name") %>% 
  left_join(province_population, by= c("location_name"="location")) %>% 
  mutate(n_national=14126) %>% 
  mutate(province_sd = (upper - lower) / (2 * 1.96))


province_data <- province_data %>%
  group_by(location_name, cause_name) %>%
  mutate(
    p_value = z_test(val, national_rate, province_sd, national_sd, province_number, n_national),
    significance = case_when(
      p_value < 0.05 & val > national_rate ~ "Significantly Higher",
      p_value < 0.05 & val < national_rate ~ "Significantly Lower",
      TRUE ~ "Not Significant"
    )
  )

national_data <- DALY_cn_2023 %>% 
  filter(location_name == "China") %>% 
  mutate(significance = "Not Significant")

province_data <- full_join(province_data, national_data, 
                           by = c("measure_id", "measure_name", "location_id", 
                                  "location_name", "sex_id", "sex_name", 
                                  "age_id", "age_name", "cause_id", 
                                  "cause_name", "rei_id", "rei_name", 
                                  "metric_id", "metric_name", "year","val",
                                  "upper", "lower", "significance"))
province_data <- province_data %>% 
  mutate(location_name=case_when(
    location_name=="Macao Special Administrative Region of China" ~ "Macao",
    location_name=="Hong Kong Special Administrative Region of China" ~ "Hong Kong",
    TRUE ~ location_name
  ))
province_data$location_name <- factor(province_data$location_name,levels = c("Macao", 
                                                                             "Hong Kong", 
                                                                             "Xinjiang", 
                                                                             "Ningxia", 
                                                                             "Qinghai", 
                                                                             "Gansu", 
                                                                             "Shaanxi", 
                                                                             "Tibet", 
                                                                             "Yunnan", 
                                                                             "Guizhou", 
                                                                             "Sichuan", 
                                                                             "Chongqing", 
                                                                             "Hainan", 
                                                                             "Guangxi", 
                                                                             "Guangdong", 
                                                                             "Hunan", 
                                                                             "Hubei", 
                                                                             "Henan", 
                                                                             "Shandong", 
                                                                             "Jiangxi", 
                                                                             "Fujian", 
                                                                             "Anhui", 
                                                                             "Zhejiang", 
                                                                             "Jiangsu", 
                                                                             "Shanghai", 
                                                                             "Heilongjiang", 
                                                                             "Jilin", 
                                                                             "Liaoning", 
                                                                             "Inner Mongolia", 
                                                                             "Shanxi", 
                                                                             "Hebei", 
                                                                             "Tianjin", 
                                                                             "Beijing", 
                                                                             "China"))
# 绘制热图
DALY_heatmap_plot <- ggplot(province_data, aes(x = cause_name, y = location_name, fill = significance)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(val, 2)), size = 5, family = "Arial",color = "black") + 
  scale_fill_manual(values = c("Significantly Higher" = "#ed000099", "Significantly Lower" = "#42b54099", "Not Significant" = "#FF9933")) +
  labs(x = "Disease", y = "Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
        axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
        axis.text = element_text(size = 20, family = "Arial",color = "black"),
        plot.title = element_text(size = 20, family = "Arial",color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.title = element_text(size = 20, family = "Arial",color = "black"),
        legend.text = element_text(size = 20, family = "Arial",color = "black"))

ggsave(DALY_heatmap_plot,file="DALY_heatmap_plot.tif",dpi = 300,width = 20,height = 15)


# provincial Non-communicable diseases of allrisk ----
provincial_disease <- dietary_annual1.0 %>% 
  filter(rei_name == "Dietary risks" &
           cause_name == "Non-communicable diseases")

##allages death线图 ----
death_province_allages <- filter(provincial_disease,provincial_disease$measure_name=="Deaths"&
                                   provincial_disease$sex_name=="Both"&
                                   provincial_disease$metric_name=="Rate"&
                                   provincial_disease$age_name=="All ages")

plot_death_allages <- ggplot(death_province_allages, aes(x = year, y = val, group = location_name, colour = location_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "All Ages Death Rate of Non-communicable diseases from 1990-2023 by province",
       x = "Year",
       y = "All Ages Death Rate (per 100,000 population)") +
  # scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
  #                                        "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
  #                                        "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
  #                                        name = "Cause of Death") +
  scale_y_continuous(limits = c(50, 210)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial"),  
        axis.title.y = element_text(size = 20, family = "Arial"),  
        axis.text = element_text(size = 20, family = "Arial"),
        plot.title = element_text(size = 20, family = "Arial"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial"),
        legend.title = element_text(size = 20, family = "Arial"),
        legend.text = element_text(size = 20, family = "Arial")) +
  guides(colour = guide_legend(ncol = 1)) +
  geom_point()
ggsave(plot_death_allages,file="plot_death_allages_allrisk_Non-communicable diseases.tif",width = 20,height = 10,dpi = 300)
##Age standardised death线图 ----
death_province_Age_standardised <- filter(provincial_disease,provincial_disease$measure_name=="Deaths"&
                                            provincial_disease$sex_name=="Both"&
                                            provincial_disease$metric_name=="Rate"&
                                            provincial_disease$age_name=="Age-standardized")
plot_death_Age_standardised <- ggplot(death_province_Age_standardised, aes(x = year, y = val, group = location_name, colour = location_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "Age-standardized Death Rate of Non-communicable diseases from 1990-2023 in China",
       x = "Year",
       y = "Age-standardized Death Rate (per 100,000 population)") +
  # scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
  #                                        "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
  #                                        "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
  #                                        name = "Cause of Death") +
  scale_y_continuous(limits = c(0, 300)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  guides(colour = guide_legend(ncol = 1))+
  geom_point()

ggsave(plot_death_Age_standardised,file="plot_death_Age_standardised_allrisk_Non-communicable diseases.tif",width = 20,height = 10,dpi = 300)


##allages DALY线图 ----
DALY_province_allages <- filter(provincial_disease,provincial_disease$measure_name=="DALYs"&
                                  provincial_disease$sex_name=="Both"&
                                  provincial_disease$metric_name=="Rate"&
                                  provincial_disease$age_name=="All ages")

plot_DALY_allages <- ggplot(DALY_province_allages, aes(x = year, y = val, group = location_name, colour = location_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "All Ages DALY Rate of Non-communicable diseases from 1990-2023 by province",
       x = "Year",
       y = "All Ages DALY Rate (per 100,000 population)") +
  # scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
  #                                        "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
  #                                        "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
  #                                        name = "Cause of DALY") +
  scale_y_continuous(limits = c(1000, 5000)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  guides(colour = guide_legend(ncol = 1)) +
  geom_point()
ggsave(plot_DALY_allages,file="plot_DALY_allages_allrisk_Non-communicable diseases.tif",width = 20,height = 10,dpi = 300)
##Age standardised DALY线图 ----
DALY_province_Age_standardised <- filter(provincial_disease,provincial_disease$measure_name=="DALYs"&
                                           provincial_disease$sex_name=="Both"&
                                           provincial_disease$metric_name=="Rate"&
                                           provincial_disease$age_name=="Age-standardized")
plot_DALY_Age_standardised <- ggplot(DALY_province_Age_standardised, aes(x = year, y = val, group = location_name, colour = location_name)) +
  geom_line(size = 1.0, alpha = 0.5) +
  labs(title = "Age-standardized DALY Rate of Non-communicable diseases from 1990-2023 in China",
       x = "Year",
       y = "Age-standardized DALY Rate (per 100,000 population)") +
  # scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
  #                                        "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
  #                                        "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
  #                                        name = "Cause of DALY") +
  scale_y_continuous(limits = c(0, 7000),breaks = seq(0,7000,by=1000)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  guides(colour = guide_legend(ncol = 1))+
  geom_point()

ggsave(plot_DALY_Age_standardised,file="plot_DALY_Age_standardised_allrisk_Non-communicable diseases.tif",width = 20,height = 10,dpi = 300)


## 改良线图 ----
## 颜色和线段类型组合
colors <- c("#ED0000FF", "#00468BFF", "#42B540FF", "#0099B4FF", "#925E9FFF",
            "#1B1919FF", "#E69F00")
linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash")

# 假设 data_prov$省份 已经包含所有的省份名称
unique_provinces <- unique(provincial_disease$location_name)
num_provinces <- length(unique_provinces)

# 创建颜色和线型的组合
color_combinations <- expand.grid(colors = colors, linetypes = linetypes)

# 为每个省份分配颜色和线型组合
assigned_colors <- color_combinations$colors[1:num_provinces]
assigned_linetypes <- color_combinations$linetypes[1:num_provinces]

# 创建颜色和线型的映射表
color_map <- setNames(assigned_colors, unique_provinces)
linetype_map <- setNames(assigned_linetypes, unique_provinces)

# provincial Non-communicable diseases of allrisk
provincial_disease <- dietary_annual1.0 %>% 
  filter(rei_name == "Dietary risks" &
           cause_name == "Non-communicable diseases")

death_province_allages <- filter(provincial_disease,provincial_disease$measure_name=="Deaths"&
                                   provincial_disease$sex_name=="Both"&
                                   provincial_disease$metric_name=="Rate"&
                                   provincial_disease$age_name=="All ages")

plot_death_allages <- ggplot(death_province_allages, aes(x = year, y = val, group = location_name, colour = location_name)) +
  geom_line(aes(color = location_name, linetype = location_name),size = 1.0, alpha = 0.5) +
  labs(title = "All Ages Death Rate of Non-communicable diseases from 1990-2023 by province",
       x = "Year",
       y = "All Ages Death Rate (per 100,000 population)") +
  # scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
  #                                        "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
  #                                        "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
  #                                        name = "Cause of Death") +
  scale_y_continuous(limits = c(50, 210)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  scale_color_manual(values = color_map) +  # 使用映射表中的颜色
  scale_linetype_manual(values = linetype_map) + 
  guides(colour = guide_legend(ncol = 1)) +# 使用映射表中的线型
  geom_point()
ggsave(plot_death_allages,file="plot_death_allages_allrisk_Non-communicable diseases.tif",width = 20,height = 10,dpi = 300)
##Age standardised death线图
death_province_Age_standardised <- filter(provincial_disease,provincial_disease$measure_name=="Deaths"&
                                            provincial_disease$sex_name=="Both"&
                                            provincial_disease$metric_name=="Rate"&
                                            provincial_disease$age_name=="Age-standardized")
plot_death_Age_standardised <- ggplot(death_province_Age_standardised, aes(x = year, y = val, group = location_name, colour = location_name)) +
  geom_line(aes(color = location_name, linetype = location_name),size = 1.0, alpha = 0.5) +
  labs(title = "Age-standardized Death Rate of Non-communicable diseases from 1990-2023 in China",
       x = "Year",
       y = "Age-standardized Death Rate (per 100,000 population)") +
  # scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
  #                                        "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
  #                                        "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
  #                                        name = "Cause of Death") +
  scale_y_continuous(limits = c(0, 300)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  scale_color_manual(values = color_map) +  # 使用映射表中的颜色
  scale_linetype_manual(values = linetype_map) +
  guides(colour = guide_legend(ncol = 1)) +# 使用映射表中的线型
  geom_point()

ggsave(plot_death_Age_standardised,file="plot_death_Age_standardised_allrisk_Non-communicable diseases.tif",width = 20,height = 10,dpi = 300)


##allages DALY线图
DALY_province_allages <- filter(provincial_disease,provincial_disease$measure_name=="DALYs"&
                                  provincial_disease$sex_name=="Both"&
                                  provincial_disease$metric_name=="Rate"&
                                  provincial_disease$age_name=="All ages")

plot_DALY_allages <- ggplot(DALY_province_allages, aes(x = year, y = val, group = location_name, colour = location_name)) +
  geom_line(aes(color = location_name, linetype = location_name),size = 1.0, alpha = 0.5) +
  labs(title = "All Ages DALY Rate of Non-communicable diseases from 1990-2023 by province",
       x = "Year",
       y = "All Ages DALY Rate (per 100,000 population)") +
  # scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
  #                                        "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
  #                                        "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
  #                                        name = "Cause of DALY") +
  scale_y_continuous(limits = c(1000, 5000)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  scale_color_manual(values = color_map) +  # 使用映射表中的颜色
  scale_linetype_manual(values = linetype_map) + 
  guides(colour = guide_legend(ncol = 1)) +# 使用映射表中的线型
  geom_point()
ggsave(plot_DALY_allages,file="plot_DALY_allages_allrisk_Non-communicable diseases.tif",width = 20,height = 10,dpi = 300)
##Age standardised DALY线图
DALY_province_Age_standardised <- filter(provincial_disease,provincial_disease$measure_name=="DALYs"&
                                           provincial_disease$sex_name=="Both"&
                                           provincial_disease$metric_name=="Rate"&
                                           provincial_disease$age_name=="Age-standardized")
plot_DALY_Age_standardised <- ggplot(DALY_province_Age_standardised, aes(x = year, y = val, group = location_name, colour = location_name)) +
  geom_line(aes(color = location_name, linetype = location_name),size = 1.0, alpha = 0.5) +
  labs(title = "Age-standardized DALY Rate of Non-communicable diseases from 1990-2023 in China",
       x = "Year",
       y = "Age-standardized DALY Rate (per 100,000 population)") +
  # scale_color_manual(values = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", 
  #                                        "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919", "#E69F00", 
  #                                        "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
  #                                        name = "Cause of DALY") +
  scale_y_continuous(limits = c(0, 7000),breaks = seq(0,7000,by=1000)) +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.title.y = element_text(size = 20, family = "Arial", face = "bold"),  
        axis.text = element_text(size = 20, family = "Arial", face = "bold"),
        plot.title = element_text(size = 20, family = "Arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial", face = "bold"),
        legend.title = element_text(size = 20, family = "Arial", face = "bold"),
        legend.text = element_text(size = 20, family = "Arial", face = "bold")) +
  scale_color_manual(values = color_map) +  # 使用映射表中的颜色
  scale_linetype_manual(values = linetype_map) + 
  guides(colour = guide_legend(ncol = 1)) +# 使用映射表中的线型
  geom_point()

ggsave(plot_DALY_Age_standardised,file="plot_DALY_Age_standardised_allrisk_Non-communicable diseases.tif",width = 20,height = 10,dpi = 300)







############################################################level 3####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&
                          dietary_annual1.0$cause_name %in% c("Diabetes mellitus",
                                                              "Chronic kidney disease",
                                                              "Ischemic heart disease",
                                                              "Aortic aneurysm",
                                                              "Lower extremity peripheral arterial disease",
                                                              "Hypertensive heart disease",
                                                              "Stroke",
                                                              "Atrial fibrillation and flutter",
                                                              "Tracheal, bronchus, and lung cancer",
                                                              "Breast cancer",
                                                              "Prostate cancer",
                                                              "Colon and rectum cancer",
                                                              "Esophageal cancer",
                                                              "Stomach cancer"))
########################death####
### rate及percentage change
death_province_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                                allrisk_cause$year=="2023"&
                                allrisk_cause$location_name!="China"&
                                allrisk_cause$sex_name=="Both"&
                                allrisk_cause$metric_name=="Rate"&
                                allrisk_cause$age_name=="All ages")
death_province_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                                allrisk_cause$year=="1990"&
                                allrisk_cause$location_name!="China"&
                                allrisk_cause$sex_name=="Both"&
                                allrisk_cause$metric_name=="Rate"&
                                allrisk_cause$age_name=="All ages")

death_province_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                                allrisk_cause$year=="2023"&
                                allrisk_cause$location_name!="China"&
                                allrisk_cause$sex_name=="Both"&
                                allrisk_cause$metric_name=="Rate"&
                                allrisk_cause$age_name=="Age-standardized")
death_province_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                                allrisk_cause$year=="1990"&
                                allrisk_cause$location_name!="China"&
                                allrisk_cause$sex_name=="Both"&
                                allrisk_cause$metric_name=="Rate"&
                                allrisk_cause$age_name=="Age-standardized")
#allages rate
death_province_2023_1 <- death_province_2023 %>% 
  select(cause_name,location_name,val,lower,upper) %>%  
  group_by(location_name) %>%
  arrange(desc(val)) %>% 
  mutate(rank_2023 = row_number()) %>% 
  mutate(death_allages_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,rank_2023,death_allages_2023) %>% 
  arrange(location_name) %>% 
  ungroup()

death_province_1990_1 <- death_province_1990 %>% 
  select(cause_name,location_name,val,lower,upper) %>% 
  group_by(location_name) %>% 
  arrange(desc(val)) %>% 
  mutate(rank_1990 = row_number()) %>% 
  mutate(death_allages_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,rank_1990,death_allages_1990) %>% 
  arrange(location_name) %>% 
  ungroup()

death <- left_join(death_province_2023_1,death_province_1990_1,by=c("location_name","cause_name"))
death <- death %>% 
  select(location_name,cause_name,rank_1990,rank_2023,death_allages_1990,death_allages_2023)
#Age standardised rate
death_province_2023_1 <- death_province_2023 %>% 
  select(cause_name,location_name,val,lower,upper) %>%
  group_by(location_name) %>% 
  mutate(death_Age_standardised_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,death_Age_standardised_2023)%>% 
  ungroup()

death_province_1990_1 <- death_province_1990 %>% 
  select(cause_name,location_name,val,lower,upper) %>% 
  group_by(location_name) %>% 
  mutate(death_Age_standardised_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,death_Age_standardised_1990)%>% 
  ungroup()

death_Age_standardised <- left_join(death_province_2023_1,death_province_1990_1,by=c("location_name","cause_name"))
death_Age_standardised <- death_Age_standardised %>% 
  select(location_name,cause_name,death_Age_standardised_1990,death_Age_standardised_2023)

#all ages percentage
death_province_2023_2 <- death_province_2023 %>% 
  select(location_name,cause_name,val) %>% 
  rename(death_2023= val)

death_province_1990_2 <- death_province_1990 %>% 
  select(location_name,cause_name,val) %>% 
  rename(death_1990=val)

death_per <- merge(death_province_1990_2,death_province_2023_2,by=c("location_name","cause_name"))
death_per_allages <- death_per %>% 
  mutate(death_per_change_allages=round(((death_2023-death_1990)/death_1990)*100,digits = 2)) %>% 
  select(location_name,cause_name,death_per_change_allages)
#Age standardised percentage
death_province_2023_2 <- death_province_2023 %>% 
  select(location_name,cause_name,val) %>% 
  rename(death_2023= val)

death_province_1990_2 <- death_province_1990 %>% 
  select(location_name,cause_name,val) %>% 
  rename(death_1990=val)

death_per <- merge(death_province_1990_2,death_province_2023_2,by=c("location_name","cause_name"))
death_per_Agestand <- death_per %>% 
  mutate(death_per_change_Agestand=round(((death_2023-death_1990)/death_1990)*100,digits = 2)) %>% 
  select(location_name,cause_name,death_per_change_Agestand)

death_final <- death %>% 
  left_join(death_Age_standardised, by=c("location_name","cause_name")) %>% 
  left_join(death_per_allages, by=c("location_name","cause_name")) %>% 
  left_join(death_per_Agestand, by=c("location_name","cause_name"))
write.csv(death_final,file="death_province.csv")

########################DALY####
DALY_province_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                               allrisk_cause$year=="2023"&
                               allrisk_cause$location_name!="China"&
                               allrisk_cause$sex_name=="Both"&
                               allrisk_cause$metric_name=="Rate"&
                               allrisk_cause$age_name=="All ages")
DALY_province_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                               allrisk_cause$year=="1990"&
                               allrisk_cause$location_name!="China"&
                               allrisk_cause$sex_name=="Both"&
                               allrisk_cause$metric_name=="Rate"&
                               allrisk_cause$age_name=="All ages")

DALY_province_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                               allrisk_cause$year=="2023"&
                               allrisk_cause$location_name!="China"&
                               allrisk_cause$sex_name=="Both"&
                               allrisk_cause$metric_name=="Rate"&
                               allrisk_cause$age_name=="Age-standardized")
DALY_province_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                               allrisk_cause$year=="1990"&
                               allrisk_cause$location_name!="China"&
                               allrisk_cause$sex_name=="Both"&
                               allrisk_cause$metric_name=="Rate"&
                               allrisk_cause$age_name=="Age-standardized")
#allages rate
DALY_province_2023_1 <- DALY_province_2023 %>% 
  select(cause_name,location_name,val,lower,upper) %>%  
  group_by(location_name) %>%
  arrange(desc(val)) %>% 
  mutate(rank_2023 = row_number()) %>% 
  mutate(DALY_allages_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,rank_2023,DALY_allages_2023) %>% 
  arrange(location_name) %>% 
  ungroup()

DALY_province_1990_1 <- DALY_province_1990 %>% 
  select(cause_name,location_name,val,lower,upper) %>% 
  group_by(location_name) %>% 
  arrange(desc(val)) %>% 
  mutate(rank_1990 = row_number()) %>% 
  mutate(DALY_allages_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,rank_1990,DALY_allages_1990) %>% 
  arrange(location_name) %>% 
  ungroup()

DALY <- left_join(DALY_province_2023_1,DALY_province_1990_1,by=c("location_name","cause_name"))
DALY <- DALY %>% 
  select(location_name,cause_name,rank_1990,rank_2023,DALY_allages_1990,DALY_allages_2023)
#Age standardised rate
DALY_province_2023_1 <- DALY_province_2023 %>% 
  select(cause_name,location_name,val,lower,upper) %>%
  group_by(location_name) %>% 
  mutate(DALY_Age_standardised_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,DALY_Age_standardised_2023)%>% 
  ungroup()

DALY_province_1990_1 <- DALY_province_1990 %>% 
  select(cause_name,location_name,val,lower,upper) %>% 
  group_by(location_name) %>% 
  mutate(DALY_Age_standardised_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,DALY_Age_standardised_1990)%>% 
  ungroup()

DALY_Age_standardised <- left_join(DALY_province_2023_1,DALY_province_1990_1,by=c("location_name","cause_name"))
DALY_Age_standardised <- DALY_Age_standardised %>% 
  select(location_name,cause_name,DALY_Age_standardised_1990,DALY_Age_standardised_2023)

#all ages percentage
DALY_province_2023_2 <- DALY_province_2023 %>% 
  select(location_name,cause_name,val) %>% 
  rename(DALY_2023= val)

DALY_province_1990_2 <- DALY_province_1990 %>% 
  select(location_name,cause_name,val) %>% 
  rename(DALY_1990=val)

DALY_per <- merge(DALY_province_1990_2,DALY_province_2023_2,by=c("location_name","cause_name"))
DALY_per_allages <- DALY_per %>% 
  mutate(DALY_per_change_allages=round(((DALY_2023-DALY_1990)/DALY_1990)*100,digits = 2)) %>% 
  select(location_name,cause_name,DALY_per_change_allages)
#Age standardised percentage
DALY_province_2023_2 <- DALY_province_2023 %>% 
  select(location_name,cause_name,val) %>% 
  rename(DALY_2023= val)

DALY_province_1990_2 <- DALY_province_1990 %>% 
  select(location_name,cause_name,val) %>% 
  rename(DALY_1990=val)

DALY_per <- merge(DALY_province_1990_2,DALY_province_2023_2,by=c("location_name","cause_name"))
DALY_per_Agestand <- DALY_per %>% 
  mutate(DALY_per_change_Agestand=round(((DALY_2023-DALY_1990)/DALY_1990)*100,digits = 2)) %>% 
  select(location_name,cause_name,DALY_per_change_Agestand)

DALY_final <- DALY %>% 
  left_join(DALY_Age_standardised, by=c("location_name","cause_name")) %>% 
  left_join(DALY_per_allages, by=c("location_name","cause_name")) %>% 
  left_join(DALY_per_Agestand, by=c("location_name","cause_name"))
write.csv(DALY_final,file="DALY_province.csv")

############################################################level 4####
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&
                          dietary_annual1.0$cause_name %in% c("Subarachnoid hemorrhage",    
                                                              "Ischemic stroke",                                            
                                                              "Diabetes mellitus type 2",    
                                                              "Intracerebral hemorrhage",                       
                                                              "Chronic kidney disease due to hypertension",                                   
                                                              "Chronic kidney disease due to other and unspecified causes",                                     
                                                              "Chronic kidney disease due to diabetes mellitus type 2",                                         
                                                              "Chronic kidney disease due to glomerulonephritis"))
allrisk_cause$cause_name <- factor(allrisk_cause$cause_name,levels = c("Subarachnoid hemorrhage",    
                                                                       "Ischemic stroke",  
                                                                       "Intracerebral hemorrhage", 
                                                                       "Diabetes mellitus type 2",    
                                                                       "Chronic kidney disease due to hypertension",                                   
                                                                       "Chronic kidney disease due to other and unspecified causes",                                     
                                                                       "Chronic kidney disease due to diabetes mellitus type 2",                                         
                                                                       "Chronic kidney disease due to glomerulonephritis"))
########################death####
death_province_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                                allrisk_cause$year=="2023"&
                                allrisk_cause$location_name!="China"&
                                allrisk_cause$sex_name=="Both"&
                                allrisk_cause$metric_name=="Rate"&
                                allrisk_cause$age_name=="All ages")
death_province_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                                allrisk_cause$year=="1990"&
                                allrisk_cause$location_name!="China"&
                                allrisk_cause$sex_name=="Both"&
                                allrisk_cause$metric_name=="Rate"&
                                allrisk_cause$age_name=="All ages")

death_province_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                                allrisk_cause$year=="2023"&
                                allrisk_cause$location_name!="China"&
                                allrisk_cause$sex_name=="Both"&
                                allrisk_cause$metric_name=="Rate"&
                                allrisk_cause$age_name=="Age-standardized")
death_province_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                                allrisk_cause$year=="1990"&
                                allrisk_cause$location_name!="China"&
                                allrisk_cause$sex_name=="Both"&
                                allrisk_cause$metric_name=="Rate"&
                                allrisk_cause$age_name=="Age-standardized")
#allages rate
death_province_2023_1 <- death_province_2023 %>% 
  select(cause_name,location_name,val,lower,upper) %>% 
  group_by(location_name) %>% 
  mutate(death_allages_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,death_allages_2023) %>% 
  arrange(location_name,cause_name) %>% 
  ungroup()

death_province_1990_1 <- death_province_1990 %>% 
  select(cause_name,location_name,val,lower,upper) %>% 
  group_by(location_name) %>% 
  mutate(death_allages_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,death_allages_1990) %>% 
  arrange(location_name,cause_name) %>% 
  ungroup()

death <- left_join(death_province_2023_1,death_province_1990_1,by=c("location_name","cause_name"))
death <- death %>% 
  select(location_name,cause_name,death_allages_1990,death_allages_2023)
#Age standardised rate
death_province_2023_1 <- death_province_2023 %>% 
  select(location_name,cause_name,val,lower,upper) %>%
  mutate(death_Age_standardised_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,death_Age_standardised_2023)

death_province_1990_1 <- death_province_1990 %>% 
  select(location_name,cause_name,val,lower,upper) %>% 
  mutate(death_Age_standardised_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,death_Age_standardised_1990)

death_Age_standardised <- left_join(death_province_2023_1,death_province_1990_1,by=c("location_name","cause_name"))
death_Age_standardised <- death_Age_standardised %>% 
  select(location_name,cause_name,death_Age_standardised_1990,death_Age_standardised_2023)

#all ages percentage
death_province_2023_2 <- death_province_2023 %>% 
  select(location_name,cause_name,val) %>% 
  rename(death_2023= val)

death_province_1990_2 <- death_province_1990 %>% 
  select(location_name,cause_name,val) %>% 
  rename(death_1990=val)

death_per <- merge(death_province_1990_2,death_province_2023_2,by=c("location_name","cause_name"))
death_per_allages <- death_per %>% 
  mutate(death_per_change_allages=round(((death_2023-death_1990)/death_1990)*100,digits = 2)) %>% 
  select(location_name,cause_name,death_per_change_allages)
#Age standardised percentage
death_province_2023_2 <- death_province_2023 %>% 
  select(location_name,cause_name,val) %>% 
  rename(death_2023= val)

death_province_1990_2 <- death_province_1990 %>% 
  select(location_name,cause_name,val) %>% 
  rename(death_1990=val)

death_per <- merge(death_province_1990_2,death_province_2023_2,by=c("location_name","cause_name"))
death_per_Agestand <- death_per %>% 
  mutate(death_per_change_Agestand=round(((death_2023-death_1990)/death_1990)*100,digits = 2)) %>% 
  select(location_name,cause_name,death_per_change_Agestand)

death_final <- death %>% 
  left_join(death_Age_standardised, by=c("location_name","cause_name")) %>% 
  left_join(death_per_allages, by=c("location_name","cause_name")) %>% 
  left_join(death_per_Agestand, by=c("location_name","cause_name"))
write.csv(death_final,file="death1_province.csv")

########################DALY####
DALY_province_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                               allrisk_cause$year=="2023"&
                               allrisk_cause$location_name!="China"&
                               allrisk_cause$sex_name=="Both"&
                               allrisk_cause$metric_name=="Rate"&
                               allrisk_cause$age_name=="All ages")
DALY_province_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                               allrisk_cause$year=="1990"&
                               allrisk_cause$location_name!="China"&
                               allrisk_cause$sex_name=="Both"&
                               allrisk_cause$metric_name=="Rate"&
                               allrisk_cause$age_name=="All ages")

DALY_province_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                               allrisk_cause$year=="2023"&
                               allrisk_cause$location_name!="China"&
                               allrisk_cause$sex_name=="Both"&
                               allrisk_cause$metric_name=="Rate"&
                               allrisk_cause$age_name=="Age-standardized")
DALY_province_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                               allrisk_cause$year=="1990"&
                               allrisk_cause$location_name!="China"&
                               allrisk_cause$sex_name=="Both"&
                               allrisk_cause$metric_name=="Rate"&
                               allrisk_cause$age_name=="Age-standardized")
#allages rate
DALY_province_2023_1 <- DALY_province_2023 %>% 
  select(cause_name,location_name,val,lower,upper) %>% 
  group_by(location_name) %>% 
  mutate(DALY_allages_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,DALY_allages_2023) %>% 
  arrange(location_name,cause_name) %>% 
  ungroup()

DALY_province_1990_1 <- DALY_province_1990 %>% 
  select(cause_name,location_name,val,lower,upper) %>% 
  group_by(location_name) %>% 
  mutate(DALY_allages_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,DALY_allages_1990) %>% 
  arrange(location_name,cause_name) %>% 
  ungroup()

DALY <- left_join(DALY_province_2023_1,DALY_province_1990_1,by=c("location_name","cause_name"))
DALY <- DALY %>% 
  select(location_name,cause_name,DALY_allages_1990,DALY_allages_2023)
#Age standardised rate
DALY_province_2023_1 <- DALY_province_2023 %>% 
  select(location_name,cause_name,val,lower,upper) %>%
  mutate(DALY_Age_standardised_2023=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,DALY_Age_standardised_2023)

DALY_province_1990_1 <- DALY_province_1990 %>% 
  select(location_name,cause_name,val,lower,upper) %>% 
  mutate(DALY_Age_standardised_1990=paste(round(val,digits = 2),"(",round(lower,digits = 2),",",round(upper,digits = 2),")")) %>% 
  select(location_name,cause_name,DALY_Age_standardised_1990)

DALY_Age_standardised <- left_join(DALY_province_2023_1,DALY_province_1990_1,by=c("location_name","cause_name"))
DALY_Age_standardised <- DALY_Age_standardised %>% 
  select(location_name,cause_name,DALY_Age_standardised_1990,DALY_Age_standardised_2023)

#all ages percentage
DALY_province_2023_2 <- DALY_province_2023 %>% 
  select(location_name,cause_name,val) %>% 
  rename(DALY_2023= val)

DALY_province_1990_2 <- DALY_province_1990 %>% 
  select(location_name,cause_name,val) %>% 
  rename(DALY_1990=val)

DALY_per <- merge(DALY_province_1990_2,DALY_province_2023_2,by=c("location_name","cause_name"))
DALY_per_allages <- DALY_per %>% 
  mutate(DALY_per_change_allages=round(((DALY_2023-DALY_1990)/DALY_1990)*100,digits = 2)) %>% 
  select(location_name,cause_name,DALY_per_change_allages)
#Age standardised percentage
DALY_province_2023_2 <- DALY_province_2023 %>% 
  select(location_name,cause_name,val) %>% 
  rename(DALY_2023= val)

DALY_province_1990_2 <- DALY_province_1990 %>% 
  select(location_name,cause_name,val) %>% 
  rename(DALY_1990=val)

DALY_per <- merge(DALY_province_1990_2,DALY_province_2023_2,by=c("location_name","cause_name"))
DALY_per_Agestand <- DALY_per %>% 
  mutate(DALY_per_change_Agestand=round(((DALY_2023-DALY_1990)/DALY_1990)*100,digits = 2)) %>% 
  select(location_name,cause_name,DALY_per_change_Agestand)

DALY_final <- DALY %>% 
  left_join(DALY_Age_standardised, by=c("location_name","cause_name")) %>% 
  left_join(DALY_per_allages, by=c("location_name","cause_name")) %>% 
  left_join(DALY_per_Agestand, by=c("location_name","cause_name"))
write.csv(DALY_final,file="DALY1_province.csv")



# 国际对比的点图1 ----
##data ----
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name=="Dietary risks"&dietary_annual1.0$cause_name=="Non-communicable diseases")

death_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                          allrisk_cause$year=="2023"&
                          allrisk_cause$sex_name=="Both"&
                          allrisk_cause$location_name!="China"&
                          allrisk_cause$metric_name=="Rate"&
                          allrisk_cause$age_name=="Age-standardized")
death_cn_2023 <- death_cn_2023 %>% 
  select(location_name,measure_name,val)

DALY_cn_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                         allrisk_cause$year=="2023"&
                         allrisk_cause$sex_name=="Both"&
                         allrisk_cause$location_name!="China"&
                         allrisk_cause$metric_name=="Rate"&
                         allrisk_cause$age_name=="Age-standardized")
DALY_cn_2023 <- DALY_cn_2023 %>% 
  select(location_name,measure_name,val)

death_and_DALY <- rbind(death_cn_2023,DALY_cn_2023)

AAPC1 <- AAPC %>% 
  mutate(measure_name=case_when(
    measure=="death" ~ "Deaths",
    measure=="DALY" ~ "DALYs" ,
    TRUE ~ measure
  )) %>% 
  select(measure_name,location_name,AAPC)

wu <- left_join(death_and_DALY,AAPC1,by=c("measure_name","location_name"))
wu <- wu %>% 
  mutate(location_name=case_when(
    location_name=="Macao Special Administrative Region of China" ~ "Macao",
    location_name=="Hong Kong Special Administrative Region of China" ~ "Hong Kong",
    TRUE ~ location_name
  ))

###death----
wu_death <- filter(wu,wu$measure_name=="Deaths")
val_quantiles <- quantile(wu_death$val, probs = c(1/3,1/2, 2/3))
val_quantiles


AAPC_quantiles <- quantile(wu_death$AAPC, probs = c(1/3,1/2, 2/3))
AAPC_quantiles

val_quantiles <- c(81.20,101.99)  
AAPC_quantiles <- c(-1.91, -1.50) 

val_quantiles1 <- c(94.24)  
AAPC_quantiles1 <- c(-1.83) 

# 创建分类变量，根据 AAPC 和 YLL 的值将数据分成不同象限
wu_death$category <- with(wu_death, 
                          ifelse(AAPC < -1.91 & val < 81.20, "High AAPC & Low Death Rate",
                                 ifelse(AAPC < -1.91 & val >= 81.20 & val <101.99, "High AAPC & Medium Death Rate",
                                        ifelse(AAPC < -1.91 & val >= 101.99, "High AAPC & High Death Rate",
                                               ifelse(AAPC >= -1.91 & AAPC < -1.5 & val < 81.20, "Medium AAPC & Low Death Rate",
                                                      ifelse(AAPC >= -1.91 & AAPC < -1.5 & val >= 81.20 & val < 101.99,"Medium AAPC & Medium Death Rate",
                                                             ifelse(AAPC >= -1.91 & AAPC < -1.5 & val >= 101.99,"Medium AAPC & High Death Rate",
                                                                    ifelse(AAPC >=-1.5 & val < 81.20, "Low AAPC & Low Death Rate",
                                                                           ifelse(AAPC >= -1.5 & val >=81.20 & val < 101.99,"Low AAPC & Medium Death Rate",
                                                                                  ifelse(AAPC >= -1.5 & val >= 101.99, "Low AAPC & High Death Rate",NA))))))))))

wu_death$category <- factor(wu_death$category,levels = c("High AAPC & High Death Rate","High AAPC & Medium Death Rate","High AAPC & Low Death Rate",        
                                                         "Medium AAPC & High Death Rate","Medium AAPC & Medium Death Rate","Medium AAPC & Low Death Rate",
                                                         "Low AAPC & High Death Rate","Low AAPC & Medium Death Rate","Low AAPC & Low Death Rate"))
color_mapping <- c("High AAPC & Low Death Rate" = "#B3E2CD",   # 浅绿色
                   "High AAPC & Medium Death Rate" = "#66C2A4", # 中等绿色
                   "High AAPC & High Death Rate" = "#238B45",   # 深绿色
                   "Medium AAPC & Low Death Rate" = "#FDDBC7",  # 浅橙色
                   "Medium AAPC & Medium Death Rate" = "#FC9272",# 中等橙色
                   "Medium AAPC & High Death Rate" = "#D94801",  # 深橙色
                   "Low AAPC & Low Death Rate" = "#DEEBF7",      # 浅蓝色
                   "Low AAPC & Medium Death Rate" = "#9ECAE1",   # 中等蓝色
                   "Low AAPC & High Death Rate" = "#08519C")     # 深蓝色
wu_death_map <- ggplot(wu_death,aes(x=AAPC,y=val, color = category))+
  geom_point(size=4)+
  geom_text_repel(aes(label = location_name), vjust = 1.5, size = 6, family = "Arial", color = "black") +
  geom_hline(yintercept = val_quantiles, linetype = "dashed", color = "#ed0000e5") + 
  geom_vline(xintercept = AAPC_quantiles, linetype = "dashed", color = "#ed0000e5") +
  geom_hline(yintercept = val_quantiles1, linetype = "dashed", color = "#00468be5") + 
  geom_vline(xintercept = AAPC_quantiles1, linetype = "dashed", color = "#00468be5") +
  annotate("text", x = -2.55, y = val_quantiles[1], label = paste("Lower tercile", val_quantiles[1]), 
           vjust = -1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = -2.61, y = val_quantiles1, label = paste("Median", val_quantiles1), 
           vjust = -1, color = "#00468be5", size = 5, family = "Arial") +
  annotate("text", x = -2.55, y = val_quantiles[2], label = paste("Upper tercile", val_quantiles[2]), 
           vjust = -1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles[1], y = 120, label = paste("Lower tercile", AAPC_quantiles[1]), 
           hjust = 1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles1, y = 125, label = paste("Median", AAPC_quantiles1), 
           hjust = 1, color = "#00468be5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles[2], y = 120, label = paste("Upper tercile", AAPC_quantiles[2]), 
           hjust = 1, color = "#ed0000e5", size = 5, family = "Arial") +
  scale_color_manual(values = color_mapping, name="") +
  scale_y_continuous(limits = c(0,140), breaks = c(0,20,40,60,80,100,120,140))+
  labs(x = "Average annual percentage change(%)",
       y = "Age-standardized Death Rate(per 100,000 population)")+
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
        axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
        axis.text = element_text(size = 20, family = "Arial",color = "black"),
        plot.title = element_text(size = 20, family = "Arial",color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.title = element_text(size = 20, family = "Arial",color = "black"),
        legend.text = element_text(size = 15, family = "Arial",color = "black"),
        legend.position = c(1, 0),  # 图例放置在右下角
        legend.justification = c(1, 0),
        legend.key.height = unit(1.3, 'cm'))  # 调整图例的行间距
ggsave(wu_death_map,file="wu_death_map.tif",width = 13,height = 11,dpi = 300)


###################################################################################1
wu_death <- allrisks[,c(1,3,4)]
val_quantiles <- quantile(wu_death$Deaths_Both_Rate_2023, probs = c(1/3, 1/2, 2/3))
val_quantiles


AAPC_quantiles <- quantile(wu_death$Deaths_rate_Both_change, probs = c(1/3,1/2, 2/3))
AAPC_quantiles

val_quantiles <- c(50.32,86.06)  
AAPC_quantiles <- c(-52.18, -39.86) 

val_quantiles1 <- c(64.61)  
AAPC_quantiles1 <- c(-43.94) 

# 创建分类变量，根据 AAPC 和 YLL 的值将数据分成不同象限
wu_death$category <- with(wu_death, 
                          ifelse(Deaths_rate_Both_change < -52.18 & Deaths_Both_Rate_2023 < 50.32, "High Percentage Change & Low Death Rate",
                                 ifelse(Deaths_rate_Both_change < -52.18 & Deaths_Both_Rate_2023 >= 50.32 & Deaths_Both_Rate_2023 <86.06, "High Percentage Change & Medium Death Rate",
                                        ifelse(Deaths_rate_Both_change < -52.18 & Deaths_Both_Rate_2023 >= 86.06, "High Percentage Change & High Death Rate",
                                               ifelse(Deaths_rate_Both_change >= -52.18 & Deaths_rate_Both_change < -39.86 & Deaths_Both_Rate_2023 < 50.32, "Medium Percentage Change & Low Death Rate",
                                                      ifelse(Deaths_rate_Both_change >= -52.18 & Deaths_rate_Both_change < -39.86 & Deaths_Both_Rate_2023 >= 50.32 & Deaths_Both_Rate_2023 < 86.06,"Medium Percentage Change & Medium Death Rate",
                                                             ifelse(Deaths_rate_Both_change >= -52.18 & Deaths_rate_Both_change < -39.86 & Deaths_Both_Rate_2023 >= 86.06,"Medium Percentage Change & High Death Rate",
                                                                    ifelse(Deaths_rate_Both_change >=-39.86 & Deaths_Both_Rate_2023 < 50.32, "Low Percentage Change & Low Death Rate",
                                                                           ifelse(Deaths_rate_Both_change >= -39.86 & Deaths_Both_Rate_2023 >=50.32 & Deaths_Both_Rate_2023 < 86.06,"Low Percentage Change & Medium Death Rate",
                                                                                  ifelse(Deaths_rate_Both_change >= -39.86 & Deaths_Both_Rate_2023 >= 86.06, "Low Percentage Change & High Death Rate",NA))))))))))

wu_death$category <- factor(wu_death$category,levels = c("High Percentage Change & High Death Rate","High Percentage Change & Medium Death Rate","High Percentage Change & Low Death Rate",        
                                                         "Medium Percentage Change & High Death Rate","Medium Percentage Change & Medium Death Rate","Medium Percentage Change & Low Death Rate",
                                                         "Low Percentage Change & High Death Rate","Low Percentage Change & Medium Death Rate","Low Percentage Change & Low Death Rate"))
color_mapping <- c("High Percentage Change & Low Death Rate" = "#B3E2CD",   # 浅绿色
                   "High Percentage Change & Medium Death Rate" = "#66C2A4", # 中等绿色
                   "High Percentage Change & High Death Rate" = "#238B45",   # 深绿色
                   "Medium Percentage Change & Low Death Rate" = "#FDDBC7",  # 浅橙色
                   "Medium Percentage Change & Medium Death Rate" = "#FC9272",# 中等橙色
                   "Medium Percentage Change & High Death Rate" = "#D94801",  # 深橙色
                   "Low Percentage Change & Low Death Rate" = "#DEEBF7",      # 浅蓝色
                   "Low Percentage Change & Medium Death Rate" = "#9ECAE1",   # 中等蓝色
                   "Low Percentage Change & High Death Rate" = "#08519C")     # 深蓝色
wu_death_map <- ggplot(wu_death,aes(x=Deaths_rate_Both_change,y=Deaths_Both_Rate_2023, color = category))+
  geom_point(size=4)+
  geom_text_repel(aes(label = location_name), vjust = 1.5, size = 6, family = "Arial", color = "black") +
  geom_hline(yintercept = val_quantiles, linetype = "dashed", color = "#ed0000e5") + 
  geom_vline(xintercept = AAPC_quantiles, linetype = "dashed", color = "#ed0000e5") +
  geom_hline(yintercept = val_quantiles1, linetype = "dashed", color = "#00468be5") + 
  geom_vline(xintercept = AAPC_quantiles1, linetype = "dashed", color = "#00468be5") +
  annotate("text", x = -2.55, y = val_quantiles[1], label = paste("Lower tercile", val_quantiles[1]), 
           vjust = -1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = -2.61, y = val_quantiles1, label = paste("Median", val_quantiles1), 
           vjust = -1, color = "#00468be5", size = 5, family = "Arial") +
  annotate("text", x = -3, y = val_quantiles[2], label = paste("Upper tercile", val_quantiles[2]), 
           vjust = -1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles[1], y = 120, label = paste("Lower tercile", AAPC_quantiles[1]), 
           hjust = 1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles1, y = 125, label = paste("Median", AAPC_quantiles1), 
           hjust = 1, color = "#00468be5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles[2], y = 120, label = paste("Upper tercile", AAPC_quantiles[2]), 
           hjust = 1, color = "#ed0000e5", size = 5, family = "Arial") +
  scale_color_manual(values = color_mapping, name="") +
  scale_y_continuous(limits = c(0,180), breaks = c(0,180, seq(0,180,by=30)))+
  labs(x = "Percentage Change (%)",
       y = "Age-standardized Death Rate (per 100,000 population)")+
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
        axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
        axis.text = element_text(size = 20, family = "Arial",color = "black"),
        plot.title = element_text(size = 20, family = "Arial",color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.title = element_text(size = 20, family = "Arial",color = "black"),
        legend.text = element_text(size = 15, family = "Arial",color = "black"),
        legend.position = c(1, 0.02),  # 图例放置在右下角
        legend.justification = c(1, 0),
        legend.key.height = unit(0.8, 'cm'))  # 调整图例的行间距
ggsave(wu_death_map,file="wu_death_map.png",width = 15,height = 11,dpi = 300)
###DALY----
wu_DALY <- allrisks[,c(1,5,6)]
val_quantiles <- quantile(wu_DALY$DALYs_Both_Rate_2023, probs = c(1/3,1/2, 2/3))
val_quantiles


AAPC_quantiles <- quantile(wu_DALY$DALYs_rate_Both_change, probs = c(1/3,1/2, 2/3))
AAPC_quantiles

val_quantiles <- c(1092.51,1931.71)
AAPC_quantiles <- c(-45.40,-31.83)

val_quantiles1 <- c(1549.05)
AAPC_quantiles1 <- c(-43.76)
# 创建分类变量，根据 AAPC 和 YLL 的值将数据分成不同象限
wu_DALY$category <- with(wu_DALY, 
                         ifelse(DALYs_rate_Both_change < -45.40 & DALYs_Both_Rate_2023 < 1092.51, "High Percentage Change & Low DALYs Rate",
                                ifelse(DALYs_rate_Both_change < -45.40 & DALYs_Both_Rate_2023 >= 1092.51 & DALYs_Both_Rate_2023 <1931.71, "High Percentage Change & Medium DALYs Rate",
                                       ifelse(DALYs_rate_Both_change < -45.40 & DALYs_Both_Rate_2023 >= 1931.71, "High Percentage Change & High DALYs Rate",
                                              ifelse(DALYs_rate_Both_change >= -45.40 & DALYs_rate_Both_change < -31.83 & DALYs_Both_Rate_2023 < 1092.51, "Medium Percentage Change & Low DALYs Rate",
                                                     ifelse(DALYs_rate_Both_change >= -45.40 & DALYs_rate_Both_change < -31.83 & DALYs_Both_Rate_2023 >= 1092.51 & DALYs_Both_Rate_2023 < 1931.71,"Medium Percentage Change & Medium DALYs Rate",
                                                            ifelse(DALYs_rate_Both_change >= -45.40 & DALYs_rate_Both_change < -31.83 & DALYs_Both_Rate_2023 >= 1931.71,"Medium Percentage Change & High DALYs Rate",
                                                                   ifelse(DALYs_rate_Both_change >=-31.83 & DALYs_Both_Rate_2023 < 1092.51, "Low Percentage Change & Low DALYs Rate",
                                                                          ifelse(DALYs_rate_Both_change >= -31.83 & DALYs_Both_Rate_2023 >=1092.51 & DALYs_Both_Rate_2023 < 1931.71,"Low Percentage Change & Medium DALYs Rate",
                                                                                 ifelse(DALYs_rate_Both_change >= -31.83 & DALYs_Both_Rate_2023 >= 1931.71, "Low Percentage Change & High DALYs Rate",NA))))))))))

wu_DALY$category <- factor(wu_DALY$category,levels = c("High Percentage Change & High DALYs Rate","High Percentage Change & Medium DALYs Rate","High Percentage Change & Low DALYs Rate",        
                                                       "Medium Percentage Change & High DALYs Rate","Medium Percentage Change & Medium DALYs Rate","Medium Percentage Change & Low DALYs Rate",
                                                       "Low Percentage Change & High DALYs Rate","Low Percentage Change & Medium DALYs Rate","Low Percentage Change & Low DALYs Rate"))
color_mapping <- c("High Percentage Change & Low DALYs Rate" = "#B3E2CD",   # 浅绿色
                   "High Percentage Change & Medium DALYs Rate" = "#66C2A4", # 中等绿色
                   "High Percentage Change & High DALYs Rate" = "#238B45",   # 深绿色
                   "Medium Percentage Change & Low DALYs Rate" = "#FDDBC7",  # 浅橙色
                   "Medium Percentage Change & Medium DALYs Rate" = "#FC9272",# 中等橙色
                   "Medium Percentage Change & High DALYs Rate" = "#D94801",  # 深橙色
                   "Low Percentage Change & Low DALYs Rate" = "#DEEBF7",      # 浅蓝色
                   "Low Percentage Change & Medium DALYs Rate" = "#9ECAE1",   # 中等蓝色
                   "Low Percentage Change & High DALYs Rate" = "#08519C")     # 深蓝色
wu_DALY_map <- ggplot(wu_DALY,aes(x=DALYs_rate_Both_change,y=DALYs_Both_Rate_2023, color = category)) +
  geom_point(size=4) +
  geom_text_repel(aes(label = location_name), vjust = 1.5, size = 6, family = "Arial", color = "black") +
  geom_hline(yintercept = val_quantiles, linetype = "dashed", color = "#ed0000e5") + 
  geom_vline(xintercept = AAPC_quantiles, linetype = "dashed", color = "#ed0000e5") +
  geom_hline(yintercept = val_quantiles1, linetype = "dashed", color = "#00468be5") + 
  geom_vline(xintercept = AAPC_quantiles1, linetype = "dashed", color = "#00468be5") +
  annotate("text", x = -2.55, y = val_quantiles[1], label = paste("Lower tercile", val_quantiles[1]), 
           vjust = -1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = -2.55, y = val_quantiles1, label = paste("Median", val_quantiles1), 
           vjust = -1, color = "#00468be5", size = 5, family = "Arial") +
  annotate("text", x = -2.55, y = val_quantiles[2], label = paste("Upper tercile", val_quantiles[2]), 
           vjust = -1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles[1], y = 400, label = paste("Lower tercile", AAPC_quantiles[1]), 
           hjust = 1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles1, y = 500, label = paste("Median", AAPC_quantiles1), 
           hjust = 1, color = "#00468be5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles[2], y = 400, label = paste("Upper tercile", AAPC_quantiles[2]), 
           hjust = 1, color = "#ed0000e5", size = 5, family = "Arial") +
  scale_color_manual(values = color_mapping, name="") +
  scale_y_continuous(limits = c(0,4000), breaks = c(0,1000,2000,3000,4000)) +
  labs(x = "Average annual percentage change(%)",
       y = "Age-standardized DALYs Rate(per 100,000 population)") +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
        axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
        axis.text = element_text(size = 20, family = "Arial",color = "black"),
        plot.title = element_text(size = 20, family = "Arial",color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.title = element_text(size = 20, family = "Arial",color = "black"),
        legend.text = element_text(size = 15, family = "Arial",color = "black"),
        legend.position = c(1, 0.02),  # 图例放置在右下角
        legend.justification = c(1, 0),
        legend.key.height = unit(0.8, 'cm'))
ggsave(wu_DALY_map,file="wu_DALY_map.png",width = 15,height = 11,dpi = 300)


# 国际对比的点图2 ----
##data ----
allrisk_cause <- read.csv(file = "D:/论文/GBD/合并数据/Diet risk factors.csv")
allrisk_cause1 <- filter(allrisk_cause,allrisk_cause$measure%in%c("Deaths","DALYs")&
                           allrisk_cause$year%in%c("1990","2023")&
                           allrisk_cause$sex=="Both"&
                           allrisk_cause$metric=="Rate"&
                           allrisk_cause$rei=="Dietary risks"&
                           allrisk_cause$cause%in%c("Non-communicable diseases","Cardiovascular diseases","Neoplasms","Diabetes and kidney diseases")&
                           allrisk_cause$location=="China"&
                           allrisk_cause$age=="Age-standardized")
allrisk_cause2 <- allrisk_cause[allrisk_cause1$cause=="Non-communicable diseases" & allrisk_cause1$measure=="Deaths",]
allrisk_cause2 <- allrisk_cause2 %>% 
  mutate(year = as.numeric(year),  # 先转换为字符型
         year = case_when(
           year == 2023 ~ 2021,
           TRUE ~ year
         )) %>% 
  select(location, measure, year, val)

compare1 <- read.csv(file = "D:/论文/GBD/Diet补充国家数据/diet补充数据.csv")
compare1 <- filter(compare1,compare1$measure %in% c("Deaths","DALYs (Disability-Adjusted Life Years)")&
                     compare1$year%in%c("1990","2021")&
                     compare1$sex=="Both"&
                     compare1$metric=="Rate"&
                     compare1$cause%in%c("Non-communicable diseases","Cardiovascular diseases","Neoplasms","Diabetes and kidney diseases")&
                     compare1$location!="People's Republic of China"&
                     compare1$age=="Age-standardized")
compare1 <- compare1 %>% 
  mutate(measure = case_when(
    measure == "DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
    TRUE ~ measure
  ))
compare1 <- compare1[compare1$cause=="Neoplasms" & compare1$measure=="Deaths",]
compare1 <- compare1 %>% 
  select(location,measure,year,val)

compare_combined <- bind_rows(allrisk_cause2,compare1)


compare2 <- dcast(compare_combined,location + measure ~ year,value.var = "val")

compare2 <- compare2 %>% 
  mutate(change = (`2021`-`1990`)/`1990`) %>% 
  mutate(change = change*100) %>% 
  mutate(`2021`= round(`2021`,digits = 2),
         change = round(change,digits = 2))

###death----
# val_quantiles <- quantile(compare2$`2021`, probs = c(1/3,1/2, 2/3))
# val_quantiles
# 
# 
# change_quantiles <- quantile(compare2$change, probs = c(1/3,1/2, 2/3))
# change_quantiles
# 
# val_quantiles <- c(52.68,92.82)
# change_quantiles <- c(-52.29, -37.19)
# 
# val_quantiles1 <- c(66.92)
# change_quantiles1 <- c(-43.94)

# # 创建分类变量，根据 AAPC 和 YLL 的值将数据分成不同象限
# wu_death$category <- with(wu_death, 
#                           ifelse(AAPC < -1.91 & val < 81.20, "High AAPC & Low Death Rate",
#                                  ifelse(AAPC < -1.91 & val >= 81.20 & val <101.99, "High AAPC & Medium Death Rate",
#                                         ifelse(AAPC < -1.91 & val >= 101.99, "High AAPC & High Death Rate",
#                                                ifelse(AAPC >= -1.91 & AAPC < -1.5 & val < 81.20, "Medium AAPC & Low Death Rate",
#                                                       ifelse(AAPC >= -1.91 & AAPC < -1.5 & val >= 81.20 & val < 101.99,"Medium AAPC & Medium Death Rate",
#                                                              ifelse(AAPC >= -1.91 & AAPC < -1.5 & val >= 101.99,"Medium AAPC & High Death Rate",
#                                                                     ifelse(AAPC >=-1.5 & val < 81.20, "Low AAPC & Low Death Rate",
#                                                                            ifelse(AAPC >= -1.5 & val >=81.20 & val < 101.99,"Low AAPC & Medium Death Rate",
#                                                                                   ifelse(AAPC >= -1.5 & val >= 101.99, "Low AAPC & High Death Rate",NA))))))))))
# 
# wu_death$category <- factor(wu_death$category,levels = c("High AAPC & High Death Rate","High AAPC & Medium Death Rate","High AAPC & Low Death Rate",        
#                                                          "Medium AAPC & High Death Rate","Medium AAPC & Medium Death Rate","Medium AAPC & Low Death Rate",
#                                                           "Low AAPC & High Death Rate","Low AAPC & Medium Death Rate","Low AAPC & Low Death Rate"))
# color_mapping <- c("High AAPC & Low Death Rate" = "#B3E2CD",   # 浅绿色
#                    "High AAPC & Medium Death Rate" = "#66C2A4", # 中等绿色
#                    "High AAPC & High Death Rate" = "#238B45",   # 深绿色
#                    "Medium AAPC & Low Death Rate" = "#FDDBC7",  # 浅橙色
#                    "Medium AAPC & Medium Death Rate" = "#FC9272",# 中等橙色
#                    "Medium AAPC & High Death Rate" = "#D94801",  # 深橙色
#                    "Low AAPC & Low Death Rate" = "#DEEBF7",      # 浅蓝色
#                    "Low AAPC & Medium Death Rate" = "#9ECAE1",   # 中等蓝色
#                    "Low AAPC & High Death Rate" = "#08519C")     # 深蓝色
compare2$color_intensity <- with(compare2, {
  # 标准化change变量（0-1范围）
  scaled_change <- (change - min(change, na.rm = TRUE)) / 
    (max(change, na.rm = TRUE) - min(change, na.rm = TRUE))
  
  # 标准化2021变量（0-1范围）
  scaled_2021 <- (`2021` - min(`2021`, na.rm = TRUE)) / 
    (max(`2021`, na.rm = TRUE) - min(`2021`, na.rm = TRUE))
  
  # 综合评分 - 离右上角(1,1)越近，值越大
  distance_to_topright <- sqrt((1 - scaled_change)^2 + (1 - scaled_2021)^2)
  1 - distance_to_topright  # 反转：距离越小，值越大
})

# 创建散点图
compare_map_death_cancer <- ggplot(compare2, aes(x = change, y = `2021`, color = color_intensity)) +
  geom_point(size = 6, alpha = 0.8) +  # 设置点的大小和透明度
  # 创建红色渐变颜色：color_intensity值越大颜色越深（越红）
  scale_color_gradient(low = "#FFC0CB",  # 浅粉色
                       high = "#8B0000", guide = FALSE # 右上角颜色最深
  ) +
  # 为关键国家添加标签
  geom_label_repel(aes(label = location),
                   size = 5,
                   color = "black",
                   fill = alpha("white", 0.7),  # 半透明白色背景
                   box.padding = 0.5,
                   max.overlaps = Inf,  # 允许无限重叠
                   segment.color = "grey50",
                   min.segment.length = 0.2) +
  labs(x = "Percentage change(%)",
       y = "Age-standardized Deaths Rate(per 100,000 population)")+
  # scale_x_continuous(limits = c(-80,20), breaks = seq(-80,20,by=20))+
  scale_y_continuous(limits = c(3,17), breaks = seq(3,17,by=2))+
  # 主题美化
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
        axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
        axis.text = element_text(size = 20, family = "Arial",color = "black"),
        plot.title = element_text(size = 20, family = "Arial",color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.title = element_text(size = 20, family = "Arial",color = "black"),
        legend.text = element_text(size = 15, family = "Arial",color = "black"),
        legend.position = c(1, 0),  # 图例放置在右下角
        legend.justification = c(1, 0),
        legend.key.height = unit(1.3, 'cm'))  # 调整图例的行间距
ggsave(compare_map_death_cancer,file="~/China GBD/GBD2023/Figure/compare_map_death_cancer.png",width = 12,height = 10,dpi = 300)

compare_map_DALY_diabeteandkidney <- ggplot(compare2, aes(x = change, y = `2021`, color = color_intensity)) +
  geom_point(size = 6, alpha = 0.8) +  # 设置点的大小和透明度
  # 创建红色渐变颜色：color_intensity值越大颜色越深（越红）
  scale_color_gradient(low = "#FFC0CB",  # 浅粉色
                       high = "#8B0000", guide = FALSE # 右上角颜色最深
  ) +
  # 为关键国家添加标签
  geom_label_repel(aes(label = location),
                   size = 5,
                   color = "black",
                   fill = alpha("white", 0.7),  # 半透明白色背景
                   box.padding = 0.5,
                   max.overlaps = Inf,  # 允许无限重叠
                   segment.color = "grey50",
                   min.segment.length = 0.2) +
  labs(x = "Percentage change(%)",
       y = "Age-standardized DALYs Rate(per 100,000 population)")+
  scale_x_continuous(limits = c(-20,100), breaks = seq(-20,100,by=20))+
  scale_y_continuous(limits = c(100,900), breaks = seq(100,900,by=200))+
  # 主题美化
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
        axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
        axis.text = element_text(size = 20, family = "Arial",color = "black"),
        plot.title = element_text(size = 20, family = "Arial",color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.title = element_text(size = 20, family = "Arial",color = "black"),
        legend.text = element_text(size = 15, family = "Arial",color = "black"),
        legend.position = c(1, 0),  # 图例放置在右下角
        legend.justification = c(1, 0),
        legend.key.height = unit(1.3, 'cm'))  # 调整图例的行间距
ggsave(compare_map_DALY_diabeteandkidney,file="~/China GBD/GBD2023/Figure/compare_map_DALY_diabeteandkidney.png",width = 12,height = 10,dpi = 300)

# wu_death_map <- ggplot(wu_death,aes(x=AAPC,y=val, color = category))+
#   geom_point(size=4)+
#   geom_text_repel(aes(label = location_name), vjust = 1.5, size = 6, family = "Arial", color = "black") +
#   geom_hline(yintercept = val_quantiles, linetype = "dashed", color = "#ed0000e5") + 
#   geom_vline(xintercept = AAPC_quantiles, linetype = "dashed", color = "#ed0000e5") +
#   geom_hline(yintercept = val_quantiles1, linetype = "dashed", color = "#00468be5") + 
#   geom_vline(xintercept = AAPC_quantiles1, linetype = "dashed", color = "#00468be5") +
#   annotate("text", x = -2.55, y = val_quantiles[1], label = paste("Lower tercile", val_quantiles[1]), 
#            vjust = -1, color = "#ed0000e5", size = 5, family = "Arial") +
#   annotate("text", x = -2.61, y = val_quantiles1, label = paste("Median", val_quantiles1), 
#            vjust = -1, color = "#00468be5", size = 5, family = "Arial") +
#   annotate("text", x = -2.55, y = val_quantiles[2], label = paste("Upper tercile", val_quantiles[2]), 
#            vjust = -1, color = "#ed0000e5", size = 5, family = "Arial") +
#   annotate("text", x = AAPC_quantiles[1], y = 120, label = paste("Lower tercile", AAPC_quantiles[1]), 
#            hjust = 1, color = "#ed0000e5", size = 5, family = "Arial") +
#   annotate("text", x = AAPC_quantiles1, y = 125, label = paste("Median", AAPC_quantiles1), 
#            hjust = 1, color = "#00468be5", size = 5, family = "Arial") +
#   annotate("text", x = AAPC_quantiles[2], y = 120, label = paste("Upper tercile", AAPC_quantiles[2]), 
#            hjust = 1, color = "#ed0000e5", size = 5, family = "Arial") +
#   scale_color_manual(values = color_mapping, name="") +
#   scale_y_continuous(limits = c(0,140), breaks = c(0,20,40,60,80,100,120,140))+
#   labs(x = "Average annual percentage change(%)",
#        y = "Age-standardized Death Rate(per 100,000 population)")+
#   theme(panel.background = element_rect(fill = "white", colour = NA),  
#         plot.background = element_rect(fill = "white", colour = NA),
#         axis.line = element_line(color = "black", linewidth = 0.7),  
#         axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
#         axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
#         axis.text = element_text(size = 20, family = "Arial",color = "black"),
#         plot.title = element_text(size = 20, family = "Arial",color = "black"),
#         strip.background = element_blank(),
#         strip.text = element_text(size = 20, family = "Arial",color = "black"),
#         legend.title = element_text(size = 20, family = "Arial",color = "black"),
#         legend.text = element_text(size = 15, family = "Arial",color = "black"),
#         legend.position = c(1, 0),  # 图例放置在右下角
#         legend.justification = c(1, 0),
#         legend.key.height = unit(1.3, 'cm'))  # 调整图例的行间距
# ggsave(wu_death_map,file="wu_death_map.tif",width = 13,height = 11,dpi = 300)


###################################################################################1
wu_death <- allrisks[,c(1,3,4)]
val_quantiles <- quantile(wu_death$Deaths_Both_Rate_2023, probs = c(1/3, 1/2, 2/3))
val_quantiles


AAPC_quantiles <- quantile(wu_death$Deaths_rate_Both_change, probs = c(1/3,1/2, 2/3))
AAPC_quantiles

val_quantiles <- c(50.32,86.06)  
AAPC_quantiles <- c(-52.18, -39.86) 

val_quantiles1 <- c(64.61)  
AAPC_quantiles1 <- c(-43.94) 

# 创建分类变量，根据 AAPC 和 YLL 的值将数据分成不同象限
wu_death$category <- with(wu_death, 
                          ifelse(Deaths_rate_Both_change < -52.18 & Deaths_Both_Rate_2023 < 50.32, "High Percentage Change & Low Death Rate",
                                 ifelse(Deaths_rate_Both_change < -52.18 & Deaths_Both_Rate_2023 >= 50.32 & Deaths_Both_Rate_2023 <86.06, "High Percentage Change & Medium Death Rate",
                                        ifelse(Deaths_rate_Both_change < -52.18 & Deaths_Both_Rate_2023 >= 86.06, "High Percentage Change & High Death Rate",
                                               ifelse(Deaths_rate_Both_change >= -52.18 & Deaths_rate_Both_change < -39.86 & Deaths_Both_Rate_2023 < 50.32, "Medium Percentage Change & Low Death Rate",
                                                      ifelse(Deaths_rate_Both_change >= -52.18 & Deaths_rate_Both_change < -39.86 & Deaths_Both_Rate_2023 >= 50.32 & Deaths_Both_Rate_2023 < 86.06,"Medium Percentage Change & Medium Death Rate",
                                                             ifelse(Deaths_rate_Both_change >= -52.18 & Deaths_rate_Both_change < -39.86 & Deaths_Both_Rate_2023 >= 86.06,"Medium Percentage Change & High Death Rate",
                                                                    ifelse(Deaths_rate_Both_change >=-39.86 & Deaths_Both_Rate_2023 < 50.32, "Low Percentage Change & Low Death Rate",
                                                                           ifelse(Deaths_rate_Both_change >= -39.86 & Deaths_Both_Rate_2023 >=50.32 & Deaths_Both_Rate_2023 < 86.06,"Low Percentage Change & Medium Death Rate",
                                                                                  ifelse(Deaths_rate_Both_change >= -39.86 & Deaths_Both_Rate_2023 >= 86.06, "Low Percentage Change & High Death Rate",NA))))))))))

wu_death$category <- factor(wu_death$category,levels = c("High Percentage Change & High Death Rate","High Percentage Change & Medium Death Rate","High Percentage Change & Low Death Rate",        
                                                         "Medium Percentage Change & High Death Rate","Medium Percentage Change & Medium Death Rate","Medium Percentage Change & Low Death Rate",
                                                         "Low Percentage Change & High Death Rate","Low Percentage Change & Medium Death Rate","Low Percentage Change & Low Death Rate"))
color_mapping <- c("High Percentage Change & Low Death Rate" = "#B3E2CD",   # 浅绿色
                   "High Percentage Change & Medium Death Rate" = "#66C2A4", # 中等绿色
                   "High Percentage Change & High Death Rate" = "#238B45",   # 深绿色
                   "Medium Percentage Change & Low Death Rate" = "#FDDBC7",  # 浅橙色
                   "Medium Percentage Change & Medium Death Rate" = "#FC9272",# 中等橙色
                   "Medium Percentage Change & High Death Rate" = "#D94801",  # 深橙色
                   "Low Percentage Change & Low Death Rate" = "#DEEBF7",      # 浅蓝色
                   "Low Percentage Change & Medium Death Rate" = "#9ECAE1",   # 中等蓝色
                   "Low Percentage Change & High Death Rate" = "#08519C")     # 深蓝色
wu_death_map <- ggplot(wu_death,aes(x=Deaths_rate_Both_change,y=Deaths_Both_Rate_2023, color = category))+
  geom_point(size=4)+
  geom_text_repel(aes(label = location_name), vjust = 1.5, size = 6, family = "Arial", color = "black") +
  geom_hline(yintercept = val_quantiles, linetype = "dashed", color = "#ed0000e5") + 
  geom_vline(xintercept = AAPC_quantiles, linetype = "dashed", color = "#ed0000e5") +
  geom_hline(yintercept = val_quantiles1, linetype = "dashed", color = "#00468be5") + 
  geom_vline(xintercept = AAPC_quantiles1, linetype = "dashed", color = "#00468be5") +
  annotate("text", x = -2.55, y = val_quantiles[1], label = paste("Lower tercile", val_quantiles[1]), 
           vjust = -1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = -2.61, y = val_quantiles1, label = paste("Median", val_quantiles1), 
           vjust = -1, color = "#00468be5", size = 5, family = "Arial") +
  annotate("text", x = -3, y = val_quantiles[2], label = paste("Upper tercile", val_quantiles[2]), 
           vjust = -1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles[1], y = 120, label = paste("Lower tercile", AAPC_quantiles[1]), 
           hjust = 1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles1, y = 125, label = paste("Median", AAPC_quantiles1), 
           hjust = 1, color = "#00468be5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles[2], y = 120, label = paste("Upper tercile", AAPC_quantiles[2]), 
           hjust = 1, color = "#ed0000e5", size = 5, family = "Arial") +
  scale_color_manual(values = color_mapping, name="") +
  scale_y_continuous(limits = c(0,180), breaks = c(0,180, seq(0,180,by=30)))+
  labs(x = "Percentage Change (%)",
       y = "Age-standardized Death Rate (per 100,000 population)")+
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
        axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
        axis.text = element_text(size = 20, family = "Arial",color = "black"),
        plot.title = element_text(size = 20, family = "Arial",color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.title = element_text(size = 20, family = "Arial",color = "black"),
        legend.text = element_text(size = 15, family = "Arial",color = "black"),
        legend.position = c(1, 0.02),  # 图例放置在右下角
        legend.justification = c(1, 0),
        legend.key.height = unit(0.8, 'cm'))  # 调整图例的行间距
ggsave(wu_death_map,file="wu_death_map.png",width = 15,height = 11,dpi = 300)
###DALY----
wu_DALY <- allrisks[,c(1,5,6)]
val_quantiles <- quantile(wu_DALY$DALYs_Both_Rate_2023, probs = c(1/3,1/2, 2/3))
val_quantiles


AAPC_quantiles <- quantile(wu_DALY$DALYs_rate_Both_change, probs = c(1/3,1/2, 2/3))
AAPC_quantiles

val_quantiles <- c(1092.51,1931.71)
AAPC_quantiles <- c(-45.40,-31.83)

val_quantiles1 <- c(1549.05)
AAPC_quantiles1 <- c(-43.76)
# 创建分类变量，根据 AAPC 和 YLL 的值将数据分成不同象限
wu_DALY$category <- with(wu_DALY, 
                         ifelse(DALYs_rate_Both_change < -45.40 & DALYs_Both_Rate_2023 < 1092.51, "High Percentage Change & Low DALYs Rate",
                                ifelse(DALYs_rate_Both_change < -45.40 & DALYs_Both_Rate_2023 >= 1092.51 & DALYs_Both_Rate_2023 <1931.71, "High Percentage Change & Medium DALYs Rate",
                                       ifelse(DALYs_rate_Both_change < -45.40 & DALYs_Both_Rate_2023 >= 1931.71, "High Percentage Change & High DALYs Rate",
                                              ifelse(DALYs_rate_Both_change >= -45.40 & DALYs_rate_Both_change < -31.83 & DALYs_Both_Rate_2023 < 1092.51, "Medium Percentage Change & Low DALYs Rate",
                                                     ifelse(DALYs_rate_Both_change >= -45.40 & DALYs_rate_Both_change < -31.83 & DALYs_Both_Rate_2023 >= 1092.51 & DALYs_Both_Rate_2023 < 1931.71,"Medium Percentage Change & Medium DALYs Rate",
                                                            ifelse(DALYs_rate_Both_change >= -45.40 & DALYs_rate_Both_change < -31.83 & DALYs_Both_Rate_2023 >= 1931.71,"Medium Percentage Change & High DALYs Rate",
                                                                   ifelse(DALYs_rate_Both_change >=-31.83 & DALYs_Both_Rate_2023 < 1092.51, "Low Percentage Change & Low DALYs Rate",
                                                                          ifelse(DALYs_rate_Both_change >= -31.83 & DALYs_Both_Rate_2023 >=1092.51 & DALYs_Both_Rate_2023 < 1931.71,"Low Percentage Change & Medium DALYs Rate",
                                                                                 ifelse(DALYs_rate_Both_change >= -31.83 & DALYs_Both_Rate_2023 >= 1931.71, "Low Percentage Change & High DALYs Rate",NA))))))))))

wu_DALY$category <- factor(wu_DALY$category,levels = c("High Percentage Change & High DALYs Rate","High Percentage Change & Medium DALYs Rate","High Percentage Change & Low DALYs Rate",        
                                                       "Medium Percentage Change & High DALYs Rate","Medium Percentage Change & Medium DALYs Rate","Medium Percentage Change & Low DALYs Rate",
                                                       "Low Percentage Change & High DALYs Rate","Low Percentage Change & Medium DALYs Rate","Low Percentage Change & Low DALYs Rate"))
color_mapping <- c("High Percentage Change & Low DALYs Rate" = "#B3E2CD",   # 浅绿色
                   "High Percentage Change & Medium DALYs Rate" = "#66C2A4", # 中等绿色
                   "High Percentage Change & High DALYs Rate" = "#238B45",   # 深绿色
                   "Medium Percentage Change & Low DALYs Rate" = "#FDDBC7",  # 浅橙色
                   "Medium Percentage Change & Medium DALYs Rate" = "#FC9272",# 中等橙色
                   "Medium Percentage Change & High DALYs Rate" = "#D94801",  # 深橙色
                   "Low Percentage Change & Low DALYs Rate" = "#DEEBF7",      # 浅蓝色
                   "Low Percentage Change & Medium DALYs Rate" = "#9ECAE1",   # 中等蓝色
                   "Low Percentage Change & High DALYs Rate" = "#08519C")     # 深蓝色
wu_DALY_map <- ggplot(wu_DALY,aes(x=DALYs_rate_Both_change,y=DALYs_Both_Rate_2023, color = category)) +
  geom_point(size=4) +
  geom_text_repel(aes(label = location_name), vjust = 1.5, size = 6, family = "Arial", color = "black") +
  geom_hline(yintercept = val_quantiles, linetype = "dashed", color = "#ed0000e5") + 
  geom_vline(xintercept = AAPC_quantiles, linetype = "dashed", color = "#ed0000e5") +
  geom_hline(yintercept = val_quantiles1, linetype = "dashed", color = "#00468be5") + 
  geom_vline(xintercept = AAPC_quantiles1, linetype = "dashed", color = "#00468be5") +
  annotate("text", x = -2.55, y = val_quantiles[1], label = paste("Lower tercile", val_quantiles[1]), 
           vjust = -1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = -2.55, y = val_quantiles1, label = paste("Median", val_quantiles1), 
           vjust = -1, color = "#00468be5", size = 5, family = "Arial") +
  annotate("text", x = -2.55, y = val_quantiles[2], label = paste("Upper tercile", val_quantiles[2]), 
           vjust = -1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles[1], y = 400, label = paste("Lower tercile", AAPC_quantiles[1]), 
           hjust = 1, color = "#ed0000e5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles1, y = 500, label = paste("Median", AAPC_quantiles1), 
           hjust = 1, color = "#00468be5", size = 5, family = "Arial") +
  annotate("text", x = AAPC_quantiles[2], y = 400, label = paste("Upper tercile", AAPC_quantiles[2]), 
           hjust = 1, color = "#ed0000e5", size = 5, family = "Arial") +
  scale_color_manual(values = color_mapping, name="") +
  scale_y_continuous(limits = c(0,4000), breaks = c(0,1000,2000,3000,4000)) +
  labs(x = "Average annual percentage change(%)",
       y = "Age-standardized DALYs Rate(per 100,000 population)") +
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 20, family = "Arial",color = "black"),  
        axis.title.y = element_text(size = 20, family = "Arial",color = "black"),  
        axis.text = element_text(size = 20, family = "Arial",color = "black"),
        plot.title = element_text(size = 20, family = "Arial",color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, family = "Arial",color = "black"),
        legend.title = element_text(size = 20, family = "Arial",color = "black"),
        legend.text = element_text(size = 15, family = "Arial",color = "black"),
        legend.position = c(1, 0.02),  # 图例放置在右下角
        legend.justification = c(1, 0),
        legend.key.height = unit(0.8, 'cm'))
ggsave(wu_DALY_map,file="wu_DALY_map.png",width = 15,height = 11,dpi = 300)


# 自制热图 ----
allrisk_cause <- filter(dietary_annual1.0,dietary_annual1.0$rei_name%in%c("Diet low in fruits",                        
                                                                          "Diet low in legumes","Diet low in vegetables",                 
                                                                          "Diet high in red meat","Diet low in whole grains",               
                                                                          "Diet high in sodium","Diet low in milk",                       
                                                                          "Diet high in processed meat","Diet low in seafood omega-3 fatty acids",
                                                                          "Diet low in polyunsaturated fatty acids","Diet low in fiber",                      
                                                                          "Diet low in nuts and seeds","Diet high in sugar-sweetened beverages", 
                                                                          "Diet low in calcium","Diet high in trans fatty acids")&
                          dietary_annual1.0$cause_name == "Non-communicable diseases")
global <- read.csv("国际.csv")
global$measure_name[global$measure_name=="DALYs (Disability-Adjusted Life Years)"] <- c("DALYs")
global_risk <- global %>% 
  filter(rei_name%in%c("Diet low in fruits",                        
                       "Diet low in legumes","Diet low in vegetables",                 
                       "Diet high in red meat","Diet low in whole grains",               
                       "Diet high in sodium","Diet low in milk",                       
                       "Diet high in processed meat","Diet low in seafood omega-3 fatty acids",
                       "Diet low in polyunsaturated fatty acids","Diet low in fiber",                      
                       "Diet low in nuts and seeds","Diet high in sugar-sweetened beverages", 
                       "Diet low in calcium","Diet high in trans fatty acids")) %>% 
  filter(cause_name == "Non-communicable diseases") %>% 
  filter(location_name=="Global")
##death----
###2023----
death_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                       allrisk_cause$year=="2023"&
                       allrisk_cause$sex_name=="Both"&
                       allrisk_cause$metric_name=="Rate"&
                       allrisk_cause$age_name=="Age-standardized")
global_risk_death_2023 <- filter(global_risk,global_risk$measure_name=="Deaths"&
                                   global_risk$year=="2023"&
                                   global_risk$sex_name=="Both"&
                                   global_risk$metric_name=="Rate"&
                                   global_risk$age_name=="Age-standardized")
death_2023 <- rbind(death_2023,global_risk_death_2023)

death_2023 <- death_2023 %>% 
  select(location_name,rei_name,val) %>% 
  group_by(location_name) %>% 
  arrange(location_name,desc(val)) %>% 
  mutate(rank = as.factor(row_number())) %>%
  mutate(location_name=case_when(
    location_name=="Macao Special Administrative Region of China" ~ "Macao",
    location_name=="Hong Kong Special Administrative Region of China" ~ "Hong Kong",
    TRUE ~ location_name
  )) %>% 
  ungroup()

death_2023$location_name <- factor(death_2023$location_name,levels=c("China","Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", "Jilin", "Heilongjiang", 
                                                                     "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi", "Shandong", "Henan", 
                                                                     "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", "Chongqing", "Sichuan", "Guizhou", 
                                                                     "Yunnan", "Tibet", "Shaanxi", "Gansu", "Qinghai", "Ningxia", "Xinjiang", "Hong Kong", "Macao","Global"))
death_2023$rei_name <- factor(death_2023$rei_name,levels = c("Diet high in sugar-sweetened beverages",
                                                             "Diet high in trans fatty acids",
                                                             "Diet high in processed meat",
                                                             "Diet low in calcium",
                                                             "Diet low in milk",
                                                             "Diet low in vegetables",
                                                             "Diet high in red meat",
                                                             "Diet low in legumes",
                                                             "Diet low in seafood omega-3 fatty acids",
                                                             "Diet low in fiber",
                                                             "Diet low in nuts and seeds",
                                                             "Diet low in polyunsaturated fatty acids",
                                                             "Diet low in fruits",
                                                             "Diet low in whole grains",
                                                             "Diet high in sodium"))

death_heatmap_plot_2023 <- ggplot(death_2023, aes(x = location_name, y = rei_name, fill = rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = rank), size = 5, family = "Arial", color = "black") + 
  scale_fill_manual(values = c(
    "1" = "#B2182B",    # Dark red, most severe
    "2" = "#D73027",
    "3" = "#F46D43",
    "4" = "#FDAE61",
    "5" = "#FEE08B",
    "6" = "#FFFFBF",    # Light yellow
    "7" = "#D9EF8B",
    "8" = "#A6D96A",
    "9" = "#66BD63",
    "10" = "#1A9850",
    "11" = "#006837",   # Dark green, less severe
    "12" = "#80CDC1",
    "13" = "#35978F",
    "14" = "#01665E",
    "15" = "#003C30" ))+
  labs(x = "Location", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
        axis.title.y = element_text(size = 12, family = "Arial", color = "black"),  
        axis.text = element_text(size = 12, family = "Arial", color = "black"),
        plot.title = element_text(size = 16, family = "Arial", color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, family = "Arial", color = "black"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none")

ggsave(death_heatmap_plot_2023,file="death_heatmap_riskfactor_2023.tif",width = 14,height = 10,dpi = 300)
###1990----
death_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="Deaths"&
                       allrisk_cause$year=="1990"&
                       allrisk_cause$sex_name=="Both"&
                       allrisk_cause$metric_name=="Rate"&
                       allrisk_cause$age_name=="Age-standardized")
global_risk_death_1990 <- filter(global_risk,global_risk$measure_name=="Deaths"&
                                   global_risk$year=="1990"&
                                   global_risk$sex_name=="Both"&
                                   global_risk$metric_name=="Rate"&
                                   global_risk$age_name=="Age-standardized")
death_1990 <- rbind(death_1990,global_risk_death_1990)

death_1990 <- death_1990 %>% 
  select(location_name,rei_name,val) %>% 
  group_by(location_name) %>%
  arrange(location_name,desc(val)) %>% 
  mutate(rank = as.factor(row_number())) %>%
  mutate(location_name=case_when(
    location_name=="Macao Special Administrative Region of China" ~ "Macao",
    location_name=="Hong Kong Special Administrative Region of China" ~ "Hong Kong",
    TRUE ~ location_name
  )) %>% 
  ungroup()

death_1990$location_name <- factor(death_1990$location_name,levels=c("China","Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", "Jilin", "Heilongjiang", 
                                                                     "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi", "Shandong", "Henan", 
                                                                     "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", "Chongqing", "Sichuan", "Guizhou", 
                                                                     "Yunnan", "Tibet", "Shaanxi", "Gansu", "Qinghai", "Ningxia", "Xinjiang", 
                                                                     "Hong Kong", "Macao","Global"))


death_1990$rei_name <- factor(death_1990$rei_name,levels = c("Diet high in red meat",
                                                             "Diet high in sugar-sweetened beverages",
                                                             "Diet high in trans fatty acids",
                                                             "Diet high in processed meat",
                                                             "Diet low in calcium",
                                                             "Diet low in milk",
                                                             "Diet low in legumes",
                                                             "Diet low in polyunsaturated fatty acids",
                                                             "Diet low in seafood omega-3 fatty acids",
                                                             "Diet low in nuts and seeds",
                                                             "Diet low in fiber",
                                                             "Diet low in whole grains",
                                                             "Diet low in vegetables",
                                                             "Diet low in fruits",
                                                             "Diet high in sodium"))

# 翻转图像并将 China 移到最右边
death_heatmap_plot_1990 <- ggplot(death_1990, aes(x = location_name, y = rei_name, fill = rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = rank), size = 5, family = "Arial", color = "black") + 
  scale_fill_manual(values = c(
    "1" = "#B2182B",    # Dark red, most severe
    "2" = "#D73027",
    "3" = "#F46D43",
    "4" = "#FDAE61",
    "5" = "#FEE08B",
    "6" = "#FFFFBF",    # Light yellow
    "7" = "#D9EF8B",
    "8" = "#A6D96A",
    "9" = "#66BD63",
    "10" = "#1A9850",
    "11" = "#006837",   # Dark green, less severe
    "12" = "#80CDC1",
    "13" = "#35978F",
    "14" = "#01665E",
    "15" = "#003C30" )) +
  labs(x = "Location", y = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # 调整x轴标签的对齐方式
    axis.text.y.right = element_text(size = 12, family = "Arial", color = "black"),
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
    axis.title.y.right = element_text(size = 12, family = "Arial", color = "black"),  
    axis.text = element_text(size = 12, family = "Arial", color = "black"),
    plot.title = element_text(size = 16, family = "Arial", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, family = "Arial", color = "black"),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.position = "none",
    plot.margin = margin(r = 10, l = 50)  # 增加左侧边距
  ) +
  scale_x_discrete(limits = rev(levels(death_1990$location_name))) +  # 反转 X 轴
  scale_y_discrete(position = "right")  # 将y轴移到右侧

ggsave(death_heatmap_plot_1990,file="death_heatmap_riskfactor_1990.tif",width = 14,height = 10,dpi = 300)

##DALY----
###2023----
DALY_2023 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                      allrisk_cause$year=="2023"&
                      allrisk_cause$sex_name=="Both"&
                      allrisk_cause$metric_name=="Rate"&
                      allrisk_cause$age_name=="Age-standardized")
global_risk_DALY_2023 <- filter(global_risk,global_risk$measure_name=="DALYs"&
                                  global_risk$year=="2023"&
                                  global_risk$sex_name=="Both"&
                                  global_risk$metric_name=="Rate"&
                                  global_risk$age_name=="Age-standardized")
DALY_2023 <- rbind(DALY_2023,global_risk_DALY_2023)
DALY_2023 <- DALY_2023 %>% 
  select(location_name,rei_name,val) %>% 
  group_by(location_name) %>% 
  arrange(desc(val)) %>% 
  mutate(rank = as.factor(row_number())) %>%
  mutate(location_name=case_when(
    location_name=="Macao Special Administrative Region of China" ~ "Macao",
    location_name=="Hong Kong Special Administrative Region of China" ~ "Hong Kong",
    TRUE ~ location_name
  )) %>% 
  ungroup()

DALY_2023$location_name <- factor(DALY_2023$location_name,levels=c("China","Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", "Jilin", "Heilongjiang", 
                                                                   "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi", "Shandong", "Henan", 
                                                                   "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", "Chongqing", "Sichuan", "Guizhou", 
                                                                   "Yunnan", "Tibet", "Shaanxi", "Gansu", "Qinghai", "Ningxia", "Xinjiang", "Hong Kong", "Macao", "Global"))
DALY_2023$rei_name <- factor(DALY_2023$rei_name,levels = c("Diet high in trans fatty acids", 
                                                           "Diet high in sugar-sweetened beverages", 
                                                           "Diet low in calcium", 
                                                           "Diet low in vegetables", 
                                                           "Diet high in processed meat", 
                                                           "Diet low in milk", 
                                                           "Diet low in legumes", 
                                                           "Diet low in seafood omega-3 fatty acids", 
                                                           "Diet high in red meat", 
                                                           "Diet low in fiber", 
                                                           "Diet low in nuts and seeds", 
                                                           "Diet low in polyunsaturated fatty acids", 
                                                           "Diet low in fruits", 
                                                           "Diet low in whole grains", 
                                                           "Diet high in sodium"))

DALY_heatmap_plot_2023 <- ggplot(DALY_2023, aes(x = location_name, y = rei_name, fill = rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = rank), size = 5, family = "Arial", color = "black") + 
  scale_fill_manual(values = c(
    "1" = "#B2182B",    # Dark red, most severe
    "2" = "#D73027",
    "3" = "#F46D43",
    "4" = "#FDAE61",
    "5" = "#FEE08B",
    "6" = "#FFFFBF",    # Light yellow
    "7" = "#D9EF8B",
    "8" = "#A6D96A",
    "9" = "#66BD63",
    "10" = "#1A9850",
    "11" = "#006837",   # Dark green, less severe
    "12" = "#80CDC1",
    "13" = "#35978F",
    "14" = "#01665E",
    "15" = "#003C30" ))+
  labs(x = "Location", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
        axis.title.y = element_text(size = 12, family = "Arial", color = "black"),  
        axis.text = element_text(size = 12, family = "Arial", color = "black"),
        plot.title = element_text(size = 16, family = "Arial", color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, family = "Arial", color = "black"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none")
ggsave(DALY_heatmap_plot_2023,file="DALY_heatmap_riskfactor_2023.tif",width = 14,height = 10,dpi = 300)
###1990----
DALY_1990 <- filter(allrisk_cause,allrisk_cause$measure_name=="DALYs"&
                      allrisk_cause$year=="1990"&
                      allrisk_cause$sex_name=="Both"&
                      allrisk_cause$metric_name=="Rate"&
                      allrisk_cause$age_name=="Age-standardized")
global_risk_DALY_1990 <- filter(global_risk,global_risk$measure_name=="DALYs"&
                                  global_risk$year=="1990"&
                                  global_risk$sex_name=="Both"&
                                  global_risk$metric_name=="Rate"&
                                  global_risk$age_name=="Age-standardized")
DALY_1990 <- rbind(DALY_1990,global_risk_DALY_1990)
DALY_1990 <- DALY_1990 %>% 
  select(location_name,rei_name,val) %>% 
  group_by(location_name) %>% 
  arrange(desc(val)) %>% 
  mutate(rank = as.factor(row_number())) %>%
  mutate(location_name=case_when(
    location_name=="Macao Special Administrative Region of China" ~ "Macao",
    location_name=="Hong Kong Special Administrative Region of China" ~ "Hong Kong",
    TRUE ~ location_name
  )) %>% 
  ungroup()

DALY_1990$location_name <- factor(DALY_1990$location_name,levels=c("China","Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", "Jilin", "Heilongjiang", 
                                                                   "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi", "Shandong", "Henan", 
                                                                   "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", "Chongqing", "Sichuan", "Guizhou", 
                                                                   "Yunnan", "Tibet", "Shaanxi", "Gansu", "Qinghai", "Ningxia", "Xinjiang", "Hong Kong", "Macao","Global"))
DALY_1990$rei_name <- factor(DALY_1990$rei_name,levels = c("Diet high in red meat", 
                                                           "Diet high in sugar-sweetened beverages", 
                                                           "Diet high in trans fatty acids", 
                                                           "Diet high in processed meat", 
                                                           "Diet low in calcium",
                                                           "Diet low in milk", 
                                                           "Diet low in legumes", 
                                                           "Diet low in polyunsaturated fatty acids", 
                                                           "Diet low in seafood omega-3 fatty acids", 
                                                           "Diet low in nuts and seeds", 
                                                           "Diet low in fiber",
                                                           "Diet low in whole grains", 
                                                           "Diet low in vegetables", 
                                                           "Diet low in fruits", 
                                                           "Diet high in sodium"))

DALY_heatmap_plot_1990 <- ggplot(DALY_1990, aes(x = location_name, y = rei_name, fill = rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = rank), size = 5, family = "Arial", color = "black") + 
  scale_fill_manual(values = c(
    "1" = "#B2182B",    # Dark red, most severe
    "2" = "#D73027",
    "3" = "#F46D43",
    "4" = "#FDAE61",
    "5" = "#FEE08B",
    "6" = "#FFFFBF",    # Light yellow
    "7" = "#D9EF8B",
    "8" = "#A6D96A",
    "9" = "#66BD63",
    "10" = "#1A9850",
    "11" = "#006837",   # Dark green, less severe
    "12" = "#80CDC1",
    "13" = "#35978F",
    "14" = "#01665E",
    "15" = "#003C30" ))+
  labs(x = "Location", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(panel.background = element_rect(fill = "white", colour = NA),  
        plot.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(color = "black", linewidth = 0.7),  
        axis.title.x = element_text(size = 12, family = "Arial", color = "black"),  
        axis.title.y = element_text(size = 12, family = "Arial", color = "black"),  
        axis.text = element_text(size = 12, family = "Arial", color = "black"),
        plot.title = element_text(size = 16, family = "Arial", color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, family = "Arial", color = "black"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        plot.margin = margin(r = 10, l = 50))+
  scale_x_discrete(limits = rev(levels(death_1990$location_name))) +  # 反转 X 轴
  scale_y_discrete(position = "right")  # 将y轴移到右侧

ggsave(DALY_heatmap_plot_1990,file="DALY_heatmap_riskfactor_1990.tif",width = 14,height = 10,dpi = 300)

#中国地图----
rei_top <- c("Diet high in sodium","Diet low in whole grains","Diet low in polyunsaturated fatty acids",
             "Diet low in fruits","Diet low in nuts and seeds","Diet low in seafood omega-3 fatty acids",
             "Diet high in red meat")
cause_top <- c("Ischemic heart disease","Intracerebral hemorrhage","Ischemic stroke","Hypertensive heart disease")


data_map1 <- dietary_annual1.0%>%
  filter(location_name!="China"&sex_name=="Both"&age_name%in%c("All ages","Age-standardized")
         &cause_name%in%cause_top&rei_name%in%rei_top&metric_name=="Rate"
         &year=="2023")%>%
  select(rei_name,cause_name,measure_name,age_name,val,upper,lower)

data_map2_val <- data_map1%>%
  dcast(rei_name+cause_name~measure_name+age_name,value.var ="val" )
names(data_map2_val)[3:6] <- paste0(names(data_map2_val)[3:6],"_val")

data_map2_lower <- dat_map1%>%
  dcast(rei_name+cause_name~measure_name+age_name,value.var ="lower" )
names(data_map2_lower)[3:6] <- paste0(names(data_map2_lower)[3:6],"_lower")

data_map2_upper <- dat_map1%>%
  dcast(rei_name+cause_name~measure_name+age_name,value.var ="upper" )
names(data_map2_upper)[3:6] <- paste0(names(data_map2_upper)[3:6],"_upper")

data_map2 <- merge(merge(data_map2_val,data_map2_lower,by = c("rei_name","cause_name")),
                   data_map2_upper,by = c("rei_name","cause_name"))


data_map2 <- data_map2 %>% 
  transmute(rei_name,
            cause_name,
            DALYs_All = `DALYs (Disability-Adjusted Life Years)_All ages_val`,
            DALYs_AgeSt = `DALYs (Disability-Adjusted Life Years)_Age-standardized_val`,
            Deaths_All = `Deaths_All ages_val`,
            Deaths_AgeSt = `Deaths_Age-standardized_val`
  )

data_map2 <- data_map2%>%
  mutate(DALYs_rank=rank(-DALYs_All),
         Deaths_rank=rank(-Deaths_All))
write.csv(data_map1,file = "data_map1.csv")

library(sf)
china_sf <- st_read("gadm41_CHN_1.shp")
taiwan_sf <- st_read("gadm41_TWN_1.shp")

realchina_sf <- rbind(china_sf,taiwan_sf)
# 绘制包含清晰省界的地图
ggplot(data = realchina_sf) +
  geom_sf(fill = "lightblue", color = "black") 

#三个最严重的膳食因素引起的疾病负担情况----
##三级病因----
level3mostseriousrisk <- dietary_annual1.0 %>% 
  filter(rei_name %in% c("Diet high in sodium","Diet low in whole grains","Diet low in fruits") &
           year %in% c(1990,2023) &
           sex_name == "Both" &
           measure_name %in% c("Deaths","DALYs") &
           metric_name=="Rate" &
           age_name %in% c("All ages","Age-standardized") &
           cause_name %in% c("Diabetes mellitus",
                             "Chronic kidney disease",
                             "Ischemic heart disease",
                             "Aortic aneurysm",
                             "Lower extremity peripheral arterial disease",
                             "Hypertensive heart disease",
                             "Stroke",
                             "Atrial fibrillation and flutter",
                             "Tracheal, bronchus, and lung cancer",
                             "Breast cancer",
                             "Prostate cancer",
                             "Colon and rectum cancer",
                             "Esophageal cancer",
                             "Stomach cancer"))
level3mostseriousrisk <- level3mostseriousrisk %>% 
  mutate(age_name=case_when(
    age_name=="All ages" ~ "All_ages",
    TRUE ~ age_name
  ))

val_wide <- dcast(level3mostseriousrisk, rei_name+location_name+cause_name ~ 
                    measure_name+age_name+year,value.var = "val")

upper_wide <- dcast(level3mostseriousrisk, rei_name+location_name+cause_name ~ 
                      measure_name+age_name+year, value.var = "upper")
colnames(upper_wide)[4:11] <- paste(colnames(upper_wide)[4:11],"_upper",sep = "")

lower_wide <- dcast(level3mostseriousrisk, rei_name+location_name+cause_name ~ 
                      measure_name+age_name+year, value.var = "lower")
colnames(lower_wide)[4:11] <- paste(colnames(lower_wide)[4:11],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("rei_name","location_name", "cause_name")),
              lower_wide, by = c("rei_name","location_name", "cause_name"))

data0 <- data0[,c("rei_name","location_name","cause_name",
                  "Deaths_All_ages_1990","Deaths_All_ages_1990_lower","Deaths_All_ages_1990_upper",
                  "Deaths_All_ages_2023","Deaths_All_ages_2023_lower","Deaths_All_ages_2023_upper",
                  "Deaths_Age-standardized_1990","Deaths_Age-standardized_1990_lower","Deaths_Age-standardized_1990_upper",
                  "Deaths_Age-standardized_2023","Deaths_Age-standardized_2023_lower","Deaths_Age-standardized_2023_upper",
                  "DALYs_All_ages_1990","DALYs_All_ages_1990_lower","DALYs_All_ages_1990_upper",
                  "DALYs_All_ages_2023","DALYs_All_ages_2023_lower","DALYs_All_ages_2023_upper",
                  "DALYs_Age-standardized_1990","DALYs_Age-standardized_1990_lower","DALYs_Age-standardized_1990_upper",
                  "DALYs_Age-standardized_2023","DALYs_Age-standardized_2023_lower","DALYs_Age-standardized_2023_upper"
)]

data1 <- data0 %>% 
  mutate(
    Deaths_allages_1990 = paste(round(Deaths_All_ages_1990,2), "(", round(Deaths_All_ages_1990_lower,2), ",", round(Deaths_All_ages_1990_upper,2), ")"),
    Deaths_allages_2023 = paste(round(Deaths_All_ages_2023,2), "(", round(Deaths_All_ages_2023_lower,2), ",", round(Deaths_All_ages_2023_upper,2), ")"),
    Deaths_age_standardized_1990 = paste(round(`Deaths_Age-standardized_1990`,2), "(", round(`Deaths_Age-standardized_1990_lower`,2), ",", round(`Deaths_Age-standardized_1990_upper`,2), ")"),
    Deaths_age_standardized_2023 = paste(round(`Deaths_Age-standardized_2023`,2), "(", round(`Deaths_Age-standardized_2023_lower`,2), ",", round(`Deaths_Age-standardized_2023_upper`,2), ")"),
    DALYs_allages_1990 = paste(round(DALYs_All_ages_1990,2), "(", round(DALYs_All_ages_1990_lower,2), ",", round(DALYs_All_ages_1990_upper,2), ")"),
    DALYs_allages_2023 = paste(round(DALYs_All_ages_2023,2), "(", round(DALYs_All_ages_2023_lower,2), ",", round(DALYs_All_ages_2023_upper,2), ")"),
    DALYs_age_standardized_1990 = paste(round(`DALYs_Age-standardized_1990`,2), "(", round(`DALYs_Age-standardized_1990_lower`,2), ",", round(`DALYs_Age-standardized_1990_upper`,2), ")"),
    DALYs_age_standardized_2023 = paste(round(`DALYs_Age-standardized_2023`,2), "(", round(`DALYs_Age-standardized_2023_lower`,2), ",", round(`DALYs_Age-standardized_2023_upper`,2), ")")
  )
data2 <- data1 %>% 
  mutate(
    Death_allages_perchange = round(((Deaths_All_ages_2023 - Deaths_All_ages_1990) / Deaths_All_ages_1990) * 100, 2),
    Death_age_standardized_perchange = round(((`Deaths_Age-standardized_2023` - `Deaths_Age-standardized_1990`) / `Deaths_Age-standardized_1990`) * 100, 2),
    DALYs_allages_perchange = round(((DALYs_All_ages_2023 - DALYs_All_ages_1990) / DALYs_All_ages_1990) * 100, 2),
    DALYs_age_standardized_perchange = round(((`DALYs_Age-standardized_2023` - `DALYs_Age-standardized_1990`) / `DALYs_Age-standardized_1990`) * 100, 2)
  ) %>% 
  mutate(
    Death_allages_perchange = ifelse(Death_allages_perchange > 0 & Death_allages_perchange < 0.01, "<0.01", Death_allages_perchange),
    Death_age_standardized_perchange = ifelse(Death_age_standardized_perchange > 0 & Death_age_standardized_perchange < 0.01, "<0.01", Death_age_standardized_perchange),
    DALYs_allages_perchange = ifelse(DALYs_allages_perchange > 0 & DALYs_allages_perchange < 0.01, "<0.01", DALYs_allages_perchange),
    DALYs_age_standardized_perchange = ifelse(DALYs_age_standardized_perchange > 0 & DALYs_age_standardized_perchange < 0.01, "<0.01", DALYs_age_standardized_perchange)
  )

##死亡----
Deaths_risk <- data2[,c(1:4,7,28:31,36:37)]

Deaths_rank_1990 <- Deaths_risk %>% 
  select(rei_name,location_name,cause_name,Deaths_All_ages_1990) %>% 
  group_by(rei_name,location_name) %>% 
  arrange(rei_name,location_name,desc(Deaths_All_ages_1990)) %>% 
  mutate(rank_death1990=row_number()) %>% 
  ungroup() %>% 
  select(rei_name,location_name,cause_name,rank_death1990)

Deaths_rank_2023 <- Deaths_risk %>% 
  select(rei_name,location_name,cause_name,Deaths_All_ages_2023) %>% 
  group_by(rei_name,location_name) %>% 
  arrange(rei_name,location_name,desc(Deaths_All_ages_2023)) %>% 
  mutate(rank_death2023=row_number()) %>% 
  ungroup() %>% 
  select(rei_name,location_name,cause_name,rank_death2023)

Deaths_risk1 <- Deaths_risk %>% 
  left_join(Deaths_rank_1990,by=c("rei_name","location_name","cause_name")) %>% 
  left_join(Deaths_rank_2023,by=c("rei_name","location_name","cause_name")) %>% 
  select("rei_name","location_name","cause_name","rank_death1990","rank_death2023",
         "Deaths_allages_1990","Deaths_allages_2023","Deaths_age_standardized_1990","Deaths_age_standardized_2023",
         "Death_allages_perchange","Death_age_standardized_perchange") %>% 
  mutate(location_name=factor(location_name,levels = c("Macao Special Administrative Region of China", 
                                                       "Hong Kong Special Administrative Region of China", 
                                                       "Xinjiang", 
                                                       "Ningxia", 
                                                       "Qinghai", 
                                                       "Gansu", 
                                                       "Shaanxi", 
                                                       "Tibet", 
                                                       "Yunnan", 
                                                       "Guizhou", 
                                                       "Sichuan", 
                                                       "Chongqing", 
                                                       "Hainan", 
                                                       "Guangxi", 
                                                       "Guangdong", 
                                                       "Hunan", 
                                                       "Hubei", 
                                                       "Henan", 
                                                       "Shandong", 
                                                       "Jiangxi", 
                                                       "Fujian", 
                                                       "Anhui", 
                                                       "Zhejiang", 
                                                       "Jiangsu", 
                                                       "Shanghai", 
                                                       "Heilongjiang", 
                                                       "Jilin", 
                                                       "Liaoning", 
                                                       "Inner Mongolia", 
                                                       "Shanxi", 
                                                       "Hebei", 
                                                       "Tianjin", 
                                                       "Beijing", 
                                                       "China"))) %>% 
  group_by(rei_name,location_name) %>% 
  arrange(rei_name,desc(location_name),rank_death2023) %>% 
  ungroup()
colnames(Deaths_risk1) <- c("Risk factors","provinces",                   
                            "NCDs","Death 1990",                  
                            "Death 2023","All ages 1990",             
                            "All ages 2023","Age standardized 1990",   
                            "Age standardized 2023","All ages perchange",         
                            "Age standardized perchange")
write.csv(Deaths_risk1,file = "Deaths_risk1.csv")
##DALYs----
DALYs_risk <- data2[,c(1:3,16,19,32:35,38:39)]

DALYs_rank_1990 <- DALYs_risk %>% 
  select(rei_name,location_name,cause_name,DALYs_All_ages_1990) %>% 
  group_by(rei_name,location_name) %>% 
  arrange(rei_name,location_name,desc(DALYs_All_ages_1990)) %>% 
  mutate(rank_DALYs1990=row_number()) %>% 
  ungroup() %>% 
  select(rei_name,location_name,cause_name,rank_DALYs1990)

DALYs_rank_2023 <- DALYs_risk %>% 
  select(rei_name,location_name,cause_name,DALYs_All_ages_2023) %>% 
  group_by(rei_name,location_name) %>% 
  arrange(rei_name,location_name,desc(DALYs_All_ages_2023)) %>% 
  mutate(rank_DALYs2023=row_number()) %>% 
  ungroup() %>% 
  select(rei_name,location_name,cause_name,rank_DALYs2023)

DALYs_risk1 <- DALYs_risk %>% 
  left_join(DALYs_rank_1990,by=c("rei_name","location_name","cause_name")) %>% 
  left_join(DALYs_rank_2023,by=c("rei_name","location_name","cause_name")) %>% 
  select("rei_name","location_name","cause_name","rank_DALYs1990","rank_DALYs2023",
         "DALYs_allages_1990","DALYs_allages_2023","DALYs_age_standardized_1990","DALYs_age_standardized_2023",
         "DALYs_allages_perchange","DALYs_age_standardized_perchange") %>% 
  mutate(location_name=factor(location_name,levels = c("Macao Special Administrative Region of China", 
                                                       "Hong Kong Special Administrative Region of China", 
                                                       "Xinjiang", 
                                                       "Ningxia", 
                                                       "Qinghai", 
                                                       "Gansu", 
                                                       "Shaanxi", 
                                                       "Tibet", 
                                                       "Yunnan", 
                                                       "Guizhou", 
                                                       "Sichuan", 
                                                       "Chongqing", 
                                                       "Hainan", 
                                                       "Guangxi", 
                                                       "Guangdong", 
                                                       "Hunan", 
                                                       "Hubei", 
                                                       "Henan", 
                                                       "Shandong", 
                                                       "Jiangxi", 
                                                       "Fujian", 
                                                       "Anhui", 
                                                       "Zhejiang", 
                                                       "Jiangsu", 
                                                       "Shanghai", 
                                                       "Heilongjiang", 
                                                       "Jilin", 
                                                       "Liaoning", 
                                                       "Inner Mongolia", 
                                                       "Shanxi", 
                                                       "Hebei", 
                                                       "Tianjin", 
                                                       "Beijing", 
                                                       "China"))) %>% 
  group_by(rei_name,location_name) %>% 
  arrange(rei_name,desc(location_name),rank_DALYs2023) %>% 
  ungroup()

colnames(DALYs_risk1) <- c("Risk factors","provinces",                   
                           "NCDs","DALYs 1990",                  
                           "DALYs 2023","All ages 1990",             
                           "All ages 2023","Age standardized 1990",   
                           "Age standardized 2023","All ages perchange",         
                           "Age standardized perchange")
write.csv(DALYs_risk1,file = "DALYs_risk1.csv")


##四级病因----
level4mostseriousrisk <- dietary_annual1.0 %>% 
  filter(rei_name %in% c("Diet high in sodium","Diet low in whole grains","Diet low in fruits") &
           year %in% c(1990,2023) &
           sex_name == "Both" &
           measure_name %in% c("Deaths","DALYs") &
           metric_name=="Rate" &
           age_name %in% c("All ages","Age-standardized") &
           cause_name %in% c("Subarachnoid hemorrhage",    
                             "Ischemic stroke",                                            
                             "Diabetes mellitus type 2",    
                             "Intracerebral hemorrhage",                       
                             "Chronic kidney disease due to hypertension",                                   
                             "Chronic kidney disease due to other and unspecified causes",                                     
                             "Chronic kidney disease due to diabetes mellitus type 2",                                         
                             "Chronic kidney disease due to glomerulonephritis"))
level4mostseriousrisk <- level4mostseriousrisk %>% 
  mutate(age_name=case_when(
    age_name=="All ages" ~ "All_ages",
    TRUE ~ age_name
  ))

val_wide <- dcast(level4mostseriousrisk, rei_name+location_name+cause_name ~ 
                    measure_name+age_name+year,value.var = "val")

upper_wide <- dcast(level4mostseriousrisk, rei_name+location_name+cause_name ~ 
                      measure_name+age_name+year, value.var = "upper")
colnames(upper_wide)[4:11] <- paste(colnames(upper_wide)[4:11],"_upper",sep = "")

lower_wide <- dcast(level4mostseriousrisk, rei_name+location_name+cause_name ~ 
                      measure_name+age_name+year, value.var = "lower")
colnames(lower_wide)[4:11] <- paste(colnames(lower_wide)[4:11],"_lower",sep = "")

data0 <-merge(merge(val_wide, upper_wide, by = c("rei_name","location_name", "cause_name")),
              lower_wide, by = c("rei_name","location_name", "cause_name"))

data0 <- data0[,c("rei_name","location_name","cause_name",
                  "Deaths_All_ages_1990","Deaths_All_ages_1990_lower","Deaths_All_ages_1990_upper",
                  "Deaths_All_ages_2023","Deaths_All_ages_2023_lower","Deaths_All_ages_2023_upper",
                  "Deaths_Age-standardized_1990","Deaths_Age-standardized_1990_lower","Deaths_Age-standardized_1990_upper",
                  "Deaths_Age-standardized_2023","Deaths_Age-standardized_2023_lower","Deaths_Age-standardized_2023_upper",
                  "DALYs_All_ages_1990","DALYs_All_ages_1990_lower","DALYs_All_ages_1990_upper",
                  "DALYs_All_ages_2023","DALYs_All_ages_2023_lower","DALYs_All_ages_2023_upper",
                  "DALYs_Age-standardized_1990","DALYs_Age-standardized_1990_lower","DALYs_Age-standardized_1990_upper",
                  "DALYs_Age-standardized_2023","DALYs_Age-standardized_2023_lower","DALYs_Age-standardized_2023_upper"
)]

data1 <- data0 %>% 
  mutate(
    Deaths_allages_1990 = paste(round(Deaths_All_ages_1990,2), "(", round(Deaths_All_ages_1990_lower,2), ",", round(Deaths_All_ages_1990_upper,2), ")"),
    Deaths_allages_2023 = paste(round(Deaths_All_ages_2023,2), "(", round(Deaths_All_ages_2023_lower,2), ",", round(Deaths_All_ages_2023_upper,2), ")"),
    Deaths_age_standardized_1990 = paste(round(`Deaths_Age-standardized_1990`,2), "(", round(`Deaths_Age-standardized_1990_lower`,2), ",", round(`Deaths_Age-standardized_1990_upper`,2), ")"),
    Deaths_age_standardized_2023 = paste(round(`Deaths_Age-standardized_2023`,2), "(", round(`Deaths_Age-standardized_2023_lower`,2), ",", round(`Deaths_Age-standardized_2023_upper`,2), ")"),
    DALYs_allages_1990 = paste(round(DALYs_All_ages_1990,2), "(", round(DALYs_All_ages_1990_lower,2), ",", round(DALYs_All_ages_1990_upper,2), ")"),
    DALYs_allages_2023 = paste(round(DALYs_All_ages_2023,2), "(", round(DALYs_All_ages_2023_lower,2), ",", round(DALYs_All_ages_2023_upper,2), ")"),
    DALYs_age_standardized_1990 = paste(round(`DALYs_Age-standardized_1990`,2), "(", round(`DALYs_Age-standardized_1990_lower`,2), ",", round(`DALYs_Age-standardized_1990_upper`,2), ")"),
    DALYs_age_standardized_2023 = paste(round(`DALYs_Age-standardized_2023`,2), "(", round(`DALYs_Age-standardized_2023_lower`,2), ",", round(`DALYs_Age-standardized_2023_upper`,2), ")")
  )
data2 <- data1 %>% 
  mutate(
    Death_allages_perchange = round(((Deaths_All_ages_2023 - Deaths_All_ages_1990) / Deaths_All_ages_1990) * 100, 2),
    Death_age_standardized_perchange = round(((`Deaths_Age-standardized_2023` - `Deaths_Age-standardized_1990`) / `Deaths_Age-standardized_1990`) * 100, 2),
    DALYs_allages_perchange = round(((DALYs_All_ages_2023 - DALYs_All_ages_1990) / DALYs_All_ages_1990) * 100, 2),
    DALYs_age_standardized_perchange = round(((`DALYs_Age-standardized_2023` - `DALYs_Age-standardized_1990`) / `DALYs_Age-standardized_1990`) * 100, 2)
  ) %>% 
  mutate(
    Death_allages_perchange = ifelse(Death_allages_perchange > 0 & Death_allages_perchange < 0.01, "<0.01", Death_allages_perchange),
    Death_age_standardized_perchange = ifelse(Death_age_standardized_perchange > 0 & Death_age_standardized_perchange < 0.01, "<0.01", Death_age_standardized_perchange),
    DALYs_allages_perchange = ifelse(DALYs_allages_perchange > 0 & DALYs_allages_perchange < 0.01, "<0.01", DALYs_allages_perchange),
    DALYs_age_standardized_perchange = ifelse(DALYs_age_standardized_perchange > 0 & DALYs_age_standardized_perchange < 0.01, "<0.01", DALYs_age_standardized_perchange)
  )
##死亡----
Deaths_risk <- data2[,c(1:3,28:31,36:37)]

Deaths_risk1 <- Deaths_risk %>% 
  select("rei_name","location_name","cause_name",
         "Deaths_allages_1990","Deaths_allages_2023","Deaths_age_standardized_1990","Deaths_age_standardized_2023",
         "Death_allages_perchange","Death_age_standardized_perchange") %>% 
  mutate(location_name=factor(location_name,levels = c("Macao Special Administrative Region of China", 
                                                       "Hong Kong Special Administrative Region of China", 
                                                       "Xinjiang", 
                                                       "Ningxia", 
                                                       "Qinghai", 
                                                       "Gansu", 
                                                       "Shaanxi", 
                                                       "Tibet", 
                                                       "Yunnan", 
                                                       "Guizhou", 
                                                       "Sichuan", 
                                                       "Chongqing", 
                                                       "Hainan", 
                                                       "Guangxi", 
                                                       "Guangdong", 
                                                       "Hunan", 
                                                       "Hubei", 
                                                       "Henan", 
                                                       "Shandong", 
                                                       "Jiangxi", 
                                                       "Fujian", 
                                                       "Anhui", 
                                                       "Zhejiang", 
                                                       "Jiangsu", 
                                                       "Shanghai", 
                                                       "Heilongjiang", 
                                                       "Jilin", 
                                                       "Liaoning", 
                                                       "Inner Mongolia", 
                                                       "Shanxi", 
                                                       "Hebei", 
                                                       "Tianjin", 
                                                       "Beijing", 
                                                       "China"))) %>% 
  group_by(rei_name,location_name) %>% 
  arrange(rei_name,desc(location_name)) %>% 
  ungroup()
colnames(Deaths_risk1) <- c("Risk factors","provinces",                   
                            "NCDs","All ages 1990",             
                            "All ages 2023","Age standardized 1990",   
                            "Age standardized 2023","All ages perchange",         
                            "Age standardized perchange")
write.csv(Deaths_risk1,file = "Deaths_risk1.csv")
##DALYs----
DALYs_risk <- data2[,c(1:3,32:35,38:39)]

DALYs_risk1 <- DALYs_risk %>% 
  select("rei_name","location_name","cause_name",
         "DALYs_allages_1990","DALYs_allages_2023","DALYs_age_standardized_1990","DALYs_age_standardized_2023",
         "DALYs_allages_perchange","DALYs_age_standardized_perchange") %>% 
  mutate(location_name=factor(location_name,levels = c("Macao Special Administrative Region of China", 
                                                       "Hong Kong Special Administrative Region of China", 
                                                       "Xinjiang", 
                                                       "Ningxia", 
                                                       "Qinghai", 
                                                       "Gansu", 
                                                       "Shaanxi", 
                                                       "Tibet", 
                                                       "Yunnan", 
                                                       "Guizhou", 
                                                       "Sichuan", 
                                                       "Chongqing", 
                                                       "Hainan", 
                                                       "Guangxi", 
                                                       "Guangdong", 
                                                       "Hunan", 
                                                       "Hubei", 
                                                       "Henan", 
                                                       "Shandong", 
                                                       "Jiangxi", 
                                                       "Fujian", 
                                                       "Anhui", 
                                                       "Zhejiang", 
                                                       "Jiangsu", 
                                                       "Shanghai", 
                                                       "Heilongjiang", 
                                                       "Jilin", 
                                                       "Liaoning", 
                                                       "Inner Mongolia", 
                                                       "Shanxi", 
                                                       "Hebei", 
                                                       "Tianjin", 
                                                       "Beijing", 
                                                       "China"))) %>% 
  group_by(rei_name,location_name) %>% 
  arrange(rei_name,desc(location_name)) %>% 
  ungroup()

colnames(DALYs_risk1) <- c("Risk factors","provinces",                   
                           "NCDs","All ages 1990",             
                           "All ages 2023","Age standardized 1990",   
                           "Age standardized 2023","All ages perchange",         
                           "Age standardized perchange")
write.csv(DALYs_risk1,file = "DALYs_risk1.csv")



#何老师文章中的图（level2的变化）----
he <- dietary_annual1.0 %>% 
  filter(location_name=="China" & 
           sex_name=="Both" &
           age_name=="All ages" &
           cause_name %in% c("Cardiovascular diseases","Neoplasms","Diabetes and kidney diseases") &
           rei_name=="Dietary risks" &
           metric_name=="Percent" & 
           year %in% c(1990,2000,2010,2023)) %>% 
  select(measure_name,cause_name,year,val)

he <- dcast(he, cause_name+year~measure_name, value.var = "val")

he_map_death <- ggplot(he,aes(x=year, y=Deaths, fill=cause_name))+
  geom_bar(stat = "identity", position = "dodge", colour = "black") +  # 添加黑色边框
  labs(fill="Cause", x = "Cause", y = "Proportion", title = "Proportion of level 2 NCDs death attributable to dietary risks") +
  scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, by = 0.1)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2023), labels = c("1990", "2000", "2010", "2023")) +
  scale_fill_lancet()+
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 20, family = "Arial"),  
    axis.title.y = element_text(size = 20, family = "Arial"),  
    axis.text.x = element_text(size = 15, family = "Arial", colour = "black"), 
    axis.text.y = element_text(size = 20, family = "Arial", colour = "black"), 
    axis.text = element_text(size = 20, family = "Arial"),
    plot.title = element_text(size = 20, family = "Arial"),
    strip.background = element_blank(),
    strip.text = element_text(size = 20, family = "Arial"),
    legend.title = element_text(size = 20, family = "Arial"),
    legend.text = element_text(size = 20, family = "Arial")
  )
ggsave(he_map_death,file="he_map_death.png",width = 18,height = 12,dpi = 300)

he_map_DALY <- ggplot(he,aes(x=year, y=DALYs, fill=cause_name))+
  geom_bar(stat = "identity", position = "dodge", colour = "black") +  # 添加黑色边框
  labs(fill="Cause", x = "Cause", y = "Proportion", title = "Proportion of level 2 NCDs DALYs attributable to dietary risks") +
  scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, by = 0.1)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2023), labels = c("1990", "2000", "2010", "2023")) +
  scale_fill_lancet()+
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.7),  
    axis.title.x = element_text(size = 20, family = "Arial"),  
    axis.title.y = element_text(size = 20, family = "Arial"),  
    axis.text.x = element_text(size = 15, family = "Arial", colour = "black"), 
    axis.text.y = element_text(size = 20, family = "Arial", colour = "black"), 
    axis.text = element_text(size = 20, family = "Arial"),
    plot.title = element_text(size = 20, family = "Arial"),
    strip.background = element_blank(),
    strip.text = element_text(size = 20, family = "Arial"),
    legend.title = element_text(size = 20, family = "Arial"),
    legend.text = element_text(size = 20, family = "Arial")
  )
ggsave(he_map_DALY,file="he_map_DALY.png",width = 18,height = 12,dpi = 300)

##########################################################BAPC预测 all-all 总人群-三种性别####
install.packages("cmprsk")
install.packages("fanplot")
install.packages("Epi")
install.packages("caTools")
install.packages("BAPC",repos = "http://R-Forge.R-project.org")
install.packages("remotes")
install.packages("foreach")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Rgraphviz")

install.packages("INLA", type = "source", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/testing"), dep = TRUE)
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)

# 加载必要的包
library(tidyverse)
library(data.table)
library(haven)
library(bapc)
library(INLA)
library(ggplot2)
library(scales)
library(stringr)
## BAPC for China (Deaths, DALYs)
##  - All-ages Number (table + plot)
##  - Age-standardized Rate (table + plot)

options(timeout = 600)
options(stringsAsFactors = FALSE)
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))

## ---------------------------1
## 0) Packages
## ---------------------------1
pkg_needed <- c("data.table","dplyr","tidyr","stringr","ggplot2","INLA","BAPC")

install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      message("Installing: ", p)
      if (p == "INLA") {
        install.packages(
          "INLA",
          repos = c(
            CRAN = "https://cloud.r-project.org",
            INLA = "https://inla.r-inla-download.org/R/stable"
          ),
          dep = TRUE
        )
      } else if (p == "BAPC") {
        install.packages("BAPC", repos = "http://R-Forge.R-project.org")
      } else {
        install.packages(p)
      }
    }
  }
}
install_if_missing(pkg_needed)

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(INLA)
library(BAPC)

## ---------------------------1
## 1) Output folders
## ---------------------------1
gbd2023_dir <- path.expand("~/China GBD/GBD2023")
out_root    <- file.path(gbd2023_dir, "BAPC")
results_dir <- file.path(out_root, "results")
plots_dir   <- file.path(out_root, "plots")

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir,   recursive = TRUE, showWarnings = FALSE)

## ---------------------------1
## 2) Inputs
## ---------------------------1
if (!exists("allrisk_cause")) {
  stop("未检测到对象 allrisk_cause，请先读入 allrisk_cause。")
}

pop_df <- read.csv(file = "~/China GBD/GBD2023/合并数据/IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.csv")
pop_df_2017before <- read.csv(file = "~/China GBD/GBD2023/合并数据/IHME_POP_2017_2100_POP_PAST_Y2020M05D011.csv")

if (ncol(pop_df) >= 16) pop_df <- pop_df[, -c(12, 13, 15, 16)]
pop_df1 <- rbind(pop_df_2017before, pop_df)

## 标准人口
gbd_age_standard <- data.frame(
  age = c("Early Neonatal","Late Neonatal","Post Neonatal","<1",
          "1 to 4","5 to 9","10 to 14","15 to 19","20 to 24","25 to 29",
          "30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59",
          "60 to 64","65 to 69","70 to 74","75 to 79","80 to 84","85 to 89",
          "90 to 94","95 plus"),
  pct = c(0.039706188,0.118021789,1.868264909,2.025992886,
          7.909875913,9.568418272,8.990277942,8.324362192,7.866450176,7.632917343,
          7.331511124,6.811055,6.136798184,5.509495973,4.921822565,4.345633072,
          3.684473754,2.991239718,2.272487547,1.607371655,1.113034599,0.61707008,
          0.255008068,0.084703935)
)

## ---------------------------1
## 3) Parameters
## ---------------------------1
start_year <- 1990
last_obs_year_default <- 2023
end_year <- 2050

measures_to_do <- c("Deaths", "DALYs")
sexes_to_do    <- c("Both", "Female", "Male")

## 你数据里可用的 5 岁年龄组（25+）
model_ages <- c(
  "25 to 29","30 to 34","35 to 39","40 to 44","45 to 49",
  "50 to 54","55 to 59","60 to 64","65 to 69","70 to 74",
  "75 to 79","80 to 84","85 to 89","90 to 94","95 plus"
)

## 标准化权重（仅对 model_ages 归一化）
wstand <- gbd_age_standard$pct[match(model_ages, gbd_age_standard$age)]
if (any(is.na(wstand))) stop("wstand 匹配失败：检查 model_ages 与 gbd_age_standard$age 是否一致。")
wstand <- wstand / sum(wstand)

## ---------------------------1
## 4) Helpers
## ---------------------------1
age_allrisk_to_pop <- function(x) {
  x <- trimws(x)
  x <- gsub(" years$", "", x)
  x <- gsub(" year$",  "", x)
  x <- gsub("([0-9]+)\\+", "\\1 plus", x)                 # 95+ -> 95 plus
  x <- gsub("([0-9]+)\\s*-\\s*([0-9]+)", "\\1 to \\2", x) # 25-29 -> 25 to 29
  x
}

## ✅ 关键修正：fun.aggregate = sum，解决 “defaulting to length”
make_year_age_matrix <- function(dt, year_col, age_col, value_col, years_all, ages_all, fill_value = 0) {
  dt <- as.data.table(dt)
  dt <- dt[, .(year  = as.integer(get(year_col)),
               age   = as.character(get(age_col)),
               value = as.numeric(get(value_col)))]
  
  wide <- dcast(
    dt,
    year ~ age,
    value.var = "value",
    fun.aggregate = sum,
    fill = fill_value
  )
  
  wide <- merge(data.table(year = years_all), wide, by = "year", all.x = TRUE)
  
  for (a in ages_all) {
    if (!a %in% names(wide)) wide[[a]] <- fill_value
  }
  
  wide <- wide[, c("year", ages_all), with = FALSE]
  mat <- as.matrix(wide[, -1, with = FALSE])
  rownames(mat) <- wide$year
  mat
}
extract_stat_matrix <- function(x, stat = "mean") {
  df <- as.data.frame(x)
  nm <- names(df)
  
  cols <- grep(paste0("^", stat, "\\."), nm, value = TRUE)
  if (length(cols) == 0) cols <- nm[grepl(stat, nm, fixed = TRUE)]
  
  out <- df[, cols, drop = FALSE]
  
  ## ✅ 把 "mean.25.to.29" -> "25 to 29"，并做统一格式
  cn <- sub(paste0("^", stat, "\\."), "", cols)
  cn <- gsub("\\.", " ", cn)          # 点号换空格
  cn <- trimws(cn)
  cn <- age_allrisk_to_pop(cn)        # 再把 25-29/95+ 等统一成 "to/plus"
  
  colnames(out) <- cn
  out
}
# 
# plot_time_series <- function(df, x = "year", y = "mean", ymin = NULL, ymax = NULL, title = "", ylab = "") {
#   p <- ggplot(df, aes_string(x = x, y = y)) +
#     geom_line() +
#     theme_bw() +
#     labs(title = title, x = "Year", y = ylab)
#   
#   if (!is.null(ymin) && !is.null(ymax) && ymin %in% names(df) && ymax %in% names(df)) {
#     p <- p + geom_ribbon(aes_string(ymin = ymin, ymax = ymax), alpha = 0.2)
#   }
#   p
# }

## ✅ sex 统一（兼容 both/male/female/Both sexes 等写法）
normalize_sex <- function(x) {
  z <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    z %in% c("male", "m") ~ "Male",
    z %in% c("female", "f") ~ "Female",
    z %in% c("both", "both sexes", "all", "combined") ~ "Both",
    TRUE ~ as.character(x)
  )
}

## ---------------------------1
## 5) Core runner
## ---------------------------1
run_bapc_one <- function(measure_sel, sex_sel) {
  
  message("Running BAPC: ", measure_sel, " | ", sex_sel)
  
  years_all <- start_year:end_year
  
  ## ---- outcome (allrisk_cause) ----
  dat_y <- allrisk_cause %>%
    dplyr::filter(
      .data$location_name == "China",
      .data$measure_name  == .env$measure_sel,
      .data$metric_name   == "Number",
      .data$sex_name      == .env$sex_sel
    ) %>%
    dplyr::mutate(age_pop = age_allrisk_to_pop(.data$age_name)) %>%
    dplyr::filter(.data$age_pop %in% model_ages) %>%
    dplyr::transmute(
      year    = as.integer(.data$year),
      age_pop = as.character(.data$age_pop),
      val     = pmax(round(as.numeric(.data$val)), 0)
    )
  
  if (nrow(dat_y) == 0) {
    warning("无数据：", measure_sel, " | ", sex_sel, "（跳过）")
    return(NULL)
  }
  
  last_obs_year <- max(dat_y$year, na.rm = TRUE)
  if (last_obs_year < start_year) last_obs_year <- last_obs_year_default
  if (last_obs_year > end_year) stop("end_year 小于观测最大年份，请增大 end_year。")
  
  y_obs_mat <- make_year_age_matrix(
    dt = dat_y,
    year_col = "year",
    age_col  = "age_pop",
    value_col = "val",
    years_all = start_year:last_obs_year,
    ages_all  = model_ages,
    fill_value = 0
  )
  
  y_mat <- matrix(
    NA_real_,
    nrow = length(years_all),
    ncol = length(model_ages),
    dimnames = list(years_all, model_ages)
  )
  y_mat[as.character(start_year:last_obs_year), ] <- y_obs_mat
  
  
  ## ---- population (pop_df1) ----
  pop_base <- pop_df1 %>%
    dplyr::mutate(
      location_name  = trimws(.data$location_name),
      sex_std        = normalize_sex(.data$sex),
      year_id        = as.integer(.data$year_id),
      age_group_name = trimws(.data$age_group_name),
      val            = pmax(round(as.numeric(.data$val)), 0)
    ) %>%
    dplyr::filter(
      .data$location_name == "China",
      .data$year_id %in% years_all,
      .data$age_group_name %in% model_ages
    )
  
  pop_tmp <- pop_base
  if ("measure_name" %in% names(pop_tmp) && any(pop_tmp$measure_name == "Population", na.rm = TRUE)) {
    pop_try <- pop_tmp %>% dplyr::filter(.data$measure_name == "Population")
    if (nrow(pop_try) > 0) pop_tmp <- pop_try
  }
  if ("metric_name" %in% names(pop_tmp) && any(pop_tmp$metric_name == "Number", na.rm = TRUE)) {
    pop_try <- pop_tmp %>% dplyr::filter(.data$metric_name == "Number")
    if (nrow(pop_try) > 0) pop_tmp <- pop_try
  }
  
  ## Both：优先用数据里的 Both；没有则 Male+Female 合成
  if (sex_sel == "Both") {
    
    pop_both <- pop_tmp %>%
      dplyr::filter(.data$sex_std == "Both") %>%
      dplyr::select(.data$year_id, .data$age_group_name, .data$val)
    
    if (nrow(pop_both) > 0) {
      pop_mat <- make_year_age_matrix(pop_both, "year_id", "age_group_name", "val",
                                      years_all, model_ages, NA_real_)
    } else {
      pop_m <- pop_tmp %>% dplyr::filter(.data$sex_std == "Male") %>%
        dplyr::select(.data$year_id, .data$age_group_name, .data$val)
      pop_f <- pop_tmp %>% dplyr::filter(.data$sex_std == "Female") %>%
        dplyr::select(.data$year_id, .data$age_group_name, .data$val)
      
      if (nrow(pop_m) == 0 || nrow(pop_f) == 0) {
        stop("人口数据中既没有 Both，也无法用 Male/Female 合成 Both。请检查 pop_df1$sex 的取值。")
      }
      
      pop_mat_m <- make_year_age_matrix(pop_m, "year_id", "age_group_name", "val",
                                        years_all, model_ages, NA_real_)
      pop_mat_f <- make_year_age_matrix(pop_f, "year_id", "age_group_name", "val",
                                        years_all, model_ages, NA_real_)
      pop_mat <- pop_mat_m + pop_mat_f
    }
    
  } else {
    
    pop_one <- pop_tmp %>%
      dplyr::filter(.data$sex_std == sex_sel) %>%
      dplyr::select(.data$year_id, .data$age_group_name, .data$val)
    
    if (nrow(pop_one) == 0) {
      stop("人口数据缺失：China | ", sex_sel, " | ", paste(range(years_all), collapse = "-"),
           "。请检查 pop_df1$sex 的取值（大小写/别名）。")
    }
    
    pop_mat <- make_year_age_matrix(pop_one, "year_id", "age_group_name", "val",
                                    years_all, model_ages, NA_real_)
  }
  
  if (any(is.na(pop_mat))) {
    miss <- which(is.na(pop_mat), arr.ind = TRUE)
    stop("人口矩阵存在 NA（示例位置）：year=", rownames(pop_mat)[miss[1,1]],
         ", age=", colnames(pop_mat)[miss[1,2]])
  }
  
  
  ## ---- Fit BAPC ----
  npredict <- end_year - last_obs_year
  
  epi_df  <- base::data.frame(y_mat,  check.names = FALSE)
  pyrs_df <- base::data.frame(pop_mat, check.names = FALSE)
  
  epi_df[]  <- lapply(epi_df,  function(z) { z <- as.numeric(z); ifelse(is.na(z), NA, as.integer(round(z))) })
  pyrs_df[] <- lapply(pyrs_df, as.numeric)
  
  apc_obj <- BAPC::APCList(epi = epi_df, pyrs = pyrs_df, gf = 5)
  
  fit <- BAPC::BAPC(
    apc_obj,
    predict = list(npredict = npredict, retro = TRUE),
    secondDiff = FALSE,
    stdweight = wstand,
    verbose = FALSE
  )
  
  
  ## ---- Extract results ----
  proj_age <- agespec.proj(fit)
  proj_age_mean <- extract_stat_matrix(proj_age, "mean")
  
  for (a in model_ages) {
    if (!a %in% names(proj_age_mean)) proj_age_mean[[a]] <- NA_real_
  }
  proj_age_mean <- proj_age_mean[, model_ages, drop = FALSE]
  
  proj_age_mean$year <- as.integer(rownames(proj_age_mean))
  proj_age_mean <- proj_age_mean %>% dplyr::relocate(.data$year)
  
  allage_number <- data.frame(
    year = proj_age_mean$year,
    mean = rowSums(proj_age_mean[, model_ages, drop = FALSE], na.rm = TRUE)
  )
  
  asr <- agestd.rate(fit) %>% as.data.frame()
  asr$year <- as.integer(rownames(asr))
  if ("mean" %in% names(asr)) asr$mean <- asr$mean * 1e5
  if ("0.025quant" %in% names(asr)) asr$`0.025quant` <- asr$`0.025quant` * 1e5
  if ("0.975quant" %in% names(asr)) asr$`0.975quant` <- asr$`0.975quant` * 1e5
  asr <- asr %>% dplyr::relocate(.data$year)
  
  
  ## ---- Save ----
  prefix <- paste0("China_", measure_sel, "_", sex_sel, "_", start_year, "_", end_year)
  
  saveRDS(fit, file = file.path(results_dir, paste0(prefix, "_bapc_fit.rds")))
  write.csv(proj_age_mean, file = file.path(results_dir, paste0(prefix, "_agespec_number_mean.csv")), row.names = FALSE)
  write.csv(allage_number, file = file.path(results_dir, paste0(prefix, "_AllAges_Number_mean.csv")), row.names = FALSE)
  write.csv(asr,          file = file.path(results_dir, paste0(prefix, "_AgeStd_Rate_per100k.csv")), row.names = FALSE)
  
  
  ## ---- Plots ----
  png(file.path(plots_dir, paste0(prefix, "_plotBAPC_ageStdProj.png")), width = 1800, height = 1300, res = 200)
  tryCatch({
    plotBAPC(fit, scale = 1e5, type = "ageStdProj", showdata = TRUE)
  }, error = function(e) {
    plot.new(); text(0.5, 0.5, paste("plotBAPC ageStdProj failed:\n", e$message))
  })
  dev.off()
  
  png(file.path(plots_dir, paste0(prefix, "_plotBAPC_ageStdRate.png")), width = 1800, height = 1300, res = 200)
  tryCatch({
    plotBAPC(fit, scale = 1e5, type = "ageStdRate", showdata = TRUE)
  }, error = function(e) {
    plot.new(); text(0.5, 0.5, paste("plotBAPC ageStdRate failed:\n", e$message))
  })
  dev.off()
  
  list(fit = fit, proj_age_mean = proj_age_mean, allage_number = allage_number, asr = asr)
}

## ---------------------------1
## 6) Batch run
## ---------------------------1
all_results <- list()

for (m in measures_to_do) {
  for (s in sexes_to_do) {
    res <- run_bapc_one(m, s)
    if (!is.null(res)) {
      key <- paste(m, s, sep = "_")
      all_results[[key]] <- res
    }
  }
}

message("Done. Tables in: ", results_dir)
message("Plots  in: ", plots_dir)



##########################################################分年龄all-allBAPC----
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(INLA)
library(BAPC)

options(timeout = 600)
options(stringsAsFactors = FALSE)
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))

## ---------------------------1
## 0) Packages
## ---------------------------1
pkg_needed <- c("data.table","dplyr","tidyr","stringr","ggplot2","INLA","BAPC")
install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      message("Installing: ", p)
      if (p == "INLA") {
        install.packages(
          "INLA",
          repos = c(
            CRAN = "https://cloud.r-project.org",
            INLA = "https://inla.r-inla-download.org/R/stable"
          ),
          dep = TRUE
        )
      } else if (p == "BAPC") {
        install.packages("BAPC", repos = "http://R-Forge.R-project.org")
      } else {
        install.packages(p)
      }
    }
  }
}
install_if_missing(pkg_needed)

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(INLA)
library(BAPC)

## ---------------------------1
## 1) Output folders
## ---------------------------1
gbd2023_dir <- path.expand("~/China GBD/GBD2023")
out_root    <- file.path(gbd2023_dir, "BAPC")
results_dir <- file.path(out_root, "results")
plots_dir   <- file.path(out_root, "plots")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir,   recursive = TRUE, showWarnings = FALSE)

## ---------------------------1
## 2) Inputs
## ---------------------------1
if (!exists("allrisk_cause")) {
  stop("未检测到对象 allrisk_cause，请先读入 allrisk_cause。")
}

pop_df <- read.csv(file = "~/China GBD/GBD2023/合并数据/IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.csv")
pop_df_2017before <- read.csv(file = "~/China GBD/GBD2023/合并数据/IHME_POP_2017_2100_POP_PAST_Y2020M05D011.csv")

if (ncol(pop_df) >= 16) pop_df <- pop_df[, -c(12, 13, 15, 16)]
pop_df1 <- rbind(pop_df_2017before, pop_df)

## 你给的 GBD 标准人口构成
gbd_age_standard <- data.frame(
  age = c("Early Neonatal","Late Neonatal","Post Neonatal","<1",
          "1 to 4","5 to 9","10 to 14","15 to 19","20 to 24","25 to 29",
          "30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59",
          "60 to 64","65 to 69","70 to 74","75 to 79","80 to 84","85 to 89",
          "90 to 94","95 plus"),
  pct = c(0.039706188,0.118021789,1.868264909,2.025992886,
          7.909875913,9.568418272,8.990277942,8.324362192,7.866450176,7.632917343,
          7.331511124,6.811055,6.136798184,5.509495973,4.921822565,4.345633072,
          3.684473754,2.991239718,2.272487547,1.607371655,1.113034599,0.61707008,
          0.255008068,0.084703935)
)

## ---------------------------1
## 3) Parameters1
## ---------------------------1
start_year <- 1990
end_year   <- 2050

measures_to_do <- c("Deaths", "DALYs")

## ✅ 不分性别：固定 Both
sex_fixed <- "Both"

## ✅ 三个年龄段（用5岁组来做 BAPC）
agebands <- list(
  "15_49"  = c("15 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49"),
  "50_69"  = c("50 to 54","55 to 59","60 to 64","65 to 69"),
  "70plus" = c("70 to 74","75 to 79","80 to 84","85 to 89","90 to 94","95 plus")
)

## ---------------------------1
## 4) Helpers
## ---------------------------1
age_allrisk_to_pop <- function(x) {
  x <- trimws(x)
  x <- gsub(" years$", "", x)
  x <- gsub(" year$",  "", x)
  x <- gsub("([0-9]+)\\+", "\\1 plus", x)                 # 95+ -> 95 plus
  x <- gsub("([0-9]+)\\s*-\\s*([0-9]+)", "\\1 to \\2", x) # 25-29 -> 25 to 29
  x
}

normalize_sex <- function(x) {
  z <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    z %in% c("male", "m") ~ "Male",
    z %in% c("female", "f") ~ "Female",
    z %in% c("both", "both sexes", "all", "combined") ~ "Both",
    TRUE ~ as.character(x)
  )
}

make_year_age_matrix <- function(dt, year_col, age_col, value_col, years_all, ages_all, fill_value = 0) {
  dt <- as.data.table(dt)
  dt <- dt[, .(year  = as.integer(get(year_col)),
               age   = as.character(get(age_col)),
               value = as.numeric(get(value_col)))]
  
  wide <- dcast(
    dt,
    year ~ age,
    value.var = "value",
    fun.aggregate = sum,
    fill = fill_value
  )
  
  wide <- merge(data.table(year = years_all), wide, by = "year", all.x = TRUE)
  
  for (a in ages_all) {
    if (!a %in% names(wide)) wide[[a]] <- fill_value
  }
  
  wide <- wide[, c("year", ages_all), with = FALSE]
  mat <- as.matrix(wide[, -1, with = FALSE])
  rownames(mat) <- wide$year
  mat
}

extract_stat_matrix <- function(x, stat = "mean") {
  df <- as.data.frame(x)
  nm <- names(df)
  
  cols <- grep(paste0("^", stat, "\\."), nm, value = TRUE)
  if (length(cols) == 0) cols <- nm[grepl(stat, nm, fixed = TRUE)]
  
  out <- df[, cols, drop = FALSE]
  
  cn <- sub(paste0("^", stat, "\\."), "", cols)
  cn <- gsub("\\.", " ", cn)
  cn <- trimws(cn)
  cn <- age_allrisk_to_pop(cn)
  
  colnames(out) <- cn
  out
}

## ✅ 取 2023 年 China Both 的年龄别人口（若没有Both就Male+Female）
get_pop_2023_vec <- function(ages_needed) {
  base <- pop_df1 %>%
    mutate(
      location_name  = trimws(location_name),
      sex_std        = normalize_sex(sex),
      year_id        = as.integer(year_id),
      age_group_name = trimws(age_group_name),
      val            = as.numeric(val)
    ) %>%
    filter(location_name == "China", year_id == 2023, age_group_name %in% ages_needed)
  
  # 可选过滤：如果文件里有这些字段
  if ("measure_name" %in% names(base) && any(base$measure_name == "Population", na.rm = TRUE)) {
    tmp <- base %>% filter(measure_name == "Population")
    if (nrow(tmp) > 0) base <- tmp
  }
  if ("metric_name" %in% names(base) && any(base$metric_name == "Number", na.rm = TRUE)) {
    tmp <- base %>% filter(metric_name == "Number")
    if (nrow(tmp) > 0) base <- tmp
  }
  
  both <- base %>% filter(sex_std == "Both") %>% select(age_group_name, val)
  if (nrow(both) > 0) {
    v <- both$val[match(ages_needed, both$age_group_name)]
    return(v)
  }
  
  male <- base %>% filter(sex_std == "Male") %>% select(age_group_name, val)
  fem  <- base %>% filter(sex_std == "Female") %>% select(age_group_name, val)
  v_m  <- male$val[match(ages_needed, male$age_group_name)]
  v_f  <- fem$val[match(ages_needed, fem$age_group_name)]
  v <- v_m + v_f
  v
}

## ✅ 每个年龄段的 stdweight：GBD pct × 2023 China 人口，然后归一化
make_wstand_band <- function(band_ages) {
  w_pct <- gbd_age_standard$pct[match(band_ages, gbd_age_standard$age)]
  if (any(is.na(w_pct))) stop("标准人口 pct 匹配失败：请检查 band_ages 与 gbd_age_standard$age 是否一致。")
  
  pop2023 <- get_pop_2023_vec(band_ages)
  if (any(is.na(pop2023))) {
    warning("2023 人口有缺失年龄组，stdweight 将退化为仅使用 GBD pct（不乘人口）。")
    w_raw <- w_pct
  } else {
    w_raw <- w_pct * pop2023
  }
  w_raw / sum(w_raw)
}

## ---------------------------1
## 5) Core runner: one measure + one ageband (Both only)
## ---------------------------1
run_bapc_one_band <- function(measure_sel, band_name, band_ages) {
  
  message("Running BAPC: ", measure_sel, " | ", sex_fixed, " | ", band_name)
  
  ## 年份范围先按人口可用范围裁剪，避免人口缺年份直接报错
  pop_years <- pop_df1 %>%
    mutate(location_name = trimws(location_name), year_id = as.integer(year_id)) %>%
    filter(location_name == "China") %>%
    pull(year_id)
  pop_years <- pop_years[!is.na(pop_years)]
  if (length(pop_years) == 0) stop("人口数据没有 China 的 year_id。")
  
  end_year_use <- min(end_year, max(pop_years))
  if (end_year_use < end_year) {
    warning("人口数据最多到 ", end_year_use, "，已将 end_year 从 ", end_year, " 自动裁剪到 ", end_year_use)
  }
  years_all <- start_year:end_year_use
  
  ## ---- outcome (allrisk_cause): 用5岁组计数 ----
  dat_y <- allrisk_cause %>%
    filter(
      location_name == "China",
      measure_name  == measure_sel,
      metric_name   == "Number",
      sex_name      == sex_fixed
    ) %>%
    mutate(age_pop = age_allrisk_to_pop(age_name)) %>%
    filter(age_pop %in% band_ages) %>%
    transmute(
      year    = as.integer(year),
      age_pop = as.character(age_pop),
      val     = pmax(round(as.numeric(val)), 0)
    )
  
  if (nrow(dat_y) == 0) {
    warning("无数据：", measure_sel, " | ", band_name, "（跳过）")
    return(NULL)
  }
  
  last_obs_year <- max(dat_y$year, na.rm = TRUE)
  if (last_obs_year > end_year_use) {
    warning("观测最大年份 ", last_obs_year, " > end_year_use ", end_year_use, "，将 end_year_use 提升到 last_obs_year。")
    end_year_use <- last_obs_year
    years_all <- start_year:end_year_use
  }
  
  y_obs_mat <- make_year_age_matrix(
    dt = dat_y,
    year_col = "year",
    age_col  = "age_pop",
    value_col = "val",
    years_all = start_year:last_obs_year,
    ages_all  = band_ages,
    fill_value = 0
  )
  
  y_mat <- matrix(NA_real_, nrow = length(years_all), ncol = length(band_ages),
                  dimnames = list(years_all, band_ages))
  y_mat[as.character(start_year:last_obs_year), ] <- y_obs_mat
  
  ## ---- population (pop_df1): Both，如果没有就 Male+Female ----
  pop_base <- pop_df1 %>%
    mutate(
      location_name  = trimws(location_name),
      sex_std        = normalize_sex(sex),
      year_id        = as.integer(year_id),
      age_group_name = trimws(age_group_name),
      val            = pmax(round(as.numeric(val)), 0)
    ) %>%
    filter(
      location_name == "China",
      year_id %in% years_all,
      age_group_name %in% band_ages
    )
  
  if ("measure_name" %in% names(pop_base) && any(pop_base$measure_name == "Population", na.rm = TRUE)) {
    tmp <- pop_base %>% filter(measure_name == "Population")
    if (nrow(tmp) > 0) pop_base <- tmp
  }
  if ("metric_name" %in% names(pop_base) && any(pop_base$metric_name == "Number", na.rm = TRUE)) {
    tmp <- pop_base %>% filter(metric_name == "Number")
    if (nrow(tmp) > 0) pop_base <- tmp
  }
  
  pop_both <- pop_base %>% filter(sex_std == "Both") %>% select(year_id, age_group_name, val)
  if (nrow(pop_both) > 0) {
    pop_mat <- make_year_age_matrix(
      dt = pop_both,
      year_col = "year_id",
      age_col  = "age_group_name",
      value_col = "val",
      years_all = years_all,
      ages_all  = band_ages,
      fill_value = NA_real_
    )
  } else {
    pop_m <- pop_base %>% filter(sex_std == "Male") %>% select(year_id, age_group_name, val)
    pop_f <- pop_base %>% filter(sex_std == "Female") %>% select(year_id, age_group_name, val)
    if (nrow(pop_m) == 0 || nrow(pop_f) == 0) stop("人口数据中既没有 Both，也无法用 Male/Female 合成 Both。")
    pop_mat_m <- make_year_age_matrix(pop_m, "year_id", "age_group_name", "val", years_all, band_ages, NA_real_)
    pop_mat_f <- make_year_age_matrix(pop_f, "year_id", "age_group_name", "val", years_all, band_ages, NA_real_)
    pop_mat <- pop_mat_m + pop_mat_f
  }
  
  if (any(is.na(pop_mat))) {
    miss <- which(is.na(pop_mat), arr.ind = TRUE)
    stop("人口矩阵存在 NA：year=", rownames(pop_mat)[miss[1,1]], ", age=", colnames(pop_mat)[miss[1,2]])
  }
  
  ## ---- stdweight: GBD pct × 2023 China人口（按你要求）----
  wstand_band <- make_wstand_band(band_ages)
  
  ## ---- Fit BAPC ----
  npredict <- end_year_use - last_obs_year
  
  epi_df  <- base::data.frame(y_mat,   check.names = FALSE)
  pyrs_df <- base::data.frame(pop_mat, check.names = FALSE)
  
  epi_df[]  <- lapply(epi_df,  function(z) { z <- as.numeric(z); ifelse(is.na(z), NA, as.integer(round(z))) })
  pyrs_df[] <- lapply(pyrs_df, as.numeric)
  
  apc_obj <- BAPC::APCList(epi = epi_df, pyrs = pyrs_df, gf = 5)
  
  fit <- BAPC::BAPC(
    apc_obj,
    predict = list(npredict = npredict, retro = TRUE),
    secondDiff = FALSE,
    stdweight = wstand_band,
    verbose = FALSE
  )
  
  ## ---- Extract results ----
  proj_age <- BAPC::agespec.proj(fit)
  proj_age_mean <- extract_stat_matrix(proj_age, "mean")
  
  for (a in band_ages) if (!a %in% names(proj_age_mean)) proj_age_mean[[a]] <- NA_real_
  proj_age_mean <- proj_age_mean[, band_ages, drop = FALSE]
  proj_age_mean$year <- as.integer(rownames(proj_age_mean))
  proj_age_mean <- proj_age_mean %>% relocate(year)
  
  ## 年龄段总人数（该年龄段的“all ages”）
  band_number <- data.frame(
    year = proj_age_mean$year,
    mean = rowSums(proj_age_mean[, band_ages, drop = FALSE], na.rm = TRUE)
  )
  
  ## 年龄段标化率（per 100k）
  asr <- BAPC::agestd.rate(fit) %>% as.data.frame()
  asr$year <- as.integer(rownames(asr))
  if ("mean" %in% names(asr)) asr$mean <- asr$mean * 1e5
  if ("0.025quant" %in% names(asr)) asr$`0.025quant` <- asr$`0.025quant` * 1e5
  if ("0.975quant" %in% names(asr)) asr$`0.975quant` <- asr$`0.975quant` * 1e5
  asr <- asr %>% relocate(year)
  
  ## ---- Save ----
  prefix <- paste0("China_", measure_sel, "_", sex_fixed, "_", band_name, "_", start_year, "_", end_year_use)
  
  saveRDS(fit, file = file.path(results_dir, paste0(prefix, "_bapc_fit.rds")))
  write.csv(proj_age_mean, file = file.path(results_dir, paste0(prefix, "_agespec_number_mean.csv")), row.names = FALSE)
  write.csv(band_number,   file = file.path(results_dir, paste0(prefix, "_AllAges_Number_mean.csv")), row.names = FALSE)
  write.csv(asr,           file = file.path(results_dir, paste0(prefix, "_AgeStd_Rate_per100k.csv")), row.names = FALSE)
  
  ## ---- Plots (only plotBAPC) ----
  png(file.path(plots_dir, paste0(prefix, "_plotBAPC_ageStdProj.png")), width = 1800, height = 1300, res = 200)
  tryCatch({
    BAPC::plotBAPC(fit, scale = 1e5, type = "ageStdProj", showdata = TRUE)
  }, error = function(e) {
    plot.new(); text(0.5, 0.5, paste("plotBAPC ageStdProj failed:\n", e$message))
  })
  dev.off()
  
  png(file.path(plots_dir, paste0(prefix, "_plotBAPC_ageStdRate.png")), width = 1800, height = 1300, res = 200)
  tryCatch({
    BAPC::plotBAPC(fit, scale = 1e5, type = "ageStdRate", showdata = TRUE)
  }, error = function(e) {
    plot.new(); text(0.5, 0.5, paste("plotBAPC ageStdRate failed:\n", e$message))
  })
  dev.off()
  
  invisible(list(fit = fit, proj_age_mean = proj_age_mean, band_number = band_number, asr = asr))
}

## ---------------------------1
## 6) Batch run (Both only)
## ---------------------------1BAPC
all_results_band <- list()

for (m in measures_to_do) {
  for (bn in names(agebands)) {
    res <- run_bapc_one_band(measure_sel = m, band_name = bn, band_ages = agebands[[bn]])
    if (!is.null(res)) {
      key <- paste(m, bn, sep = "_")
      all_results_band[[key]] <- res
    }
  }
}

message("Done. Tables in: ", results_dir)
message("Plots  in: ", plots_dir)


##########################################################15种因素-all 不分性别年龄----
run_bapc_risk_both <- function(measure_sel, risk_sel) {
  
  ## --- 0. 参数设置 ---
  sex_sel <- "Both"
  # 文件名清洗
  risk_clean <- gsub(" ", "_", risk_sel) 
  risk_clean <- gsub("-", "_", risk_clean)
  
  message("--------------------------------------------------")
  message("正在运行 BAPC | 指标: ", measure_sel, " | 风险: ", risk_sel)
  
  years_all <- start_year:end_year 
  
  ## --- 1. 提取结局数据 ---
  dat_y <- dietary_annual1.0 %>%
    dplyr::filter(
      .data$location_name == "China",
      .data$measure_name  == .env$measure_sel,
      .data$metric_name   == "Number",
      .data$sex_name      == .env$sex_sel,
      .data$cause_name    == "Non-communicable diseases",
      .data$rei_name      == .env$risk_sel
    ) %>%
    dplyr::mutate(age_pop = age_allrisk_to_pop(.data$age_name)) %>%
    dplyr::filter(.data$age_pop %in% model_ages) %>%
    dplyr::transmute(
      year    = as.integer(.data$year),
      age_pop = as.character(.data$age_pop),
      val     = pmax(round(as.numeric(.data$val)), 0)
    )
  
  if (nrow(dat_y) == 0) {
    warning("无数据：", measure_sel, " | ", risk_sel, "（跳过）")
    return(NULL)
  }
  
  last_obs_year <- max(dat_y$year, na.rm = TRUE)
  if (last_obs_year < start_year) last_obs_year <- 2023
  
  y_obs_mat <- make_year_age_matrix(dat_y, "year", "age_pop", "val", start_year:last_obs_year, model_ages, 0)
  y_mat <- matrix(NA_real_, nrow = length(years_all), ncol = length(model_ages), dimnames = list(years_all, model_ages))
  y_mat[as.character(start_year:last_obs_year), ] <- y_obs_mat
  
  ## --- 2. 提取人口数据 ---
  pop_tmp <- pop_df1 %>%
    dplyr::filter(.data$location_name == "China", .data$year_id %in% years_all, .data$age_group_name %in% model_ages) %>%
    dplyr::mutate(sex_std = normalize_sex(.data$sex))
  
  pop_both <- pop_tmp %>% filter(sex_std == "Both")
  if (nrow(pop_both) > 0) {
    pop_mat <- make_year_age_matrix(pop_both, "year_id", "age_group_name", "val", years_all, model_ages, NA_real_)
  } else {
    pop_m <- pop_tmp %>% filter(sex_std == "Male")
    pop_f <- pop_tmp %>% filter(sex_std == "Female")
    if (nrow(pop_m)==0 | nrow(pop_f)==0) stop("人口缺失 Both")
    pop_mat <- make_year_age_matrix(pop_m, "year_id", "age_group_name", "val", years_all, model_ages, NA_real_) + 
      make_year_age_matrix(pop_f, "year_id", "age_group_name", "val", years_all, model_ages, NA_real_)
  }
  
  ## --- 3. 运行 BAPC 模型 ---
  npredict <- end_year - last_obs_year
  epi_df  <- base::data.frame(y_mat,  check.names = FALSE)
  pyrs_df <- base::data.frame(pop_mat, check.names = FALSE)
  
  epi_df[]  <- lapply(epi_df,  function(z) ifelse(is.na(z), NA, as.integer(round(as.numeric(z)))))
  pyrs_df[] <- lapply(pyrs_df, as.numeric)
  
  apc_obj <- BAPC::APCList(epi = epi_df, pyrs = pyrs_df, gf = 5)
  fit <- BAPC::BAPC(apc_obj, predict = list(npredict = npredict, retro = TRUE), secondDiff = FALSE, stdweight = wstand, verbose = FALSE)
  
  ## --- 4. 提取数据结果 ---
  proj_age <- agespec.proj(fit)
  proj_age_mean <- extract_stat_matrix(proj_age, "mean")
  for (a in model_ages) if (!a %in% names(proj_age_mean)) proj_age_mean[[a]] <- NA_real_
  proj_age_mean <- proj_age_mean[, model_ages, drop=FALSE]
  proj_age_mean$year <- as.integer(rownames(proj_age_mean))
  
  allage_number <- data.frame(year = proj_age_mean$year, mean = rowSums(proj_age_mean[, model_ages], na.rm = TRUE))
  
  asr <- agestd.rate(fit) %>% as.data.frame()
  asr$year <- as.integer(rownames(asr))
  if("mean" %in% names(asr)) asr$mean <- asr$mean * 1e5
  if("0.025quant" %in% names(asr)) asr$`0.025quant` <- asr$`0.025quant` * 1e5
  if("0.975quant" %in% names(asr)) asr$`0.975quant` <- asr$`0.975quant` * 1e5
  
  ## --- 5. 保存表格 ---
  prefix <- paste0("China_", measure_sel, "_", risk_clean, "_Both_", start_year, "_", end_year)
  sub_dir <- file.path(results_dir, "SubRisks")
  if(!dir.exists(sub_dir)) dir.create(sub_dir, recursive = TRUE)
  
  write.csv(allage_number, file = file.path(sub_dir, paste0(prefix, "_AllAges_Number.csv")), row.names = FALSE)
  write.csv(asr,           file = file.path(sub_dir, paste0(prefix, "_ASR.csv")), row.names = FALSE)
  
  ## --- 6. 画图 (新增部分) ---
  # 定义图片路径
  plot_path_rate <- file.path(sub_dir, paste0(prefix, "_plot_ASR.png"))
  plot_path_proj <- file.path(sub_dir, paste0(prefix, "_plot_Proj.png"))
  
  # 绘图 1: Age Standardized Rate (带扇形预测区间)
  png(plot_path_rate, width = 1800, height = 1300, res = 200)
  tryCatch({
    # scale=1e5 代表每10万
    plotBAPC(fit, scale = 1e5, type = "ageStdRate", showdata = TRUE) 
    title(main = paste0(risk_sel, " - ", measure_sel, " (ASR)"))
  }, error = function(e) {
    plot.new(); text(0.5, 0.5, paste("Plot Failed:", e$message))
  })
  dev.off()
  
  # 绘图 2: Age Standardized Projection (另一种视角的投影)
  png(plot_path_proj, width = 1800, height = 1300, res = 200)
  tryCatch({
    plotBAPC(fit, scale = 1e5, type = "ageStdProj", showdata = TRUE)
    title(main = paste0(risk_sel, " - ", measure_sel, " (Proj)"))
  }, error = function(e) {
    plot.new(); text(0.5, 0.5, paste("Plot Failed:", e$message))
  })
  dev.off()
  
  return(TRUE)
}

# 确保 risks_to_do 存在 (如果刚才那步没做，这里再做一次)
all_reis <- unique(dietary_annual1.0$rei_name)
risks_to_do <- setdiff(all_reis, "Dietary risks")

# 运行循环
for (m in measures_to_do) {
  for (r in risks_to_do) {
    tryCatch({
      run_bapc_risk_both(measure_sel = m, risk_sel = r)
    }, error = function(e) {
      message("Error in ", r, " - ", m, ": ", e$message)
    })
  }
}
message("Done! 图片和表格已保存至 results/SubRisks")
############################################################BAPC预测图拼接合成版----
############################################################1
## BAPC预测 + 单图(ASR) + 表格(CSV) + 拼图
## ✅ 所有“单图 + 表格 + 拼图”统一保存到 montage_dir（一个文件夹里找齐）
## ✅ 不再手动设置 ylab（避免与 plotBAPC 自带 y 轴标题重叠）
############################################################1

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringr)
  library(INLA)
  library(BAPC)
  library(png)
  library(grid)
})

options(timeout = 600)
options(stringsAsFactors = FALSE)

## ---------------------------1
## 1) Inputs must exist
## ---------------------------1
if (!exists("allrisk_cause")) stop("未检测到对象 allrisk_cause，请先读入 allrisk_cause。")
if (!exists("dietary_annual1.0")) stop("未检测到对象 dietary_annual1.0，请先读入 dietary_annual1.0。")

## 人口：你之前已读入的话可跳过；否则按你原路径读
if (!exists("pop_df1")) {
  pop_df <- read.csv("~/China GBD/GBD2023/合并数据/IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.csv")
  pop_df_2017before <- read.csv("~/China GBD/GBD2023/合并数据/IHME_POP_2017_2100_POP_PAST_Y2020M05D011.csv")
  if (ncol(pop_df) >= 16) pop_df <- pop_df[, -c(12, 13, 15, 16)]
  pop_df1 <- rbind(pop_df_2017before, pop_df)
}

## ---------------------------1
## 2) Output folder（全部输出都在 Montage）
## ---------------------------1
gbd2023_dir <- path.expand("~/China GBD/GBD2023")
out_root    <- file.path(gbd2023_dir, "BAPC")
montage_dir <- file.path(out_root, "plots", "Montage")
dir.create(montage_dir, recursive = TRUE, showWarnings = FALSE)

## ---------------------------1
## 3) Parameters
## ---------------------------1
start_year <- 1990
end_year   <- 2050

measures_to_do <- c("Deaths", "DALYs")
sexes_to_do    <- c("Both", "Female", "Male")

## 总人群/分性别：25+ 5岁组
model_ages <- c(
  "25 to 29","30 to 34","35 to 39","40 to 44","45 to 49",
  "50 to 54","55 to 59","60 to 64","65 to 69","70 to 74",
  "75 to 79","80 to 84","85 to 89","90 to 94","95 plus"
)

## 分年龄段：15+
agebands <- list(
  "15_49"  = c("15 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49"),
  "50_69"  = c("50 to 54","55 to 59","60 to 64","65 to 69"),
  "70plus" = c("70 to 74","75 to 79","80 to 84","85 to 89","90 to 94","95 plus")
)
ageband_title <- c("15_49"="Ages 15-49", "50_69"="Ages 50-69", "70plus"="Ages 70+")

## ---------------------------1
## 4) Standard population + weights
## ---------------------------1
gbd_age_standard <- data.frame(
  age = c("Early Neonatal","Late Neonatal","Post Neonatal","<1",
          "1 to 4","5 to 9","10 to 14","15 to 19","20 to 24","25 to 29",
          "30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59",
          "60 to 64","65 to 69","70 to 74","75 to 79","80 to 84","85 to 89",
          "90 to 94","95 plus"),
  pct = c(0.039706188,0.118021789,1.868264909,2.025992886,
          7.909875913,9.568418272,8.990277942,8.324362192,7.866450176,7.632917343,
          7.331511124,6.811055,6.136798184,5.509495973,4.921822565,4.345633072,
          3.684473754,2.991239718,2.272487547,1.607371655,1.113034599,0.61707008,
          0.255008068,0.084703935)
)

wstand <- gbd_age_standard$pct[match(model_ages, gbd_age_standard$age)]
if (any(is.na(wstand))) stop("wstand 匹配失败：检查 model_ages 与 gbd_age_standard$age。")
wstand <- wstand / sum(wstand)

## ---------------------------1
## 5) Helpers
## ---------------------------1
age_allrisk_to_pop <- function(x) {
  x <- trimws(x)
  x <- gsub(" years$", "", x)
  x <- gsub(" year$",  "", x)
  x <- gsub("([0-9]+)\\+", "\\1 plus", x)
  x <- gsub("([0-9]+)\\s*-\\s*([0-9]+)", "\\1 to \\2", x)
  x
}

normalize_sex <- function(x) {
  z <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    z %in% c("male", "m") ~ "Male",
    z %in% c("female", "f") ~ "Female",
    z %in% c("both", "both sexes", "all", "combined") ~ "Both",
    TRUE ~ as.character(x)
  )
}

make_year_age_matrix <- function(dt, year_col, age_col, value_col, years_all, ages_all, fill_value = 0) {
  dt <- as.data.table(dt)
  dt <- dt[, .(year  = as.integer(get(year_col)),
               age   = as.character(get(age_col)),
               value = as.numeric(get(value_col)))]
  wide <- dcast(dt, year ~ age, value.var = "value", fun.aggregate = sum, fill = fill_value)
  wide <- merge(data.table(year = years_all), wide, by = "year", all.x = TRUE)
  for (a in ages_all) if (!a %in% names(wide)) wide[[a]] <- fill_value
  wide <- wide[, c("year", ages_all), with = FALSE]
  mat <- as.matrix(wide[, -1, with = FALSE])
  rownames(mat) <- wide$year
  mat
}

extract_stat_matrix <- function(x, stat = "mean") {
  df <- as.data.frame(x)
  nm <- names(df)
  cols <- grep(paste0("^", stat, "\\."), nm, value = TRUE)
  if (length(cols) == 0) cols <- nm[grepl(stat, nm, fixed = TRUE)]
  out <- df[, cols, drop = FALSE]
  
  cn <- sub(paste0("^", stat, "\\."), "", cols)
  cn <- gsub("\\.", " ", cn)
  cn <- trimws(cn)
  cn <- age_allrisk_to_pop(cn)
  colnames(out) <- cn
  out
}

risk_to_clean <- function(x) {
  x <- trimws(x)
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("-", "_", x, fixed = TRUE)
  x
}

## ---------------------------1
## 6) Plot wrapper（只加 title，不加 ylab）
## ---------------------------1
plot_bapc_png <- function(fit, out_png, type = "ageStdRate", main = "", scale = 1e5,
                          width = 1800, height = 1300, res = 200) {
  png(out_png, width = width, height = height, res = res)
  tryCatch({
    BAPC::plotBAPC(fit, scale = scale, type = type, showdata = TRUE)
    title(main = main)  # ✅ 不设置 ylab，避免重叠
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("plotBAPC failed:\n", e$message))
  })
  dev.off()
}

## ---------------------------1
## 7) Save tables (统一输出到 montage_dir)
## ---------------------------1
save_tables_from_fit <- function(fit, prefix, ages_vec, out_dir = montage_dir) {
  ## 1) ASR per100k
  asr <- BAPC::agestd.rate(fit) %>% as.data.frame()
  asr$year <- as.integer(rownames(asr))
  if ("mean" %in% names(asr)) asr$mean <- asr$mean * 1e5
  if ("0.025quant" %in% names(asr)) asr$`0.025quant` <- asr$`0.025quant` * 1e5
  if ("0.975quant" %in% names(asr)) asr$`0.975quant` <- asr$`0.975quant` * 1e5
  asr <- asr %>% dplyr::relocate(year)
  write.csv(asr, file = file.path(out_dir, paste0(prefix, "_ASR_per100k.csv")), row.names = FALSE)
  
  ## 2) agespec number mean + AllAges number mean
  proj_age <- BAPC::agespec.proj(fit)
  proj_age_mean <- extract_stat_matrix(proj_age, "mean")
  
  for (a in ages_vec) if (!a %in% names(proj_age_mean)) proj_age_mean[[a]] <- NA_real_
  proj_age_mean <- proj_age_mean[, ages_vec, drop = FALSE]
  proj_age_mean$year <- as.integer(rownames(proj_age_mean))
  proj_age_mean <- proj_age_mean %>% dplyr::relocate(year)
  
  allage_number <- data.frame(
    year = proj_age_mean$year,
    mean = rowSums(proj_age_mean[, ages_vec, drop = FALSE], na.rm = TRUE)
  )
  
  write.csv(proj_age_mean,  file = file.path(out_dir, paste0(prefix, "_AgeSpec_Number_mean.csv")), row.names = FALSE)
  write.csv(allage_number,  file = file.path(out_dir, paste0(prefix, "_AllAges_Number_mean.csv")), row.names = FALSE)
  
  invisible(TRUE)
}

## ---------------------------1
## 8) 总人群/分性别：allrisk_cause（输出到 montage_dir）
## ---------------------------1
run_bapc_one <- function(measure_sel, sex_sel) {
  
  years_all <- start_year:end_year
  
  dat_y <- allrisk_cause %>%
    filter(location_name == "China",
           measure_name  == measure_sel,
           metric_name   == "Number",
           sex_name      == sex_sel) %>%
    mutate(age_pop = age_allrisk_to_pop(age_name)) %>%
    filter(age_pop %in% model_ages) %>%
    transmute(year = as.integer(year),
              age_pop = as.character(age_pop),
              val = pmax(round(as.numeric(val)), 0))
  
  if (nrow(dat_y) == 0) {
    warning("无数据：", measure_sel, " | ", sex_sel, "（跳过）")
    return(NULL)
  }
  
  last_obs_year <- max(dat_y$year, na.rm = TRUE)
  y_obs_mat <- make_year_age_matrix(dat_y, "year", "age_pop", "val", start_year:last_obs_year, model_ages, 0)
  
  y_mat <- matrix(NA_real_, nrow = length(years_all), ncol = length(model_ages),
                  dimnames = list(years_all, model_ages))
  y_mat[as.character(start_year:last_obs_year), ] <- y_obs_mat
  
  pop_base <- pop_df1 %>%
    mutate(location_name  = trimws(location_name),
           sex_std        = normalize_sex(sex),
           year_id        = as.integer(year_id),
           age_group_name = trimws(age_group_name),
           val            = pmax(round(as.numeric(val)), 0)) %>%
    filter(location_name == "China",
           year_id %in% years_all,
           age_group_name %in% model_ages)
  
  if (sex_sel == "Both") {
    pop_both <- pop_base %>% filter(sex_std == "Both") %>% select(year_id, age_group_name, val)
    if (nrow(pop_both) > 0) {
      pop_mat <- make_year_age_matrix(pop_both, "year_id", "age_group_name", "val", years_all, model_ages, NA_real_)
    } else {
      pop_m <- pop_base %>% filter(sex_std == "Male")   %>% select(year_id, age_group_name, val)
      pop_f <- pop_base %>% filter(sex_std == "Female") %>% select(year_id, age_group_name, val)
      if (nrow(pop_m) == 0 || nrow(pop_f) == 0) stop("人口无法合成 Both（缺 Male 或 Female）。")
      pop_mat <- make_year_age_matrix(pop_m, "year_id", "age_group_name", "val", years_all, model_ages, NA_real_) +
        make_year_age_matrix(pop_f, "year_id", "age_group_name", "val", years_all, model_ages, NA_real_)
    }
  } else {
    pop_one <- pop_base %>% filter(sex_std == sex_sel) %>% select(year_id, age_group_name, val)
    if (nrow(pop_one) == 0) stop("人口数据缺失：China | ", sex_sel)
    pop_mat <- make_year_age_matrix(pop_one, "year_id", "age_group_name", "val", years_all, model_ages, NA_real_)
  }
  
  if (any(is.na(pop_mat))) stop("人口矩阵存在 NA。")
  
  npredict <- end_year - last_obs_year
  
  epi_df  <- base::data.frame(y_mat,   check.names = FALSE)
  pyrs_df <- base::data.frame(pop_mat, check.names = FALSE)
  epi_df[]  <- lapply(epi_df, function(z) { z <- as.numeric(z); ifelse(is.na(z), NA, as.integer(round(z))) })
  pyrs_df[] <- lapply(pyrs_df, as.numeric)
  
  apc_obj <- BAPC::APCList(epi = epi_df, pyrs = pyrs_df, gf = 5)
  fit <- BAPC::BAPC(apc_obj,
                    predict = list(npredict = npredict, retro = TRUE),
                    secondDiff = FALSE,
                    stdweight = wstand,
                    verbose = FALSE)
  
  prefix <- paste0("China_", measure_sel, "_", sex_sel, "_", start_year, "_", end_year)
  
  ## 单图（ASR）
  plot_bapc_png(
    fit,
    out_png = file.path(montage_dir, paste0(prefix, "_plotBAPC_ageStdRate.png")),
    type = "ageStdRate",
    main = paste0("China - ", measure_sel, " - ", sex_sel, " (ASR)")
  )
  
  ## 表格（全部输出到 montage_dir）
  save_tables_from_fit(fit, prefix, ages_vec = model_ages, out_dir = montage_dir)
  
  ## 可选：fit 也放 montage_dir（方便复现，不想要可删掉这行）
  saveRDS(fit, file = file.path(montage_dir, paste0(prefix, "_bapc_fit.rds")))
  
  invisible(fit)
}

## ---------------------------1
## 9) 分年龄段：Both only（输出到 montage_dir）
## ---------------------------1
run_bapc_one_band <- function(measure_sel, band_name, band_ages) {
  
  sex_fixed <- "Both"
  
  pop_years <- pop_df1 %>%
    mutate(location_name = trimws(location_name), year_id = as.integer(year_id)) %>%
    filter(location_name == "China") %>%
    pull(year_id)
  pop_years <- pop_years[!is.na(pop_years)]
  end_year_use <- min(end_year, max(pop_years))
  years_all <- start_year:end_year_use
  
  dat_y <- allrisk_cause %>%
    filter(location_name == "China",
           measure_name  == measure_sel,
           metric_name   == "Number",
           sex_name      == sex_fixed) %>%
    mutate(age_pop = age_allrisk_to_pop(age_name)) %>%
    filter(age_pop %in% band_ages) %>%
    transmute(year = as.integer(year),
              age_pop = as.character(age_pop),
              val = pmax(round(as.numeric(val)), 0))
  
  if (nrow(dat_y) == 0) {
    warning("无数据：", measure_sel, " | ", band_name, "（跳过）")
    return(NULL)
  }
  
  last_obs_year <- max(dat_y$year, na.rm = TRUE)
  if (last_obs_year > end_year_use) {
    end_year_use <- last_obs_year
    years_all <- start_year:end_year_use
  }
  
  y_obs_mat <- make_year_age_matrix(dat_y, "year", "age_pop", "val", start_year:last_obs_year, band_ages, 0)
  y_mat <- matrix(NA_real_, nrow = length(years_all), ncol = length(band_ages),
                  dimnames = list(years_all, band_ages))
  y_mat[as.character(start_year:last_obs_year), ] <- y_obs_mat
  
  pop_base <- pop_df1 %>%
    mutate(location_name  = trimws(location_name),
           sex_std        = normalize_sex(sex),
           year_id        = as.integer(year_id),
           age_group_name = trimws(age_group_name),
           val            = pmax(round(as.numeric(val)), 0)) %>%
    filter(location_name == "China",
           year_id %in% years_all,
           age_group_name %in% band_ages)
  
  pop_both <- pop_base %>% filter(sex_std == "Both") %>% select(year_id, age_group_name, val)
  if (nrow(pop_both) > 0) {
    pop_mat <- make_year_age_matrix(pop_both, "year_id", "age_group_name", "val", years_all, band_ages, NA_real_)
  } else {
    pop_m <- pop_base %>% filter(sex_std == "Male")   %>% select(year_id, age_group_name, val)
    pop_f <- pop_base %>% filter(sex_std == "Female") %>% select(year_id, age_group_name, val)
    if (nrow(pop_m) == 0 || nrow(pop_f) == 0) stop("人口无法合成 Both（缺 Male 或 Female）。")
    pop_mat <- make_year_age_matrix(pop_m, "year_id", "age_group_name", "val", years_all, band_ages, NA_real_) +
      make_year_age_matrix(pop_f, "year_id", "age_group_name", "val", years_all, band_ages, NA_real_)
  }
  
  if (any(is.na(pop_mat))) stop("人口矩阵存在 NA。")
  
  w_band <- gbd_age_standard$pct[match(band_ages, gbd_age_standard$age)]
  if (any(is.na(w_band))) stop("band 标准人口匹配失败：", band_name)
  w_band <- w_band / sum(w_band)
  
  npredict <- end_year_use - last_obs_year
  
  epi_df  <- base::data.frame(y_mat,   check.names = FALSE)
  pyrs_df <- base::data.frame(pop_mat, check.names = FALSE)
  epi_df[]  <- lapply(epi_df, function(z) { z <- as.numeric(z); ifelse(is.na(z), NA, as.integer(round(z))) })
  pyrs_df[] <- lapply(pyrs_df, as.numeric)
  
  apc_obj <- BAPC::APCList(epi = epi_df, pyrs = pyrs_df, gf = 5)
  fit <- BAPC::BAPC(apc_obj,
                    predict = list(npredict = npredict, retro = TRUE),
                    secondDiff = FALSE,
                    stdweight = w_band,
                    verbose = FALSE)
  
  prefix <- paste0("China_", measure_sel, "_Both_", band_name, "_", start_year, "_", end_year_use)
  
  plot_bapc_png(
    fit,
    out_png = file.path(montage_dir, paste0(prefix, "_plotBAPC_ageStdRate.png")),
    type = "ageStdRate",
    main = paste0("China - ", measure_sel, " - Both - ", ageband_title[band_name], " (ASR)")
  )
  
  save_tables_from_fit(fit, prefix, ages_vec = band_ages, out_dir = montage_dir)
  saveRDS(fit, file = file.path(montage_dir, paste0(prefix, "_bapc_fit.rds")))
  
  invisible(fit)
}

## ---------------------------1
## 10) 15种膳食风险（Both）：输出到 montage_dir
## ---------------------------1
risk_order <- c(
  "Diet high in sodium",
  "Diet low in fruits",
  "Diet low in whole grains",
  "Diet low in legumes",
  "Diet low in nuts and seeds",
  "Diet high in trans fatty acids",
  "Diet low in seafood omega-3 fatty acids",
  "Diet low in fiber",
  "Diet high in red meat",
  "Diet low in milk",
  "Diet low in omega-6 polyunsaturated fatty acids",
  "Diet low in vegetables",
  "Diet high in processed meat",
  "Diet low in calcium",
  "Diet high in sugar-sweetened beverages"
)

run_bapc_risk_both <- function(measure_sel, risk_sel) {
  
  sex_sel <- "Both"
  risk_clean <- risk_to_clean(risk_sel)
  years_all <- start_year:end_year
  
  dat_y <- dietary_annual1.0 %>%
    filter(location_name == "China",
           measure_name  == measure_sel,
           metric_name   == "Number",
           sex_name      == sex_sel,
           cause_name    == "Non-communicable diseases",
           rei_name      == risk_sel) %>%
    mutate(age_pop = age_allrisk_to_pop(age_name)) %>%
    filter(age_pop %in% model_ages) %>%
    transmute(year = as.integer(year),
              age_pop = as.character(age_pop),
              val = pmax(round(as.numeric(val)), 0))
  
  if (nrow(dat_y) == 0) {
    warning("无数据：", measure_sel, " | ", risk_sel, "（跳过）")
    return(NULL)
  }
  
  last_obs_year <- max(dat_y$year, na.rm = TRUE)
  npredict <- end_year - last_obs_year
  
  y_obs_mat <- make_year_age_matrix(dat_y, "year", "age_pop", "val", start_year:last_obs_year, model_ages, 0)
  y_mat <- matrix(NA_real_, nrow = length(years_all), ncol = length(model_ages),
                  dimnames = list(years_all, model_ages))
  y_mat[as.character(start_year:last_obs_year), ] <- y_obs_mat
  
  pop_tmp <- pop_df1 %>%
    mutate(location_name  = trimws(location_name),
           sex_std        = normalize_sex(sex),
           year_id        = as.integer(year_id),
           age_group_name = trimws(age_group_name),
           val            = pmax(round(as.numeric(val)), 0)) %>%
    filter(location_name == "China",
           year_id %in% years_all,
           age_group_name %in% model_ages)
  
  pop_both <- pop_tmp %>% filter(sex_std == "Both") %>% select(year_id, age_group_name, val)
  if (nrow(pop_both) > 0) {
    pop_mat <- make_year_age_matrix(pop_both, "year_id", "age_group_name", "val", years_all, model_ages, NA_real_)
  } else {
    pop_m <- pop_tmp %>% filter(sex_std == "Male")   %>% select(year_id, age_group_name, val)
    pop_f <- pop_tmp %>% filter(sex_std == "Female") %>% select(year_id, age_group_name, val)
    if (nrow(pop_m) == 0 || nrow(pop_f) == 0) stop("人口缺失 Both 且无法 Male+Female 合成 Both。")
    pop_mat <- make_year_age_matrix(pop_m, "year_id", "age_group_name", "val", years_all, model_ages, NA_real_) +
      make_year_age_matrix(pop_f, "year_id", "age_group_name", "val", years_all, model_ages, NA_real_)
  }
  
  if (any(is.na(pop_mat))) stop("人口矩阵存在 NA。")
  
  epi_df  <- base::data.frame(y_mat,   check.names = FALSE)
  pyrs_df <- base::data.frame(pop_mat, check.names = FALSE)
  epi_df[]  <- lapply(epi_df, function(z) ifelse(is.na(z), NA, as.integer(round(as.numeric(z)))))
  pyrs_df[] <- lapply(pyrs_df, as.numeric)
  
  apc_obj <- BAPC::APCList(epi = epi_df, pyrs = pyrs_df, gf = 5)
  fit <- BAPC::BAPC(apc_obj,
                    predict = list(npredict = npredict, retro = TRUE),
                    secondDiff = FALSE,
                    stdweight = wstand,
                    verbose = FALSE)
  
  prefix <- paste0("China_", measure_sel, "_", risk_clean, "_Both_", start_year, "_", end_year)
  
  plot_bapc_png(
    fit,
    out_png = file.path(montage_dir, paste0(prefix, "_plot_ASR.png")),
    type = "ageStdRate",
    main = paste0(risk_sel, " - ", measure_sel, " (ASR)")
  )
  
  save_tables_from_fit(fit, prefix, ages_vec = model_ages, out_dir = montage_dir)
  saveRDS(fit, file = file.path(montage_dir, paste0(prefix, "_bapc_fit.rds")))
  
  invisible(fit)
}

## ---------------------------1
## 11) Batch run：总/性别 + 年龄段 + 15风险
## ---------------------------1
for (m in measures_to_do) {
  
  ## 总人群+分性别
  for (sx in sexes_to_do) {
    tryCatch(run_bapc_one(m, sx),
             error = function(e) message("Total/Sex error: ", m, " | ", sx, " => ", e$message))
  }
  
  ## 分年龄段（Both）
  for (bn in names(agebands)) {
    tryCatch(run_bapc_one_band(m, bn, agebands[[bn]]),
             error = function(e) message("Ageband error: ", m, " | ", bn, " => ", e$message))
  }
  
  ## 15风险（Both）
  for (r in risk_order) {
    tryCatch(run_bapc_risk_both(m, r),
             error = function(e) message("Risk error: ", m, " | ", r, " => ", e$message))
  }
}

## ---------------------------1
## 12) 拼图函数（自动按第一张图尺寸拼接）
## ---------------------------1
montage_png_grid <- function(img_paths, out_png, ncol, nrow, res = 200) {
  stopifnot(ncol >= 1, nrow >= 1)
  need <- ncol * nrow
  if (length(img_paths) < need) img_paths <- c(img_paths, rep(NA_character_, need - length(img_paths)))
  if (length(img_paths) > need) img_paths <- img_paths[1:need]
  
  first_ok <- img_paths[!is.na(img_paths) & file.exists(img_paths)][1]
  if (length(first_ok) == 0) stop("No valid images found to infer size.")
  tmp <- png::readPNG(first_ok)
  img_h <- dim(tmp)[1]; img_w <- dim(tmp)[2]
  
  blank_img <- array(1, dim = c(img_h, img_w, 4))
  imgs <- lapply(img_paths, function(p) if (!is.na(p) && file.exists(p)) png::readPNG(p) else blank_img)
  
  png(out_png, width = img_w * ncol, height = img_h * nrow, res = res)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow, ncol)))
  for (i in seq_along(imgs)) {
    rr <- ceiling(i / ncol)
    cc <- i - (rr - 1) * ncol
    pushViewport(viewport(layout.pos.row = rr, layout.pos.col = cc))
    grid.raster(imgs[[i]], interpolate = TRUE)
    popViewport()
  }
  dev.off()
  message("Montage saved: ", out_png)
}

## ---------------------------1
## 13) 两类拼图输出（都保存在 montage_dir）
## A) 15风险：5x3（15张）
## B) 性别3 + 年龄段3：3x2（6张）
## ---------------------------1
make_montage_15risks <- function(measure_sel) {
  imgs <- sapply(risk_order, function(r) {
    pref <- paste0("China_", measure_sel, "_", risk_to_clean(r), "_Both_", start_year, "_", end_year)
    file.path(montage_dir, paste0(pref, "_plot_ASR.png"))
  }, USE.NAMES = FALSE)
  
  out_png <- file.path(montage_dir,
                       paste0("China_", measure_sel, "_Dietary15_Both_", start_year, "_", end_year, "_ASR_5x3.png"))
  montage_png_grid(imgs, out_png, ncol = 5, nrow = 3)
}

make_montage_sex_plus_age <- function(measure_sel) {
  ## 3张性别（ASR）
  sex_imgs <- sapply(c("Both","Female","Male"), function(sx) {
    pref <- paste0("China_", measure_sel, "_", sx, "_", start_year, "_", end_year)
    file.path(montage_dir, paste0(pref, "_plotBAPC_ageStdRate.png"))
  }, USE.NAMES = FALSE)
  
  ## 3张年龄段：end_year_use 可能不是 2050，用 glob 找 montage_dir 内的文件
  age_imgs <- sapply(c("15_49","50_69","70plus"), function(bn) {
    pat <- file.path(montage_dir, paste0("China_", measure_sel, "_Both_", bn, "_", start_year, "_", "*_plotBAPC_ageStdRate.png"))
    cand <- Sys.glob(pat)
    if (length(cand) == 0) NA_character_ else cand[1]
  }, USE.NAMES = FALSE)
  
  imgs <- c(sex_imgs, age_imgs)
  
  out_png <- file.path(montage_dir,
                       paste0("China_", measure_sel, "_SexPlusAgeband_ASR_3x2_", start_year, "_", end_year, ".png"))
  montage_png_grid(imgs, out_png, ncol = 3, nrow = 2)
}

for (m in measures_to_do) {
  make_montage_15risks(m)
  make_montage_sex_plus_age(m)
}

message("ALL DONE. Everything (single plots + tables + montages) saved in: ", montage_dir)
