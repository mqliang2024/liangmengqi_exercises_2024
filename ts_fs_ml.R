#
#Author: liangmengqi
#Copyright   Copyright 2024-liangmengqi
#Email:mqliang2023@mail.ustc.edu.cn
#
#Date:2024-05-24
#
#Script Name:Homework-10-liangmengqi
#
#Script Description:Download the monitoring data of fishes of the Le Doubs from the following website :
# https://figshare.com/articles/dataset/Data_for_Contemporary_loss_of_genetic_diversity_in_wild_fish_populations_reduces_biomass_stability_over_time_/13095380
#1. Focusing on the data of fishes’ densities at the station of VERCah, selecting the species of VIA to create a timeseries object and visualizing it.
#2. Extracting the features of this timeseries and building a forecast model with tidymodels package.
#
#
#SETUP ----------------------------------------------
# 安装和加载所需的包
install.packages("forecast")
install.packages("timetk")
install.packages("tsibble")
install.packages("tidymodels")
install.packages("modeltime")
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

#已提前从"https://figshare.com/articles/dataset/Data_for_Contemporary_loss_of_genetic_diversity_in_wild_fish_populations_reduces_biomass_stability_over_time_/13095380"
#下载数据到电脑本地

# 读取数据文件
data=read.table('D:/data-driven-ecology/liangmengqi_exercises_2024/Prunier et al._RawBiomassData.txt',h=TRUE)
head(data) # 查看数据的前几行

# 数据清洗
data_clean <- data |>
  dplyr::select(-YEAR) |> # 去除年份列
  distinct() # 去除重复数据
# 检查站点和物种的唯一值
unique(data_clean$STATION) # 查看站点的唯一值
table(data_clean$STATION) # 查看站点的频数分布
unique(data_clean$SP) # 查看物种的唯一值
table(data_clean$SP) # 查看物种的频数分布

# # 选择VERCah站点的VAI物种数据
mydata <- data_clean |>
  subset(STATION=="VERCah" & SP == "VAI")

# 创建时间序列对象
time_series_data <- mydata %>% 
  select(-c(1:5)) %>% 
  as.matrix() # 选择除前三列以外的所有列作为数据

data_ts = ts(data = time_series_data,
             start = c(1994), # 开始年份为1994
             frequency = 1)  # 频率为1，表示每年记录一次数据

# 加载forecast库，用于处理ggplot2的autoplot()函数
library(forecast)

# 加载ggplot2库
library(ggplot2)

# 使用autoplot()函数绘制时间序列图，并设置分面、标题和坐标轴标签
autoplot(data_ts, facets = TRUE) +
  ggtitle("VAI") +
  ylab("变化量") + xlab("年份")

# 加载timetk库
library(timetk)

# 选择特定的站点和物种
mydata <- data_clean |>
  subset(STATION=="VERCah" & SP == "VAI") 

# 加载tidyverse和tsibble库
library(tidyverse)
library(tsibble)

# 将数据转换为tibble格式，并进行一些列操作和重命名
datatk_ts <- mydata |>
  tk_tbl() |> # 转换为tibble
  select(-1) |>
  rename(date = DATE) |>
  relocate(date, .before = STATION) |>
  pivot_longer( # 转换为长格式
    cols = c("BIOMASS", "DENSITY"))

# 使用timetk绘制时间序列图
datatk_ts |>
  group_by(name) |>
  plot_time_series(date, value, 
                   .facet_ncol = 2, 
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "VAI"
  )

# 对数据进行按年汇总，并填充缺失值，然后绘制时间序列图
datatk_ts1 <- 
  datatk_ts |>
  group_by(name) |>
  summarise_by_time(
    date, 
    .by = "year",
    value = first(value)) |>
  pad_by_time(date, .by = "year") |>
  plot_time_series(date, value,
                   .facet_ncol = 2,
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "VAI"
  )

##----------------------------------------------------
# 提取时间序列特征
#  生成新特征
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)

# 从数据集中选择特定条件的数据
mydata <- data_clean |> 
  subset(STATION=="VERCah" & SP == "VAI")

# 提取生物量数据
library(tibble)
biomtk_ts <- mydata %>%
  as_tibble() %>% # 转换为 tibble 格式
  dplyr::select(DATE, BIOMASS)  # 选择日期和生物量列

# 检查时间序列的规则性
library(tsibble)
library(dplyr)

# 对时间序列数据进行摘要统计诊断
biomtk_ts %>%
  summarise(
    min_date = min(DATE),
    max_date = max(DATE),
    num_obs = n(),
    avg_biomass = mean(BIOMASS),
    sd_biomass = sd(BIOMASS)
  )
##----------------------------------------------------
# 基于日历的特征

library(dplyr)
library(lubridate)

biomtk_ts_features_C <- biomtk_ts %>%
  mutate(
    BIOMASS = log1p(BIOMASS),  # 对生物量进行log(x+1)转换
    BIOMASS = scale(BIOMASS)  # 对转换后的生物量进行标准化
  ) %>%
  mutate(
    year = year(DATE),  # 提取年份信息
    month = month(DATE),  # 提取月份信息
    day = day(DATE)  # 提取日期信息
  )

glimpse(biomtk_ts_features_C)  # 查看数据集的摘要信息

biomtk_ts_features_C  # 显示数据集
dim(biomtk_ts_features_C)
summary(biomtk_ts_features_C)
library(ggplot2)
ggplot(biomtk_ts_features_C, aes(x = DATE, y = BIOMASS)) +
  geom_point()

# 执行线性回归
timetk::plot_time_series_regression(
  .date_var = DATE,  # 时间变量
  .data = biomtk_ts_features_C,  # 数据集
  .formula = BIOMASS ~ as.Date(biomtk_ts_features_C$DATE) + year +  month + day,  # 回归公式
  .show_summary = TRUE  # 显示回归摘要
)

##----------------------------------------------------
# 傅立叶项特征

biomtk_ts_features_F <- biomtk_ts |> 
  mutate(DATE = as.Date(DATE)) |> 
   # 测量转换：Log(x+1) 方差减少
  mutate(BIOMASS = log1p(x = BIOMASS)) |> 
  # 测量转换：标准化
  mutate(BIOMASS = standardize_vec(BIOMASS)) |> 
  # 添加傅立叶特征
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1) 

biomtk_ts_features_F  # 显示数据集

# 执行线性回归
plot_time_series_regression(
  .date_var = DATE,  # 时间变量
  .data = biomtk_ts_features_F,  # 数据集
  .formula = BIOMASS ~ as.numeric(DATE) + DATE_sin5_K1 + DATE_cos5_K1,  # 回归公式
  .show_summary = TRUE  # 显示回归摘要
)
##----------------------------------------------------
##  滞后特征

# 创建包含滞后特征的数据框架
biomtk_ts_features_L <- biomtk_ts |>
  # 测量转换：使用Log(x+1)进行方差减少
  mutate(BIOMASS =  log1p(x = BIOMASS)) |>
  # 测量转换：标准化
  mutate(BIOMASS =  standardize_vec(BIOMASS)) |>
  # 添加滞后特征
  tk_augment_lags(.value = BIOMASS, .lags = c(4, 7))  

# 输出数据框架
biomtk_ts_features_L 

# 执行线性回归
plot_time_series_regression(.date_var = DATE,
                            .data = biomtk_ts_features_L_filled,
                            .formula = BIOMASS ~ as.Date(biomtk_ts_features_L$DATE) + BIOMASS_lag4 + BIOMASS_lag7,
                            .show_summary = TRUE)


##-----------------------------------------------------------
## 移动窗口统计
# 创建包含移动窗口统计特征的数据框架
biomtk_ts_features_M <- biomtk_ts |>
  # 测量转换：使用Log(x+1)进行方差减少
  mutate(BIOMASS =  log1p(x = BIOMASS)) |>
  # 测量转换：标准化
  mutate(BIOMASS =  standardize_vec(BIOMASS)) |>
  # 添加滞后特征
  tk_augment_lags(.value = BIOMASS, .lags = c(4, 7)) |>
  # 添加移动窗口统计特征
  tk_augment_slidify(.value   = contains("BIOMASS"),
                     .f       = ~ mean(.x, na.rm = TRUE), 
                     .period  = c(3, 6),
                     .partial = TRUE,
                     .align   = "center")
# 输出数据框架
biomtk_ts_features_M 

# 执行线性回归
plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_ts_features_M,
                            .formula = BIOMASS ~ as.Date(biomtk_ts_features_M$DATE) + 
                              BIOMASS_roll_3 + BIOMASS_roll_6,
                            .show_summary = TRUE)
##-------------------------------------------------------------
# 将所有特征合并 
# 创建包含所有特征的数据框架
biomtk_ts_features_all <- biomtk_ts |>
  mutate(DATE = as.Date(DATE)) |>
   # 测量转换：使用Log(x+1)进行方差减少
  mutate(BIOMASS =  log1p(x = BIOMASS)) |>
  # 测量转换：标准化
  mutate(BIOMASS =  standardize_vec(BIOMASS)) |>
  # 添加基于日历的特征
  tk_augment_timeseries_signature(.date_var = DATE) |>
  select(-diff, -matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) |>
  select(-month.lbl) |>
  # 归一化指标向量
  mutate(index.num = normalize_vec(x = index.num)) |>
  mutate(year = normalize_vec(x = year)) |>
  # 添加傅里叶特征
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1) |>
  # 添加滞后特征
  tk_augment_lags(.value = BIOMASS, .lags = c(4,7)) |>
  # 添加移动窗口统计特征
  tk_augment_slidify(.value   = contains("BIOMASS"),
                     .f       = ~ mean(.x, na.rm = TRUE), 
                     .period  = c(3, 6),
                     .partial = TRUE,
                     .align   = "center")

# 查看数据框架结构
biomtk_ts_features_all |>
  glimpse()

# 执行线性回归
plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_ts_features_all,
                            .formula = BIOMASS ~ as.numeric(DATE) + 
                              index.num + year + half + quarter + month + 
                              DATE_sin5_K1 + DATE_sin5_K1 + 
                              BIOMASS_roll_3 + BIOMASS_roll_6,
                            .show_summary = TRUE)

##----------------------------------------------------
# 构建预测模型
# 训练/测试集划分和特征创建
# 计算训练集大小，将数据分为训练集和测试集
n_rows <- nrow(biomtk_ts)                  # 获取数据行数
train_rows <- round(0.8 * n_rows)          # 计算训练集大小（80%）

train_data <- biomtk_ts |>
  slice(1:train_rows)                      # 从数据中取出前 train_rows 行作为训练集
test_data <- biomtk_ts |>
  slice((train_rows):n_rows)               # 从数据中取出剩余部分作为测试集

# 绘制训练集和测试集的生物量随时间变化的折线图
ggplot() +
  geom_line(data = train_data,             # 添加训练集数据的折线图
            aes(x = DATE, y = BIOMASS, color = "Training"), 
            linewidth = 1) +
  geom_line(data = test_data,              # 添加测试集数据的折线图
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  scale_color_manual(values = c("Training" = "blue", 
                                "Test" = "red")) +  # 设置线条颜色
  labs(title = "Training and Test Sets",   # 设置图表标题
       x = "DATE", y = "BIOMASS") +        # 设置坐标轴标签
  theme_minimal()                          # 设置图表主题为简洁风格

# 创建特征工程
library(recipes)                           # 加载 recipes 包，用于特征工程
library(tidymodels)                        # 加载 tidymodels 包，提供了用于建模的函数
train_data <- train_data %>%
  mutate(DATE = ymd(DATE))  # 将 DATE 列转换为日期类型
recipe_spec_final <- recipe(BIOMASS ~ ., train_data) |>  # 创建特征工程蓝图
  step_timeseries_signature(DATE) |>       # 为时间戳添加时间序列的特征
  step_rm(DATE) |>                         # 移除日期列
  step_zv(all_predictors()) |>             # 移除方差为零的特征
  step_dummy(all_nominal_predictors(), one_hot = TRUE)  # 对所有分类变量进行独热编码

summary(prep(recipe_spec_final))           # 总结特征工程过程

# 训练和评估模型
# 导入必要的库和函数
library(tidymodels)
library(patchwork)

test_data$DATE <- as.Date(test_data$DATE)

# 训练一个随机森林模型
# 创建一个工作流对象
rf <- workflow() |>
  # 添加随机森林算法模型
  add_model(
    spec = rand_forest("regression") |> set_engine("ranger")
  ) |>
  # 添加数据处理步骤（包括特征工程等）
  add_recipe(recipe_spec_final) |>
  # 在训练集上拟合模型
  fit(train_data)

# 输出随机森林模型对象
rf
# 在测试集上进行模型性能评估
rf_test <- rf |> 
  # 在测试集上进行预测
  predict(test_data) |>
  # 将预测结果与测试集数据合并
  bind_cols(test_data) 

# 输出合并后的测试集数据
rf_test

# 创建预测结果可视化图表
prf <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "Train"), 
            linewidth = 1) +
  geom_line(data = rf_test, 
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  geom_line(data = rf_test, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "rf-Train/Test and validation", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()

# 输出预测结果可视化图表
prf

# 计算预测误差
rf_test |>
  metrics(BIOMASS, .pred)

##--------------------------------------------------------
# 保存工作
# 将工作保存到一个RDS文件中
workflow_Doubs <- list(
  workflows = list(
    wflw_random_forest = rf
  ),
  calibration = list(
    calibration_tbl = calibrated_tbl
  )
)
# 将工作保存到RDS文件
# 将 workflow_Doubs 对象保存到 RDS 文件中
saveRDS(workflow_Doubs, "D:/data-driven-ecology/liangmengqi_exercises_2024/workflows_Doubs_list.rds")