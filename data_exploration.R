#
#Author: liangmengqi
#Copyright   Copyright 2024-liangmengqi
#Email:mqliang2023@mail.ustc.edu.cn
#
#Date:2024-04-16
#
#Script Name:Homework-06-liangmengqi
#
#Script Description:1. write a short code to remove the sites with missing data of the Doubs dataset, 
#and detect whether environmental factors are collinearity.
# 2. Analysis on the relationships between fishes and environment factors and visualize such relationships.
#
#
#SETUP ----------------------------------------------

# 前期准备
# 安装必要的包
install.packages("corrplot")

#加载必要的包
library(ade4)
library(caret)
library(corrplot)
library(ggplot2)

#加载Doubs数据集并查看相关信息
data(doubs)
str(doubs)
head(doubs)
class(doubs)
# 摘要统计
# 使用summary() 函数获取摘要统计信息
summary(doubs) 

# 检查是否存在缺失值
if (anyNA(doubs)) {
  # 如果存在缺失值
  #  删除包含缺失值的行
  Doubs_clean <- na.omit(doubs)
  
  # 输出信息
  cat("包含缺失值的行已被删除。\n")
} else {
  # 如果不存在缺失值，直接使用原始数据集
  Doubs_clean <- doubs
  cat("数据集中没有缺失值。\n")
}
# 检查数据集的结构
str(Doubs_clean)


# 检查环境因素之间的共线性
correlation_matrix <- cor(Doubs_clean$env) 

# 输出相关性矩阵
print(correlation_matrix)

#创建一个直观的相关性矩阵可视化图
corrplot(correlation_matrix, method = "circle")

# 查找相关性大于0.8的共线变量
highly_correlated <- findCorrelation(correlation_matrix, cutoff = 0.8)
if (length(highly_correlated) > 0) {
  print("检测到共线变量:")
  print(names(Doubs_clean$env)[highly_correlated])
} else {
  print("未检测到共线变量.")
}


# 绘制环境因素可视化
ggplot(Doubs_clean$env, aes(x = Doubs_clean$xy$x, y = dfs)) +
  geom_point() +
  labs(x = "X 坐标", y = "水深") +
  ggtitle("水深随观测位置的变化")


# 绘制鱼类分布可视化
library(reshape2)
# 获取 fish 数据框
fish_data <- Doubs_clean$fish
# 获取 xy 数据框
xy_data <- Doubs_clean$xy
# 合并 x 和 y 到 fish 数据框中
fish_data <- cbind(fish_data, xy_data)
# 使用 melt 函数将数据框转换为长格式
fish_melted <- melt(fish_data, id.vars = c("x", "y"))
ggplot(fish_melted, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  labs(x = "X 坐标", y = "Y 坐标", fill = "鱼类分布") +
  ggtitle("鱼类分布情况")


#不同鱼类的种类分布
# 对数据进行融合处理
species_melted <- melt(Doubs_clean$species, id.vars = "code")
# 绘制堆叠条形图
p <- ggplot(species_melted, aes(x = variable, y = value, fill = value)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "变量", y = "比例", fill = "鱼类") +
  ggtitle("不同鱼类的种类分布")
# 直接显示图形
p


# 分析鱼类与环境因素之间的关系并可视化
#加载reshape2包
library(reshape2)

#查看 Doubs数据集中鱼类数据框的列名
colnames(Doubs_clean$fish)
#查看 Doubs数据集中环境因素数据框的列名
colnames(Doubs_clean$env)

#确定环境因素和鱼类之间的相关性
fish_env_correlation <- cor(Doubs_clean$fish, Doubs_clean$env)

#环境因素和鱼类之间的相关性关系可视化
ggplot(data = melt(fish_env_correlation), 
       aes(x = Var2, 
           y = Var1, 
           fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", 
                      high = "red") +
  labs(x = "环境因素",  y = "鱼类", 
       title = "环境因素和鱼类之间的相关性") +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))


# 分析flo环境因素与鱼类Alal的关系
ggplot(data = Doubs_clean$env, 
       aes(x = flo, 
           y = Doubs_clean$fish$Alal)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = FALSE) +
  labs(x = "水流量", 
       y= "鱼类Alal", 
       title = "河流水流量和Alal鱼之间的关系")

