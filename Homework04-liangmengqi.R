#
#Author: liangmengqi
#Copyright   Copyright 2024-liangmengqi
#Email:mqliang2023@mail.ustc.edu.cn
#
#Date:2024-04-02
#
#Script Name: Homework04-liangmengqi
#
#Script Description: creating a regression model of mpg as target and others as features (from a built-in dataset of mtcars) using random forest algorithm with caret package
#
#
#SETUP ----------------------------------------------
#前期准备
#安装必要的包
install.packages(c('caret', 'skimr', 'RANN', 'randomForest'))

# 加载caret包
library(caret)

# 导入mtcars数据集
data(mtcars)

# 查看数据框结构
str(mtcars)

# 查看 mtcars 数据集的前 6 行和前 10 列
head(mtcars[, 1:10])


#数据准备与预处理
set.seed(100)
#设置随机种子，以确保结果的可重复性

trainRowNumbers <- createDataPartition (mtcars$mpg, p=0.80, list=FALSE)
#获取用于训练数据的行号

trainData <- mtcars[trainRowNumbers,]
#使用获取的行号创建训练数据集

testData <- mtcars[-trainRowNumbers,]
#使用从未包含在训练数据中的行号创建测试数据集

x = trainData[, 2:11]
y = trainData$mpg
#将训练数据集中的自变量和因变量存储在 x 和 y 中，以备后续使用

#描述性统计
library(skimr)
skimmed <- skim(trainData)
#观察训练数据集中每列的描述性统计量
print(skimmed) #查看结果

#查看训练数据集和测试数据集前几行情况
head(trainData)
head(testData)

#检查训练集是否有缺失值
train_missing <- colSums(is.na(trainData))
train_missing <- train_missing[train_missing > 0]

#检查测试集是否有缺失值
test_missing <- colSums(is.na(testData))
test_missing <- test_missing[test_missing > 0]

# 输出结果
if (length(train_missing) > 0) {
  cat("Missing values in training dataset:\n")
  print(train_missing)
} else {
  cat("No missing values in training dataset.\n")
}

if (length(test_missing) > 0) {
  cat("\nMissing values in test dataset:\n")
  print(test_missing)
} else {
  cat("\nNo missing values in test dataset.\n")
}


#特征选择和可视化
#使用递归特征消除方法选择特征
#定义要在 RFE 中尝试的特征子集的大小
subsets = c(1:10)

#定义RFE算法的控制参数
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)#创建一个控制对象 ctrl，指定了 RFE 的参数,使用交叉验证（cv）方法

#使用 rfe() 函数执行 RFE特征选择算法
lmprofile <- rfe(trainData[,-1], trainData$mpg, subsets, rfeControl = ctrl)#有关特征选择的详细信息储存在lmprofile中
print(lmprofile)
features_selected <- c("disp")#从特征选择的结果中选择了一个top 1的特征 "disp"，并将其存储在features_selected中

#特征可视化
#创建箱线图矩阵
featurePlot(x = as.matrix(trainData[, features_selected]),
            y = as.factor(trainData$mpg),
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))
#创建密度图矩阵
featurePlot(x = as.matrix(trainData[, features_selected]), 
            y = as.factor(trainData$mpg), 
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))


#训练调整模型
#输出并查看在caret包中可用的算法
modelnames <- paste(names(getModelInfo()), collapse=',  ')
modelnames
#加载随机森林包
library(randomForest)
#查找随机森林模型
modelLookup('rf')
#设置随机数种子，以确保结果的可复现性
set.seed(100)

#使用randomForest函数来训练模型,trainData是训练数据集，Purchase是目标变量，disp则是用来预测目标变量的特征
randomForest_model <- randomForest(mpg ~ disp, data = trainData)

#可视化特征重要性
varImpPlot(randomForest_model, main = "Feature Importance with Random Forest")

#保存训练好的随机森林模型
saveRDS(randomForest_model, "randomForest_model.rds")

#输出模型的一些信息
print(randomForest_model)

#绘制随机森林模型的精度图
plot(randomForest_model, main = "Model Accuracies with randomForest")


#评估随机森林模型在测试数据集上的性能表现
#查看trainData与testData的结构
str(trainData)
str(testData)
#利用之前训练好的随机森林模型 randomForest_model 对测试数据集 testData 进行预测，预测结果保存在 predicted 变量中
predicted <- predict(randomForest_model, newdata = testData)

#计算预测结果 predicted 和实际值 testData$mpg 之间的均方根误差（RMSE），它表示模型预测值与实际值之间的差异程度
RMSE <- sqrt(mean((predicted - testData$mpg)^2))
print(paste("RMSE:", RMSE))   

#计算预测结果 predicted 和实际值 testData$mpg 之间的决定系数（R-squared），它表示模型对观测数据方差的解释程度
R_squared <- cor( testData$mpg,predicted)^2
print(paste("R-squared:", R_squared))  

#绘制实际值 testData$mpg 与预测值 predicted 的散点图。其中 main 参数设置图的标题为 "Predicted Results"，xlab 参数设置了 x 轴标签为 "Actual value"，ylab 参数设置了 y 轴标签为 "Predicted value"
plot(testData$mpg, predicted, main = "Predicted Results", xlab = "Actual value", ylab = "Predicted value")
abline(0, 1, col = "pink")  #添加一条斜率为1（1:1关系）的直线，代表完美预测的情况，即预测值等于实际值
