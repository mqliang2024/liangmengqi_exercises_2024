#
#Author: liangmengqi
#Copyright   Copyright 2024-liangmengqi
#Email:mqliang2023@mail.ustc.edu.cn
#
#Date:2024-04-10
#
#Script Name: Homework05-liangmengqi
#
#Script Description:
#
#
#SETUP ----------------------------------------------

# 安装与Python交互的R包——reticulate
install.packages("reticulate") 
#加载reticulate包
library(reticulate)
#查看当前 Python 解释器的配置信息
py_config()

#安装miniconda
reticulate::install_miniconda( )

#指定要在 R 中使用的 Python 解释器的路径
reticulate::use_python("C:/Users/lenovo/AppData/Local/r-miniconda/python.exe")
#指定路径后再次查看当前 Python 解释器的配置信息
library(reticulate)
py_config()

#在当前 Python 环境中安装名为 retriever 的 Python 包
reticulate::py_install("retriever")
library(retriever)#在 R 中加载名为 retriever 的 Python 包

#在 R 中安装名为 rdataretriever 的 R 包
install.packages("rdataretriever")
library(rdataretriever)#加载已安装的 rdataretriever 包

rdataretriever::datasets()#列出可用的数据集及其相关信息

rdataretriever::get_updates()#用于检查数据检索器的更新

# 查看可用的内置数据集列表
data_list <- data()

# 打印数据集列表
print(data_list)

#####################################################################
##将数据集上传到SQLite数据库
#####################################################################
# 安装 RSQLite 包
install.packages("RSQLite")

#安装ade4包
install.packages("ade4")

#检查当前环境中是否已经安装了名为 "DBI" 的 R 包
#如果尚未安装，则会通过 install.packages("DBI") 命令安装该包
if (!requireNamespace("DBI", quietly = TRUE)) {
  install.packages("DBI")
}

# 加载必要的包
library(RSQLite)
library(DBI)
library(ade4)
library(dplyr)

# 创建 SQLite 数据库
con <- dbConnect(SQLite(), dbname = "my_database.sqlite")

#设置连接参数
sqlite_db <- "D:/data-driven-ecology/liangmengqi_exercises_2024/my_database.sqlite"

# 加载Doubs数据集
data(doubs)
#检查doubs数据集的结构
str(doubs)

# 创建一个与SQLite数据库的连接
con_1 <- dbConnect(RSQLite::SQLite(), "my_database.sqlite")

# 将 Doubs 数据集转换为数据框
# 使用 bind_rows() 函数将doubs数据集中的env、fish、xy、species数据框按行合并成一个大的数据框
doubs_df <- bind_rows(doubs)

# 查看合并后的数据框
head(doubs_df)

# 查看合并后的数据框结构
str(doubs_df)

# 将合并后的数据框写入SQLite数据库表格
dbWriteTable(con_1, "Doubs", doubs_df, row.names = FALSE)

# 关闭数据库连接
dbDisconnect(con_1)

cat("数据成功上传。\n")

#####################################################################
##将数据集上传到PostgreSQL数据库
#####################################################################

#安装RPostgres包，这个包是用于连接 PostgreSQL 数据库的关键包
install.packages("RPostgres")

#加载RPostgres包
library(RPostgres)

# 建立数据库连接，在此之前已经在电脑上正确配置了 PostgreSQL 数据库
# 并使用超级用户"postgres"登录了 PostgreSQL，使用 CREATE DATABASE 命令创建了"doubs1"数据库
# 然后就可以使用 R 中的代码连接到"doubs1"这个数据库并进行下面的操作
con_2 <- dbConnect(RPostgres::Postgres(),
                   dbname = "doubs1",
                   port = 5432,
                   user = "postgres",
                   password = "654321987")

# 检查连接是否成功
if (dbIsValid(con_2)) {
  print("成功连接到数据库")
} else {
  print("连接失败，请检查数据库配置")
}

# 查看PostgreSQL数据库中的内容
tables <- dbListTables(con_2)
print(tables)

# 将Doubs数据集上传到到PostgreSQL数据库
dbWriteTable(con_2, "Doubs", doubs_df, row.names = FALSE)

# 再次检查PostgreSQL数据库中的内容，确保Doubs数据集已成功上传到到PostgreSQL数据库
tables_updated <- dbListTables(con_2)
print(tables_updated)

# 关闭数据库连接
dbDisconnect(con_2)

cat("数据成功上传。\n")
