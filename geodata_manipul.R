#
#Author: liangmengqi
#Copyright   Copyright 2024-liangmengqi
#Email:mqliang2023@mail.ustc.edu.cn
#
#Date:2024-04-24
#
#Script Name:Homework-08-liangmengqi
#
#Script Description:1.set 2-km buffer along the Doubs river 
#and clip from the map to extract the raster values of the catchment area and slope for each point with the qgisprocess package.
#2.merge the extracted data with other environmental factors from Doubs dataset to form a dataframe, 
#and finally transfer the dataframe to a sf object, which contains a geometry column.
#
#
#SETUP ----------------------------------------------
#安装必要的包
install.packages("qgisprocess")
install.packages("sf")
install.packages("raster")
install.packages("terra")


# 加载必要的包
library(qgisprocess)
library(sf)
library(terra)
library(raster)
library(ggplot2)

#导入数据
#加载doubs地域的tif文件
doubs_dem <- terra::rast("D:/data-driven-ecology/QGIS/map.tif")

#读取之前已保存的doubs河流线数据
doubs_line <- sf::st_read("D:/data-driven-ecology/QGIS/doubs-line-shp.shp")

#读取之前已保存的doubs河流点数据
doubs_points <- sf::st_read("D:/data-driven-ecology/QGIS/doubs-point-shp.shp")

################################################################################
#沿着Doubs河设置2公里的缓冲区，并从地图中剪辑

# 将河流线数据转换到UTM坐标系
doubs_line_utm <- st_transform(doubs_line, 32631)

# 建立缓冲区
doubs_line_buffer <- st_buffer(doubs_line_utm, dist = 2000)

# 检查空间数据是否有效
if (!is.null(doubs_line_buffer) && !is.null(doubs_dem)) {
  
  # 转换DEM数据到UTM坐标系 (EPSG:32631为UTM Zone 31N)
  utm_crs <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
  doubs_dem_utm <- project(doubs_dem, utm_crs)
  
  # 裁剪 DEM 数据到河流线缓冲区范围内并掩蔽
  doubs_dem_utm_masked <- mask(doubs_dem_utm, doubs_line_buffer)
  
  # 可视化裁剪后的高程数据
  plot(doubs_dem_utm_masked, main = "Masked Elevation Data along Doubs River", axes = TRUE)
  
  # 存储裁剪后的 DEM 数据
  writeRaster(doubs_dem_utm_masked, filename = "doubs_dem_utm_masked.tif")
  
} else {
  # 输出错误信息或者采取其他处理措施
  print("空间数据无效，请检查数据是否完整或者正确")
}


#计算并可视化地形参数坡度的分布情况，对地形数据进行分析

# 计算坡度
slope <- terrain(doubs_dem_utm_masked, "slope", unit = "degrees", neighbors = 8)

# 转换为数据框
slope_df <- as.data.frame(slope, xy = TRUE)

# 绘制直方图
ggplot(slope_df, aes(x = slope)) +
  geom_histogram(bins = 30, fill = "purple", color = "black") +
  labs(x = "Slope", y = "Frequency") +
  theme_minimal()

################################################################################
#提取每个点的集水面积和坡度的光栅值
qgis_configure()
qgis_algorithms()
qgis_show_help("native:buffer")
qgis_search_algorithms("wetness") |>
  dplyr::select(provider_title,algorithm) |>
  head(2)

topo_total = qgisprocess::qgis_run_algorithm(
  alg = "sagang:sagwetnessindex",
  DEM = doubs_dem_utm_masked,
  SLOPE_TYPE = 1,
  SLOPE = tempfile(fileext = ".sdat"),
  AREA = tempfile(fileext = ".sdat"),
  .quiet = TRUE)

topo_select <- topo_total[c("AREA","SLOPE")] |>
  unlist() |>
  rast()

names(topo_select) = c("carea","cslope")
origin(topo_select) = origin(doubs_dem_utm_masked)
topo_char = c(doubs_dem_utm_masked,topo_select)
topo_env <- terra::extract(topo_char,doubs_points_utm,ID = FALSE)

watershed <- area(doubs_dem_utm_masked)
slope <- terrain(doubs_dem_utm_masked, opt = "slope")
writeRaster(watershed, filename = "D:/data-driven-ecology/QGIS/watershed.tif", format = "GTiff", overwrite = TRUE)
writeRaster(slope, filename = "D:/data-driven-ecology/QGIS/slope.tif", format = "GTiff", overwrite = TRUE)
# 保存集水区和坡度数据
watershed_values <- raster::extract(watershed_raster, sample_points[, c("mapX", "mapY")])
slope_values <- raster::extract(slope_raster, sample_points[, c("mapX", "mapY")])

################################################################################
#将提取的数据与来自Doubs数据集中的其他环境因子合并，形成一个数据帧
#最后将数据帧传输到一个包含几何列的sf对象
data("doubs")
env<-doubs$env
doubsxy_utm_data<-read.csv("D:/data-driven-ecology/QGIS/doubs-xy-adjust.png.csv")
doubsxy_utm_Lon<-doubsxy_utm_data[,1]

doubsxy_utm_Lat<-doubsxy_utm_data[,2]

doubs_env <- cbind(Lon=doubsxy_utm_Lon,Lat=doubsxy_utm_Lat,carea = watershed_values, cslope = slope_values)
# 合并数据

# 将数据框传输到包含几何列的sf对象
doubs_env_sf <- st_as_sf(doubs_env, coords = c("geometry.x", "geometry.y"), crs = st_crs(32631))
# 将数据框转换为 sf 对象
print(doubs_env_sf)

doubs_env_df <- st_drop_geometry(doubs_env_sf)
write.csv(doubs_env_df, "D:/data-driven-ecology/QGIS/doubs_env_data.csv", row.names = FALSE)
