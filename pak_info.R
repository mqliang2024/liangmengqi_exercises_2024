#
#Author: liangmengqi
#Copyright   Copyright 2024-liangmengqi
#Email:mqliang2023@mail.ustc.edu.cn
#
#Date:2024-03-13
#
#Script Name:Homework02-tidyverse package
#
#Script Description:install tidyverse package and  access information about the tidyverse package
#
#
#SETUP ----------------------------------------------
install.packages("tidyverse")
#download and install the tidyverse package
library(tidyverse)
#load the tidyverse package
packageDescription("tidyverse")
#access information about the tidyverse package
help(package="tidyverse")
#get help document