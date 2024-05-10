############################
###   MAIN FILE          ###
############################
# Write a comment after one or more hash symbols

###############
# Purpose: To demonstrate an example
# Authors: Hoa, Kim Phu
###############

# load packages
###############
pacman::p_load(
  rio,       # for import/export of files
  here,      # for locating files in my R project
  tidyverse, # for data management and visualization
  stringr,
  tidyr,
  dplyr,
  zoo,
  Metrics,
  caret,
  MASS,
  ggplot2,
  reshape2,
  mltools,
  DescTools,
  plotly,
  httpgd,
  cowplot
  )


# load data 
###############
intel_data_raw <- read.csv(
  "data/Intel_CPUs.csv", 
  na.strings = c("", " ","   ","N/A","NA") # and process missing
)
# chọn các cột cần sử dụng
intel_data <- intel_data_raw[,c("Product_Collection","Vertical_Segment","Status","Launch_Date","Lithography"
                          ,"nb_of_Cores","nb_of_Threads",
                          "Cache","Max_Memory_Size","Max_nb_of_Memory_Channels","Instruction_Set")]

# in ra bảng thống kê sơ bộ của dữ liệu
print(summary(intel_data))
print(skimr::skim(intel_data))
# XỬ LÝ DỮ LIỆU KHUYỂT
###############
# KIỂM TRA DỮ LIỆU KHUYẾT
print(apply(is.na(intel_data),2,sum))

#dữ liệu toàn mất đồng loạt nên phải được phải bỏ
nrow(intel_data[is.na(intel_data$Max_Memory_Size) & is.na(intel_data$Launch_Date),])

intel_data <- intel_data[complete.cases(intel_data$Max_Memory_Size), ]   # drop toàn bộ dòng có N/A của cột này

max_mem_size_clean <- function(size){  
  if(grepl("G",size)){
    return ( as.double(gsub(" GB","",size)) )
  }
  return ( as.double(gsub(" TB","",size)) * 1024 )
}

# apply hàm để xử lý từng ô dữ liệu: đổi TB về GB và đổi định dạng số 
intel_data$Max_Memory_Size <- sapply(intel_data$Max_Memory_Size,max_mem_size_clean)     

# SẮP XẾP LẠI DATA
##############


## PRODUCT COLLECTION
##############
product_collect <- c("Legacy", "Celeron", "Pentium", "Quark", "Atom", "Itanium", "Xeon","Core")
for (i in product_collect) {
  # nhóm dữ liệu thành các loại dòng chip hiện tại
  intel_data$Product_Collection <- ifelse(grepl(i, intel_data$Product_Collection), i, intel_data$Product_Collection)
}
intel_data$Product_Collection <- factor(intel_data$Product_Collection, levels = product_collect)

##LAUNCH DATE
##############
intel_data <- intel_data[complete.cases(intel_data$Launch_Date), ]                   
intel_data$Launch_Date <- substr(intel_data$Launch_Date,nchar(intel_data$Launch_Date)-1,nchar(intel_data$Launch_Date)) 
intel_data$Launch_Date <- as.integer(intel_data$Launch_Date)
# biến đổi về năm
intel_data$Launch_Date <- ifelse(intel_data$Launch_Date>22,1900+intel_data$Launch_Date,2000+intel_data$Launch_Date) 
# sắp xếp lại dataframe theo ưu tiên sau: năm, loại CPU, loại phân khúc
intel_data <- intel_data[order(intel_data$Launch_Date,intel_data$Product_Collection,intel_data$Vertical_Segment), ]

##LITHOGRAPHY
##############
intel_data$Lithography<- na.locf(intel_data$Lithography) # fill theo forrward (theo năm trước đó)
intel_data$Lithography <-as.double( gsub(" nm$", "", intel_data$Lithography)) # chuyển định dạng thành số thực

##NUMBER OF THREAD
##############
# 1 core thường sẽ có 2 luồng
intel_data$nb_of_Threads <- ifelse(is.na(intel_data$nb_of_Threads), intel_data$nb_of_Cores * 2, intel_data$nb_of_Threads)


##CACHE
##############
Cache_Size_Clean <- function(size){
  if(grepl("K", size)){
    return (as.double(gsub(" K", "", size)) /1024)
  }
  else{
    return (as.double(gsub(" M","",size)))
  }
}



# tách dữ liệu thành 2 cột gồm loại cache và size của nó
intel_data <- separate(intel_data,Cache,into = c("Cache_Size","Cache_Type"),sep="B") 
# xử lý chuỗi và đưa về kiểu số thực
intel_data$Cache_Size <- sapply(intel_data$Cache_Size,Cache_Size_Clean) 
intel_data$Cache_Size <- log(intel_data$Cache_Size)
# vì lệnh separate nên loại thường không được thêm vào
intel_data$Cache_Type <- ifelse(intel_data$Cache_Type == "", "Normal", sub(" ","",intel_data$Cache_Type))



##INSTRUCTION SET
##############
intel_data$Instruction_Set <- na.fill(intel_data$Instruction_Set,"64-bit")   # 64-bit là mode value của các loại máy nên ta fill bằng mode
intel_data$Instruction_Set <- factor(intel_data$Instruction_Set, levels = unique(intel_data$Instruction_Set))

## VERTICAL SEGMENT
##############
## Ta thấy không có dữ liệu NaN nên ta không cần xử lý NaN chỉ cần xử lý chuỗi thành factor
intel_data$Vertical_Segment <- factor(intel_data$Vertical_Segment, levels = unique(intel_data$Vertical_Segment))

## STATUS
##############
## Ta thấy không có dữ liệu NaN nên ta không cần xử lý NaN chỉ cần xử lý chuỗi thành factor
intel_data$Status <- factor(intel_data$Status, levels = unique(intel_data$Status))
##
##############

##KIỂM TRA LẠI DỮ LIỆU
##############
# check xem còn dữ liệu nào thiếu không 
print(apply(is.na(intel_data), 2, sum))
print(skimr::skim(intel_data))

# kiểm tra lại số liệu và định dạng
print(str(intel_data))
##LÀM RÕ DỮ LIỆU
##############
# Các cột dữ liệu số
numerical_cols = c("Launch_Date","Lithography","nb_of_Cores","nb_of_Threads",
                   "Cache_Size","Max_Memory_Size","Max_nb_of_Memory_Channels")
# Các cột dữ liệu phân loại
categorical_cols = c("Product_Collection","Vertical_Segment","Status","Cache_Type","Instruction_Set")
# Xây dựng bảng thống kê mô tả
summary_numeric_table <- data.frame(
  Staticstic=c("Count", "Mean", "STD", "Min", "Median", "Max")
)
for (i in numerical_cols){
  count <- length(intel_data[[i]])
  mean<- mean(intel_data[[i]])
  std <- sd(intel_data[[i]])
  min <- min(intel_data[[i]])
  median <- median(intel_data[[i]])
  max <- max(intel_data[[i]])
  summary_numeric_table <- cbind(summary_numeric_table,new_col=c(count,mean,std,min,median,max))
}

colnames(summary_numeric_table) <- c("",numerical_cols)

summary_categorical_table <- data.frame(
  Staticstic = c("Count","Unique","Mode","Freq")
)
for (i in categorical_cols) {
  count <- length(intel_data[[i]])
  unique <- length( unique(intel_data[[i]]))
  mode <- Mode(intel_data[[i]])
  freq <- attr(mode,"freq")
  summary_categorical_table <- cbind(summary_categorical_table,new_col=c(count,unique,mode,freq))
}
colnames(summary_categorical_table) <- c("",categorical_cols)

print(summary_numeric_table)
print(summary_categorical_table)

# VẼ ĐỒ THỊ MÔ TẢ
##############
## VẼ ĐỒ THỊ MÔ TẢ CHO DỮ LIỆU SỐ
### Cache Size
nf <- layout( matrix(c(1,2), ncol=2) )
hist(intel_data$Cache_Size, main = "Cache Size Distribution", xlab = "Cache Size", breaks = 20)
boxplot(intel_data$Cache_Size, main = "Cache Size Distribution", xlab = "Cache Size")

### Lithography
hist(intel_data$Lithography, main = "Lithography Distribution", xlab = "Lithography", breaks = 20)
boxplot(intel_data$Lithography, main = "Lithography Distribution", xlab = "Lithography")

### Max Memory Size
hist(intel_data$Max_Memory_Size, main = "Max Memory Size Distribution", xlab = "Max Memory Size", breaks = 20)
boxplot(intel_data$Max_Memory_Size, main = "Max Memory Size Distribution", xlab = "Max Memory Size")

### Number of Cores
hist(intel_data$nb_of_Cores, main = "Number of Cores Distribution", xlab = "Number of Cores", breaks = 20)
boxplot(intel_data$nb_of_Cores, main = "Number of Cores Distribution", xlab = "Number of Cores")


# THỐNG KÊ SUY DIỄN
##############
## Kiểm định giả thuyết
anova <- aov(Launch_Date ~ ., data = intel_data)
anova_summary <- summary(anova)
anova_summary



## Mô hình hồi quy
index <- createDataPartition(intel_data$Launch_Date, p = 0.8, list = FALSE)
train_data <- intel_data[index,]
test_data <- intel_data[-index,]

lm_model <- lm(Launch_Date ~ Product_Collection + Vertical_Segment + Lithography + nb_of_Cores + Instruction_Set + Cache_Size, data = train_data)
lm_summary <- summary(lm_model)
y_train_pred <- predict(lm_model,newdata=train_data,response = "Lauch_Date")
y_test_pred <- predict(lm_model, newdata=test_data,response = "Lauch_Date")
# Đánh giá mô hình
mse_train<- mse(y_train_pred,train_data$Launch_Date)
mse_test<- mse(y_test_pred,test_data$Launch_Date)
mae_train <- mae(y_train_pred,train_data$Launch_Date)
mae_test <- mae(y_test_pred,test_data$Launch_Date)
metric <- data.frame(
  variable = c("MSE","MSE","MAE","MAE"),
  value = c(mse_train,mse_test, mae_train, mae_test),
  type = c("train","test","train","test")
)
ggplot(metric,aes(x = variable, y = value,color = type,group=type)) +
  geom_line() +
  geom_point(size=4) +
  labs(x = "", y = "Value", color = "Type")
# export result
###############
# export(intel_data_raw, here("data", "clean", "my_data.rds"))
