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
  plotly
  )

# load data ----
###############
intel_data_raw <- read.csv(
  "data/Intel_CPUs.csv", 
  na.string = c("", " ","   ","N/A","NA") # and process missing
)
# chọn các cột cần sử dụng
intel_data <- intel_data_raw[,c("Product_Collection","Vertical_Segment","Status","Launch_Date","Lithography",
                          "Recommended_Customer_Price","nb_of_Cores","nb_of_Threads",
                          "Cache","Max_Memory_Size","Max_nb_of_Memory_Channels","Instruction_Set")]
# in ra bảng thống kê sơ bộ của dữ liệu
print(summary(intel_data))

# Xử lý dữ liệu khuyết ----
###############
# KIỂM TRA DỮ LIỆU KHUYẾT
print(apply(is.na(intel_data),2,sum))


intel_data <- intel_data[complete.cases(intel_data$Max_Memory_Size), ]   # drop toàn bộ dòng có N/A của cột này
max_mem_size_clean <- function(size){  
  if(grepl('G',size)){
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
product_collect <- c('Legacy', 'Celeron', 'Pentium', 'Quark', 'Atom', 'Itanium', 'Xeon','Core')
for (i in product_collect) {
  # nhóm dữ liệu thành các loại dòng chip hiện tại
  intel_data$Product_Collection <- ifelse(grepl(i, intel_data$Product_Collection), i, intel_data$Product_Collection)
}

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

##RECOMMENDED CUSTOMER PRICE
##############
recommend_price <- function(price_range) {
  if(grepl('-', price_range)) {
    range <- strsplit(price_range, "-")[[1]]
    return((as.double(range[1]) + as.double(range[2])) / 2)
  }
  return (price_range)
}
# sửa định dạng chuỗi
intel_data$Recommended_Customer_Price <- gsub("\\$", "", intel_data$Recommended_Customer_Price) 
intel_data$Recommended_Customer_Price <- gsub(",", "", intel_data$Recommended_Customer_Price)
# apply hàm để xử lý số liệu
intel_data$Recommended_Customer_Price <- sapply(intel_data$Recommended_Customer_Price, recommend_price) 
intel_data$Recommended_Customer_Price <- as.double(intel_data$Recommended_Customer_Price) 
intel_data$Recommended_Customer_Price <- log(intel_data$Recommended_Customer_Price) 
intel_data <- intel_data %>%
  group_by(Product_Collection) %>%
  # fill theo dữ liệu từng loại chip theo thứ tự là forward rồi tới backward
  fill(Recommended_Customer_Price, .direction = "updown")

##CACHE
##############
Cache_Size_Clean <- function(size){
  if(grepl('K',size)){
    return (as.double(gsub(" K","",size)) /1024)
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




## Kiểm tra lại dữ liệu ----
##############
# check xem còn dữ liệu nào thiếu không 
print(apply(is.na(intel_data),2,sum) )
# kiểm tra lại số liệu và định dạng
print(str(intel_data) )
print(summary(intel_data))
head(intel_data, 10)


## Thống kê tả ----
##############

### LÀM RÕ DỮ LIỆU
##############
# chia dữ liệu thành 2 loại biến : numeric - định lượng và categorical - định tính
numerical_cols = c("Launch_Date","Lithography","Recommended_Customer_Price","nb_of_Cores","nb_of_Threads",
                   "Cache_Size","Max_Memory_Size","Max_nb_of_Memory_Channels","Max_Memory_Bandwidth")
categorical_cols = c("Product_Collection","Vertical_Segment","Status","Cache_Type","Instruction_Set")

# Xây dựng bảng thống kê
summary_numeric_table <- data.frame(
  Staticstic=c("Count", "Mean", "STD", "Min", "First Quantile", "Median", "Third Quantile", "Max")
)
for (i in numerical_cols){
  count <- length(intel_data[[i]])
  mean<- mean(intel_data[[i]])
  std <- sd(intel_data[[i]])
  min <- min(intel_data[[i]])
  first_quantile <- sapply(intel_data[i], function(x) quantile(x, 0.25) )[[1]]
  median <- median(intel_data[[i]])
  third_quantile <- sapply(intel_data[i], function(x) quantile(x, 0.75))[[1]]
  max <- max(intel_data[[i]])
  summary_numeric_table <- cbind(summary_numeric_table,new_col=c(count,mean,std,min,first_quantile,median,third_quantile,max))
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
summary_numeric_table

### Vẽ biểu đồ
##############
histogram_bplot <- function(intel_data, col, na.rm = T) { 
  par(mfrow = c(1,2))
  
  hist(intel_data[[col]],
       main = paste("Histogram of", col),
       xlab = col)
  
  boxplot(intel_data[[col]],
          main = paste("Boxplot of", col),
          horizontal = T,
          xlab = col,
          ylab = "",
          las = 1)}
histogram_bplot(intel_data,"Max_Memory_Size")
histogram_bplot(intel_data,"Lithography")
histogram_bplot(intel_data,"nb_of_Cores")
histogram_bplot(intel_data,"nb_of_Threads")
histogram_bplot(intel_data,"Cache_Size")

## export result ----
###############
export(intel_data_raw,here("data","clean","my_data.rds"))

