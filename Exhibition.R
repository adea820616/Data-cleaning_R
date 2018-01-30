data <- read.csv(file = "exhibiiont.csv" , header = FALSE)
#data <- as.matrix(data[ 2:11,4])
data <- as.matrix(data)

#replace
#刪除()值
subject = gsub("\\(.*?\\)", replacement="", data)

subject = gsub(",", replacement="-", subject)
subject = gsub("/", replacement="-", subject)
subject = gsub(";", replacement="-", subject)
subject = gsub("&", replacement="-", subject)
View(subject)

#設置collapse參數，連成一個字符串
subj = paste(subject , collapse = "-")

#把兩個以上"--"都換成一個"-"
subj = gsub("-*-", replacement="-",subj)
subj = strsplit(subj, split ="-",fixed = TRUE)
View(subj)

#caculate top 10
frequency = as.data.frame(subj)

library(plyr)
frequency = count(frequency)
frequency = frequency[order(frequency$freq, decreasing=T),]
View(frequency)

top_10 = head(frequency, 10)
colnames(top_10) <- c("industry","freq")
rownames(top_10) <- 1:10
View(top_10)
