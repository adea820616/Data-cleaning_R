library(dplyr)
library(tidyr)
library(ggplot2)

###sales###
sales <- read.csv(file = "sales.csv" , header = FALSE)
sales <- sales[2:nrow(sales),]
colnames(sales) <-c("date","materials","size","kg","id")

sales$size <- as.character(sales$size)
View(sales)

#年月分欄
sales$kg <- as.character(sales$kg)
sales$kg <- as.numeric(sales$kg)

sales%>%
  separate( col = date, into = c("Year","Month","Day"),sep = "/", convert=TRUE) ->sales

#材質-尺寸,重量相加
sales %>% 
  select( Year, Month, materials, size, kg ) %>%
  group_by( Year, Month, materials, size) %>%
  summarise(value = sum(kg)) -> sales_1

sales_1 <- as.data.frame(sales_1)

#年月欄位合併
sales_1 <- unite(sales_1, "date", Year, Month, sep="-")
View(sales_1)



##############produce#####################
produce <- read.csv(file = "produce.csv" , header = FALSE)
produce <- produce[2:nrow(produce),]
colnames(produce) <-c("date","materials","size","kg")

produce$size <- as.character(produce$size)
View(produce)

#年月分欄
produce$kg <- as.character(produce$kg)
produce$kg <- as.numeric(produce$kg)
produce%>%
  separate( col = date, into = c("Year","Month","Day"),sep = "/", convert=TRUE) ->produce

#材質-尺寸,重量相加
produce %>% 
  select( Year, Month, materials, size, kg ) %>%
  group_by( Year, Month, materials, size)  %>%
  summarise(value = sum(kg))-> produce_1

produce_1 <- as.data.frame(produce_1)

#年月欄位合併
produce_1 <- unite(produce_1, "date", Year, Month, sep="-")
View(produce_1)


###############計算庫存################
#合併produce_1 & sales_1

stock_1 <- merge(produce_1,sales_1, by=c("date","materials","size"), all=TRUE)
stock_1[is.na(stock_1)] <- 0
colnames(stock_1) <- c("date","materials","size","value_p","value_s")
stock_1 <- unite(stock_1, "M_Size", materials, size, sep="-")

stock_1$M_Size = gsub("[*]",replacement="_",stock_1$M_Size)
View(stock_1)

#20種 Materials-Size
sub <- unique(stock_1$M_Size)
length(unique(stock_1$M_Size))
sub <- as.matrix(sub)
sub = gsub("[*]",replacement="_",sub)

#算庫存量

stock_1%>%
  separate( col = date, into = c("Year","Month"),sep = "-", convert=TRUE) ->stock_1


for(i in 1:length(sub)){
  
  stock_2 <- filter(stock_1, stock_1$M_Size == sub[1])
  #照日期排序
  stock_2 <- stock_2[order(stock_2$Year, stock_2$Month),]
  stock_2<- unite(stock_2, "date", Year, Month, sep="-")
  
    stock_2$st = 0
    for( j in 1:nrow(stock_2)){
      if (j == 1) {
        stock_2$st[j] = stock_2$value_p[j] - stock_2$value_s[j]
      }else{
        stock_2$st[j] = stock_2$st[j-1] + stock_2$value_p[j] - stock_2$value_s[j]
      }
    }
    
  #graph
    date <- rep(stock_2$date,3)
    M_size <- rep(stock_2$M_Size,3)
    
    value = ""
    type = ""
    stock_3 <- data.frame(date, M_size, value,type)
    stock_3$value = as.numeric(stock_3$value)
    stock_3$type = as.character(stock_3$type)
    for(m in 1:nrow(stock_3)){
      if(m <= nrow(stock_2)){   #1-41
        stock_3$value[m] = stock_2$value_p[m]
        stock_3$type[m] ="produce"
      }else if( m > nrow(stock_2) && m <= (nrow(stock_2)*2) ) { #42-82
        stock_3$value[m] = stock_2$value_s[m - nrow(stock_2)] 
        stock_3$type[m] ="sales"
      }else{
        stock_3$value[m] = stock_2$st[m - (nrow(stock_2)*2)] #83-123
        stock_3$type[m] ="stock"
      }
    }
  #print graphic 
  name = paste(sub[i],".png",sep = "")
  
  png(filename =  name , width=800, height=600)  
  
    print(ggplot(stock_3, aes(date, value))+
      geom_bar(stat = "identity", aes(fill = type), position = "dodge"))
    print(i)
  dev.off()
  
}



##################
ggplot(data = stock_2) + 
  geom_bar(aes(x = date,y = value_p),stat="identity",fill = "red",position = "dodge") +
  geom_bar(aes(x = date,y = value_s),stat="identity",fill = "blue") +
  geom_line(data = stock_2,aes(x = date,y = st,group = 1),colour = "black")
##################  

###2013stock###
#stock <- read.csv(file = "stock.csv" , header = FALSE)