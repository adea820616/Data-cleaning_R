file = read.csv(file = "Temp.csv" , header = FALSE)
company_code_text = as.matrix(file[1,])
View(company_code_text1)

company_code_text1 = paste(company_code_text , collapse = "-")
company_code_text1 = strsplit(company_code_text1 , split = "-" , fixed = TRUE)
company_code = as.data.frame(table(company_code_text))
View(company_code)
company_name = as.vector(company_code[c(47),1])

Year = as.vector(file[2:28,1])

Year = rep(Year,length(company_name))
length(Year)

company_name_final = vector(mode = "character",length = length(Year))
for(i in 1:length(Year)){
  if((i %% 27 )==1)
    company_name_final[i] = company_name[i/27+1]
  else
    company_name_final[i] = " "
}

output_file = data.frame(company_name_final,Year)
length(file[1,])
#---------
for(i in 2:length(file[1,])){
  if(file[1,i] != "#ERROR"){
    company_variable = as.data.frame(strsplit(file[1,i] , split = "-" , fixed = TRUE))
    output_file[((1+27*(which(company_variable[1,1] == company_name)-1)):(27+27*(which(company_variable[1,1] == company_name)-1))),company_variable[2,1]] = file[2:28,i]
    
  }
}
class(output_file)
write.csv(output_file,"Chemicals.csv")

