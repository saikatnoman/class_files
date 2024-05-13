

getwd()
setwd("E:/R Jaman Sir")
data = read.csv(file.choose())

cleaned_data = data

write.csv(cleaned_data, file = "cleaned_file.csv", row.names = TRUE)

dim(data)
matrix_mat= matrix(c(1:9), nrow=3, ncol=3)

matrix_mat[2:3,3]
matrix_mat[3,1:3]

# 1 number row er shob element
matrix_mat[1,1:3]
matrix_mat[1,]

# 3rd column er shob row
matrix_mat[,3]

# akhon data theke ami row column nilam
subset_data = data [1:100, 4:5]

matrix_matt = matrix(c(1,2,3,4,5,6,7,8,9),nrow=3, ncol=3)
mat_data = data.frame(matrix_matt)
colnames (mat_data) = c("orange","apple","red")


colnames(data)

mat_data$red = NULL

data$X = NULL
data$X.1 = NULL
data$X.2 = NULL
data$X.3 = NULL
data$X.4 = NULL
data$X.5 = NULL
data$X.6 = NULL
data$X.7 = NULL
data$X.8 = NULL
data$X.9 = NULL
data$X.10 = NULL
data$X.11 = NULL
data$X.12 = NULL
data$X.13 = NULL

colnames(data)

data_manual = data.frame(name=c("shibly","arif","saikat"),
                         id=c(100,101,102),
                         salary=c(20000,30000,40000),
                         joining_date=c("April_10,2024","June_4,2023","March_8,2024"))
data_3 = data.frame(id=c("id1","id2","id3","id4"),
                    salary=c("10000","20000","30000","40000"))

data_3$cost = c("70","80","90","100")
data_3$profit = c("70","70","70","70")

data_subset_0 = data_2[,c("cost","profit")]

data_subset_1 = data_2[4,]

profit_margin = c("100%","70%","60%","84%")
data_3$profit_margin = profit_margin

ratio_an = c("40","40.8","60.54","78")

data_4 = cbind(data_3,ratio_an)

new_row = c("id5","60000","90","70","61%","42")
data_5 = rbind(data_4,new_row)


