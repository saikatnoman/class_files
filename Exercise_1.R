

#Question_1
x=1:10
y=2:11

a= 0.5
b = -8

z=exp(cos(x)+sin(y))/a+b

 #Question 2

matrix_mat = matrix(c(1:30), nrow = 5, ncol = 6)

#(a)
matrix_mat[5,]

#(b)
matrix_mat[5,4]

#(c)
matrix_mat[1:4,2:5]

#(d)
matrix_mat[,4]


#Question 3

mat_data = data.frame(matrix_mat)

colnames(mat_data) = c("apple","orange","red","pineapple","blue","yellow")


#Question 4

student_data = data.frame(ID=c(100,200,300,400,500,600,700,800,900,1000),
                          Name = c("A","B","C","D","E","F","G","H","I","J"),
                          Age = c(18,19,20,21,22,23,24,25,26,27),
                          Subject=c("BBA","MBA","CSE","ETE","PHI","BBA","MBA","CSE","ETE","PHI"),
                          Department =c("business","Science","History","business","Science","History","business","Science","History","K jane"))

#Question 5

#(a)

campus = c("urban","urban","urban","urban","rural","urban","urban","urban","urban","rural")

new_student_data = cbind(student_data,campus)

#(b)

first_student = c(1200,"I",29,"MKT","Marketing","Urban")
second_student = c(1100,"I",24,"MKT","Marketing","Rural")

final_data=rbind(new_student_data,first_student, second_student)





