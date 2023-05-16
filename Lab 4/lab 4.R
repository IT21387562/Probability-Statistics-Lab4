getwd()
setwd("C:\\Users\\sisil\\Desktop\\SLIIT\\2nd year\\2nd sem\\2nd Sem\\Probability and Statics\\Labs\\Lab 4")
getwd()

data<-read.table("DATA 4.txt" , header = TRUE, sep = "")
fix(data)

#rename columns
names(data)<-c("Team","Attendance","Salary","Years")
data

attach(data)

#boxflot
boxplot(Attendance, main="Boxplot of attendence", ylab="Attendence")

boxplot(Salary, main="Boxplot of salary", ylab="Attendence")

boxplot(Years, main="Boxplot of years", ylab="Attendence")


#histogram
hist(Attendance, main ="Histogram for attendance " , ylab = "frequence" )
abline(h=0)

hist(Salary, main ="Histogram for salary" , ylab = "frequence" )
abline(h=0)

hist(Years, main ="Histogram for years" , ylab = "frequence" )
abline(h=0)

#stem-n-leaf
stem(Attendance)
#mean
mean(Attendance)
#median
median(Attendance)
#standard deviation
sd(Attendance)

#summery all the values
summary(Attendance)

#quantile values
quantile(Attendance)

#quantile values-   Q1
quantile(Attendance)[2]

#IQR
IQR(Attendance)

#function that accept argument Years and give the mode
get.modes<-function(y){
  counts<-table(y)
  names(counts)[counts==max(counts)]
}

get.modes(Years)

#Q4 outliers

get.outlier<-function(z){
  q1<-quantile(z)[2]
  q3<-quantile(z)[4]
  iqr<-q3-q1
  up<- q3+1.5*iqr
  lp<- q1-1.5*iqr
  print(paste("Upper bound",up))
  print(paste("Lower bound",lp))
  print(paste("outliers",paste(sort(z[z<lp | z>up]),collapse = ",")))
}

get.outlier(Years)
