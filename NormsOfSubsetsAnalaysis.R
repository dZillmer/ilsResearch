library(tidyverse)
df=read.csv(file='C:\\Users\\thomas.mussmann\\OneDrive - West Point\\Desktop\\Master Teacher\\Project\\CleanData.csv')
Student=data.frame(df$StudentAR,df$StudentSI,df$StudentVV,df$StudentSG)
Instructor255=data.frame(df$InstructorAR255,df$InstructorSI255,df$InstructorVV255,df$InstructorSG255)
Instructor153=data.frame(df$InstructorAR153,df$InstructorSI153,df$InstructorVV153,df$InstructorSG153)
df$GradeChangeNotTest <- df$NotTestGrades153-df$NotTestGrades255-mean(df$NotTestGrades153-df$NotTestGrades255)
df$GradeChangeTotal <- df$TotalGrades153-df$TotalGrades255-mean(df$TotalGrades153-df$TotalGrades255)
df$GradeChangeTest <- df$TestGrades153-df$TestGrades255-mean(df$TestGrades153-df$TestGrades255) #this tells us how much their overall grade changed

hist(df$GradeChangeNotTest,breaks = 30,main="Histogram of mean adjusted Not Test Grade change",xlab = "change in non test grade")
hist(df$GradeChangeTest,breaks = 30,main="Histogram of mean adjusted Test Grade change",xlab = "change in test grade")
hist(df$GradeChangeTotal,breaks = 30,main="Histogram of mean adjusted Total Grade change",xlab = "change in total grade")

Norm<-function(x,p){
  #x is the vector p is the pnorm where 0 is for infinity
  x<-abs(x)
  
  if (p==0) {
    Output <- max(x)
  } else
  {
  tempsum <- integer(1)
  for (i in seq(1,length(x))) {tempsum <- tempsum + x[[i]]^p}
  Output<-tempsum^(1/p)
  }
  return(Output)
}

dfNorm<-function(Inst,Stud,NormType,columns){
  #Inst is the instructor data: Stud is student data:NormType is type of norm 0 for infty or p:
  #columns is which columns to use as vector c(1,3,4)
  n <- dim(Inst)[1]
  Output <- integer(n)
  for (i in seq(1,n)) {
  Output[i] = Norm(Inst[i,columns]-Stud[i,columns],NormType)
  }
  return(Output)
}

Combinations=matrix(0,4,15)
Combinations[1,seq(1,4)]=seq(1,4)
Combinations[1,seq(5,10)]=c(1,1,1,2,2,3)
Combinations[2,seq(5,10)]=c(2,3,4,3,4,4)
Combinations[1,seq(11,14)]=seq(1,4)
Combinations[2,seq(11,14)]=c(2,3,4,1)
Combinations[3,seq(11,14)]=c(3,4,1,2)
Combinations[seq(1,4),15]=seq(1,4)

Results=matrix(0,dim(Combinations)[1]+4,dim(Combinations)[2])
Results[seq(1,dim(Combinations)[1]),seq(1,dim(Combinations)[2])]=Combinations

NormModel<-function(target,p,i){
  columns=Combinations[seq(1,4),i]
  Norm153=dfNorm(Instructor153,Student,p,columns)
  Norm255=dfNorm(Instructor255,Student,p,columns)
  Normdiff=Norm153-Norm255
  return(lm(target ~ Normdiff))
}

# NormModelPlot<-function(target,p,i){
#   columns=Combinations[seq(1,4),i]
#   Norm153=dfNorm(Instructor153,Student,p,columns)
#   Norm255=dfNorm(Instructor255,Student,p,columns)
#   Normdiff=Norm153-Norm255
#   tempmodel=lm(target ~ Normdiff)
#   plot(Normdiff,target)
#   abline(tempmodel,col='red')
# }

p=2
for (i in seq(1,15)) {
  tempmodel=NormModel(df$GradeChangeTotal,p,i)
  B=summary(tempmodel)
  Results[5,i]<-B$r.squared
  Results[6,i]<-B$coefficients[2,1]
  F=B$fstatistic
  Results[7,i]<-1-pf(F[1],F[2],F[3])
  Results[8,i]<-p
}

Results

