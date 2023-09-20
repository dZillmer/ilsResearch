### Learning Style Analysis Script
### MAJ Devon Zillmer
### 17 MAY 22, for MTP Research Project

path <- "C:\\Users\\devon.zillmer\\OneDrive - West Point\\Documents\\mtpMasterTeacherProgram\\research\\data\\CleanData2.csv"
#path <- "C:\\Users\\Devon\\OneDrive - West Point\\Documents\\mtpMasterTeacherProgram\\research\\data\\CleanData2.csv"
df <- read.csv(path)
require(ggplot2)

### 0. Preliminary data exploration
#Plots to show us that, in general, crude instructor difference from cadet didn't impact directly
plot(df$InstructorDiffQ153,df$TotalGrades153,main="Plot of MA153 grade \n as a function of instructor crude distance.",
     ylab = "Raw MA153 Score",xlab = "Instructor-Student Crude Learning Style Difference")
crude153lm <- lm(TotalGrades153~InstructorDiffQ153,data = df) #returns basically noise
abline(crude153lm,col="green") #note -- very flat line
#summary(crude153lm)

plot(df$InstructorDiffQ255,df$TotalGrades255,main="Plot of MA255 grade \n as a function of instructor crude distance.",
     ylab = "Raw MA255 Score",xlab = "Instructor-Student Crude Learning Style Difference")
crude255lm <- lm(TotalGrades255~InstructorDiffQ255,data = df) #also less than 1% effect
abline(crude255lm,col="blue") #note -- less flat line!
#summary(crude255lm) #this returns a .05-level significance for crude distance! But low r-squared

#this normalizes the total grades
df$StdGrade153 <- (df$TotalGrades153-mean(df$TotalGrades153))
df$StdGrade255 <- (df$TotalGrades255-mean(df$TotalGrades255))
#we'll generally use this standardized grade for all subsequent analysis
#that way we account for "harder" vs. "easier" classes between the two

df$gradeChangeRaw <- df$TotalGrades153-df$TotalGrades255



### 1. Total grade and crude instructor-distance
df$InstrChange <- df$InstructorDiffQ153-df$InstructorDiffQ255 #this tells us how much the crude instructor-distance changed
#we note for the above, if a negative number -> the new teacher is more different
df$GradeChange <- df$StdGrade153-df$StdGrade255 #this tells us how much their overall grade changed
crudeChangelm <- lm(GradeChange ~ InstrChange,data=df)
plot(df$InstrChange,df$GradeChange,main="Plot of Grade Change as a function of \n crude instructor closeness change",
     ylab = "Normalized Grade Change from 153 to 255", xlab = "Crude Instructor Change from 153 to 255 \n (negative means first instructor was more similar)")
abline(crudeChangelm,col="red")
#summary(crudeChangelm) #this returns a .01 significance -- meaning it seems pretty likely! But very low R-squared



### 2. Test and non-test grades
df$GradeChangeTest <- df$TestGrades153-df$TestGrades255-mean(df$TestGrades153-df$TestGrades255) #this tells us how much their overall grade changed
crudeChange2lm <- lm(GradeChangeTest ~ InstrChange,data=df)
plot(df$InstrChange,df$GradeChangeTest,main="Plot of Test Grade Change as a function of \n crude instructor closeness change",
     ylab = "Test Grade Change from 153 to 255", xlab = "Crude Instructor Change from 153 to 255 \n (negative means more similar)")
abline(crudeChange2lm,col="green")
#summary(crudeChange2lm)

df$GradeChangeNonTest <- df$NotTestGrades153-df$NotTestGrades255-mean(df$NotTestGrades153-df$NotTestGrades255) #this tells us how much their overall grade changed
crudeChange3lm <- lm(GradeChangeNonTest ~ InstrChange,data=df)
plot(df$InstrChange,df$GradeChangeNonTest,main="Plot of Non-Test Grade Change as a function of \n crude instructor closeness change",
     ylab = "Non-Test Grade Change from 153 to 255", xlab = "Crude Instructor Change from 153 to 255 \n (negative means more similar)")
abline(crudeChange3lm,col="blue")
#summary(crudeChange3lm)



### 3. 1-norm exploration
df$oneNorm153 <- abs(df$StudentAR-df$InstructorAR153) + abs(df$StudentSG-df$InstructorSG153) + abs(df$StudentSI-df$InstructorSI153) + abs(df$StudentVV-df$InstructorVV153)
on153lm <- lm(StdGrade153~oneNorm153,data = df)
plot(df$oneNorm153,df$StdGrade153,main="Plot of MA153 grade \n as a function of 1-norm instructor distance.",
     ylab = "Score Distance from Average", xlab = "Instructor-Student One-Norm Distance")
abline(on153lm,col="green")
#summary(on153lm)

df$oneNorm255 <- abs(df$StudentAR-df$InstructorAR255) + abs(df$StudentSG-df$InstructorSG255) + abs(df$StudentSI-df$InstructorSI255) + abs(df$StudentVV-df$InstructorVV255)
on255lm <- lm(StdGrade255~oneNorm255,data = df)
plot(df$oneNorm255,df$StdGrade255,main="Plot of MA255 grade \n as a function of 1-norm instructor distance.",
     ylab = "Score Distance from Average", xlab = "Instructor-Student One-Norm Distance")
abline(on255lm,col="blue")
#summary(on255lm)

#below examines how frequently increased one-norm returned decreased grade (and vice versa)
df$oneNormChange <- df$oneNorm153-df$oneNorm255
lmOneNorm <- lm(GradeChange ~ oneNormChange,data=df) #very small coefficient with standardized grades
plot(df$oneNormChange,df$GradeChange,main="Plot of standardized grade change \n as a function of One-Norm Distance",
     ylab = "Score Change", xlab = "Instructor-Student One-Norm Distance Change Across Semesters \n (Negative values mean first instructor \"more similar\" to student)")
abline(lmOneNorm,col='red')
#summary(lmOneNorm)

#This compares one-norm discrepancies between "test" and "non-test" data for the 1-norm data
oneChange2lm <- lm(GradeChangeTest ~ oneNormChange,data=df)
plot(df$oneNormChange,df$GradeChangeTest,main="Plot of Test Grade Change as a function of \n instructor-student one-norm change",
     ylab = "Test Grade Change from 153 to 255", xlab = "1-Norm Instructor Change from 153 to 255 \n (Negative values mean second instructor \"less similar\" to student)")
abline(oneChange2lm,col="green")
#summary(oneChange2lm)

oneChange3lm <- lm(GradeChangeNonTest ~ oneNormChange,data=df)
plot(df$oneNormChange,df$GradeChangeNonTest,main="Plot of Non-Test Grade Change as a function of \n instructor-student one-norm change",
     ylab = "Non-Test Grade Change from 153 to 255", xlab = "1-Norm Instructor Change from 153 to 255 \n (negative means more similar)")
abline(oneChange3lm,col="blue")
summary(oneChange3lm)



### 4. Two-norm Considerations
df$twoNorm153 <- sqrt((df$StudentAR-df$InstructorAR153)^2 + (df$StudentSG-df$InstructorSG153)^2 + (df$StudentSI-df$InstructorSI153)^2 + (df$StudentVV-df$InstructorVV153)^2)
tn153lm <- lm(StdGrade153~oneNorm153,data = df)
plot(df$twoNorm153,df$StdGrade153,main="Plot of MA153 grade \n as a function of 2-norm instructor distance.",
     ylab = "Score Distance from Average", xlab = "Instructor-Student Two-Norm Distance")
abline(tn153lm,col="green")
summary(tn153lm)

df$twoNorm255 <- sqrt((df$StudentAR-df$InstructorAR255)^2 + (df$StudentSG-df$InstructorSG255)^2 + (df$StudentSI-df$InstructorSI255)^2 + (df$StudentVV-df$InstructorVV255)^2)
tn255lm <- lm(StdGrade255~twoNorm255,data = df)
plot(df$twoNorm255,df$StdGrade255,main="Plot of MA255 grade \n as a function of 2-norm instructor distance.",
     ylab = "Standardized Score Distance from Average", xlab = "Instructor-Student Two-Norm Distance")
abline(tn255lm,col="blue")
#summary(tn255lm)

#below examines how frequently increased two-norm returned decreased grade (and vice versa)
df$twoNormChange <- df$twoNorm153-df$twoNorm255
lmTwoNorm <- lm(GradeChange ~ twoNormChange,data=df) #very small coefficient with standardized grades
plot(df$twoNormChange,df$GradeChange,main="Plot of standardized grade change \n as a function of Two-Norm Distance",
     ylab = "Standardized Score Change", xlab = "Instructor-Student Two-Norm Distance Change Across Semesters \n (Negative values mean first instructor \"more similar\" to student)")
abline(lmTwoNorm,col='red')
#summary(lmTwoNorm)


  


#This compares one-norm discrepancies between "test" and "non-test" data for the 2-norm data
twoChange2lm <- lm(GradeChangeTest ~ twoNormChange,data=df)
plot(df$twoNormChange,df$GradeChangeTest,main="Plot of Test Grade Change as a function of \n instructor-student two-norm change",
     ylab = "Test Grade Change from 153 to 255", xlab = "2-Norm Instructor Change from 153 to 255 \n (Negative values mean second instructor \"less similar\" to student)")
abline(twoChange2lm,col="green")
#summary(twoChange2lm)

twoChange3lm <- lm(GradeChangeNonTest ~ twoNormChange,data=df)
plot(df$twoNormChange,df$GradeChangeNonTest,main="Plot of Non-Test Grade Change as a function of \n instructor-student two-norm change",
     ylab = "Non-Test Grade Change from 153 to 255", xlab = "2-Norm Instructor Change from 153 to 255 \n (negative means more similar)")
abline(twoChange3lm,col="blue")
#summary(twoChange3lm)



### 5. Infty-norm Considerations
df$INorm153 <- pmax(abs(df$StudentAR-df$InstructorAR153),abs(df$StudentSG-df$InstructorSG153),abs(df$StudentSI-df$InstructorSI153),abs(df$StudentVV-df$InstructorVV153))
in153lm <- lm(StdGrade153~INorm153,data = df)
plot(df$INorm153,df$StdGrade153,main="Plot of MA153 grade \n as a function of Infty-norm instructor distance.",
     ylab = "Score Distance from Average", xlab = "Instructor-Student Infty-Norm Distance")
abline(in153lm,col="green")
#summary(in153lm)

df$INorm255 <- pmax(abs(df$StudentAR-df$InstructorAR255),abs(df$StudentSG-df$InstructorSG255),abs(df$StudentSI-df$InstructorSI255),abs(df$StudentVV-df$InstructorVV255))
in255lm <- lm(StdGrade255~INorm255,data = df)
plot(df$INorm255,df$StdGrade255,main="Plot of MA255 grade \n as a function of Infty-norm instructor distance.",
     ylab = "Score Distance from Average", xlab = "Instructor-Student Infty-Norm Distance")
abline(in255lm,col="blue")
#summary(in255lm)

#below examines how frequently increased infty-norm returned decreased grade (and vice versa)
df$INormChange <- df$INorm153-df$INorm255
lmINorm <- lm(GradeChange ~ INormChange,data=df) #very small coefficient with standardized grades
plot(df$INormChange,df$GradeChange,main="Plot of grade change \n as a function of Infty-Norm Distance",
     ylab = "Score Change", xlab = "Instructor-Student Infty-Norm Distance Change Across Semesters \n (Negative values mean second instructor \"more different\" from student)")
abline(lmINorm,col='red')
#summary(lmINorm)

#This compares one-norm discrepancies between "test" and "non-test" data for the I-norm data
IChange2lm <- lm(GradeChangeTest ~ INormChange,data=df)
plot(df$INormChange,df$GradeChangeTest,main="Plot of Test Grade Change as a function of \n instructor-student infty-norm change",
     ylab = "Test Grade Change from 153 to 255", xlab = "Infty-Norm Instructor Change from 153 to 255 \n (Negative values mean second instructor \"less similar\" to student)")
abline(IChange2lm,col="green")
#summary(twoChange2lm)

IChange3lm <- lm(GradeChangeNonTest ~ INormChange,data=df)
plot(df$INormChange,df$GradeChangeNonTest,main="Plot of Non-Test Grade Change as a function of \n instructor-student infty-norm change",
     ylab = "Non-Test Grade Change from 153 to 255 \n(positive means 153 grade was higher)", xlab = "Infty-Norm Instructor Change from 153 to 255 \n (positive means first instructor more similar)")
abline(IChange3lm,col="blue")
#summary(IChange3lm)



### 6. Hypothesis Testing ("Initial")
# Null Hypothesis: no correlation. This means the probability of improving or
#getting worse as a function of closeness of instructor should be completely random. 
# Hence, we would expect 25% of the population if all "four quadrants" of the
# plots previously. Hence,
#h0 <- c(.25,.25,.25,.25) #I don't actually need this!

countEm <- function(teacherDelta,gradeDelta,inclZeros=FALSE){
        #purpose: return "binned" responses for analysis in chi-sq testing, 2x2 bins
        #inputs: vector of teacher changes in learning styles (your norm), change in grades,inclZeros=FALSE if you want me to throw 0's away
        #outputs: vector of count on items in each quadrant
        tempDf <- data.frame(teacherDelta,gradeDelta)
        q1 <- dim(tempDf[teacherDelta>0&gradeDelta>0,])[1] #upper-right quadrant
        q2 <- dim(tempDf[teacherDelta<0&gradeDelta>0,])[1] #upper-left quadrant
        q3 <- dim(tempDf[teacherDelta<0&gradeDelta<0,])[1] #lower-left quadrant
        q4 <- dim(tempDf[teacherDelta>0&gradeDelta<0,])[1] #lower-right quadrant
        if(inclZeros==TRUE) {
                q1 <- q1 + floor(.5*(dim(tempDf[teacherDelta==0&gradeDelta>0,])[1])) + floor(.5*(dim(tempDf[teacherDelta>0&gradeDelta==0,])[1]))
                q2 <- q2 + floor(.5*(dim(tempDf[teacherDelta==0&gradeDelta>0,])[1])) + floor(.5*(dim(tempDf[teacherDelta<0&gradeDelta==0,])[1]))
                q3 <- q3 + floor(.5*(dim(tempDf[teacherDelta<0&gradeDelta==0,])[1])) + floor(.5*(dim(tempDf[teacherDelta==0&gradeDelta<0,])[1]))
                q4 <- q4 + floor(.5*(dim(tempDf[teacherDelta==0&gradeDelta<0,])[1])) + floor(.5*(dim(tempDf[teacherDelta>0&gradeDelta==0,])[1]))
        }
        ans <- c(q1,q2,q3,q4)
        return(ans)
}

#We can now examine each of the respective vectors of norms of distances...
h1 <- countEm(df$InstrChange,df$GradeChange) #we note we get a "worse" p-value if we include the zeros... but still significant
chisq.test(h1)
h2 <- countEm(df$oneNormChange,df$GradeChange)
chisq.test(h2) 
h3 <- countEm(df$twoNormChange,df$GradeChange)
chisq.test(h3)
h4 <- countEm(df$INormChange,df$GradeChange)
chisq.test(h4)



### 7. Further Hypothesis Testing ("More nuances")
#so what if we think a little more carefully, and say that in general, students...
# do generally the same -- you have to show meaningful change? 
countEmV2 <- function(teacherDelta,gradeDelta,inclZeros=FALSE){
        #purpose: return more "binned" responses, but this time 
        #inputs: vector of teacher changes in learning styles (your norm), change in grades,inclZeros=FALSE if you want me to throw 0's away
        #outputs: vector of count on items in each quadrant
        tempDf <- data.frame(teacherDelta,gradeDelta)
        gradeMin <- .03
        q1 <- dim(tempDf[teacherDelta>0&gradeDelta> gradeMin,])[1] #upper-right quadrant
        q2 <- dim(tempDf[teacherDelta<0&gradeDelta> gradeMin,])[1] #upper-left quadrant
        q3 <- dim(tempDf[teacherDelta<0&gradeDelta< -gradeMin,])[1] #lower-left quadrant
        q4 <- dim(tempDf[teacherDelta>0&gradeDelta< -gradeMin,])[1] #lower-right quadrant
        q5 <- dim(tempDf[gradeDelta<gradeMin&gradeDelta> -gradeMin&teacherDelta!=0,])[1] #grades that are basically zero
        if(inclZeros==TRUE) {
                q1 <- q1 + floor(.5*(dim(tempDf[teacherDelta==0&gradeDelta> gradeMin,])[1])) + floor(.5*(dim(tempDf[teacherDelta>0&gradeDelta== gradeMin,])[1]))
                q2 <- q2 + floor(.5*(dim(tempDf[teacherDelta==0&gradeDelta> gradeMin,])[1])) + floor(.5*(dim(tempDf[teacherDelta<0&gradeDelta== gradeMin,])[1]))
                q3 <- q3 + floor(.5*(dim(tempDf[teacherDelta<0&gradeDelta== -gradeMin,])[1])) + floor(.5*(dim(tempDf[teacherDelta==0&gradeDelta< -gradeMin,])[1]))
                q4 <- q4 + floor(.5*(dim(tempDf[teacherDelta==0&gradeDelta< -gradeMin,])[1])) + floor(.5*(dim(tempDf[teacherDelta>0&gradeDelta== -gradeMin,])[1]))
        }
        ans <- c(q1,q2,q3,q4,q5)
        return(ans)
}

#so if we assume students get roughly "the same" grade across semesters ...
#... using the standard deviation from the 
s<-sd(df$GradeChange)
m<-mean(df$GradeChange)
cutOff <- .03
estQ5 <- pnorm(cutOff,mean=m,sd=s)-pnorm(-cutOff,mean=m,sd=s) #this captures the "no real effect" zone
estQ4 <- pnorm(-cutOff,mean=m,sd=s)/2 #if we estimate the SD based on all of our grades...
estQ3 <- pnorm(-cutOff,mean=m,sd=s)/2 #... and divide by two because of our two possible options
estQ2 <- pnorm(cutOff,mean=m,sd=s,lower.tail=FALSE)/2 #same story for these two estimates
estQ1 <- pnorm(cutOff,mean=m,sd=s,lower.tail=FALSE)/2
estP <- c(estQ1,estQ2,estQ3,estQ4,estQ5) #collecting for us in chi-sq test

#now we can test...
h1 <- countEmV2(df$InstrChange,df$GradeChange)
chisq.test(h1,p=estP)
h2 <- countEmV2(df$oneNormChange,df$GradeChange)
chisq.test(h2,p=estP) 
h3 <- countEmV2(df$twoNormChange,df$GradeChange)
chisq.test(h3,p=estP)
h4 <- countEmV2(df$INormChange,df$GradeChange)
chisq.test(h4,p=estP)

#now some plots to support what I'm trying to say...
x <- df$GradeChange
x2 <- seq(min(x), max(x), length = 40)
hist(x, probability = TRUE,main="Histogram of Grade Changes \n with overlaid Normal using Data",
     xlab="Test Score Change across Semesters")
fun <- dnorm(x2, mean = mean(x), sd = sd(x))
lines(x2,fun,col="red",lwd=2)
fun2 <- dt(x2,df=10)
lines(x2,fun2,col="blue",lwd=2)

#this plots the ecdf and the cdf for comparison
plot(ecdf(x),main="ECDF of Grade Changes \n with overlaid Normal")
lines(x2,pnorm(x2,mean=mean(x),sd=sd(x)),col="red")
lines(x2,pt(x2,df=100),col="blue")



#this generates our first two plots; simply remove the annotates to get the very first plot
ggplot(df,aes(twoNormChange,GradeChange))+
  geom_point()+
  labs(x="Two-Norm Change \n (Negative Values Indicate Second Instructor 'Less Similar' to Student)",
       y="Grade Change \n (Negative Values Indicate Higher Performance Second Semester",
       title="Plot of Standardized Grade Change \n as a Function of Two-Norm Distance")+
  theme(plot.title=element_text(hjust=.5))+
  annotate("rect",xmin=min(df$twoNormChange),xmax=0,ymin=min(df$GradeChange),ymax=0,alpha=.3,fill="red")+
  annotate("rect",xmin=0,xmax=max(df$twoNormChange),ymin=0,ymax=max(df$GradeChange),alpha=.3,fill="red")+
  annotate("rect",xmin=0,xmax=max(df$twoNormChange),ymin=min(df$GradeChange),ymax=0,alpha=.3,fill="green")+
  annotate("rect",xmin=min(df$twoNormChange),xmax=0,ymin=0,ymax=max(df$GradeChange),alpha=.3,fill="green")+
  annotate("text",x=.5*max(df$twoNormChange),y=.1,label="Negative Effect \n Expect 25% of Population",colour="black")+
  annotate("text",x=.5*min(df$twoNormChange),y=.1,label="Positive Effect \n Expect 25% of Population",colour="black")+
  annotate("text",x=.5*max(df$twoNormChange),y=-.1,label="Positive Effect \n Expect 25% of Population",colour="black")+
  annotate("text",x=.5*min(df$twoNormChange),y=-.1,label="Negative Effect \n Expect 25% of Population",colour="black")

#this generates the third plot
ggplot(df,aes(twoNormChange,GradeChange))+
  geom_point()+
  labs(x="Two-Norm Change \n (Negative Values Indicate Second Instructor 'Less Similar' to Student)",
       y="Grade Change \n (Negative Values Indicate Higher Performance Second Semester",
       title="Plot of Standardized Grade Change \n Overlaid Disbributions by Quadrant")+
  theme(plot.title=element_text(hjust=.5))+
  annotate("rect",xmin=min(df$twoNormChange),xmax=0,ymin=min(df$GradeChange),ymax=0,alpha=.2,fill="red")+
  annotate("rect",xmin=0,xmax=max(df$twoNormChange),ymin=0,ymax=max(df$GradeChange),alpha=.2,fill="red")+
  annotate("rect",xmin=0,xmax=max(df$twoNormChange),ymin=min(df$GradeChange),ymax=0,alpha=.2,fill="green")+
  annotate("rect",xmin=min(df$twoNormChange),xmax=0,ymin=0,ymax=max(df$GradeChange),alpha=.2,fill="green")+
  annotate("text",x=.5*max(df$twoNormChange),y=.1,label="Negative Effect \n Approx. 19% of Population",colour="black")+
  annotate("text",x=.5*min(df$twoNormChange),y=.1,label="Positive Effect \n Approx. 25% of Population",colour="black")+
  annotate("text",x=.5*max(df$twoNormChange),y=-.1,label="Positive Effect \n Approx. 36% of Population",colour="black")+
  annotate("text",x=.5*min(df$twoNormChange),y=-.1,label="Negative Effect \n Approx. 20% of Population",colour="black")+ 
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "grey"))


### this conducts a chi-squared on the subset including only AR-VV, evaluating the infinity-norm 
df$INorm153arvv <- pmax(abs(df$StudentAR-df$InstructorAR153),abs(df$StudentVV-df$InstructorVV153))
df$INorm255arvv <- pmax(abs(df$StudentAR-df$InstructorAR255),abs(df$StudentVV-df$InstructorVV255))
df$INormChangeArvv <- df$INorm153-df$INorm255
h5 <- countEm(df$INormChangeArvv,df$GradeChange) #we note we get a "worse" p-value if we include the zeros... but still significant
chisq.test(h5)

#this is for the norm-discussion plot
x <- c(1,5)
y <- c(1,4) 
df <- data.frame(x,y)

ggplot(df,aes(x,y))+
  geom_point()+
  geom_point(x=1,y=4)+
  geom_text(x=5,y=4.2,label="Instructor",aes(fontface=2)) +
  geom_text(x=1,y=4.2,label="Student 1",aes(fontface=2)) +
  geom_text(x=1,y=0.8,label="Student 2",aes(fontface=2)) +
  geom_line(linetype = "dashed",size=0.8) +
  geom_text(x=2.4,y=2.8,label="2-Norm (dashed)") +
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "grey")) +
  xlim(0,6)+
  ylim(0,6)+
  geom_line(aes(x=c(1,5),y=c(1,1)),linetype="dotted",size=0.8) +
  geom_line(aes(x=c(5,5),y=c(1,4)),linetype="dotted",size=0.8) +
  geom_text(x=5.7,y=2.5,label="1-Norm (dotted)") +
  geom_line(aes(x=c(1,5),y=c(.9,.9)),size=0.8) +
  geom_text(x=3,y=.75,label="Infinity-Norm (solid)") +
  geom_line(aes(x=c(1,5),y=c(4,4)),size=0.8,linetype="dotdash") +
  geom_text(x=3,y=3.85,label="1-, 2-, and Infinity-Norms (dot/dash)") +
  labs(x="Distance in Active-Reflective (AR) Dimension",
     y="Distance in Visual-Verbal (VV) Dimension",
     title="Example Plot of 1-,2- and Infinity-norms") +
  theme(plot.title=element_text(hjust=.5)) +
  coord_fixed()

