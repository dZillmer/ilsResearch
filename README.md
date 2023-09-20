# Index of Learning Styles Research Project

This project began as a fellow junior rotating instructor (in the Department of Mathematical Sciences (D/Math) at the United States Military Academy (USMA)) and I were working on completing a teacher-oriented professional development program called the Master Teacher Program (MTP). As part of the MTP program, which consisted of reaching a few books, writing reflections and sharing insights and best practices, and observing/being observed by other instructors, we also had to complete a research project. A fellow D/Math instructor and I were interested in considering how different "learning styles" could potentially help cadets (i.e., students at USMA, all of whom take many disparate classes in many subjects while balancing many other commitments) learn better -- but we were unimpressed with most of the existing "research." We were dissatisfied with it because largely the daa were unconvincing: low sample numbers, content classification of learning style was done with a different metric than student classification, poor study design, and a host of other factors. 

So we designed our own study, ran it through our institution's Human Resources Protection Program (HRPP) Institutional Review Board (IRB) process. Once that was approved, we built surveys, collected data, merged disparate data sets, and cleaned / sanitized the data. Part of why we selected Felder and Silverman's Index of Learning Styles (ILS) was that it generates a "score" in four dimensions to assess learning style preferences; this means that we could leverage quantitative variables between students and instructors (rather than arbitrary factors), and these quantitative variables had "strength" (weak to strong preference) rather than binary options. So we considered students one point in "learning space" (a four position vector), and the instructor another point, and evaluated the change in grade across semesters (hence controlling for students) as a function of student-instructor distance in learning space. It was a fun project that combined our passion for teaching, some data science flavour and work, and a mathematically elegent transformation of the oft-contestend learning style discussion. Two sample graphics are below.

A visualization we used to describe the four dimensions in ILS: 
![Visualization of ILS' Four Dimensions](/images/ils4d.png)

A scatterplot of our refined data to visualize the 2-norm distances that demonstrates how we considered statistical significance of our results: 
![Scatter Plot](/images/2normPlot.png)

Our results were statistically significant, and so we pursued publication. Ultimately, our first acceptance was from the International Journal on Studies in Education, where our paper was published [here](https://ijonse.net/index.php/ijonse/article/view/133). The major files we used for our analysis are hosted in this repository. Because the data involve human subjects, we are not supposed to publicly air them; see either myself or my coauthor if you are interested. 

Abstract: Students are often assigned to instructors by student schedule or request. We present findings that assigning students to instructors based on learning style similarity can improve learning. We assume that instructors teach consistent with their learning style, and use the Felder and Silverman Index of Learning Styles (ILS) to describe learning style as a vector in $R^4$. We examine the effect on grade difference a as a function of the learning style “distance” between students and their instructors. Chi-squared tests show that the students (N=300) who were more similar (nearer) to their instructor performed better (p-value: 0.000453), and linear regression indicates that student performance could improve by up to 2% by being assigned to a more similar instructor.
