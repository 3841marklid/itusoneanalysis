###this section is for calculating the ideal relative heart rate using Vo2
##worker1
age1 <- Vo2[1,"Age.Worker.1"] #this calls the age
absmaxhr1=209-.7*(age1) #maximum hr equation
rhr1=((Heat.vs.Hr$Worker.1.HR)/absmaxhr1)*100 #calculates relative heart rate index
vo2rhr1 <- data.frame(Relative.Heart.Rate=rhr1, Vo2.Abs=Vo2$VO2.Abs.Worker.1) #create new data framewith just vo2 and rhr
vo2w1=vo2rhr1$Vo2.Abs #create index for vo2 values
plot(rhr1,Vo2$VO2.Abs.Worker.1) #plot vo2 vs rhr
abline(h=fit_thresholdworker1,untf=FALSE,col="red") #create the cutoff value for vo2. Anything less than 2.4 is considered "unsafe"
model1 <- lm(vo2w1 ~ rhr1, data=vo2rhr1) #create a linear model
abline(model1,col="green") #plot the model
equation1=coef(model1) #display the coefficients of the model
paste('y=',coef(model1)[[2]],'*x','+',coef(model1)[[1]]) #output the linear equation
slope1=coef(model1)[[2]] #pulls slope from model
intercept1=coef(model1)[[1]] #pulls intercept from model
maxrhr1=(fit_thresholdworker1-intercept1)/slope1 #find where the plot drops below the acceptable vo2 value
maxhr1=(maxrhr1*(absmaxhr1)/100) #calculate corresponding heart rate
summary(model1)

##worker2
age2 <- Vo2[1,"Age.Worker.2"] #this calls the age
absmaxhr2=209-.7*(age2) #maximum hr equation
rhr2=((Heat.vs.Hr$Worker.2.HR)/absmaxhr2)*100 #calculates relative heart rate index
vo2rhr2 <- data.frame(Relative.Heart.Rate=rhr2, Vo2.Abs=Vo2$VO2.Abs.Worker.2) #create new data framewith just vo2 and rhr
vo2w2=vo2rhr2$Vo2.Abs #create index for vo2 values
plot(rhr2,Vo2$VO2.Abs.Worker.2) #plot vo2 vs rhr
abline(h=fit_thresholdworker2,untf=FALSE,col="red") #create the cutoff value for vo2. Anything less than 2.4 is considered "unsafe"
model2 <- lm(vo2w2 ~ rhr2, data=vo2rhr2) #create a linear model
abline(model2,col="green") #plot the model
equation2=coef(model2) #display the coefficients of the model
paste('y=',coef(model2)[[2]],'*x','+',coef(model2)[[1]]) #output the linear equation
slope2=coef(model2)[[2]] #pulls slope from model
intercept2=coef(model2)[[1]] #pulls intercept from model
maxrhr2=(fit_thresholdworker2-intercept2)/slope2 #find where the plot drops below the acceptable vo2 value
maxhr2=(maxrhr2*(absmaxhr2)/100) #calculate corresponding heart rate
summary(model2) # p value is less than 2.2e-16 and r squared adjusted is .95. Can assume data is significant.

##worker3
age3 <- Vo2[1,"Age.Worker.3"] #this calls the age
absmaxhr3=209-.7*(age3) #maximum hr equation
rhr3=((Heat.vs.Hr$Worker.3.HR)/absmaxhr3)*100 #calculates relative heart rate index
vo2rhr3 <- data.frame(Relative.Heart.Rate=rhr3, Vo2.Abs=Vo2$VO2.Abs.Worker.3) #create new data framewith just vo2 and rhr
vo2w3=vo2rhr3$Vo2.Abs #create index for vo2 values
plot(rhr3,Vo2$VO2.Abs.Worker.3) #plot vo2 vs rhr
abline(h=fit_thresholdworker3,untf=FALSE,col="red") #create the cutoff value for vo2. Anything less than 2.4 is considered "unsafe"
model3 <- lm(vo2w3 ~ rhr3, data=vo2rhr3) #create a linear model
abline(model3,col="green") #plot the model
equation3=coef(model3) #display the coefficients of the model
paste('y=',coef(model3)[[2]],'*x','+',coef(model3)[[1]]) #output the linear equation
slope3=coef(model3)[[2]] #pulls slope from model
intercept3=coef(model3)[[1]] #pulls intercept from model
maxrhr3=(fit_thresholdworker3-intercept3)/slope3 #find where the plot drops below the acceptable vo2 value
maxhr3=(maxrhr3*(absmaxhr3)/100) #calculate corresponding heart rate
summary(model3)

##worker4
age4 <- Vo2[1,"Age.Worker.4"] #this calls the age
absmaxhr4=209-.7*(age4) #maximum hr equation
rhr4=((Heat.vs.Hr$Worker.4.HR)/absmaxhr4)*100 #calculates relative heart rate index
vo2rhr4 <- data.frame(Relative.Heart.Rate=rhr4, Vo2.Abs=Vo2$VO2.Abs.Worker.4) #create new data framewith just vo2 and rhr
vo2w4=vo2rhr4$Vo2.Abs #create index for vo2 values
plot(rhr4,Vo2$VO2.Abs.Worker.4) #plot vo2 vs rhr
abline(h=fit_thresholdworker4,untf=FALSE,col="red") #create the cutoff value for vo2. Anything less than 2.4 is considered "unsafe"
model4 <- lm(vo2w4 ~ rhr4, data=vo2rhr4) #create a linear model
abline(model4,col="green") #plot the model
equation4=coef(model4) #display the coefficients of the model
paste('y=',coef(model4)[[2]],'*x','+',coef(model4)[[1]]) #output the linear equation
slope4=coef(model4)[[2]] #pulls slope from model
intercept4=coef(model4)[[1]] #pulls intercept from model
maxrhr4=(fit_thresholdworker4-intercept4)/slope4 #find where the plot drops below the acceptable vo2 value
maxhr4=(maxrhr4*(absmaxhr4)/100) #calculate corresponding heart rate
summary(model4)

##create data frame with all max hr values
Max_HR_Values <- data.frame(Worker.1=maxhr1,Worker.2=maxhr2,Worker.3=maxhr3,Worker.4=maxhr4)

##this section is for calculating the ideal heat index given rhr
#worker1
plot(rhr1,Heat.vs.Hr$Worker.1.Heat)
modelheat1 <- lm(worker1heat ~ rhr1)
abline(modelheat1)
heatvsrhrslope1=coef(modelheat1)[[2]]
heatvsrhrintercept1=coef(modelheat1)[[1]]
maxheat1=((heatvsrhrslope1)*maxrhr1)+heatvsrhrintercept1
summary(modelheat1) #p value is .871. We can safely assume no relationship exists between heat index and rhr. Therefore, the 74 high heat index cannot be reliable.