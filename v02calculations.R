##this section is for v02 vs heat
jpeg('worker1vo2vsheat.jpg')
worker1vo2=Vo2$VO2.Abs.Worker.1
fit_thresholdworker1=2.4
plot(Heat.vs.Hr$Worker.1.Heat,Vo2$VO2.Abs.Worker.1,col="green")
abline(h=fit_thresholdworker1,untf=FALSE,col="red")
abline(lm(worker1vo2 ~ worker1heat),col="blue")
dev.off()
##
jpeg('worker2vo2vsheat.jpg')
worker2vo2=Vo2$VO2.Abs.Worker.2
fit_thresholdworker2=2.4
plot(Heat.vs.Hr$Worker.2.Heat,Vo2$VO2.Abs.Worker.2,col="green")
abline(h=fit_thresholdworker2,untf=FALSE,col="red")
abline(lm(worker2vo2 ~ worker2heat),col="blue")
##
jpeg('worker3vo2vsheat.jpg')
worker3vo2=Vo2$VO2.Abs.Worker.3
fit_thresholdworker3=2.4
plot(Heat.vs.Hr$Worker.3.Heat,Vo2$VO2.Abs.Worker.3,col="green")
abline(h=fit_thresholdworker3,untf=FALSE,col="red")
abline(lm(worker3vo2 ~ worker3heat),col="blue")
dev.off()
##
jpeg('worker4vo2vsheat.jpg')
worker4vo2=Vo2$VO2.Abs.Worker.4
fit_thresholdworker4=2.4
plot(Heat.vs.Hr$Worker.4.Heat,Vo2$VO2.Abs.Worker.4,col="green")
abline(h=fit_thresholdworker4,untf=FALSE,col="red")
abline(lm(worker4vo2 ~ worker4heat),col="blue")
dev.off()
#### this section is for vo2 vs activity,physio,pace, and class
###activity
##worker1
jpeg('worker1vo2vsact.jpg')
worker1activity=activity_physio_pace_class$Worker.1.Activity
plot(activity_physio_pace_class$Worker.1.Activity,Vo2$VO2.Abs.Worker.1,col="yellow")
abline(h=fit_thresholdworker1,untf=FALSE,col="red")
abline(lm(worker1vo2 ~ worker1activity),col="blue")
dev.off()
##worker2
jpeg('worker2vo2vsact.jpg')
worker2activity=activity_physio_pace_class$Worker.2.Activity
plot(activity_physio_pace_class$Worker.2.Activity,Vo2$VO2.Abs.Worker.2,col="yellow")
abline(h=fit_thresholdworker2,untf=FALSE,col="red")
abline(lm(worker2vo2 ~ worker2activity),col="blue")
dev.off()
##worker3
jpeg('worker3vo2vsact.jpg')
worker3activity=activity_physio_pace_class$Worker.3.Activity
plot(activity_physio_pace_class$Worker.3.Activity,Vo2$VO2.Abs.Worker.3,col="yellow")
abline(h=fit_thresholdworker3,untf=FALSE,col="red")
abline(lm(worker3vo2 ~ worker3activity),col="blue")
dev.off()
##worker4
jpeg('worker4vo2vsact.jpg')
worker4activity=activity_physio_pace_class$Worker.4.Activity
plot(activity_physio_pace_class$Worker.4.Activity,Vo2$VO2.Abs.Worker.4,col="yellow")
abline(h=fit_thresholdworker4,untf=FALSE,col="red")
abline(lm(worker4vo2 ~ worker4activity),col="blue")
dev.off()
###physio