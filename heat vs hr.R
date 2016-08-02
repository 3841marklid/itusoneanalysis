Worker3heatindex <- heat.index(t = Worker.3$Temp, rh = Hum) #worker3heatindex defines a value, heat.index is a function, t and rh are the variables the function uses to compute the heat index
print(Worker3heatindex)
worker3dataframe <-data.frame(Worker3heatindex)
##
Worker4heatindex <- heat.index(t = Worker.4$Temp, rh = Hum)
worker4dataframe <-data.frame(Worker4heatindex)
##
Worker2heatindex <- heat.index(t = Worker.2$Temp, rh = Hum)
worker2dataframe <-data.frame(Worker2heatindex)
##
jpeg('worker1hrheatplot.jpg')
worker1hr=Heat.vs.Hr$Worker.1.HR
worker1heat=Heat.vs.Hr$Worker.1.Heat
plot(Heat.vs.Hr$Worker.1.Heat,Heat.vs.Hr$Worker.1.HR)
abline(lm(worker1hr ~ worker1heat))
dev.off()
##
jpeg('worker2hrheatplot.jpg')
worker2hr=Heat.vs.Hr$Worker.2.HR
worker2heat=Heat.vs.Hr$Worker.2.Heat
plot(Heat.vs.Hr$Worker.2.Heat,Heat.vs.Hr$Worker.2.HR)
abline(lm(worker2hr ~ worker2heat))
dev.off()
##
jpeg('worker3hrheatplot.jpg')
worker3hr=Heat.vs.Hr$Worker.3.HR
worker3heat=Heat.vs.Hr$Worker.3.Heat
plot(Heat.vs.Hr$Worker.3.Heat,Heat.vs.Hr$Worker.3.HR)
abline(lm(worker3hr ~ worker3heat))
dev.off()
##
jpeg('worker4hrheatplot.jpg')
worker4hr=Heat.vs.Hr$Worker.4.HR
worker4heat=Heat.vs.Hr$Worker.4.Heat
plot(Heat.vs.Hr$Worker.4.Heat,Heat.vs.Hr$Worker.4.HR)
abline(lm(worker4hr ~ worker4heat))
dev.off()
