library('adagio')
library('xlsx')
library("ggplot2")
library("broom")
d <- read.xlsx('D.xlsx',1)
#setwd("C:/Users/MarioJaraCobos/Desktop")

d <- as.data.frame(d)
as.data.frame(summary(d))

b <- d[ ,-1]
c <- (as.data.frame(cor(b)))
Orders <- b[,2]
JEdel <- b[,8]
JEnodel <- b[,10]

Betas <- lm(Orders~JEdel+JEnodel)

##PLOT
plot(Orders,JEdel)
plot(Orders,JEnodel)

#diagnostics
model.diag.metrics <- augment(Betas)
ggplot(model.diag.metrics, aes(Orders, JEdel)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Orders, yend = .std.resid), color = "red", size = 0.3)

model.diag.metrics <- augment(Betas)
ggplot(model.diag.metrics, aes(Orders, JEnodel)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Orders, yend = .fitted), color = "red", size = 0.3)

#WHICH RESTAURANTS
d[,12]<- (((b[,7]*Betas[[1]][2]+b[,9]*Betas[[1]][3])+Betas[[1]][1])-b[,2])
d[,13]<-(d[,12]/sum(d[,12]))*758
d[,14]<-(d[,3]/d[,7])


summary(d[,14]>65)

393/(393+257)


##WHOLE MODEL
p <- lm(Orders~b[,1]+b[,3]+b[,4]+b[,5]+b[,6]+b[,7]+b[,8]+b[,9]+b[,10])





#KNAPSACK ALGORITHM
w <- cbind(d[,8],d[,10])

z <- d[,3]/d[,9]
y <- d[,3]/d[,11]
z[is.na(z)] <- 0
z[is.infinite(z)]<-0
y[is.na(y)] <- 0
y[is.infinite(y)]<-0


p <- cbind(z,y)
cap <- 759


x <- knapsack(w, p, cap)
#x <- x[[3]][-128]


p <- matrix(0,nrow = length(x[[3]]))

for (i in 1:length(x[[3]])) {
  p[i,1] <- matrix((x[[3]][i]))
}



l <- list(0)
for (i in 1:length(p)) {
  l[[i]] <- as.character(d[p[i],1])
}


s <- data.frame(0)
for (i in 1:length(p)) {
  s[i,2:11] <- d[p[i],2:11]
}

sum(na.omit(s[,3])/length(s[,1]))
sum(d[,3])/length(d[,1])
sum(na.omit(s[,3])/length(s[,1]))/(na.omit(sum(d[,3])/length(d[,1])))

