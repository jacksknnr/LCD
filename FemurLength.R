#######################################################
## LCD S2 2024, Module 2 tutorial
## J Kasza 2024-08-21
##Exploring Denver bone data
#######################################################

library(readxl)
library(ggplot2)

#Read in data:
femur <- read_excel("C:/Users/kjessica/Monash Uni Enterprise Dropbox/Jessica Kasza/LCD Teaching/2024 Sem 2/Tutorials/Module2/Code/Denver_data_edited.xlsx")

#Number of observations
dim(femur)

View(femur)

#Look at the variables
table(femur$Examagernd)
table(femur$Sex)

#Spaghetti plot
p1 <- ggplot(data = femur, aes(x = Examagernd, y = Fmaxln, group = Ind)) +
  theme_grey(base_size = 22)  +
  labs(x = "Age (years)", y ='Femur length')
p1 + geom_line(color='blue') 

#Spaghetti plot by sex
p2 <- ggplot(data = femur, aes(x = Examagernd, y = Fmaxln, group = Ind)) +
  theme_grey(base_size = 22)  +
  labs(x = "Age (years)", y ='Femur length')
p2 + geom_line(color='blue') + facet_grid(cols = vars(Sex))


#Substantial non-linearity: calculate residuals from a lowess curve
femur.lowess<-loess(Fmaxln~Examagernd, data=femur,span =0.5)

femur$pred <- predict(femur.lowess, femur$Examagernd)


plot(x=femur$Examagernd,y=femur$Fmaxln,pch='.',xlab='Age (years)',
     ylab='Femur length')
lines(seq(3, 21, 0.5),predict(femur.lowess, data.frame(Examagernd = seq(3, 21, 0.5))) )

p3 <- ggplot(data = femur, aes(x = Examagernd, y = Fmaxln, group = Ind)) +
  theme_grey(base_size = 22)  +
  labs(x = "Age (years)", y ='Femur length')
p3 + geom_line(color='blue') + facet_grid(cols = vars(Sex)) + 
  geom_line(data = femur, aes(x=Examagernd, y=pred))
  
#Generate the residuals
femur$loess_red <- femur$Fmaxln - femur$pred

p4 <- ggplot(data = femur, aes(x = Examagernd, y = loess_red, group = Ind)) +
  theme_grey(base_size = 22)  +
  labs(x = "Age (years)", y ='Femur length residual')
p4 + geom_line(color='blue') + facet_grid(cols = vars(Sex))

#Separate loess curves for each sex
femur.lowess.sex1<-loess(Fmaxln~Examagernd, data=femur[femur$Sex == 1,],span =0.5)
femur.lowess.sex2<-loess(Fmaxln~Examagernd, data=femur[femur$Sex == 2,],span =0.5)

femur$pred.sex <- rep(0, nrow=nrow(femur))

femur$pred.sex[femur$Sex == 1] <-   predict(femur.lowess.sex1, femur$Examagernd)[femur$Sex == 1]
femur$pred.sex[femur$Sex == 2] <-   predict(femur.lowess.sex2, femur$Examagernd)[femur$Sex == 2]

p5 <- ggplot(data = femur, aes(x = Examagernd, y = Fmaxln, group = Ind)) +
  theme_grey(base_size = 22)  +
  labs(x = "Age (years)", y ='Femur length')
p5 + geom_line(color='blue') + facet_grid(cols = vars(Sex)) + 
  geom_line(data = femur, aes(x=Examagernd, y=pred.sex))

#Generate the residuals
femur$loess_red.sex <- femur$Fmaxln - femur$pred.sex

p6 <- ggplot(data = femur, aes(x = Examagernd, y = loess_red.sex, group = Ind)) +
  theme_grey(base_size = 22)  +
  labs(x = "Age (years)", y ='Femur length residual')
p6 + geom_line(color='blue') + facet_grid(cols = vars(Sex))


#Consider the pairwise scatter plots and the correlation matrices
#First need to reshape the data to wide form

femurwide <- reshape(as.data.frame(femur),idvar = "Ind", timevar = "Examagernd", 
                     v.names=c("Fmaxln", "pred", "pred.sex", "loess_red", "loess_red.sex" ),
                     direction='wide')
femurwide <- as.matrix(femurwide)


options(digits=3)
#correlations between observed values
cor(femurwide[, c(3,8,13,18,23,28,33,38, 43,48,53,58,63,68)], use="pairwise.complete.obs")

#What about for residuals?
cor(femurwide[, c(7,12,17,22,27,32,37,42,47,52,57,62,67,72,77)], use="pairwise.complete.obs")

#Scatterplot matrix:
pairs(femurwide[femurwide[,2] == 1, c(8,13,18,23,28,33,38, 43,48,53,58,63,68)])


