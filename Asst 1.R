#input data
x1 = c(-1.32,0.50,-0.50,1.32,0)
#sum of sq
#a
ss<-t(x1)%*%x1 #we want only 1 SS variable thats why xt* x if we do x(4x1) * xt(1x4) we would get 4x4 mat
sum(x1^2)
#both have the same value
#b
variance <- ss/(NROW(x1)-1)
var(x1)
#both the values are almost similar
#c and d
sqrt(variance)
sd(x1)
#both are same


#second sheet
#a
y = c(-3.40,-1.40,0.60,4.60,-0.40)
scalarProd<-t(x1)%*%y
#its same as xy
sum(x1*y)

#b
coVariance <- scProd/(NROW(x1)-1)
cov(x1,y)
#both have the same results


#c corelation

#correlation <- scalarProd/(sum(abs(x1))*sum(abs(y)))
correlation <- scalarProd/(sqrt(sum(x1^2)*sum(y^2))) #||x|| norm of x sqrt of ss
cor(x1,y)

cov(x1,y)/(sd(x1)*sd(y))

#all three give the same value

#cor stdize cov or why do we need cor?



#sheet 4

regressionCoefficent <- scalarProd/ss
coVariance/var(x1) #both are same why

hatMatrix <- x1 %*% solve(t(x1)%*%x1) %*% t(x1)

yPred <- hatMatrix %*% y

residualsOFY <- y- yPred


# [1,] -0.233166
# [2,] -2.599558
# [3,]  1.799558
# [4,]  1.433166
# [5,] -0.400000
#through sspred
#a
1-var(yPred-y)/var(y) #not sure should ask professor

#b
rsq = 1-(var(residualsOFY)/var(y))
#c

(cor(y,yPred))^2

#to verify
summary(lm(y~x1))
