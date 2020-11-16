data0=read_csv("geneexp.csv")
data=data0[, -1]
data$CellType=as.factor(data$CellType)
set.seed(12345)
n <- dim(data)[1]
id=sample(1:n, floor(n*0.7))
train=data[id,]
test=data[-id,]

library(pamr)
rownames(train)=1:nrow(train)
genenames <- colnames(train[ ,-ncol(train)])
x <- t(train[ ,-ncol(train)])
y=train[[ncol(train)]]
mydata=list(x=x,y=as.factor(y),geneid=as.character(1:nrow(x)), genenames=genenames)
model=pamr.train(mydata)
pamr.plotcen(model, mydata, threshold = 6.7)
pamr.plotcen(model, mydata, threshold=6.7)
a=pamr.listgenes(model,mydata,threshold=6.7)
cat( paste( colnames(data)[as.numeric(a[,1])], collapse='\n' ) )
cvmodel=pamr.cv(model,mydata)
print(cvmodel)
pamr.plotcv(cvmodel)



library(glmnet)
x <- as.matrix(train[ ,-ncol(train)])
cv.lelastic <- cv.glmnet(x, train$CellType, alpha = 0.5, family = "multinomial")
elastic <- glmnet(x, train$CellType, alpha = 0.5, family = "multinomial",
                      lambda = cv.lelastic$lambda.min)

res <- predict(elastic, newx = as.matrix(test[ ,-ncol(test)]), s = cv.lelastic$lambda.min, type = "class")