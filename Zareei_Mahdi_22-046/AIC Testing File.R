

set.seed(101)
d <- data.frame(x=1:20,y=rpois(20,lambda=2))
m0 <- glm(y~1,data=d)
m1 <- update(m0,.~x)
m2 <- update(m0,.~poly(x,2))
bbmle::AICtab(m0,m1,m2,mnames=LETTERS[1:3])
Abbmle::ICtab(m0,m1,m2,base=TRUE,logLik=TRUE)
AICtab(m0,m1,m2,logLik=TRUE)
bbmle::AICctab(m0,m1,m2,weights=TRUE)
print(bbmle::AICctab(m0,m1,m2,weights=TRUE),min.weight=0.1)


extractAIC(m0)
extractAIC(m1)
extractAIC(m2)



summary(m2)


bbmle::AICtab(m2, m1, m0 )

models = list(m0,m1,m2)
model.names = c("first", "second", "third")
AICtab(m0,m1,m2)


(m0,m1,m2,mnames=LETTERS[1:3])