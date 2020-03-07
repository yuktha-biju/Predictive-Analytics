df <- read.csv("Z:\\pa\\data_banknote_authentication.txt", sep=',' , header = F)
logit <- glm(df[,5] ~ df[,1]+df[,2]+df[,3]+df[,4],data=df,family="binomial")
print(summary(logit))
print(length(df[,1]))
b1=summary(logit)$coefficients[2,1]
b2=summary(logit)$coefficients[3,1]
b3=summary(logit)$coefficients[4,1]
b4=summary(logit)$coefficients[5,1]
p=df[,1]
q=df[,2]
r=df[,3]
s=df[,4]
print("Predict")
for(i in 1:length(p))
{
  z=b1*p[i]+b2*q[i]+b3*r[i]+b4*s[i]-7.3218
  if(z<0.5)
#    print("Not Authenticated")
  else
#    print("Authenticated")
}

newdat <- data.frame(hp=seq(min(df[,1]+df[,2]+df[,3]+df[,4]), max(df[,1]+df[,2]+df[,3]+df[,4]),len=1372))
newdat
Authentic = predict(logit, newdata=newdat, type="response")
plot(Authentic ~ df[,1]+df[,2]+df[,3]+df[,4], col="darkgreen")
#lines(Authentic ~ df[,1]+df[,2]+df[,3]+df[,4], col="green4", lwd=2)
abline(logit,col="blue",lw=2)
