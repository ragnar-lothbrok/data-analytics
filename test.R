library(class)
E=read.csv("/home/raghunandangupta/Downloads/soc_gen_data/train.csv")

cl=E[,ncol(E)]
cl=as.factor(cl)
E=E[,-ncol(E)]

E=E[,-1]
#E=E[,fs]

#ma <- function(x,n){filter(x,2*c(1:n)/(n*(n+1)), sides=1)}
ma <- function(x,n){filter(x,rep(1/n,n), sides=1)}
nma=20
nk=30
svm_res=vector(length = nma)
acc=matrix(ncol = nk,nrow = nma)
for(ii in 1:nma)
{
  
  print(ii)
  n=ii+5
  ME=ma(E,n)
  
  rem=(which(is.na(ME[,1])))
  ME=ME[-rem,]
  TE=E[-rem,]
  clT=cl[-rem]
  
  TE.train=TE[1:(2500-n),]
  ME.train=ME[1:(2500-n),]
  
  TE.test=TE[(2500-n+1):nrow(TE),]
  ME.test=ME[(2500-n+1):nrow(ME),]
  
  
  cltr=clT[1:(2500-n)]
  clte=clT[(2500-n+1):length(clT)]
  
  train=matrix(-1,ncol = ncol(TE.train),nrow = nrow(TE.train))
  for(i in 1:nrow(train))
  {
    for(j in 1:ncol(train))
    {
      if(ME.train[i,j]>=TE.train[i,j])
      {
        train[i,j]=1
      }
    }
  }
  
  test=matrix(-1,ncol = ncol(TE.test),nrow = nrow(TE.test))
  for(i in 1:nrow(test))
  {
    for(j in 1:ncol(test))
    {
      if(ME.test[i,j]>=TE.test[i,j])
      {
        test[i,j]=1
      }
    }
  }
  #ax=vector(length = nk)
  for(kk in 1:nk)
  {
    pred=knn(train,test,cltr,k=(2*kk-1))
    #model=svm(train,cltr,kernel = "radial",gamma = 0.00195)
    #pred=predict(model,test)
    t=table(pred,clte)
    acc[ii,kk]=(t[1,1]+t[2,2])/(t[1,1]+t[2,1]+t[1,2]+t[2,2])
    #ax[kk]=(t[1,1]+t[2,2])/(t[1,1]+t[2,1]+t[1,2]+t[2,2])
    #svm_res[ii]=(t[1,1]+t[2,2])/(t[1,1]+t[2,1]+t[1,2]+t[2,2])
  }
  #print(ax)
  #print(abs(acc[ii,]-0.5)*10,digits = 1)
  
}

place=which(acc==max(acc))
pla=place/10
cat(place-((round(pla)-1)*10),round(pla))

