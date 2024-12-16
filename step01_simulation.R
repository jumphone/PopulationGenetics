

GetP0<-function(N=1000, Freq=c(0.5,0.5), Type=c('A','a')){
    N=N
    ExGeneFreq=Freq
    ExGeneType=Type
    H1G1= ExGeneType[1-rbinom(N, 1, ExGeneFreq[1])+1]
    H2G1= ExGeneType[1-rbinom(N, 1, ExGeneFreq[1])+1]
    SEX=sample(c(rep('M', N/2),rep('F',N/2)))
    TAG=rep('',N)
    i=1
    while(i<=N){
        TAG[i]=paste0(sort(c(H1G1[i],H2G1[i]),decreasing=TRUE),collapse='')
        i=i+1}
    OUT=list()
    OUT$H1G1=H1G1
    OUT$H2G1=H2G1
    OUT$SEX=SEX
    OUT$TAG=TAG
    OUT$N=N
    return(OUT)
    }




Cross <-function(OUT){
    H1G1=OUT$H1G1
    H2G1=OUT$H2G1
    SEX=OUT$SEX
    N=OUT$N
    
    N=length(H1G1)
    TMP=table(c(H1G1,H2G1))
    GeneType=names(TMP)
    GeneFreq=TMP/(2*N)
    M.index=which(SEX=='M')
    F.index=which(SEX=='F')
    M.index=sample(M.index)
    F.index=sample(F.index)    
    C.H1G1=c()
    C.H2G1=c()
    i=1
    while(i<=length(M.index)){
        this_M=M.index[i]
        this_F=F.index[i]
        this_M_H1H2G1= c(H1G1[this_M], H2G1[this_M])
        this_F_H1H2G1= c(H1G1[this_F], H2G1[this_F])
        ChildN=2
        j=1
        while(j<=ChildN){
            this_H1G1=sample(this_M_H1H2G1,1)
            this_H2G1=sample(this_F_H1H2G1,1)
            C.H1G1=c(C.H1G1,this_H1G1)
            C.H2G1=c(C.H2G1,this_H2G1)
            j=j+1
            }
        i=i+1}
    C.SEX=sample(c(rep('M', N/2),rep('F',N/2)))
    C.TAG=rep('',N)
    i=1
    while(i<=N){
        C.TAG[i]=paste0(sort(c(C.H1G1[i],C.H2G1[i]),decreasing=TRUE),collapse='')
        i=i+1}
    OUT=list()
    OUT$H1G1=C.H1G1
    OUT$H2G1=C.H2G1
    OUT$SEX=C.SEX
    OUT$TAG=C.TAG
    OUT$N=N
    return(OUT)
    }

StatFreq<-function(OUT){
    print(table(c(OUT$H1G1,OUT$H2G1)))
    print(table(c(OUT$H1G1,OUT$H2G1))/(2*OUT$N))
    print(table(OUT$TAG))
    print(table(OUT$TAG)/OUT$N)
    }




set.seed(123)
OUT=GetP0(1000,c(0.5,0.5),c('A','a'))

P.H1G1=OUT$H1G1
P.H2G1=OUT$H2G1
P.TAG=OUT$TAG

set.seed(123)
X=rnorm(OUT$N)
Y=rnorm(OUT$N)
XLIM=c(min(X)-0.5,max(X)+0.5)
YLIM=c(min(Y)-0.5,max(Y)+0.5)
plot(0,0,xlim=XLIM,ylim=YLIM,col='white')
text(P.TAG, x=X,y=Y)

StatFreq(OUT)

###########################################################


set.seed(123)
P.OUT=GetP0(10,c(0.9,0.1),c('A','a'))

F1.OUT=Cross(P.OUT)
F2.OUT=Cross(F1.OUT)
F3.OUT=Cross(F2.OUT)
F4.OUT=Cross(F3.OUT)
F5.OUT=Cross(F4.OUT)

StatFreq(P.OUT)
StatFreq(F1.OUT)
StatFreq(F2.OUT)
StatFreq(F3.OUT)
StatFreq(F4.OUT)
StatFreq(F5.OUT)



set.seed(123)
P.OUT=GetP0(1000,c(0.9,0.1),c('A','a'))

F1.OUT=Cross(P.OUT)
F2.OUT=Cross(F1.OUT)
F3.OUT=Cross(F2.OUT)
F4.OUT=Cross(F3.OUT)
F5.OUT=Cross(F4.OUT)

StatFreq(P.OUT)
StatFreq(F1.OUT)
StatFreq(F2.OUT)
StatFreq(F3.OUT)
StatFreq(F4.OUT)
StatFreq(F5.OUT)



