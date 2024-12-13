



ExGeneType=c('A','a')
ExGeneFreq=c(0.5,0.5) #p and q

N=1000

set.seed(123)
H1G1= ExGeneType[rbinom(N, 1, ExGeneFreq[1])+1]
H2G1= ExGeneType[rbinom(N, 1, ExGeneFreq[2])+1]
SEX=sample(c(rep('M', N/2),rep('F',N/2)))

TMP=table(c(H1G1,H2G1))
print(TMP)
GeneType=names(TMP)
GeneFreq=TMP/(2*N)

TAG=rep('',N)
i=1
while(i<=N){
    TAG[i]=paste0(sort(c(H1G1[i],H2G1[i]),decreasing=TRUE),collapse='')
    i=i+1}

set.seed(123)
X=rnorm(N)
Y=rnorm(N)
XLIM=c(min(X)-0.5,max(X)+0.5)
YLIM=c(min(Y)-0.5,max(Y)+0.5)

plot(0,0,xlim=XLIM,ylim=YLIM,col='white')
text(TAG, x=X,y=Y)

print(table(TAG))
print(table(TAG)/N)


Cross_func <-function(H1G1, H2G1, SEX){
    N=length(H1G1)
    TMP=table(c(H1G1,H2G1))
    GeneType=names(TMP)
    GeneFreq=TMP/(2*N)
    M.index=which(SEX=='M')
    F.index=which(SEX=='F')
    M.index=sample(M.index)
    F.index=sample(F.index)
    i=1
    while(i<=length(M.index)){
        this_M=M.index[i]
        this_F=F.index[i]
        this_M_H1G1= H1G1[this_M]
        this_M_H
        i=i+1}
    
    OUT=list()
    

    }


