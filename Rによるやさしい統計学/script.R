#２章
指導法<-c("C","B","A","B");

心理学テスト<-c(13,14,7,12,10,6,8,15,4,4,9,6,10,12,5,12,8,8,12,15)
hist(心理学テスト,breaks=10)
sum(心理学テスト)
mean(心理学テスト)
median(心理学テスト)
table(心理学テスト)
sqrt(心理学テスト)
length(心理学テスト)
var(心理学テスト)
summary(心理学テスト)
sd(心理学テスト)
z得点<-scale(心理学テスト)
hist(z得点,breaks=10)

######３章
#相関
統計テスト１<-c(6,10,6,10,5,3,5,9,3,3,11,6,11,9,7,5,8,7,7,9)
統計テスト２<-c(10,13,8,15,8,6,9,10,7,3,18,14,18,11,12,5,7,12,7,7)
plot(統計テスト１,統計テスト２)
plot(心理学テスト,統計テスト１)
plot(心理学テスト,統計テスト２)
cov(統計テスト１,統計テスト２)#共分散の計算
cor(統計テスト１,統計テスト２)#相関係数の計算

#４章
#正規分布
curve(dnorm(x,mean=0,sd=1),from=-4,to=4,add=TRUE)
curve(dnorm(x,mean=1,sd=2),from=-4,to=4,add=TRUE)
curve(dnorm(x,mean=1,sd=1),from=-4,to=4,add=TRUE)
curve(dnorm(x,mean=0,sd=1)+dnorm(x,mean=0,sd=1),from=-4,to=4,add=TRUE)

rnorm(5,mean=50,sd=10)
hist(rnorm(500,mean=0,sd=1))　#標準正規分布を推定して表示
dnorm(5,mean=50,sd=10)

#########余談
#プリンを買う確率がポアソン分布に従うと仮定して
#どれぐらい買うか考えてみる
#poisはポアソン分布のやつ
#まずtwitterを見て４月から７月１２までの約１５週間の間に買ったプリンの個数は１３個
#つまり13/15がポアソン分布の母数λとなる。

hist(rpois(1000,13/15)) #取りあえず図示!
#つぎに1週間でプリンを食べない確率を求める
ppois(0,13/15)
#一個だけ買う確率
dpois(1,13/15)
#３個以下買う確率
ppois(3,13/15)-dpois(0,13/15)


#　※dpoisは起こる確率（確率密度関数の値）　ppoisは累積分布関数の値
#############

標本<-rnorm(10,mean=50,sd=10)
summary(標本)


標本平均<-numeric(length=1000)

for(i in 1:10000){
  標本<-rnorm(10,mean=50,sd=10)
  標本平均[i]=mean(標本)
}
hist(標本平均,freq=FALSE)
絶対値５以下<-ifelse(abs(標本平均-50)<=5,1,0)
table(絶対値５以下)
curve(dnorm(x,mean=50,sd=sqrt(10)),add=TRUE)

curve(1*x,from=0,to=6)


#５章
###t検定
#帰無仮説　母平均は１２である
#対立仮説　母平均は１２でない
t.test(心理学テスト,mu=12,conf.level=0.95)
#ｐ値が0.05以下ので、帰無仮説は破棄される。




