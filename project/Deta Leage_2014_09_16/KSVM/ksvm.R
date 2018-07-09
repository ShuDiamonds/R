#KSVMによる回帰予測
#参考URL　http://www1.doshisha.ac.jp/~mjin/R/31/31.html

#install.packages("kernlab")
library(kernlab)

#ｘとｙのデータを作り、曲線を作る。
 x1=seq(-10,10,0.1);set.seed(10)
 y1=50*sin(x1)+x1^2+10*rnorm(length(x1),0,1)
#xy.svmに予測フィルタをつくる。
 xy.svm<-ksvm(ksmooth(x1,y1)$x,ksmooth(x1,y1, "normal", bandwidth=1.4)$y,epsilon=0.01,kpar=list(sigma=16))
#リスト型のx1に対するyの予測をxy.svmを使いsy.preに代入
 sy.pre<-predict(xy.svm,x1)
#表示
 plot(x1,y1,type="l")
 lines(x1,sy.pre,col="red",lty=2)
 legend(locator(1),c("実測値","予測値"), lty=c(1,2),col=c(1,2))

#回帰の平滑化
attach(cars)
plot(speed, dist)
lines(ksmooth(speed, dist, "normal", bandwidth=1.3), col=2)
lines(ksmooth(speed, dist, "normal", bandwidth=4), col=3,lty=2)
detach("cars")


#このプログラムは、年間通算打数と年間通算安打をSVMで求め、予測するプログラムである。
#SVMでは、x軸に月を置く。年間通算打数と年間通算安打より年間通算打率が求まる。

##########ここからSVM分析をおこなう


##########初期共通処理
#学習データの読み込み
data.0 <- read.csv("train.csv")
summary(data.0)
head(data.0)

#出力用データフレーム定義
DF<-0
DF <- data.frame(ID=0,monthdata=0)
DF <- DF[-1,]

#カウント用変数定義
x<- 0
dumy<-0

##########データ処理開始
for(i in data.0$打者ID)
{
#１度行ったIDに対して何度も繰り返さないようにする分岐
if(i!=dumy)
{
#xをカウント
x<- x+1
##########１レコードに対して単回帰分析を行う

#同じIDのデータフレームを読みこむ
data <- data.0[data.0$打者ID=="10029", ]

#年間通算打数の予測(month6に６月の、month7に７月段階の通算打数が入る)

#xy.svmに予測フィルタをつくる。
xy.svm<-ksvm(data$年間通算打数,data$月,epsilon=0.01,kpar=list(sigma=16))
#リスト型の年間通算打数に対する６，７月の予測をxy.svmを使い予測
month6 <- predict(xy.svm,6)
month7 <- predict(xy.svm,7)

#年間通算安打の予測(month26に６月の、month27に７月段階の通算安打が入る)
xy.svm2<-ksvm(data$年間通算安打,data$月,epsilon=0.01,kpar=list(sigma=16))
month26 <- predict(xy.svm2,6)
month27 <- predict(xy.svm2,7)
ab<-month26/month6
cd<-month27/month7
if(!is.na(ab) && !is.na(cd))
{
if(ab>cd)
{
cd<-ab
}
}

#データ代入
DF[2*x-1,1]<-paste(data$打者ID[1],"_2014_6",sep = "")
DF[2*x-1,2]<-ab
DF[2*x,1]<-paste(data$打者ID[1],"_2014_7",sep = "")
DF[2*x,2]<-cd

}
dumy<-i
}

##########ここまで１レコードに対しての単回帰分析


#欠損値:Na を０に変換
DF$monthdata<- ifelse(is.na(DF$monthdata),0,DF$monthdata)
#マイナスの時に値を平均値に変換
DF$monthdata<- ifelse(DF$monthdata<0,0.21374,DF$monthdata)
#ありえない打率を平均値に変換
DF$monthdata<- ifelse(DF$monthdata>0.7,0.21374,DF$monthdata)


#メモIDは全部で1173個
#確認
x
length(data.0$打者ID)
head(DF)
#予測データの保存
write.table( DF, file="outputfile3.csv", sep=",", fileEncoding="UTF-8" )



##############データの照合と代入
#応募参考フォルダの読み込み
outputsample <- read.csv("submission_sample.csv")
#初めの行がoutputsampleには、消えているので追加
nick <-data.frame(X10278_2014_6="10278_2014_6",X0=0)
outputsample <- rbind(nick,outputsample)

#提出用データの中のIDに合うIDのものを予測データから探しそれを出力する
z<-0

for(k in outputsample$X10278_2014_6)
{
z<-z+1

#予測データの中に提出用データのIDがあるかの分岐
if(length(DF[DF$ID==k,2])!=0)
{
outputsample[outputsample$X10278_2014_6==k,2] <- DF[DF$ID==k,2]
}else
{
#ないときは、とりあえず平均値を代入しておく
outputsample[outputsample$X10278_2014_6==k,2] <- 0.21374
}

}

# メモ　DFに600101_2014_6がなくて困ってるDF[DF$ID=="600241_2014_6",2]で確認
z
length(outputsample$X10278_2014_6)

head(outputsample)
#提出ファイル出力
write.table( outputsample, file="outputfile2.csv", sep=",", fileEncoding="UTF-8" )

#######ここまで単回帰分析




#提出データの年間通算打率のヒストグラムの表示
library(ggplot2)
dev.new()
ggplot(outputsample,aes(x=X0)) +
geom_histogram() +
theme_bw(16) +
ylab("count")


#学習用データの年間通算打率のヒストグラムの表示
dev.new()
ggplot(DF,aes(x=monthdata)) +
geom_histogram() +
theme_bw(16) +
ylab("count")






#参考プログラム

aaa<-DF[DF$ID=="11034_2014_6",2]
ccc<-DF[DF$ID=="600241_2014_6",2]
length(aaa)
length(ccc)

mode(aaa)
aaa
bbb<-as.numeric(aaa)
mode(bbb)
f<-c(1,12,"wfd")
mode(f)
f











