
##########初期共通処理
#学習データの読み込み
data.means <- read.csv("train.csv")

#欠損値:Na を０に変換
data.means$年間通算打率<- ifelse(is.na(data.means$年間通算打率),0,data.means$年間通算打率)


#data.means.pca <- prcomp(data.means[,6:20],scale=T)
data.means.pca <- prcomp(data.means[,c(6,7,8,9,11,12,13,16,17,19,20,28,29,30)],scale=T)
#表示
#dev.new()
biplot(data.means.pca)


############# k-means法による非階層クラスタリング
#data.means.km <- kmeans(scale(data.means[,6:20]),5)
data.means.km <- kmeans(scale(data.means[,30]),20)
#主成分分析の結果にクラスターの情報を付加する
data.means.pca.df <- data.frame(data.means.pca$x)
data.means.pca.df$name <- rownames(data.means)
data.means.pca.df$cluster <- as.factor(data.means.km$cluster)

library(ggplot2)
#描画
dev.new()
ggplot(data.means.pca.df,aes(x=PC1,y=PC2,label=name,col=cluster))+
geom_text()+
theme_bw(16)



#レーダーチャートの作成
#install.packages("fmsb")
library(fmsb)
#レーダーチャート用にデータを整形
df <- as.data.frame(scale(data.means.km$centers))
dfmax <- apply(df,2,max)+1
dfmin <- apply(df,2,min)-1
df <- rbind(dfmax,dfmin,df)
#レーダーチャートの描画
dev.new()
radarchart(df,seg=5,plty=1,pcol=rainbow(5))
legend("topright",legend=1:3,col=rainbow(5),lty=1)




#結合
x1 <- data.frame(data.means.km$cluster)
head(x1)
x2 <- cbind(data.means,x1)
head(x2)
write.table( x2, file="train2.csv", sep=",")

x3 <- x2

#平均値を出すため欠損値:Na を０に変換
x3$年間通算打率<- ifelse(is.na(x3$年間通算打率),0,x3$年間通算打率)

mean1 <- mean(x3[x3$data.means.km.cluster==1,]$年間通算打率)
mean2 <- mean(x3[x3$data.means.km.cluster==2,]$年間通算打率)
mean3 <- mean(x3[x3$data.means.km.cluster==3,]$年間通算打率)
mean4 <- mean(x3[x3$data.means.km.cluster==4,]$年間通算打率)
mean5 <- mean(x3[x3$data.means.km.cluster==5,]$年間通算打率)
mean6 <- mean(x3[x3$data.means.km.cluster==6,]$年間通算打率)
mean7 <- mean(x3[x3$data.means.km.cluster==7,]$年間通算打率)
mean8 <- mean(x3[x3$data.means.km.cluster==8,]$年間通算打率)
mean9 <- mean(x3[x3$data.means.km.cluster==9,]$年間通算打率)
mean10 <- mean(x3[x3$data.means.km.cluster==10,]$年間通算打率)
mean11 <- mean(x3[x3$data.means.km.cluster==11,]$年間通算打率)
mean12 <- mean(x3[x3$data.means.km.cluster==12,]$年間通算打率)
mean13 <- mean(x3[x3$data.means.km.cluster==13,]$年間通算打率)
mean14 <- mean(x3[x3$data.means.km.cluster==14,]$年間通算打率)
mean15 <- mean(x3[x3$data.means.km.cluster==15,]$年間通算打率)
mean16 <- mean(x3[x3$data.means.km.cluster==16,]$年間通算打率)
mean17 <- mean(x3[x3$data.means.km.cluster==17,]$年間通算打率)
mean18 <- mean(x3[x3$data.means.km.cluster==18,]$年間通算打率)
mean19 <- mean(x3[x3$data.means.km.cluster==19,]$年間通算打率)
mean20 <- mean(x3[x3$data.means.km.cluster==20,]$年間通算打率)

##########ここから単回帰分析をおこなう


##########初期共通処理
#学習データの読み込み
data.0 <- read.csv("train2.csv")
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
data <- data.0[data.0$打者ID==i, ]
LL <- names(which.max(table(data$data.means.km.cluster)))
MMM <- switch(LL,
"1"=mean1,
"2"=mean2,
"3"=mean3,
"4"=mean4,
"5"=mean5,
"6"=mean6,
"7"=mean7,
"8"=mean8,
"9"=mean9,
"10"=mean10,
"11"=mean11,
"12"=mean12,
"13"=mean13,
"14"=mean14,
"15"=mean15,
"16"=mean16,
"17"=mean17,
"18"=mean18,
"19"=mean19,
"20"=mean20
)

#データ代入
DF[2*x-1,1]<-paste(data$打者ID[1],"_2014_6",sep = "")
DF[2*x-1,2]<-MMM
DF[2*x,1]<-paste(data$打者ID[1],"_2014_7",sep = "")
DF[2*x,2]<-MMM

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
head(DF,15)
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



#t検定

t.test(x3[x3$data.means.km.cluster==1,]$年間通算打率,x3[x3$data.means.km.cluster==4,]$年間通算打率)

#提出データの年間通算打率のヒストグラムの表示
dev.new()
ggplot(outputsample,aes(x=X0)) +
geom_histogram() +
theme_bw(16) +
ylab("count")


#学習用データの年間通算打率のヒストグラムの表示
dev.new()
ggplot(data.means,aes(x=年間通算安打)) +
geom_histogram() +
theme_bw(16) +
ylab("count")


#各クラスタごとの年間通算打率のヒストグラムの表示
ggplot(x3[x3$data.means.km.cluster==4,],aes(x=年間通算打率)) +
geom_histogram() +
theme_bw(16) +
ylab("count")

#年間通算打率のヒストグラムの表示
ggplot(x3,aes(x=年間通算打率)) +
geom_histogram() +
theme_bw(16) +
ylab("count")
dev.new()

