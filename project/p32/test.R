#データを読みこみ
body.data <- read.csv("body_sample.csv",header=T,stringsAsFactors=F)

# ライブラリの読み込み
library(ggplot2)

#要約データ表示
summary(body.data)
#条件に合ったデータを表示
body.data[body.data$height,]
#標準偏差の表示
sd(body.data$height)
#ヒストグラムの表示
ggplot(body.data,aes(x=height,y=weight))+
geom_histogram()+
theme_bw(16)+
ylab("coun"t)
# データと全体設定を持ったggplotオブジェクトを作る
ggplot(body.data,aes(x=height,y=weight,col=gender))+
geom_point()+
theme_bw(16)+
geom_smooth(method=glm)
#相関係数表示
cor(body.data$height,body.data$weight,method="spearman")
#相関検定
cor.test(body.data$height,body.data$weight,method="pearson")



# ライブラリの読み込み
library(ggplot2)

# データと全体設定を持ったggplotオブジェクトを作る
gp = ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, colour=Species))


# グラフのレイヤーを重ねる
gp = gp + geom_point(size=3, alpha=0.7)

# 描画してみる
print(gp)

# さらに変更を重ねる
gp = gp + geom_smooth(method=glm, family=gaussian)
gp = gp + labs(title="花の種類：Iris Sepal")
#gp = gp + geom_histogram(fill=..count..)
#gp = gp + geom_density(alpha = 0.2)
print(gp)
