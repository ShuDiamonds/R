#参考ＵＲＬ: http://www.trifields.jp/research-conjoint-analysis-344

#conjointパッケージのインストール
install.packages("conjoint")
#conjointパッケージの読み込み
library("conjoint")
#teaデータの読み込み
data(tea)

#因子の組み合わせを作成

experiment<-expand.grid(
price=c("low","medium","high"),
variety=c("black","green","red"),
kind=c("bags","granulated","leafy"),
aroma=c("yes","no"))

#因子の直交表を作成
design<-caFactorialDesign(data=experiment,type="orthogonal")
#因子の直交表を表示
print(design)
#因子の直交表をコード化
code<-caEncodedDesign(design)
#コード化された直交表の表示
print(code)
#相関係数の表示
print(cor(code))

#商品プロファイル-アンケートに記載する13個の商品
print(tprof)

#各水準のラベル
print(tlevn)
#アンケートデータの表示
#このデータは直交計画から13個の組み合わせを作り10点満点で評価
head(tprefm)

Conjoint(tprefm, tprof, tlevn)

#5つのクラスターに分類
segments<-caSegmentation(tprefm,tprof,5)

newdata<-merge(tprefm,segments$cluster)
head(newdata)


hist(newdata[newdata$y==5,]$profil1)


