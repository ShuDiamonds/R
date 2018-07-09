library(MASS)
x <- read.csv("test_for_employment.csv")　#入社試験の成績データの読み込み
x

plot(x[,1],x[,2],col=x$入社後の評価, pch=x$入社後の評価)		#データのプロット

z <- lda(入社後の評価 ~ 学科+面接, x)					#判別分析の実行
z

abline(0,-(z$scaling[1,1])/z$scaling[2,1], col = 'red')　　　	# y = -(a/b) xのラインの描画
