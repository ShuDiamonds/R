x <- read.csv("test_for_employment.csv")　#入社試験の成績データの読み込み
x

plot(x[,1],x[,2],col=x$入社後の評価, pch=x$入社後の評価)

mean_全体 <- colMeans(x[,-3])				#列ごとの平均を求める
mean_全体

x_S <- subset(x[,-3],  x$入社後の評価 < 1.5)		# 高評価グループの抽出
x_S

x_F <- subset(x[,-3],  x$入社後の評価 > 1.5)		# 低評価グループの抽出
x_F


num_S <- nrow(x_S)					# 高評価グループのデータ数
num_S

num_F <- nrow(x_F)					# 低評価グループのデータ数
num_F


mean_S <- colMeans(x_S)					#列ごとの平均を求める
mean_S

WS <- scale(x_S,center = TRUE, scale = FALSE)	#列ごとに平均０とする
WS

mean_F <- colMeans(x_F)					#列ごとの平均を求める
mean_F

WF <- scale(x_F,center = TRUE, scale = FALSE)	#列ごとに平均０とする
WF


UB <- num_S*((mean_S-mean_全体) %*% t(mean_S-mean_全体)) +
	 num_F*((mean_F-mean_全体) %*% t(mean_F-mean_全体))
UB										# UB行列を求める

UW <- t(WS) %*% WS + t(WF) %*% WF
UW										# UW行列を求める

inv_UW <- solve(UW)							# UW行列の逆行列を求める
inv_UW

Eigen_value <- eigen(inv_UW %*% UB)					# UW^(-1) UB の固有値計算
Eigen_value

abline(0,-Re(Eigen_value$vectors[1,1])/Re(Eigen_value$vectors[2,1]), col = "red")　　　# y = -(a/b) xのラインの描画　
