x <- read.csv("C:/Users/Furuhashi/Documents/Discriminant_Analysis/test_for_employment2.csv")　#入社試験の成績データの読み込み
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
UB								# UB行列を求める

UW <- t(WS) %*% WS + t(WF) %*% WF
UW								# UW行列を求める

inv_UW <- solve(UW)					# UW行列の逆行列を求める
inv_UW

Eigen_value <- eigen(inv_UW %*% UB)			# UW^(-1) UB の固有値計算
Eigen_value

v <- Re(Eigen_value$vectors[,1])			#固有ベクトルをvに代入
v

if (v[2] > 0)						#z軸の方向決定
	{ v <- -v
	} 

ve_S <- (t(v) %*% t(WS) %*% WS %*% v)/(num_S-1)	#veS^2の計算
ve_S

ve_F <- (t(v) %*% t(WF) %*% WF %*% v)/(num_F-1)	#veF^2の計算
ve_F

mean_zS <- t(v) %*% mean_S				#zSの平均値の計算(c = 0)
mean_zS

mean_zF <- t(v) %*% mean_F				#zFの平均値の計算(c = 0)
mean_zF

zc <- (sqrt(ve_F)*mean_zS + sqrt(ve_S)*mean_zF)/(sqrt(ve_F) + sqrt(ve_S))	
zc 								#zcの計算

y_intercept <- zc/v[2] 					#y切片の計算
y_intercept

abline(y_intercept,-v[1]/v[2], col = "red")
 								#y = -(a/b) x -(c/b)のラインの描画
