par(mfrow=c(2,1)) 					# Divide R Graphics window to 2×1

x <- read.csv("C:/Users/Furuhashi/Documents/Discriminant_Analysis/test_for_employment_3variables.csv")　
								#入社試験の成績データの読み込み
x


min_x1 <- min(x[,1]) - 0.5				# x1の最低値の抽出
min_x1

max_x1 <- max(x[,1]) + 0.5				# x1の最高値の抽出
max_x1

min_x2 <- min(x[,2]) - 0.5				# x2の最低値の抽出
min_x2

max_x2 <- max(x[,2]) + 0.5				# x2の最高値の抽出
max_x2

min_x3 <- min(x[,3])					# x3の最低値の抽出
min_x3

max_x3 <- max(x[,3])					# x3の最高値の抽出
max_x3

x_S <- subset(x[,-4],  x$入社後の評価 < 1.5)		# 高評価グループの抽出
x_S

x_F <- subset(x[,-4],  x$入社後の評価 > 1.5)		# 低評価グループの抽出
x_F

plot(x_S[,1],x_S[,2],xlim=c(min_x1,max_x1), ylim=c(min_x2,max_x2), col="black", pch=1,  xlab = "", ylab = "")
								# ｘ1 - x2の関係の描画　　　高評価グループを○で附置
par(new = TRUE)
plot(x_F[,1],x_F[,2],xlim=c(min_x1,max_x1), ylim=c(min_x2,max_x2), col="red", pch=4,  xlab = "x1", ylab = "x2")
								# ｘ1 - x2の関係の描画　　　低評価グループをXで附置

par(new = FALSE)
plot(x_S[,2],x_S[,3],xlim=c(min_x2,max_x2), ylim=c(min_x3,max_x3), col="black", pch=1,  xlab = "", ylab = "")
								# ｘ2 - x3の関係の描画　　　高評価グループを○で附置
par(new = TRUE)
plot(x_F[,2],x_F[,3],xlim=c(min_x2,max_x2), ylim=c(min_x3,max_x3), col="red", pch=4,  xlab = "x2", ylab = "x3")
								# ｘ2 - x3の関係の描画　　　低評価グループをXで附置

library(rgl)
plot3d(x_S[,1], x_S[,2], x_S[,3], xlim=c(min_x1,max_x1), ylim=c(min_x2,max_x2), zlim=c(min_x3,max_x3), xlab="x1", ylab="x2", zlab="x3", col=1, type="n") 
text3d(x_S[,1], x_S[,2], x_S[,3], col="black", "o") 
text3d(x_F[,1], x_F[,2], x_F[,3], col="red", "x") 
								# 3次元プロット
mean_全体　<- colMeans(x[,-4])
mean_全体

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

if (v[3] > 0)						#z軸の方向決定
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
								#zcの計算
zc 

x3_intercept <-  zc/v[3]				#x3切片の計算
x3_intercept


x1_cd <- seq(min_x1, max_x1, length = 50)		# x1軸の[min, max]区間を50分割座標の生成
x1_cd
x2_cd <- seq(min_x2, max_x2, length = 50)		# x2軸の[min, max]区間を50分割座標の生成
x2_cd

x1 <- matrix(0, 2500)
x2 <- matrix(0, 2500)
y <- matrix(0, 2500) 

for(i in 1:50) for(j in 1:50){x1[(i-1)*50+j] <- x1_cd[i];
x2[(i-1)*50+j] <- x2_cd[j];  y[(i-1)*50 + j] <-  x3_intercept - v[1]/v[3]*x1_cd[i] - v[2]/v[3]*x2_cd[j]}
								# x1-x2平面の50*50 = 2500の格子点上のｙの値の計算
plot3d(x1, x2, y, col="blue", pch=2, add = 1)	# 判別面の描画　（上で求めた2500点を附置
