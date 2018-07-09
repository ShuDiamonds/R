x_身体測定 <- read.csv("身体測定_身長_座高.csv")
x_身体測定

plot(x_身体測定$身長,x_身体測定$座高,col="red", pch=1)

XX <- as.matrix(x_身体測定)						#データフレームを行列へ変換
XX

n <- nrow(XX)								# 行数のカウント
n

S <- (1/(n-1))*t(XX) %*% XX　　　　　　　　				#S = X^t Xの計算
S

Eigen_value <- eigen(S)　　　　　　　　					# Sの固有値計算
Eigen_value

abline(0, Eigen_value$vectors[2,1]/Eigen_value$vectors[1,1])#第1主成分軸の描画
