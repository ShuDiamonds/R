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
#plot3d(x_S[,1], x_S[,2], x_S[,3], xlim=c(min_x1,max_x1), ylim=c(min_x2,max_x2), zlim=c(min_x3,max_x3), xlab="x1", ylab="x2", zlab="x3", col=1, type="n") 
#text3d(x_S[,1], x_S[,2], x_S[,3], col="black", "o") 
#text3d(x_F[,1], x_F[,2], x_F[,3], col="red", "x") 
								# 3次元プロット

library(MASS)
lda_out <- lda(入社後の評価 ~ 学科+面接+実技, x)	#判別分析の実行
lda_out

v <- lda_out$scaling
v_norm <- sqrt(t(v) %*% v)
v <-  v/v_norm[1]						#ベクトルの規格化
v

if (v[3] > 0)						#z軸の方向決定
	{ v <- -v
	} 

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


n <- 8								#データ数
col_num <- n*4							#出力行列の列数
result <- matrix(1:col_num, nrow=n,ncol=4)		#結果の出力用行列の定義
xu <- matrix(1:3, nrow=1, ncol=3)

for(i in 1:n){

	xu <- as.matrix(x[i,-4])				#　i行目のデータ（4列目は除く）を抽出

	zu <- xu %*% v						# 各データが判別面のどちら側にあるかを判定

	result[i,1] <- xu[1,1]
	result[i,2] <- xu[1,2]
	result[i,3] <- xu[1,3]
	
	if (zu < zc){
		result[i,3] <- 1					#verグループ　　１:○
	}
	else{
		result[i,3] <- 2					#virグループ　　２:×
	}
}



plot3d(x_S[,1], x_S[,2], x_S[,3], xlim=c(min_x1,max_x1), ylim=c(min_x2,max_x2), zlim=c(min_x3,max_x3), xlab="x1", ylab="x2", zlab="x3", col=1, type="n") 
text3d(x_S[,1], x_S[,2], x_S[,3], col="black", "o") 
text3d(x_F[,1], x_F[,2], x_F[,3], col="red", "x") 
								# 3次元プロット

