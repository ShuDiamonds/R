par(mfrow=c(1,2)) 							# グラフ表示画面を1×2分割

x_身体測定 <- read.csv("身体測定_4変数.csv")
x_身体測定

plot(x_身体測定$身長,x_身体測定$体重,col="red", pch=1)
plot(x_身体測定$胸囲,x_身体測定$座高,col="red", pch=1)

x_normalized <- scale(x_身体測定, apply(x_身体測定, 2, mean), apply(x_身体測定, 2, sd))		#列ごとに平均0，分散1に正規化
x_normalized

XX <- as.matrix(x_normalized)						# データフレームを行列へ変換
XX

n <- nrow(XX)								# 行数のカウント
n

S <- (1/(n-1))*t(XX) %*% XX　　　　　　　　				# S = X^t Xの計算
S

Eigen_value <- eigen(S)　　　　　　　　					# Sの固有値計算
Eigen_value

princip_score <- XX %*% Eigen_value$vectors   			#主成分得点の計算
princip_score

princip_score_data = as.data.frame(princip_score)		#行列をデータフレームに変換
princip_score_data

plot(princip_score_data$V1, princip_score_data$V2,col="red", pch=1)
										#主成分得点の描画

Root_mean_eigen <- sqrt(Eigen_value$values)			#　固有値の平方根
Root_mean_eigen

princip_loading <- cbind(Root_mean_eigen[1] * Eigen_value$vectors[,1],  Root_mean_eigen[2] * Eigen_value$vectors[,2])  
princip_loading								# 第1，2主成分負荷量の計算 

princip_loading_data = as.data.frame(princip_loading)		#行列をデータフレームに変換
princip_loading_data

loading_label <- c(1:4)							# プロットのマーカー指定　1: 〇(身長), 2:△(体重), 3:+(胸囲), 4:×(座高)
loading_label
princip_loading_data = data.frame(princip_loading_data, loading_label)
princip_loading_data

plot(princip_loading_data$V1, princip_loading_data$V2,col=princip_loading_data$loading_label, pch=princip_loading_data$loading_label, xlim=c(-0.9, 0))	#主成分負荷量の描画

