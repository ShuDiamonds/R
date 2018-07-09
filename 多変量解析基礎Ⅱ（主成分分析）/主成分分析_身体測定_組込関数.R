par(mfrow=c(1,2)) 							# グラフ表示画面を2×2分割

x_身体測定 <- read.csv("身体測定_4変数.csv")
x_身体測定

plot(x_身体測定$身長,x_身体測定$体重,col="red", pch=1)
plot(x_身体測定$胸囲,x_身体測定$座高,col="red", pch=1)

x_normalized <- scale(x_身体測定, apply(x_身体測定, 2, mean), apply(x_身体測定, 2, sd))		#列ごとに平均0，分散1に正規化
x_normalized

XX <- as.matrix(x_normalized)						# データフレームを行列へ変換
XX

PC <- princomp(XX, cor=TRUE)
#biplotの描画
biplot(PC)


eigen_vectors <- unclass(loadings(PC))
eigen_vectors

PC$sd

princip_score <- XX %*% eigen_vectors	   			#主成分得点の計算
princip_score

princip_score_data = as.data.frame(princip_score)		#行列をデータフレームに変換
princip_score_data

plot(princip_score_data[,1], princip_score_data[,2],col="red", pch=1)
										#主成分得点の描画

princip_loading <- cbind(PC$sd[1] * eigen_vectors[,1],  PC$sd[2] * eigen_vectors[,2])  
princip_loading								# 第1，2主成分負荷量の計算 

princip_loading_data = as.data.frame(princip_loading)		#行列をデータフレームに変換
princip_loading_data

loading_label <- c(1:4)							# プロットのマーカー指定　1: 〇(身長), 2:△(体重), 3:+(胸囲), 4:×(座高)
loading_label
princip_loading_data = data.frame(princip_loading_data, loading_label)
princip_loading_data

plot(princip_loading_data$V1, princip_loading_data$V2,col=princip_loading_data$loading_label, pch=princip_loading_data$loading_label, xlim=c(-0.9, 0))	#主成分負荷量の描画

