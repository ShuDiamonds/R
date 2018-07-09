par(mfrow=c(1,2)) 						# Divide R Graphics window to 1×2

x <- iris								# irisデータの読み込み

x <- subset(x, as.integer(x[,5]) > 1)			#　Versicolor と Virginica の抽出
x

plot(x[,3],x[,4],col=as.integer(x$Species), pch=as.integer(x$Species))

x_ver <- subset(x[,-5],  (as.integer(x$Species)) >1 & (as.integer(x$Species)) < 3)  
x_ver									#Versicolorの抽出

x_vir <- subset(x[,-5],  (as.integer(x$Species)) >2)  #Virginicaの抽出
x_vir

library(MASS)
lda_out <- lda(as.integer(x$Species) ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, x)	#判別分析の実行
lda_out

v <- lda_out$scaling
v_norm <- sqrt(t(v) %*% v)
v <-  v/v_norm[1]							#ベクトルの規格化
v

if (v[4] > 0)							#z軸の方向決定
	{ v <- -v
	} 

num_ver = nrow(x_ver)
num_vir = nrow(x_vir)

mean_ver <- colMeans(x_ver)					#列ごとの平均を求める
mean_ver

Wver <- scale(x_ver,center = TRUE, scale = FALSE)	#列ごとに平均０とする
Wver

mean_vir <- colMeans(x_vir)					#列ごとの平均を求める
mean_vir


Wvir <- scale(x_vir,center = TRUE, scale = FALSE)	#列ごとに平均０とする
Wvir


ve_ver <- (t(v) %*% t(Wver) %*% Wver %*% v)/(num_ver-1)	#veS^2の計算
ve_ver

ve_vir <- (t(v) %*% t(Wvir) %*% Wvir %*% v)/(num_vir-1)	#veF^2の計算
ve_vir

mean_zver <- t(v) %*% mean_ver				#zSの平均値の計算(a0 = 0)
mean_zver

mean_zvir <- t(v) %*% mean_vir				#zFの平均値の計算(a0 = 0)
mean_zvir

zc <- (sqrt(ve_vir)*mean_zver + sqrt(ve_ver)*mean_zvir)/(sqrt(ve_vir) + sqrt(ve_ver))	
									#zcの計算
zc 

n <- 100								#データ数
col_num <- n*3							#出力行列の列数
result <- matrix(1:col_num, nrow=n,ncol=3)		#結果の出力用行列の定義
xu <- matrix(1:4, nrow=1, ncol=4)

for(i in 1:n){

	xu <- as.matrix(x[i,-5])				#　i行目のデータ（5列目は除く）を抽出

	zu <- xu %*% v						# 各データが判別面のどちら側にあるかを判定

	result[i,1] <- xu[1,3]
	result[i,2] <- xu[1,4]
	
	if (zu > zc){
		result[i,3] <- 2					#verグループ　　2:△
	}
	else{
		result[i,3] <- 3					#virグループ　　3:＋
	}
}

plot(result[,1],result[,2],col=result[,3], pch = result[,3])　　#判別結果のプロット

