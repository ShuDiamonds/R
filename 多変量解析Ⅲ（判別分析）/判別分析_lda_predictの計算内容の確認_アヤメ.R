par(mfrow=c(1,2)) 						# Divide R Graphics window to 1×2

x <- iris								# irisデータの読み込み

x <- subset(x, as.integer(x[,5]) > 1)			#　Versicolor と Virginica の抽出
x

plot(x[,3],x[,4],col=as.integer(x$Species), pch=as.integer(x$Species))

x_ver <- subset(x[,-5],  (as.integer(x$Species)) >1 & (as.integer(x$Species)) < 3)  
									#Versicolorの抽出

x_vir <- subset(x[,-5],  (as.integer(x$Species)) >2)  #Virginicaの抽出

library(MASS)
lda_out <- lda(as.integer(x$Species) ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, x)	#判別分析の実行
lda_out

iris_predict <- predict(lda_out)
iris_predict

v <- lda_out$scaling

mean_ver <- colMeans(x_ver)					#列ごとの平均を求める
n_ver <- nrow(x_ver)						#行数を求める

Wver <- scale(x_ver,center = TRUE, scale = FALSE)	#列ごとに平均０とする

mean_vir <- colMeans(x_vir)					#列ごとの平均を求める
n_vir <- nrow(x_vir)						#行数を求める

Wvir <- scale(x_vir,center = TRUE, scale = FALSE)	#列ごとに平均０とする

ve_ver <- (t(v) %*% t(Wver) %*% Wver %*% v)/(n_ver-1)	#veS^2の計算
ve_ver

ve_vir <- (t(v) %*% t(Wvir) %*% Wvir %*% v)/(n_vir-1)	#veF^2の計算
ve_vir

mean_zver <- t(v) %*% mean_ver				#zSの平均値の計算(a0 = 0)
mean_zver

mean_zvir <- t(v) %*% mean_vir				#zFの平均値の計算(a0 = 0)
mean_zvir


zc <- (mean_zver+mean_zvir)/2
zc

ve <- (ve_ver+ve_vir)/2
ve

f_z <- function(z, mean_z, ve){				#正規分布の定義
	f_zz <- exp(-(z-mean_z)^2/2/ve)
	return(f_zz)
	}


n <- 100								#データ数
col_num <- n*6							#出力行列の列数
result <- matrix(0, nrow=n,ncol=6)		#結果の出力用行列の定義
xu <- matrix(0, nrow=1, ncol=4)

for(i in 1:n){

	xu <- as.matrix(x[i,-5])				#　i行目のデータ（5列目は除く）を抽出

	zu <- xu %*% v						# 各データが判別面のどちら側にあるかを判定

	result[i,1] <- xu[1,3]
	result[i,2] <- xu[1,4]
	result[i,4] <- zu - zc

	result[i,5] <- f_z(zu, mean_zver, ve)/(f_z(zu, mean_zver, ve)+f_z(zu, mean_zvir,ve))
	result[i,6] <- f_z(zu, mean_zvir, ve)/(f_z(zu, mean_zver, ve)+f_z(zu, mean_zvir,ve))


	if (zu < zc){
		result[i,3] <- 2					#verグループ　　2:△
	}
	else{
		result[i,3] <- 3					#virグループ　　3:＋
	}
}

result

plot(result[,1],result[,2],col=result[,3], pch = result[,3])　　#判別結果のプロット

