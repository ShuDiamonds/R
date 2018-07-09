par(mfrow=c(1,2)) 							#グラフ表示画面を1×2分割

xx <- data.frame(iris[,-5], as.integer(iris[,5]))　　		# setosa -> 1, versicolor -> 2, virginica -> 3
xx

x_ver_vir <- subset(xx,  xx[,5] > 1)				# versicolorとvirginicaの抽出
x_ver_vir

plot(x_ver_vir[,3],x_ver_vir[,4],col=x_ver_vir[,5], pch=x_ver_vir[,5]) #データのプロット

x_ver <- subset(xx[,-5], (xx[,5] > 1) & (xx[,5] <3))		# versicolorの抽出
x_ver

x_vir <- subset(xx[,-5], xx[,5] > 2)				# virginicaの抽出
x_vir

mean_ver <- colMeans(x_ver)						#列ごとの平均を求める
mean_ver

cov_ver <- cov(x_ver)							#分散・共分散行列を求める
cov_ver

S_inv_ver <- solve(cov_ver)						#ΣSの逆行列をもとめる
S_inv_ver

mean_vir <- colMeans(x_vir)						#列ごとの平均を求める
mean_vir

cov_vir <- cov(x_vir)							#分散・共分散行列を求める
cov_vir

S_inv_vir <- solve(cov_vir)						#ΣSの逆行列をもとめる
S_inv_vir


maha_dis <- function(X, mean_X, S_inv){				#マハラノビス距離関数の定義
	dis <- t(X-mean_X) %*% S_inv %*% (X - mean_X)
	return(dis)
	}

n <- 100									#データ数
col_num <- n*5								#出力行列の列数
result <- matrix(1:col_num, nrow=n,ncol=5)			#結果の出力用行列の定義
xy <- matrix(1:4, nrow=4, ncol=1)

for(i in 1:n){

	xy <- t(x_ver_vir[i,-5])					# ver, virデータを1セットずつ抽出

	md_ver <- maha_dis(xy,mean_ver,S_inv_ver)			# マハラノビス距離　d_ver(x, y)
	md_vir <- maha_dis(xy,mean_vir,S_inv_vir)			# マハラノビス距離　d_vir(x, y)

	result[i,1] <- xy[3]
	result[i,2] <- xy[4]
	result[i,3] <- md_ver
	result[i,4] <- md_vir
	
	if (md_ver < md_vir){
		result[i,5] <- 2						#verグループ　　2:△
	}
	else{
		result[i,5] <- 3						#virグループ　　3:＋
	}
}
result

plot(result[,1],result[,2],col=result[,5], pch = result[,5])　　#判別結果のプロット


