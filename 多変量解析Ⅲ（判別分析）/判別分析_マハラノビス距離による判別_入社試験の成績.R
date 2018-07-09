x <- read.csv("C:/Users/Furuhashi/Documents/Discriminant_Analysis/test_for_employment2.csv")　#入社試験の成績データの読み込み
x

x_S <- subset(x[,-3],  x$入社後の評価 < 1.5)		# 高評価グループの抽出
x_S

x_F <- subset(x[,-3],  x$入社後の評価 > 1.5)		# 低評価グループの抽出
x_F

mean_S <- colMeans(x_S)					#列ごとの平均を求める
mean_S

var_S <- cov(x_S)						#分散・共分散行列を求める
var_S

mean_F <- colMeans(x_F)					#列ごとの平均を求める
mean_F

var_F <- cov(x_F)						#分散・共分散行列を求める
var_F

rho_S <- cor(x_S[,1],x_S[,2])				#相関係数の計算
rho_S

rho_F <- cor(x_F[,1],x_F[,2])				#相関係数の計算
rho_F




maha_dis <- function(x, y, mean_x, mean_y, var_x, var_y, rho){				#マハラノビス距離関数の定義
	dis <- 1/(1-rho^2)*((x-mean_x)^2/var_x-2*rho*(x-mean_x)*(y-mean_y)/sqrt(var_x*var_y)+(y-mean_y)^2/var_y)
	return(dis)
	}

n <- 1000							#生成乱数の数
col_num <- n*5						#出力行列の列数
result <- matrix(0, nrow=n,ncol=5)	#結果の出力用行列の定義
ran <- matrix(0, nrow = 2, ncol = 1)

for(i in 1:n){
	ran[1] <- runif(1)*8				#一様乱数の生成 x in [0, 8]
	ran[2] <- runif(1)*9-3				#一様乱数の生成 y in [-3, 6]
	md_S <- maha_dis(ran[1],ran[2],mean_S[1],mean_S[2],var_S[1,1],var_S[2,2],rho_S)
								# マハラノビス距離　dS(x, y)
	md_F <- maha_dis(ran[1],ran[2],mean_F[1],mean_F[2],var_F[1,1],var_F[2,2],rho_F)
								# マハラノビス距離　dF(x, y)
	result[i,1] <- ran[1]
	result[i,2] <- ran[2]
	result[i,3] <- md_S
	result[i,4] <- md_F
	
	if (md_S < md_F){
		result[i,5] <- 5				#高評価グループ　　1:◇
	}
	else{
		result[i,5] <- 7				#低評価グループ　　2:□
	}
}
result

plot(result[,1],result[,2],col=result[,5], pch = result[,5])　　#判別結果のプロット
par(new = "T")
plot(x[,1],x[,2],xlim = c(0,8),ylim = c(-3, 6), col=x$入社後の評価, pch=x$入社後の評価)



