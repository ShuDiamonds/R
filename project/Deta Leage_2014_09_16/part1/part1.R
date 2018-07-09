#�d��A���͂ɂ���
#http://itbc-world.com/home/rfm/r%E3%81%AE%E7%B5%B1%E8%A8%88%E9%96%A2%E6%95%B0/%E9%87%8D%E5%9B%9E%E5%B8%B0%E5%88%86%E6%9E%90/

head(iris)
#iris�̑��֍s���\��
round(cor(iris[, 1:4]),4)

plot(iris, panel=panel.smooth)

#Petal.Width�ɑ΂��ďd��A���͂��P��ڂ���S��ڂ��g���čs��
data1 <- lm( iris$Petal.Width~., data = iris[, 1:4] )

summary(data1)

#iris �̃f�[�^�Q�̌����� x �Ƃ��܂��B
length( iris$Petal.Width )
x <- 1:150
#iris ����e�f�[�^�Q�̒l���擾���āA�ϐ��Ɋi�[���܂�
x1 <- iris$Sepal.Length
x2 <- iris$Sepal.Width
x3 <- iris$Petal.Length

#��A���ʂœ���ꂽ�W���ƐؕЂ��g�p���ă��f�����쐬���܂��B
y <- -0.2073*x1 + 0.2228*x2 + 0.5241*x3 - 0.2403

plot( x,iris$Petal.Width, type="l", ylim=c(0, 3) ) 
par( new=T )
plot( x, y, type="l", col="blue", ylim=c(0, 3), ylab="" )

#���x�́A�J�e�S���ϐ���species���g���ďd��A���͂���
data2 <- lm( iris$Petal.Width~., data = iris )
data2
summary(data2)

x1 <- iris$Sepal.Length
x2 <- iris$Sepal.Width
x3 <- iris$Petal.Length
x4 <- c( rep(0:0, 50), rep(1:1, 50), rep(0:0, 50) )
x5 <- c( rep(0:0, 50), rep(0:0, 50), rep(1:1, 50) )


y2 <- -0.09293*x1 + 0.24220*x2 + 0.24220*x3 +0.64811*x4 + 1.04637*x5 - 0.47314
plot( x,iris$Petal.Width, type="l", ylim=c(0, 3) ) 
par( new=T )
plot( x, y, type="l", col="blue", ylim=c(0, 3), ylab="" )
par( new=T )
plot( x, y2, type="l", col="red", ylim=c(0, 3), ylab="" )
 
#�X�e�b�v���C�Y�A���S���Y�����g�p����
step(data2)

par( mfrow = c(2, 2) )  
plot( data1 )

par( mfrow = c(2, 2) )  
plot( data2 )

