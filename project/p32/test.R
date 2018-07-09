#�f�[�^��ǂ݂���
body.data <- read.csv("body_sample.csv",header=T,stringsAsFactors=F)

# ���C�u�����̓ǂݍ���
library(ggplot2)

#�v��f�[�^�\��
summary(body.data)
#�����ɍ������f�[�^��\��
body.data[body.data$height,]
#�W���΍��̕\��
sd(body.data$height)
#�q�X�g�O�����̕\��
ggplot(body.data,aes(x=height,y=weight))+
geom_histogram()+
theme_bw(16)+
ylab("coun"t)
# �f�[�^�ƑS�̐ݒ��������ggplot�I�u�W�F�N�g�����
ggplot(body.data,aes(x=height,y=weight,col=gender))+
geom_point()+
theme_bw(16)+
geom_smooth(method=glm)
#���֌W���\��
cor(body.data$height,body.data$weight,method="spearman")
#���֌���
cor.test(body.data$height,body.data$weight,method="pearson")



# ���C�u�����̓ǂݍ���
library(ggplot2)

# �f�[�^�ƑS�̐ݒ��������ggplot�I�u�W�F�N�g�����
gp = ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, colour=Species))


# �O���t�̃��C���[���d�˂�
gp = gp + geom_point(size=3, alpha=0.7)

# �`�悵�Ă݂�
print(gp)

# ����ɕύX���d�˂�
gp = gp + geom_smooth(method=glm, family=gaussian)
gp = gp + labs(title="�Ԃ̎�ށFIris Sepal")
#gp = gp + geom_histogram(fill=..count..)
#gp = gp + geom_density(alpha = 0.2)
print(gp)