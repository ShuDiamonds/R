#�d��A���̗͂��K�Q
#�d��A���͂ƕϐ��̑I�����s���B
#URL�@http://www1.doshisha.ac.jp/~mjin/R/14.html

#�ϐ��̐���
#rating�F�@�@�����]��
#complaints�F�]�ƈ��̋��̎�舵��
#privileges�F���ʂȓ����͋����Ȃ�
#learning�F�@�w�K�̋@��
#raises�F�@�@�\�͊�Â�������
#critical�F�@���d
#advancel:�@ ���i


head(attitude)
#���֌W���̕\��
round(cor(attitude),2)
#�v���b�g
pairs(attitude,panel=panel.smooth,attitude)
#rating�ɑ΂��đS�f�[�^������ϐ��ł̏d��A���͎��s
attitudelm1 <- lm(rating ~ ., data = attitude)
summary(attitudelm1)
#step�֐��ōœK�Ȑ����ϐ���I��
attitudelm2<-step(attitudelm1)
summary(attitudelm2)

plot(attitudelm2,col="blue")

