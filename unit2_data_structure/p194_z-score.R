# �E(�ƭ�-������)/�зǮt
# �E�N�ƭȼзǤƬ������Ȭ�0�A�зǮt��1���ƦC
# �E���������Ȫi�ʪ��v�T
# �E�ھڱ`�A������z
# �V��95%���ר�Z-score�O�����1.96����
# �V��99%���ר�Z-score�O�����3����
# �V�i�H�ھڦ���h�w�q���ݭ�


# scale�Ѽ� center=T���ܥΥ����ȭp��Ascale=F���ܤ����H�зǮt
data <- c(1,2,3,6,3)
scale(data,center = T, scale = F)
scale(data,center = T, scale = T)

a <- scale(data,center = T, scale = T)
attributes(a)
attributes(a)$'scaled:center'