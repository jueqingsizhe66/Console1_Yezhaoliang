Fortran����matlab��������ʵ�֣���ͨ��ϵͳ������matlab����matlab����������һ��modelģ�顣����ʾ���������£�
˵����ʵ���õ���CVF+matlab7.1 �������Ŷ���һ���ģ���Ϊ������������޹ء�
��ʵ���������磬������������fortran�Զ�����һ���������Ȼ�󽫾����ת��д��һ��txt�ļ���Ȼ����ϵͳ����matlab���򣬶�ȡ�ļ���������ֵ��Ȼ�������ֵ����ĳ�仯����ͼ���������matlab��ȡ�����ļ���֪ʶ����������鼮�ο���
1��Fortran�б�д���룺
program main
implicit none
integer,parameter :: rows=100, cols=100
real*8 :: M(rows,cols)
integer :: i
call random_seed() 
call random_number(M)
open(100,file='matrix M.txt')
write(100,'(<cols>E25.15)') transpose(M)
close(100)
call system("matlab -r test") 
end program main
2��Matlab����һ�� test.m �ļ���ע���׺����model�ļ�����дΪ��
clear all; close all; clc;
M=load('matrix M.txt');
eigvalues=eig(M);
plot(real(eigvalues),imag(eigvalues),'-r*');
title('����M������ֵ');
xlabel('����ֵʵ������');
ylabel('����ֵ��������');
3��ע�⣬test.m�ļ�Ҫ���ڱ�����ɵ�Fortran�ļ��Ĺ����ļ����¡�Matlab���һ���ǰ�װ��ģ�����ϵͳ���ܵ��á���һ�ΰ�װ����Ҫ��������һ�Σ�ϵͳ����ȷ���Զ�����matlab����
���������ȫ������ȥ���У�û�����⡣ף�ɹ���


