clear all; close all; clc;
M=load('fortranmatlab/matrixM.txt');
eigvalues=eig(M);
plot(real(eigvalues),imag(eigvalues),'-r*');
title('�����ͼ');
xlabel('��������');
ylabel('ʵ������');

