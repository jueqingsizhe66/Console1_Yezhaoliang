clear all; close all; clc;
M=load('fortranmatlab/matrixM.txt');
eigvalues=eig(M);
plot(real(eigvalues),imag(eigvalues),'-r*');
title('矩阵绘图');
xlabel('虚数部分');
ylabel('实数部分');

