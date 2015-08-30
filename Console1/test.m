clear all; close all; clc;
M=load('fortranmatlab/matrixM.txt');
eigvalues=eig(M);
plot(real(eigvalues),imag(eigvalues),'-r*');
title('矩阵M的特征值');
xlabel('特征值实数部分');
ylabel('特征值虚数部分');

