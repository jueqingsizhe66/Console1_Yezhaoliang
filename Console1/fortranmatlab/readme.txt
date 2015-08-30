Fortran调用matlab，在这里实现，是通过系统调用了matlab程序，matlab程序运行了一个model模块。具体示例操作如下：
说明：实例用的是CVF+matlab7.1 不过相信都是一样的，因为代码跟编译器无关。
该实例来自网络，程序内容是用fortran自动生成一个随机矩阵，然后将矩阵的转置写入一个txt文件，然后用系统调用matlab程序，读取文件，求特征值，然后对特征值进行某变化后作图。更多关于matlab读取已有文件的知识可以找相关书籍参考。
1、Fortran中编写代码：
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
2、Matlab建立一个 test.m 文件，注意后缀，是model文件，编写为：
clear all; close all; clc;
M=load('matrix M.txt');
eigvalues=eig(M);
plot(real(eigvalues),imag(eigvalues),'-r*');
title('矩阵M的特征值');
xlabel('特征值实数部分');
ylabel('特征值虚数部分');
3、注意，test.m文件要放在编译完成的Fortran文件的工程文件夹下。Matlab软件一定是安装版的，这样系统才能调用。第一次安装后，需要重启电脑一次，系统才能确认自动调用matlab程序。
代码可以完全拷贝过去运行，没有问题。祝成功！


