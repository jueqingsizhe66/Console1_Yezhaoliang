module usenamelist
    implicit none
    integer :: winBlade,winBlade1,Num,Num2
    integer,parameter :: slen=168
    Character(LEN=slen) :: task
    Character(LEN=64):: NameOfBlade!!得指定大小 默认的话是1个字节
    namelist /windTurbine/ winBlade,Num,NameOfBlade
    namelist /windTurbine23/ winBlade1,Num2,NameOfBlade
    namelist /TOPTASK/ task
    
    
    contains
    !读取NML文件
    subroutine callNameList()
    implicit none
    Character(len=30),parameter:: filename=trim('TestNameList/In.txt') !!!不加parameter编译不通过 字符串常量
    Character(len=33),parameter:: filename12=trim('TestNameList/task.dat') !!!不加parameter编译不通过 字符串常量
    
    open(32,FILE=filename)
    ! 读取windTurbine数据块
    !read(32,NML=windTurbine)
    ! 读取windTurbine23数据块
    read(32,NML=windTurbine23)
    write(*,*) 'WinBlade=', winBlade1,'Num=',Num2
    
    
       open(33,FILE=filename12)
    ! 读取windTurbine数据块
    !read(32,NML=windTurbine)
    ! 读取windTurbine23数据块
    read(33,NML=TOPTASK)
    write(*,*) 'Task=',task
    
    end subroutine callNameList
    
    !!! 把NML数据 写入文件
    subroutine IntoNameList()
    implicit none
    Character(len=30),parameter:: filename='TestNameList/Out.txt' !!!不加parameter编译不通过 字符串常量
    
    winBlade=10
    Num=30
    NameOfBlade="hfuk"
    open(33,FILE=filename)
    !read(32,NML=windTurbine)
    write(33,NML=windTurbine)
   
    
    end subroutine IntoNameList
end module usenamelist