module usenamelist
    implicit none
    integer :: winBlade,winBlade1,Num,Num2
    integer,parameter :: slen=168
    Character(LEN=slen) :: task
    Character(LEN=64):: NameOfBlade!!��ָ����С Ĭ�ϵĻ���1���ֽ�
    namelist /windTurbine/ winBlade,Num,NameOfBlade
    namelist /windTurbine23/ winBlade1,Num2,NameOfBlade
    namelist /TOPTASK/ task
    
    
    contains
    !��ȡNML�ļ�
    subroutine callNameList()
    implicit none
    Character(len=30),parameter:: filename=trim('TestNameList/In.txt') !!!����parameter���벻ͨ�� �ַ�������
    Character(len=33),parameter:: filename12=trim('TestNameList/task.dat') !!!����parameter���벻ͨ�� �ַ�������
    
    open(32,FILE=filename)
    ! ��ȡwindTurbine���ݿ�
    !read(32,NML=windTurbine)
    ! ��ȡwindTurbine23���ݿ�
    read(32,NML=windTurbine23)
    write(*,*) 'WinBlade=', winBlade1,'Num=',Num2
    
    
       open(33,FILE=filename12)
    ! ��ȡwindTurbine���ݿ�
    !read(32,NML=windTurbine)
    ! ��ȡwindTurbine23���ݿ�
    read(33,NML=TOPTASK)
    write(*,*) 'Task=',task
    
    end subroutine callNameList
    
    !!! ��NML���� д���ļ�
    subroutine IntoNameList()
    implicit none
    Character(len=30),parameter:: filename='TestNameList/Out.txt' !!!����parameter���벻ͨ�� �ַ�������
    
    winBlade=10
    Num=30
    NameOfBlade="hfuk"
    open(33,FILE=filename)
    !read(32,NML=windTurbine)
    write(33,NML=windTurbine)
   
    
    end subroutine IntoNameList
end module usenamelist