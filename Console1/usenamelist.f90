module usenamelist
    implicit none
    integer :: winBlade,winBlade1,Num,Num2
    Character(LEN=64):: NameOfBlade!!��ָ����С Ĭ�ϵĻ���1���ֽ�
    namelist /windTurbine/ winBlade,Num,NameOfBlade
    namelist /windTurbine23/ winBlade1,Num2,NameOfBlade
    
    
    contains
    !��ȡNML�ļ�
    subroutine callNameList()
    implicit none
    Character(len=15),parameter:: filename="In.txt" !!!����parameter���벻ͨ�� �ַ�������
    
    open(32,FILE=filename)
    ! ��ȡwindTurbine���ݿ�
    !read(32,NML=windTurbine)
    ! ��ȡwindTurbine23���ݿ�
    read(32,NML=windTurbine23)
    write(*,*) 'WinBlade=', winBlade1,'Num=',Num2
   
    
    end subroutine callNameList
    
    !!! ��NML���� д���ļ�
    subroutine IntoNameList()
    implicit none
    Character(len=15),parameter:: filename="Out.txt" !!!����parameter���벻ͨ�� �ַ�������
    
    winBlade=10
    Num=30
    NameOfBlade="hfuk"
    open(33,FILE=filename)
    !read(32,NML=windTurbine)
    write(33,NML=windTurbine)
   
    
    end subroutine IntoNameList
end module usenamelist