    module My_Func
    Implicit None
    
    integer,private :: i 
  
    integer,public :: now_year = 2015 ! �������save �޷�����
   
    !Integer :: PrintCopyRight
    !  i = PrintCopyRight( "fcode.cn" , now_year )
    !  i = PrintCopyRight( "www.fcode.cn" , now_year )

    contains
    Integer Function PrintCopyRight( cDomain , now_year )
    Implicit None
    Character(len=*) , Intent( IN ) :: cDomain
    Integer , Intent( IN ) :: now_year
    write( * , * ) 'Powered by http://' , cDomain , now_year
    PrintCopyRight = now_year
    !return
    End Function PrintCopyRight
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!subroutine ��д֮��Ҳû����
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !subroutine PrintCopyRight( cDomain , now_year )
    !Implicit None
    !Character(len=*) , Intent( IN ) :: cDomain
    !Integer , Intent( IN ) :: now_year
    !write( * , * ) 'Powered by http://' , cDomain , now_year
    !!PrintCopyRight = now_year
    !!return
    !End subroutine PrintCopyRight
    
    end module My_Func