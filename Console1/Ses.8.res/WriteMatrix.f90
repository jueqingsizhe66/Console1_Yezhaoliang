module WriteMatrix12
  Implicit None
  integer :: x(3,3) = reshape( &  ! &表示换行的作用
    [1,2,3, &
     4,5,6, &
     7,8,9],[3,3])  !!!reshape 这边的效果相当于是转秩
     
!  call WriteMatrix( x(1:2,3:3) )
  
contains

  Subroutine WriteMatrix( iMat )
    implicit none
    Integer , Intent( IN ) :: iMat(:,:)
    integer :: i
    Do i = 1 , size( iMat , dim = 2 ) ! dim=2 第二维的数据
      write( * , * ) iMat( : , i )    ! 整列读取
    End Do
  End Subroutine WriteMatrix

End module WriteMatrix12