module WriteMatrix12
  Implicit None
  integer :: x(3,3) = reshape( &
    [1,2,3, &
     4,5,6, &
     7,8,9],[3,3])
     
!  call WriteMatrix( x(1:2,3:3) )
  
contains

  Subroutine WriteMatrix( iMat )
    implicit none
    Integer , Intent( IN ) :: iMat(:,:)
    integer :: i
    Do i = 1 , size( iMat , dim = 2 )
      write( * , * ) iMat( : , i )
    End Do
  End Subroutine WriteMatrix

End module WriteMatrix12