
    module optionalP
  Implicit None
!  call sub( 7 , 6 )
!  call sub( 5 , 4 , 3 )
contains

  Subroutine sub( a , b , c )
    implicit none
    Integer :: a , b
    Integer , optional :: c
    write( * , * ) a , b
    if ( present( c ) ) write(*,*) 'optional:' , c
  End Subroutine sub

End module optionalP

