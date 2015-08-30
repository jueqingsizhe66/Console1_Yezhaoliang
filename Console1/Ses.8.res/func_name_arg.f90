module func_arg_name
  Implicit None
!  write( * , * ) integral( x2 , 1.0 , 2.0 , 0.01 )
!  write( * , * ) 2.0**3/3 - 1.0**3/3
!  write( * , * ) integral( x3 , 1.0 , 2.0 , 0.01 )
!  write( * , * ) 2.0**4/4 - 1.0**4/4
contains

  Real Function x2( x )
  implicit none
    real :: x
    x2 = x*x
  End Function x2  
  
  Real Function x3( x )
  implicit none
    real :: x
    x3 = x*x*x
  End Function x3
  
  Real Function integral( func , low_bound , up_bound , delta ) result ( y )
    !Real , External :: func
  ! 我把它放进module之后 而不是放在program的contains 所以其实是不需要 书写Interface  为了安全起见 还是这样保留着
  implicit none
    !Interface 
    !  Real Function func( x )
    !    real :: x
    !  End Function func
    !End Interface
  real :: func
    Real , intent( IN ) :: low_bound , up_bound , delta
    integer :: i
    real :: x
    y = 0.0
    x = low_bound + delta/2.0
    Do !// Do x = low_bound , up_bound , delta
      y = y + func( x )
      x = x + delta
      if ( x > up_bound ) exit
    End Do
    y = y * delta
  End Function integral

End module func_arg_name

