module isTest
 implicit none
interface is
 module procedure is_REAL,is_INT,is_String
end interface

    contains 
!!!!!!!!!!!!!!!!!!!!!!!!
!! 对整数进行判断 并选择……
!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
integer function is_INT(logic, res1,res2)
implicit none
!logical,intent(in) :: x
logical,intent(in) :: logic
integer :: res1,res2
if(logic) then
is_INT=res1
else
    is_INT=res2
end if  !!! end  if 而不是 end

end function is_INT  !  最好加上函数结尾  不是最好而是必须加上

real function  is_REAL(logic, res1,res2)
implicit none
!logical,intent(in) :: x
logical,intent(in) :: logic
real  :: res1,res2
!real kind=8)  :: res1,res2  改成这中精度  对应返回值

if(logic) then
is_REAL=res1 !!!! 这是区分is_INT的地方2
else
is_REAL=res2
endif
end function is_REAL
!!! character(dimension)
!character(len=30) function  is_String( x, res1,res2)
!character function  is_String( x, res1,res2)
character(LEN=64) function  is_String( logic, res1,res2)
implicit none
!logical,intent(in) :: x
logical,intent(in) :: logic
!character(len=30):: res1,res2
!character:: res1,res2
character(len=*):: res1,res2
if(logic) then
is_String=res1
else
is_String=res2
endif
end function is_String

end module isTest