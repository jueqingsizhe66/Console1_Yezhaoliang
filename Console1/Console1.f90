!  Console1.f90 
!
!  FUNCTIONS:
!  Console1 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Console1
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program Console1
    use fc_init
    use func_arg_name
    use isTest
    use usenamelist
    implicit none
    ! Variables

    ! Body of Console1
    print *, 'Hello World'

    print *,"Before call fc_update",a,b,c
    call fc_update()
    print *,"After call fc_update",a,b,c
    
    
    !! ��ʼ���� func_arg_name����ģ��
    
    
      write( * , * ) integral( x2 , 1.0 , 2.0 , 0.01 )
      write( * , * ) 2.0**3/3 - 1.0**3/3
    !  write( * , * ) integral( x3 , 1.0 , 2.0 , 0.01 )
    !  write( * , * ) 2.0**4/4 - 1.0**4/4
      
      
!!!!!! ��ʼ����isTest ģ��
!   write(*,*) is(5>3,"����","������")     !!!���ĺ���������
      write (*,*) is(6>3,'ok','not ok')
      write (*,*) is(6<3,'ok','not ok')
      write (*,*) is(6>3,34,8)
      write (*,*) is(6>3,45,8)
       
       
!!!!����ʼ����usenamelistģ��   
       call callNameList()
       call IntoNameList()
    end program Console1

    	