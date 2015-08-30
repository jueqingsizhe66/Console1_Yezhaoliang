    !  Console1.f90
    !*Author : Ye Zhaoliang
    !*Date   :
    !*Email  : zhaoturkkey@163.com
    !*Version: V1.0
    !*Purpose:
    !*Input  :
    !*Usage  :
    !**************************************************************
    program Console1
    use fc_init
    use func_arg_name
    use isTest
    use usenamelist
    use My_Func,only :this_year=>now_year,PrintCopyRight
    use STUDENTCLASS
    use optionalP
    use typedef
    use WriteMatrix12
    use SecMod
    use Grid_Mod
    use key_func
    use GUST
    implicit none

    INTEGER :: i
    !!**************************************
    !* StudentClass
    !!**************************************

    TYPE(STUDENT) :: LiMing


    !!**************************************
    !* typedef
    !!***************************************
    type(ST_Degree) ::st
    real(kind=DP),parameter :: r = 9.34234

    !!**************************************
    !* secMod
    !!***************************************
    Type( ST_Sec ) :: st1 = ST_Sec(100,22,20)
    Type( ST_Sec ) :: st2 = ST_Sec(100,20,10)
    !!**************************************
    !* Grid_Mod
    !!***************************************
    logical :: bGrid
    
    !!**************************************
    !* func
    !!***************************************
    REAL, PARAMETER :: pi = acos(-1.)
    CHARACTER (LEN=:), ALLOCATABLE :: keywd
    REAL, ALLOCATABLE :: pars(:)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Variables

    ! Body of Console1
    print *, 'Hello World'

    print *,"Before call fc_update",a,b,c
    call fc_update()
    print *,"After call fc_update",a,b,c


    !!**************************************
    !* ��ʼ���� func_arg_name����ģ��
    !!**************************************

    write( * , * ) integral( x2 , 1.0 , 2.0 , 0.01 )
    write( * , * ) 2.0**3/3 - 1.0**3/3
    !  write( * , * ) integral( x3 , 1.0 , 2.0 , 0.01 )
    !  write( * , * ) 2.0**4/4 - 1.0**4/4
    !!Todo Test

    !!**************************************
    !* ��ʼ����isTest ģ��
    !!**************************************

    !   write(*,*) is(5>3,"����","������")     !!!���ĺ���������
    write (*,*) is(6>3,'ok','not ok')
    write (*,*) is(6<3,'ok','not ok')
    write (*,*) is(6>3,34,8)
    write (*,*) is(6>3,45,8)

    !!**************************************
    !*  ��ʼ����usenamelistģ��
    !!**************************************

    call callNameList()
    call IntoNameList()

    !!**************************************
    !* ��ʼ���� Funcģ��
    !!**************************************

    i=PrintCopyRight("fcode.cn",this_year)
    !call PrintCopyRight("fcode.cn",now_year)
    ! print *,today

    !!**************************************
    !* ��ʼ���� STUDENTCLASSģ��
    !!**************************************

    Liming = STUDENT(3,"Liming",69,95,88)
    call PRINT_STUDENT_INFORTMATION(Liming)

    !!**************************************
    !*  ��ʼ���� optionalPģ��
    !!***************************************
    call sub(7,6)
    call sub(7,6,3)


    !!**************************************
    !* ��ʼ����typedefģ��� С��ת��Ϊ�ȷ���
    !!***************************************
    st = RDegree_To_TypeDegree(r)
    write(*,*) r,"==>",st

    !!**************************************
    !* WriteMatrixд��������
    !!***************************************
    call WriteMatrix(x(1:2,3:3))

    !!**************************************
    !* ��ʼ���� SecMod
    !!***************************************
    write(*,'(a,g0,a)') 'between: ',st2-st1,' uSec'

    !!**************************************
    !* ��ʼ����Grid_mod ����ʡ�ǲ���
    !!***************************************
    bGrid = Grid_Alloc( 5 , 5 )
    if ( .not.b ) stop "cann't allocate"
    write( * , '(a,g0,"x",g0)' ) 'Grid Size: ' , grd_stInfo
    !grd_stInfo%m = 50 !// ����  !!��Ϊ��protected�����޷��޸� ֻ�ܶ�ȡ
    call FillGrid()
    write( * , * ) 'Grid: '
    Do i = 1 , grd_stInfo%n
        write( * , '(999f6.2)' ) grd_rData( : , i )
    End Do
    bGrid = Grid_Dealloc()
    
    !!**************************************
    !* ��ʼ���� funcģ��
    !!***************************************
    DO
        ! Try input the following strings in the commandline:
        ! cir 20.
        ! sqr 10.
        ! rect 1. 2.
        CALL readcmd(keywd, pars)
        
        IF (keywd=='end') THEN
            EXIT
        ELSE IF (keywd=='cir' .OR. keywd=='sqr') THEN
            IF (size(pars) /= 1) THEN
                WRITE (*, *) 'Incorrect number of parameters.'
                CYCLE
            END IF
        ELSE IF (keywd=='rect') THEN
            IF (size(pars) /= 2) THEN
                WRITE (*, *) 'Incorrect number of parameters.'
                CYCLE
            END IF
        ELSE
            WRITE (*,*) 'Keyword unknown...'
            CYCLE
        END IF
            
        IF (keywd=='cir') THEN
            WRITE (*, *) 'Area = ', pi*pars(1)**2.
        ELSE IF (keywd=='sqr') THEN
            WRITE (*, *) 'Area = ', pars(1)**2.
        ELSE IF (keywd=='rect') THEN
            WRITE (*, *) 'Area = ', pars(1)*pars(2)
        END IF
        
    END DO

    !!**************************************
    !* ��ʼ�������ģ��
    !!***************************************
    call generateGust()
    end program Console1

