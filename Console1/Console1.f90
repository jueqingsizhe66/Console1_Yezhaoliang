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
    use fmatlab
    use MyReadModule
    use FFT_Mod
    use John_Complex
    use JohnTimeStamp  ! ��timestamp()����������ģ�鶼����ʹ��
    use f90split
    use geometry
    use linearStability
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

    !!**************************************
    !* MyReadModule
    !!***************************************
    integer :: n
    integer, allocatable   :: arrayMy(:)

    !!**************************************
    !* ��ʼ����FFT_Mod
    !* ������DP ��x����
    !!***************************************
    integer,parameter :: FFT_ModN=8
    complex(KIND= FFT_ModDP) :: FFT_MODx(FFT_ModN) =[36.d0,21.d0,33.d0,44.d0,55.d0,63.d0,73.d0,38.d0],FFT_ModOriginal(FFT_ModN)
    complex(KIND=FFT_ModDP) :: temp
    !!!complex �������͵Ķ���

    ! Variables

    ! Body of Console1
    ! print *, 'Hello World'
    !
    ! print *,"Before call fc_update",a,b,c
    ! call fc_update()
    ! print *,"After call fc_update",a,b,c
    !
    !
    ! !!**************************************
    ! !* ��ʼ���� func_arg_name����ģ��
    ! !!**************************************
    !
    ! write( * , * ) integral( x2 , 1.0 , 2.0 , 0.01 )
    ! write( * , * ) 2.0**3/3 - 1.0**3/3
    ! !  write( * , * ) integral( x3 , 1.0 , 2.0 , 0.01 )
    ! !  write( * , * ) 2.0**4/4 - 1.0**4/4
    ! !!Todo Test
    !
    ! !!**************************************
    ! !* ��ʼ����isTest ģ��
    ! !!**************************************
    !
    ! !   write(*,*) is(5>3,"����","������")     !!!���ĺ���������
    ! write (*,*) is(6>3,'ok','not ok')
    ! write (*,*) is(6<3,'ok','not ok')
    ! write (*,*) is(6>3,34,8)
    ! write (*,*) is(6>3,45,8)
    !
    ! !!**************************************
    ! !*  ��ʼ����usenamelistģ��
    ! !!**************************************
    !
    ! write(*,*) '��ʼ����namelist'
    ! call callNameList()
    ! call IntoNameList()
    !
    ! !!**************************************
    ! !* ��ʼ���� Funcģ��
    ! !!**************************************
    !
    ! i=PrintCopyRight("fcode.cn",this_year)
    ! !call PrintCopyRight("fcode.cn",now_year)
    ! ! print *,today
    !
    ! !!**************************************
    ! !* ��ʼ���� STUDENTCLASSģ��
    ! !!**************************************
    !
    ! Liming = STUDENT(3,"Liming",69,95,88)
    ! call PRINT_STUDENT_INFORTMATION(Liming)
    !
    ! !!**************************************
    ! !*  ��ʼ���� optionalPģ��
    ! !!***************************************
    ! call sub(7,6)
    ! call sub(7,6,3)
    !
    !
    ! !!**************************************
    ! !* ��ʼ����typedefģ��� С��ת��Ϊ�ȷ���
    ! !!***************************************
    ! st = RDegree_To_TypeDegree(r)
    ! write(*,*) r,"==>",st
    !
    ! !!**************************************
    ! !* WriteMatrixд��������
    ! !!***************************************
    ! call WriteMatrix(x(1:2,3:3))
    !
    ! !!**************************************
    ! !* ��ʼ���� SecMod
    ! !!***************************************
    ! write(*,'(a,g0,a)') 'between: ',st2-st1,' uSec'
    !
    ! !!**************************************
    ! !* ��ʼ����Grid_mod ����ʡ�ǲ���
    ! !!***************************************
    ! bGrid = Grid_Alloc( 5 , 5 )
    ! if ( .not.b ) stop "cann't allocate"
    ! write( * , '(a,g0,"x",g0)' ) 'Grid Size: ' , grd_stInfo
    ! !grd_stInfo%m = 50 !// ����  !!��Ϊ��protected�����޷��޸� ֻ�ܶ�ȡ
    ! call FillGrid()
    ! write( * , * ) 'Grid: '
    ! Do i = 1 , grd_stInfo%n
    !     write( * , '(999f6.2)' ) grd_rData( : , i )
    ! End Do
    ! bGrid = Grid_Dealloc()
    !
    ! !!**************************************
    ! !* ��ʼ���� funcģ��
    ! !!***************************************
    ! DO
    !     ! Try input the following strings in the commandline:
    !     ! cir 20.
    !     ! sqr 10.
    !     ! rect 1. 2.
    !     CALL readcmd(keywd, pars)
    !
    !     IF (keywd=='end') THEN
    !         EXIT
    !     ELSE IF (keywd=='cir' .OR. keywd=='sqr') THEN
    !         IF (size(pars) /= 1) THEN
    !             WRITE (*, *) 'Incorrect number of parameters.'
    !             CYCLE
    !         END IF
    !     ELSE IF (keywd=='rect') THEN
    !         IF (size(pars) /= 2) THEN
    !             WRITE (*, *) 'Incorrect number of parameters.'
    !             CYCLE
    !         END IF
    !     ELSE
    !         WRITE (*,*) 'Keyword unknown...'
    !         CYCLE
    !     END IF
    !     ! �����ǰ�ȫ������  �����ǵ�������ԺϷ���
    !     ! keyword�������  ��ȻҲ���Է������棡
    !     IF (keywd=='cir') THEN
    !         WRITE (*, *) 'Area = ', pi*pars(1)**2.
    !     ELSE IF (keywd=='sqr') THEN
    !         WRITE (*, *) 'Area = ', pars(1)**2.
    !     ELSE IF (keywd=='rect') THEN
    !         WRITE (*, *) 'Area = ', pars(1)*pars(2)
    !     END IF
    !
    ! END DO
    !
    ! !!**************************************
    ! !* ��ʼ�������ģ��
    ! !!***************************************
    ! call generateGust()
    !
    !
    ! !!**************************************
    ! !* ��ʼ���� fortran�������� ����matlab���л�ͼ
    ! !!***************************************
    ! !call generateDataForMatlab()
    !
    ! !!**************************************
    ! !* ��ʼ���� MyReadModule
    ! !!***************************************
    ! call readFile(n,arrayMy)
    ! write(*,*) (arrayMy(i),i=1,n)
    !
    ! !!**************************************
    ! !* ��ʼ����FFT_MOD
    ! !!***************************************
    ! write(*,*) 'Original Input Data'
    ! Do i = 1 , FFT_ModN
    !     Write (*, *) FFT_MODx(i)
    ! End Do
    ! FFT_ModOriginal =FFT_MODx  !��ֵ ����ԭʼֵ
    ! ! ��������FFT
    ! Call fcFFT( FFT_MODx , FFT_Forward )
    ! Write(*,*) 'FFT_Forward:'
    ! Do i = 1 , FFT_ModN
    !     Write (*, *) FFT_MODx(i)
    ! End Do
    !
    ! Write(*,*) 'FFT_Backward:'
    ! Call fcFFT(  FFT_MODx , FFT_Inverse )
    ! Do i = 1 , FFT_ModN
    !     temp =FFT_MODx(i)/FFT_ModN
    !     Write (*, *)  temp,'-->',(abs(FFT_ModOriginal(i))-abs(temp))/abs(FFT_ModOriginal(i))!,'%'
    ! End Do
    !
    ! write(*,*) '��ͣһС�� �ո����'
    ! read(*,*)
    !! call system("pause")  !�ӽ�ȥ������
    ! !!**************************************
    ! !* ��ʼ����JohnComplexģ��
    ! !!***************************************
    !call timestamp ( )
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'COMPLEX_NUMBERS:'
    !write ( *, '(a)' ) '  FORTRAN90 version.'
    !write ( *, '(a)' ) '  Demonstrate complex number usage.'
    !!
    !!  Single precision complex.
    !!
    !call test01 ( )
    !call test02 ( )
    !call test03 ( )
    !!
    !!  Double precision complex.
    !!
    !call test04 ( )
    !call test05 ( )
    !call test06 ( )
    !!
    !!  Terminate.
    !!
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'COMPLEX_NUMBERS:'
    !write ( *, '(a)' ) '  Normal end of execution.'
    !
    !write ( *, '(a)' ) ' '
    !call timestamp ( )

    !!**************************************
    !*  ����F90Split
    !!***************************************
    !write(*,*) '��ʼ����F90Split'
    !read(*,*)
    !call start()
    !open(67,FILE="filegeometry.txt")
    !open(68,FILE="filegeometryPrb.txt")
    !open(69,FILE="functiongeometry.txt")
    !open(70,FILE="functiongeometryPrb.txt")
    !write(67,*) "#!/bin/bash -f"
    !do i = 1 ,size(storeFunction)
    !    write(67,'(A80)') storeFunctionGeo(i)
    !    write(68,'(A80)') storeFunctionPrb(i)
    !    write(69,'(A120)') FunctionGeo(i)
    !    write(70,'(A120)') FunctionPrb(i)
    !end do

    !!!ֻҪ�ѽ�������cygwin�Ϳ���ɾ������Ҫ�ĺ���������
    !F90SPLIT:
    !  What is the name of the input file?
    !geometry/geometry.f90
    !
    !F90SPLIT:
    !  Reached end of geometry/geometry.f90
    !  Lines read:                 34516
    !  Longest line length:          113
    !  Longest line location:      34213
    !  Named modules created:         59
    ! 5 September 2015   7:11:48.804 PM
    !�밴���������. . .
    !!**************************************
    !*  ����Area
    !!***************************************
    write(*,*) 'Area=',Area(3.0,4.0,5.0)

    !
    !!**************************************
    !* Test LinearStability
    !!*************************************** 
    call GetCircle()
    call getEuler()
    call getPCC()
    call getPC2B()
    call getPC3B()
    call getPC_Zhou()
   
    end program Console1

