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
    use JohnTimeStamp  ! 把timestamp()上升到所有模块都可以使用
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
    !* 开始测试FFT_Mod
    !* 引入了DP 和x变量
    !!***************************************
    integer,parameter :: FFT_ModN=8
    complex(KIND= FFT_ModDP) :: FFT_MODx(FFT_ModN) =[36.d0,21.d0,33.d0,44.d0,55.d0,63.d0,73.d0,38.d0],FFT_ModOriginal(FFT_ModN)
    complex(KIND=FFT_ModDP) :: temp
    !!!complex 数据类型的定义

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
    ! !* 开始测试 func_arg_name积分模块
    ! !!**************************************
    !
    ! write( * , * ) integral( x2 , 1.0 , 2.0 , 0.01 )
    ! write( * , * ) 2.0**3/3 - 1.0**3/3
    ! !  write( * , * ) integral( x3 , 1.0 , 2.0 , 0.01 )
    ! !  write( * , * ) 2.0**4/4 - 1.0**4/4
    ! !!Todo Test
    !
    ! !!**************************************
    ! !* 开始测试isTest 模块
    ! !!**************************************
    !
    ! !   write(*,*) is(5>3,"及格","不及格")     !!!中文好像有问题
    ! write (*,*) is(6>3,'ok','not ok')
    ! write (*,*) is(6<3,'ok','not ok')
    ! write (*,*) is(6>3,34,8)
    ! write (*,*) is(6>3,45,8)
    !
    ! !!**************************************
    ! !*  开始测试usenamelist模块
    ! !!**************************************
    !
    ! write(*,*) '开始测试namelist'
    ! call callNameList()
    ! call IntoNameList()
    !
    ! !!**************************************
    ! !* 开始测试 Func模块
    ! !!**************************************
    !
    ! i=PrintCopyRight("fcode.cn",this_year)
    ! !call PrintCopyRight("fcode.cn",now_year)
    ! ! print *,today
    !
    ! !!**************************************
    ! !* 开始测试 STUDENTCLASS模块
    ! !!**************************************
    !
    ! Liming = STUDENT(3,"Liming",69,95,88)
    ! call PRINT_STUDENT_INFORTMATION(Liming)
    !
    ! !!**************************************
    ! !*  开始测试 optionalP模块
    ! !!***************************************
    ! call sub(7,6)
    ! call sub(7,6,3)
    !
    !
    ! !!**************************************
    ! !* 开始测试typedef模块的 小数转化为度分秒
    ! !!***************************************
    ! st = RDegree_To_TypeDegree(r)
    ! write(*,*) r,"==>",st
    !
    ! !!**************************************
    ! !* WriteMatrix写入矩阵测试
    ! !!***************************************
    ! call WriteMatrix(x(1:2,3:3))
    !
    ! !!**************************************
    ! !* 开始测试 SecMod
    ! !!***************************************
    ! write(*,'(a,g0,a)') 'between: ',st2-st1,' uSec'
    !
    ! !!**************************************
    ! !* 开始测试Grid_mod 网格省城部分
    ! !!***************************************
    ! bGrid = Grid_Alloc( 5 , 5 )
    ! if ( .not.b ) stop "cann't allocate"
    ! write( * , '(a,g0,"x",g0)' ) 'Grid Size: ' , grd_stInfo
    ! !grd_stInfo%m = 50 !// 出错  !!因为是protected所以无法修改 只能读取
    ! call FillGrid()
    ! write( * , * ) 'Grid: '
    ! Do i = 1 , grd_stInfo%n
    !     write( * , '(999f6.2)' ) grd_rData( : , i )
    ! End Do
    ! bGrid = Grid_Dealloc()
    !
    ! !!**************************************
    ! !* 开始测试 func模块
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
    !     ! 上面是安全检查过程  下面是单独的针对合法的
    !     ! keyword处理程序  当然也可以放在上面！
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
    ! !* 开始测试阵风模块
    ! !!***************************************
    ! call generateGust()
    !
    !
    ! !!**************************************
    ! !* 开始测试 fortran产生数据 调用matlab进行绘图
    ! !!***************************************
    ! !call generateDataForMatlab()
    !
    ! !!**************************************
    ! !* 开始测试 MyReadModule
    ! !!***************************************
    ! call readFile(n,arrayMy)
    ! write(*,*) (arrayMy(i),i=1,n)
    !
    ! !!**************************************
    ! !* 开始测试FFT_MOD
    ! !!***************************************
    ! write(*,*) 'Original Input Data'
    ! Do i = 1 , FFT_ModN
    !     Write (*, *) FFT_MODx(i)
    ! End Do
    ! FFT_ModOriginal =FFT_MODx  !赋值 保留原始值
    ! ! 经过正向FFT
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
    ! write(*,*) '暂停一小会 空格继续'
    ! read(*,*)
    !! call system("pause")  !加进去有问题
    ! !!**************************************
    ! !* 开始测试JohnComplex模块
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
    !*  测试F90Split
    !!***************************************
    !write(*,*) '开始测试F90Split'
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

    !!!只要把结果黏贴到cygwin就可以删除不必要的函数声明了
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
    !请按任意键继续. . .
    !!**************************************
    !*  测试Area
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

