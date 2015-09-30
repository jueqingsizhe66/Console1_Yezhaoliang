    !! 首先根据实验方程 dy/dx = y i + ae**(miu x)
    module linearStability

    ! 存储 Func00 的值    e^(i*theta) =cos(theta)+i*sin(theta)
    ! 表示复平面的圆环
    !WRITE(10,*) 'FUNC00'
    !X = 0.0
    !
    !DO I = 1,300
    !    ! Y = EXP(OMIG)
    !    OMIG = CMPLX(0,X)
    !
    !    Y = EXP(OMIG)
    !
    !    ! 获得复平面的 x方向的值   Y方向的值
    !    WRITE(10,*) REAL(Y),AIMAG(Y)
    !    X = X + 0.5
    !ENDDO

    !call GetCircle()
    !call getEuler()
    !call getPCC()


    !WRITE(10,*) 'FUNC02'
    !X = 0.0
    !DO I = 1,300
    !  OMIG = CMPLX(0,X)
    !
    !  Y = 1/（23-12*）
    !
    !  WRITE(10,*) REAL(Y),AIMAG(Y)
    !  X = X + 0.5
    !ENDDO
    !
    !WRITE(10,*) 'FUNC03'
    !X = 0.0
    !DO I = 1,300
    !  OMIG = CMPLX(0,X)
    !
    !  Y = (2-CSQRT(1+2*OMIG)) / (3-2*OMIG)
    !
    !  WRITE(10,*) REAL(Y),AIMAG(Y)
    !  X = X + 0.5
    !ENDDO

    contains
    !! 获取 圆圈
    subroutine GetCircle()
    implicit none
    complex :: OMIG,Y
    INTEGER :: I,N
    real :: X
    integer ,parameter :: unit1=50
    character(len=25),parameter ::  filename="./linearStabilty/Circle.DAT"

    OPEN(unit1,FILE =filename)

    ! 存储 Func00 的值    e^(i*theta) =cos(theta)+i*sin(theta)
    ! 表示复平面的圆环
    WRITE(unit1,*) 'FUNC00'
    X = 0.0

    DO I = 1,300
        ! Y = EXP(OMIG)
        OMIG = CMPLX(0,X)

        Y = EXP(OMIG)

        ! 获得复平面的 x方向的值   Y方向的值
        WRITE(unit1,*) REAL(Y),AIMAG(Y)
        X = X + 0.5
    ENDDO
    close(unit1)
    end subroutine getCircle
    !! 欧拉显示差分的格式为 dy(x+deltaX)/dx =[ y(x+deltaX） -y(x)] /deltaX
    !! 带入试验方程得
    !!  [y(x+deltaX) - y(x)] /DeltaX= y(x) i + ae**(miu*DeltaX)
    !! 令 y(x+deltaX) = csi * y(x)

    !!  Euler  :  P(csi) = csi -1 -DeltaX i
    !!  csi = 1+DeltaX                                ------------>这就是编程需要的
    subroutine getEuler()
    implicit none
    complex :: OMIG,Y
    INTEGER :: I,N
    real :: X
    integer ,parameter :: unit1=51
    character(len=25),parameter ::  filename="./linearStabilty/Euler.DAT"

    OPEN(unit1,FILE =filename)

    ! 存储 Func00 的值    e^(i*theta) =cos(theta)+i*sin(theta)
    ! 表示复平面的圆环
    WRITE(unit1,*) 'FUNCEuler'
    X = 0.0

    DO I = 1,300
        ! Y = EXP(OMIG)
        OMIG =1 + CMPLX(0,X) ! 1+ deltaX i   i=cmplx(0,1)  delta X i = cmplx(0,X)

        Y = OMIG

        ! 获得复平面的 x方向的值   Y方向的值
        WRITE(unit1,*) REAL(Y),AIMAG(Y)
        X = X + 0.5
    ENDDO
    close(unit1)
    end subroutine getEuler
    !!PCC 中心差分格式
    !! 中心差分的格式为 dy(x+deltaX/2)/dx =[ y(x+deltaX） -y(x)] /deltaX(多了一哥divede by 2)
    !! 带入试验方程得
    !!  [y(x+deltaX) - y(x)]/DeltaX = (y(x+deltaX) + y(x)/2 i + ae**(miu*DeltaX)
    !! 令 y(x+deltaX) = csi * y(x)

    !!  PCC  :  P(csi) =(1-1/2 deltaX i)csi -(1+1/2 deltaX i)
    !!  令P(csi)= 0    csi = (1+0.5 deltaX i) /(1-0.5 deltaX i)   ------------->这就是编程需要的
    subroutine getPCC()
    implicit none
    complex :: OMIG,Y
    INTEGER :: I,N
    real :: X
    integer ,parameter :: unit1=52
    character(len=25),parameter ::  filename="./linearStabilty/PCC.DAT"
    OPEN(unit1,FILE =filename)

    ! 存储 Func00 的值    e^(i*theta) =cos(theta)+i*sin(theta)
    ! 表示复平面的圆环
    WRITE(unit1,*) 'FUNPCC'

    X = 0.0

    DO I = 1,300
        ! Y = EXP(OMIG)
        OMIG =1 + CMPLX(0,X) ! 1+ deltaX i   i=cmplx(0,1)  delta X i = cmplx(0,X)

        Y = OMIG/conjg(OMIG)   ! conjg(OMIG)=1-CMPLX(0,X)

        ! 获得复平面的 x方向的值   Y方向的值
        WRITE(unit1,*) REAL(Y),AIMAG(Y)
        X = X + 0.5
    ENDDO
    close(unit1)
    end subroutine getPCC

    !! PC2B 和PC3B稳定性的判定方式
    !! 对应的特征根方程
    !!  PC2B  :  P(csi) = (3-2 deltaXi)csi**3 - (1+2 deltaXi)csi**2 -3*csi+1
    !!  PC3B  :  P(csi) = (23-12 deltaXi)csi**3 - (21+12 deltaXi)csi**2 -3*csi+1
    !!  周文平  :  P(csi) = (13-6 deltaXi)csi**3 - (15+6 deltaXi)csi**2 -3*csi+1
    !!PC2B
    subroutine getPC2B()

    implicit none
    REAL :: X
    COMPLEX :: OMIG,Y
    COMPLEX :: A,B,C,D,S1,S2,S3

    INTEGER :: I,N
    integer ,parameter :: unit1=58
    character(len=25),parameter ::  filename="./linearStabilty/PC2B.DAT"
    !!!getPC2B
    !!  开始获得 三次方特征根的值！  A=(3-2*deltaX)   deltaX=CMPLX(0,X)
    open(unit1,FILE=filename)
    WRITE(unit1,'(A47)') 'S1Real/ S1Imag/ S2Real/ S2Imag/ S3Real/ S3Imag/'
    X = 0.0
    DO I = 1,300
        OMIG = CMPLX(0,X)

        !PC2B  的系数
        A = 3-2*OMIG
        B = -(1+2*OMIG)
        C = -3
        D = 1

        !PC3B 格式
        !A = (23-12*OMIG)
        !B = - (21+12*OMIG)
        !C = -3
        !D = 1

        !!  周乐平方式
        !! 三次方程的系数 A   B    C     D
        !A = (13-6*OMIG)
        !B = - (15+6*OMIG)
        !C = 3
        !D = -1
        !!  特征根的值  获取三个特征根的值 S1  S2   S3
        CALL EIGENVALUE3 (A,B,C,D,S1,S2,S3)

        !! 特征根头  实数  虚数
        WRITE(unit1,'(6(F10.4))') REAL(S1),AIMAG(S1),REAL(S2),AIMAG(S2),REAL(S3),AIMAG(S3)

        !!  delta X
        X = X + 0.5
    ENDDO
    close(unit1)

    end subroutine getPC2B

    !!PC3B
    subroutine getPC3B()

    implicit none
    REAL :: X
    COMPLEX :: OMIG,Y
    COMPLEX :: A,B,C,D,S1,S2,S3

    INTEGER :: I,N
    integer ,parameter :: unit1=58
    character(len=25),parameter ::  filename="./linearStabilty/PC3B.DAT"
    !!!getPC2B
    !!  开始获得 三次方特征根的值！  A=(3-2*deltaX)   deltaX=CMPLX(0,X)
    open(unit1,FILE=filename)
    WRITE(unit1,'(A47)') 'S1Real/ S1Imag/ S2Real/ S2Imag/ S3Real/ S3Imag/'
    X = 0.0
    DO I = 1,300
        OMIG = CMPLX(0,X)

        !PC2B  的系数
        !A = 3-2*OMIG
        !B = -(1+2*OMIG)
        !C = -3
        !D = 1

        !PC3B 格式
        A = (23-12*OMIG)
        B = - (21+12*OMIG)
        C = -3
        D = 1

        !!  周乐平方式
        !! 三次方程的系数 A   B    C     D
        !A = (13-6*OMIG)
        !B = - (15+6*OMIG)
        !C = 3
        !D = -1
        !!  特征根的值  获取三个特征根的值 S1  S2   S3
        CALL EIGENVALUE3 (A,B,C,D,S1,S2,S3)

        !! 特征根头  实数  虚数
        WRITE(unit1,'(6(F10.4))') REAL(S1),AIMAG(S1),REAL(S2),AIMAG(S2),REAL(S3),AIMAG(S3)

        !!  delta X
        X = X + 0.5
    ENDDO
    close(unit1)

    end subroutine getPC3B
    !!PC_Zhou
    subroutine getPC_Zhou()

    implicit none
    REAL :: X
    COMPLEX :: OMIG,Y
    COMPLEX :: A,B,C,D,S1,S2,S3

    INTEGER :: I,N
    integer ,parameter :: unit1=58
    character(len=25),parameter ::  filename="./linearStabilty/Zhou.DAT"
    !!!getPC2B
    !!  开始获得 三次方特征根的值！  A=(3-2*deltaX)   deltaX=CMPLX(0,X)
    open(unit1,FILE=filename)
    WRITE(unit1,'(A47)') 'S1Real/ S1Imag/ S2Real/ S2Imag/ S3Real/ S3Imag/'
    X = 0.0
    DO I = 1,300
        OMIG = CMPLX(0,X)

        !PC2B  的系数
        !A = 3-2*OMIG
        !B = -(1+2*OMIG)
        !C = -3
        !D = 1

        !PC3B 格式
        !A = (23-12*OMIG)
        !B = - (21+12*OMIG)
        !C = -3
        !D = 1

        !!  周乐平方式
        !! 三次方程的系数 A   B    C     D
        A = (13-6*OMIG)
        B = - (15+6*OMIG)
        C = 3
        D = -1
        !!  特征根的值  获取三个特征根的值 S1  S2   S3
        CALL EIGENVALUE3 (A,B,C,D,S1,S2,S3)

        !! 特征根头  实数  虚数
        WRITE(unit1,'(6(F10.4))') REAL(S1),AIMAG(S1),REAL(S2),AIMAG(S2),REAL(S3),AIMAG(S3)

        !!  delta X
        X = X + 0.5
    ENDDO
    close(unit1)

    end subroutine getPC_Zhou
    SUBROUTINE EIGENVALUE3 (A,B,C,D,S1,S2,S3)
    IMPLICIT NONE
    COMPLEX,INTENT(IN) :: A,B,C,D
    COMPLEX,INTENT(OUT) :: S1,S2,S3
    COMPLEX :: AA,BB,CC
    COMPLEX :: M,N,P,Q,W
    COMPLEX :: TERM1,TERM2

    !! 特征根系数一般 AA=B/A   把三次方根的系数化为1
    AA = B/A
    BB = C/A
    CC = D/A
    !  鲍赫公式求取特征根的方法
    !
    M = 36*AA*BB - 8*AA**CMPLX(3,0)-108*CC
    N = (M**CMPLX(2,0) - (4*AA**CMPLX(2,0) - 12*BB)**CMPLX(3,0))**CMPLX(0.5,0)

    P = (M + N)**(CMPLX(1,0)/CMPLX(3,0))
    !! 鲍赫公式求取得时候 需要判断P=0 和P不等于0的情况
    IF (P == CMPLX(0,0)) THEN
        Q = (M - N)**(CMPLX(1,0)/CMPLX(3,0))
    ELSE
        Q = (4*AA**CMPLX(2,0) - 12*BB)/P
    ENDIF

    S1 = ((P + Q) - 2*AA)/6
    !!  3**(0.5)  =sqrt(3)     CMPLX(0,1)=i
    S2 = -((P + Q) + 4*AA)/12 + CMPLX(0,1)*((3**CMPLX(0.5,0)*(P - Q))/12)
    S3 = -((P + Q) + 4*AA)/12 - CMPLX(0,1)*((3**CMPLX(0.5,0)*(P - Q))/12)

    RETURN
    END SUBROUTINE EIGENVALUE3
    end module linearStability

