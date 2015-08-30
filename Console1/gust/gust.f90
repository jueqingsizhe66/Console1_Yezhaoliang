    module GUST
    IMPLICIT NONE

    contains
    subroutine generateGust()
    implicit none
    REAL(KIND=8),PARAMETER :: PI = 3.141592654D0
    REAL(KIND=8) :: Z,Z1,Z2,Z0
    REAL(KIND=8) :: WSPI
    REAL(KIND=8) :: VHUB,V50,V1,VGUST,V0
    REAL(KIND=8) :: FACTOR
    REAL(KIND=8) :: IREF,B,D1
    REAL(KIND=8) :: WD,AD1
    REAL(KIND=8) :: T,T1,T2,DT,TIME
    REAL(KIND=8) :: C1,C2,C3
    CHARACTER (LEN=128) :: STR0,STR1
    INTEGER :: I,J

    ! DF77
    !Z0 = 70.0D0     !
    !WD = 77.0D0     !
    !VHUB = 15.0D0   !
    ! 5MWBaseline
    Z0 = 70.0D0     !
    WD = 77.0D0     !
    VHUB = 15.0D0   !

    ! Sample
    !Z0 = 12.191D0     !
    !WD = 10.058D0     !
    !VHUB = 10.0D0   !
    IREF = 0.16D0   !

    Z1 = Z0 - WD/2.0D0
    Z2 = Z0 + WD/2.0D0

    Z = Z1          !
    STR1 = 'Z1'     !

    ! V50
    ! the steady extreme wind model
    V50 = 1.4D0 * VHUB * (Z / Z0)**0.11D0

    ! the turbulent extreme wind model
    !V50 =         VHUB * (Z / Z0)**0.11D0

    V1 = 0.8D0 * V50
    B = 5.6D0
    !D1 = IREF * (0.75D0 * VHUB + B)
    !D1 = 0.11D0 * VHUB
    D1 = 2.0D0 * IREF * (0.072D0 * (VHUB/2.0D0+3.0D0) * (VHUB/2.0D0-4.0D0) + 10.0D0)

    IF (Z <= 60.0D0) THEN
        AD1 = 0.7D0 * Z
    ELSEIF (Z > 60.0D0) THEN
        AD1 = 42.0D0
    ENDIF
    C1 = WD / AD1
    C2 = 1.0D0 + 0.1D0 * C1

    VGUST = MAX(1.35D0*(V1-VHUB) , 3.3D0*D1/C2)

    T = 0.0D0
    DT = 0.1D0     !
    T1 = 5.0D0     !
    T2 = 15.5D0    !
!! 会在项目文件 vfproj所在目录的gust子目录下
    !! ivf 是以vfpproj作为根目录
    STR0 = 'gust/VGUST_'//TRIM(ADJUSTL(STR1))//'.DAT'
    OPEN (10,FILE = STR0)
    DO I = 1,200
        V0 = VHUB * (Z / Z0)**0.2D0
        IF (T >= T1 .AND. T <= T2) THEN
            C3 = (T - T1) / (T2 - T1)
            V0 = V0 - 0.37D0 * VGUST * DSIN(3.0D0*PI*C3) * (1.0D0 - DCOS(2.0D0*PI*C3))
        ENDIF
        WRITE(10,*) T,V0
        T = T + DT
    ENDDO
    CLOSE (10)
    end subroutine generateGust
    END module GUST