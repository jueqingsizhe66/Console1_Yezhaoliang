!PROGRAM main
!    USE func
!    IMPLICIT NONE
!    REAL, PARAMETER :: pi = acos(-1.)
!    CHARACTER (LEN=:), ALLOCATABLE :: keywd
!    REAL, ALLOCATABLE :: pars(:)
!
!    DO
!        ! Try input the following strings in the commandline:
!        ! cir 20.
!        ! sqr 10.
!        ! rect 1. 2.
!        CALL readcmd(keywd, pars)
!        
!        IF (keywd=='end') THEN
!            EXIT
!        ELSE IF (keywd=='cir' .OR. keywd=='sqr') THEN
!            IF (size(pars) /= 1) THEN
!                WRITE (*, *) 'Incorrect number of parameters.'
!                CYCLE
!            END IF
!        ELSE IF (keywd=='rect') THEN
!            IF (size(pars) /= 2) THEN
!                WRITE (*, *) 'Incorrect number of parameters.'
!                CYCLE
!            END IF
!        ELSE
!            WRITE (*,*) 'Keyword unknown...'
!            CYCLE
!        END IF
!            
!        IF (keywd=='cir') THEN
!            WRITE (*, *) 'Area = ', pi*pars(1)**2.
!        ELSE IF (keywd=='sqr') THEN
!            WRITE (*, *) 'Area = ', pars(1)**2.
!        ELSE IF (keywd=='rect') THEN
!            WRITE (*, *) 'Area = ', pars(1)*pars(2)
!        END IF
!        
!    END DO
!END PROGRAM
