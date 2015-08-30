MODULE func
    IMPLICIT NONE
CONTAINS
    SUBROUTINE readcmd(keyword, pars)
        ! This SUBROUTINE reads a line of inpur from the commandline, and
        ! It returns the first phrase as keyword using variable 'keyword', and 
        ! returns the following numerical parameters using the allocatable real
        ! array 'pars'
        ! The size of array 'pars' is determined by another FUNCTION ct_pars.
        CHARACTER (LEN=:), ALLOCATABLE, INTENT (INOUT) :: keyword
        REAL, ALLOCATABLE, INTENT (INOUT) :: pars(:)
        CHARACTER (LEN=80) :: cmdline, kwt
        INTEGER :: npars

        READ (*, '(a80)') cmdline
        READ (cmdline, *) kwt
        IF (ALLOCATED(keyword)) DEALLOCATE(keyword)
        IF (ALLOCATED(pars)) DEALLOCATE(pars)
        ALLOCATE (CHARACTER(LEN=len_trim(adjustl(kwt))) :: keyword)
        ALLOCATE (pars(ct_pars(cmdline)))
        READ (cmdline, *) keyword, pars
    END SUBROUTINE

    FUNCTION ct_pars(cmdline) RESULT (npars)
        ! This FUNCTION counts the number of space separated parameters in the 
        ! command line string 'cmdline'.
        ! Abbreviations:
        ! * par = Parameter
        ! * cmd = Command
        ! * tmp = Temporary
        ! * n = Number
        CHARACTER (LEN=*), INTENT (IN) :: cmdline
        CHARACTER (LEN=:), ALLOCATABLE :: ctmp
        INTEGER :: npars, i

        npars = 0
        i = 1
        ALLOCATE (CHARACTER(LEN=len_trim(adjustl(cmdline))) :: ctmp)
        ctmp = trim(adjustl(cmdline))
        DO
            IF (ctmp(i:i)/=' ') THEN
                i = i + 1
            ELSE
                npars = npars + 1
                DO
                    i = i + 1
                    IF (ctmp(i:i)/=' ') EXIT
                END DO
            END IF
            IF (i==len(ctmp)) EXIT
        END DO

    END FUNCTION
END MODULE
