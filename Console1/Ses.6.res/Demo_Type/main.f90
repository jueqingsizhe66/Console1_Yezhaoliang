    MODULE types
        TYPE :: var_base
            CHARACTER (LEN=10) :: name
        END TYPE
        TYPE, EXTENDS (var_base) :: real_var
            REAL :: r
        END TYPE
        TYPE, EXTENDS (var_base) :: int_var
            INTEGER :: i
        END TYPE
    
    CONTAINS
        SUBROUTINE pnt_var(var)
            IMPLICIT NONE
            CLASS (var_base), INTENT (IN) :: var
    
            SELECT TYPE (var)
            CLASS IS (real_var)
                WRITE (*, *) 'Real Value ' // trim(var%name) // ' = ', var%r
            CLASS IS (int_var)
                WRITE (*, *) 'Integer Value ' // trim(var%name) // ' = ', var%i
            CLASS DEFAULT
                ERROR STOP 'Unknown type.'
            END SELECT
        END SUBROUTINE
    END MODULE
    !PROGRAM main
    !    USE types
    !    IMPLICIT NONE
    !    TYPE (real_var) :: vr = real_var('a', 3.14)
    !    TYPE (int_var) :: vi = int_var('b', 3)
    !
    !    CALL pnt_var(vr)
    !    CALL pnt_var(vi)
    !END PROGRAM
    
