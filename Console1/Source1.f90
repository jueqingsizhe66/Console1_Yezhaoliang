    module fc_init
    implicit none
    INTEGER :: a=0,b=3,c=4
    contains

    subroutine fc_update()
    implicit none
    a=8
    b=29
    c=56
    end subroutine fc_update


    end module fc_init