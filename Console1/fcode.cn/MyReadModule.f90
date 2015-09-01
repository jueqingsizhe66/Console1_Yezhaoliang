module MyReadModule
    implicit none
    contains
    subroutine readFile(n, array)
        integer :: n
        integer, allocatable :: array(:)
        integer :: i
        write(*,*) '请输入读入的个数,当做数组的尺寸大小，'
        read(*, *) n
        write(*,*) '其实是忽悠你 你输入什么都无所谓 我就不读你！'
        call system("pause")
        open(100, file='fcode.cn/INPUT.txt')
        read(100, *) n
        allocate(array(n))
        do i=1, n
            read(100, *) array(i)
        enddo
        close(100)
    end subroutine
end module MyReadModule