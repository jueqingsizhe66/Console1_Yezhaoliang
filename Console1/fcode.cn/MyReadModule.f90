module MyReadModule
    implicit none
    contains
    subroutine readFile(n, array)
        integer :: n
        integer, allocatable :: array(:)
        integer :: i
        write(*,*) '���������ĸ���,��������ĳߴ��С��'
        read(*, *) n
        write(*,*) '��ʵ�Ǻ����� ������ʲô������ν �ҾͲ����㣡'
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