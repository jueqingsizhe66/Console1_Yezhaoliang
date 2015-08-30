    module fmatlab
    implicit none
    contains
    !!**************************************
    !*  ���ӳ��� ���ڵ���system��matlab ������Ҫ���ú�matlab��path·�� ���� m�ļ�������Ŀ�� fortran-matlab�ļ����� 
    !!***************************************
    subroutine generateDataForMatlab
    integer,parameter :: rows=100, cols=100
    character(len=64),parameter :: dataFile="fortranmatlab/matrixM.txt",matFile="test"
    character(len=76),parameter :: cmdMatlab="matlab -r "//matFile
    real*8 :: M(rows,cols)
    integer :: i
    call random_seed()
    call random_number(M)
    open(100,file=dataFile)
    write(100,'(<cols>E25.15)') transpose(M)
    close(100)
    call system(cmdMatlab)
    end subroutine generateDataForMatlab
    end module fmatlab
