    module f90split
    use JohnTimeStamp
    implicit none

    integer,parameter ::MaxModule=60
    character (len=256),dimension(MaxModule) :: storeFunction
    character (len=256),dimension(MaxModule) :: storeFunctionGeo
    character (len=256),dimension(MaxModule) :: storeFunctionPrb
    character (len=256),dimension(MaxModule) :: FunctionGeo
    character (len=256),dimension(MaxModule) :: FunctionPrb 
    integer ( kind = 4 ) :: module_num=0
    integer ( kind = 4 ) :: module_num1=0
    logical :: ReadOver=.FALSE.

    contains
    subroutine start()
    implicit none
    character ( len = 255 ) input_file

    integer :: count
    call timestamp ( )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F90SPLIT:'
    write ( *, '(a)' ) '  What is the name of the input file?'
    read ( *, '(a)' ) input_file
    call handle ( input_file )
    call timestamp ( )
    end subroutine start

    subroutine handle ( input_file )

    !*****************************************************************************80
    !
    !! HANDLE handles one file.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    23 August 2011
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, character ( len = * ) INPUT_FILE, the name of the file to
    !    be split up.
    !
    implicit none

    integer ( kind = 4 ) i
    character ( len = 255 ) input_file
    character ( len = 255 ) output_file
    integer ( kind = 4 ) input_unit
    integer ( kind = 4 ) ios
    integer ( kind = 4 ) j
    character ( len = 255 ) line
    integer ( kind = 4 ) line_length
    integer ( kind = 4 ) line_length_loc
    integer ( kind = 4 ) line_length_max
    integer ( kind = 4 ) line_num



    !  Open the file.
    !
    call get_unit ( input_unit )

    open ( unit = input_unit, file = input_file, status = 'old', &
        iostat = ios )

    if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F90SPLIT - Fatal error!'
        write ( *, '(a)' ) &
            '  Could not open the input file "' // trim ( input_file ) // '".'
        stop
    end if

    line_num = 0


    line_length_max = -1;
    line_length_loc = -1;

    do

        read ( input_unit, '(a)', iostat = ios ) line

        if ( ios /= 0 ) then
            exit
        end if

        line_num = line_num + 1

        line_length = len_trim ( line )
        if ( line_length_max < line_length ) then
            line_length_max = line_length
            line_length_loc = line_num
        end if
        !
        !  If we don't have a module name, then it's not clear what to do.
        !  My vote is to discard the information for now.
        !
        !  It's important to check whether the next line marks the beginning of
        !  a named module.
        !

        call f90_line_is_begin ( line, output_file )
        !if( output_file /= ' ') then
        !
        !    module_num = module_num + 1
        !    storeFunction(module_num)=output_file
        !end if

    end do
    rewind(input_unit)
    do  i = 1,size(storeFunction)
        rewind(input_unit)
        ReadOver=.False.
        do

            read ( input_unit, '(a)', iostat = ios ) line

            if ( ios /= 0 ) then
                exit
            end if

            line_num = line_num + 1

            line_length = len_trim ( line )
            if ( line_length_max < line_length ) then
                line_length_max = line_length
                line_length_loc = line_num
            end if
            !
            !  If we don't have a module name, then it's not clear what to do.
            !  My vote is to discard the information for now.
            !
            !  It's important to check whether the next line marks the beginning of
            !  a named module.
            !

            call f90_line_handle ( line, storeFunction(i) )
            !if(ReadOver .eq. .TRUE.) then
            !    rewind(input_unit)
            !    exit
            !end if
            if(ReadOver .eq. .TRUE.) exit
            !if( output_file /= ' ') then
            !
            !    module_num = module_num + 1
            !    storeFunction(module_num)=output_file
            !end if
        end do
    end do
    close ( unit = input_unit )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F90SPLIT:'
    write ( *, '(a)' ) '  Reached end of ' // trim ( input_file )
    write ( *, '(a,i8)' ) '  Lines read:              ', line_num
    write ( *, '(a,i8)' ) '  Longest line length:     ', line_length_max
    write ( *, '(a,i8)' ) '  Longest line location:   ', line_length_loc
    write ( *, '(a,i8)' ) '  Named modules created:   ', module_num


    return
    end

    subroutine f90_line_is_begin ( line, name )
    !
    implicit none

    integer ( kind = 4 ) i
    character ( len = * ) line
    character ( len = 255 ) line2
    character ( len = * ) name
    !logical s_eqi

    name = ' '

    line2 = line
    call s_blank_delete ( line2 )

    if ( s_eqi ( line2(1:9), 'blockdata' ) ) then
        if ( line2(10:) == ' ' ) then
            name = 'blockdata'
        else
            call s_before_ss_copy ( line2(10:), '(', name )
        end if
    else if ( s_eqi ( line2(1:17), 'characterfunction' ) ) then
        call s_before_ss_copy ( line2(18:), '(', name )
    else if ( s_eqi ( line2(1:15), 'complexfunction' ) ) then
        call s_before_ss_copy ( line2(16:), '(', name )
    else if ( s_eqi ( line2(1:23), 'doubleprecisionfunction' ) ) then
        call s_before_ss_copy ( line2(24:), '(', name )
    else if ( s_eqi ( line2(1:8), 'function' ) ) then
        call s_before_ss_copy ( line2(9:), '(', name )
        if( name /= ' ') then
            module_num = module_num + 1
            !storeFunction(module_num)="sed -ie '/real "//name//"/d' geometry/geometry.f90"
            !storeFunction(module_num)="sed -ie '/real "//trim(name)//"/d' ./geometry/geometry.f90"
            !storeFunctionPrb(module_num)="sed -ie '/real "//trim(name)//"/d' ./geometry/geometry_prb.f90"
            !storeFunction(module_num)="sed -ie '/real "//trim(name)//"/d' ./geometry/geometry.f90"
            storeFunction(module_num)=trim(name)
        end if

    else if ( s_eqi ( line2(1:15), 'integerfunction' ) ) then
        call s_before_ss_copy ( line2(16:), '(', name )
    else if ( s_eqi ( line2(1:15), 'logicalfunction' ) ) then
        call s_before_ss_copy ( line2(16:), '(', name )
    else if ( s_eqi ( line2(1:6), 'module' ) ) then
        call s_before_ss_copy ( line2(7:), '(', name )
    else if ( s_eqi ( line2(1:7), 'program' ) ) then
        call s_before_ss_copy ( line2(8:), '(', name )
    else if ( s_eqi ( line2(1:12), 'realfunction' ) ) then
        call s_before_ss_copy ( line2(13:), '(', name )
    else if ( s_eqi ( line2(1:17), 'recursivefunction' ) ) then
        call s_before_ss_copy ( line2(18:), '(', name )
    else if ( s_eqi ( line2(1:10), 'subroutine' ) ) then
        call s_before_ss_copy ( line2(11:), '(', name )
    else if ( s_eqi ( line2(1:19), 'recursivesubroutine' ) ) then
        call s_before_ss_copy ( line2(20:), '(', name )
    end if
    !
    !  In some "clever" cases, people write the name of the routine
    !  on one line, continue with an ampersand, and the rest of the
    !  routine follows.
    !
    !  I really should be reading the logical line, not the literal
    !  line, but for now, let me just chop off trailing ampersands.
    !
    i = index ( name, '&' )

    if ( i /= 0 ) then
        name(i:i) = ' '
    end if

    return
    end


    subroutine f90_line_handle ( line, name )
    !
    implicit none

    integer ( kind = 4 ) i
    character ( len = * ) line
    character ( len = 255 ) line2
    character (len=20) variableType
    character ( len = * ) name
    character ( len = 512 ) name2
    !integer :: temp,temp2
    character(LEN=30) :: tempLogi,tempReal,tempInt
    !logical s_eqi


    !name2=name
    name2=' ' ! 就是因为这个错误导致我一直失败
    line2 = line
    call s_blank_delete ( line2 )


    !temp=8+len_trim(name)
    !temp2=5+len_trim(name)
    tempLogi='logical'//name
    tempReal='real'//name
    tempInt='integer'//name
    if(s_eqi(line2, tempReal) .OR. s_eqi(line2, tempInt).OR. s_eqi(line2, tempLogi) ) then
        !call s_before_ss_copy ( line2(1:), '(', name2 )
        name2=adjustl(trim(line))
        ReadOver=.TRUE.
    end if

    if( name2 /= ' ') then
        !call s_before_ss_copy(line,name,variableType)
        call s_before_ss_copy(adjustl(trim(line)),adjustl(trim(name)),variableType)
       ! write(*,*) variableType,' | name=',name,'|  line=',line
        module_num1 = module_num1 + 1
        !storeFunction(module_num)="sed -ie '/real "//name//"/d' geometry/geometry.f90"
        !storeFunction(module_num)="sed -ie '/real "//trim(name)//"/d' ./geometry/geometry.f90"
        storeFunctionPrb(module_num1)="sed -ie '/"//(adjustl(trim(name2)))//"/d' ./geometry/geometry_prb.f90"
        storeFunctionGeo(module_num1)="sed -ie '/"//(adjustl(trim(name2)))//"/d' ./geometry/geometry.f90"
        !FunctionPrb(module_num1)="sed -ie 's/function "//(adjustl(trim(name)))//"/"variableType//" function "//name//"/g' ./geometry/geometry_prb.f90"
        FunctionGeo(module_num1)="sed -ie 's/function "//adjustl(trim(name))//"/"//adjustl(trim(variableType))//" function "//adjustl(trim(name))//"/g' ./geometry/geometry.f90"
        FunctionPrb(module_num1)="sed -ie 's/function "//adjustl(trim(name))//"/"//adjustl(trim(variableType))//" function "//adjustl(trim(name))//"/g' ./geometry/geometry_prb.f90"
        !storeFunction(module_num)=trim(name)
    end if


    !
    !  In some "clever" cases, people write the name of the routine
    !  on one line, continue with an ampersand, and the rest of the
    !  routine follows.
    !
    !  I really should be reading the logical line, not the literal
    !  line, but for now, let me just chop off trailing ampersands.
    !

    return
    end

    subroutine s_blank_delete ( s )

    implicit none

    character c
    integer ( kind = 4 ) iget
    integer ( kind = 4 ) iput
    character ( len = * ) s
    character TAB

    TAB = char ( 9 )
    iput = 0

    do iget = 1, len ( s )

        c = s(iget:iget)

        if ( c /= ' ' .and. c /= TAB ) then
            iput = iput + 1
            s(iput:iput) = c
        end if

    end do

    s(iput+1:) = ' '

    return
    end

    !subroutine s_blank_delete( s )
    !
    !implicit none
    !
    !character c
    !integer ( kind = 4 ) iget
    !integer ( kind = 4 ) iput
    !character ( len = * ) s
    !character TAB
    !
    !TAB = char ( 9 )
    !iput = 0
    !
    !do iget = 1, len ( s )
    !
    !    c = s(iget:iget)
    !
    !    if ( c /= ' ' .and. c /= TAB ) then
    !        iput = iput + 1
    !        s(iput:iput) = c
    !    end if
    !
    !end do
    !
    !s(iput+1:) = ' '
    !
    !return
    !end
    !

    function s_eqi ( s1, s2 )

    !*****************************************************************************80
    !
    !! S_EQI is a case insensitive comparison of two strings for equality.
    !
    !  Example:
    !
    !    S_EQI ( 'Anjana', 'ANJANA' ) is .TRUE.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    14 April 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, character ( len = * ) S1, S2, the strings to compare.
    !
    !    Output, logical S_EQI, the result of the comparison.
    !
    implicit none

    character c1
    character c2
    integer ( kind = 4 ) i
    integer ( kind = 4 ) len1
    integer ( kind = 4 ) len2
    integer ( kind = 4 ) lenc
    logical s_eqi
    character ( len = * ) s1
    character ( len = * ) s2

    len1 = len ( s1 )
    len2 = len ( s2 )
    lenc = min ( len1, len2 )

    s_eqi = .false.

    do i = 1, lenc

        c1 = s1(i:i)
        c2 = s2(i:i)
        call ch_cap ( c1 )
        call ch_cap ( c2 )

        if ( c1 /= c2 ) then
            return
        end if

    end do

    do i = lenc + 1, len1
        if ( s1(i:i) /= ' ' ) then
            return
        end if
    end do

    do i = lenc + 1, len2
        if ( s2(i:i) /= ' ' ) then
            return
        end if
    end do

    s_eqi = .true.

    return
    end
    subroutine ch_cap ( c )

    !*****************************************************************************80
    !
    !! CH_CAP capitalizes a single character.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    19 July 1998
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input/output, character C, the character to capitalize.
    !
    implicit none

    character c
    integer ( kind = 4 ) itemp

    itemp = ichar ( c )

    if ( 97 <= itemp .and. itemp <= 122 ) then
        c = char ( itemp - 32 )
    end if

    return
    end

    subroutine s_before_ss_copy ( s, ss, s2 )

    !*****************************************************************************80
    !
    !! S_BEFORE_SS_COPY copies a string up to a given substring.
    !
    !  Discussion:
    !
    !    S and S2 can be the same object, in which case the string is
    !    overwritten by a copy of itself up to the substring, followed
    !    by blanks.
    !
    !  Example:
    !
    !    Input:
    !
    !      S = 'ABCDEFGH'
    !      SS = 'EF'
    !
    !    Output:
    !
    !      S2 = 'ABCD'.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    21 November 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, character ( len = * ) S, the string to be copied.
    !
    !    Input, character ( len = * ) SS, the substring before which the copy stops.
    !
    !    Output, character ( len = * ) S2, the copied portion of S.
    !
    implicit none

    integer ( kind = 4 ) last
    integer ( kind = 4 ) last_s2
    character ( len = * ) s
    character ( len = * ) s2
    character ( len = * ) ss
    !
    !  Find the first occurrence of the substring.
    !
    last = index ( s, ss )
    !
    !  If the substring doesn't occur at all, behave as though it begins
    !  just after the string terminates.
    !
    !  Now redefine LAST to point to the last character to copy before
    !  the substring begins.
    !
    if ( last == 0 ) then
        last = len ( s )
    else
        last = last - 1
    end if
    !
    !  Now adjust again in case the copy holder is "short".
    !
    last_s2 = len ( s2 )

    last = min ( last, last_s2 )
    !
    !  Copy the beginning of the string.
    !  Presumably, compilers now understand that if LAST is 0, we don't
    !  copy anything.
    !  Clear out the rest of the copy.
    !
    s2(1:last) = s(1:last)
    s2(last+1:last_s2) = ' '

    return
    end

    subroutine get_unit ( iunit )

    !*****************************************************************************80
    !
    !! GET_UNIT returns a free FORTRAN unit number.
    !
    !  Discussion:
    !
    !    A "free" FORTRAN unit number is a value between 1 and 99 which
    !    is not currently associated with an I/O device.  A free FORTRAN unit
    !    number is needed in order to open a file with the OPEN command.
    !
    !    If IUNIT = 0, then no free FORTRAN unit could be found, although
    !    all 99 units were checked (except for units 5, 6 and 9, which
    !    are commonly reserved for console I/O).
    !
    !    Otherwise, IUNIT is a value between 1 and 99, representing a
    !    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
    !    are special, and will never return those values.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    18 September 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Output, integer ( kind = 4 ) IUNIT, the free unit number.
    !
    implicit none

    integer ( kind = 4 ) i
    integer ( kind = 4 ) ios
    integer ( kind = 4 ) iunit
    logical lopen

    iunit = 0

    do i = 1, 99

        if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

            inquire ( unit = i, opened = lopen, iostat = ios )

            if ( ios == 0 ) then
                if ( .not. lopen ) then
                    iunit = i
                    return
                end if
            end if

        end if

    end do

    return
    end


    end module f90split