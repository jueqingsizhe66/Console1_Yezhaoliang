    module geometry_prb
    !
    !*******************************************************************************
    !
    !! GEOMETRY_PRB tests routines from the GEOMETRY library.
    !
    use geometry
    implicit none
    !
    !call timestamp ( )
    !
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'GEOMETRY_PRB'
    !write ( *, '(a)' ) '  A set of test programs for GEOMETRY.'
    !
    !call test00
    !call test01
    !call test02
    !call test03
    !call test032
    !call test034
    !call test0325
    !call test0326
    !call test04
    !call test044
    !call test045
    !call test05
    !call test015
    !call test016
    !call test06
    !call test07
    !call test08
    !call test09
    !call test095
    !call test10
    !
    !call test11
    !call test12
    !call test13
    !call test14
    !call test15
    !call test16
    !call test17
    !call test18
    !call test1855
    !call test19
    !call test20
    !call test2055
    !
    !call test21
    !call test2155
    !call test22
    !call test2235
    !call test2255
    !call test23
    !call test24
    !call test25
    !call test255
    !call test26
    !call test27
    !call test28
    !call test284
    !call test285
    !call test29
    !call test30
    !
    !call test32
    !call test33
    !call test34
    !call test35
    !call test36
    !call test37
    !call test38
    !call test39
    !call test40
    !
    !call test41
    !call test42
    !call test43
    !call test44
    !call test45
    !call test46
    !call test47
    !call test48
    !call test49
    !call test499
    !call test4995
    !call test50
    !call test505
    !call test0595
    !call test0596
    !call test05965
    !call test0597
    !call test0598
    !call test506
    !
    !call test52
    !call test53
    !call test54
    !call test55
    !call test56
    !call test57
    !call test58
    !
    !call test170
    !call test171
    !call test172
    !call test173
    !call test174
    !call test175
    !call test176
    !call test177
    !call test178
    !call test179
    !
    !call test180
    !call test181
    !call test182
    !call test183
    !call test184
    !call test185
    !call test186
    !call test187
    !call test188
    !call test189
    !
    !call test190
    !call test191
    !call test192
    !call test193
    !call test194
    !call test195
    !call test196
    !call test197
    !call test198
    !call test199
    !
    !call test200
    !call test201
    !call test202
    !call test203
    !call test204
    !call test205
    !call test206
    !call test207
    !call test208
    !call test209
    !
    !call test210
    !call test211
    !call test212
    !call test213
    !call test214
    !call test215
    !call test216
    !call test217
    !call test218
    !call test219
    !
    !call test220
    !call test221
    !call test222
    !call test223
    !call test224
    !call test225
    !call test226
    !call test227
    !
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'GEOMETRY_PRB'
    !write ( *, '(a)' ) '  Normal end of execution.'
    !
    !stop
    contains
    subroutine test00
    !
    !*******************************************************************************
    !
    !! TEST00 tests RANDOM_SEED.
    !
    implicit none
    !
    integer seed
    !
    seed = 123456789

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST00'
    write ( *, '(a)' ) '  Call RANDOM_SEED to initialize the random number'
    write ( *, '(a)' ) '  generator.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i12)' ) '  Input SEED = ', seed

    call random_seed ( seed )

    return
    end
    subroutine test01
    !
    !*******************************************************************************
    !
    !! TEST01 tests ANGLE_CONTAINS_RAY_2D.
    !! TEST01 tests ANGLE_RAD_2D;
    !
    implicit none
    !
    integer, parameter :: ntest = 6
    !
    integer i
    logical inside
    integer j
    integer, parameter :: n_angle = 12
    real temp
    real thetar
    real x
    real x1
    real x2
    real x3
    real y
    real y1
    real y2
    real y3
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST01'
    write ( *, '(a)' ) '  ANGLE_CONTAINS_RAY_2D sees if a ray'
    write ( *, '(a)' ) '    lies within an angle.'
    write ( *, '(a)' ) '  ANGLE_RAD_2D computes the angle between two'
    write ( *, '(a)' ) '    rays;'
    write ( *, '(a)' ) ' '
    !
    !  An acute angle (45 degrees)
    !
    do j = 1, ntest

        if ( j == 1 ) then

            x1 = 1.0E+00
            y1 = 0.0E+00

            x2 = 0.0E+00
            y2 = 0.0E+00

            x3 = 1.0E+00
            y3 = 1.0E+00

        else if ( j == 2 ) then

            x1 = 1.0E+00
            y1 = 0.0E+00

            x2 = 0.0E+00
            y2 = 0.0E+00

            x3 = 0.0E+00
            y3 = 1.0E+00

        else if ( j == 3 ) then

            x1 = 1.0E+00
            y1 = -1.0E+00

            x2 = 0.0E+00
            y2 = 0.0E+00

            x3 = 0.0E+00
            y3 = 1.0E+00

        else if ( j == 4 ) then

            x1 = 1.0E+00
            y1 = 0.0E+00

            x2 = 0.0E+00
            y2 = 0.0E+00

            x3 = -1.0E+00
            y3 =  0.0E+00

        else if ( j == 5 ) then

            x1 = 1.0E+00
            y1 = 0.0E+00

            x2 = 0.0E+00
            y2 = 0.0E+00

            x3 =  0.0E+00
            y3 = -1.0E+00

        else if ( j == 6 ) then

            x1 = 1.0E+00
            y1 = 0.0E+00

            x2 = 0.0E+00
            y2 = 0.0E+00

            x3 =  1.0E+00
            y3 = -0.01E+00

        end if

        temp = angle_rad_2d ( x1, y1, x2, y2, x3, y3 )

        write ( *, '(a)' ) ' '
        write ( *, '(a,g14.6)' ) '  Angle = ', temp
        write ( *, '(a)' ) ' '
        call rvec_print_2d ( x1, y1, '  Vertex A' )
        call rvec_print_2d ( x2, y2, '  Vertex B' )
        call rvec_print_2d ( x3, y3, '  Vertex C' )
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '       X            Y       Inside?'
        write ( *, '(a)' ) ' '

        do i = 0, n_angle

            thetar = real ( i ) * 2.0E+00 * r_pi ( ) / real ( n_angle )
            x = cos ( thetar )
            y = sin ( thetar )
            call angle_contains_ray_2d ( x1, y1, x2, y2, x3, y3, x, y, inside )
            write ( *, '(2g14.6,2x,l1)' ) x, y, inside

        end do

    end do

    return
    end
    subroutine test02
    !
    !*******************************************************************************
    !
    !! TEST02 tests ANGLE_DEG_2D;
    !! TEST02 tests ANGLEI_DEG_2D;
    !! TEST02 tests ANGLE_RAD_ND.
    !
    !use geometry
    implicit none
    !
    integer, parameter :: n = 2
    !
    integer i
    integer, parameter :: n_angle = 12
    real temp1
    real temp2
    real temp3
    real temp4
    real thetad
    real thetar
    real v1(n)
    real v2(n)
    real x
    real y
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST02'
    write ( *, '(a)' ) '  ANGLE_DEG_2D computes an angle;'
    write ( *, '(a)' ) '  ANGLEI_DEG_2D computes an interior angle;'
    write ( *, '(a)' ) '  ANGLE_RAD_ND computes an angle.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  X, Y, Theta, atan2(y, x), ' // &
        'ANGLE_RAD_ND, ANGLE_DEG_2D, ANGLEI_DEG_2D'
    write ( *, '(a)' ) ' '
    v1(1:2) = (/ 1.0E+00, 0.0E+00 /)

    do i = 0, n_angle

        thetad = real ( i ) * 360.0E+00 / real ( n_angle )
        thetar = degrees_to_radians ( thetad )

        x = cos ( thetar )
        y = sin ( thetar )
        v2(1:2) = (/ x, y /)

        temp1 = radians_to_degrees ( atan2 ( y, x ) )

        temp2 = angle_rad_nd ( n, v1, v2 )

        temp3 = angle_deg_2d ( v1(1), v1(2), 0.0E+00, 0.0E+00, v2(1), v2(2) )

        temp4 = anglei_deg_2d ( v1(1), v1(2), 0.0E+00, 0.0E+00, v2(1), v2(2) )

        write ( *, '(7f10.3)') x, y, thetad, temp1, temp2, temp3, temp4

    end do

    return
    end
    subroutine test03
    !
    !*******************************************************************************
    !
    !! TEST03 tests ANGLE_RAD_3D;
    !
    implicit none
    !
    integer, parameter :: ntest = 3
    !
    integer i
    real temp1
    real temp2
    real x1
    real, parameter :: x2 = 0.0E+00
    real, parameter :: x3 = 0.0E+00
    real, parameter, dimension ( ntest ) :: xtest = (/ &
        1.0E+00, 1.0E+00, 0.0E+00 /)
    real y1
    real, parameter :: y2 = 0.0E+00
    real, parameter :: y3 = 0.0E+00
    real, parameter, dimension ( ntest ) :: ytest = (/ &
        0.0E+00, 2.0E+00, 0.0E+00 /)
    real z1
    real, parameter :: z2 = 0.0E+00
    real, parameter :: z3 = 1.0E+00
    real, parameter, dimension ( ntest ) :: ztest = (/ &
        0.0E+00, 3.0E+00, 1.0E+00 /)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST03'
    write ( *, '(a)' ) '  ANGLE_RAD_3D computes an angle;'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  X, Y, Z, ANGLE_RAD_3D, (Degrees)'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x1 = xtest(i)
        y1 = ytest(i)
        z1 = ztest(i)

        temp1 = angle_rad_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3 )
        temp2 = radians_to_degrees ( temp1 )

        write ( *, '(6g12.4)') x1, y1, z1, temp1, temp2

    end do

    return
    end
    subroutine test032
    !
    !*******************************************************************************
    !
    !! TEST032 tests ARC_COSINE;
    !
    implicit none
    !
    integer, parameter :: test_num = 9
    !
    integer i
    real temp1
    real temp2
    real, dimension ( test_num ) :: test_x = (/ 5.0E+00, 1.2E+00, 1.0E+00, &
        0.9E+00, 0.5E+00, 0.0E+00, -0.9E+00, -1.0E+00, -1.01E+00 /)
    real x
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST032'
    write ( *, '(a)' ) '  ARC_COSINE computes an angle with a given cosine;'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  X, ARC_COSINE(X), (Degrees)'
    write ( *, '(a)' ) ' '

    do i = 1, test_num

        x = test_x(i)

        temp1 = arc_cosine ( x )
        temp2 = radians_to_degrees ( temp1 )

        write ( *, '(6g12.4)') x, temp1, temp2

    end do

    return
    end
    subroutine test034
    !
    !*******************************************************************************
    !
    !! TEST034 tests ATAN4;
    !
    implicit none
    !
    integer, parameter :: test_num = 8
    !
    integer i
    real temp1
    real temp2
    real temp3
    real, dimension ( test_num ) :: test_x = (/ 1.0E+00, 1.0E+00, 1.0E+00, &
        1.0E+00, 1.0E+00, -1.0E+00, -1.0E+00, 0.0E+00 /)
    real x
    real, dimension ( test_num ) :: test_y = (/ 0.0E+00, 1.0E+00, 2.0E+00, &
        0.0E+00, -1.0E+00, -1.0E+00, -1.0E+00, -1.0E+00 /)
    real y
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST034'
    write ( *, '(a)' ) '  ATAN4 computes an angle with a given tangent.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  X, Y, ATAN(Y/X), ATAN2(Y,X), ATAN4(Y,X)'
    write ( *, '(a)' ) ' '

    do i = 1, test_num

        x = test_x(i)
        y = test_y(i)

        temp1 = atan ( y / x )
        temp2 = atan2 ( y, x )
        temp3 = atan4 ( y, x )

        write ( *, '(6g12.4)') x, y, temp1, temp2, temp3

    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Repeat, but display answers in degrees.'
    write ( *, '(a)' ) ' '

    do i = 1, test_num

        x = test_x(i)
        y = test_y(i)

        temp1 = radians_to_degrees ( atan ( y / x ) )
        temp2 = radians_to_degrees ( atan2 ( y, x ) )
        temp3 = radians_to_degrees ( atan4 ( y, x ) )

        write ( *, '(6g12.4)') x, y, temp1, temp2, temp3

    end do

    return
    end
    subroutine test0325
    !
    !*******************************************************************************
    !
    !! TEST0325 tests BALL_UNIT_SAMPLE_2D.
    !
    integer, parameter :: n = 2
    !
    real average(n)
    real average_r
    real average_theta
    integer i
    integer j
    integer, parameter :: n_sample = 1000
    integer seed
    real theta
    real v(n)
    real x(n)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST0325'
    write ( *, '(a)' ) '  For the unit ball in 2 dimensions (the disk):'
    write ( *, '(a)' ) '  BALL_UNIT_SAMPLE_2D samples;'

    seed = 123456789
    call random_seed ( seed )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  A few sample values:'
    write ( *, '(a)' ) ' '

    do i = 1, 5
        call ball_unit_sample_2d ( x )
        write ( *, '(2f8.4)' ) x(1:n)
    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  Number of sample points = ', n_sample

    seed = 123456789
    call random_seed ( seed )

    average(1:n) = 0.0E+00

    do i = 1, n_sample
        call ball_unit_sample_2d ( x )
        average(1:n) = average(1:n) + x(1:n)
    end do

    average(1:n) = average(1:n) / real ( n_sample )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now average the points, which should get a value'
    write ( *, '(a)' ) '  close to zero, and closer as N increases.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2f8.4)' ) '  Average:        ', average(1:n)

    seed = 123456789
    call random_seed ( seed )

    average_r = 0.0E+00

    do i = 1, n_sample
        call ball_unit_sample_2d ( x )
        average_r = average_r + sqrt ( sum ( x(1:n)**2 ) )
    end do

    average_r = average_r / real ( n_sample )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now average the distance of the points from'
    write ( *, '(a,f8.4)' ) '  the center, which should be 1/sqrt(2) = ', &
        1.0E+00 / sqrt ( 2.0E+00 )
    write ( *, '(a)' ) ' '
    write ( *, '(a,2f8.4)' ) '  Average:        ', average_r

    seed = 123456789
    call random_seed ( seed )

    average_theta = 0.0E+00

    do i = 1, n_sample
        call ball_unit_sample_2d ( x )
        theta = atan4 ( x(2), x(1) )
        average_theta = average_theta + theta
    end do

    average_theta = average_theta / real ( n_sample )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now average the THETA,'
    write ( *, '(a)' ) '  which should be PI.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2f8.4)' ) '  Average:        ', average_theta

    return
    end
    subroutine test0326
    !
    !*******************************************************************************
    !
    !! TEST0326 tests BALL_UNIT_SAMPLE_3D.
    !
    integer, parameter :: n = 3
    !
    real average(n)
    real average_phi
    real average_r
    real average_theta
    integer i
    integer j
    integer, parameter :: n_sample = 1000
    integer seed
    real theta
    real v(n)
    real x(n)
    real  r,phi
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST0326'
    write ( *, '(a)' ) '  For the unit ball in 3 dimensions:'
    write ( *, '(a)' ) '  BALL_UNIT_SAMPLE_3D samples;'

    seed = 123456789
    call random_seed ( seed )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  A few sample values:'
    write ( *, '(a)' ) ' '

    do i = 1, 5
        call ball_unit_sample_3d ( x )
        write ( *, '(3f8.4)' ) x(1:n)
    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  Number of sample points = ', n_sample

    seed = 123456789
    call random_seed ( seed )

    average(1:n) = 0.0E+00

    do i = 1, n_sample
        call ball_unit_sample_3d ( x )
        average(1:n) = average(1:n) + x(1:n)
    end do

    average(1:n) = average(1:n) / real ( n_sample )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now average the points, which should get a value'
    write ( *, '(a)' ) '  close to zero, and closer as N increases.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3f8.4)' ) '  Average:        ', average(1:n)

    seed = 123456789
    call random_seed ( seed )

    average_r = 0.0E+00

    do i = 1, n_sample
        call ball_unit_sample_3d ( x )
        r = sqrt ( sum ( x(1:n)**2 ) )
        average_r = average_r + r
    end do

    seed = 123456789
    call random_seed ( seed )

    average_r = average_r / real ( n_sample )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now average the distance of the points from'
    write ( *, '(a,f8.4)' ) '  the center, which should be the '
    write ( *, '(a,f8.4)' ) '  1/2**(1/n) = ', &
        0.5E+00**( 1.0E+00 / real ( n ) )
    write ( *, '(a)' ) ' '
    write ( *, '(a,f8.4)' ) '  Average:        ', average_r

    seed = 123456789
    call random_seed ( seed )

    average_theta = 0.0E+00

    do i = 1, n_sample
        call ball_unit_sample_3d ( x )
        theta = atan4 ( x(2), x(1) )
        average_theta = average_theta + theta
    end do

    average_theta = average_theta / real ( n_sample )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now average the angle THETA,'
    write ( *, '(a)' ) '  which should be PI.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,f8.4)' ) '  Average:        ', average_theta

    seed = 123456789
    call random_seed ( seed )

    average_phi = 0.0E+00

    do i = 1, n_sample
        call ball_unit_sample_3d ( x )
        r = sqrt ( sum ( x(1:n)**2 ) )
        phi = acos ( x(3) / r )
        average_phi = average_phi + phi
    end do

    average_phi = average_phi / real ( n_sample )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now average the angle PHI,'
    write ( *, '(a)' ) '  which should be PI/2.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,f8.4)' ) '  Average:        ', average_phi

    return
    end
    subroutine test04
    !
    !*******************************************************************************
    !
    !! TEST04 tests BASIS_MAP_3D.
    !
    implicit none
    !
    real a(3,3)
    real b(3,3)
    real c(3,3)
    integer i
    integer ierror
    integer j
    integer k
    real, dimension ( 3 ) :: u1 = (/ 1.0E+00, 2.0E+00, 3.0E+00 /)
    real, dimension ( 3 ) :: u2 = (/ 0.0E+00, 0.0E+00, 1.0E+00 /)
    real, dimension ( 3 ) :: u3 = (/ 1.0E+00, 0.0E+00, 2.0E+00 /)
    real, dimension ( 3 ) :: v1 = (/ 14.0E+00, 4.0E+00, 4.0E+00 /)
    real, dimension ( 3 ) :: v2 = (/  3.0E+00, 1.0E+00, 0.0E+00 /)
    real, dimension ( 3 ) :: v3 = (/  7.0E+00, 3.0E+00, 2.0E+00 /)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST04'
    write ( *, '(a)' ) '  BASIS_MAP_3D computes the linear transform A'
    write ( *, '(a)' ) '  which maps vectors U1, U2 and U3 to vectors'
    write ( *, '(a)' ) '  V1, V2 and V3.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The column vectors U1, U2, U3:'
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(3g14.6)' ) u1(i), u2(i), u3(i)
    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The column vectors V1, V2, V3:'
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(3g14.6)' ) v1(i), v2(i), v3(i)
    end do

    call basis_map_3d ( u1, u2, u3, v1, v2, v3, a, ierror )

    if ( ierror /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  The matrix [ U1 | U2 | U3 ] was singular.'
        write ( *, '(a)' ) '  No transformation was computed.'
        return
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The transformation matrix A:'
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(3g14.6)' ) a(i,1:3)
    end do

    b(1:3,1) = u1(1:3)
    b(1:3,2) = u2(1:3)
    b(1:3,3) = u3(1:3)

    c(1:3,1:3) = matmul ( a(1:3,1:3), b(1:3,1:3) )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The product matrix A * [ U1 | U2 | U3 ]:'
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(3g14.6)' ) c(i,1:3)
    end do

    return
    end
    subroutine test044
    !
    !*******************************************************************************
    !
    !! TEST044 tests BOX_CLIP_LINE_2D.
    !
    implicit none
    !
    integer, parameter :: ntest = 5
    !
    integer itest
    integer ival
    real x1
    real, dimension ( ntest ) :: x1_test = (/ &
        1.0E+00, -3.0E+00, -20.0E+00, -20.0E+00, 10.0E+00 /)
    real x2
    real, dimension ( ntest ) :: x2_test = (/ &
        8.0E+00, 5.0E+00, 7.0E+00, 0.0E+00, 20.0E+00 /)
    real x3
    real x4
    real, parameter :: xmax = 10.0E+00
    real, parameter :: xmin = -10.0E+00
    real y1
    real, dimension ( ntest ) :: y1_test = (/ &
        2.0E+00, 12.0E+00, 20.0E+00, 40.0E+00, 40.0E+00 /)
    real y2
    real, dimension ( ntest ) :: y2_test = (/ &
        16.0E+00, 12.0E+00, 20.0E+00, 0.0E+00, 30.0E+00 /)
    real y3
    real y4
    real, parameter :: ymax = 20.0E+00
    real, parameter :: ymin = 10.0E+00
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST044'
    write ( *, '(a)' ) '  BOX_CLIP_LINE_2D clips a line with respect to a box.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The lower left box corner is:'
    write ( *, '(a)' ) ' '
    write ( *, '(4f8.4)' ) xmin, ymin
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The upper right box corner is:'
    write ( *, '(a)' ) ' '
    write ( *, '(4f8.4)' ) xmax, ymax
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  We list the points (X1,Y1) and (X2,Y2), and then'
    write ( *, '(a)' ) '  the clipped values.'
    write ( *, '(a)' ) ' '

    do itest = 1, ntest

        x1 = x1_test(itest)
        y1 = y1_test(itest)
        x2 = x2_test(itest)
        y2 = y2_test(itest)

        call box_clip_line_2d ( xmin, ymin, xmax, ymax, x1, y1, x2, y2, x3, y3, &
            x4, y4, ival )

        write ( *, '(a)' ) ' '
        write ( *, '(4f8.4)' ) x1, y1, x2, y2
        if ( ival == -1 ) then
            write ( *, '(a)' ) '  Line is outside the box.'
        else if ( ival == 0 ) then
            write ( *, '(a)' ) '  Line is inside the box.'
        else if ( ival == 1 ) then
            write ( *, '(2f8.4)' ) x3, y3
        else if ( ival == 2 ) then
            write ( *, '(16x,2f8.4)' )         x4, y4
        else if ( ival == 3 ) then
            write ( *, '(4f8.4)' ) x3, y3, x4, y4
        end if

    end do

    return
    end
    subroutine test045
    !
    !*******************************************************************************
    !
    !! TEST045 tests BOX_RAY_INT_2D.
    !
    implicit none
    !
    integer, parameter :: ntest = 3
    !
    integer i
    real xa
    real xa_test(ntest)
    real xb
    real xb_test(ntest)
    real xc_test(ntest)
    real xi
    real, parameter :: xmax = 5.0E+00
    real, parameter :: xmin = 0.0E+00
    real ya
    real ya_test(ntest)
    real yb
    real yb_test(ntest)
    real yc_test(ntest)
    real yi
    real, parameter :: ymax = 3.0E+00
    real, parameter :: ymin = 0.0E+00
    !
    !  Coordinates of the test points.
    !
    xa_test(1) = 3.0E+00
    ya_test(1) = 1.0E+00

    xb_test(1) = 5.0E+00
    yb_test(1) = 5.0E+00

    xc_test(1) = 4.0E+00
    yc_test(1) = 3.0E+00


    xa_test(2) = 4.0E+00
    ya_test(2) = 1.0E+00

    xb_test(2) = 3.0E+00
    yb_test(2) = 1.0E+00

    xc_test(2) = 0.0E+00
    yc_test(2) = 1.0E+00


    xa_test(3) = 3.0E+00
    ya_test(3) = 1.0E+00

    xb_test(3) = 4.0E+00
    yb_test(3) = 2.0E+00

    xc_test(3) = 5.0E+00
    yc_test(3) = 3.0E+00
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST045'
    write ( *, '(a)' ) '  For a box with coordinate line sides in 2D,'
    write ( *, '(a)' ) '  BOX_RAY_INT_2D computes the intersection of'
    write ( *, '(a)' ) '    a shape and a ray whose origin is within'
    write ( *, '(a)' ) '    the shape.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Lower left box corner:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) xmin, ymin
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Upper right box corner:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) xmax, ymax
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        xa = xa_test(i)
        ya = ya_test(i)

        xb = xb_test(i)
        yb = yb_test(i)

        call box_ray_int_2d ( xmin, ymin, xmax, ymax, xa, ya, xb, yb, xi, yi )

        write ( *, '(a)' ) ' '
        write ( *, '(a,2f12.4)' ) '  Origin:       ', xa, ya
        write ( *, '(a,2f12.4)' ) '  Point 2:      ', xb, yb
        write ( *, '(a,2f12.4)' ) '  Intersection: ', xi, yi
        write ( *, '(a,2f12.4)' ) '  Correct:      ', xc_test(i), yc_test(i)

    end do

    return
    end
    subroutine test05
    !
    !*******************************************************************************
    !
    !! TEST05 tests PARAPP_POINT_DIST_3D.
    !
    implicit none
    !
    integer, parameter :: ntest = 7
    !
    real dist
    integer i
    real x
    real x1
    real x2
    real x3
    real x4
    real xtest(ntest)
    real y
    real y1
    real y2
    real y3
    real y4
    real ytest(ntest)
    real z
    real z1
    real z2
    real z3
    real z4
    real ztest(ntest)
    !
    !  Center of box.
    !
    xtest(1) = 1.0E+00
    ytest(1) = 4.0E+00
    ztest(1) = 0.5E+00
    !
    !  The middle of a face.
    !
    xtest(2) = 1.0E+00
    ytest(2) = 0.0E+00
    ztest(2) = 0.5E+00
    !
    !  The middle of an edge.
    !
    xtest(3) = 0.0E+00
    ytest(3) = 4.0E+00
    ztest(3) = 1.0E+00
    !
    !  A corner
    !
    xtest(4) = 2.0E+00
    ytest(4) = 8.0E+00
    ztest(4) = 1.0E+00
    !
    !  Close to a face
    !
    xtest(5) = -0.5E+00
    ytest(5) = 4.0E+00
    ztest(5) = 0.5E+00
    !
    !  Close to an edge
    !
    xtest(6) = 1.0E+00
    ytest(6) = -1.0E+00
    ztest(6) = -1.0E+00
    !
    !  Close to a corner
    !
    xtest(7) = 3.0E+00
    ytest(7) = 9.0E+00
    ztest(7) = 2.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST05'
    write ( *, '(a)' ) '  PARAPP_POINT_DIST_3D computes the distance '
    write ( *, '(a)' ) '    from a point to a box (parallelipiped) in 3D.'
    write ( *, '(a)' ) ' '

    x1 = 0.0E+00
    y1 = 0.0E+00
    z1 = 0.0E+00

    x2 = 2.0E+00
    y2 = 0.0E+00
    z2 = 0.0E+00

    x3 = 0.0E+00
    y3 = 8.0E+00
    z3 = 0.0E+00

    x4 = 0.0E+00
    y4 = 0.0E+00
    z4 = 1.0E+00

    write ( *, '(a)' ) '  The 4 box corners that are specified:'
    write ( *, '(a)' ) ' '
    write ( *, '(i2,3f10.2)' ) 1, x1, y1, z1
    write ( *, '(i2,3f10.2)' ) 2, x2, y2, z2
    write ( *, '(i2,3f10.2)' ) 3, x3, y3, z3
    write ( *, '(i2,3f10.2)' ) 4, x4, y4, z4

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' I         X,Y,Z,                 Distance to box'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x = xtest(i)
        y = ytest(i)
        z = ztest(i)

        call parapp_point_dist_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, &
            y4, z4, x, y, z, dist )

        write ( *, '(i3,3f10.2,g14.6)' ) i, x, y, z, dist

    end do

    return
    end
    subroutine test015
    !
    !*******************************************************************************
    !
    !! TEST015 tests CIRCLE_LUNE_AREA_2D.
    !! TEST015 tests CIRCLE_SECTOR_AREA_2D.
    !! TEST015 tests CIRCLE_TRIANGLE_AREA_2D.
    !
    implicit none
    !
    real area1
    real area2
    real area3
    integer i
    integer, parameter :: n_test = 12
    real r
    real theta
    !
    r = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST015'
    write ( *, '(a)' ) '  CIRCLE_LUNE_AREA_2D computes the area of a'
    write ( *, '(a)' ) '    circular lune, defined by joining the endpoints'
    write ( *, '(a)' ) '    of a circular arc.'
    write ( *, '(a)' ) '  CIRCLE_SECTOR_AREA_2D computes the area of a'
    write ( *, '(a)' ) '    circular sector, defined by joining the endpoints'
    write ( *, '(a)' ) '    of a circular arc to the center.'
    write ( *, '(a)' ) '  CIRCLE_TRIANGLE_AREA_2D computes the signed area of a'
    write ( *, '(a)' ) '    triangle, defined by joining the endpoints'
    write ( *, '(a)' ) '    of a circular arc and the center.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) &
        '      R            Theta        Sector       Triangle     Lune'
    write ( *, '(a)' ) ' '

    do i = 0, n_test

        theta = real ( i ) * 2.0E+00 * r_pi ( ) / real ( n_test )

        call circle_sector_area_2d ( r, theta, area1 )

        call circle_triangle_area_2d ( r, theta, area2 )

        call circle_lune_area_2d ( r, theta, area3 )

        write ( *, '(5f14.8)' ) r, theta, area1, area2, area3

    end do

    return
    end
    subroutine test016
    !
    !*******************************************************************************
    !
    !! TEST016 tests CIRCLE_LUNE_CENTROID_2D.
    !! TEST016 tests CIRCLE_SECTOR_CENTROID_2D.
    !
    implicit none
    !
    integer i
    integer, parameter :: n_test = 12
    real r
    real theta1
    real theta2
    real xc
    real x1
    real x2
    real yc
    real y1
    real y2
    !
    r = 2.0E+00
    xc = 5.0E+00
    yc = 3.0E+00
    theta1 = 0.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST016'
    write ( *, '(a)' ) '  CIRCLE_LUNE_CENTROID_2D computes the centroid of a'
    write ( *, '(a)' ) '    circular lune, defined by joining the endpoints'
    write ( *, '(a)' ) '    of a circular arc.'
    write ( *, '(a)' ) '  CIRCLE_SECTOR_CENTROID_2D computes the centroid of a'
    write ( *, '(a)' ) '    circular sector, defined by joining the endpoints'
    write ( *, '(a)' ) '    of a circular arc to the center.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Our circle is of radius ', r
    write ( *, '(a,2g14.6)' ) '  with center ', xc, yc
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The first angle of our lune and sector is always 0.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) &
        '                         Lune                       Sector'
    write ( *, '(a)' ) &
        '  THETA2           X             Y             X             Y'
    write ( *, '(a)' ) ' '

    do i = 0, n_test

        theta2 = real ( i ) * 2.0E+00 * r_pi ( ) / real ( n_test )

        call circle_lune_centroid_2d ( r, xc, yc, theta1, theta2, x1, y1 )

        call circle_sector_centroid_2d ( r, xc, yc, theta1, theta2, x2, y2 )

        write ( *, '(5f14.8)' ) theta2, x1, y1, x2, y2

    end do

    return
    end
    subroutine test06
    !
    !*******************************************************************************
    !
    !! TEST06 tests CIRCLE_EXP_CONTAINS_POINT_2D.
    !
    implicit none
    !
    integer inside
    character ( len = 60 ) message(-1:7)
    real x1
    real x2
    real x3
    real x4
    real y1
    real y2
    real y3
    real y4
    !
    message(-1) = 'The point is inside the circle.'
    message(0) = 'The point is on the circle.'
    message(1) = 'The point is outside the circle'
    message(2) = 'Colinear data, the point is on the line.'
    message(3) = 'Colinear data, the point is not on the line.'
    message(4) = 'Two equal data points, the point is on the line.'
    message(5) = 'Two equal data points, the point is not on the line.'
    message(6) = 'All data points equal, the point is equal.'
    message(7) = 'All data points equal, the point is not equal.'

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST06'
    write ( *, '(a)' ) '  CIRCLE_EXP_CONTAINS_POINT_2D determines if a'
    write ( *, '(a)' ) '    point lies inside a circle.'
    !
    !  This point is inside.
    !
    x1 = 4.0E+00
    y1 = 2.0E+00

    x2 = 1.0E+00
    y2 = 5.0E+00

    x3 = -2.0E+00
    y3 = 2.0E+00

    x4 = 2.0E+00
    y4 = 3.0E+00

    call circle_exp_contains_point_2d ( x1, y1, x2, y2, x3, y3, x4, y4, inside )

    write ( *, '(a)' ) ' '
    call rvec_print_2d ( x1, y1, '  Point 1' )
    call rvec_print_2d ( x2, y2, '  Point 2' )
    call rvec_print_2d ( x3, y3, '  Point 3' )
    call rvec_print_2d ( x4, y4, '  Point 4' )
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  INSIDE = ', inside
    write ( *, '(2x,a)' ) message(inside)
    !
    !  This point is actually right on the circle.
    !
    x1 = 4.0E+00
    y1 = 2.0E+00

    x2 = 1.0E+00
    y2 = 5.0E+00

    x3 = -2.0E+00
    y3 = 2.0E+00

    x4 = 1.0E+00
    y4  = -1.0E+00

    call circle_exp_contains_point_2d ( x1, y1, x2, y2, x3, y3, x4, y4, inside )

    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  X1,Y1 = ', x1, y1
    write ( *, '(a,2g14.6)' ) '  X2,Y2 = ', x2, y2
    write ( *, '(a,2g14.6)' ) '  X3,Y3 = ', x3, y3
    write ( *, '(a,2g14.6)' ) '  X4,Y4 = ', x4, y4
    write ( *, '(a,i6)' ) '  INSIDE = ', inside
    write ( *, '(2x,a)' ) message(inside)
    !
    !  This point is outside.
    !
    x1 = 4.0E+00
    y1 = 2.0E+00

    x2 = 1.0E+00
    y2 = 5.0E+00

    x3 = -2.0E+00
    y3 = 2.0E+00

    x4 = 4.0E+00
    y4 = 6.0E+00

    call circle_exp_contains_point_2d ( x1, y1, x2, y2, x3, y3, x4, y4, inside )

    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  X1,Y1 = ', x1, y1
    write ( *, '(a,2g14.6)' ) '  X2,Y2 = ', x2, y2
    write ( *, '(a,2g14.6)' ) '  X3,Y3 = ', x3, y3
    write ( *, '(a,2g14.6)' ) '  X4,Y4 = ', x4, y4
    write ( *, '(a,i6)' ) '  INSIDE = ', inside
    write ( *, '(2x,a)' ) message(inside)

    return
    end
    subroutine test07
    !
    !*******************************************************************************
    !
    !! TEST07 tests CIRCLE_EXP2IMP_2D.
    !! TEST07 tests TRIANGLE_DIAMETER_2D.
    !! TEST07 tests TRIANGLE_CIRCUMCIRCLE_2D.
    !
    implicit none
    !
    integer, parameter :: ntest = 3
    !
    real diam
    integer i
    real r
    real x1
    real x1test(ntest)
    real x2
    real x2test(ntest)
    real x3
    real x3test(ntest)
    real xc
    real y1
    real y1test(ntest)
    real y2
    real y2test(ntest)
    real y3
    real y3test(ntest)
    real yc
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST07'
    write ( *, '(a)' ) '  CIRCLE_EXP2IMP_2D computes the radius and '
    write ( *, '(a)' ) '    center of the circle through three points.'
    write ( *, '(a)' ) '  TRIANGLE_CIRCUMCIRCLE_2D computes the radius and '
    write ( *, '(a)' ) '    center of the circle through the vertices of'
    write ( *, '(a)' ) '    a triangle.'
    write ( *, '(a)' ) '  TRIANGLE_DIAMETER_2D computes the diameter of '
    write ( *, '(a)' ) '    the SMALLEST circle around the triangle.'

    x1test(1) = 4.0E+00
    y1test(1) = 2.0E+00
    x2test(1) = 1.0E+00
    y2test(1) = 5.0E+00
    x3test(1) = -2.0E+00
    y3test(1) = 2.0E+00

    x1test(2) = 4.0E+00
    y1test(2) = 2.0E+00
    x2test(2) = 5.0E+00
    y2test(2) = 4.0E+00
    x3test(2) = 6.0E+00
    y3test(2) = 6.0E+00

    x1test(3) = 4.0E+00
    y1test(3) = 2.0E+00
    x2test(3) = 1.0E+00
    y2test(3) = 5.0E+00
    x3test(3) = 4.0E+00
    y3test(3) = 2.0E+00

    do i = 1, ntest

        x1 = x1test(i)
        x2 = x2test(i)
        x3 = x3test(i)
        y1 = y1test(i)
        y2 = y2test(i)
        y3 = y3test(i)

        write ( *, '(a)' ) ' '
        write ( *, '(a,2g14.6)' ) '  X1, Y1 = ', x1, y1
        write ( *, '(a,2g14.6)' ) '  X2, Y2 = ', x2, y2
        write ( *, '(a,2g14.6)' ) '  X3, Y3 = ', x3, y3

        call circle_exp2imp_2d ( x1, y1, x2, y2, x3, y3, r, xc, yc )

        write ( *, '(a,3g14.6)' ) '  #1 R, XC, YC = ', r, xc, yc

        call triangle_circumcircle_2d ( x1, y1, x2, y2, x3, y3, r, xc, yc )

        write ( *, '(a,3g14.6)' ) '  #2 R, XC, YC = ', r, xc, yc

        call triangle_diameter_2d ( x1, y1, x2, y2, x3, y3, diam )

        write ( *, '(a,g14.6)' ) '  #3 D/2 =       ', diam / 2.0E+00

    end do

    return
    end
    subroutine test08
    !
    !*******************************************************************************
    !
    !! TEST08 tests CIRCLE_IMP_POINTS_2D;
    !! TEST08 tests POLYGON_AREA_2D.
    !
    implicit none
    !
    integer, parameter :: nmax = 24
    !
    integer i
    integer n
    real r
    real result
    real x(nmax)
    real xc
    real y(nmax)
    real yc
    !
    xc =  5.0E+00
    yc = -2.0E+00
    r = 2.0E+00
    n = 8

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST08'
    write ( *, '(a)' ) '  CIRCLE_IMP_POINTS_2D gets points on a circle;'
    write ( *, '(a)' ) '  POLYGON_AREA_2D finds the area of a polygon.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  The circle will have center ', xc, yc
    write ( *, '(a,g14.6)' ) '  and radius ', r
    write ( *, '(a,g14.6)' ) '  and hence area = ', r_pi() * r**2

    call circle_imp_points_2d ( r, xc, yc, n, x, y )

    call rvec2_print ( n, x, y, '  Sample results:' )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  For any N, the sampled points define a polygon'
    write ( *, '(a)' ) '  whose area approximates the circle area.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  N      Area'
    write ( *, '(a)' ) ' '

    do n = 3, nmax
        call circle_imp_points_2d ( r, xc, yc, n, x, y )
        call polygon_area_2d ( n, x, y, result )
        write ( *, '(i6,g14.6)' ) n, result
    end do

    return
    end
    subroutine test09
    !
    !*******************************************************************************
    !
    !! TEST09 tests CIRCLE_IMP_POINTS_ARC_2D.
    !
    implicit none
    !
    integer, parameter :: n = 5
    !
    integer i
    real r
    real theta1
    real theta2
    real x(n)
    real xc
    real y(n)
    real yc
    !
    xc =  5.0E+00
    yc = -2.0E+00
    r = 2.0E+00
    theta1 = r_pi ( ) / 2.0E+00
    theta2 = 3.0E+00 * r_pi ( ) / 2.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST09'
    write ( *, '(a)' ) '  CIRCLE_IMP_POINTS_ARC_2D returns points on a'
    write ( *, '(a)' ) '  circular arc.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  The circle will have center ', xc, yc
    write ( *, '(a,2g14.6)' ) '  and radius ', r
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  The arc extends from THETA1 = ', theta1
    write ( *, '(a,2g14.6)' ) '  to THETA2 = ', theta2

    call circle_imp_points_arc_2d ( r, xc, yc, theta1, theta2, n, x, y )

    call rvec2_print ( n, x, y, '  Sample results:' )

    return
    end
    subroutine test095
    !
    !*******************************************************************************
    !
    !! TEST095 tests CIRCLES_IMP_INT_2D;
    !! TEST095 tests CIRCLE_IMP_POINT_DIST_2D.
    !
    implicit none
    !
    integer, parameter :: ntest = 5
    !
    real d1
    real d2
    integer i
    integer num_int
    real, parameter :: r1 = 5.0E+00
    real r2
    real, parameter, dimension ( ntest ) :: r2_test = &
        (/ 0.5E+00, 5.0E+00, 3.0E+00, 3.0E+00, 5.0E+00 /)
    real x(2)
    real, parameter :: xc1 = 0.0E+00
    real xc2
    real, parameter, dimension ( ntest ) :: xc2_test = &
        (/ 5.0E+00, 7.0710678E+00, 4.0E+00, 6.0E+00, 0.0E+00 /)
    real y(2)
    real, parameter :: yc1 = 0.0E+00
    real yc2
    real, parameter, dimension ( ntest ) :: yc2_test = &
        (/ 5.0E+00, 7.0710678E+00, 0.0E+00, 0.0E+00, 0.0E+00 /)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST095'
    write ( *, '(a)' ) '  CIRCLES_IMP_INT_2D determines the intersections of'
    write ( *, '(a)' ) '    two circles in 2D.'
    write ( *, '(a)' ) '  CIRCLE_IMP_POINT_DIST_2D checks, by finding the'
    write ( *, '(a)' ) '    distance from a point to a circle.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) &
        '  The first circle will always have center ', xc1, yc1
    write ( *, '(a,g14.6)' ) '  and radius ', r1

    do i = 1, ntest

        r2 = r2_test(i)
        xc2 = xc2_test(i)
        yc2 = yc2_test(i)

        write ( *, '(a)' ) ' '
        write ( *, '(a,2g14.6)' ) '  The second circle will have center ', xc2, yc2
        write ( *, '(a,g14.6)' ) '  and radius ', r2

        call circles_imp_int_2d ( r1, xc1, yc1, r2, xc2, yc2, num_int, x, y )

        if ( num_int == 0 ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '  The circles do not intersect.'
        else if ( num_int == 1 ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '  The circles intersect at one point:'
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '    X       Y     Dist 1  Dist 2'
            write ( *, '(a)' ) ' '
            call circle_imp_point_dist_2d ( r1, xc1, yc1, x(1), y(1), d1 )
            call circle_imp_point_dist_2d ( r2, xc2, yc2, x(1), y(1), d2 )
            write ( *, '(4f8.4)' ) x(1), y(1), d1, d2
        else if ( num_int == 2 ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '  The circles intersect at two points:'
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '    X       Y     Dist 1  Dist 2'
            write ( *, '(a)' ) ' '
            call circle_imp_point_dist_2d ( r1, xc1, yc1, x(1), y(1), d1 )
            call circle_imp_point_dist_2d ( r2, xc2, yc2, x(1), y(1), d2 )
            write ( *, '(4f8.4)' ) x(1), y(1), d1, d2
            call circle_imp_point_dist_2d ( r1, xc1, yc1, x(2), y(2), d1 )
            call circle_imp_point_dist_2d ( r2, xc2, yc2, x(2), y(2), d2 )
            write ( *, '(4f8.4)' ) x(2), y(2), d1, d2
        else if ( num_int == 3 ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '  The circles coincide (infinite intersection).'
        end if

    end do

    return
    end
    subroutine test10
    !
    !*******************************************************************************
    !
    !! TEST10 tests CORPL_2D.
    !
    implicit none
    !
    real dist
    real x1
    real x2
    real x3
    real x4
    real x5
    real y1
    real y2
    real y3
    real y4
    real y5
    !
    !  These points define the lines
    !    y = 0
    !  and
    !    y = 2x-6
    !
    x1 = 0.0E+00
    y1 = 0.0E+00
    x2 = 3.0E+00
    y2 = 0.0E+00
    x3 = 4.0E+00
    y3 = 2.0E+00
    dist = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST10'
    write ( *, '(a)' ) '  CORPL_2D'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Compute (X4,Y4) and (X5,Y5), normal to '
    write ( *, '(a)' ) '  line through (X1,Y1) and (X2,Y2), and'
    write ( *, '(a)' ) '  line through (X2,Y2) and (X3,Y3), '
    write ( *, '(a)' ) '  and DIST units from (X2,Y2).'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  Point 1:', x1, y1
    write ( *, '(a,2g14.6)' ) '  Point 2:', x2, y2
    write ( *, '(a,2g14.6)' ) '  Point 3:', x3, y3
    write ( *, '(a,g14.6)' ) '  DIST = ', dist

    call corpl_2d ( dist, x1, y1, x2, y2, x3, y3, x4, y4, x5, y5 )

    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  Point 4:', x4, y4
    write ( *, '(a,2g14.6)' ) '  Point 5:', x5, y5
    !
    !  These points define the lines
    !    y = 0
    !  and
    !    y = 2x-6
    !
    x1 = 0.0E+00
    y1 = 0.0E+00
    x2 = 3.0E+00
    y2 = 0.0E+00
    x3 = 2.0E+00
    y3  = -2.0E+00
    dist = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  Point 1:', x1, y1
    write ( *, '(a,2g14.6)' ) '  Point 2:', x2, y2
    write ( *, '(a,2g14.6)' ) '  Point 3:', x3, y3
    write ( *, '(a,g14.6)' ) '  DIST = ', dist

    call corpl_2d ( dist, x1, y1, x2, y2, x3, y3, x4, y4, x5, y5 )

    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  Point 4:', x4, y4
    write ( *, '(a,2g14.6)' ) '  Point 5:', x5, y5
    !
    !  By setting (X1,Y1) = (X2,Y2), we are asking that CORPL_2D
    !  simply extend the line
    !    y = 2x-6
    !  from (X3,Y3) to (X2,Y2) through to the other side.
    !
    x1 = 3.0E+00
    y1 = 0.0E+00
    x2 = 3.0E+00
    y2 = 0.0E+00
    x3 = 2.0E+00
    y3  = -2.0E+00
    dist = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  Point 1:', x1, y1
    write ( *, '(a,2g14.6)' ) '  Point 2:', x2, y2
    write ( *, '(a,2g14.6)' ) '  Point 3:', x3, y3
    write ( *, '(a,g14.6)' ) '  DIST = ', dist

    call corpl_2d ( dist, x1, y1, x2, y2, x3, y3, x4, y4, x5, y5 )

    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  Point 4:', x4, y4
    write ( *, '(a,2g14.6)' ) '  Point 5:', x5, y5

    return
    end
    subroutine test11
    !
    !*******************************************************************************
    !
    !! TEST11 tests CUBE_SHAPE_3D;
    !! TEST11 tests DUAL_SHAPE_3D;
    !! TEST11 tests OCTAHEDRON_SHAPE_3D;
    !! TEST11 tests SHAPE_PRINT_3D.
    !
    implicit none
    !
    integer, parameter :: max_num = 20
    integer, parameter :: max_order = 6
    !
    integer face_num1
    integer face_num2
    integer face_order1
    integer face_order2
    integer face_point1(max_order,max_num)
    integer face_point2(max_order,max_num)
    integer point_num1
    integer point_num2
    real point_coord1(3,max_num)
    real point_coord2(3,max_num)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST11'
    write ( *, '(a)' ) '  CUBE_SHAPE_3D returns shape information'
    write ( *, '(a)' ) '    for the cube;'
    write ( *, '(a)' ) '  DUAL_SHAPE_3D finds the dual of a regular'
    write ( *, '(a)' ) '    polygon;'
    write ( *, '(a)' ) '  OCTAHEDRON_SHAPE_3D returns shape information'
    write ( *, '(a)' ) '    for the octahedron;'
    write ( *, '(a)' ) '  SHAPE_PRINT_3D prints 3D shape information.'
    write ( *, '(a)' ) '    polygon;'
    !
    !  Get the cube shape.
    !
    call cube_shape_3d ( max_num, max_order, point_num1, &
        face_num1, face_order1, point_coord1, face_point1 )
    !
    !  Print the shape information.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  THE CUBE:'
    write ( *, '(a)' ) ' '

    call shape_print_3d ( max_num, max_order, point_num1, &
        face_num1, face_order1, point_coord1, face_point1 )
    !
    !  Get the octahedron shape.
    !
    call octahedron_shape_3d ( max_num, max_order, point_num2, &
        face_num2, face_order2, point_coord2, face_point2 )
    !
    !  Print the shape information.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  THE OCTAHEDRON:'
    write ( *, '(a)' ) ' '

    call shape_print_3d ( max_num, max_order, point_num2, &
        face_num2, face_order2, point_coord2, face_point2 )
    !
    !  Get the dual of the cube.
    !
    call dual_shape_3d ( max_num, max_order, point_num1, &
        face_num1, face_order1, point_coord1, face_point1, &
        point_num2, face_num2, face_order2, point_coord2, &
        face_point2 )
    !
    !  Print the shape information.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  DUAL OF THE CUBE:'
    write ( *, '(a)' ) ' '

    call shape_print_3d ( max_num, max_order, point_num2, &
        face_num2, face_order2, point_coord2, face_point2 )
    !
    !  Get the dual of the dual of the cube.
    !
    call dual_shape_3d ( max_num, max_order, point_num2, &
        face_num2, face_order2, point_coord2, face_point2, &
        point_num1, face_num1, face_order1, point_coord1, face_point1 )
    !
    !  Print the shape information.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  DUAL OF THE DUAL OF THE CUBE:'
    write ( *, '(a)' ) ' '

    call shape_print_3d ( max_num, max_order, point_num1, &
        face_num1, face_order1, point_coord1, face_point1 )

    return
    end
    subroutine test12
    !
    !*******************************************************************************
    !
    !! TEST12 tests DIRECTION_PERT_3D;
    !! TEST12 tests GET_SEED.
    !
    implicit none
    !
    integer, parameter :: ntest = 3
    !
    integer i
    integer itest
    integer j
    real sigma(ntest)
    real vbase(3)
    real vran(3)
    !
    vbase(1:3) = (/ 1.0E+00,  0.0E+00, 0.0E+00 /)
    sigma(1:3) = (/ 0.99E+00, 0.5E+00, 0.1E+00 /)

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST12'
    write ( *, '(a)' ) '  DIRECTION_PERT_3D perturbs a direction vector.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Base vector:'
    write ( *, '(3f8.4)' ) vbase(1:3)

    do itest = 1, ntest

        write ( *, '(a)' ) ' '
        write ( *, '(a,g14.6)' ) '  Using Sigma = ', sigma(itest)
        write ( *, '(a)' ) ' '

        do i = 1, 20
            call direction_pert_3d ( sigma(itest), vbase, vran )
            write ( *, '(3f8.4)' ) vran(1:3)
        end do

    end do

    return
    end
    subroutine test13
    !
    !*******************************************************************************
    !
    !! TEST13 tests DIRECTION_RANDOM_3D;
    !! TEST13 tests GET_SEED.
    !
    implicit none
    !
    integer i
    integer j
    real vran(3)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST13'
    write ( *, '(a)' ) '  DIRECTION_RANDOM_3D picks a random direction vector.'
    write ( *, '(a)' ) ' '

    do i = 1, 10
        call direction_random_3d ( vran )
        write ( *, '(3f8.4)' ) vran(1:3)
    end do

    return
    end
    subroutine test14
    !
    !*******************************************************************************
    !
    !! TEST14 tests DIRECTION_RANDOM_ND;
    !! TEST14 tests GET_SEED.
    !
    implicit none
    !
    integer, parameter :: n = 4
    !
    integer i
    integer j
    real vran(n)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST14'
    write ( *, '(a)' ) '  DIRECTION_RANDOM_ND picks a random direction vector.'
    write ( *, '(a)' ) ' '

    do i = 1, 10
        call direction_random_nd ( n, vran )
        write ( *, '(4f8.4)' ) vran(1:n)
    end do

    return
    end
    subroutine test15
    !
    !*******************************************************************************
    !
    !! TEST15 tests DODEC_SHAPE_3D;
    !! TEST15 tests DUAL_SHAPE_3D;
    !! TEST15 tests ICOS_SHAPE_3D;
    !! TEST15 tests SHAPE_PRINT_3D.
    !
    implicit none
    !
    integer, parameter :: max_num = 20
    integer, parameter :: max_order = 6
    !
    integer face_num1
    integer face_num2
    integer face_order1
    integer face_order2
    integer face_point1(max_order,max_num)
    integer face_point2(max_order,max_num)
    integer point_num1
    integer point_num2
    real point_coord1(3,max_num)
    real point_coord2(3,max_num)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST15'
    write ( *, '(a)' ) '  DODEC_SHAPE_3D returns shape information'
    write ( *, '(a)' ) '    for the dodecahedron;'
    write ( *, '(a)' ) '  DUAL_SHAPE_3D finds the dual of a regular'
    write ( *, '(a)' ) '    polygon;'
    write ( *, '(a)' ) '  ICOS_SHAPE_3D returns shape information'
    write ( *, '(a)' ) '    for the icosahedron;'
    write ( *, '(a)' ) '  SHAPE_PRINT_3D prints 3D shape information.'
    write ( *, '(a)' ) '    polygon.'
    !
    !  Get the dodecahedron shape.
    !
    call dodec_shape_3d ( max_num, max_order, point_num1, &
        face_num1, face_order1, point_coord1, face_point1 )
    !
    !  Print the shape information.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  THE DODECAHEDRON:'
    write ( *, '(a)' ) ' '

    call shape_print_3d ( max_num, max_order, point_num1, &
        face_num1, face_order1, point_coord1, face_point1 )
    !
    !  Get the icosahedron shape.
    !
    call icos_shape_3d ( max_num, max_order, point_num2, &
        face_num2, face_order2, point_coord2, face_point2 )
    !
    !  Print the shape information.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  THE ICOSAHEDRON:'
    write ( *, '(a)' ) ' '

    call shape_print_3d ( max_num, max_order, point_num2, &
        face_num2, face_order2, point_coord2, face_point2 )
    !
    !  Get the dual of the cube.
    !
    call dual_shape_3d ( max_num, max_order, point_num1, face_num1, &
        face_order1, point_coord1, face_point1, point_num2, face_num2, &
        face_order2, point_coord2, face_point2 )
    !
    !  Print the shape information.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  DUAL OF THE DODECAHEDRON:'
    write ( *, '(a)' ) ' '

    call shape_print_3d ( max_num, max_order, point_num2, face_num2, &
        face_order2, point_coord2, face_point2 )
    !
    !  Get the dual of the dual of the dodecahedron.
    !
    call dual_shape_3d ( max_num, max_order, point_num2, &
        face_num2, face_order2, point_coord2, face_point2, &
        point_num1, face_num1, face_order1, point_coord1, face_point1 )
    !
    !  Print the shape information.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  DUAL OF THE DUAL OF THE DODECAHEDRON:'
    write ( *, '(a)' ) ' '

    call shape_print_3d ( max_num, max_order, point_num1, &
        face_num1, face_order1, point_coord1, face_point1 )

    return
    end
    subroutine test16
    !
    !*******************************************************************************
    !
    !! TEST16 tests DUAL_SHAPE_3D;
    !! TEST16 tests SHAPE_PRINT_3D;
    !! TEST16 tests TETRA_SHAPE_3D.
    !
    implicit none
    !
    integer, parameter :: max_num = 20
    integer, parameter :: max_order = 6
    !
    integer face_num1
    integer face_num2
    integer face_order1
    integer face_order2
    integer face_point1(max_order,max_num)
    integer face_point2(max_order,max_num)
    integer point_num1
    integer point_num2
    real point_coord1(3,max_num)
    real point_coord2(3,max_num)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST16'
    write ( *, '(a)' ) '  DUAL_SHAPE_3D finds the dual of a regular'
    write ( *, '(a)' ) '    polygon;'
    write ( *, '(a)' ) '  SHAPE_PRINT_3D prints 3D shape information.'
    write ( *, '(a)' ) '    polygon;'
    write ( *, '(a)' ) '  TETRA_SHAPE_3D returns shape information'
    write ( *, '(a)' ) '    for the tetrahedron.'
    !
    !  Get the tetrahedron shape.
    !
    call tetra_shape_3d ( max_num, max_order, point_num1, &
        face_num1, face_order1, point_coord1, face_point1 )
    !
    !  Print the shape information.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  THE TETRAHEDRON:'
    write ( *, '(a)' ) ' '

    call shape_print_3d ( max_num, max_order, point_num1, &
        face_num1, face_order1, point_coord1, face_point1 )
    !
    !  Get the dual of the tetrahedron.
    !
    call dual_shape_3d ( max_num, max_order, point_num1, &
        face_num1, face_order1, point_coord1, face_point1, &
        point_num2, face_num2, face_order2, point_coord2, face_point2 )
    !
    !  Print the shape information.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  DUAL OF THE TETRAHEDRON:'
    write ( *, '(a)' ) ' '

    call shape_print_3d ( max_num, max_order, point_num2, &
        face_num2, face_order2, point_coord2, face_point2 )
    !
    !  Get the dual of the dual of the tetrahedron.
    !
    call dual_shape_3d ( max_num, max_order, point_num2, &
        face_num2, face_order2, point_coord2, face_point2, &
        point_num1, face_num1, face_order1, point_coord1, face_point1 )
    !
    !  Print the shape information.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  DUAL OF THE DUAL OF THE TETRAHEDRON:'
    write ( *, '(a)' ) ' '

    call shape_print_3d ( max_num, max_order, point_num1, &
        face_num1, face_order1, point_coord1, face_point1 )

    return
    end
    subroutine test17
    !
    !*******************************************************************************
    !
    !! TEST17 tests ELLIPSE_POINTS_2D;
    !! TEST17 tests ELLIPSE_AREA_2D;
    !! TEST17 tests POLYGON_AREA_2D.
    !
    implicit none
    !
    integer, parameter :: nmax = 24
    !
    real area
    integer i
    integer n
    real psi
    real r1
    real r2
    real result
    real x(nmax)
    real x0
    real y(nmax)
    real y0
    !
    x0 =  5.0E+00
    y0 = -2.0E+00
    r1 = 3.0E+00
    r2 = 1.0E+00
    psi = r_pi ( ) / 6.0E+00
    n = 16

    call ellipse_area_2d ( r1, r2, area )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST17'
    write ( *, '(a)' ) '  ELLIPSE_POINTS_2D returns points on an ellipse;'
    write ( *, '(a)' ) '  ELLIPSE_AREA_2D returns the area of an ellipse;'
    write ( *, '(a)' ) '  POLYGON_1_2D finds the area of a polygon.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  The ellipse will have center ', x0, y0
    write ( *, '(a,g14.6,a,g14.6)' ) '  radii R1 = ', r1, ' R2 = ', r2
    write ( *, '(a,g14.6)' ) '  and angle PSI = ', psi
    write ( *, '(a,g14.6)' ) '  and area = ', area

    call ellipse_points_2d ( x0, y0, r1, r2, psi, n, x, y )

    call rvec2_print ( n, x, y, '  Sample points:' )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  For any N, the sampled points define a polygon'
    write ( *, '(a)' ) '  whose area approximates the ellipse area.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  N      Area'
    write ( *, '(a)' ) ' '

    do n = 3, nmax
        call ellipse_points_2d ( x0, y0, r1, r2, psi, n, x, y )
        call polygon_area_2d ( n, x, y, result )
        write ( *, '(i6,g14.6)' ) n, result
    end do

    return
    end
    subroutine test18
    !
    !*******************************************************************************
    !
    !! TEST18 tests ELLIPSE_POINTS_ARC_2D.
    !
    implicit none
    !
    integer, parameter :: n = 13
    !
    integer i
    real psi
    real r1
    real r2
    real theta1
    real theta2
    real x(n)
    real x0
    real y(n)
    real y0
    !
    x0 =   5.0E+00
    y0 = - 2.0E+00
    r1 = 3.0E+00
    r2 = 1.0E+00
    psi = r_pi ( ) / 6.0E+00
    theta1 = r_pi ( ) / 2.0E+00
    theta2 = 2.0E+00 * r_pi ( )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST18'
    write ( *, '(a)' ) '  ELLIPSE_POINTS_ARC_2D returns points on an'
    write ( *, '(a)' ) '    elliptical arc.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  The ellipse will have center ', x0, y0
    write ( *, '(a,g14.6,a,g14.6)' ) '  radii R1 = ', r1, ' R2 = ', r2
    write ( *, '(a,g14.6)' ) '  and angle PSI = ', psi
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  The arc extends from THETA1 = ', theta1
    write ( *, '(a,g14.6)') '  to THETA2 = ', theta2

    call ellipse_points_arc_2d ( x0, y0, r1, r2, psi, theta1, theta2, n, x, y )

    call rvec2_print ( n, x, y, '  Sample points:' )

    return
    end
    subroutine test1855
    !
    !*******************************************************************************
    !
    !! TEST1855 tests HALFPLANE_CONTAINS_POINT_2D
    !
    implicit none
    !
    logical expected
    logical halfplane_contains_point_2d
    integer j
    integer, parameter :: ntest = 4
    logical temp
    real x
    real xa
    real xb
    real y
    real ya
    real yb
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST1855'
    write ( *, '(a)' ) '  HALFPLANE_CONTAINS_POINT_2D determines whether a'
    write ( *, '(a)' ) '  halfplane bounded by (XA,YA):(XB,YB) contains the'
    write ( *, '(a)' ) '  point (X,Y).'
    write ( *, '(a)' ) ' '

    do j = 1, ntest

        if ( j == 1 ) then

            xa = 0.0E+00
            ya = 0.0E+00

            xb = 2.0E+00
            yb = 0.0E+00

            x = 1.0E+00
            y = 1.0E+00

            expected = .true.

        else if ( j == 2 ) then

            xa = 0.0E+00
            ya = 0.0E+00

            xb = 2.0E+00
            yb = 0.0E+00

            x =  1.0E+00
            y = -1.0E+00

            expected = .false.

        else if ( j == 3 ) then

            xa = -5.0E+00
            ya = -5.0E+00

            xb = 10.0E+00
            yb = 10.0E+00

            x = -1.0E+00
            y = +1.0E+00

            expected = .true.

        else if ( j == 4 ) then

            xa = 3.0E+00
            ya = 150.0E+00

            xb = 1.0E+00
            yb = 50.0E+00

            x =   2.0E+00
            y =  200.0E+00

            expected = .false.

        end if

        temp = halfplane_contains_point_2d ( xa, ya, xb, yb, x, y )

        write ( *, '(a)' ) ' '
        write ( *, '(a,2g14.6)' ) '(XA,YA)=', xa, ya
        write ( *, '(a,2g14.6)' ) '(XB,YB)=', xb, yb
        write ( *, '(a,2g14.6)' ) '(X,Y) = ', x,  y
        write ( *, '(a,l,a,l)' ) 'Contains? = ', temp, '  Correct = ', expected

    end do

    return
    end
    subroutine test19
    !
    !*******************************************************************************
    !
    !! TEST19 tests HALFSPACE_IMP_TRIANGLE_INT_3D.
    !
    implicit none
    !
    real a
    real b
    real c
    real d
    integer i
    integer j
    integer ntest
    integer num_int
    real x(4)
    real x1
    real x2
    real x3
    real y(4)
    real y1
    real y2
    real y3
    real z(4)
    real z1
    real z2
    real z3
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST19'
    write ( *, '(a)' ) '  HALFSPACE_IMP_TRIANGLE_INT_3D finds'
    write ( *, '(a)' ) '    intersection points of an implicit'
    write ( *, '(a)' ) '    halfspace and a triangle.'

    a =   1.0E+00
    b = - 2.0E+00
    c = - 3.0E+00
    d =   6.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The implicitly defined bounding plane'
    write ( *, '(a)' ) '  has the form: A*X + B*Y + C*Z + D = 0.'
    write ( *, '(a,4g14.6)' ) '  A,B,C,D = ', a, b, c, d

    ntest = 6

    do i = 1, ntest

        if ( i == 1 ) then
            x1 =   0.0E+00
            y1 =   0.0E+00
            z1 =   0.0E+00
            x2 =   0.0E+00
            y2 = - 1.0E+00
            z2 =   0.0E+00
            x3 =   0.0E+00
            y3 =   0.0E+00
            z3 = - 2.0E+00
        else if ( i == 2 ) then
            x1 = - 6.0E+00
            y1 =   0.0E+00
            z1 =   0.0E+00
            x2 =   0.0E+00
            y2 = - 1.0E+00
            z2 =   0.0E+00
            x3 =   0.0E+00
            y3 =   0.0E+00
            z3 = - 2.0E+00
        else if ( i == 3 ) then
            x1 =   0.0E+00
            y1 =   0.0E+00
            z1 =   0.0E+00
            x2 =   0.0E+00
            y2 =   3.0E+00
            z2 =   0.0E+00
            x3 =   0.0E+00
            y3 =   0.0E+00
            z3 =   2.0E+00
        else if ( i == 4 ) then
            x1 = - 6.0E+00
            y1 =   0.0E+00
            z1 =   0.0E+00
            x2 =   0.0E+00
            y2 =   4.0E+00
            z2 =   0.0E+00
            x3 =   0.0E+00
            y3 =   0.0E+00
            z3 =   3.0E+00
        else if ( i == 5 ) then
            x1 = - 8.0E+00
            y1 =   0.0E+00
            z1 =   0.0E+00
            x2 =   0.0E+00
            y2 = - 1.0E+00
            z2 =   0.0E+00
            x3 =   0.0E+00
            y3 =   0.0E+00
            z3 = - 2.0E+00
        else if ( i == 6 ) then
            x1 =   0.0E+00
            y1 =   0.0E+00
            z1 =   0.0E+00
            x2 =   0.0E+00
            y2 =   4.0E+00
            z2 =   0.0E+00
            x3 =   0.0E+00
            y3 =   0.0E+00
            z3 =   4.0E+00
        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,i6)' ) 'Case ', i
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Triangle vertices:'
        write ( *, '(a)' ) ' '
        write ( *, '(3g14.6)' ) x1, y1, z1
        write ( *, '(3g14.6)' ) x2, y2, z2
        write ( *, '(3g14.6)' ) x3, y3, z3

        call halfspace_imp_triangle_int_3d ( x1, y1, z1, x2, y2, z2, &
            x3, y3, z3, a, b, c, d, num_int, x, y, z )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i6)' ) '  Number of intersection points is ', num_int
        write ( *, '(a)' ) ' '

        if ( num_int > 0 ) then
            do j = 1, num_int
                write ( *, '(i6,3g14.6)' ) j, x(j), y(j), z(j)
            end do
        end if

    end do

    return
    end
    subroutine test20
    !
    !*******************************************************************************
    !
    !! TEST20 tests HALFSPACE_NORM_TRIANGLE_INT_3D.
    !
    implicit none
    !
    integer i
    integer j
    integer ntest
    integer num_int
    real x(4)
    real x1
    real x2
    real x3
    real xn
    real xp
    real y(4)
    real y1
    real y2
    real y3
    real yn
    real yp
    real z(4)
    real z1
    real z2
    real z3
    real zn
    real zp
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST20'
    write ( *, '(a)' ) '  HALFSPACE_NORM_TRIANGLE_INT_3D finds'
    write ( *, '(a)' ) '    intersection points of a normal form'
    write ( *, '(a)' ) '    halfspace and a triangle.'

    xn =   2.0E+00
    yn = - 4.0E+00
    zn = - 6.0E+00

    xp = - 6.0E+00
    yp =   0.0E+00
    zp =   0.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  A point on the plane is '
    write ( *, '(3g14.6)' ) xp, yp, zp
    write ( *, '(a)' ) '  A normal vector to the plane is '
    write ( *, '(3g14.6)' ) xn, yn, zn

    ntest = 6

    do i = 1, ntest

        if ( i == 1 ) then
            x1 =   0.0E+00
            y1 =   0.0E+00
            z1 =   0.0E+00
            x2 =   0.0E+00
            y2 = - 1.0E+00
            z2 =   0.0E+00
            x3 =   0.0E+00
            y3 =   0.0E+00
            z3 = - 2.0E+00
        else if ( i == 2 ) then
            x1 = - 6.0E+00
            y1 =   0.0E+00
            z1 =   0.0E+00
            x2 =   0.0E+00
            y2 = - 1.0E+00
            z2 =   0.0E+00
            x3 =   0.0E+00
            y3 =   0.0E+00
            z3 = - 2.0E+00
        else if ( i == 3 ) then
            x1 =   0.0E+00
            y1 =   0.0E+00
            z1 =   0.0E+00
            x2 =   0.0E+00
            y2 =   3.0E+00
            z2 =   0.0E+00
            x3 =   0.0E+00
            y3 =   0.0E+00
            z3 =   2.0E+00
        else if ( i == 4 ) then
            x1 = - 6.0E+00
            y1 =   0.0E+00
            z1 =   0.0E+00
            x2 =   0.0E+00
            y2 =   4.0E+00
            z2 =   0.0E+00
            x3 =   0.0E+00
            y3 =   0.0E+00
            z3 =   3.0E+00
        else if ( i == 5 ) then
            x1 = - 8.0E+00
            y1 =   0.0E+00
            z1 =   0.0E+00
            x2 =   0.0E+00
            y2 = - 1.0E+00
            z2 =   0.0E+00
            x3 =   0.0E+00
            y3 =   0.0E+00
            z3 = - 2.0E+00
        else if ( i == 6 ) then
            x1 =   0.0E+00
            y1 =   0.0E+00
            z1 =   0.0E+00
            x2 =   0.0E+00
            y2 =   4.0E+00
            z2 =   0.0E+00
            x3 =   0.0E+00
            y3 =   0.0E+00
            z3 =   4.0E+00
        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,i6)' ) 'Case ', i
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Triangle vertices:'
        write ( *, '(a)' ) ' '
        write ( *, '(3g14.6)' ) x1, y1, z1
        write ( *, '(3g14.6)' ) x2, y2, z2
        write ( *, '(3g14.6)' ) x3, y3, z3

        call halfspace_norm_triangle_int_3d ( x1, y1, z1, x2, y2, z2, &
            x3, y3, z3, xp, yp, zp, xn, yn, zn, num_int, x, y, z )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i6)' ) '  Number of intersection points is ', num_int
        write ( *, '(a)' ) ' '

        if ( num_int > 0 ) then
            do j = 1, num_int
                write ( *, '(i6,3g14.6)' ) j, x(j), y(j), z(j)
            end do
        end if

    end do

    return
    end
    subroutine test2055
    !
    !*******************************************************************************
    !
    !! TEST2055 tests HAVERSINE.
    !
    implicit none
    !
    real d
    real hx
    integer i
    integer, parameter :: n_test = 12
    real x
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST2055'
    write ( *, '(a)' ) '  HAVERSINE computes the haversine of an angle.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Degrees  Radians  Haversine'
    write ( *, '(a)' ) ' '

    do i = 0, n_test
        x = real ( i ) * 2.0E+00 * r_pi ( ) / real ( n_test )
        d = radians_to_degrees ( x )
        hx = haversine ( x )
        write ( *, '(2f8.4,g14.6)' ) d, x, hx
    end do

    return
    end
    subroutine test21
    !
    !*******************************************************************************
    !
    !! TEST21 tests HEXAGON_SHAPE_2D.
    !
    implicit none
    !
    real angle
    integer i
    real x
    real y
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST21'
    write ( *, '(a)' ) '  HEXAGON_SHAPE_2D: points on a unit hexagon.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Angle    X    Y '
    write ( *, '(a)' ) ' '

    do i = -10, 370, 10
        angle = real ( i )
        call hexagon_shape_2d ( angle, x, y )
        write ( *, '(3g14.6)' ) angle, x, y
    end do

    return
    end
    subroutine test2155
    !
    !*******************************************************************************
    !
    !! TEST2155 tests LINE_EXP_PERP_2D.
    !
    implicit none
    !
    integer, parameter :: ntest = 3
    !
    integer i
    real x1
    real x2
    real x3
    real x4
    real xtest(ntest)
    real y1
    real y2
    real y3
    real y4
    real ytest(ntest)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST2155'
    write ( *, '(a)' ) '  LINE_EXP_PERP_2D is given a point P and a line L,'
    write ( *, '(a)' ) '  and finds a point Q on the line L, so that PQ'
    write ( *, '(a)' ) '  is perpendicular to L.'

    x1 = 1.0E+00
    y1 = 3.0E+00

    x2 = 4.0E+00
    y2 = 0.0E+00

    xtest(1) = 0.0E+00
    ytest(1) = 0.0E+00

    xtest(2) =  5.0E+00
    ytest(2) = -1.0E+00

    xtest(3) = 5.0E+00
    ytest(3) = 3.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) 'Line point 1 = ', x1, y1
    write ( *, '(a,2g14.6)' ) 'Line point 2 = ', x2, y2

    do i = 1, ntest

        x3 = xtest(i)
        y3 = ytest(i)

        call line_exp_perp_2d ( x1, y1, x2, y2, x3, y3, x4, y4 )

        write ( *, '(a)' ) ' '
        write ( *, '(a,2g14.6)' ) 'Point P = ', x3, y3
        write ( *, '(a,2g14.6)' ) 'Point Q = ', x4, y4

    end do

    return
    end
    subroutine test22
    !
    !*******************************************************************************
    !
    !! TEST22 tests LINE_EXP_POINT_NEAR_2D.
    !
    implicit none
    !
    integer, parameter :: ntest = 3
    !
    real dist
    integer i
    real t
    real x
    real x1
    real x2
    real xn
    real xtest(ntest)
    real y
    real y1
    real y2
    real yn
    real ytest(ntest)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST22'
    write ( *, '(a)' ) '  LINE_EXP_POINT_NEAR_2D finds the point on'
    write ( *, '(a)' ) '    a line nearest in point in 2D.'

    x1 = 1.0E+00
    y1 = 3.0E+00

    x2 = 4.0E+00
    y2 = 0.0E+00

    xtest(1) = 0.0E+00
    ytest(1) = 0.0E+00

    xtest(2) =  5.0E+00
    ytest(2) = -1.0E+00

    xtest(3) = 5.0E+00
    ytest(3) = 3.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) 'Line point 1 = ', x1, y1
    write ( *, '(a,2g14.6)' ) 'Line point 2 = ', x2, y2

    do i = 1, ntest

        x = xtest(i)
        y = ytest(i)

        call line_exp_point_near_2d ( x1, y1, x2, y2, x, y, xn, yn, dist, t )

        write ( *, '(a)' ) ' '
        write ( *, '(a,2g14.6)' ) 'Point = ', x, y
        write ( *, '(a,2g14.6)' ) 'Nearest point on line = ', xn, yn
        write ( *, '(a,g14.6)' ) 'Distance = ', dist
        write ( *, '(a,g14.6)' ) 'Relative line position T = ', t

    end do

    return
    end
    subroutine test2235
    !
    !*******************************************************************************
    !
    !! TEST2235 tests LINE_IMP_POINT_DIST_2D.
    !
    implicit none
    !
    integer, parameter :: ntest = 3
    !
    real a
    real, parameter, dimension ( ntest ) :: atest = (/ &
        2.0E+00, 2.0E+00, 2.0E+00 /)
    real b
    real, parameter, dimension ( ntest ) :: btest = (/ &
        5.0E+00, 5.0E+00, 5.0E+00 /)
    real c
    real, parameter, dimension ( ntest ) :: ctest = (/ &
        3.0E+00, 3.0E+00, 3.0E+00 /)
    real dist
    integer i
    real x
    real, parameter, dimension ( ntest ) :: xtest = (/ &
        0.0E+00, 0.0E+00, 0.0E+00 /)
    real y
    real, parameter, dimension ( ntest ) :: ytest = (/ &
        6.0E+00, 5.0E+00, 4.0E+00 /)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST2235'
    write ( *, '(a)' ) '  LINE_IMP_POINT_DIST_2D finds the distance from'
    write ( *, '(a)' ) '    a point X Y to a line A * X + B * Y + C = 0.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '   X       Y       A       B       C       DIST'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        a = atest(i)
        b = btest(i)
        c = ctest(i)
        x = xtest(i)
        y = ytest(i)

        call line_imp_point_dist_2d ( a, b, c, x, y, dist )

        write ( *, '(6f8.4)' ) x, y, a, b, c, dist

    end do

    return
    end
    subroutine test2255
    !
    !*******************************************************************************
    !
    !! TEST2255 tests LINE_SEG_CONTAINS_POINT_1D.
    !
    implicit none
    !
    integer, parameter :: ntest = 4
    !
    integer itest
    real u
    real x1
    real, dimension ( ntest ) :: x1test = (/ &
        2.0E+00,  10.0E+00,  8.0E+00, 88.0E+00 /)
    real x2
    real, dimension ( ntest ) :: x2test = (/ &
        6.0E+00, -10.0E+00, 10.0E+00, 88.0E+00 /)
    real x3
    real, dimension ( ntest ) :: x3test = (/ &
        3.0E+00,   7.5, 20.0E+00,  5.0E+00 /)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST2255'
    write ( *, '(a)' ) '  LINE_SEG_CONTAINS_POINT_1D determines if a point'
    write ( *, '(a)' ) '    lies within a line segment in 1D.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     X1	  X2	 X3	U'
    write ( *, '(a)' ) ' '

    do itest = 1, ntest

        x1 = x1test(itest)
        x2 = x2test(itest)
        x3 = x3test(itest)

        call line_seg_contains_point_1d ( x1, x2, x3, u )
        write ( *, '(3f7.2,g14.6)' ) x1, x2, x3, u

    end do

    return
    end
    subroutine test23
    !
    !*******************************************************************************
    !
    !! TEST23 tests LINE_SEG_POINT_NEAR_3D.
    !
    implicit none
    !
    real dist
    real t
    real x
    real x1
    real x2
    real xn
    real y
    real y1
    real y2
    real yn
    real z
    real z1
    real z2
    real zn
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST23'
    write ( *, '(a)' ) '  LINE_SEG_POINT_NEAR_3D computes the nearest'
    write ( *, '(a)' ) '    point on a line segment, to a given point,'
    write ( *, '(a)' ) '    in 3D.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Case, T, Distance, XN, YN, ZN.'
    write ( *, '(a)' ) ' '
    !
    !  Case 1, point is nearest end of segment.
    !
    !  LS: (2,3,0) + t * (2,1,0) for t = 0 to 3.
    !  P (11,6,4)
    !  Distance is 5.
    !
    x1 = 2.0E+00
    y1 = 3.0E+00
    z1 = 0.0E+00

    x2 = 8.0E+00
    y2 = 6.0E+00
    z2 = 0.0E+00

    x = 11.0E+00
    y =  6.0E+00
    z =  4.0E+00

    call line_seg_point_near_3d ( x1, y1, z1, x2, y2, z2, x, y, z, &
        xn, yn, zn, dist, t )

    write ( *, '(i2,5f9.4)' ) 1, t, dist, xn, yn, zn
    !
    !  Case 2, point is nearest interior point of segment.
    !
    !  LS: (2,3,0) + t * (2,1,0) for t = 0 to 3.
    !  P (4,4,1)
    !  Distance is 1.
    !
    x1 = 2.0E+00
    y1 = 3.0E+00
    z1 = 0.0E+00

    x2 = 8.0E+00
    y2 = 6.0E+00
    z2 = 0.0E+00

    x = 4.0E+00
    y = 4.0E+00
    z = 1.0E+00

    call line_seg_point_near_3d ( x1, y1, z1, x2, y2, z2, x, y, z, &
        xn, yn, zn, dist, t )

    write ( *, '(i2,5f9.4)' ) 2, t, dist, xn, yn, zn
    !
    !  Case 3, point is on the line.
    !
    !  LS: (2,3,0) + t * (2,1,0) for t = 0 to 3.
    !  P (6,5,0)
    !  Distance is 0.
    !
    x1 = 2.0E+00
    y1 = 3.0E+00
    z1 = 0.0E+00

    x2 = 8.0E+00
    y2 = 6.0E+00
    z2 = 0.0E+00

    x = 6.0E+00
    y = 5.0E+00
    z = 0.0E+00

    call line_seg_point_near_3d ( x1, y1, z1, x2, y2, z2, x, y, z, &
        xn, yn, zn, dist, t )

    write ( *, '(i2,5f9.4)' ) 3, t, dist, xn, yn, zn

    return
    end
    subroutine test24
    !
    !*******************************************************************************
    !
    !! TEST24 tests LINES_EXP_ANGLE_3D;
    !! TEST24 tests LINES_EXP_DIST_3D.
    !
    implicit none
    !
    real angle
    real dist
    real x1
    real x2
    real x3
    real x4
    real y1
    real y2
    real y3
    real y4
    real z1
    real z2
    real z3
    real z4
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST24'
    write ( *, '(a)' ) '  LINES_EXP_ANGLE_3D finds the angle between'
    write ( *, '(a)' ) '    two explicit lines in 3D;'
    write ( *, '(a)' ) '  LINES_EXP_DIST_3D finds the distance between'
    write ( *, '(a)' ) '    two explicit lines in 3D.'

    x1 = 0.0E+00
    y1 = 0.0E+00
    z1 = 0.0E+00

    x2 = 1.0E+00
    y2 = 2.0E+00
    z2 = 0.0E+00

    x3 = 0.0E+00
    y3 = 3.0E+00
    z3 = 3.0E+00

    x4 = 3.0E+00
    y4 = 0.0E+00
    z4 = 3.0E+00

    call lines_exp_angle_3d ( x1, y1, z1, x2, y2, z2, &
        x3, y3, z3, x4, y4, z4, angle )

    call lines_exp_dist_3d ( x1, y1, z1, x2, y2, z2, &
        x3, y3, z3, x4, y4, z4, dist )

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6,a)' ) '  Angle between lines is ', angle, ' radians.'
    write ( *, '(a,g14.6)' ) '  Distance is ', dist

    x1 = 0.0E+00
    y1 = 0.0E+00
    z1 = 0.0E+00

    x2 = 1.0E+00
    y2 = 2.0E+00
    z2 = 0.0E+00

    x3 =  1.0E+00
    y3 =  2.0E+00
    z3 = -1.0E+00

    x4 = 1.0E+00
    y4 = 2.0E+00
    z4 = 3.0E+00

    call lines_exp_angle_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, &
        angle )

    call lines_exp_dist_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, &
        dist )

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6,a)' ) '  Angle between lines is ', angle, ' radians.'
    write ( *, '(a,g14.6)' ) '  Distance is ', dist

    return
    end
    subroutine test25
    !
    !*******************************************************************************
    !
    !! TEST25 tests LINES_EXP_INT_2D.
    !
    implicit none
    !
    integer itest
    integer ival
    real x
    real x1
    real x2
    real x3
    real x4
    real y
    real y1
    real y2
    real y3
    real y4
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST25'
    write ( *, '(a)' ) '  LINES_EXP_INT_2D finds intersections of '
    write ( *, '(a)' ) '    two explicit lines in 2D.'
    write ( *, '(a)' ) ' '

    do itest = 1, 3
        !
        !  x + 2y - 4 = 0
        !  x - y - 1 = 0
        !
        if ( itest == 1 ) then

            x1 = 0.0E+00
            y1 = 2.0E+00
            x2 = 4.0E+00
            y2 = 0.0E+00
            x3 = 0.0E+00
            y3 = -1.0E+00
            x4 = 1.0E+00
            y4 = 0.0E+00
            !
            !  x + 2y - 4 = 0
            !  2x + 4y - 1 = 0
            !
        else if ( itest == 2 ) then

            x1 = 0.0E+00
            y1 = 2.0E+00
            x2 = 4.0E+00
            y2 = 0.0E+00
            x3 = 0.0E+00
            y3 = 0.25E+00
            x4 = 0.50E+00
            y4 = 0.0E+00
            !
            !  x + 2y - 4 = 0
            !  -3x - 6y +12 = 0
            !
        else if ( itest == 3 ) then

            x1 = 0.0E+00
            y1 = 2.0E+00
            x2 = 4.0E+00
            y2 = 0.0E+00
            x3 = 0.0E+00
            y3 = 2.0E+00
            x4 = 4.0E+00
            y4 = 0.0E+00

        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,2g14.6)' ) '  X1,Y1', x1, y1
        write ( *, '(a,2g14.6)' ) '  X2,Y2', x2, y2
        write ( *, '(a)' ) ' '
        write ( *, '(a,2g14.6)' ) '  X3,Y3', x3, y3
        write ( *, '(a,2g14.6)' ) '  X4,Y4', x4, y4

        call lines_exp_int_2d ( x1, y1, x2, y2, x3, y3, x4, y4, ival, x, y )

        if ( ival == 1 ) then
            write ( *, '(a,2g14.6)' ) '  Intersection at ', x, y
        else if ( ival == 0 ) then
            write ( *, '(a)' ) '  Lines are parallel, no intersection.'
        else if ( ival == 2 ) then
            write ( *, '(a)' ) '  Lines are coincident.'
        else
            write ( *, '(a,i6)' ) '  Unknown return value of IVAL = ', ival
        end if

    end do

    return
    end
    subroutine test255
    !
    !*******************************************************************************
    !
    !! TEST255 tests LINES_IMP_DIST_2D.
    !
    implicit none
    !
    integer, parameter :: ntest = 3
    !
    real a1
    real, parameter, dimension ( ntest ) :: a1test = &
        (/  4.0E+00,  2.0E+00, 1.0E+00 /)
    real a2
    real, parameter, dimension ( ntest ) :: a2test = &
        (/  4.0E+00,  4.0E+00, 2.0E+00 /)
    real b1
    real, parameter, dimension ( ntest ) :: b1test = &
        (/ -1.0E+00, -1.0E+00, 2.0E+00 /)
    real b2
    real, parameter, dimension ( ntest ) :: b2test = &
        (/ -1.0E+00, -2.0E+00, 3.0E+00 /)
    real c1
    real, parameter, dimension ( ntest ) :: c1test = &
        (/  3.0E+00,  0.0E+00, 2.0E+00 /)
    real c2
    real, parameter, dimension ( ntest ) :: c2test = &
        (/ 12.0E+00,  6.0E+00, 1.0E+00 /)
    real dist
    integer i
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST255'
    write ( *, '(a)' ) '  LINES_IMP_DIST_3D finds the distance between'
    write ( *, '(a)' ) '    two implicit lines in 2D.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '   A1      B1      C1      A2      B2      C2   DIST'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        a1 = a1test(i)
        b1 = b1test(i)
        c1 = c1test(i)
        a2 = a2test(i)
        b2 = b2test(i)
        c2 = c2test(i)

        call lines_imp_dist_2d ( a1, b1, c1, a2, b2, c2, dist )

        write ( *, '(7f8.4)' ) a1, b1, c1, a2, b2, c2, dist

    end do

    return
    end
    subroutine test26
    !
    !*******************************************************************************
    !
    !! TEST26 tests LINES_IMP_INT_2D.
    !! TEST26 tests LINES_IMP_INT_2D.
    !
    implicit none
    !
    real a1
    real a2
    real angle
    real b1
    real b2
    real c1
    real c2
    integer ival
    real x
    real y
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST26'
    write ( *, '(a)' ) '  For two lines written in implicit form:'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  LINES_IMP_ANGLE_2D finds the angle;'
    write ( *, '(a)' ) '  LINES_IMP_INT_2D finds the intersection.'
    write ( *, '(a)' ) ' '
    !
    !  x + 2y - 4 = 0
    !
    a1 =  1.0E+00
    b1 =  2.0E+00
    c1 = -4.0E+00
    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  Line 1 coefficients:', a1, b1, c1
    !
    !  x - y - 1 = 0
    !
    a2 =  1.0E+00
    b2 = -1.0E+00
    c2 = -1.0E+00
    write ( *, '(a,3g14.6)' ) '  Line 2 coefficients:', a2, b2, c2

    call lines_imp_angle_2d ( a1, b1, c1, a2, b2, c2, angle )

    angle = radians_to_degrees ( angle )

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Angle between lines is ', angle

    call lines_imp_int_2d ( a1, b1, c1, a2, b2, c2, ival, x, y )

    if ( ival == 1 ) then
        write ( *, '(a,2g14.6)' ) '  Intersection at ', x, y
    else if ( ival == 0 ) then
        write ( *, '(a)' ) '  Lines are parallel, no intersection.'
    else if ( ival == 2 ) then
        write ( *, '(a)' ) '  Lines are coincident.'
    else
        write ( *, '(a,i6)' ) '  Unknown return value of ival = ', ival
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  Line 1 coefficients:', a1, b1, c1
    !
    !  2x + 4y - 1 = 0
    !
    a2 =  2.0E+00
    b2 = +4.0E+00
    c2 = -1.0E+00
    write ( *, '(a,3g14.6)' ) '  Line 2 coefficients:', a2, b2, c2

    call lines_imp_angle_2d ( a1, b1, c1, a2, b2, c2, angle )

    angle = radians_to_degrees ( angle )

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Angle between lines is ', angle

    call lines_imp_int_2d (a1, b1, c1, a2, b2, c2, ival, x, y )

    if ( ival == 1 ) then
        write ( *, '(a,2g14.6)' ) '  Intersection at ', x, y
    else if ( ival == 0 ) then
        write ( *, '(a)' ) '  Lines are parallel, no intersection.'
    else if ( ival == 2 ) then
        write ( *, '(a)' ) '  Lines are coincident.'
    else
        write ( *, '(a,i6)' ) '  Unknown return value of ival = ', ival
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  Line 1 coefficients:', a1, b1, c1
    !
    !  -3x - 6y +12 = 0
    !
    a2 =  -3.0E+00
    b2 =  -6.0E+00
    c2 = +12.0E+00
    write ( *, '(a,3g14.6)' ) '  Line 2 coefficients:', a2, b2, c2

    call lines_imp_angle_2d ( a1, b1, c1, a2, b2, c2, angle )

    angle = radians_to_degrees ( angle )

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Angle between lines is ', angle

    call lines_imp_int_2d  ( a1, b1, c1, a2, b2, c2, ival, x, y )

    if ( ival == 1 ) then
        write ( *, '(a,2g14.6)' ) '  Intersection at ', x, y
    else if ( ival == 0 ) then
        write ( *, '(a)' ) '  Lines are parallel, no intersection.'
    else if ( ival == 2 ) then
        write ( *, '(a)' ) '  Lines are coincident.'
    else
        write ( *, '(a,i6)' ) '  Unknown return value of ival = ', ival
    end if

    return
    end
    subroutine test27
    !
    !*******************************************************************************
    !
    !! TEST27 tests LINES_SEG_DIST_2D.
    !
    implicit none
    !
    real dist
    real x1
    real x2
    real x3
    real x4
    real y1
    real y2
    real y3
    real y4
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST27'
    write ( *, '(a)' ) '  LINES_SEG_DIST_2D computes the distance between'
    write ( *, '(a)' ) '    line segments in 2D.'
    !
    !  Case 1, parallel, not coincident.
    !
    x1 = 2.0E+00
    y1 = 3.0E+00

    x2 = 8.0E+00
    y2 = 6.0E+00

    x3 = 8.0E+00
    y3 = 3.0E+00

    x4 = 14.0E+00
    y4 =  6.0E+00

    call lines_seg_dist_2d ( x1, y1, x2, y2, x3, y3, x4, y4, dist )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Same slope, different intercepts.'
    write ( *, '(a,g14.6)' ) '  Distance = ', dist
    !
    !  Case 2, parallel, coincident, overlapping.
    !
    x1 = 2.0E+00
    y1 = 3.0E+00

    x2 = 8.0E+00
    y2 = 6.0E+00

    x3 = 4.0E+00
    y3 = 4.0E+00

    x4 = 14.0E+00
    y4 =  9.0E+00

    call lines_seg_dist_2d ( x1, y1, x2, y2, x3, y3, x4, y4, dist )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Same slope, same intercepts, overlapping.'
    write ( *, '(a,g14.6)' ) '  Distance = ', dist
    !
    !  Case 3, parallel, coincident, disjoint.
    !
    x1 = 2.0E+00
    y1 = 3.0E+00

    x2 = 8.0E+00
    y2 = 6.0E+00

    x3 = 14.0E+00
    y3 =  9.0E+00

    x4 = 16.0E+00
    y4 = 10.0E+00

    call lines_seg_dist_2d ( x1, y1, x2, y2, x3, y3, x4, y4, dist )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Same slope, same intercepts, disjoint.'
    write ( *, '(a,g14.6)' ) '  Distance = ', dist
    !
    !  Case 4, nonparallel, intersecting.
    !
    x1 = 2.0E+00
    y1 = 3.0E+00

    x2 = 8.0E+00
    y2 = 6.0E+00

    x3 =  0.0E+00
    y3 =  8.0E+00

    x4 =  5.0E+00
    y4 =  3.0E+00

    call lines_seg_dist_2d ( x1, y1, x2, y2, x3, y3, x4, y4, dist )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Different slopes, intersecting.'
    write ( *, '(a,g14.6)' ) '  Distance = ', dist
    !
    !  Case 5, nonparallel, disjoint.
    !
    x1 = 2.0E+00
    y1 = 3.0E+00

    x2 = 8.0E+00
    y2 = 6.0E+00

    x3 =  7.0E+00
    y3 =  3.0E+00

    x4 =   9.0E+00
    y4 =  -1.0E+00

    call lines_seg_dist_2d ( x1, y1, x2, y2, x3, y3, x4, y4, dist )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Different slopes, not intersecting.'
    write ( *, '(a,g14.6)' ) '  Distance = ', dist

    return
    end
    subroutine test28
    !
    !*******************************************************************************
    !
    !! TEST28 tests LINES_SEG_DIST_3D.
    !
    implicit none
    !
    real dist
    real x1
    real x2
    real x3
    real x4
    real y1
    real y2
    real y3
    real y4
    real z1
    real z2
    real z3
    real z4
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST28'
    write ( *, '(a)' ) '  LINES_SEG_DIST_3D computes the distance between'
    write ( *, '(a)' ) '    line segments in 3D.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Case   Computed    True'
    write ( *, '(a)' ) ' '
    !
    !  Case 1, parallel, not coincident.
    !
    !  LS1: (2,3,0) + t * (2,1,0) for t = 0 to 3.
    !  LS2: (11,6,4) + t * (2,1,0) for t = 0 to 3.
    !  Distance is 5.
    !
    x1 = 2.0E+00
    y1 = 3.0E+00
    z1 = 0.0E+00

    x2 = 8.0E+00
    y2 = 6.0E+00
    z2 = 0.0E+00

    x3 = 11.0E+00
    y3 =  6.0E+00
    z3 =  4.0E+00

    x4 = 17.0E+00
    y4 =  9.0E+00
    z4 =  4.0E+00

    call lines_seg_dist_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, &
        dist )

    write ( *, '(i6,2g14.6)' ) 1, dist, 5.0E+00
    !
    !  Case 2, parallel, coincident, overlapping.
    !
    !  (1,2,3) + t * ( 1,-1,2)
    !  LS1: t = 0 to t = 3.
    !  Distance is 0.
    !
    x1 = 1.0E+00
    y1 = 2.0E+00
    z1 = 3.0E+00

    x2 =  4.0E+00
    y2 = -1.0E+00
    z2 =  9.0E+00

    x3 = 3.0E+00
    y3 = 0.0E+00
    z3 = 7.0E+00

    x4 =  6.0E+00
    y4 = -3.0E+00
    z4 = 13.0E+00

    call lines_seg_dist_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, &
        dist )

    write ( *, '(i6,2g14.6)' ) 2, dist, 0.0E+00
    !
    !  Case 3, parallel, coincident, disjoint.
    !
    !  LS1: (3,4,5) + t * ( 2,2,1) for 0 <= t <= 2.
    !  LS2: (3,4,5) + t * ( 2,2,1) for 3 <= t <= 5.
    !  Distance = 3.
    !
    x1 = 3.0E+00
    y1 = 4.0E+00
    z1 = 5.0E+00

    x2 =  7.0E+00
    y2 =  8.0E+00
    z2 =  7.0E+00

    x3 =  9.0E+00
    y3 = 10.0E+00
    z3 =  8.0E+00

    x4 =  13.0E+00
    y4 =  14.0E+00
    z4 =  10.0E+00

    call lines_seg_dist_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, &
        dist )

    write ( *, '(i6,2g14.6)' ) 3, dist, 3.0E+00
    !
    !  Case 4, nonparallel, could intersect, and does intersect.
    !
    !  L1: (1,1,1) + t * (0,1,2)
    !  L2: (0,2,3) + t * (1,0,0)
    !  intersect at (1,2,3)
    !  Distance is 0.
    !
    x1 = 1.0E+00
    y1 = 1.0E+00
    z1 = 1.0E+00

    x2 = 1.0E+00
    y2 = 4.0E+00
    z2 = 7.0E+00

    x3 =  0.0E+00
    y3 =  2.0E+00
    z3 =  3.0E+00

    x4 =  5.0E+00
    y4 =  2.0E+00
    z4 =  3.0E+00

    call lines_seg_dist_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, &
        dist )

    write ( *, '(i6,2g14.6)' ) 4, dist, 0.0E+00
    !
    !  Case 5, nonparallel, could intersect, and does not intersect.
    !
    !  L1: (1,1,1) + t * (0,1,2)
    !  L2: (0,2,3) + t * (1,0,0)
    !  lines intersect at (1,2,3), line segments do not.
    !  Distance is 1.0E+00
    !
    x1 = 1.0E+00
    y1 = 1.0E+00
    z1 = 1.0E+00

    x2 = 1.0E+00
    y2 = 4.0E+00
    z2 = 7.0E+00

    x3 =  0.0E+00
    y3 =  2.0E+00
    z3 =  3.0E+00

    x4 = -5.0E+00
    y4 =  2.0E+00
    z4 =  3.0E+00

    call lines_seg_dist_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, &
        dist )

    write ( *, '(i6,2g14.6)' ) 5, dist, 1.0E+00
    !
    !  Case 6, nonparallel, can not intersect, "end-to-end".
    !
    !  L1: (2,2,1) + t * (0,1,2)  0 <= t <= 5
    !  L2: (0,0,0) + t * (-1,-1,-1) 0 <= t <= 5
    !  Distance is 3.
    !
    x1 = 2.0E+00
    y1 = 2.0E+00
    z1 = 1.0E+00

    x2 =  2.0E+00
    y2 =  7.0E+00
    z2 = 11.0E+00

    x3 =  0.0E+00
    y3 =  0.0E+00
    z3 =  0.0E+00

    x4 =  -5.0E+00
    y4 =  -5.0E+00
    z4 =  -5.0E+00

    call lines_seg_dist_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, &
        dist )

    write ( *, '(i6,2g14.6)' ) 6, dist, 3.0E+00
    !
    !  Case 7, nonparallel, can not intersect, "end-to-mid".
    !
    !  L1: (1,1,1) + t * (0,1,2) 0 <= t <= 5
    !  L2: (0,4,7) + t * (-1,0,0) 0 <= t <= 5
    !  Distance is 1.
    !
    x1 = 1.0E+00
    y1 = 1.0E+00
    z1 = 1.0E+00

    x2 =  1.0E+00
    y2 =  6.0E+00
    z2 = 11.0E+00

    x3 =  0.0E+00
    y3 =  4.0E+00
    z3 =  7.0E+00

    x4 = -5.0E+00
    y4 =  4.0E+00
    z4 =  7.0E+00

    call lines_seg_dist_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, &
        dist )

    write ( *, '(i6,2g14.6)' ) 7, dist, 1.0E+00
    !
    !  Case 8, nonparallel, can not intersect, "mid-to-mid".
    !
    !  L1: (0,5,10) + t * (1,-1,0) 0 <= t <= 5
    !  L2: (0,0,0) + t * (1,1,0) 0 <= t <= 6
    !  Distance = 10.
    !
    x1 = 0.0E+00
    y1 = 5.0E+00
    z1 = 10.0E+00

    x2 =  5.0E+00
    y2 =  0.0E+00
    z2 =  10.0E+00

    x3 = 0.0E+00
    y3 = 0.0E+00
    z3 = 0.0E+00

    x4 = 6.0E+00
    y4 = 6.0E+00
    z4 = 0.0E+00

    call lines_seg_dist_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, &
        dist )

    write ( *, '(i6,2g14.6)' ) 8, dist, 10.0E+00
    !
    !  Case 9, nonparallel, can not intersect, "mid-to-end".
    !
    !  L1: (-2,0,0) + t * (1,0,0) 0 <= t <= 12
    !  L2: (-2,8,1) + t * (9,-4,-1) 0 <= t <= 1
    !  Distance = 4.
    !
    x1 = -2.0E+00
    y1 = 0.0E+00
    z1 = 0.0E+00

    x2 = 10.0E+00
    y2 =  0.0E+00
    z2 =  0.0E+00

    x3 = -2.0E+00
    y3 = 8.0E+00
    z3 = 1.0E+00

    x4 = 7.0E+00
    y4 = 4.0E+00
    z4 = 0.0E+00

    call lines_seg_dist_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, &
        dist )

    write ( *, '(i6,2g14.6)' ) 9, dist, 4.0E+00

    return
    end
    subroutine test284
    !
    !*******************************************************************************
    !
    !! TEST284 tests LINES_SEG_INT_1D.
    !
    implicit none
    !
    integer, parameter :: ntest = 7
    !
    integer flag
    integer itest
    real x1
    real x2
    real x3
    real, dimension ( ntest ) :: x3test = &
        (/ -1.0E+00, 3.0E+00, 1.0E+00, 0.5E+00, 0.25E+00, 0.5E+00, 2.0E+00 /)
    real x4
    real, dimension ( ntest ) :: x4test = &
        (/  1.0E+00, 2.0E+00, 2.0E+00, -3.0E+00, 0.50E+00, 0.5E+00, 2.0E+00 /)
    real x5
    real x6
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST284'
    write ( *, '(a)' ) '  LINES_SEG_INT_1D searches for an intersection of two'
    write ( *, '(a)' ) '  line segments in 1D.'

    x1 = -1.0E+00
    x2 = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  All tests use the same line segment 1:'
    write ( *, '(a,2g14.6)' ) '  (X1,X2) = ', x1, x2

    do itest = 1, ntest

        x3 = x3test(itest)
        x4 = x4test(itest)

        write ( *, '(a)' ) ' '
        write ( *, '(a,i6)' ) '  Test ', itest
        write ( *, '(a,2g14.6)' ) '  (X3,X4) = ', x3, x4

        call lines_seg_int_1d ( x1, x2, x3, x4, flag, x5, x6 )

        if ( flag == 0 ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '  The line segments do not intersect.'
        else
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '  The line segments intersect:'
            write ( *, '(a,2g14.6)' ) x5, x6
        end if

    end do

    return
    end
    subroutine test285
    !
    !*******************************************************************************
    !
    !! TEST285 tests LINES_SEG_INT_2D.
    !
    implicit none
    !
    integer, parameter :: ntest = 4
    !
    integer flag
    integer itest
    real x1
    real x2
    real x3
    real, dimension ( ntest ) :: x3test = (/ &
        -1.0E+00, 3.0E+00, 0.0E+00, 1.0E+00 /)
    real x4
    real, dimension ( ntest ) :: x4test = (/  1.0E+00, 2.0E+00, 0.0E+00, 3.0E+00 /)
    real x5
    real y1
    real y2
    real y3
    real, dimension ( ntest ) :: y3test = (/  1.0E+00, -1.0E+00, 0.0E+00, 2.0E+00 /)
    real y4
    real, dimension ( ntest ) :: y4test = (/ -1.0E+00,  0.0E+00, 9.0E+00, 2.0E+00 /)
    real y5
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST285'
    write ( *, '(a)' ) '  LINES_SEG_INT_2D searches for an intersection of two'
    write ( *, '(a)' ) '  line segments in 2D.'

    x1 = -1.0E+00
    y1 = 3.0E+00

    x2 = 1.0E+00
    y2 = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  All tests use the same line segment 1:'
    write ( *, '(a,2g14.6)' ) '  (X1,Y1) = ', x1, y1
    write ( *, '(a,2g14.6)' ) '  (X2,Y2) = ', x2, y2

    do itest = 1, ntest

        x3 = x3test(itest)
        y3 = y3test(itest)
        x4 = x4test(itest)
        y4 = y4test(itest)

        write ( *, '(a)' ) ' '
        write ( *, '(a,i6)' ) '  Test ', itest
        write ( *, '(a,2g14.6)' ) '  (X3,Y3) = ', x3, y3
        write ( *, '(a,2g14.6)' ) '  (X4,Y4) = ', x4, y4

        call lines_seg_int_2d ( x1, y1, x2, y2, x3, y3, x4, y4, flag, x5, y5 )

        if ( flag == 0 ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '  The line segments do not intersect.'
        else if ( flag == 1 ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '  The line segments intersect:'
            write ( *, '(a,2g14.6)' ) x5, y5
        end if

    end do

    return
    end
    subroutine test29
    !
    !*******************************************************************************
    !
    !! TEST29 tests MINABS.
    !
    implicit none
    !
    real x1
    real x2
    real x3
    real xmin
    real y1
    real y2
    real y3
    real ymin

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST29'
    write ( *, '(a)' ) '  MINABS finds the minimum of a function'
    write ( *, '(a)' ) '    F(X) = a * ABS ( X ) + B'
    write ( *, '(a)' ) '  within an interval, given three data points.'
    !
    !  Case 1: the three points lie on a straight line.
    !  (XMIN=9,YMIN=2).
    !
    x1 = 14.0E+00
    y1 = 7.0E+00

    x2 = 9.0E+00
    y2 = 2.0E+00

    x3 = 12.0E+00
    y3 = 5.0E+00

    call minabs ( x1, y1, x2, y2, x3, y3, xmin, ymin )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The points lie on a straight line.'
    write ( *, '(a,2g14.6)' ) '  XMIN, YMIN = ', xmin, ymin
    !
    !  Case 2: the three points straddle a minimum.
    !  (XMIN=7, YMIN=2).
    !
    x1 = 3.0E+00
    y1 = 6.0E+00

    x2 = 12.0E+00
    y2 = 7.0E+00

    x3 = 9.0E+00
    y3 = 4.0E+00

    call minabs ( x1, y1, x2, y2, x3, y3, xmin, ymin )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The points straddle a minimum.'
    write ( *, '(a,2g14.6)' ) '  XMIN, YMIN = ', xmin, ymin
    !
    !  Case 3: the three points straddle a maximum.
    !  (XMIN=2, YMIN=5).
    !
    x1 = 11.0E+00
    y1 = 6.0E+00

    x2 = 6.0E+00
    y2 = 9.0E+00

    x3 = 2.0E+00
    y3 = 5.0E+00

    call minabs ( x1, y1, x2, y2, x3, y3, xmin, ymin )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The points straddle a maximum.'
    write ( *, '(a,2g14.6)' ) '  XMIN, YMIN = ', xmin, ymin

    return
    end
    subroutine test30
    !
    !*******************************************************************************
    !
    !! TEST30 tests MINQUAD.
    !
    implicit none
    !
    real x1
    real x2
    real x3
    real xmin
    real y1
    real y2
    real y3
    real ymin

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST30'
    write ( *, '(a)' ) '  MINQUAD finds the minimum of a function'
    write ( *, '(a)' ) '    F(X) = A * X**2 + B * X + C'
    write ( *, '(a)' ) '    within an interval, given three data points.'
    !
    !  Case 1: a minimum is in the interval.
    !  y = ( x - 1 )**2 + 4
    !
    x1 = 0.0E+00
    y1 = ( x1 - 1.0E+00 )**2 + 4.0E+00

    x2 = 2.0E+00
    y2 = ( x2 - 1.0E+00 )**2 + 4.0E+00

    x3 = 3.0E+00
    y3 = ( x3 - 1.0E+00 )**2 + 4.0E+00

    call minquad ( x1, y1, x2, y2, x3, y3, xmin, ymin )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The minimum lies in the interval.'
    write ( *, '(a,2g14.6)' ) '  X1,   Y1   = ', x1, y1
    write ( *, '(a,2g14.6)' ) '  X2,   Y2   = ', x2, y2
    write ( *, '(a,2g14.6)' ) '  X3,   Y3   = ', x3, y3
    write ( *, '(a,2g14.6)' ) '  XMIN, YMIN = ', xmin, ymin
    !
    !  Case 2: the minimum is to the left of the interval.
    !  y = ( x - 1 )**2 + 4
    !
    x1 = 2.0E+00
    y1 = ( x1 - 1.0E+00 )**2 + 4.0E+00

    x2 = 4.0E+00
    y2 = ( x2 - 1.0E+00 )**2 + 4.0E+00

    x3 = 5.0E+00
    y3 = ( x3 - 1.0E+00 )**2 + 4.0E+00

    call minquad ( x1, y1, x2, y2, x3, y3, xmin, ymin )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The minimum is to the left of the interval'
    write ( *, '(a,2g14.6)' ) '  X1,   Y1   = ', x1, y1
    write ( *, '(a,2g14.6)' ) '  X2,   Y2   = ', x2, y2
    write ( *, '(a,2g14.6)' ) '  X3,   Y3   = ', x3, y3
    write ( *, '(a,2g14.6)' ) '  XMIN, YMIN = ', xmin, ymin
    !
    !  Case 3: the function is flat.
    !
    x1 = 11.0E+00
    y1 = 6.0E+00

    x2 = 6.0E+00
    y2 = 6.0E+00

    x3 = 2.0E+00
    y3 = 6.0E+00

    call minquad ( x1, y1, x2, y2, x3, y3, xmin, ymin )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The function is flat.'
    write ( *, '(a,2g14.6)' ) '  X1,   Y1   = ', x1, y1
    write ( *, '(a,2g14.6)' ) '  X2,   Y2   = ', x2, y2
    write ( *, '(a,2g14.6)' ) '  X3,   Y3   = ', x3, y3
    write ( *, '(a,2g14.6)' ) '  XMIN, YMIN = ', xmin, ymin
    !
    !  Case 4: the function has a maximum.
    !  y = - ( x - 1 )**2 + 4
    !
    x1 = 0.0E+00
    y1 = - ( x1 - 1.0E+00 )**2 + 4.0E+00

    x2 = 2.0E+00
    y2 = - ( x2 - 1.0E+00 )**2 + 4.0E+00

    x3 = 3.0E+00
    y3 = - ( x3 - 1.0E+00 )**2 + 4.0E+00

    call minquad ( x1, y1, x2, y2, x3, y3, xmin, ymin )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The function has a maximum.'
    write ( *, '(a,2g14.6)' ) '  X1,   Y1   = ', x1, y1
    write ( *, '(a,2g14.6)' ) '  X2,   Y2   = ', x2, y2
    write ( *, '(a,2g14.6)' ) '  X3,   Y3   = ', x3, y3
    write ( *, '(a,2g14.6)' ) '  XMIN, YMIN = ', xmin, ymin

    return
    end
    subroutine test32
    !
    !*******************************************************************************
    !
    !! TEST32 tests PARA_CONTAINS_POINT_2D.
    !
    implicit none
    !
    integer, parameter :: ntest = 4
    !
    integer i
    logical inside
    real x
    real x1
    real x2
    real x3
    real xtest(ntest)
    real y
    real y1
    real y2
    real y3
    real ytest(ntest)
    !
    !  In
    !
    xtest(1) = 1.0E+00
    ytest(1) = 0.5E+00
    !
    !  Out
    !
    xtest(2) = 2.0E+00
    ytest(2) = 0.0E+00
    !
    !  Out
    !
    xtest(3) = 0.5E+00
    ytest(3) = -0.1E+00
    !
    !  Out
    !
    xtest(4) = 0.1E+00
    ytest(4) = 0.5E+00

    x1 = 0.0E+00
    y1 = 0.0E+00

    x2 = 1.0E+00
    y2 = 0.0E+00

    x3 = 1.0E+00
    y3 = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST32'
    write ( *, '(a)' ) '  PARA_CONTAINS_POINT_2D determines if a point '
    write ( *, '(a)' ) '    is within a parallelogram in 2D.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  X, Y,  Inside?'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x = xtest(i)
        y = ytest(i)

        call para_contains_point_2d ( x1, y1, x2, y2, x3, y3, x, y, inside )

        write ( *, '(1x,2g14.6,2x,l1)' ) x, y, inside

    end do

    return
    end
    subroutine test33
    !
    !*******************************************************************************
    !
    !! TEST33 tests PARA_CONTAINS_POINT_3D.
    !
    implicit none
    !
    integer, parameter :: ntest = 5
    !
    integer i
    logical inside
    real x
    real x1
    real x2
    real x3
    real xtest(ntest)
    real y
    real y1
    real y2
    real y3
    real ytest(ntest)
    real z
    real z1
    real z2
    real z3
    real ztest(ntest)
    !
    !  In
    !
    xtest(1) = 1.0E+00
    ytest(1) = 1.0E+00
    ztest(1) = 0.5E+00
    !
    !  Out
    !
    xtest(2) = 3.0E+00
    ytest(2) = 3.0E+00
    ztest(2) = 0.0E+00
    !
    !  Out
    !
    xtest(3) = 0.5E+00
    ytest(3) = 0.5E+00
    ztest(3) = -0.1E+00
    !
    !  Out
    !
    xtest(4) = 0.1E+00
    ytest(4) = 0.1E+00
    ztest(4) = 0.5E+00
    !
    !  Out
    !
    xtest(5) = 1.5E+00
    ytest(5) = 1.6E+00
    ztest(5) = 0.5E+00

    x1 = 0.0E+00
    y1 = 0.0E+00
    z1 = 0.0E+00

    x2 = 2.0E+00
    y2 = 2.0E+00
    z2 = 0.0E+00

    x3 = 1.0E+00
    y3 = 1.0E+00
    z3 = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST33'
    write ( *, '(a)' ) '  PARA_CONTAINS_POINT_3D determines if a point '
    write ( *, '(a)' ) '    is within a parallelogram in 3D.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  X, Y, Z,  Inside?'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x = xtest(i)
        y = ytest(i)
        z = ztest(i)

        call para_contains_point_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x, y, z, &
            inside )

        write ( *, '(1x,3g14.6,2x,l1)' ) x, y, z, inside

    end do

    return
    end
    subroutine test34
    !
    !*******************************************************************************
    !
    !! TEST34 tests PLANE_EXP_NORMAL_3D.
    !
    implicit none
    !
    real x1
    real x2
    real x3
    real xn
    real y1
    real y2
    real y3
    real yn
    real z1
    real z2
    real z3
    real zn
    !
    x1 = -10.56E+00
    y1 = -10.56E+00
    z1 = 78.09E+00

    x2 = 44.66E+00
    y2 = -65.77E+00
    z2 = 0.0E+00

    x3 = 44.66E+00
    y3 = 44.66E+00
    z3 = 0.0E+00

    call plane_exp_normal_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, xn, yn, zn )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST34'
    write ( *, '(a)' ) '  PLANE_EXP_NORMAL_3D finds the normal '
    write ( *, '(a)' ) '    to a plane.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (X,Y,Z) coordinates of 3 points:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) x1, y1, z1
    write ( *, '(3g14.6)' ) x2, y2, z2
    write ( *, '(3g14.6)' ) x3, y3, z3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (X,Y,Z) coordinates of unit normal vector:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) xn, yn, zn

    return
    end
    subroutine test35
    !
    !*******************************************************************************
    !
    !! TEST35 tests PLANE_EXP2IMP_3D.
    !
    implicit none
    !
    real a
    real b
    real c
    real d
    real x1
    real x2
    real x3
    real y1
    real y2
    real y3
    real z1
    real z2
    real z3
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST35'
    write ( *, '(a)' ) '  PLANE_EXP2IMP_3D puts a plane defined by '
    write ( *, '(a)' ) '    3 points into A*X+B*Y+C*Z+D = 0 form.'
    write ( *, '(a)' ) ' '

    x1 = -1.0E+00
    y1 = 0.0E+00
    z1 = -1.0E+00

    x2 = -4.0E+00
    y2 = 0.0E+00
    z2 = 0.0E+00

    x3 = -20.0E+00
    y3 = 2.0E+00
    z3 = 4.0E+00

    call plane_exp2imp_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, a, b, c, d )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  P1, P2, P3:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) x1, y1, z1
    write ( *, '(3g14.6)' ) x2, y2, z2
    write ( *, '(3g14.6)' ) x3, y3, z3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (A,B,C,D)= '
    write ( *, '(4g14.6)' ) a, b, c, d
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Correct answer is a multiple of 1, 2, 3, 4.'

    x1 = -16.0E+00
    y1 = 2.0E+00
    z1 = 4.0E+00

    x2 = 0.0E+00
    y2 = 0.0E+00
    z2 = 0.0E+00

    x3 = 4.0E+00
    y3 = -2.0E+00
    z3 = 0.0E+00

    call plane_exp2imp_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, a, b, c, d )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  P1, P2, P3:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) x1, y1, z1
    write ( *, '(3g14.6)' ) x2, y2, z2
    write ( *, '(3g14.6)' ) x3, y3, z3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (A,B,C,D)= '
    write ( *, '(a)' ) ' '
    write ( *, '(4g14.6)' ) a, b, c, d
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Correct answer is a multiple of 1, 2, 3, 0.'

    return
    end
    subroutine test36
    !
    !*******************************************************************************
    !
    !! TEST36 tests PLANE_EXP2NORM_3D.
    !
    implicit none
    !
    real x1
    real x2
    real x3
    real xn
    real xp
    real y1
    real y2
    real y3
    real yn
    real yp
    real z1
    real z2
    real z3
    real zn
    real zp
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST36'
    write ( *, '(a)' ) '  PLANE_EXP2NORM_3D puts a plane defined by '
    write ( *, '(a)' ) '    3 points into point, normal form'
    write ( *, '(a)' ) ' '

    x1 = -1.0E+00
    y1 = 0.0E+00
    z1 = -1.0E+00

    x2 = -4.0E+00
    y2 = 0.0E+00
    z2 = 0.0E+00

    x3 = -20.0E+00
    y3 = 2.0E+00
    z3 = 4.0E+00

    call plane_exp2norm_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, xp, yp, zp, &
        xn, yn, zn )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'P1, P2, P3:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) x1, y1, z1
    write ( *, '(3g14.6)' ) x2, y2, z2
    write ( *, '(3g14.6)' ) x3, y3, z3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Point, Normal:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) xp, yp, zp
    write ( *, '(3g14.6)' ) xn, yn, zn

    x1 = -16.0E+00
    y1 = 2.0E+00
    z1 = 4.0E+00

    x2 = 0.0E+00
    y2 = 0.0E+00
    z2 = 0.0E+00

    x3 = 4.0E+00
    y3 = -2.0E+00
    z3 = 0.0E+00

    call plane_exp2norm_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, xp, yp, zp, &
        xn, yn, zn )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'P1, P2, P3:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) x1, y1, z1
    write ( *, '(3g14.6)' ) x2, y2, z2
    write ( *, '(3g14.6)' ) x3, y3, z3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Point, Normal:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) xp, yp, zp
    write ( *, '(3g14.6)' ) xn, yn, zn

    return
    end
    subroutine test37
    !
    !*******************************************************************************
    !
    !! TEST37 tests PLANE_EXP_PROJECT_3D.
    !
    implicit none
    !
    integer, parameter :: ntest = 5
    !
    integer i
    integer ivis(ntest)
    real x1
    real x2
    real x3
    real xf
    real xo(ntest)
    real xp(ntest)
    real y1
    real y2
    real y3
    real yf
    real yo(ntest)
    real yp(ntest)
    real z1
    real z2
    real z3
    real zf
    real zo(ntest)
    real zp(ntest)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST37'
    write ( *, '(a)' ) '  PLANE_EXP_PROJECT_3D projects a point through'
    write ( *, '(a)' ) '    a focus point into a plane.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (XO,YO,ZO), (XP,YP,ZP), IVIS'
    write ( *, '(a)' ) ' '

    x1 = 1.0E+00
    y1 = 0.0E+00
    z1 = 0.0E+00

    x2 = 0.0E+00
    y2 = 1.0E+00
    z2 = 0.0E+00

    x3 = 0.0E+00
    y3 = 0.0E+00
    z3 = 1.0E+00

    xf = 0.0E+00
    yf = 0.0E+00
    zf = 0.0E+00
    !
    !  Projection is ( 0, 0.5, 0.5 ), IVIS is 3.
    !
    xo(1) = 0.0E+00
    yo(1) = 2.0E+00
    zo(1) = 2.0E+00
    !
    !  Projection is ( 4, 5, -8 ), IVIS is 2.
    !
    xo(2) = 4.0E+00
    yo(2) = 5.0E+00
    zo(2) = -8.0E+00
    !
    !  Projection is ( 0.33, 0.33, 0.33), IVIS is 1.
    !
    xo(3) = 0.25
    yo(3) = 0.25
    zo(3) = 0.25
    !
    !  "Projection" is ( 0, 0, 0 ), IVIS is 0.
    !
    xo(4) = 5.0E+00
    yo(4) = -2.0E+00
    zo(4) = -3.0E+00
    !
    !  Projection is ( 1, 0, 0 ), IVIS is -1.
    !
    xo(5) = -2.0E+00
    yo(5) = 0.0E+00
    zo(5) = 0.0E+00

    call plane_exp_project_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, &
        xf, yf, zf, ntest, xo, yo, zo, xp, yp, zp, ivis )

    do i = 1, ntest
        write ( *, '(1x,6g12.4,i4)' ) xo(i), yo(i), zo(i), xp(i), yp(i), zp(i), &
            ivis(i)
    end do

    return
    end
    subroutine test38
    !
    !*******************************************************************************
    !
    !! TEST38 tests PLANE_IMP2EXP_3D.
    !
    implicit none
    !
    real a
    real b
    real c
    real d
    real x1
    real x2
    real x3
    real y1
    real y2
    real y3
    real z1
    real z2
    real z3
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST38'
    write ( *, '(a)' ) '  PLANE_IMP2EXP_3D converts a plane in implicit'
    write ( *, '(a)' ) '    (A,B,C,D) form to explicit form.'

    a = 1.0E+00
    b = -2.0E+00
    c = -3.0E+00
    d = 6.0E+00

    call plane_imp2exp_3d ( a, b, c, d, x1, y1, z1, x2, y2, z2, x3, y3, z3 )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (A,B,C,D) = '
    write ( *, '(4g14.6)' ) a, b, c, d
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Output:'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  (X1,Y1,Z1) = ', x1, y1, z1
    write ( *, '(a,3g14.6)' ) '  (X2,Y2,Z2) = ', x2, y2, z2
    write ( *, '(a,3g14.6)' ) '  (X3,Y3,Z3) = ', x3, y3, z3

    return
    end
    subroutine test39
    !
    !*******************************************************************************
    !
    !! TEST39 tests PLANE_IMP2NORM_3D.
    !
    implicit none
    !
    real a
    real b
    real c
    real d
    real xn
    real xp
    real yn
    real yp
    real zn
    real zp
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST39'
    write ( *, '(a)' ) '  PLANE_IMP2NORM_3D converts a plane in implicit'
    write ( *, '(a)' ) '    (A,B,C,D) form to point, normal form.'

    a = 1.0E+00
    b = -2.0E+00
    c = -3.0E+00
    d = 6.0E+00

    call plane_imp2norm_3d ( a, b, c, d, xp, yp, zp, xn, yn, zn )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Input:'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (A,B,C,D) = ', a, b, c, d
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Output:'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  (XP,YP,ZP) = ', xp, yp, zp
    write ( *, '(a,3g14.6)' ) '  (XN,YN,ZN) = ', xn, yn, zn

    return
    end
    subroutine test40
    !
    !*******************************************************************************
    !
    !! TEST40 tests PLANE_IMP_LINE_PAR_INT_3D.
    !
    implicit none
    !
    real a
    real b
    real c
    real d
    real f
    real g
    real h
    logical intersect
    real x
    real x0
    real y
    real y0
    real z
    real z0
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST40'
    write ( *, '(a)' ) '  PLANE_IMP_LINE_PAR_INT_3D finds the '
    write ( *, '(a)' ) '    intersection of an implicit plane and'
    write ( *, '(a)' ) '    a parametric line, in 3D.'

    a = 1.0E+00
    b = -2.0E+00
    c = -3.0E+00
    d = 6.0E+00

    f = 2.0E+00
    g = 1.0E+00
    h = 5.0E+00
    x0 = 3.0E+00
    y0 = 0.0E+00
    z0 = -7.0E+00

    call plane_imp_line_par_int_3d ( a, b, c, d, x0, y0, z0, f, g, h, &
        intersect, x, y, z )

    if ( intersect ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'The plane and line intersect at '
        write ( *, '(3g14.6)' ) x, y, z
    else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'The plane and the line do not intersect.'
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Expected answer:'
    write ( *, '(a)' ) '  The plane and line intersect at '
    write ( *, '(a)' ) '  7, 2, 3.'

    return
    end
    subroutine test41
    !
    !*******************************************************************************
    !
    !! TEST41 tests PLANE_IMP_LINE_SEG_NEAR_3D.
    !
    implicit none
    !
    real a
    real b
    real c
    real d
    real dist
    integer i
    real x1
    real x2
    real xls
    real xp
    real y1
    real y2
    real yls
    real yp
    real z1
    real z2
    real zls
    real zp
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST41'
    write ( *, '(a)' ) '  PLANE_IMP_LINE_SEG_NEAR_3D finds the point'
    write ( *, '(a)' ) '    on a line segment nearest a plane.'

    do i = 1, 2

        x1 = 3.0E+00
        y1 = 0.0E+00
        z1 = -7.0E+00

        if ( i == 1 ) then
            x2 = 9.0E+00
            y2 = 3.0E+00
            z2 = 8.0E+00
        else if ( i == 2 ) then
            x2 = 5.0E+00
            y2 = 1.0E+00
            z2 = -2.0E+00
        end if

        a = 1.0E+00
        b = -2.0E+00
        c = -3.0E+00
        d = 6.0E+00

        call plane_imp_line_seg_near_3d ( x1, y1, z1, x2, y2, z2, &
            a, b, c, d, dist, xp, yp, zp, xls, yls, zls )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'The distance between the plane and the'
        write ( *, '(a,g14.6)' ) 'line segment is ', dist
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'A nearest point on the line segment is '
        write ( *, '(3g14.6)' ) xls, yls, zls
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'A nearest point on the plane is '
        write ( *, '(3g14.6)' ) xp, yp, zp

    end do

    return
    end
    subroutine test42
    !
    !*******************************************************************************
    !
    !! TEST42 tests PLANE_IMP_POINT_DIST_3D;
    !! TEST42 tests PLANE_IMP_POINT_DIST_SIGNED_3D;
    !
    implicit none
    !
    integer, parameter :: ntest = 4
    !
    real a
    real b
    real c
    real d
    real dist
    real dist_signed
    integer i
    real x
    real xtest(ntest)
    real y
    real ytest(ntest)
    real z
    real ztest(ntest)
    !
    !  This is the plane Z = 10.
    !
    a =    0.0E+00
    b =    0.0E+00
    c =    1.0E+00
    d = - 10.0E+00

    xtest(1) = - 12.0E+00
    ytest(1) =   14.0E+00
    ztest(1) =    0.0E+00

    xtest(2) =    7.0E+00
    ytest(2) =    8.0E+00
    ztest(2) =    9.0E+00

    xtest(3) =    1.0E+00
    ytest(3) =    2.0E+00
    ztest(3) =   10.0E+00

    xtest(4) =    0.0E+00
    ytest(4) =    0.0E+00
    ztest(4) =   12.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST42'
    write ( *, '(a)' ) '  PLANE_IMP_POINT_DIST_3D computes the distance'
    write ( *, '(a)' ) '    between an implicit plane and a point in 3D;'
    write ( *, '(a)' ) '  PLANE_IMP_POINT_DIST_SIGNED 3D computes the '
    write ( *, '(a)' ) '    signed distance between an implicit plane '
    write ( *, '(a)' ) '    and a point in 3D.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  For all tests, we use the implicit plane with'
    write ( *, '(a,4g14.6)' ) '  (A,B,C,D) = ', a, b, c, d
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (X,Y,Z)  DISTANCE   SIGNED_DISTANCE'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x = xtest(i)
        y = ytest(i)
        z = ztest(i)

        call plane_imp_point_dist_3d ( a, b, c, d, x, y, z, dist )

        call plane_imp_point_dist_signed_3d ( a, b, c, d, x, y, z, dist_signed )

        write ( *, '(5g14.6)' ) x, y, z, dist, dist_signed

    end do

    return
    end
    subroutine test43
    !
    !*******************************************************************************
    !
    !! TEST43 tests PLANE_IMP_TRIANGLE_NEAR_3D.
    !
    implicit none
    !
    real a
    real b
    real c
    real d
    real dist
    integer i
    integer j
    integer num_near
    real x(6)
    real x1
    real x2
    real x3
    real y(6)
    real y1
    real y2
    real y3
    real z(6)
    real z1
    real z2
    real z3
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST43'
    write ( *, '(a)' ) '  PLANE_IMP_TRIANGLE_NEAR_3D finds the'
    write ( *, '(a)' ) '    nearest points on an implicit plane and'
    write ( *, '(a)' ) '    a triangle.'

    a = 1.0E+00
    b = -2.0E+00
    c = -3.0E+00
    d = 6.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Implicit plane: A*X + B*Y + C*Z + D = 0.'
    write ( *, '(a)' ) '  A,B,C,D = '
    write ( *, '(4g14.6)' ) a, b, c, d
    x1 = 3.0E+00
    y1 = 0.0E+00
    z1 = -7.0E+00

    x2 = 13.0E+00
    y2 = -4.0E+00
    z2 = -1.0E+00

    do i = 1, 2

        if ( i == 1 ) then
            x3 = 5.0E+00
            y3 = 1.0E+00
            z3 = -2.0E+00
        else if ( i == 2 ) then
            x3 = 9.0E+00
            y3 = 3.0E+00
            z3 = 8.0E+00
        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Triangle vertices:'
        write ( *, '(a)' ) ' '
        write ( *, '(3g14.6)' ) x1, y1, z1
        write ( *, '(3g14.6)' ) x2, y2, z2
        write ( *, '(3g14.6)' ) x3, y3, z3

        call plane_imp_triangle_near_3d ( x1, y1, z1, x2, y2, z2, &
            x3, y3, z3, a, b, c, d, dist, num_near, x, y, z )

        write ( *, '(a)' ) ' '
        write ( *, '(a,g14.6)' ) 'Triangle to plane distance is ', dist
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Nearest points'
        write ( *, '(a)' ) ' '
        do j = 1, num_near
            write ( *, '(3g14.6)' ) x(j), y(j), z(j)
        end do

    end do

    return
    end
    subroutine test44
    !
    !*******************************************************************************
    !
    !! TEST44 tests PLANE_IMP_TRIANGLE_INT_3D.
    !
    implicit none
    !
    real a
    real b
    real c
    real d
    integer i
    integer j
    integer num_int
    real x(3)
    real x1
    real x2
    real x3
    real y(3)
    real y1
    real y2
    real y3
    real z(3)
    real z1
    real z2
    real z3
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST44'
    write ( *, '(a)' ) '  PLANE_IMP_TRIANGLE_INT_3D finds the'
    write ( *, '(a)' ) '    intersection points of an implicit plane'
    write ( *, '(a)' ) '    and a triangle.'

    a = 1.0E+00
    b = -2.0E+00
    c = -3.0E+00
    d = 6.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The implicit plane: A*X + B*Y + C*Z + D = 0.'
    write ( *, '(a,4g14.6)' ) '  A,B,C,D = ', a, b, c, d

    do i = 1, 4

        if ( i == 1 ) then
            x1 =  3.0E+00
            y1 =  0.0E+00
            z1 = -7.0E+00
            x2 = 13.0E+00
            y2 = -4.0E+00
            z2 = -1.0E+00
            x3 =  5.0E+00
            y3 =  1.0E+00
            z3 = -2.0E+00
        else if ( i == 2 ) then
            x1 =  3.0E+00
            y1 =  0.0E+00
            z1 = -7.0E+00
            x2 = 13.0E+00
            y2 = -4.0E+00
            z2 = -1.0E+00
            x3 =  9.0E+00
            y3 =  3.0E+00
            z3 =  8.0E+00
        else if ( i == 3 ) then
            x1 = -6.0E+00
            y1 =  0.0E+00
            z1 =  0.0E+00
            x2 =  0.0E+00
            y2 =  3.0E+00
            z2 =  0.0E+00
            x3 =  0.0E+00
            y3 =  0.0E+00
            z3 =  2.0E+00
        else if ( i == 4 ) then
            x1 = -4.0E+00
            y1 = +1.0E+00
            z1 =  0.0E+00
            x2 =  0.0E+00
            y2 =  6.0E+00
            z2 = -2.0E+00
            x3 =  0.0E+00
            y3 =  0.0E+00
            z3 =  1.0E+00
        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,i6)' ) 'Case ', i
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Triangle vertices:'
        write ( *, '(a)' ) ' '
        write ( *, '(3g14.6)' ) x1, y1, z1
        write ( *, '(3g14.6)' ) x2, y2, z2
        write ( *, '(3g14.6)' ) x3, y3, z3

        call plane_imp_triangle_int_3d ( x1, y1, z1, x2, y2, z2, &
            x3, y3, z3, a, b, c, d, num_int, x, y, z )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i6)' ) '  Number of intersection points is ', num_int
        write ( *, '(a)' ) ' '

        if ( num_int > 0 ) then
            do j = 1, num_int
                write ( *, '(i6,3g14.6)' ) j, x(j), y(j), z(j)
            end do
        end if

    end do

    return
    end
    subroutine test45
    !
    !*******************************************************************************
    !
    !! TEST45 tests PLANE_NORM_BASIS_3D.
    !
    implicit none
    !
    real xn
    real xp
    real xq
    real xr
    real yn
    real yp
    real yq
    real yr
    real zn
    real zp
    real zq
    real zr

    xp = 0.0E+00
    yp = 0.0E+00
    zp = 0.0E+00

    xn = 1.0E+00
    yn = 1.0E+00
    zn = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST45'
    write ( *, '(a)' ) '  PLANE_NORM_BASIS_3D, given a plane in'
    write ( *, '(a)' ) '    point, normal form (P,N), finds two unit'
    write ( *, '(a)' ) '    vectors Q and R that "lie" in the plane'
    write ( *, '(a)' ) '    and are mutually orthogonal.'

    call plane_norm_basis_3d ( xp, yp, zp, xn, yn, zn, &
        xq, yq, zq, xr, yr, zr )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Input:'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  P', xp, yp, zp
    write ( *, '(a,3g14.6)' ) '  N', xn, yn, zn
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Output:'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  Q', xq, yq, zq
    write ( *, '(a,3g14.6)' ) '  R', xr, yr, zr
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) 'N dot N = ', dot_3d ( xn, yn, zn, xn, yn, zn )
    write ( *, '(a,g14.6)' ) 'N dot Q = ', dot_3d ( xn, yn, zn, xq, yq, zq )
    write ( *, '(a,g14.6)' ) 'N dot R = ', dot_3d ( xn, yn, zn, xr, yr, zr )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) 'Q dot Q = ', dot_3d ( xq, yq, zq, xq, yq, zq )
    write ( *, '(a,g14.6)' ) 'Q dot R = ', dot_3d ( xq, yq, zq, xr, yr, zr )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) 'R dot R = ', dot_3d ( xr, yr, zr, xr, yr, zr )

    return
    end
    subroutine test46
    !
    !*******************************************************************************
    !
    !! TEST46 tests PLANE_NORM_TRIANGLE_INT_3D.
    !
    implicit none
    !
    integer, parameter :: ntest = 4
    !
    integer i
    integer j
    integer num_int
    real x(3)
    real x1
    real x2
    real x3
    real xn
    real xp
    real y(3)
    real y1
    real y2
    real y3
    real yn
    real yp
    real z(3)
    real z1
    real z2
    real z3
    real zn
    real zp
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST46'
    write ( *, '(a)' ) '  PLANE_NORM_TRIANGLE_INT_3D finds the'
    write ( *, '(a)' ) '    intersection points of a normal form plane'
    write ( *, '(a)' ) '    and a triangle.'

    xn =   1.0E+00
    yn = - 2.0E+00
    zn = - 3.0E+00

    xp = 0.0E+00
    yp = 0.0E+00
    zp = 2.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The normal plane is determined by the point:'
    write ( *, '(3g14.6)' ) xp, yp, zp
    write ( *, '(a)' ) '  and the normal vector:'
    write ( *, '(3g14.6)' ) xn, yn, zn

    do i = 1, ntest

        if ( i == 1 ) then
            x1 =  3.0E+00
            y1 =  0.0E+00
            z1 = -7.0E+00
            x2 = 13.0E+00
            y2 = -4.0E+00
            z2 = -1.0E+00
            x3 =  5.0E+00
            y3 =  1.0E+00
            z3 = -2.0E+00
        else if ( i == 2 ) then
            x1 =  3.0E+00
            y1 =  0.0E+00
            z1 = -7.0E+00
            x2 = 13.0E+00
            y2 = -4.0E+00
            z2 = -1.0E+00
            x3 =  9.0E+00
            y3 =  3.0E+00
            z3 =  8.0E+00
        else if ( i == 3 ) then
            x1 = -6.0E+00
            y1 =  0.0E+00
            z1 =  0.0E+00
            x2 =  0.0E+00
            y2 =  3.0E+00
            z2 =  0.0E+00
            x3 =  0.0E+00
            y3 =  0.0E+00
            z3 =  2.0E+00
        else if ( i == 4 ) then
            x1 = -4.0E+00
            y1 = +1.0E+00
            z1 =  0.0E+00
            x2 =  0.0E+00
            y2 =  6.0E+00
            z2 = -2.0E+00
            x3 =  0.0E+00
            y3 =  0.0E+00
            z3 =  1.0E+00
        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,i6)' ) 'Case ', i
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Triangle vertices:'
        write ( *, '(a)' ) ' '
        write ( *, '(3g14.6)' ) x1, y1, z1
        write ( *, '(3g14.6)' ) x2, y2, z2
        write ( *, '(3g14.6)' ) x3, y3, z3

        call plane_norm_triangle_int_3d ( x1, y1, z1, x2, y2, z2, &
            x3, y3, z3, xp, yp, zp, xn, yn, zn, num_int, x, y, z )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i6)' ) '  Number of intersection points is ', num_int
        write ( *, '(a)' ) ' '

        if ( num_int > 0 ) then
            do j = 1, num_int
                write ( *, '(i6,3g14.6)' ) j, x(j), y(j), z(j)
            end do
        end if

    end do

    return
    end
    subroutine test47
    !
    !*******************************************************************************
    !
    !! TEST47 tests PLANE_NORM2EXP_3D.
    !
    implicit none
    !
    real x1
    real x2
    real x3
    real xn
    real xp
    real y1
    real y2
    real y3
    real yn
    real yp
    real z1
    real z2
    real z3
    real zn
    real zp
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST47'
    write ( *, '(a)' ) '  PLANE_NORM2EXP_3D puts a plane defined by '
    write ( *, '(a)' ) '    point, normal form into explicit form.'
    write ( *, '(a)' ) ' '

    xp = - 1.0E+00
    yp =   0.0E+00
    zp = - 1.0E+00

    xn = - 0.2672612E+00
    yn = - 0.5345225E+00
    zn = - 0.8017837E+00

    call plane_norm2exp_3d ( xp, yp, zp, xn, yn, zn, x1, y1, z1, &
        x2, y2, z2, x3, y3, z3 )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Input:'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  (XP,YP,ZP)= ', xp, yp, zp
    write ( *, '(a,3g14.6)' ) '  (XN,YN,ZN)= ', xn, yn, zn
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Output:'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  (X1,Y1,Z1)= ', x1, y1, z1
    write ( *, '(a,3g14.6)' ) '  (X2,Y2,Z2)= ', x2, y2, z2
    write ( *, '(a,3g14.6)' ) '  (X3,Y3,Z3)= ', x3, y3, z3

    return
    end
    subroutine test48
    !
    !*******************************************************************************
    !
    !! TEST48 tests PLANE_NORM2IMP_3D.
    !
    implicit none
    !
    real a
    real b
    real c
    real d
    real xn
    real xp
    real yn
    real yp
    real zn
    real zp
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST48'
    write ( *, '(a)' ) '  PLANE_NORM2IMP_3D puts a plane defined by '
    write ( *, '(a)' ) '    point, normal form into implicit ABCD form.'
    write ( *, '(a)' ) ' '

    xp = - 1.0E+00
    yp =   0.0E+00
    zp = - 1.0E+00

    xn = - 0.2672612E+00
    yn = - 0.5345225E+00
    zn = - 0.8017837E+00

    call plane_norm2imp_3d ( xp, yp, zp, xn, yn, zn, a, b, c, d )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Input:'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  (XP,YP,ZP)= ', xp, yp, zp
    write ( *, '(a,3g14.6)' ) '  (XN,YN,ZN)= ', xn, yn, zn
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Output:'
    write ( *, '(a)' ) ' '
    write ( *, '(a,4g14.6)' ) '  (A,B,C,D)= ', a, b, c, d

    xp = - 16.0E+00
    yp =    2.0E+00
    zp =    4.0E+00

    xn = - 0.2672612E+00
    yn = - 0.5345225E+00
    zn = - 0.8017837E+00

    call plane_norm2imp_3d ( xp, yp, zp, xn, yn, zn, a, b, c, d )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Input:'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  (XP,YP,ZP)= ', xp, yp, zp
    write ( *, '(a,3g14.6)' ) '  (XN,YN,ZN)= ', xn, yn, zn
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Output:'
    write ( *, '(a)' ) ' '
    write ( *, '(a,4g14.6)' ) '  (A,B,C,D)= ', a, b, c, d

    return
    end
    subroutine test49
    !
    !*******************************************************************************
    !
    !! TEST49 tests POINTS_COLIN_2D.
    !
    implicit none
    !
    real colin
    real x1
    real x2
    real x3
    real y1
    real y2
    real y3
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST49'
    write ( *, '(a)' ) '  POINTS_COLIN_2D estimates the colinearity'
    write ( *, '(a)' ) '    of three points.'

    x1 = 0.0E+00
    y1 = 0.0E+00

    x2 = 10.0E+00
    y2 = 10.0E+00

    x3 = 5.0E+00
    y3 = 4.99E+00

    call points_colin_2d ( x1, y1, x2, y2, x3, y3, colin )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Points almost on a line:'
    write ( *, '(a)' ) '  Expect COLIN to be close to 0'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) x1, y1
    write ( *, '(a,2g14.6)' ) x2, y2
    write ( *, '(a,2g14.6)' ) x3, y3
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Colinearity index = ', colin

    x1 = 0.0E+00
    y1 = 0.0E+00

    x2 = 0.0E+00
    y2 = 1.0E+00

    x3 = 100.0E+00
    y3 = 0.0E+00

    call points_colin_2d ( x1, y1, x2, y2, x3, y3, colin )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Two points close, one far:'
    write ( *, '(a)' ) '  Expect COLIN to be close to 0'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) x1, y1
    write ( *, '(a,2g14.6)' ) x2, y2
    write ( *, '(a,2g14.6)' ) x3, y3
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Colinearity index = ', colin

    x1 = 0.0E+00
    y1 = 0.0E+00

    x2 = 1.0E+00
    y2 = 0.0E+00

    x3 = 0.5E+00
    y3 = 0.5E+00 * sqrt ( 3.0E+00 )

    call points_colin_2d ( x1, y1, x2, y2, x3, y3, colin )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Points on an equilateral triangle:'
    write ( *, '(a)' ) '  Expect COLIN to be close to 1'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) x1, y1
    write ( *, '(a,2g14.6)' ) x2, y2
    write ( *, '(a,2g14.6)' ) x3, y3
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Colinearity index = ', colin

    return
    end
    subroutine test499
    !
    !*******************************************************************************
    !
    !! TEST499 tests POINTS_DELAUNAY_NAIVE_2D.
    !
    !  !....3&11....
    !  !............
    !  !............
    !  X..9.........
    !  !.....5......
    !  !...........6
    !  !.4.2...10...
    !  !.....8...12.
    !  V............
    !  !..7.........
    !  !......1.....
    !  !............
    !  !............
    !  !----V----X--
    !
    implicit none
    !
    integer, parameter :: maxtri = 20
    integer, parameter :: n = 12
    !
    integer i
    integer j
    integer ntri
    integer tri(3,maxtri)
    real, dimension ( n ) :: x = &
        (/ 7.0E+00, 4.0E+00,  5.0E+00, 2.0E+00, 6.0E+00,12.0E+00, &
        3.0E+00, 6.0E+00,  3.0E+00, 8.0E+00, 5.0E+00,10.0E+00 /)
    real, dimension ( n ) :: y = &
        (/ 3.0E+00, 7.0E+00, 13.0E+00, 7.0E+00, 9.0E+00, 8.0E+00, &
        4.0E+00, 6.0E+00, 10.0E+00, 7.0E+00,13.0E+00, 6.0E+00 /)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST499'
    write ( *, '(a)' ) '  POINTS_DELAUNAY_NAIVE_2D computes the Delaunay'
    write ( *, '(a)' ) '    triangulation of a set of points.'

    call rvec2_print ( n, x, y, '  The points:' )

    call points_delaunay_naive_2d ( n, x, y, maxtri, ntri, tri )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The Delaunay triangles are:'
    write ( *, '(a)' ) ' '
    do i = 1, ntri
        write ( *, '(i6,3g14.6)' ) i, tri(1:3,i)
    end do

    return
    end
    subroutine test4995
    !
    !*******************************************************************************
    !
    !! TEST4995 tests POINTS_DIST_SPHERE.
    !
    implicit none
    !
    integer, parameter :: ntest = 6
    !
    real dist
    character ( len = 18 ), dimension ( ntest ) :: name = &
        (/ 'Atlanta, Georgia  ', &
        'North Pole        ', &
        'South Pole        ', &
        'Timbuktu          ', &
        'San Antonio, Texas', &
        'Savannah, Georgia ' /)
    integer, dimension ( ntest ) :: lat_d =  (/ 33, 90, -90, 16, 29, 32 /)
    integer, dimension ( ntest ) :: lat_m =  (/ 11,  0,   0, 49, 25,  5 /)
    integer, dimension ( ntest ) :: long_d = (/ 82,  0,   0,  3, 98, 81 /)
    integer, dimension ( ntest ) :: long_m = (/ 34,  0,   0,  0, 30,  6 /)
    integer i
    integer j
    real lat1
    real lat2
    real long1
    real long2
    real, parameter :: radius = 3957.0E+00
    integer,parameter :: second =0.0E+00
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST4995'
    write ( *, '(a)' ) '  POINTS_DIST_SPHERE measures the distance between two'
    write ( *, '(a)' ) '  points on a sphere.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  All tests uses RADIUS = ', radius
    write ( *, '(a)' ) '  which is the radius of the earth in miles.'
    write ( *, '(a)' ) ' '

    do i = 1, ntest-1

        call dms_to_radians ( lat_d(i), lat_m(i),second, lat1 )
        call dms_to_radians ( long_d(i), long_m(i),second, long1 )

        write ( *, '(a)' ) ' '
        write ( *, '(a,a)' ) '  Distance from ', name(i)

        do j = i+1, ntest

            call dms_to_radians ( lat_d(j), lat_m(j), second, lat2 )
            call dms_to_radians ( long_d(j), long_m(j), second, long2 )

            call points_dist_sphere ( lat1, long1, lat2, long2, radius, dist )

            write ( *, '(a,a,g14.6)' ) '             to ', name(j), dist

        end do

    end do

    return
    end
    subroutine test50
    !
    !*******************************************************************************
    !
    !! TEST50 tests POINTS_HULL_2D.
    !
    !  !....3.......
    !  !............
    !  !..9.........
    !  !.....5......
    !  !...........6
    !  !.4.2...10...
    !  !.....8......
    !  !.........12.
    !  !..7.........
    !  !......1.....
    !  !............
    !  !............
    !  !-----------
    !
    implicit none
    !
    integer, parameter :: n = 12
    !
    integer i
    integer ival(n)
    integer nval
    real, dimension ( n ) :: x = &
        (/ 7.0E+00, 4.0E+00,  5.0E+00, 2.0E+00, 6.0E+00,12.0E+00, &
        3.0E+00, 6.0E+00,  3.0E+00, 8.0E+00, 5.0E+00,10.0E+00 /)
    real, dimension ( n ) :: y = &
        (/ 3.0E+00, 7.0E+00, 13.0E+00, 7.0E+00, 9.0E+00, 8.0E+00, &
        4.0E+00, 6.0E+00, 10.0E+00, 7.0E+00,13.0E+00, 6.0E+00 /)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST50'
    write ( *, '(a)' ) '  POINTS_HULL_2D computes the convex hull'
    write ( *, '(a)' ) '    of a set of points.'

    call rvec2_print ( n, x, y, '  The points:' )

    call points_hull_2d ( ival, n, nval, x, y )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The convex hull is formed by connecting:'
    write ( *, '(a)' ) ' '
    do i = 1, nval
        write ( *, '(1x,2i3,2g14.6)' ) i, ival(i), x(ival(i)), y(ival(i))
    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The correct sequence of nodes is:'
    write ( *, '(a)' ) '  4, 9, 3, 6, 12, 1, 7, (4).'
    write ( *, '(a)' ) ' '

    return
    end
    subroutine test505
    !
    !*******************************************************************************
    !
    !! TEST505 tests POINTS_CENTROID_2D.
    !
    !  !....3&11....
    !  !............
    !  !............
    !  X..9.........
    !  !.....5......
    !  !...........6
    !  !.4.2...10...
    !  !.....8...12.
    !  V............
    !  !..7.........
    !  !......1.....
    !  !............
    !  !............
    !  !----V----X--
    !
    implicit none
    !
    integer, parameter :: maxtri = 20
    integer, parameter :: n = 12
    !
    integer cent
    integer i
    real, dimension ( n ) :: x = &
        (/ 7.0E+00, 4.0E+00,  5.0E+00, 2.0E+00, 6.0E+00,12.0E+00, &
        3.0E+00, 6.0E+00,  3.0E+00, 8.0E+00, 5.0E+00,10.0E+00 /)
    real, dimension ( n ) :: y = &
        (/ 3.0E+00, 7.0E+00, 13.0E+00, 7.0E+00, 9.0E+00, 8.0E+00, &
        4.0E+00, 6.0E+00, 10.0E+00, 7.0E+00,13.0E+00, 6.0E+00 /)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST505'
    write ( *, '(a)' ) '  POINTS_CENTROID_2D computes the centroid of a'
    write ( *, '(a)' ) '    discrete set of points.'

    call rvec2_print ( n, x, y, '  The points:' )

    call points_centroid_2d ( n, x, y, cent )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  The centroid is point #:', cent

    return
    end
    subroutine test0595
    !
    !*******************************************************************************
    !
    !! TEST0595 tests POINTS_NEAREST_POINT_BINS_2D.
    !! TEST0595 tests POINTS_NEAREST_POINT_BINS2_2D.
    !! TEST0595 tests POINTS_NEAREST_POINT_BINS3_2D.
    !! TEST0595 tests POINTS_NEAREST_POINT_NAIVE_2D.
    !
    implicit none
    !
    integer, parameter :: ndim = 2
    !
    integer, parameter :: nbin = 10
    integer, parameter, dimension ( ndim ) :: nbin2 = (/ 20, 5 /)
    integer, parameter :: nset = 1000
    integer, parameter :: ntest = 10
    !
    real, parameter, dimension ( ndim ) :: bin_min = (/  0.0E+00,  0.0E+00 /)
    real, parameter, dimension ( ndim ) :: bin_max = (/  20.0E+00, 5.0E+00 /)
    integer bin_last(nbin,nbin)
    integer bin_last2(nbin2(1),nbin2(2))
    integer bin_next(nset)
    integer bin_next2(nset)
    integer bin_start(nbin,nbin)
    integer bin_start2(nbin2(1),nbin2(2))
    integer compares
    real d
    real d_min
    logical :: debug = .false.
    integer i
    integer i_min
    real p(ndim)
    real pset(ndim,nset)
    real pset2(ndim,nset)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST0595'
    write ( *, '(a)' ) '  Given a point in 2D, we want to find its nearest'
    write ( *, '(a)' ) '  neighbor among points in a set.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  POINTS_NEAREST_POINT_NAIVE_2D uses a naive algorithm.'
    write ( *, '(a)' ) '  POINTS_NEAREST_POINT_BINS_2D and'
    write ( *, '(a)' ) '  POINTS_NEAREST_POINT_BINS2_2D use bins, but require the'
    write ( *, '(a)' ) '    same number in each direction.'
    write ( *, '(a)' ) '  POINTS_NEAREST_POINT_BINS3_2D uses bins, and can use'
    write ( *, '(a)' ) '    a different number in each direction.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  The number of points in the pointset is ', nset
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  POINTS_NEAREST_POINT_BINS_2D and'
    write ( *, '(a,i6)' ) '  POINTS_NEAREST_POINT_BINS2_2D use ', nbin
    write ( *, '(a)' ) '    bins in each direction.'
    write ( *, '(a,2i6)' ) '  POINTS_NEAREST_POINT_BINS3_2D uses ', nbin2(1:ndim)
    write ( *, '(a)' ) '    bins in each direction.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  The X coordinate range: ', bin_min(1), bin_max(1)
    write ( *, '(a,2g14.6)' ) '  The Y coordinate range: ', bin_min(2), bin_max(2)
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  Test point X range:     ', bin_min(1), bin_max(1)
    write ( *, '(a,2g14.6)' ) '  Test point Y range:     ', bin_min(2), bin_max(2)
    !
    !  Set the pointset.
    !
    call r2vec_random ( bin_min, bin_max, nset, pset )
    !
    !  We need to make a copy of the point set, because it gets sorted.
    !
    pset2(1:ndim,1:nset) = pset(1:ndim,1:nset)
    !
    !  For the POINTS_NEAREST_POINT_BINS_2D code:
    !
    !    Implicitly bin the data
    !    Explicitly reorder the data by bins.
    !    Within each bin, sort the data.
    !
    call r2vec_bin_even2 ( nset, pset, nbin, bin_min, bin_max, bin_start, &
        bin_last, bin_next )

    call r2vec_binned_reorder ( nset, pset, nbin, bin_start, bin_last, bin_next )

    call r2vec_binned_sort_a ( nset, pset, nbin, bin_start, bin_last )
    !
    !  For the POINTS_NEAREST_POINT_BINS3_2D code:
    !
    !    Implicitly bin the data
    !    Explicitly reorder the data by bins.
    !    Within each bin, sort the data.
    !
    call r2vec_bin_even3 ( nset, pset2, nbin2, bin_min, bin_max, bin_start2, &
        bin_last2, bin_next2 )

    call r2vec_binned_reorder2 ( nset, pset2, nbin2, bin_start2, bin_last2, &
        bin_next2 )

    call r2vec_binned_sort_a2 ( nset, pset2, nbin2, bin_start2, bin_last2 )
    !
    !  Seek nearest neighbors.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    Test point           Neighbor point      Distance'
    write ( *, '(a)' ) '--------------------  --------------------  ----------'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        call r2_random ( bin_min, bin_max, p )

        write ( *, '(a)' ) ' '

        call points_nearest_point_naive_2d ( nset, pset, p, i_min, d_min )

        compares = nset
        write ( *, '(2f10.4,2x,2f10.4,2x,f10.4,2x,i4)' ) p(1:ndim), &
            pset(1:ndim,i_min), d_min, compares

        call points_nearest_point_bins_2d ( nset, pset, nbin, bin_min, bin_max, &
            bin_start, bin_last, bin_next, p, i_min, d_min, compares )

        write ( *, '(2f10.4,2x,2f10.4,2x,f10.4,2x,i4)' ) p(1:ndim), &
            pset(1:ndim,i_min), d_min, compares

        call points_nearest_point_bins2_2d ( nset, pset, nbin, bin_min, bin_max, &
            bin_start, bin_last, bin_next, p, i_min, d_min, compares )

        write ( *, '(2f10.4,2x,2f10.4,2x,f10.4,2x,i4)' ) p(1:ndim), &
            pset(1:ndim,i_min), d_min, compares

        call points_nearest_point_bins3_2d ( nset, pset2, nbin2, bin_min, bin_max, &
            bin_start2, bin_last2, bin_next2, p, i_min, d_min, compares )

        write ( *, '(2f10.4,2x,2f10.4,2x,f10.4,2x,i4)' ) p(1:ndim), &
            pset(1:ndim,i_min), d_min, compares

    end do

    return
    end
    subroutine test0596
    !
    !*******************************************************************************
    !
    !! TEST0596 tests POINTS_NEAREST_POINTS_BINS_2D.
    !! TEST0596 tests POINTS_NEAREST_POINTS_BINS2_2D.
    !! TEST0596 tests POINTS_NEAREST_POINTS_BINS3_2D.
    !! TEST0596 tests POINTS_NEAREST_POINTS_NAIVE_2D.
    !
    implicit none
    !
    integer, parameter :: ndim = 2
    !
    integer, parameter :: nbin = 10
    integer, parameter, dimension ( ndim ) :: nbin2 = (/ 10, 10 /)
    integer, parameter :: nset = 1000
    integer, parameter :: ntest = 100
    !
    real, parameter, dimension ( ndim ) :: bin_min = (/  0.0E+00,  0.0E+00 /)
    real, parameter, dimension ( ndim ) :: bin_max = (/ 10.0E+00,  10.0E+00 /)
    integer bin_last(nbin,nbin)
    integer bin_last2(nbin2(1),nbin2(2))
    integer bin_next(nset)
    integer bin_next2(nset)
    integer bin_start(nbin,nbin)
    integer bin_start2(nbin2(1),nbin2(2))
    integer clock_count1
    integer clock_count2
    integer clock_count3
    integer clock_count4
    integer clock_count5
    integer clock_count6
    integer clock_max
    integer clock_rate
    integer compares(ntest)
    real d
    real d_min0(ntest)
    real d_min1(ntest)
    real d_min2(ntest)
    real d_min3(ntest)
    real eps
    integer i
    integer i_min0(ntest)
    integer i_min1(ntest)
    integer i_min2(ntest)
    integer i_min3(ntest)
    integer n_different
    real pset(ndim,nset)
    real ptest(ndim,ntest)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST0596'
    write ( *, '(a)' ) '  Given a point set in 2D, and a set of test points,'
    write ( *, '(a)' ) '  for each testpoint, find the nearest neighbor in'
    write ( *, '(a)' ) '  the point set.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  POINTS_NEAREST_POINTS_NAIVE_2D uses a naive algorithm.'
    write ( *, '(a)' ) '  POINTS_NEAREST_POINTS_BINS_2D uses equal bins.'
    write ( *, '(a)' ) '  POINTS_NEAREST_POINTS_BINS2_2D uses equal bins.'
    write ( *, '(a)' ) '  POINTS_NEAREST_POINTS_BINS3_2D uses variable bins.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  The number of points in the pointset is ', nset
    write ( *, '(a,i6)' ) '  The number of points in the test set is ', ntest
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  POINTS_NEAREST_POINTS_BINS_2D and '
    write ( *, '(a,i6)' ) '  POINTS_NEAREST_POINTS_BINS2_2D use ', nbin
    write ( *, '(a)' ) '    bins in each direction.'
    write ( *, '(a,2i6)' ) '  POINTS_NEAREST_POINTS_BINS3_2D uses ', nbin2(1:ndim)
    write ( *, '(a)' ) '    bins in each direction.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  The X coordinate range: ', bin_min(1), bin_max(1)
    write ( *, '(a,2g14.6)' ) '  The Y coordinate range: ', bin_min(2), bin_max(2)
    write ( *, '(a)' ) ' '
    !
    !  Set the pointset.
    !
    call r2vec_random ( bin_min, bin_max, nset, pset )
    !
    !  Set the test points.
    !
    call r2vec_random ( bin_min, bin_max, ntest, ptest )
    !
    !  For POINTS_NEAREST_POINTS_BINS_2D and POINTS_NEAREST_POINTS_BINS2_2D:
    !
    !  Implicitly bin the data.
    !  Explicitly reorder the data by bins.
    !  Within each bin, sort the data.
    !
    call r2vec_bin_even2 ( nset, pset, nbin, bin_min, bin_max, bin_start, &
        bin_last, bin_next )

    call r2vec_binned_reorder ( nset, pset, nbin, bin_start, bin_last, bin_next )

    call r2vec_binned_sort_a ( nset, pset, nbin, bin_start, bin_last )
    !
    !  Seek nearest neighbors.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Print results for up to first 10 points...'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    Test point		      Distance'
    write ( *, '(a)' ) '                       Naive     Bins     Bins2     Bins3'
    write ( *, '(a)' ) &
        '--------------------  ------------------------------------'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' '

    call system_clock ( clock_count1, clock_rate, clock_max )

    call points_nearest_points_naive_2d ( nset, pset, ntest, ptest, i_min0, &
        d_min0 )

    call system_clock ( clock_count2, clock_rate, clock_max )

    call points_nearest_points_bins_2d ( nset, pset, nbin, bin_min, bin_max, &
        bin_start, bin_last, bin_next, ntest, ptest, i_min1, d_min1, compares )

    call system_clock ( clock_count3, clock_rate, clock_max )

    call points_nearest_points_bins2_2d ( nset, pset, nbin, bin_min, bin_max, &
        bin_start, bin_last, bin_next, ntest, ptest, i_min2, d_min2, compares )

    call system_clock ( clock_count4, clock_rate, clock_max )
    !
    !  We have to rework the data for BINS3, since we allow a different
    !  number of bins in each direction.
    !
    call r2vec_bin_even3 ( nset, pset, nbin2, bin_min, bin_max, bin_start2, &
        bin_last2, bin_next2 )

    !call r2vec_binned_reorder ( nset, pset, nbin2, bin_start2, bin_last2, &
    !bin_next2 )
    call r2vec_binned_reorder ( nset, pset, nbin, bin_start2, bin_last2, &
        bin_next2 )

    !call r2vec_binned_sort_a ( nset, pset, nbin2, bin_start2, bin_last2 )
    call r2vec_binned_sort_a ( nset, pset, nbin, bin_start2, bin_last2 )
    call system_clock ( clock_count5, clock_rate, clock_max )

    call points_nearest_points_bins3_2d ( nset, pset, nbin2, bin_min, bin_max, &
        bin_start2, bin_last2, bin_next2, ntest, ptest, i_min3, d_min3, compares )

    call system_clock ( clock_count6, clock_rate, clock_max )
    !
    !  Print the results.
    !
    do i = 1, min ( ntest, 10 )
        write ( *, '(2f10.4,4x,4f10.4)' ) ptest(1:ndim,i), d_min0(i), d_min1(i), &
            d_min2(i), d_min3(i)
    end do
    !
    !  Check the results.
    !
    eps = epsilon ( eps )

    n_different = 0
    do i = 1, ntest
        if ( abs ( d_min0(i) - d_min1(i) ) > eps ) then
            n_different = n_different + 1
        end if
    end do

    if ( n_different == 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Naive and bin1 codes computed the same results.'
    else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WARNING:'
        write ( *, '(a)' ) '  Naive and bin1 codes disagreed.'
        write ( *, '(a,i6)' ) '  Number of discrepancies was ', n_different
    end if

    n_different = 0
    do i = 1, ntest
        if ( abs ( d_min0(i) - d_min2(i) ) > eps ) then
            n_different = n_different + 1
        end if
    end do

    if ( n_different == 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Naive and bin2 codes computed the same results.'
    else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WARNING:'
        write ( *, '(a)' ) '  Naive and bin2 codes disagreed.'
        write ( *, '(a,i6)' ) '  Number of discrepancies was ', n_different
    end if

    n_different = 0
    do i = 1, ntest
        if ( abs ( d_min0(i) - d_min3(i) ) > eps ) then
            n_different = n_different + 1
        end if
    end do

    if ( n_different == 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Naive and bin3 codes computed the same results.'
    else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WARNING:'
        write ( *, '(a)' ) '  Naive and bin3 codes disagreed.'
        write ( *, '(a,i6)' ) '  Number of discrepancies was ', n_different
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Naive code time = ', &
        real ( clock_count2 - clock_count1 ) / real ( clock_rate )

    write ( *, '(a,g14.6)' ) '  Bin code time =	', &
        real ( clock_count3 - clock_count2 ) / real ( clock_rate )

    write ( *, '(a,g14.6)' ) '  Bin2 code time =   ', &
        real ( clock_count4 - clock_count3 ) / real ( clock_rate )

    write ( *, '(a,g14.6)' ) '  Bin3 code time =   ', &
        real ( clock_count6 - clock_count5 ) / real ( clock_rate )

    return
    end
    subroutine test05965
    !
    !*******************************************************************************
    !
    !! TEST05965 tests POINTS_NEAREST_POINTS_BINS_2D.
    !! TEST05965 tests POINTS_NEAREST_POINTS_BINS2_2D.
    !! TEST05965 tests POINTS_NEAREST_POINTS_BINS3_2D.
    !! TEST05965 tests POINTS_NEAREST_POINTS_NAIVE_2D.
    !
    implicit none
    !
    integer, parameter :: ndim = 2
    !
    integer, parameter :: nbin = 10
    integer, parameter, dimension ( ndim ) :: nbin2 = (/ 4, 25 /)
    integer, parameter :: nset = 1000
    integer, parameter :: ntest = 100
    !
    real, parameter, dimension ( ndim ) :: bin_min = (/  0.0E+00,  0.0E+00 /)
    real, parameter, dimension ( ndim ) :: bin_max = (/ 4.0E+00, 25.0E+00 /)
    integer bin_last(nbin,nbin)
    integer bin_last2(nbin2(1),nbin2(2))
    integer bin_next(nset)
    integer bin_next2(nset)
    integer bin_start(nbin,nbin)
    integer bin_start2(nbin2(1),nbin2(2))
    integer clock_count1
    integer clock_count2
    integer clock_count3
    integer clock_count4
    integer clock_count5
    integer clock_count6
    integer clock_max
    integer clock_rate
    integer compares(ntest)
    real d
    real d_min0(ntest)
    real d_min1(ntest)
    real d_min2(ntest)
    real d_min3(ntest)
    real eps
    integer i
    integer i_min0(ntest)
    integer i_min1(ntest)
    integer i_min2(ntest)
    integer i_min3(ntest)
    integer n_different
    real pset(ndim,nset)
    real ptest(ndim,ntest)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST05965'
    write ( *, '(a)' ) '  Given a point set in 2D, and a set of test points,'
    write ( *, '(a)' ) '  for each testpoint, find the nearest neighbor in'
    write ( *, '(a)' ) '  the point set.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  In this test, the region is RECTANGULAR.'
    write ( *, '(a)' ) &
        '  The BINS and BINS2 codes will end up using rectangular bins;'
    write ( *, '(a)' ) &
        '  We will set the BINS3 code to use the same number of bins,'
    write ( *, '(a)' ) '  but they will be square.  This should mean that BINS3'
    write ( *, '(a)' ) '  finds a match faster.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  POINTS_NEAREST_POINTS_NAIVE_2D uses a naive algorithm.'
    write ( *, '(a)' ) '  POINTS_NEAREST_POINTS_BINS_2D uses bins.'
    write ( *, '(a)' ) '  POINTS_NEAREST_POINTS_BINS2_2D uses bins.'
    write ( *, '(a)' ) '  POINTS_NEAREST_POINTS_BINS3_2D uses bins.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  The number of points in the pointset is ', nset
    write ( *, '(a,i6)' ) '  The number of bins used in each direction is ', nbin
    write ( *, '(a,i6)' ) '  The number of points in the test set is ', ntest
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  The X coordinate range: ', bin_min(1), bin_max(1)
    write ( *, '(a,2g14.6)' ) '  The Y coordinate range: ', bin_min(2), bin_max(2)
    write ( *, '(a)' ) ' '
    !
    !  Set the pointset.
    !
    call r2vec_random ( bin_min, bin_max, nset, pset )
    !
    !  Set the test points.
    !
    call r2vec_random ( bin_min, bin_max, ntest, ptest )
    !
    !  Implicitly bin the data.
    !
    call r2vec_bin_even2 ( nset, pset, nbin, bin_min, bin_max, bin_start, &
        bin_last, bin_next )
    !
    !  Explicitly reorder the data by bins.
    !
    call r2vec_binned_reorder ( nset, pset, nbin, bin_start, bin_last, bin_next )
    !
    !  Within each bin, sort the data.
    !
    call r2vec_binned_sort_a ( nset, pset, nbin, bin_start, bin_last )
    !
    !  Seek nearest neighbors.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Print results for up to first 10 points...'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    Test point		      Distance'
    write ( *, '(a)' ) '                       Naive     Bins     Bins2     Bins3'
    write ( *, '(a)' ) &
        '--------------------  ------------------------------------'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' '

    call system_clock ( clock_count1, clock_rate, clock_max )

    call points_nearest_points_naive_2d ( nset, pset, ntest, ptest, i_min0, &
        d_min0 )

    call system_clock ( clock_count2, clock_rate, clock_max )

    call points_nearest_points_bins_2d ( nset, pset, nbin, bin_min, bin_max, &
        bin_start, bin_last, bin_next, ntest, ptest, i_min1, d_min1, compares )

    call system_clock ( clock_count3, clock_rate, clock_max )

    call points_nearest_points_bins2_2d ( nset, pset, nbin, bin_min, bin_max, &
        bin_start, bin_last, bin_next, ntest, ptest, i_min2, d_min2, compares )

    call system_clock ( clock_count4, clock_rate, clock_max )
    !
    !  We have to rework the data for BINS3, since we allow a different
    !  number of bins in each direction.
    !
    call r2vec_bin_even3 ( nset, pset, nbin2, bin_min, bin_max, bin_start2, &
        bin_last2, bin_next2 )

    call r2vec_binned_reorder2 ( nset, pset, nbin2, bin_start2, bin_last2, &
        bin_next2 )

    call r2vec_binned_sort_a2 ( nset, pset, nbin2, bin_start2, bin_last2 )

    call system_clock ( clock_count5, clock_rate, clock_max )

    call points_nearest_points_bins3_2d ( nset, pset, nbin2, bin_min, bin_max, &
        bin_start2, bin_last2, bin_next2, ntest, ptest, i_min3, d_min3, compares )

    call system_clock ( clock_count6, clock_rate, clock_max )
    !
    !  Print the results.
    !
    do i = 1, min ( ntest, 10 )
        write ( *, '(2f10.4,4x,4f10.4)' ) ptest(1:ndim,i), d_min0(i), d_min1(i), &
            d_min2(i), d_min3(i)
    end do
    !
    !  Check the results.
    !
    eps = epsilon ( eps )

    n_different = 0
    do i = 1, ntest
        if ( abs ( d_min0(i) - d_min1(i) ) > eps ) then
            n_different = n_different + 1
        end if
    end do

    if ( n_different == 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Naive and bin1 codes computed the same results.'
    else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WARNING:'
        write ( *, '(a)' ) '  Naive and bin1 codes disagreed.'
        write ( *, '(a,i6)' ) '  Number of discrepancies was ', n_different
    end if

    n_different = 0
    do i = 1, ntest
        if ( abs ( d_min0(i) - d_min2(i) ) > eps ) then
            n_different = n_different + 1
        end if
    end do

    if ( n_different == 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Naive and bin2 codes computed the same results.'
    else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WARNING:'
        write ( *, '(a)' ) '  Naive and bin2 codes disagreed.'
        write ( *, '(a,i6)' ) '  Number of discrepancies was ', n_different
    end if

    n_different = 0
    do i = 1, ntest
        if ( abs ( d_min0(i) - d_min3(i) ) > eps ) then
            n_different = n_different + 1
        end if
    end do

    if ( n_different == 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Naive and bin3 codes computed the same results.'
    else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WARNING:'
        write ( *, '(a)' ) '  Naive and bin3 codes disagreed.'
        write ( *, '(a,i6)' ) '  Number of discrepancies was ', n_different
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Naive code time = ', &
        real ( clock_count2 - clock_count1 ) / real ( clock_rate )

    write ( *, '(a,g14.6)' ) '  Bin code time =   ', &
        real ( clock_count3 - clock_count2 ) / real ( clock_rate )

    write ( *, '(a,g14.6)' ) '  Bin2 code time =   ', &
        real ( clock_count4 - clock_count3 ) / real ( clock_rate )

    write ( *, '(a,g14.6)' ) '  Bin3 code time =   ', &
        real ( clock_count6 - clock_count5 ) / real ( clock_rate )

    return
    end
    subroutine test0597
    !
    !*******************************************************************************
    !
    !! TEST0597 tests POINTS_NEAREST_POINTS_BINS2_3D.
    !! TEST0597 tests POINTS_NEAREST_POINTS_NAIVE_3D.
    !
    implicit none
    !
    integer, parameter :: nbin = 32
    integer, parameter :: ndim = 3
    integer, parameter :: nset = 4096
    integer, parameter :: ntest = 1000
    !
    real, parameter, dimension ( ndim ) :: bin_min = (/  &
        0.0E+00,  0.0E+00, 0.0E+00 /)
    real, parameter, dimension ( ndim ) :: bin_max = (/ &
        10.0E+00, 10.0E+00, 10.0E+00 /)
    integer bin_last(nbin,nbin,nbin)
    integer bin_next(nset)
    integer bin_start(nbin,nbin,nbin)
    integer clock_count1
    integer clock_count2
    integer clock_count3
    integer clock_max
    integer clock_rate
    integer compares(ntest)
    real d
    real d_min1(ntest)
    real d_min2(ntest)
    real eps
    integer i
    integer i_min1(ntest)
    integer i_min2(ntest)
    integer n_different
    real pset(ndim,nset)
    real ptest(ndim,ntest)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST0597'
    write ( *, '(a)' ) '  Given a point set in 3D, and a set of test points,'
    write ( *, '(a)' ) '  for each testpoint, find the nearest neighbor in'
    write ( *, '(a)' ) '  the point set.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  POINTS_NEAREST_POINTS_NAIVE_3D uses a naive algorithm.'
    write ( *, '(a)' ) '  POINTS_NEAREST_POINTS_BINS2_3D uses bins.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  The number of points in the pointset is ', nset
    write ( *, '(a,i6)' ) '  The number of bins used in each direction is ', nbin
    write ( *, '(a,i6)' ) '  The number of points in the test set is ', ntest
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  The X coordinate range: ', bin_min(1), bin_max(1)
    write ( *, '(a,2g14.6)' ) '  The Y coordinate range: ', bin_min(2), bin_max(2)
    write ( *, '(a,2g14.6)' ) '  The Z coordinate range: ', bin_min(3), bin_max(3)
    write ( *, '(a)' ) ' '
    !
    !  Set the pointset.
    !
    call r3vec_random ( bin_min, bin_max, nset, pset )
    !
    !  Set the test points.
    !
    call r3vec_random ( bin_min, bin_max, ntest, ptest )
    !
    !  Implicitly bin the data.
    !
    call r3vec_bin_even2 ( nset, pset, nbin, bin_min, bin_max, bin_start, &
        bin_last, bin_next )
    !
    !  Explicitly reorder the data by bins.
    !
    call r3vec_binned_reorder ( nset, pset, nbin, bin_start, bin_last, bin_next )
    !
    !  Within each bin, sort the data.
    !
    call r3vec_binned_sort_a ( nset, pset, nbin, bin_start, bin_last )
    !
    !  Seek nearest neighbors.
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Print up to the first 10 points.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) &
        '    Test point                       Distance        Comparisons'
    write ( *, '(a)' ) &
        '                                 Naive     Bins     Naive Bins'
    write ( *, '(a)' ) &
        '-----------------------------  --------------------  ----------'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' '

    call system_clock ( clock_count1, clock_rate, clock_max )

    call points_nearest_points_naive_3d ( nset, pset, ntest, ptest, i_min1, &
        d_min1 )

    call system_clock ( clock_count2, clock_rate, clock_max )

    call points_nearest_points_bins2_3d ( nset, pset, nbin, bin_min, bin_max, &
        bin_start, bin_last, bin_next, ntest, ptest, i_min2, d_min2, compares )

    call system_clock ( clock_count3, clock_rate, clock_max )

    do i = 1, min ( ntest, 10 )
        write ( *, '(5f10.4,2x,2i6)' ) ptest(1:ndim,i), d_min1(i), d_min2(i), &
            nset, compares(i)
    end do

    eps = epsilon ( eps )
    n_different = 0
    do i = 1, ntest
        if ( abs ( d_min1(i) - d_min2(i) ) > eps ) then
            n_different = n_different + 1
        end if
    end do

    if ( n_different == 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Naive and bin codes computed the same results.'
    else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WARNING:'
        write ( *, '(a)' ) '  Naive and bin codes disagreed.'
        write ( *, '(a,i6)' ) '  Number of discrepancies was ', n_different
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Naive code time = ', &
        real ( clock_count2 - clock_count1 ) / real ( clock_rate )

    write ( *, '(a,g14.6)' ) '  Bin code time =   ', &
        real ( clock_count3 - clock_count2 ) / real ( clock_rate )

    return
    end
    subroutine test0598
    !
    !*******************************************************************************
    !
    !! TEST0598 tests POINTS_NEAREST_POINT_NAIVE_2D.
    !! TEST0598 tests POINTS_NEAREST_POINT_DEL_2D.
    !
    implicit none
    !
    integer, parameter :: nabes_max = 500
    integer, parameter :: num_pts = 13
    integer, parameter :: ntest = 10
    !
    real dd(ntest)
    real dnear1
    real dnear2
    integer i
    integer itest
    integer nabes(nabes_max)
    integer nabes_dim
    integer nabes_first(num_pts)
    integer nabes_num(num_pts)
    integer nd(ntest)
    integer nnear1
    integer nnear2
    integer nod_tri(3,2*num_pts)
    integer num_tri
    integer seed
    integer td(ntest)
    integer tnbr(3,2*num_pts)
    integer tnear
    real, dimension (2,num_pts) :: xc = reshape ( (/ &
        0.0E+00, 0.0E+00, &
        2.0E+00, 2.0E+00, &
        -1.0E+00, 3.0E+00, &
        -2.0E+00, 2.0E+00, &
        8.0E+00, 2.0E+00, &
        9.0E+00, 5.0E+00, &
        7.0E+00, 4.0E+00, &
        5.0E+00, 6.0E+00, &
        6.0E+00, 7.0E+00, &
        8.0E+00, 8.0E+00, &
        11.0E+00, 7.0E+00, &
        10.0E+00, 4.0E+00, &
        6.0E+00, 4.0E+00 /), (/ 2, num_pts /) )
    real xd(2,ntest)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST0598'
    write ( *, '(a)' ) '  Given a point set XC, and a single point XD,'
    write ( *, '(a)' ) '  find the nearest point in XC to XD.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  POINTS_NEAREST_POINT_NAIVE_2D uses a naive method.'
    write ( *, '(a)' ) '  POINTS_NEAREST_POINT_DEL_2D uses the Delaunay'
    write ( *, '(a)' ) '    triangulation'
    write ( *, '(a)' ) '  TRIANGULATION_PRINT prints a triangulation.'

    seed = 0
    call random_initialize ( seed )
    !
    !  Set up the Delaunay triangulation.
    !
    call rtris2 ( num_pts, xc, num_tri, nod_tri, tnbr )

    call triangulation_print ( num_pts, xc, num_tri, nod_tri, tnbr )
    !
    !  Determine the node neigbhor array.
    !
    call triangulation_nabe_nodes ( num_pts, num_tri, nod_tri, nabes_first, &
        nabes_num, nabes_max, nabes_dim, nabes )

    ! call triangulation_nabe_nodes_print ( num_pts, nabes_first, &
    !   nabes_num, nabes_dim, nabes )
    !
    !  Get the test points.
    !
    call triangulation_sample_2d ( num_pts, xc, num_tri, nod_tri, &
        ntest, xd, td )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    X        Y     Distance  Index'
    write ( *, '(a)' ) ' '

    do itest = 1, ntest

        call points_nearest_point_naive_2d ( num_pts, xc, xd(1,itest), nnear1, &
            dnear1 )

        call points_nearest_point_del_2d ( num_pts, xc, xd(1,itest), &
            nabes_first, nabes_num, nabes_dim, nabes, nnear2, dnear2 )

        write ( *, '(a)' ) ' '
        write ( *, '(a,2f8.4   )' ) '  XD       ', xd(1:2,itest)
        write ( *, '(a,3f8.4,i6)' ) '  Naive    ', xc(1:2,nnear1), dnear1, nnear1
        write ( *, '(a,3f8.4,i6)' ) '  Delaunay ', xc(1:2,nnear2), dnear2, nnear2

    end do

    return
    end
    subroutine test506
    !
    !*******************************************************************************
    !
    !! TEST506 tests POINTS_NEAREST_POINT_NAIVE_ND.
    !
    !  !....3&11....
    !  !............
    !  !............
    !  X..9.........
    !  !.....5......
    !  !...........6
    !  !.4.2...10...
    !  !.....8...12.
    !  V............
    !  !..7.........
    !  !......1.....
    !  !............
    !  !......*.....
    !  !----V----X--
    !
    implicit none
    !
    integer, parameter :: n = 12
    integer, parameter :: ndim = 2
    integer, parameter :: n_test = 3
    !
    real d_min
    integer i_min
    integer i_test
    real, dimension ( ndim, n ) :: x = &
        reshape ( &
        (/  7.0E+00,  3.0E+00, &
        4.0E+00,  7.0E+00, &
        5.0E+00, 13.0E+00, &
        2.0E+00,  7.0E+00, &
        6.0E+00,  9.0E+00, &
        12.0E+00,  8.0E+00, &
        3.0E+00,  4.0E+00, &
        6.0E+00,  6.0E+00, &
        3.0E+00, 10.0E+00, &
        8.0E+00,  7.0E+00, &
        5.0E+00, 13.0E+00, &
        10.0E+00,  6.0E+00 /), (/ ndim, n /) )
    real, dimension ( ndim, n_test ) :: x_test = &
        reshape ( &
        (/  7.0E+00,  1.0E+00, &
        4.0E+00,  7.0E+00, &
        8.0E+00, 11.0E+00 /), (/ ndim, n_test /) )
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST506'
    write ( *, '(a)' ) &
        '  POINTS_NEAREST_POINT_NAIVE_ND computes the nearest point'
    write ( *, '(a)' ) '    in a set of points, to a given point, in ND.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  The spatial dimension NDIM is ', ndim
    write ( *, '(a,i6)' ) '  The number of points N is ', n

    call rmat_print ( ndim, ndim, n, x, '  The set of points:' )

    do i_test = 1, n_test

        call points_nearest_point_naive_nd ( ndim, n, x, x_test(1,i_test), &
            i_min, d_min )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Test point is '
        write ( *, '(3g14.6)' ) x_test(1:ndim,i_test)
        write ( *, '(a)' ) '  Nearest point is '
        write ( *, '(3g14.6)' ) x(1:ndim,i_min)
        write ( *, '(a,g14.6)' ) '  Distance is ', d_min

    end do

    return
    end
    subroutine test52
    !
    !*******************************************************************************
    !
    !! TEST52 tests POLYGON_AREA_2D;
    !! TEST52 tests POLYGON_AREA_2_2D;
    !! TEST52 tests POLYGON_CENTROID_2D;
    !! TEST52 tests POLYGON_CENTROID_2_2D.
    !
    implicit none
    !
    integer, parameter :: n = 4
    !
    real area
    real area2
    real cx
    real cx2
    real cy
    real cy2
    integer i
    real x(n)
    real y(n)
    !
    x(1) = 1.0E+00
    y(1) = 0.0E+00

    x(2) = 2.0E+00
    y(2) = 1.0E+00

    x(3) = 1.0E+00
    y(3) = 2.0E+00

    x(4) = 0.0E+00
    y(4) = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST52'
    write ( *, '(a)' ) '  For a polygon in 2D:'
    write ( *, '(a)' ) '  POLYGON_AREA_2D computes the area;'
    write ( *, '(a)' ) '  POLYGON_AREA_2_2D computes the area;'
    write ( *, '(a)' ) '  POLYGON_CENTROID_2D computes the centroid.'
    write ( *, '(a)' ) '  POLYGON_CENTROID_2_2D computes the centroid.'

    call rvec2_print ( n, x, y, '  The polygonal vertices:' )

    call polygon_area_2d ( n, x, y, area )

    call polygon_area_2_2d ( n, x, y, area2 )

    call polygon_centroid_2d ( n, x, y, cx, cy )

    call polygon_centroid_2_2d ( n, x, y, cx2, cy2 )

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Exact area is ', 0.5E+00
    write ( *, '(a,g14.6)' ) '  The computed area is ', area
    write ( *, '(a,g14.6)' ) '  The computed area is ', area2
    write ( *, '(a,2g14.6)') '  The computed centroid is ', cx, cy
    write ( *, '(a,2g14.6)' ) '  The computed centroid is ', cx2, cy2

    return
    end
    subroutine test53
    !
    !*******************************************************************************
    !
    !! TEST53 tests POLYGON_AREA_3D;
    !! TEST53 tests POLYGON_AREA_2_3D;
    !! TEST53 tests POLYGON_CENTROID_3D.
    !
    implicit none
    !
    integer, parameter :: n = 4
    !
    real area
    real area2
    real cx
    real cy
    real cz
    integer i
    real normal(3)
    real x(n)
    real y(n)
    real z(n)
    !
    x(1) = 1.0E+00
    y(1) = 0.0E+00
    z(1) = 0.0E+00

    x(2) = 2.0E+00
    y(2) = 1.0E+00
    z(2) = 1.0E+00

    x(3) = 1.0E+00
    y(3) = 2.0E+00
    z(3) = 1.0E+00

    x(4) = 0.0E+00
    y(4) = 1.0E+00
    z(4) = 0.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST53'
    write ( *, '(a)' ) '  For a polygon in 3D:'
    write ( *, '(a)' ) '  POLYGON_AREA_3D computes the area;'
    write ( *, '(a)' ) '  POLYGON_AREA_2_3D computes the area;'
    write ( *, '(a)' ) '  POLYGON_CENTROID_3D computes the centroid.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Vertices:'
    write ( *, '(a)' ) ' '
    do i = 1, n
        write ( *, '(3g14.6)' ) x(i), y(i), z(i)
    end do

    call polygon_area_3d ( n, x, y, z, area, normal )

    call polygon_area_2_3d ( n, x, y, z, area2 )

    call polygon_centroid_3d ( n, x, y, z, cx, cy, cz )

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Exact area is ', sqrt ( 6.0E+00 )
    write ( *, '(a,g14.6)' ) '  The computed area is ', area
    write ( *, '(a,g14.6)' ) '  The computed area is ', area2
    write ( *, '(a)' ) '  The unit normal vector:'
    write ( *, '(3g14.6)' ) normal(1:3)
    write ( *, '(a,3g14.6)' ) '  Centroid: ', cx, cy, cz

    return
    end
    subroutine test54
    !
    !*******************************************************************************
    !
    !! TEST54 tests POLYGON_CONTAINS_POINT_2_2D;
    !! TEST54 tests POLYGON_CONTAINS_POINT_2D.
    !
    implicit none
    !
    integer, parameter :: n = 5
    integer, parameter :: ntest = 4
    !
    integer i
    logical inside1
    logical inside2
    real x(n)
    real xtest(ntest)
    real xval
    real y(n)
    real ytest(ntest)
    real yval
    !
    x(1) = 0.0E+00
    y(1) = 0.0E+00

    x(2) = 1.0E+00
    y(2) = 0.0E+00

    x(3) = 2.0E+00
    y(3) = 1.0E+00

    x(4) = 1.0E+00
    y(4) = 2.0E+00

    x(5) = 0.0E+00
    y(5) = 2.0E+00

    xtest(1) = 1.0E+00
    ytest(1) = 1.0E+00

    xtest(2) = 3.0E+00
    ytest(2) = 4.0E+00

    xtest(3) = 0.0E+00
    ytest(3) = 2.0E+00

    xtest(4) = 0.5E+00
    ytest(4) = -0.25E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST54'
    write ( *, '(a)' ) '  POLYGON_CONTAINS_POINT_2D determines if '
    write ( *, '(a)' ) '    a point is in a polygon.'
    write ( *, '(a)' ) '  POLYGON_CONTAINS_POINT_2_2D determines if'
    write ( *, '(a)' ) '    a point is in a polygon.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  X, Y, Inside1 Inside2'
    write ( *, '(a)' ) ' '
    do i = 1, ntest

        xval = xtest(i)
        yval = ytest(i)

        call polygon_contains_point_2d ( n, x, xval, y, yval, inside1 )

        call polygon_contains_point_2_2d ( n, x, xval, y, yval, inside2 )

        write ( *, '(2g14.6,2x,l1,2x,l1)' ) xval, yval, inside1, inside2

    end do

    return
    end
    subroutine test55
    !
    !*******************************************************************************
    !
    !! TEST55 tests POLYGON_CONVEX.
    !
    implicit none
    !
    integer, parameter :: maxn = 10
    !
    real angle
    integer i
    character ( len = 80 ) message(-1:2)
    integer n
    integer result
    real x(maxn)
    real y(maxn)

    message(-1) = 'The polygon is not convex.'
    message( 0) = 'The polygon is degenerate and convex.'
    message( 1) = 'The polygon is convex and counterclockwise.'
    message( 2) = 'The polygon is convex and clockwise.'
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST55'
    write ( *, '(a)' ) '  POLYGON_CONVEX determines if a polygon'
    write ( *, '(a)' ) '    is convex.'
    !
    !  Shape 1: a point
    !
    n = 1

    x(1) = 0.0E+00
    y(1) = 0.0E+00

    call rvec2_print ( n, x, y, '  Shape #1, a point:' )
    result = polygon_convex ( n, x, y )

    write ( *, '(a)' ) message(result)
    !
    !  Shape 2: a line
    !
    n = 2

    x(1) = 0.0E+00
    y(1) = 0.0E+00

    x(2) = 1.0E+00
    y(2) = 2.0E+00

    call rvec2_print ( n, x, y, '  Shape #2, a line:' )

    result = polygon_convex ( n, x, y )

    write ( *, '(a)' ) message(result)
    !
    !  Shape 3: a flat "triangle."
    !
    n = 3
    x(1) = 0.0E+00
    y(1) = 0.0E+00

    x(2) = 2.0E+00
    y(2) = 0.0E+00

    x(3) = 1.0E+00
    y(3) = 0.0E+00

    call rvec2_print ( n, x, y, '  Shape #3, a flat triangle:' )

    result = polygon_convex ( n, x, y )

    write ( *, '(a)' ) message(result)
    !
    !  Shape 4: a triangle.
    !
    n = 3
    x(1) = 0.0E+00
    y(1) = 0.0E+00

    x(2) = 1.0E+00
    y(2) = 0.0E+00

    x(3) = 0.0E+00
    y(3) = 2.0E+00

    call rvec2_print ( n, x, y, '  Shape #4, a CCW triangle:' )

    result = polygon_convex ( n, x, y )

    write ( *, '(a)' ) message(result)
    !
    !  Shape 5: CW triangle.
    !
    n = 3
    x(1) = 0.0E+00
    y(1) = 0.0E+00

    x(2) = 0.0E+00
    y(2) = 2.0E+00

    x(3) = 1.0E+00
    y(3) = 0.0E+00

    call rvec2_print ( n, x, y, '  Shape #5, a CW triangle:' )

    result = polygon_convex ( n, x, y )

    write ( *, '(a)' ) message(result)
    !
    !  Shape 6: polygon with an interior angle of more than 90.
    !
    n = 4
    x(1) = 1.0E+00
    y(1) = 0.0E+00
    x(2) = 2.0E+00
    y(2) = 0.0E+00
    x(3) = 3.0E+00
    y(3) = 1.0E+00
    x(4) = 0.0E+00
    y(4) = 1.0E+00

    call rvec2_print ( n, x, y, &
        '  Shape #6, a polygon with large interior angle:' )
    result = polygon_convex ( n, x, y )

    write ( *, '(a)' ) message(result)
    !
    !  Shape 7: polygon with an interior angle of more than 180.
    !
    n = 5
    x(1) = 0.0E+00
    y(1) = 0.0E+00
    x(2) = 0.5E+00
    y(2) = 0.5E+00
    x(3) = 1.0E+00
    y(3) = 0.0E+00
    x(4) = 1.0E+00
    y(4) = 1.0E+00
    x(5) = 0.0E+00
    y(5) = 1.0E+00

    call rvec2_print ( n, x, y, '  Shape #7, a polygon with huge interior angle:' )

    result = polygon_convex ( n, x, y )

    write ( *, '(a)' ) message(result)
    !
    !  Shape 8: star
    !
    n = 5
    do i = 1, n
        angle = real ( i - 1 ) * 4.0E+00 * r_pi ( ) / real ( n )
        x(i) = cos ( angle )
        y(i) = sin ( angle )
    end do

    call rvec2_print ( n, x, y, '  Shape #8, a star:' )

    result = polygon_convex ( n, x, y )

    write ( *, '(a)' ) message(result)
    !
    !  Shape 9: regular hexagon
    !
    n = 6

    do i = 0, n-1
        angle = real ( i ) * 2.0E+00 * r_pi ( ) / real ( n )
        x(i+1) = cos ( angle )
        y(i+1) = sin ( angle )
    end do

    call rvec2_print ( n, x, y, '  Shape #9, a regular hexagon:' )

    result = polygon_convex ( n, x, y )

    write ( *, '(a)' ) message(result)
    !
    !  Shape 10: double triangle
    !
    n = 6

    x(1) = 0.0E+00
    y(1) = 0.0E+00

    x(2) = 2.0E+00
    y(2) = 0.0E+00

    x(3) = 1.0E+00
    y(3) = 1.0E+00

    x(4) = 0.0E+00
    y(4) = 0.0E+00

    x(5) = 2.0E+00
    y(5) = 0.0E+00

    x(6) = 1.0E+00
    y(6) = 1.0E+00

    call rvec2_print ( n, x, y, '  Shape #10, a double triangle:' )

    result = polygon_convex ( n, x, y )

    write ( *, '(a)' ) message(result)
    !
    !  Shape 11: "square knot"
    !
    n = 8

    x(1) = 1.0E+00
    y(1) = 0.0E+00

    x(2) = 3.0E+00
    y(2) = 0.0E+00

    x(3) = 3.0E+00
    y(3) = 3.0E+00

    x(4) = 0.0E+00
    y(4) = 3.0E+00

    x(5) = 0.0E+00
    y(5) = 1.0E+00

    x(6) = 2.0E+00
    y(6) = 1.0E+00

    x(7) = 2.0E+00
    y(7) = 2.0E+00

    x(8) = 1.0E+00
    y(8) = 2.0E+00

    call rvec2_print ( n, x, y, '  Shape #11, a square knot:' )

    result = polygon_convex ( n, x, y )

    write ( *, '(a)' ) message(result)

    return
    end
    subroutine test56
    !
    !*******************************************************************************
    !
    !! TEST56 tests POLYHEDRON_SURFACE_3D;
    !! TEST56 tests POLYHEDRON_VOLUME_3D.
    !
    implicit none
    !
    integer, parameter :: maxorder = 3
    integer, parameter :: nface = 4
    integer, parameter :: numnode = 4
    !
    real area
    real coord(3,numnode)
    integer i
    integer j
    integer node(nface,maxorder)
    integer order(nface)
    real volume
    !
    order(1:nface) = 3

    node(1,1) = 3
    node(1,2) = 2
    node(1,3) = 1

    node(2,1) = 1
    node(2,2) = 2
    node(2,3) = 4

    node(3,1) = 1
    node(3,2) = 4
    node(3,3) = 3

    node(4,1) = 2
    node(4,2) = 3
    node(4,3) = 4

    coord(1,1) = 0.0E+00
    coord(2,1) = 0.0E+00
    coord(3,1) = 0.0E+00

    coord(1,2) = 1.0E+00
    coord(2,2) = 0.0E+00
    coord(3,2) = 0.0E+00

    coord(1,3) = 0.0E+00
    coord(2,3) = 1.0E+00
    coord(3,3) = 0.0E+00

    coord(1,4) = 0.0E+00
    coord(2,4) = 0.0E+00
    coord(3,4) = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST56'
    write ( *, '(a)' ) '  For a polyhedron in 3D:'
    write ( *, '(a)' ) '  POLYHEDRON_SURFACE_3D computes surface area;'
    write ( *, '(a)' ) '  POLYHEDRON_VOLUME_3D computes volume.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  Number of faces is ', nface
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Order of each face:'
    write ( *, '(a)' ) ' '
    do i = 1, nface
        write ( *, '(2i6)' ) i, order(i)
    end do
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Nodes per face:'
    write ( *, '(a)' ) ' '
    do i = 1, nface
        write ( *, '(5i6)' ) i, ( node(i,j), j = 1, order(i) )
    end do
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Nodal coordinates:'
    write ( *, '(a)' ) ' '
    do i = 1, numnode
        write ( *, '(i6,3g14.6)' ) i, coord(1:3,i)
    end do

    call polyhedron_surface_3d ( coord, maxorder, nface, node, numnode, order, &
        area )

    write ( *, '(a,g14.6)' ) '  Surface area = ', area
    write ( *, '(a,g14.6)' ) '  Exact area = ', 1.5E+00 + sqrt ( 0.75E+00 )

    call polyhedron_volume_3d ( coord, maxorder, nface, node, numnode, order, &
        volume )

    write ( *, '(a,g14.6)' ) '  Volume = ', volume
    write ( *, '(a,g14.6)' ) '  Exact = ', 1.0E+00 / 6.0E+00

    return
    end
    subroutine test57
    !
    !*******************************************************************************
    !
    !! TEST57 tests POLYLINE_INDEX_POINT_ND;
    !! TEST57 tests POLYLINE_LENGTH_ND.
    !
    implicit none
    !
    integer, parameter :: maxpts = 4
    integer, parameter :: ndim = 2
    integer, parameter :: npts = maxpts
    !
    integer i
    integer j
    real stotal
    real sxpts(npts)
    real t
    real x(ndim)
    real xpts(npts,ndim)
    !
    xpts(1,1) = 0.0E+00
    xpts(1,2) = 0.0E+00

    xpts(2,1) = 1.0E+00
    xpts(2,2) = 1.0E+00

    xpts(3,1) = 2.0E+00
    xpts(3,2) = 0.0E+00

    xpts(4,1) = 0.0E+00
    xpts(4,2) = 0.0E+00

    t = 2.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST57'
    write ( *, '(a)' ) '  POLYLINE_INDEX_POINT_ND finds a point on a '
    write ( *, '(a)' ) '    polyline with given arclength.'
    write ( *, '(a)' ) '  POLYLINE_LENGTH_ND computes the arclength '
    write ( *, '(a)' ) '    of the polyline, and its nodes.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The line we examine is defined by these points:'
    write ( *, '(a)' ) ' '
    !
    !  The call to POLYLINE_LENGTH_ND is just to help us believe the final result
    !
    call polyline_length_nd ( maxpts, ndim, npts, sxpts, stotal, xpts )

    write ( *, '(a)' ) '      X            Y            Arclength(X,Y)'
    write ( *, '(a)' ) ' '
    do i = 1, npts
        write ( *, '(3g14.6)' ) xpts(i,1:ndim), sxpts(i)
    end do
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  We search for the point with coordinate ', t

    call polyline_index_point_nd ( maxpts, ndim, npts, t, xpts, x )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The computed point is'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) x(1:ndim)

    return
    end
    subroutine test58
    !
    !*******************************************************************************
    !
    !! TEST58 tests PROPLANE3.
    !
    implicit none
    !
    integer, parameter :: ntest = 5
    !
    integer i
    real x1
    real x2
    real x3
    real xo(ntest)
    real xp(ntest)
    real y1
    real y2
    real y3
    real yo(ntest)
    real yp(ntest)
    real z1
    real z2
    real z3
    real zo(ntest)
    real zp(ntest)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST58'
    write ( *, '(a)' ) '  PROPLANE3 projects an object point '
    write ( *, '(a)' ) '    orthographically into a plane.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (XO,YO,ZO), (XP,YP,ZP)'
    write ( *, '(a)' ) ' '

    x1 = 1.0E+00
    y1 = 0.0E+00
    z1 = 0.0E+00

    x2 = 0.0E+00
    y2 = 1.0E+00
    z2 = 0.0E+00

    x3 = 0.0E+00
    y3 = 0.0E+00
    z3 = 1.0E+00
    !
    !  Projection is ( -1, 1, 1 ).
    !
    xo(1) = 0.0E+00
    yo(1) = 2.0E+00
    zo(1) = 2.0E+00
    !
    !  Projection is ( 4, 5, -8 ).
    !
    xo(2) = 4.0E+00
    yo(2) = 5.0E+00
    zo(2) = -8.0E+00
    !
    !  Projection is ( 0.33, 0.33, 0.33).
    !
    xo(3) = 0.25E+00
    yo(3) = 0.25E+00
    zo(3) = 0.25E+00
    !
    !  Projection is ( 5.33, -1.66, -2.66 ).
    !
    xo(4) = 5.0E+00
    yo(4) = -2.0E+00
    zo(4) = -3.0E+00
    !
    !  Projection is ( -1, 1, 1 ).
    !
    xo(5) = -2.0E+00
    yo(5) = 0.0E+00
    zo(5) = 0.0E+00

    call proplane3 ( x1, y1, z1, x2, y2, z2, x3, y3, z3, ntest, xo, yo, zo, &
        xp, yp, zp )

    do i = 1, ntest
        write ( *, '(1x,6g12.4)' ) xo(i), yo(i), zo(i), xp(i), yp(i), zp(i)
    end do

    return
    end
    subroutine test170
    !
    !*******************************************************************************
    !
    !! TEST170 tests PROVEC.
    !
    implicit none
    !
    integer, parameter :: m = 4
    integer, parameter :: n = 2
    !
    real base(m, n)
    integer i
    integer j
    real vecm(m)
    real vecn(n)
    real vecnm(m)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST170'
    write ( *, '(a)' ) '  PROVEC projects a vector onto a subspace.'
    write ( *, '(a)' ) ' '

    base(1,1) = 4.0E+00
    base(2,1) = 3.0E+00
    base(3,1) = 2.0E+00
    base(4,1) = 1.0E+00

    base(1,2) = 1.0E+00
    base(2,2) = 2.0E+00
    base(3,2) = 3.0E+00
    base(4,2) = 4.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Base vectors:'
    write ( *, '(a)' ) ' '
    do j = 1, n
        write ( *, '(4g14.6)' ) base(1:m,j)
    end do

    vecm(1) = 1.0E+00
    vecm(2) = 1.0E+00
    vecm(3) = 1.0E+00
    vecm(4) = 2.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Vector to be projected:'
    write ( *, '(a)' ) ' '
    write ( *, '(4g14.6)' ) vecm(1:m)

    call provec ( base, m, n, vecm, vecn, vecnm )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Projected vector in BASE coordinates'
    write ( *, '(a)' ) ' '
    write ( *, '(4g14.6)' ) vecn(1:n)

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Projected vector in original coordinates'
    write ( *, '(a)' ) ' '
    write ( *, '(4g14.6)' ) vecnm(1:m)

    return
    end
    subroutine test171
    !
    !*******************************************************************************
    !
    !! TEST171 tests QUAD_AREA_2D;
    !! TEST171 tests QUAD_CONTAINS_POINT_2D;
    !! TEST171 tests QUAD_POINT_DIST_2D;
    !! TEST171 tests QUAD_POINT_DIST_SIGNED_2D.
    !
    implicit none
    !
    integer, parameter :: ntest = 7
    !
    real area
    real dist
    real dist_signed
    integer i
    logical inside
    real x
    real xtest(ntest)
    real x1
    real x2
    real x3
    real x4
    real y
    real ytest(ntest)
    real y1
    real y2
    real y3
    real y4
    !
    !  Coordinates of the corners.
    !
    x1 = 0.0E+00
    y1 = 0.0E+00

    x2 = 1.0E+00
    y2 = 0.0E+00

    x3 = 1.0E+00
    y3 = 1.0E+00

    x4 = 0.0E+00
    y4 = 1.0E+00
    !
    !  Coordinates of the test points.
    !
    xtest(1) = 0.25E+00
    ytest(1) = 0.25E+00

    xtest(2) = 0.75E+00
    ytest(2) = 0.25E+00

    xtest(3) = 1.0E+00
    ytest(3) = 1.0E+00

    xtest(4) = 11.0E+00
    ytest(4) = 0.5E+00

    xtest(5) = 0.0E+00
    ytest(5) = 0.5E+00

    xtest(6) = 0.5E+00
    ytest(6) = -10.0E+00

    xtest(7) = 2.0E+00
    ytest(7) = 2.0E+00
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST171'
    write ( *, '(a)' ) '  For a quadrilateral in 2D:'
    write ( *, '(a)' ) '  QUAD_AREA_2D finds the area;'
    write ( *, '(a)' ) '  QUAD_CONTAINS_POINT_2D tells if a point is inside;'
    write ( *, '(a)' ) '  QUAD_POINT_DIST_2D computes the distance.'
    write ( *, '(a)' ) '  QUAD_POINT_DIST_SIGNED_2D computes signed distance.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The corners:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) x1, y1
    write ( *, '(2g14.6)' ) x2, y2
    write ( *, '(2g14.6)' ) x3, y3
    write ( *, '(2g14.6)' ) x4, y4

    call quad_area_2d ( x1, y1, x2, y2, x3, y3, x4, y4, area )

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  QUAD_AREA_2D reports the area is ', area

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     X	 Y     INQUAD	DISQUAD DISQUAD'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x = xtest(i)
        y = ytest(i)

        call quad_contains_point_2d ( x1, y1, x2, y2, x3, y3, x4, y4, x, y, inside )

        call quad_point_dist_signed_2d ( x1, y1, x2, y2, x3, y3, x4, y4, x, y, &
            dist_signed )

        call quad_point_dist_2d ( x1, y1, x2, y2, x3, y3, x4, y4, x, y, dist )

        write ( *, '(2g14.6,2x,l1,2x,2f12.4)' ) x, y, inside, dist_signed, dist

    end do

    return
    end
    subroutine test172
    !
    !*******************************************************************************
    !
    !! TEST172 tests QUAT_CONJ;
    !! TEST172 tests QUAT_INV;
    !! TEST172 tests QUAT_MUL;
    !! TEST172 tests QUAT_NORM.
    !
    implicit none
    !
    integer i
    real q1(4)
    real q2(4)
    real q3(4)
    !
    q1(1) = 2.0E+00
    q1(2) = 3.0E+00
    q1(3) = 4.0E+00
    q1(4) = 5.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST172'
    write ( *, '(a)' ) '  QUAT_CONJ conjugats a quaternion;'
    write ( *, '(a)' ) '  QUAT_INV inverts a quaternion;'
    write ( *, '(a)' ) '  QUAT_MUL multiplies quaternions.'
    write ( *, '(a)' ) '  QUAT_NORM computes the norm.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,4g14.6)' ) '  Q1 =            ', q1(1:4)

    write ( *, '(a,g14.6)' ) '  Norm ( Q1 ) = ', quat_norm ( q1 )

    q2(1:4) = q1(1:4)

    call quat_conj ( q2 )

    write ( *, '(a,4g14.6)' ) '  Q2 = conj(Q1) = ', q2(1:4)

    call quat_mul ( q1, q2, q3 )

    write ( *, '(a,4g14.6)' ) '  Q3 = Q1*Q2 =    ', q3(1:4)

    q2(1:4) = q1(1:4)

    call quat_inv ( q2 )

    write ( *, '(a,4g14.6)' ) '  Q2 = inv(Q1) =  ', q2(1:4)

    call quat_mul ( q1, q2, q3 )

    write ( *, '(a,4g14.6)' ) '  Q3 = Q1*Q2 =    ', q3(1:4)

    return
    end
    subroutine test173
    !
    !*******************************************************************************
    !
    !! TEST173 tests RADEC_DISTANCE_3D.
    !
    implicit none
    !
    integer, parameter :: ntest = 6
    !
    real dec1
    real dec2
    integer i
    integer j
    real ra1
    real ra2
    real theta
    real theta_deg
    real xtest(ntest)
    real ytest(ntest)
    real ztest(ntest)
    !
    xtest(1) = 1.0E+00
    ytest(1) = 0.0E+00
    ztest(1) = 0.0E+00

    xtest(2) = 0.0E+00
    ytest(2) = 1.0E+00
    ztest(2) = 0.0E+00

    xtest(3) = 0.0E+00
    ytest(3) = 0.0E+00
    ztest(3) = 1.0E+00

    xtest(4) = 1.0E+00
    ytest(4) = 1.0E+00
    ztest(4) = 1.0E+00

    xtest(5) =  5.0E+00
    ytest(5) = -2.0E+00
    ztest(5) = -1.0E+00

    xtest(6) = -2.0E+00
    ytest(6) = -2.0E+00
    ztest(6) = -2.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST173'
    write ( *, '(a)' ) '  RADEC_DISTANCE_3D computes the angular separation'
    write ( *, '(a)' ) '    between two points on a sphere described in terms of'
    write ( *, '(a)' ) '    right ascension and declination.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) &
        '     RA1       DEC1      RA2       DEC2    Radians  Degrees'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        call xyz_to_radec ( xtest(i), ytest(i), ztest(i), ra1, dec1 )

        do j = i+1, ntest

            call xyz_to_radec ( xtest(j), ytest(j), ztest(j), ra2, dec2 )
            call radec_distance_3d ( ra1, dec1, ra2, dec2, theta )
            theta_deg = radians_to_degrees ( theta )
            write ( *, '(6f10.4)' ) ra1, dec1, ra2, dec2, theta, theta_deg

        end do

    end do

    return
    end
    subroutine test174
    !
    !*******************************************************************************
    !
    !! TEST174 tests RADEC_TO_XYZ.
    !! TEST174 tests XYZ_TO_RADEC.
    !
    implicit none
    !
    integer, parameter :: ntest = 6
    !
    real dec
    integer i
    real ra
    real xtest(ntest)
    real x2
    real ytest(ntest)
    real y2
    real ztest(ntest)
    real z2
    !
    xtest(1) = 1.0E+00
    ytest(1) = 0.0E+00
    ztest(1) = 0.0E+00

    xtest(2) = 0.0E+00
    ytest(2) = 1.0E+00
    ztest(2) = 0.0E+00

    xtest(3) = 0.0E+00
    ytest(3) = 0.0E+00
    ztest(3) = 1.0E+00

    xtest(4) = 1.0E+00
    ytest(4) = 1.0E+00
    ztest(4) = 1.0E+00

    xtest(5) =  5.0E+00
    ytest(5) = -2.0E+00
    ztest(5) = -1.0E+00

    xtest(6) = -2.0E+00
    ytest(6) = -2.0E+00
    ztest(6) = -2.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST174'
    write ( *, '(a)' ) '  RADEC_TO_XYZ converts XYZ to RADEC coordinates.'
    write ( *, '(a)' ) '  XYZ_TO_RADEC converts RADEC to XYZ coordinates.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '   X      Y      Z      RA     DEC    X      Y      Z'
    write ( *, '(a)' ) ' '

    do i = 1, ntest
        call xyz_to_radec ( xtest(i), ytest(i), ztest(i), ra, dec )
        call radec_to_xyz ( ra, dec, x2, y2, z2 )
        write ( *, '(8f7.3)' ) xtest(i), ytest(i), ztest(i), ra, dec, x2, y2, z2
    end do

    return
    end
    subroutine test175
    !
    !*******************************************************************************
    !
    !! TEST175 tests ROTATION_AXIS_VECTOR_3D;
    !! TEST175 tests ROTATION_MAT_VECTOR_3D;
    !! TEST175 tests ROTATION_QUAT_VECTOR_3D.
    !
    implicit none
    !
    real a(3,3)
    real angle
    real axis(3)
    integer i
    integer j
    real q(4)
    real v(3)
    real w(3)
    !
    axis(1) =   0.2361737E+00
    axis(2) = - 0.8814124E+00
    axis(3) = - 0.4090649E+00

    angle = 1.159804E+00

    v(1:3) = (/ 1.0E+00, 4.0E+00, 10.0E+00 /)

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST175'
    write ( *, '(a)' ) '  ROTATION_AXIS_VECTOR_3D applies an axis'
    write ( *, '(a)' ) '    rotation to a vector;'
    write ( *, '(a)' ) '  ROTATION_MAT_VECTOR_3D applies a matrix'
    write ( *, '(a)' ) '    rotation to a vector.'
    write ( *, '(a)' ) '  ROTATION_QUAT_VECTOR_3D applies a quaternion'
    write ( *, '(a)' ) '    rotation to a vector.'

    write ( *, '(a)' ) '  The vector is:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) v(1:3)
    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  Rotation axis is ', axis(1:3)
    write ( *, '(a,g14.6)' ) '  Rotation angle is ', angle

    call rotation_axis_vector_3d ( axis, angle, v, w )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotated vector is:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) w(1:3)
    write ( *, '(a)' ) ' '

    call rotation_axis2mat_3d ( axis, angle, a )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotation matrix:'
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(3g14.6)' ) a(i,1:3)
    end do

    call rotation_mat_vector_3d ( a, v, w )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotated vector is:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) w(1:3)
    write ( *, '(a)' ) ' '

    call rotation_axis2quat_3d ( axis, angle, q )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotation quaternion:'
    write ( *, '(a)' ) ' '
    write ( *, '(4g14.6)' ) q(1:4)

    call rotation_quat_vector_3d ( q, v, w )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotated vector is:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) w(1:3)
    write ( *, '(a)' ) ' '

    return
    end
    subroutine test176
    !
    !*******************************************************************************
    !
    !! TEST176 tests ROTATION_AXIS2MAT_3D;
    !! TEST176 tests ROTATION_MAT2AXIS_3D.
    !
    implicit none
    !
    real a(3,3)
    real angle
    real axis(3)
    integer i
    integer j
    !
    a(1,1) = sqrt ( 3.0E+00 ) / 4.0E+00
    a(2,1) = - 0.5E+00
    a(3,1) =   0.75E+00

    a(1,2) =   0.25E+00
    a(2,2) = sqrt ( 3.0E+00 ) / 2.0E+00
    a(3,2) = sqrt ( 3.0E+00 ) / 4.0E+00

    a(1,3) = - sqrt ( 3.0E+00 ) / 2.0E+00
    a(2,3) = 0.0E+00
    a(3,3) = 0.5E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST176'
    write ( *, '(a)' ) '  ROTATION_MAT2AXIS_3D computes a rotation axis'
    write ( *, '(a)' ) '    and angle from a rotation matrix.'
    write ( *, '(a)' ) '  ROTATION_AXIS2MAT_3D computes a rotation matrix'
    write ( *, '(a)' ) '    from a rotation axis and angle.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotation matrix:'
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(3g14.6)' ) a(i,1:3)
    end do

    call rotation_mat2axis_3d ( a, axis, angle )

    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  Rotation axis is ', axis(1:3)
    write ( *, '(a,g14.6)' ) '  Rotation angle is ', angle

    a(1:3,1:3) = 0.0E+00

    call rotation_axis2mat_3d ( axis, angle, a )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotation matrix:'
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(3g14.6)' ) a(i,1:3)
    end do

    return
    end
    subroutine test177
    !
    !*******************************************************************************
    !
    !! TEST177 tests ROTATION_AXIS2QUAT_3D;
    !! TEST177 tests ROTATION_QUAT2AXIS_3D.
    !
    implicit none
    !
    real angle
    real axis(3)
    integer i
    real q(4)
    !
    axis(1) =   0.2361737E+00
    axis(2) = - 0.8814124E+00
    axis(3) = - 0.4090649E+00

    angle = 1.159804E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST177'
    write ( *, '(a)' ) '  ROTATION_QUAT2AXIS_3D computes a rotation axis'
    write ( *, '(a)' ) '    and angle from a rotation quaternion.'
    write ( *, '(a)' ) '  ROTATION_AXIS2QUAT_3D computes a rotation'
    write ( *, '(a)' ) '    quaternion from a rotation axis and angle.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotation axis:'
    write ( *, '(4x,3g14.6)' ) axis(1:3)
    write ( *, '(a,g14.6)' ) '  Rotation angle is ', angle

    call rotation_axis2quat_3d ( axis, angle, q )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotation quaternion:'
    write ( *, '(a)' ) ' '
    write ( *, '(4g14.6)' ) q(1:4)

    call rotation_quat2axis_3d ( q, axis, angle )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotation axis:'
    write ( *, '(3g14.6)' ) axis(1:3)
    write ( *, '(a,g14.6)' ) '  Rotation angle is ', angle

    return
    end
    subroutine test178
    !
    !*******************************************************************************
    !
    !! TEST178 tests ROTATION_MAT2QUAT_3D;
    !! TEST178 tests ROTATION_QUAT2MAT_3D.
    !
    implicit none
    !
    real a(3,3)
    integer i
    integer j
    real q(4)
    !
    a(1,1) = sqrt ( 3.0E+00 ) / 4.0E+00
    a(2,1) = - 0.5E+00
    a(3,1) =   0.75E+00

    a(1,2) =   0.25E+00
    a(2,2) = sqrt ( 3.0E+00 ) / 2.0E+00
    a(3,2) = sqrt ( 3.0E+00 ) / 4.0E+00

    a(1,3) = - sqrt ( 3.0E+00 ) / 2.0E+00
    a(2,3) = 0.0E+00
    a(3,3) = 0.5E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST178'
    write ( *, '(a)' ) '  ROTATION_MAT2QUAT_3D computes a rotation'
    write ( *, '(a)' ) '    quaternion from a rotation matrix.'
    write ( *, '(a)' ) '  ROTATION_QUAT2MAT_3D computes a rotation matrix'
    write ( *, '(a)' ) '    from a rotation quaternion.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotation matrix:'
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(3g14.6)' ) a(i,1:3)
    end do

    call rotation_mat2quat_3d ( a, q )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotation quaternion:'
    write ( *, '(4g14.6)' ) q(1:4)

    a(1:3,1:3) = 0.0E+00

    call rotation_quat2mat_3d ( q, a )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotation matrix:'
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(3g14.6)' ) a(i,1:3)
    end do

    return
    end
    subroutine test179
    !
    !*******************************************************************************
    !
    !! TEST179 tests SOCCER_SHAPE_3D.
    !
    implicit none
    !
    real area
    integer face_num
    integer i
    integer j
    integer k
    integer point_num
    real point_coord(3,60)
    integer face_order(32)
    integer face_point(6,32)
    real normal(3)
    real x(6)
    real xave
    real y(6)
    real yave
    real z(6)
    real zave
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST179'
    write ( *, '(a)' ) '  SOCCER_SHAPE_3D returns information abou the'
    write ( *, '(a)' ) '  truncated icosahedron, or soccer ball.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  We will use this information to compute the'
    write ( *, '(a)' ) '  areas and centers of each face.'

    call soccer_shape_3d ( point_num, face_num, face_order, point_coord, &
        face_point )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Face  Order  Area'
    write ( *, '(a)' ) ' '

    do i = 1, face_num

        do j = 1, face_order(i)

            k = face_point(j,i)
            x(j) = point_coord(1,k)
            y(j) = point_coord(2,k)
            z(j) = point_coord(3,k)
        end do

        call polygon_area_3d ( face_order(i), x, y, z, area, normal )

        write ( *, '(i6,i7,f8.4)' ) i, face_order(i), area

    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Face  Center'
    write ( *, '(a)' ) ' '

    do i = 1, face_num

        xave = 0.0E+00
        yave = 0.0E+00
        zave = 0.0E+00

        do j = 1, face_order(i)
            k = face_point(j,i)
            xave = xave + point_coord(1,k)
            yave = yave + point_coord(2,k)
            zave = zave + point_coord(3,k)
        end do

        xave = xave / real ( face_order(i) )
        yave = yave / real ( face_order(i) )
        zave = zave / real ( face_order(i) )

        write ( *, '(i6,3f8.4)' ) i, xave, yave, zave

    end do

    return
    end
    subroutine test180
    !
    !*******************************************************************************
    !
    !! TEST180 tests SORT_HEAP_EXTERNAL.
    !
    implicit none
    !
    integer, parameter :: n = 20
    !
    integer i
    integer iarray(n)
    integer indx
    integer isgn
    integer itemp
    integer j
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST180'
    write ( *, '(a)' ) '  SORT_HEAP_EXTERNAL sorts objects externally.'
    write ( *, '(a)' ) ' '

    indx = 0
    i = 0
    j = 0
    isgn = 0

    do i = 1, n
        call i_random ( 1, n, iarray(i) )
    end do

    write ( *, '(a)' )  '  Before sorting:'
    write ( *, '(10i4)' ) iarray(1:n)

    do

        call sort_heap_external ( n, indx, i, j, isgn )

        if ( indx < 0 ) then
            if ( iarray(i) <= iarray(j) ) then
                isgn = -1
            else
                isgn = +1
            end if
        else if ( indx > 0 ) then
            call i_swap ( iarray(i), iarray(j) )
        else
            exit
        end if

    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  After sorting:'
    write ( *, '(10i4)' ) iarray(1:n)

    return
    end
    subroutine test181
    !
    !*******************************************************************************
    !
    !! TEST181 tests SPHERE_DIA2IMP_3D.
    !
    implicit none
    !
    real r
    real x1
    real x2
    real xc
    real y1
    real y2
    real yc
    real z1
    real z2
    real zc
    !
    x1 = - 1.0E+00
    y1 = - 1.0E+00
    z1 =   4.0E+00

    x2 = 5.0E+00
    y2 = 7.0E+00
    z2 = 4.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST181'
    write ( *, '(a)' ) '  SPHERE_DIA2IMP_3D converts a sphere from'
    write ( *, '(a)' ) '    diameter to implicit form.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The endpoints of the diameter are:'
    write ( *, '(a,3g14.6)' ) '    P1:', x1, y1, z1
    write ( *, '(a,3g14.6)' ) '	 P2:', x2, y2, z2

    call sphere_dia2imp_3d ( x1, y1, z1, x2, y2, z2, r, xc, yc, zc )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The implicit form of the sphere is '
    write ( *, '(a,3g14.6)' ) '    Center: ', xc, yc, zc
    write ( *, '(a,g14.6)' ) '    Radius: ', r

    return
    end
    subroutine test182
    !
    !*******************************************************************************
    !
    !! TEST182 tests SPHERE_EXP_CONTAINS_POINT_3D.
    !! TEST182 tests SPHERE_IMP_CONTAINS_POINT_3D.
    !
    implicit none
    !
    integer, parameter :: ntest = 4
    !
    integer i
    logical inside
    real r
    real x(ntest)
    real x1
    real x2
    real x3
    real x4
    real xc
    real y(ntest)
    real y1
    real y2
    real y3
    real y4
    real yc
    real z(ntest)
    real z1
    real z2
    real z3
    real z4
    real zc
    !
    !  Define the sphere of radius 3, and center 1, 2, 3.
    !
    xc = 1.0E+00
    yc = 2.0E+00
    zc = 3.0E+00

    r = 3.0E+00
    !
    !  Define 4 points on the sphere, for the explicit tests.
    !
    x1 = xc + r
    y1 = yc
    z1 = zc

    x2 = xc
    y2 = yc + r
    z2 = zc

    x3 = xc
    y3 = yc
    z3 = zc + r

    x4 = xc - r
    y4 = yc
    z4 = zc
    !
    !  Set the test points.
    !
    x(1) = xc
    y(1) = yc
    z(1) = zc

    x(2) = xc + 2.0E+00 * r
    y(2) = yc
    z(2) = zc

    x(3) = xc
    y(3) = yc + r
    z(3) = zc

    x(4) = xc + 0.5E+00 * r
    y(4) = yc + 0.5E+00 * r
    z(4) = zc + 0.5E+00 * r

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST182'
    write ( *, '(a)' ) '  SPHERE_EXP_CONTAINS_POINT_3D determines if a'
    write ( *, '(a)' ) '    point is within an explicit sphere;'
    write ( *, '(a)' ) '  SPHERE_IMP_CONTAINS_POINT_3D determines if a'
    write ( *, '(a)' ) '    point is within an implicit sphere;'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  SPHERE_EXP_CONTAINS_POINT_3D:'
    write ( *, '(a)' ) '    Inside, X, Y, Z'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        call sphere_exp_contains_point_3d ( x1, y1, z1, x2, y2, z2, &
            x3, y3, z3, x4, y4, z4, x(i), y(i), z(i), inside )

        write ( *, '(l1,3g14.6)' ) inside, x(i), y(i), z(i)

    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  SPHERE_IMP_CONTAINS_POINT_3D:'
    write ( *, '(a)' ) '    Inside, X, Y, Z'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        call sphere_imp_contains_point_3d ( r, xc, yc, zc, x(i), y(i), z(i), &
            inside )

        write ( *, '(l1,3g14.6)' ) inside, x(i), y(i), z(i)

    end do

    return
    end
    subroutine test183
    !
    !*******************************************************************************
    !
    !! TEST183 tests SPHERE_EXP_NEAR_POINT_3D.
    !! TEST183 tests SPHERE_IMP_NEAR_POINT_3D.
    !
    implicit none
    !
    integer, parameter :: ntest = 4
    !
    integer i
    real r
    real x(ntest)
    real x1
    real x2
    real x3
    real x4
    real xc
    real xn
    real y(ntest)
    real y1
    real y2
    real y3
    real y4
    real yc
    real yn
    real z(ntest)
    real z1
    real z2
    real z3
    real z4
    real zc
    real zn
    !
    !  Define the sphere of radius 3, and center 1, 2, 3.
    !
    xc = 1.0E+00
    yc = 2.0E+00
    zc = 3.0E+00

    r = 3.0E+00
    !
    !  Define 4 points on the sphere, for the explicit tests.
    !
    x1 = xc + r
    y1 = yc
    z1 = zc

    x2 = xc
    y2 = yc + r

    z2 = zc

    x3 = xc
    y3 = yc
    z3 = zc + r

    x4 = xc - r
    y4 = yc
    z4 = zc
    !
    !  Set the test points.
    !
    x(1) = xc
    y(1) = yc
    z(1) = zc

    x(2) = xc + 2.0E+00 * r
    y(2) = yc
    z(2) = zc

    x(3) = xc
    y(3) = yc + r
    z(3) = zc

    x(4) = xc + 0.5E+00 * r
    y(4) = yc + 0.5E+00 * r
    z(4) = zc + 0.5E+00 * r

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST183'
    write ( *, '(a)' ) '  SPHERE_EXP_NEAR_POINT_3D determines if a'
    write ( *, '(a)' ) '    point is within an explicit sphere;'
    write ( *, '(a)' ) '  SPHERE_IMP_NEAR_POINT_3D determines if a'
    write ( *, '(a)' ) '    point is within an implicit sphere;'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The sphere has center:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) xc, yc, zc
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  and radius ', r
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  SPHERE_EXP_NEAR_POINT_3D:'
    write ( *, '(a)' ) '    X, Y, Z, XN, YN, ZN'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        call sphere_exp_near_point_3d ( x1, y1, z1, x2, y2, z2, &
            x3, y3, z3, x4, y4, z4, x(i), y(i), z(i), xn, yn, zn )

        write ( *, '(6f10.4)' ) x(i), y(i), z(i), xn, yn, zn

    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  SPHERE_IMP_NEAR_POINT_3D:'
    write ( *, '(a)' ) '    X, Y, Z, XN, YN, ZN'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        call sphere_imp_near_point_3d ( r, xc, yc, zc, x(i), y(i), z(i), &
            xn, yn, zn )

        write ( *, '(6f10.4)' ) x(i), y(i), z(i), xn, yn, zn

    end do

    return
    end
    subroutine test184
    !
    !*******************************************************************************
    !
    !! TEST184 tests SPHERE_IMP_GRIDPOINTS_3D.
    !
    implicit none
    !
    integer, parameter :: maxpoint = 100
    !
    integer i
    integer j
    integer k
    integer nlat
    integer nlong
    integer npoint
    real r
    real x(maxpoint)
    real xc
    real y(maxpoint)
    real yc
    real z(maxpoint)
    real zc
    !
    r = 10.0E+00

    xc = 0.0E+00
    yc = 0.0E+00
    zc = 0.0E+00

    nlat = 3
    nlong = 4

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST184'
    write ( *, '(a)' ) '  SPHERE_IMP_GRIDPOINTS_3D produces a grid of'
    write ( *, '(a)' ) '  points on an implicit sphere in 3D.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The sphere has center:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) xc, yc, zc
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  and radius ', r
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  The number of intermediate latitudes = ', nlat
    write ( *, '(a,i6)' ) '  The number of longitudes = ', nlong

    call sphere_imp_gridpoints_3d ( r, xc, yc, zc, maxpoint, &
        nlat, nlong, npoint, x, y, z )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  The number of grid points is ', npoint
    write ( *, '(a)' ) ' '

    k = 1
    write ( *, '(i6,3g14.6)' ) k, x(k), y(k), z(k)

    do i = 1, nlat
        write ( *, '(a)' ) ' '
        do j = 0, nlong - 1
            k = k + 1
            write ( *, '(i6,3g14.6)' ) k, x(k), y(k), z(k)
        end do
    end do

    k = k + 1
    write ( *, '(a)' ) ' '
    write ( *, '(i6,3g14.6)' ) k, x(k), y(k), z(k)

    return
    end
    subroutine test185
    !
    !*******************************************************************************
    !
    !! TEST185 tests SPHERE_IMP_SPIRALPOINTS_3D.
    !
    implicit none
    !
    integer, parameter :: n = 20
    !
    integer i
    real r
    real x(n)
    real xc
    real y(n)
    real yc
    real z(n)
    real zc
    !
    r = 1.0E+00

    xc = 0.0E+00
    yc = 0.0E+00
    zc = 0.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST185'
    write ( *, '(a)' ) '  SPHERE_IMP_SPIRALPOINTS_3D produces a spiral of'
    write ( *, '(a)' ) '  points on an implicit sphere in 3D.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The sphere has center:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) xc, yc, zc
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  and radius ', r
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  The number of spiral points is ', n

    call sphere_imp_spiralpoints_3d ( r, xc, yc, zc, n, x, y, z )

    write ( *, * ) ' '
    write ( *, * ) '  The spiral points:'
    write ( *, * ) ' '

    do i = 1, n
        write ( *, '(3f8.4)' ) x(i), y(i), z(i)
    end do

    return
    end
    subroutine test186
    !
    !*******************************************************************************
    !
    !! TEST186 tests SPHERE_IMP_GRIDLINES_3D.
    !
    implicit none
    !
    integer, parameter :: maxline = 1000
    !
    integer i
    integer line(2,maxline)
    integer nlat
    integer nline
    integer nlong
    !
    nlat = 3
    nlong = 4

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST186'
    write ( *, '(a)' ) '  SPHERE_IMP_GRIDLINES_3D computes gridlines'
    write ( *, '(a)' ) '  on a sphere in 3D.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  Number of intermediate latitudes is ', nlat
    write ( *, '(a,i6)' ) '  Number of longitudes is ', nlong

    call sphere_imp_gridlines_3d ( maxline, nlat, nlong, nline, line )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) 'Number of line segments is ', nline

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Grid line vertices:'
    write ( *, '(a)' ) ' '

    do i = 1, nline
        write ( *, '(2i6)' ) line(1,i), line(2,i)
    end do

    return
    end
    subroutine test187
    !
    !*******************************************************************************
    !
    !! TEST187 tests SPHERE_IMP_GRIDFACES_3D.
    !
    implicit none
    !
    integer, parameter :: maxtri = 1000
    !
    integer i
    integer nlat
    integer nlong
    integer ntri
    integer tri(3,maxtri)
    !
    nlat = 3
    nlong = 4

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST187'
    write ( *, '(a)' ) '  SPHERE_IMP_GRIDFACES_3D computes gridfaces'
    write ( *, '(a)' ) '  on a sphere in 3D.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  Number of intermediate latitudes is ', nlat
    write ( *, '(a,i6)' ) '  Number of longitudes is ', nlong

    call sphere_imp_gridfaces_3d ( maxtri, nlat, nlong, ntri, tri )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) 'Number of triangles is ', ntri

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Triangle vertices:'
    write ( *, '(a)' ) ' '

    do i = 1, ntri
        write ( *, '(3g14.6)' ) tri(1,i), tri(2,i), tri(3,i)
    end do

    return
    end
    subroutine test188
    !
    !*******************************************************************************
    !
    !! TEST188 tests SPHERE_IMP_POINT_PROJECT_3D.
    !
    implicit none
    !
    integer, parameter :: ntest = 4
    !
    integer i
    real r
    real x(ntest)
    real xc
    real xp(ntest)
    real y(ntest)
    real yc
    real yp(ntest)
    real z(ntest)
    real zc
    real zp(ntest)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST188'
    write ( *, '(a)' ) '  SPHERE_IMP_POINT_PROJECT_3D projects a 3D point'
    write ( *, '(a)' ) '    onto a sphere.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (X,Y,Z), Projected (X,Y,Z)'
    write ( *, '(a)' ) ' '

    r = 2.0E+00
    xc = 2.0E+00
    yc = 4.0E+00
    zc = 0.0E+00

    x(1) = 2.0E+00
    y(1) = 0.0E+00
    z(1) = 0.0E+00

    x(2) = 0.0E+00
    y(2) = 4.0E+00
    z(2) = 0.0E+00

    x(3) = 2.0E+00
    y(3) = 4.0E+00
    z(3) = 10.0E+00

    x(4) = 3.0E+00
    y(4) = 5.0E+00
    z(4) = 0.0E+00

    do i = 1, ntest

        call sphere_imp_point_project_3d ( r, xc, yc, zc, x(i), y(i), z(i), &
            xp(i), yp(i), zp(i) )

        write ( *, '(6g12.4)' ) x(i), y(i), z(i), xp(i), yp(i), zp(i)
    end do

    return
    end
    subroutine test189
    !
    !*******************************************************************************
    !
    !! TEST189 tests SPHERE_IMP_AREA_ND.
    !! TEST189 tests SPHERE_IMP_VOLUME_ND.
    !
    real area
    integer n
    real, parameter :: r = 1.0E+00
    real volume
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST189'
    write ( *, '(a)' ) '  For a implicit sphere in N dimensions:'
    write ( *, '(a)' ) '  SPHERE_IMP_AREA_ND computes the area;'
    write ( *, '(a)' ) '  SPHERE_IMP_VOLUME_ND computes the volume.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  We use a radius of R = ', r
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  N    Area    Volume'
    write ( *, '(a)' ) ' '

    do n = 2, 10
        call sphere_imp_area_nd ( n, r, area )
        call sphere_imp_volume_nd ( n, r, volume )
        write ( *, '(i3,2g14.6)' ) n, area, volume
    end do

    return
    end
    subroutine test190
    !
    !*******************************************************************************
    !
    !! TEST190 tests SPHERE_UNIT_SAMPLE_2D.
    !
    integer, parameter :: n = 2
    !
    real average(n)
    real average_dot
    real dot_average
    integer i
    integer j
    integer, parameter :: n_sample = 1000
    integer seed
    real v(n)
    real x(n)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST190'
    write ( *, '(a)' ) '  For the unit sphere in 2 dimensions (the circle):'
    write ( *, '(a)' ) '  SPHERE_UNIT_SAMPLE_2D samples;'

    seed = 123456789
    call random_seed ( seed )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  A few sample values:'
    write ( *, '(a)' ) ' '

    do i = 1, 5
        call sphere_unit_sample_2d ( x )
        write ( *, '(2f8.4)' ) x(1:n)
    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  Number of sample points = ', n_sample

    seed = 123456789
    call random_seed ( seed )

    average(1:n) = 0.0E+00

    do i = 1, n_sample
        call sphere_unit_sample_2d ( x )
        average(1:n) = average(1:n) + x(1:n)
    end do

    average(1:n) = average(1:n) / real ( n_sample )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now average the points, which should get a value'
    write ( *, '(a)' ) '  close to zero, and closer as N increases.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2f8.4)' ) '  Average:        ', average(1:n)

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now choose a random direction, sample the same'
    write ( *, '(a)' ) '  number of points, and compute the dot product with'
    write ( *, '(a)' ) '  the direction.'
    write ( *, '(a)' ) '  Take the absolute value of each dot product '
    write ( *, '(a)' ) '  and sum and average.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  We expect a value near 2 / PI = 0.6366...'

    do j = 1, 5

        seed = 0
        call random_seed ( seed )

        call sphere_unit_sample_2d ( v )

        seed = 123456789
        call random_seed ( seed )

        dot_average = 0.0E+00

        do i = 1, n_sample
            call sphere_unit_sample_2d ( x )
            dot_average = dot_average + abs ( dot_product ( x(1:n), v(1:n) ) )
        end do

        dot_average = dot_average / real ( n_sample )

        write ( *, '(a)' ) ' '
        write ( *, '(a,2f8.4)' ) '  V:                ', v(1:n)
        write ( *, '(a, f8.4)' ) '  Average |(XdotV)| ', dot_average

    end do

    return
    end
    subroutine test191
    !
    !*******************************************************************************
    !
    !! TEST191 tests SPHERE_UNIT_SAMPLE_3D.
    !
    integer, parameter :: n = 3
    !
    real average(n)
    real dot_average
    integer i
    integer j
    integer, parameter :: n_sample = 1000
    integer seed
    real v(n)
    real x(n)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST191'
    write ( *, '(a)' ) '  For the unit sphere in 3 dimensions:'
    write ( *, '(a)' ) '  SPHERE_UNIT_SAMPLE_3D samples;'

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  A few sample values:'
    write ( *, '(a)' ) ' '

    seed = 123456789
    call random_seed ( seed )

    do i = 1, 5
        call sphere_unit_sample_3d ( x )
        write ( *, '(3f8.4)' ) x(1:n)
    end do

    average(1:n) = 0.0E+00

    do i = 1, n_sample
        call sphere_unit_sample_3d ( x )
        average(1:n) = average(1:n) + x(1:n)
    end do

    average(1:n) = average(1:n) / real ( n_sample )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now average the points, which should get a value'
    write ( *, '(a)' ) '  close to zero, and closer as N increases.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3f8.4)' ) '  Average:        ', average(1:n)

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now choose a random direction, sample the same'
    write ( *, '(a)' ) '  number of points, and compute the dot product with'
    write ( *, '(a)' ) '  the direction.'
    write ( *, '(a)' ) '  Take the absolute value of each dot product '
    write ( *, '(a)' ) '  and sum and average.'

    do j = 1, 5

        seed = 0
        call random_seed ( seed )

        call sphere_unit_sample_3d ( v )

        seed = 123456789
        call random_seed ( seed )

        dot_average = 0.0E+00

        do i = 1, n_sample
            call sphere_unit_sample_3d ( x )
            dot_average = dot_average + abs ( dot_product ( x(1:n), v(1:n) ) )
        end do

        dot_average = dot_average / real ( n_sample )

        write ( *, '(a)' ) ' '
        write ( *, '(a,3f8.4)' ) '  V:                ', v(1:n)
        write ( *, '(a, f8.4)' ) '  Average |(XdotV)| ', dot_average

    end do

    return
    end
    subroutine test192
    !
    !*******************************************************************************
    !
    !! TEST192 tests SPHERE_UNIT_SAMPLE2_3D.
    !
    integer, parameter :: n = 3
    !
    real average(n)
    real dot_average
    integer i
    integer j
    integer, parameter :: n_sample = 1000
    integer seed
    real v(n)
    real x(n)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST192'
    write ( *, '(a)' ) '  For the unit sphere in 3 dimensions:'
    write ( *, '(a)' ) '  SPHERE_UNIT_SAMPLE2_3D samples;'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Warning: SPHERE_UNIT_SAMPLE2_3D is NOT a good code!'
    write ( *, '(a)' ) '  I only implemented it for comparison.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  A few sample values:'
    write ( *, '(a)' ) ' '

    seed = 123456789
    call random_seed ( seed )

    do i = 1, 5
        call sphere_unit_sample2_3d ( x )
        write ( *, '(3f8.4)' ) x(1:n)
    end do

    average(1:n) = 0.0E+00

    do i = 1, n_sample
        call sphere_unit_sample2_3d ( x )
        average(1:n) = average(1:n) + x(1:n)
    end do

    average(1:n) = average(1:n) / real ( n_sample )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now average the points, which should get a value'
    write ( *, '(a)' ) '  close to zero, and closer as N increases.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3f8.4)' ) '  Average:        ', average(1:n)

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now choose a random direction, sample the same'
    write ( *, '(a)' ) '  number of points, and compute the dot product with'
    write ( *, '(a)' ) '  the direction.'
    write ( *, '(a)' ) '  Take the absolute value of each dot product '
    write ( *, '(a)' ) '  and sum and average.'

    do j = 1, 5

        seed = 0
        call random_seed ( seed )

        call sphere_unit_sample2_3d ( v )

        seed = 123456789
        call random_seed ( seed )

        dot_average = 0.0E+00

        do i = 1, n_sample
            call sphere_unit_sample2_3d ( x )
            dot_average = dot_average + abs ( dot_product ( x(1:n), v(1:n) ) )
        end do

        dot_average = dot_average / real ( n_sample )

        write ( *, '(a)' ) ' '
        write ( *, '(a,3f8.4)' ) '  V:                ', v(1:n)
        write ( *, '(a, f8.4)' ) '  Average |(XdotV)| ', dot_average

    end do

    return
    end
    subroutine test193
    !
    !*******************************************************************************
    !
    !! TEST193 tests SPHERE_UNIT_SAMPLE_ND.
    !
    integer, parameter :: n = 3
    !
    real average(n)
    real dot_average
    integer i
    integer j
    integer, parameter :: n_sample = 1000
    integer seed
    real v(n)
    real x(n)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST193'
    write ( *, '(a)' ) '  For the unit sphere in N dimensions:'
    write ( *, '(a)' ) '  SPHERE_UNIT_SAMPLE_ND samples;'

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  A few sample values:'
    write ( *, '(a)' ) ' '

    seed = 123456789
    call random_seed ( seed )

    do i = 1, 5
        call sphere_unit_sample_nd ( n, x )
        write ( *, '(3f8.4)' ) x(1:n)
    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  Spatial dimension = ', n
    write ( *, '(a,i6)' ) '  Number of sample points = ', n_sample

    average(1:n) = 0.0E+00

    do i = 1, n_sample
        call sphere_unit_sample_nd ( n, x )
        average(1:n) = average(1:n) + x(1:n)
    end do

    average(1:n) = average(1:n) / real ( n_sample )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now average the points, which should get a value'
    write ( *, '(a)' ) '  close to zero, and closer as N increases.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3f8.4)' ) '  Average:        ', average(1:n)

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now choose a random direction, sample the same'
    write ( *, '(a)' ) '  number of points, and compute the dot product with'
    write ( *, '(a)' ) '  the direction.'
    write ( *, '(a)' ) '  Take the absolute value of each dot product '
    write ( *, '(a)' ) '  and sum and average.'

    do j = 1, 5

        seed = 0
        call random_seed ( seed )

        call sphere_unit_sample_nd ( n, v )

        seed = 123456789
        call random_seed ( seed )

        dot_average = 0.0E+00

        do i = 1, n_sample
            call sphere_unit_sample_nd ( n, x )
            dot_average = dot_average + abs ( dot_product ( x(1:n), v(1:n) ) )
        end do

        dot_average = dot_average / real ( n_sample )

        write ( *, '(a)' ) ' '
        write ( *, '(a,3f8.4)' ) '  V:                ', v(1:n)
        write ( *, '(a, f8.4)' ) '  Average |(XdotV)| ', dot_average

    end do

    return
    end
    subroutine test194
    !
    !*******************************************************************************
    !
    !! TEST194 tests SPHERE_UNIT_SAMPLE2_ND.
    !
    integer, parameter :: n = 3
    !
    real average(n)
    real dot_average
    integer i
    integer j
    integer, parameter :: n_sample = 1000
    integer seed
    real v(n)
    real x(n)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST194'
    write ( *, '(a)' ) '  For the unit sphere in N dimensions:'
    write ( *, '(a)' ) '  SPHERE_UNIT_SAMPLE2_ND samples;'

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  A few sample values:'
    write ( *, '(a)' ) ' '

    seed = 123456789
    call random_seed ( seed )

    do i = 1, 5
        call sphere_unit_sample2_nd ( n, x )
        write ( *, '(3f8.4)' ) x(1:n)
    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  Spatial dimension = ', n
    write ( *, '(a,i6)' ) '  Number of sample points = ', n_sample

    average(1:n) = 0.0E+00

    do i = 1, n_sample
        call sphere_unit_sample2_nd ( n, x )
        average(1:n) = average(1:n) + x(1:n)
    end do

    average(1:n) = average(1:n) / real ( n_sample )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now average the points, which should get a value'
    write ( *, '(a)' ) '  close to zero, and closer as N increases.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3f8.4)' ) '  Average:        ', average(1:n)

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now choose a random direction, sample the same'
    write ( *, '(a)' ) '  number of points, and compute the dot product with'
    write ( *, '(a)' ) '  the direction.'
    write ( *, '(a)' ) '  Take the absolute value of each dot product '
    write ( *, '(a)' ) '  and sum and average.'

    do j = 1, 5

        seed = 0
        call random_seed ( seed )

        call sphere_unit_sample2_nd ( n, v )

        seed = 123456789
        call random_seed ( seed )

        dot_average = 0.0E+00

        do i = 1, n_sample
            call sphere_unit_sample2_nd ( n, x )
            dot_average = dot_average + abs ( dot_product ( x(1:n), v(1:n) ) )
        end do

        dot_average = dot_average / real ( n_sample )

        write ( *, '(a)' ) ' '
        write ( *, '(a,3f8.4)' ) '  V:                ', v(1:n)
        write ( *, '(a, f8.4)' ) '  Average |(XdotV)| ', dot_average

    end do

    return
    end
    subroutine test195
    !
    !*******************************************************************************
    !
    !! TEST195 tests SPHERE_UNIT_SAMPLE3_ND.
    !
    integer, parameter :: n = 3
    !
    real average(n)
    real dot_average
    integer i
    integer j
    integer, parameter :: n_sample = 1000
    integer seed
    real v(n)
    real x(n)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST195'
    write ( *, '(a)' ) '  For the unit sphere in N dimensions:'
    write ( *, '(a)' ) '  SPHERE_UNIT_SAMPLE3_ND samples;'

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  A few sample values:'
    write ( *, '(a)' ) ' '

    seed = 123456789
    call random_seed ( seed )

    do i = 1, 5
        call sphere_unit_sample3_nd ( n, x )
        write ( *, '(3f8.4)' ) x(1:n)
    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  Spatial dimension = ', n
    write ( *, '(a,i6)' ) '  Number of sample points = ', n_sample

    average(1:n) = 0.0E+00

    do i = 1, n_sample
        call sphere_unit_sample3_nd ( n, x )
        average(1:n) = average(1:n) + x(1:n)
    end do

    average(1:n) = average(1:n) / real ( n_sample )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now average the points, which should get a value'
    write ( *, '(a)' ) '  close to zero, and closer as N increases.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,3f8.4)' ) '  Average:        ', average(1:n)

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Now choose a random direction, sample the same'
    write ( *, '(a)' ) '  number of points, and compute the dot product with'
    write ( *, '(a)' ) '  the direction.'
    write ( *, '(a)' ) '  Take the absolute value of each dot product '
    write ( *, '(a)' ) '  and sum and average.'

    do j = 1, 5

        seed = 0
        call random_seed ( seed )

        call sphere_unit_sample3_nd ( n, v )

        seed = 123456789
        call random_seed ( seed )

        dot_average = 0.0E+00

        do i = 1, n_sample
            call sphere_unit_sample3_nd ( n, x )
            dot_average = dot_average + abs ( dot_product ( x(1:n), v(1:n) ) )
        end do

        dot_average = dot_average / real ( n_sample )

        write ( *, '(a)' ) ' '
        write ( *, '(a,3f8.4)' ) '  V:                ', v(1:n)
        write ( *, '(a, f8.4)' ) '  Average |(XdotV)| ', dot_average

    end do

    return
    end
    subroutine test196
    !
    !*******************************************************************************
    !
    !! TEST196 tests SHAPE_POINT_DIST_2D.
    !
    implicit none
    !
    integer, parameter :: nside = 4
    integer, parameter :: ntest = 9
    !
    real dist
    integer i
    real x
    real x1
    real xc
    real xtest(ntest)
    real y
    real y1
    real yc
    real ytest(ntest)
    !
    !  Define the square.
    !
    xc = 3.0E+00
    yc = 0.0E+00

    x1 = 5.0E+00
    y1 = 0.0E+00
    !
    !  Coordinates of the test points.
    !
    xtest(1) = 3.0E+00
    ytest(1) = 0.0E+00

    xtest(2) = 5.0E+00
    ytest(2) = 0.0E+00

    xtest(3) = 4.0E+00
    ytest(3) = 0.0E+00

    xtest(4) = 10.0E+00
    ytest(4) = 0.0E+00

    xtest(5) = 8.0E+00
    ytest(5) = 5.0E+00

    xtest(6) = 6.0E+00
    ytest(6) = 6.0E+00

    xtest(7) = 1.0E+00
    ytest(7) = 2.0E+00

    xtest(8) =   2.5E+00
    ytest(8) = - 0.5E+00

    xtest(9) =   4.0E+00
    ytest(9) = - 1.0E+00
    !
    write ( *, '(a)' ) ' '
    write ( *,'(a)' ) 'TEST196'
    write ( *, '(a)' ) '  For a shape in 2D,'
    write ( *, '(a)' ) '  SHAPE_POINT_DIST_2D computes the distance'
    write ( *, '(a)' ) '    to a point;'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Number of sides:'
    write ( *, '(a)' ) ' '
    write ( *, '(i6)' ) nside
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Square center:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) xc, yc
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Square vertex #1:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) x1, y1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     I       X            Y            DIST'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x = xtest(i)
        y = ytest(i)

        call shape_point_dist_2d ( xc, yc, x1, y1, nside, x, y, dist )

        write ( *, '(i6,3g14.6)' ) i, x, y, dist

    end do

    return
    end
    subroutine test197
    !
    !*******************************************************************************
    !
    !! TEST197 tests SHAPE_POINT_DIST_2D.
    !
    implicit none
    !
    integer, parameter :: nside = 6
    integer, parameter :: ntest = 8
    !
    real dist
    integer i
    real x
    real x1
    real xc
    real xtest(ntest)
    real y
    real y1
    real yc
    real ytest(ntest)
    !
    !  Define the hexagon.
    !
    xc = 3.0E+00
    yc = 0.0E+00

    x1 = 5.0E+00
    y1 = 0.0E+00
    !
    !  Coordinates of the test points.
    !
    xtest(1) = 3.0E+00
    ytest(1) = 0.0E+00

    xtest(2) = 5.0E+00
    ytest(2) = 0.0E+00

    xtest(3) = 4.0E+00
    ytest(3) = 0.0E+00

    xtest(4) = 10.0E+00
    ytest(4) = 0.0E+00

    xtest(5) = 4.0E+00
    ytest(5) = sqrt ( 3.0E+00 )

    xtest(6) = 5.0E+00
    ytest(6) = 2.0E+00 * sqrt ( 3.0E+00 )

    xtest(7) = 3.0E+00
    ytest(7) = sqrt ( 3.0E+00 )

    xtest(8) = 3.0E+00
    ytest(8) = sqrt ( 3.0E+00 ) / 2.0E+00
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST197'
    write ( *, '(a)' ) '  For a shape in 2D,'
    write ( *, '(a)' ) '  SHAPE_POINT_DIST_2D computes the distance'
    write ( *, '(a)' ) '    to a point;'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Number of sides:'
    write ( *, '(a)' ) ' '
    write ( *, '(i6)' ) nside
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Hexagon center:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) xc, yc
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Hexagon vertex #1:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) x1, y1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     I	   X		Y	     DIST'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x = xtest(i)
        y = ytest(i)

        call shape_point_dist_2d ( xc, yc, x1, y1, nside, x, y, dist )

        write ( *, '(i6,3g14.6)' ) i, x, y, dist

    end do

    return
    end
    subroutine test198
    !
    !*******************************************************************************
    !
    !! TEST198 tests SHAPE_POINT_NEAR_2D.
    !
    implicit none
    !
    integer, parameter :: nside = 6
    integer, parameter :: ntest = 8
    !
    real dist
    integer i
    real x
    real x1
    real xc
    real xn
    real xtest(ntest)
    real y
    real y1
    real yc
    real yn
    real ytest(ntest)
    !
    !  Define the hexagon.
    !
    xc = 3.0E+00
    yc = 0.0E+00

    x1 = 5.0E+00
    y1 = 0.0E+00
    !
    !  Coordinates of the test points.
    !
    xtest(1) = 3.0E+00
    ytest(1) = 0.0E+00

    xtest(2) = 5.0E+00
    ytest(2) = 0.0E+00

    xtest(3) = 4.0E+00
    ytest(3) = 0.0E+00

    xtest(4) = 10.0E+00
    ytest(4) = 0.0E+00

    xtest(5) = 4.0E+00
    ytest(5) = sqrt ( 3.0E+00 )

    xtest(6) = 5.0E+00
    ytest(6) = 2.0E+00 * sqrt ( 3.0E+00 )

    xtest(7) = 3.0E+00
    ytest(7) = sqrt ( 3.0E+00 )

    xtest(8) = 3.0E+00
    ytest(8) = sqrt ( 3.0E+00 ) / 2.0E+00
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST198'
    write ( *, '(a)' ) '  For a shape in 2D,'
    write ( *, '(a)' ) '  SHAPE_POINT_NEAR_2D computes the nearest'
    write ( *, '(a)' ) '    point to a point;'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Number of sides:'
    write ( *, '(a)' ) ' '
    write ( *, '(i6)' ) nside
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Hexagon center:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) xc, yc
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Hexagon vertex #1:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) x1, y1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     I       X            Y            ' // &
        'Xnear Ynear Dist'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x = xtest(i)
        y = ytest(i)

        call shape_point_near_2d ( xc, yc, x1, y1, nside, x, y, xn, yn, dist )

        write ( *, '(i6,5f12.4)' ) i, x, y, xn, yn, dist

    end do

    return
    end
    subroutine test199
    !
    !*******************************************************************************
    !
    !! TEST199 tests SHAPE_RAY_INT_2D.
    !
    implicit none
    !
    integer, parameter :: nside = 6
    integer, parameter :: ntest = 4
    !
    integer i
    real x1
    real xa
    real xa_test(ntest)
    real xb
    real xb_test(ntest)
    real xc
    real xi
    real y1
    real ya
    real ya_test(ntest)
    real yb
    real yb_test(ntest)
    real yc
    real yi
    !
    !  Define the hexagon.
    !
    xc = 3.0E+00
    yc = 0.0E+00

    x1 = 5.0E+00
    y1 = 0.0E+00
    !
    !  Coordinates of the test points.
    !
    xa_test(1) = 3.0E+00
    ya_test(1) = 0.0E+00

    xb_test(1) = 4.0E+00
    yb_test(1) = 0.0E+00

    xa_test(2) = 3.0E+00
    ya_test(2) = 0.0E+00

    xb_test(2) = 3.0E+00
    yb_test(2) = 1.0E+00

    xa_test(3) = 3.0E+00
    ya_test(3) = -1.0E+00

    xb_test(3) = 3.0E+00
    yb_test(3) = 1.0E+00

    xa_test(4) = 3.0E+00
    ya_test(4) = -1.0E+00

    xb_test(4) = 7.0E+00
    yb_test(4) = 5.0E+00
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST199'
    write ( *, '(a)' ) '  For a shape in 2D,'
    write ( *, '(a)' ) '  SHAPE_RAY_INT_2D computes the intersection of'
    write ( *, '(a)' ) '    a shape and a ray whose origin is within'
    write ( *, '(a)' ) '    the shape.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Number of sides:'
    write ( *, '(a)' ) ' '
    write ( *, '(i6)' ) nside
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Hexagon center:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) xc, yc
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Hexagon vertex #1:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) x1, y1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     I       XA          YA          XB' // &
        '          YB          XI          YI'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        xa = xa_test(i)
        ya = ya_test(i)

        xb = xb_test(i)
        yb = yb_test(i)

        call shape_ray_int_2d ( xc, yc, x1, y1, nside, xa, ya, xb, yb, xi, yi )

        write ( *, '(i6,6f12.4)' ) i, xa, ya, xb, yb, xi, yi

    end do

    return
    end
    subroutine test200
    !
    !*******************************************************************************
    !
    !! TEST200 tests STRI_SIDES_TO_ANGLES_3D.
    !
    implicit none
    !
    real a
    real as
    real b
    real bs
    real c
    real cs
    real, parameter :: r = 10.0E+00
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST200'
    write ( *, '(a)' ) '  STRI_SIDES_TO_ANGLES_3D takes the sides of a'
    write ( *, '(a)' ) '  spherical triangle and determines the angles.'

    as = 121.0E+00 + ( 15.4E+00 / 60.0E+00 )
    bs = 104.0E+00 + ( 54.7E+00 / 60.0E+00 )
    cs =  65.0E+00 + ( 42.5E+00 / 60.0E+00 )

    as = degrees_to_radians ( as )
    bs = degrees_to_radians ( bs )
    cs = degrees_to_radians ( cs )

    as = r * as
    bs = r * bs
    cs = r * cs
    !
    !  Get the spherical angles.
    !
    call stri_sides_to_angles_3d ( r, as, bs, cs, a, b, c )

    write ( *, '(a)' ) ' '
    write ( *, '(a,f8.4,a)' ) '  A       = ', a,  ' (radians)'
    a = radians_to_degrees ( a )
    write ( *, '(a,f8.4,a)' ) '          = ', a,  ' ( degrees )'
    a = 117.0E+00 + ( 58.0E+00 / 60.0E+00 )
    write ( *, '(a,f8.4,a)' ) '  Correct = ', a, ' (degrees)'

    write ( *, '(a)' ) ' '
    write ( *, '(a,f8.4,a)' ) '  B       = ', b,  ' (radians)'
    b = radians_to_degrees ( b )
    write ( *, '(a,f8.4,a)' ) '          = ', b,  ' ( degrees )'
    b = 93.0E+00 + ( 13.8E+00 / 60.0E+00 )
    write ( *, '(a,f8.4,a)' ) '  Correct = ', b, ' (degrees)'

    write ( *, '(a)' ) ' '
    write ( *, '(a,f8.4,a)' ) '  C       = ', c,  ' (radians)'
    c = radians_to_degrees ( c )
    write ( *, '(a,f8.4,a)' ) '          = ', c,  ' ( degrees )'
    c = 70.0E+00 + ( 20.6E+00 / 60.0E+00 )
    write ( *, '(a,f8.4,a)' ) '  Correct = ', c, ' (degrees)'

    return
    end
    subroutine test201
    !
    !*******************************************************************************
    !
    !! TEST201 tests STRING_2D.
    !
    implicit none
    !
    integer, parameter :: nvec = 15
    !
    integer i
    integer iorder(nvec)
    integer istrng(nvec)
    integer jstrng
    integer nstrng
    real x1vec(nvec)
    real x2vec(nvec)
    real y1vec(nvec)
    real y2vec(nvec)
    !
    i = 0

    i = i+1
    x1vec(i) = 0.0E+00
    y1vec(i) = 0.0E+00
    x2vec(i) = 1.0E+00
    y2vec(i) = 1.0E+00

    i = i+1
    x1vec(i) = 3.0E+00
    y1vec(i) = 4.0E+00
    x2vec(i) = 2.0E+00
    y2vec(i) = 4.0E+00

    i = i+1
    x1vec(i) = 2.0E+00
    y1vec(i) = 2.0E+00
    x2vec(i) = 1.0E+00
    y2vec(i) = 3.0E+00

    i = i+1
    x1vec(i) = 3.0E+00
    y1vec(i) = 2.0E+00
    x2vec(i) = 2.0E+00
    y2vec(i) = 3.0E+00

    i = i+1
    x1vec(i) = 2.0E+00
    y1vec(i) = 1.0E+00
    x2vec(i) = 2.0E+00
    y2vec(i) = 2.0E+00

    i = i+1
    x1vec(i) = 1.0E+00
    y1vec(i) = 1.0E+00
    x2vec(i) = 1.0E+00
    y2vec(i) = 2.0E+00

    i = i+1
    x1vec(i) = 0.0E+00
    y1vec(i) = 5.0E+00
    x2vec(i) = 1.0E+00
    y2vec(i) = 6.0E+00

    i = i+1
    x1vec(i) = 1.0E+00
    y1vec(i) = 2.0E+00
    x2vec(i) = 1.0E+00
    y2vec(i) = 3.0E+00

    i = i+1
    x1vec(i) = 3.0E+00
    y1vec(i) = 2.0E+00
    x2vec(i) = 3.0E+00
    y2vec(i) = 3.0E+00

    i = i+1
    x1vec(i) = 0.0E+00
    y1vec(i) = 0.0E+00
    x2vec(i) = 1.0E+00
    y2vec(i) = 0.0E+00

    i = i+1
    x1vec(i) = 5.0E+00
    y1vec(i) = 5.0E+00
    x2vec(i) = 6.0E+00
    y2vec(i) = 6.0E+00

    i = i+1
    x1vec(i) = 3.0E+00
    y1vec(i) = 3.0E+00
    x2vec(i) = 3.0E+00
    y2vec(i) = 4.0E+00

    i = i+1
    x1vec(i) = 2.0E+00
    y1vec(i) = 4.0E+00
    x2vec(i) = 2.0E+00
    y2vec(i) = 3.0E+00

    i = i+1
    x1vec(i) = 7.0E+00
    y1vec(i) = 4.0E+00
    x2vec(i) = 5.0E+00
    y2vec(i) = 5.0E+00

    i = i+1
    x1vec(i) = 1.0E+00
    y1vec(i) = 0.0E+00
    x2vec(i) = 2.0E+00
    y2vec(i) = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST201'
    write ( *, '(a)' ) '  STRING_2D takes a set of line segments, and'
    write ( *, '(a)' ) '    "strings" them together.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  I, (X1,Y1), (X2,Y2)'
    write ( *, '(a)' ) ' '
    do i = 1, nvec
        write ( *, '(i6,4g14.6)' ) i, x1vec(i), y1vec(i), x2vec(i), y2vec(i)
    end do

    call string_2d ( iorder, istrng, nstrng, nvec, x1vec, x2vec, y1vec, y2vec )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i6,a)' ) '  Found ', nstrng, ' groups of segments.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  ISTRNG, IORDER, (X1,Y1), (X2,Y2)'
    write ( *, '(a)' ) ' '

    jstrng = 1

    do i = 1, nvec

        if ( istrng(i) > jstrng ) then
            write ( *, '(a)' ) ' '
            jstrng = jstrng + 1
        end if

        write ( *, '(1x,i3,1x,i3,4f10.4)' ) istrng(i), iorder(i), x1vec(i), &
            y1vec(i), x2vec(i), y2vec(i)

    end do

    return
    end
    subroutine test202
    !
    !*******************************************************************************
    !
    !! TEST202 tests SUPER_ELLIPSE_POINTS_2D;
    !
    implicit none
    !
    integer, parameter :: n = 24
    !
    real area
    real expo
    real psi
    real r1
    real r2
    real x(n)
    real x0
    real y(n)
    real y0
    !
    x0 =  5.0E+00
    y0 = -2.0E+00
    r1 = 3.0E+00
    r2 = 1.0E+00
    expo = 1.5E+00
    psi = r_pi ( ) / 6.0E+00

    call ellipse_area_2d ( r1, r2, area )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST202'
    write ( *, '(a)' ) &
        '  SUPER_ELLIPSE_POINTS_2D returns points on a super ellipse;'
    write ( *, '(a)' ) ' '
    write ( *, '(a,2g14.6)' ) '  The super ellipse will have center ', x0, y0
    write ( *, '(a,g14.6,a,g14.6)' ) '  radii R1 = ', r1, ' R2 = ', r2
    write ( *, '(a,g14.6)' ) '  exponent EXPO = ', expo
    write ( *, '(a,g14.6)' ) '  and angle PSI = ', psi
    write ( *, '(a,g14.6)' ) '  and area = ', area

    call super_ellipse_points_2d ( x0, y0, r1, r2, expo, psi, n, x, y )

    call rvec2_print ( n, x, y, '  Sample points:' )

    return
    end
    subroutine test203
    !
    !*******************************************************************************
    !
    !! TEST203 tests TETRA_CENTROID_3D;
    !! TEST203 tests TETRA_CONTAINS_POINT_3D;
    !! TEST203 tests TETRA_CIRCUMSPHERE_3D;
    !! TEST203 tests TETRA_VOLUME_3D;
    !
    implicit none
    !
    real a
    real b
    real c
    logical inside
    real r
    real volume
    real x
    real x1
    real x2
    real x3
    real x4
    real xc
    real y
    real y1
    real y2
    real y3
    real y4
    real yc
    real z
    real z1
    real z2
    real z3
    real z4
    real zc
    !
    !  Coordinates of the tetrahedron.
    !
    x1 =   0.0E+00
    y1 =   2.0E+00 * sqrt ( 2.0E+00 ) / 3.0E+00
    z1 = - 1.0E+00 / 3.0E+00

    x2 = - sqrt ( 2.0E+00 / 3.0E+00 )
    y2 = - sqrt ( 2.0E+00 ) / 3.0E+00
    z2 = - 1.0E+00 / 3.0E+00

    x3 =   sqrt ( 2.0E+00 / 3.0E+00 )
    y3 = - sqrt ( 2.0E+00 ) / 3.0E+00
    z3 = - 1.0E+00 / 3.0E+00

    x4 =   0.0E+00
    y4 =   0.0E+00
    z4 =   1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST203'
    write ( *, '(a)' ) '  For a tetrahedron in 3D,'
    write ( *, '(a)' ) '  TETRA_CENTROID_3D computes the centroid;'
    write ( *, '(a)' ) '  TETRA_CONTAINS_POINT_3D finds if a point is inside;'
    write ( *, '(a)' ) '  TETRA_CIRCUMSPHERE_3D computes the circumsphere;'
    write ( *, '(a)' ) '  TETRA_VOLUME_3D computes the volume;'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Corners:'
    write ( *, '(a)' ) ' '
    write ( *, '(3g14.6)' ) x1, y1, z1
    write ( *, '(3g14.6)' ) x2, y2, z2
    write ( *, '(3g14.6)' ) x3, y3, z3
    write ( *, '(3g14.6)' ) x4, y4, z4

    call tetra_volume_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, &
        z4, volume )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Volume:'
    write ( *, '(g14.6)' ) volume

    call tetra_centroid_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, &
        x, y, z )

    write ( *, '(a)' ) '  Centroid:'
    write ( *, '(3g14.6)' ) x, y, z

    call tetra_circumsphere_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, &
        r, xc, yc, zc )

    write ( *, '(a)' ) '  Circumcircle center:'
    write ( *, '(3g14.6)' ) xc, yc, zc

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Circumcircle radius is ', r

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'X, Y, Z, Inside_Tetra?'
    write ( *, '(a)' ) ' '

    a = 0.1E+00
    b = 0.2E+00
    c = 1.0E+00 - a - b

    x = x1 + a * ( x2 - x1 ) + b * ( x3 - x1 ) + c * ( x4 - x1 )
    y = y1 + a * ( y2 - y1 ) + b * ( y3 - y1 ) + c * ( y4 - y1 )
    z = z1 + a * ( z2 - z1 ) + b * ( z3 - z1 ) + c * ( z4 - z1 )

    call tetra_contains_point_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, &
        x4, y4, z4, x, y, z, inside )

    write ( *, '(3g14.6,2x,l1)' ) x, y, z, inside

    a = 2.0E+00
    b = 0.2E+00
    c = 0.1E+00

    x = x1 + a * ( x2 - x1 ) + b * ( x3 - x1 ) + c * ( x4 - x1 )
    y = y1 + a * ( y2 - y1 ) + b * ( y3 - y1 ) + c * ( y4 - y1 )
    z = z1 + a * ( z2 - z1 ) + b * ( z3 - z1 ) + c * ( z4 - z1 )

    call tetra_contains_point_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, &
        x4, y4, z4, x, y, z, inside )

    write ( *, '(3g14.6,2x,l1)' ) x, y, z, inside

    a =   0.6E+00
    b = - 0.5E+00
    c =   0.1E+00

    x = x1 + a * ( x2 - x1 ) + b * ( x3 - x1 ) + c * ( x4 - x1 )
    y = y1 + a * ( y2 - y1 ) + b * ( y3 - y1 ) + c * ( y4 - y1 )
    z = z1 + a * ( z2 - z1 ) + b * ( z3 - z1 ) + c * ( z4 - z1 )

    call tetra_contains_point_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, &
        x4, y4, z4, x, y, z, inside )

    write ( *, '(3g14.6,2x,l1)' ) x, y, z, inside

    return
    end
    subroutine test204
    !
    !*******************************************************************************
    !
    !! TEST204 tests TMAT_INIT;
    !! TEST204 tests TMAT_ROT_AXIS;
    !! TEST204 tests TMAT_ROT_VECTOR;
    !! TEST204 tests TMAT_SCALE;
    !! TEST204 tests TMAT_SHEAR;
    !! TEST204 tests TMAT_TRANS.
    !
    implicit none
    !
    real a(4,4)
    real angle
    real axis(3)
    character axis1
    character ( len = 2 ) axis2
    real b(4,4)
    integer i
    integer j
    real s
    real x
    real y
    real z
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST204'
    write ( *, '(a)' ) '  TMAT geometric transformation matrix routines:'
    write ( *, '(a)' ) '  TMAT_INIT initializes,'
    write ( *, '(a)' ) '  TMAT_ROT_AXIS for rotation about an axis,'
    write ( *, '(a)' ) '  TMAT_ROT_VECTOR for rotation about a vector,'
    write ( *, '(a)' ) '  TMAT_SCALE for scaling,'
    write ( *, '(a)' ) '  TMAT_SHEAR for shear,'
    write ( *, '(a)' ) '  TMAT_TRANS for translation'
    !
    !  Initialization.
    !
    call tmat_init ( a )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Initial transformation matrix:'
    write ( *, '(a)' ) ' '
    do i = 1, 4
        write ( *, '(4g14.6)' ) a(i,1:4)
    end do
    !
    !  Rotation about an axis.
    !
    angle = 30.0E+00
    axis1 = 'x'
    call tmat_rot_axis ( a, b, angle, axis1 )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Transformation matrix for'
    write ( *, '(a)' ) '  rotation about ' // axis1
    write ( *, '(a,g14.6)' ) '  by ' , angle
    write ( *, '(a)' ) ' '
    do i = 1, 4
        write ( *, '(4g14.6)' ) b(i,1:4)
    end do
    !
    !  Rotation about a vector.
    !
    angle = 30.0E+00
    axis(1) = 1.0E+00
    axis(2) = 2.0E+00
    axis(3) = 3.0E+00
    call tmat_rot_vector ( a, b, angle, axis )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Transformation matrix for'
    write ( *, '(a,3g14.6)' ) '  rotation about ', axis(1), axis(2), axis(3)
    write ( *, '(a,g14.6)' ) '  of ', angle
    write ( *, '(a)' ) ' '
    do i = 1, 4
        write ( *, '(4g14.6)' ) b(i,1:4)
    end do
    !
    !  Scaling.
    !
    x = 2.0E+00
    y = 0.5E+00
    z = 10.0E+00
    call tmat_scale ( a, b, x, y, z )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Transformation matrix for'
    write ( *, '(a,3g14.6)' ) '  scaling by ', x, y, z
    write ( *, '(a)' ) ' '
    do i = 1, 4
        write ( *, '(4g14.6)' ) b(i,1:4)
    end do
    !
    !  Shear.
    !
    axis2 = 'xy'
    s = 0.5E+00
    call tmat_shear ( a, b, axis2, s )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Transformation matrix for'
    write ( *, '(2x,a)' ) axis2
    write ( *, '(a,g14.6)' ) '  shear coefficient of ', s
    write ( *, '(a)' ) ' '
    do i = 1, 4
        write ( *, '(4g14.6)' ) b(i,1:4)
    end do
    !
    !  Translation.
    !
    x = 1.0E+00
    y = 2.0E+00
    z = 3.0E+00
    call tmat_trans ( a, b, x, y, z )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Transformation matrix for'
    write ( *, '(a,3g14.6)' ) '  translation by ', x, y, z
    write ( *, '(a)' ) ' '
    do i = 1, 4
        write ( *, '(4g14.6)' ) b(i,1:4)
    end do

    return
    end
    subroutine test205
    !
    !*******************************************************************************
    !
    !! TEST205 tests TMAT_MXP2.
    !
    implicit none
    !
    integer, parameter :: n = 4
    !
    real a(4,4)
    real angle
    real axis(3)
    character axis1
    character ( len = 2 ) axis2
    real b(4,4)
    integer i
    integer j
    real point(3,n)
    real point2(3,n)
    real s
    real x
    real y
    real z
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST205'
    write ( *, '(a)' ) '  TMAT_MXP2 applies a geometric transformation'
    write ( *, '(a)' ) '    matrix to a set of points.'
    !
    !  Initialization.
    !
    point(1,1) = 1.0E+00
    point(2,1) = 0.0E+00
    point(3,1) = 0.0E+00

    point(1,2) = 0.0E+00
    point(2,2) = 1.0E+00
    point(3,2) = 0.0E+00

    point(1,3) = 0.0E+00
    point(2,3) = 0.0E+00
    point(3,3) = 1.0E+00

    point(1,4) = 1.0E+00
    point(2,4) = 1.0E+00
    point(3,4) = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Points as column vectors:'
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(4g14.6)' ) point(i,1:n)
    end do
    !
    !  Initialization of transformation matrix.
    !
    call tmat_init ( a )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Initial transformation matrix:'
    write ( *, '(a)' ) ' '
    do i = 1, 4
        write ( *, '(4g14.6)' ) a(i,1:4)
    end do
    !
    !  Rotation about an axis.
    !
    angle = 30.0E+00
    axis1 = 'x'
    call tmat_rot_axis ( a, b, angle, axis1 )

    call tmat_mxp2 ( b, point, point2, n )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Rotation about ' // axis1
    write ( *, '(a,g14.6)' ) '  by ' , angle
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(4g14.6)' ) point2(i,1:n)
    end do
    !
    !  Rotation about a vector.
    !
    angle = 30.0E+00
    axis(1) = 1.0E+00
    axis(2) = 2.0E+00
    axis(3) = 3.0E+00
    call tmat_rot_vector ( a, b, angle, axis )

    call tmat_mxp2 ( b, point, point2, n )

    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  Rotation about ', x, y, z
    write ( *, '(a,g14.6)' ) '  of ', angle
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(4g14.6)' ) point2(i,1:n)
    end do
    !
    !  Scaling.
    !
    x = 2.0E+00
    y = 0.5E+00
    z = 10.0E+00
    call tmat_scale ( a, b, x, y, z )

    call tmat_mxp2 ( b, point, point2, n )

    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  Scaling by ', x, y, z
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(4g14.6)' ) point2(i,1:n)
    end do
    !
    !  Shear.
    !
    axis2 = 'xy'
    s = 0.5E+00
    call tmat_shear ( a, b, axis2, s )

    call tmat_mxp2 ( b, point, point2, n )

    write ( *, '(a)' ) ' '
    write ( *, '(2x,a)' ) axis2
    write ( *, '(a,g14.6)' ) ' shear coefficient of ', s
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(4g14.6)' ) point2(i,1:n)
    end do
    !
    !  Translation.
    !
    x = 1.0E+00
    y = 2.0E+00
    z = 3.0E+00
    call tmat_trans ( a, b, x, y, z )

    call tmat_mxp2 ( b, point, point2, n )

    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  Translation by ', x, y, z
    write ( *, '(a)' ) ' '
    do i = 1, 3
        write ( *, '(4g14.6)' ) point2(i,1:n)
    end do
    return
    end
    subroutine test206
    !
    !*******************************************************************************
    !
    !! TEST206 tests TRIANGLE_ANGLES_2D;
    !! TEST206 tests TRIANGLE_AREA_2D;
    !! TEST206 tests TRIANGLE_AREA_SIGNED_2D;
    !! TEST206 tests TRIANGLE_CENTROID_2D;
    !! TEST206 tests TRIANGLE_INCIRCLE_2D;
    !! TEST206 tests TRIANGLE_CIRCUMCIRCLE_2D;
    !
    implicit none
    !
    real a1
    real a2
    real a3
    real area
    real area_signed
    integer i
    real r
    real x
    real x1
    real x2
    real x3
    real xc
    real xn
    real y
    real y1
    real y2
    real y3
    real yc
    real yn
    !
    !  Coordinates of the corners of the triangle.
    !
    x1 = 0.0E+00
    y1 = 1.0E+00

    x2 = 0.0E+00
    y2 = 0.0E+00

    x3 = 1.0E+00
    y3 = 0.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST206'
    write ( *, '(a)' ) '  For a triangle in 2D,'
    write ( *, '(a)' ) '  TRIANGLE_ANGLES_2D computes the angles;'
    write ( *, '(a)' ) '  TRIANGLE_AREA_2D computes the area;'
    write ( *, '(a)' ) '  TRIANGLE_AREA_SIGNED_2D computes signed area.'
    write ( *, '(a)' ) '  TRIANGLE_CENTROID_2D computes the centroid.'
    write ( *, '(a)' ) '  TRIANGLE_CIRCUMCIRCLE_2D computes the circumcircle.'
    write ( *, '(a)' ) '  TRIANGLE_INCENTER_2D computes the incircle.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The corners of the triangle are:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) x1, y1
    write ( *, '(2g14.6)' ) x2, y2
    write ( *, '(2g14.6)' ) x3, y3
    write ( *, '(a)' ) ' '

    call triangle_angles_2d ( x1, y1, x2, y2, x3, y3, a1, a2, a3 )

    write ( *, '(a)' ) ' '
    write ( *, '(a,3g14.6)' ) '  Angles in radians: ', a1, a2, a3
    write ( *, '(a,3g14.6)' ) '  Angles in degrees: ', &
        radians_to_degrees ( a1 ), &
        radians_to_degrees ( a2 ), radians_to_degrees ( a3 )

    call triangle_area_2d ( x1, y1, x2, y2, x3, y3, area )

    write ( *, '(a,g14.6)' ) '  Area is ', area

    call triangle_area_signed_2d ( x1, y1, x2, y2, x3, y3, area_signed )

    write ( *, '(a,g14.6)' ) '  Signed area is ', area_signed

    call triangle_centroid_2d ( x1, y1, x2, y2, x3, y3, x, y )

    write ( *, '(a,2g14.6)' ) '  Centroid is ', x, y

    call triangle_incircle_2d ( x1, y1, x2, y2, x3, y3, r, xc, yc )

    write ( *, '(a,2g14.6)' ) '  Incircle center is ', xc, yc
    write ( *, '(a,g14.6)' ) '  Incircle radius is ', r

    call triangle_circumcircle_2d ( x1, y1, x2, y2, x3, y3, r, xc, yc )

    write ( *, '(a,2g14.6)' ) '  Circumcircle center is ', xc, yc
    write ( *, '(a,g14.6)' ) '  Circumcircle radius is ', r

    return
    end
    subroutine test207
    !
    !*******************************************************************************
    !
    !! TEST207 tests TRIANGLE_CONTAINS_POINT_1_2D;
    !! TEST207 tests TRIANGLE_CONTAINS_POINT_2_2D;
    !! TEST207 tests TRIANGLE_POINT_DIST_2D;
    !! TEST207 tests TRIANGLE_POINT_DIST_SIGNED_2D;
    !! TEST207 tests TRIANGLE_POINT_NEAR_2D;
    !
    implicit none
    !
    integer, parameter :: ntest = 7
    !
    real a1
    real a2
    real a3
    real dist
    real dist2
    real dist_signed
    integer i
    logical inside1
    logical inside2
    real x
    real x1
    real x2
    real x3
    real xc
    real xn
    real xtest(ntest)
    real y
    real y1
    real y2
    real y3
    real yc
    real yn
    real ytest(ntest)
    !
    !  Coordinates of the corners of the triangle.
    !
    x1 = 0.0E+00
    y1 = 1.0E+00

    x2 = 0.0E+00
    y2 = 0.0E+00

    x3 = 1.0E+00
    y3 = 0.0E+00
    !
    !  Coordinates of the test points.
    !
    xtest(1) = 0.25E+00
    ytest(1) = 0.25E+00

    xtest(2) = 0.75E+00
    ytest(2) = 0.25E+00

    xtest(3) = 1.0E+00
    ytest(3) = 1.0E+00

    xtest(4) = 11.0E+00
    ytest(4) = 0.5E+00

    xtest(5) = 0.0E+00
    ytest(5) = 1.0E+00

    xtest(6) = 0.5E+00
    ytest(6) = -10.0E+00

    xtest(7) = 0.6E+00
    ytest(7) = 0.6E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST207'
    write ( *, '(a)' ) '  For a triangle in 2D,'
    write ( *, '(a)' ) '  TRIANGLE_CONTAINS_POINT_1_2D reports if a point '
    write ( *, '(a)' ) '    is inside a triangle;'
    write ( *, '(a)' ) '  TRIANGLE_CONTAINS_POINT_2_2D reports if a point '
    write ( *, '(a)' ) '    is inside a triangle;'
    write ( *, '(a)' ) '  TRIANGLE_POINT_DIST_2D computes the distance'
    write ( *, '(a)' ) '    to a point;'
    write ( *, '(a)' ) '  TRIANGLE_POINT_DIST_SIGNED_2D computes signed'
    write ( *, '(a)' ) '    distance to a point;'
    write ( *, '(a)' ) '  TRIANGLE_POINT_NEAR_2D computes the nearest'
    write ( *, '(a)' ) '    point to a point.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The corners of the triangle are:'
    write ( *, '(a)' ) ' '
    write ( *, '(2f8.3)' ) x1, y1
    write ( *, '(2f8.3)' ) x2, y2
    write ( *, '(2f8.3)' ) x3, y3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '   X       Y     In1  In2   DIST_S    DIST      XN     YN'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x = xtest(i)
        y = ytest(i)

        call triangle_contains_point_1_2d ( x1, y1, x2, y2, x3, y3, x, y, inside1 )
        call triangle_contains_point_2_2d ( x1, y1, x2, y2, x3, y3, x, y, inside2 )

        call triangle_point_dist_signed_2d ( x1, y1, x2, y2, x3, y3, x, y, &
            dist_signed )

        call triangle_point_dist_2d ( x1, y1, x2, y2, x3, y3, x, y, dist )

        call triangle_point_near_2d ( x1, y1, x2, y2, x3, y3, x, y, xn, yn, dist2 )

        write ( *, '(2f8.3,2x,l2,2x,l2,2x,2f8.3,2x,2f8.3)' ) x, y, inside1, &
            inside2, dist_signed, dist, xn, yn

    end do

    return
    end
    subroutine test208
    !
    !*******************************************************************************
    !
    !! TEST208 tests TRIANGLE_GRIDPOINTS_2D;
    !
    implicit none
    !
    integer, parameter :: maxgrid = 50
    !
    integer i
    integer ngrid
    integer nsub
    real x(maxgrid)
    real, parameter :: x1 = 0.0E+00
    real, parameter :: x2 = 0.0E+00
    real, parameter :: x3 = 1.0E+00
    real y(maxgrid)
    real, parameter :: y1 = 1.0E+00
    real, parameter :: y2 = 0.0E+00
    real, parameter :: y3 = 0.0E+00
    !
    nsub = 3

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST208'
    write ( *, '(a)' ) '  For a triangle in 2D,'
    write ( *, '(a)' ) '  TRIANGLE_GRIDPOINTS_2D produces a set of'
    write ( *, '(a)' ) '  gridpoints in or on the triangle.'
    write ( *, '(a)' ) ' '
    call rvec_print_2d ( x1, y1, '  Vertex A' )
    call rvec_print_2d ( x2, y2, '  Vertex B' )
    call rvec_print_2d ( x3, y3, '  Vertex C' )

    call triangle_gridpoints_2d ( x1, y1, x2, y2, x3, y3, nsub, maxgrid, ngrid, &
        x, y )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  Number of grid points is ', ngrid
    write ( *, '(a)' ) ' '

    do i = 1, ngrid
        call rvec_print_2d ( x(i), y(i), ' ' )
    end do

    return
    end
    subroutine test209
    !
    !*******************************************************************************
    !
    !! TEST209 tests TRIANGLE_ANGLES_3D;
    !! TEST209 tests TRIANGLE_AREA_3D;
    !! TEST209 tests TRIANGLE_AREA_2_3D;
    !! TEST209 tests TRIANGLE_CENTROID_3D;
    !! TEST209 tests TRIANGLE_POINT_DIST_2D;
    !
    implicit none
    !
    integer, parameter :: ntest = 3
    !
    real a1
    real a2
    real a3
    real area
    real dist
    integer i
    real x
    real x1
    real x2
    real x3
    real xtest(ntest)
    real y
    real y1
    real y2
    real y3
    real ytest(ntest)
    real z
    real z1
    real z2
    real z3
    real ztest(ntest)
    !
    !  Coordinates of the triangle vertices.
    !
    x1 = 1.0E+00
    y1 = 2.0E+00
    z1 = 3.0E+00

    x2 = 1.0E+00 + sqrt ( 2.0E+00 )
    y2 = 2.0E+00 + sqrt ( 2.0E+00 )
    z2 = 3.0E+00 + 0.0E+00

    x3 = 1.0E+00 + sqrt ( 2.0E+00 ) / 2.0E+00
    y3 = 2.0E+00 + sqrt ( 2.0E+00 ) / 2.0E+00
    z3 = 3.0E+00 + 1.0E+00
    !
    !  Coordinates of the test points.
    !
    xtest(1) = 1.0E+00
    ytest(1) = 2.0E+00
    ztest(1) = 3.0E+00

    xtest(2) = 1.0E+00 + 0.25E+00 * sqrt ( 2.0E+00 )
    ytest(2) = 2.0E+00 + 0.25E+00 * sqrt ( 2.0E+00 )
    ztest(2) = 3.0E+00 + 0.25E+00 * 0.0E+00

    xtest(3) = 0.0E+00
    ytest(3) = 0.0E+00
    ztest(3) = 0.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST209'
    write ( *, '(a)' ) '  For a triangle in 3D:'
    write ( *, '(a)' ) '  TRIANGLE_ANGLES_3D computes the angles;'
    write ( *, '(a)' ) '  TRIANGLE_AREA_3D computes the area;'
    write ( *, '(a)' ) '  TRIANGLE_AREA_2_3D computes the area;'
    write ( *, '(a)' ) '  TRIANGLE_CENTROID_3D computes the centroid.'
    write ( *, '(a)' ) ' '
    call rvec_print_3d ( x1, y1, z1, '  Vertex A' )
    call rvec_print_3d ( x2, y2, z2, '  Vertex B' )
    call rvec_print_3d ( x3, y3, z3, '  Vertex C' )

    write ( *, '(a)' ) ' '

    call triangle_angles_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, a1, a2, a3 )

    write ( *, '(a,3g14.6)' ) '  Angles in radians: ', a1, a2, a3
    write ( *, '(a,3g14.6)' ) '  Angles in degrees: ', &
        radians_to_degrees ( a1 ), &
        radians_to_degrees ( a2 ), radians_to_degrees ( a3 )

    call triangle_area_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, area )

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Area is ', area

    call triangle_area_2_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, area )

    write ( *, '(a,g14.6)' ) '  Second area calculation is ', area

    call triangle_centroid_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, x, y, z )

    write ( *, '(a)' ) ' '
    call rvec_print_3d ( x, y, z, '  Centroid' )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     X	 Y     Z    DIST'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x = xtest(i)
        y = ytest(i)
        z = ztest(i)

        call triangle_point_dist_3d ( x1, y1, z1, x2, y2, z2, x3, y3, z3, &
            x, y, z, dist )

        write ( *, '(3g12.4,2x,g14.6)' ) x, y, z, dist

    end do

    return
    end
    subroutine test210
    !
    !*******************************************************************************
    !
    !! TEST210 tests TRIANGLE_CENTROID_2D;
    !! TEST210 tests TRIANGLE_CIRCUMCENTER_2D;
    !! TEST210 tests TRIANGLE_INCENTER_2D;
    !! TEST210 tests TRIANGLE_ORTHOCENTER_2D;
    !
    implicit none
    !
    integer, parameter :: ntest = 4
    !
    integer i
    real x1
    real x2
    real x3
    real x4
    real x5
    real x6
    real x7
    real y1
    real y2
    real y3
    real y4
    real y5
    real y6
    real y7
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST210'
    write ( *, '(a)' ) '  For a triangle in 2D:'
    write ( *, '(a)' ) '  TRIANGLE_CENTROID_2D computes the centroid.'
    write ( *, '(a)' ) '  TRIANGLE_CIRCUMCENTER_2D computes the circumcenter.'
    write ( *, '(a)' ) '  TRIANGLE_INCENTER_2D computes the incenter.'
    write ( *, '(a)' ) '  TRIANGLE_ORTHOCENTER_2D computes the orthocenter.'

    do i = 1, ntest

        if ( i == 1 ) then
            x1 = 0.0E+00
            y1 = 0.0E+00
            x2 = 1.0E+00
            y2 = 0.0E+00
            x3 = 0.0E+00
            y3 = 1.0E+00
        else if ( i == 2 ) then
            x1 = 0.0E+00
            y1 = 0.0E+00
            x2 = 1.0E+00
            y2 = 0.0E+00
            x3 = 0.5E+00
            y3 = sqrt ( 3.0E+00 ) / 2.0E+00
        else if ( i == 3 ) then
            x1 = 0.0E+00
            y1 = 0.0E+00
            x2 = 1.0E+00
            y2 = 0.0E+00
            x3 = 0.5E+00
            y3 = 10.0E+00
        else if ( i == 4 ) then
            x1 = 0.0E+00
            y1 = 0.0E+00
            x2 = 1.0E+00
            y2 = 0.0E+00
            x3 = 10.0E+00
            y3 = 2.0E+00
        end if

        write ( *, '(a)' ) ' '
        call rvec_print_2d ( x1, y1, '  Vertex A' )
        call rvec_print_2d ( x2, y2, '  Vertex B' )
        call rvec_print_2d ( x3, y3, '  Vertex C' )

        write ( *, '(a)' ) ' '

        call triangle_centroid_2d ( x1, y1, x2, y2, x3, y3, x4, y4 )

        call rvec_print_2d ( x4, y4, '  Centroid' )

        call triangle_circumcenter_2d ( x1, y1, x2, y2, x3, y3, x5, y5 )

        call rvec_print_2d ( x5, y5, '  Circumcenter' )

        call triangle_incenter_2d ( x1, y1, x2, y2, x3, y3, x6, y6 )

        call rvec_print_2d ( x6, y6, '  Incenter' )

        call triangle_orthocenter_2d ( x1, y1, x2, y2, x3, y3, x7, y7 )

        call rvec_print_2d ( x7, y7, '  Orthocenter' )

    end do

    return
    end
    subroutine test211
    !
    !*******************************************************************************
    !
    !! TEST211 tests TRIANGLE_ORIENTATION_2D.
    !
    implicit none
    !
    integer i
    character ( len = 16 ) message(0:3)
    real x1
    real x2
    real x3
    real y1
    real y2
    real y3
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST211'
    write ( *, '(a)' ) '  TRIANGLE_ORIENTATION_2D determines orientation'
    write ( *, '(a)' ) '    of a triangle.'

    message(0) = 'Counterclockwise'
    message(1) = 'Clockwise'
    message(2) = 'Colinear'
    message(3) = 'Nondistinct'

    x1 = 4.0E+00
    y1 = 2.0E+00

    x2 = 1.0E+00
    y2 = 5.0E+00

    x3 = -2.0E+00
    y3 = 2.0E+00

    i = triangle_orientation_2d ( x1, y1, x2, y2, x3, y3 )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Triangle vertices:'
    write ( *, '(2g14.6)' ) x1, y1
    write ( *, '(2g14.6)' ) x2, y2
    write ( *, '(2g14.6)' ) x3, y3
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  TRIANGLE_ORIENTATION_2D = ', i
    write ( *, '(a)' ) '  These points are ' // message(i)

    x1 =  1.0E+00
    y1 =  5.0E+00

    x2 =  4.0E+00
    y2 =  2.0E+00

    x3 =  1.0E+00
    y3 = -1.0E+00

    i = triangle_orientation_2d ( x1, y1, x2, y2, x3, y3 )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Triangle vertices:'
    write ( *, '(2g14.6)' ) x1, y1
    write ( *, '(2g14.6)' ) x2, y2
    write ( *, '(2g14.6)' ) x3, y3
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  TRIANGLE_ORIENTATION_2D = ', i
    write ( *, '(a)' ) '  These points are ' // message(i)
    !
    !  Colinear points
    !
    x1 =  1.0E+00
    y1 =  5.0E+00

    x2 =  2.0E+00
    y2 =  7.0E+00

    x3 =  3.0E+00
    y3 =  9.0E+00

    i = triangle_orientation_2d ( x1, y1, x2, y2, x3, y3 )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Triangle vertices:'
    write ( *, '(2g14.6)' ) x1, y1
    write ( *, '(2g14.6)' ) x2, y2
    write ( *, '(2g14.6)' ) x3, y3
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  TRIANGLE_ORIENTATION_2D = ', i
    write ( *, '(a)' ) '  The points are ' // message(i)
    !
    !  Nondistinct points
    !
    x1 =  1.0E+00
    y1 =  5.0E+00

    x2 =  4.0E+00
    y2 =  2.0E+00

    x3 =  1.0E+00
    y3 =  5.0E+00

    i = triangle_orientation_2d ( x1, y1, x2, y2, x3, y3 )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Triangle vertices:'
    write ( *, '(2g14.6)' ) x1, y1
    write ( *, '(2g14.6)' ) x2, y2
    write ( *, '(2g14.6)' ) x3, y3
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  TRIANGLE_ORIENTATION_2D = ', i
    write ( *, '(a)' ) '  The points are ' // message(i)

    return
    end
    subroutine test212
    !
    !*******************************************************************************
    !
    !! TEST212 tests TRIANGLE_SAMPLE_2D.
    !! TEST212 tests TRIANGLE_XY_TO_XSI_2D.
    !
    implicit none
    !
    integer i
    real x
    real, parameter :: x1 = 4.0E+00
    real, parameter :: x2 = 1.0E+00
    real, parameter :: x3 = -2.0E+00
    real xsi1
    real xsi2
    real xsi3
    real y
    real, parameter :: y1 = 2.0E+00
    real, parameter :: y2 = 5.0E+00
    real, parameter :: y3 = 2.0E+00
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST212'
    write ( *, '(a)' ) '  TRIANGLE_SAMPLE_2D samples a triangle.'
    write ( *, '(a)' ) '  TRIANGLE_XY_TO_XSI_2D converts XY to XSI coordinates.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  We are computing the XSI coordinates just to verify'
    write ( *, '(a)' ) '  that the points are inside the triangle.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Triangle vertices:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) x1, y1
    write ( *, '(2g14.6)' ) x2, y2
    write ( *, '(2g14.6)' ) x3, y3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Sample points (X,Y) and (XSI1,XSI2,XSI3) coordinates:'
    write ( *, '(a)' ) ' '

    do i = 1, 10
        call triangle_sample_2d ( x1, y1, x2, y2, x3, y3, x, y )
        call triangle_xy_to_xsi_2d ( x1, y1, x2, y2, x3, y3, x, y, xsi1, &
            xsi2, xsi3 )
        write ( *, '(2f8.4,4x,3f8.4)' ) x, y, xsi1, xsi2, xsi3
    end do

    return
    end
    subroutine test213
    !
    !*******************************************************************************
    !
    !! TEST213 tests TRIANGLE_SAMPLE_2D.
    !! TEST213 tests TRIANGLE_XY_TO_XSI_2D.
    !! TEST213 tests TRIANGLE_XSI_TO_XY_2D.
    !
    implicit none
    !
    integer i
    real x
    real, parameter :: x1 = 4.0E+00
    real, parameter :: x2 = 1.0E+00
    real, parameter :: x3 = -2.0E+00
    real xsi1
    real xsi2
    real xsi3
    real xx
    real y
    real, parameter :: y1 = 2.0E+00
    real, parameter :: y2 = 5.0E+00
    real, parameter :: y3 = 2.0E+00
    real yy
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST213'
    write ( *, '(a)' ) '  TRIANGLE_SAMPLE_2D samples a triangle.'
    write ( *, '(a)' ) '  TRIANGLE_XY_TO_XSI_2D converts XY to XSI coordinates.'
    write ( *, '(a)' ) '  TRIANGLE_XSI_TO_XY_2D converts XSI to XY coordinates.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  We verify that (X,Y) -> (XSI1,XSI2,XSI3) -> (X,Y)'
    write ( *, '(a)' ) '  works properly.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Triangle vertices:'
    write ( *, '(a)' ) ' '
    write ( *, '(2g14.6)' ) x1, y1
    write ( *, '(2g14.6)' ) x2, y2
    write ( *, '(2g14.6)' ) x3, y3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Sample points:'
    write ( *, '(a)' ) ' '

    do i = 1, 10

        if ( i == 1 ) then
            x = ( x1 + x2 + x3 ) / 3.0E+00
            y = ( y1 + y2 + y3 ) / 3.0E+00
        else if ( i == 2 ) then
            x = 3.0E+00
            y = 0.0E+00
        else
            call triangle_sample_2d ( x1, y1, x2, y2, x3, y3, x, y )
        end if

        call triangle_xy_to_xsi_2d ( x1, y1, x2, y2, x3, y3, x, y, xsi1, xsi2, xsi3 )
        call triangle_xsi_to_xy_2d ( x1, y1, x2, y2, x3, y3, xsi1, xsi2, xsi3, &
            xx, yy )

        write ( *, '(a)' ) ' '
        write ( *, '(2f8.4,4x,3f8.4)' ) x, y, xsi1, xsi2, xsi3
        write ( *, '(2f8.4)' ) xx, yy

    end do

    return
    end
    subroutine test214
    !
    !*******************************************************************************
    !
    !! TEST214 tests TRIANGULATION_BOUNDARY_COUNT.
    !
    implicit none
    !
    integer bound_num
    integer, parameter :: point_num = 13
    integer, parameter :: tri_num = 16
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST214'
    write ( *, '(a)' ) '  TRIANGULATION_BOUNDARY_COUNT determines the number of'
    write ( *, '(a)' ) '  edges that lie on the convex hull of a region that has'
    write ( *, '(a)' ) '  been triangulated.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  Number of points =         ', point_num
    write ( *, '(a,i6)' ) '  Number of triangles =      ', tri_num

    call triangulation_boundary_count ( point_num, tri_num, bound_num )

    write ( *, '(a,i6)' ) '  Number of boundary edges = ', bound_num

    return
    end
    subroutine test215
    !
    !*******************************************************************************
    !
    !! TEST215 tests TRIANGULATION_CHECK.
    !
    implicit none
    !
    integer, parameter :: tri_num = 16
    integer, parameter :: point_num = 13
    !
    integer i
    integer ierror
    integer isave
    integer point_num2
    integer tri_num2
    integer, dimension (3,tri_num ) :: tri_vert = reshape ( (/ &
        3,   4,   1, &
        3,   1,   2, &
        3,   2,   8, &
        2,   1,   5, &
        8,   2,  13, &
        8,  13,   9, &
        3,   8,   9, &
        13,   2,   5, &
        9,  13,   7, &
        7,  13,   5, &
        6,   7,   5, &
        9,   7,   6, &
        10,   9,   6, &
        6,   5,  12, &
        11,   6,  12, &
        10,   6,  11 /), (/ 3, tri_num /) )
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST215'
    write ( *, '(a)' ) '  For a triangulation of a set of nodes,'
    write ( *, '(a)' ) '  TRIANGULATION_CHECK checks the triangulation.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Triangle  Nodes'
    write ( *, '(a)' ) ' '
    do i = 1, tri_num
        write ( *, '(7x,i3,2x,3i4)' ) i, tri_vert(1:3,i)
    end do
    !
    !  Pass all tests
    !
    call triangulation_check ( point_num, tri_num, tri_vert, ierror )

    write ( *, '(a,i6)' ) '  Error code = ', ierror
    !
    !  Fail test 1
    !
    point_num2 = 2

    call triangulation_check ( point_num2, tri_num, tri_vert, ierror )

    write ( *, '(a,i6)' ) '  Error code = ', ierror
    !
    !  Fail test 2
    !
    tri_num2 = 0

    call triangulation_check ( point_num, tri_num2, tri_vert, ierror )

    write ( *, '(a,i6)' ) '  Error code = ', ierror
    !
    !  Fail test 3
    !
    isave = tri_vert(2,5)
    tri_vert(2,5) = 0

    call triangulation_check ( point_num, tri_num, tri_vert, ierror )

    write ( *, '(a,i6)' ) '  Error code = ', ierror
    tri_vert(2,5) = isave
    !
    !  Fail test 4
    !
    isave = tri_vert(3,10)
    tri_vert(3,10) = 2 * point_num + 1

    call triangulation_check ( point_num, tri_num, tri_vert, ierror )

    write ( *, '(a,i6)' ) '  Error code = ', ierror
    tri_vert(3,10) = isave
    !
    !  Fail test 5
    !
    tri_vert(3,4) = 3
    tri_vert(3,8) = 3
    tri_vert(3,10) = 3
    tri_vert(3,11) = 3
    tri_vert(2,14) = 3

    call triangulation_check ( point_num, tri_num, tri_vert, ierror )
    write ( *, '(a,i6)' ) '  Error code = ', ierror

    tri_vert(3,4) = 5
    tri_vert(3,8) = 5
    tri_vert(3,10) = 5
    tri_vert(3,11) = 5
    tri_vert(2,14) = 5
    !
    !  Fail test 6
    !
    tri_vert(1,9) = 7
    call triangulation_check ( point_num, tri_num, tri_vert, ierror )
    write ( *, '(a,i6)' ) '  Error code = ', ierror
    tri_vert(1,9) = 9
    !
    !  Fail test 7
    !
    tri_vert(3,7) = 2
    call triangulation_check ( point_num, tri_num, tri_vert, ierror )
    write ( *, '(a,i6)' ) '  Error code = ', ierror
    tri_vert(3,7) = 9

    return
    end
    subroutine test216
    !
    !*******************************************************************************
    !
    !! TEST216 tests TRIANGULATION_NEIGHBOR.
    !
    implicit none
    !
    integer, parameter :: tri_num = 16
    integer, parameter :: point_num = 13
    !
    integer s1
    integer s2
    integer t1
    integer t2
    integer, dimension (3,tri_num ) :: tri_vert = reshape ( (/ &
        3,   4,   1, &
        3,   1,   2, &
        3,   2,   8, &
        2,   1,   5, &
        8,   2,  13, &
        8,  13,   9, &
        3,   8,   9, &
        13,   2,   5, &
        9,  13,   7, &
        7,  13,   5, &
        6,   7,   5, &
        9,   7,   6, &
        10,   9,   6, &
        6,   5,  12, &
        11,   6,  12, &
        10,   6,  11 /), (/ 3, tri_num /) )
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST216'
    write ( *, '(a)' ) '  For a triangulation of a set of nodes,'
    write ( *, '(a)' ) '  TRIANGULATION_NEIGHBOR determines triangle neighbors.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  T1  S1  T2  S2'
    write ( *, '(a)' ) ' '
    do t1 = 1, tri_num
        do s1 = 1, 3
            call triangulation_neighbor ( tri_num, tri_vert, t1, s1, t2, s2 )
            write ( *, '(4i4)' ) t1, s1, t2, s2
        end do
    end do

    return
    end
    subroutine test217
    !
    !*******************************************************************************
    !
    !! TEST217 tests TRIANGULATION_NABE_TRIANGLES.
    !
    implicit none
    !
    integer, parameter :: num_tri = 16
    !
    integer i
    integer, dimension (3,num_tri ) :: nod_tri = reshape ( (/ &
        3,   4,   1, &
        3,   1,   2, &
        3,   2,   8, &
        2,   1,   5, &
        8,   2,  13, &
        8,  13,   9, &
        3,   8,   9, &
        13,   2,   5, &
        9,  13,   7, &
        7,  13,   5, &
        6,   7,   5, &
        9,   7,   6, &
        10,   9,   6, &
        6,   5,  12, &
        11,   6,  12, &
        10,   6,  11 /), (/ 3, num_tri /) )
    integer, dimension (3,num_tri) :: tnbr
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST217'
    write ( *, '(a)' ) '  For a triangulation of a set of nodes,'
    write ( *, '(a)' ) '  TRIANGULATION_NABE_NODES determines the adjacency'
    write ( *, '(a)' ) '    relationships between triangles.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Triangle  Nodes'
    write ( *, '(a)' ) ' '
    do i = 1, num_tri
        write ( *, '(7x,i3,2x,3i4)' ) i, nod_tri(1:3,i)
    end do

    call triangulation_nabe_triangles ( num_tri, nod_tri, tnbr )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Triangle  Neighbors'
    write ( *, '(a)' ) ' '
    do i = 1, num_tri
        write ( *, '(7x,i3,2x,3i4)' ) i, tnbr(1:3,i)
    end do

    return
    end
    subroutine test218
    !
    !*******************************************************************************
    !
    !! TEST218 tests POINTS_NEAREST_POINT_NAIVE_2D.
    !! TEST218 tests TRIANGULATION_SEARCH_2D.
    !
    implicit none
    !
    integer, parameter :: num_pts = 13
    integer, parameter :: ntest = 10
    !
    real d1
    real d2
    real d3
    real dist
    real dnear
    integer edge
    integer i1
    integer i2
    integer i3
    integer itest
    integer nd(ntest)
    integer nnear
    integer nod_tri(3,2*num_pts)
    integer num_tri
    integer seed
    integer td(ntest)
    integer tnbr(3,2*num_pts)
    integer tnear
    integer triangle
    real x1
    real x2
    real x3
    real, dimension (2,num_pts) :: xc = reshape ( (/ &
        0.0E+00, 0.0E+00, &
        2.0E+00, 2.0E+00, &
        -1.0E+00, 3.0E+00, &
        -2.0E+00, 2.0E+00, &
        8.0E+00, 2.0E+00, &
        9.0E+00, 5.0E+00, &
        7.0E+00, 4.0E+00, &
        5.0E+00, 6.0E+00, &
        6.0E+00, 7.0E+00, &
        8.0E+00, 8.0E+00, &
        11.0E+00, 7.0E+00, &
        10.0E+00, 4.0E+00, &
        6.0E+00, 4.0E+00 /), (/ 2, num_pts /) )
    real xd(2,ntest)
    real y1
    real y2
    real y3
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST218'
    write ( *, '(a)' ) '  Given a point set XC, and a single point XD,'
    write ( *, '(a)' ) '  find the nearest point in XC to XD.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  POINTS_NEAREST_POINT_NAIVE_2D uses a naive method.'
    write ( *, '(a)' ) '  TRIANGULATION_SEARCH_2D finds a triangle containing'
    write ( *, '(a)' ) '    the point.  Often, one of these vertices is the'
    write ( *, '(a)' ) '    closest point.'

    seed = 0
    call random_initialize ( seed )
    !
    !  Set up the Delaunay triangulation.
    !
    call rtris2 ( num_pts, xc, num_tri, nod_tri, tnbr )
    !
    !  Get the test points.
    !
    call triangulation_sample_2d ( num_pts, xc, num_tri, nod_tri, &
        ntest, xd, td )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    X        Y     Distance  Index'
    write ( *, '(a)' ) ' '

    do itest = 1, ntest

        call points_nearest_point_naive_2d ( num_pts, xc, xd(1,itest), nnear, &
            dnear )

        write ( *, '(a)' ) ' '
        write ( *, '(a,2f8.4   )' ) '  XD       ', xd(1:2,itest)
        write ( *, '(a,3f8.4,i6)' ) '  Naive    ', xc(1:2,nnear), dnear, nnear

        call triangulation_search_2d ( num_pts, xc, num_tri, nod_tri, tnbr, &
            xd(1,itest), xd(2,itest), triangle, edge )

        i1 = nod_tri(1,triangle)
        x1 = xc(1,i1)
        y1 = xc(2,i1)

        d1 = sqrt ( ( xd(1,itest) - x1 )**2 + ( xd(2,itest) - y1 )**2 )

        dist = d1
        nnear = i1

        i2 = nod_tri(2,triangle)
        x2 = xc(1,i2)
        y2 = xc(2,i2)

        d2 = sqrt ( ( xd(1,itest) - x2 )**2 + ( xd(2,itest) - y2 )**2 )

        if ( d2 < dist ) then
            dnear = d2
            nnear = i2
        end if

        i3 = nod_tri(3,triangle)
        x3 = xc(1,i3)
        y3 = xc(2,i3)
        d3 = sqrt ( ( xd(1,itest) - x3 )**2 + ( xd(2,itest) - y3 )**2 )

        if ( d3 < dist ) then
            dnear = d3
            nnear = i3
        end if

        write ( *, '(a,3f8.4,i6)' ) '  Delaunay ', xc(1:2,nnear), dnear, nnear

    end do

    return
    end
    subroutine test219
    !
    !*******************************************************************************
    !
    !! TEST219 tests TUBE_2D.
    !
    implicit none
    !
    integer, parameter :: maxn = 5
    !
    real dist
    integer i
    integer itest
    integer n
    real x(maxn)
    real x1(maxn)
    real x2(maxn)
    real y(maxn)
    real y1(maxn)
    real y2(maxn)
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST219'
    write ( *, '(a)' ) '  TUBE_2D computes corners of a tube of radius'
    write ( *, '(a)' ) '    DIST surrounding a sequence of points.'

    do itest = 1, 4

        if ( itest == 1 ) then

            n = 4

            x(1) =  0.0E+00
            y(1) =  0.0E+00

            x(2) =  4.0E+00
            y(2) =  3.0E+00

            x(3) =  4.0E+00
            y(3) =  0.0E+00

            x(4) =  0.0E+00
            y(4) =  0.0E+00

            dist = 0.5E+00

        else if ( itest == 2 ) then

            n = 5

            x(1) =  0.0E+00
            y(1) =  0.0E+00

            x(2) =  2.0E+00
            y(2) =  0.0E+00

            x(3) =  2.0E+00
            y(3) =  1.0E+00

            x(4) =  0.0E+00
            y(4) =  1.0E+00

            x(5) =  0.0E+00
            y(5) =  0.0E+00

            dist = 0.5E+00

        else if ( itest == 3 ) then

            n = 5

            x(1) =  10.0E+00
            y(1) =  20.0E+00

            x(2) =  20.0E+00
            y(2) =  20.0E+00

            x(3) =  10.0E+00
            y(3) =  10.0E+00

            x(4) =  20.0E+00
            y(4) =  10.0E+00

            x(5) =  10.0E+00
            y(5) =  20.0E+00

            dist = 1.0E+00

        else if ( itest == 4 ) then

            n = 5

            x(1) =  0.0E+00
            y(1) =  0.0E+00

            x(2) =  10.0E+00
            y(2) =  0.0E+00

            x(3) =  10.0E+00
            y(3) =  10.0E+00

            x(4) =  10.0E+00
            y(4) =  0.0E+00

            x(5) =  0.0E+00
            y(5) =  0.0E+00

            dist = 1.0E+00

        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,i6)' ) '  Test number ', itest
        write ( *, '(a,i6)' ) '  Number of points N = ', n
        write ( *, '(a,g14.6)' ) '  Tube radius DIST = ', dist
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Points to surround:'
        write ( *, '(a)' ) ' '
        do i = 1, n
            write ( *, '(2g14.6)' ) x(i), y(i)
        end do

        call tube_2d ( dist, n, x, y, x1, y1, x2, y2 )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '      X1,	  Y1'
        write ( *, '(a)' ) ' '
        do i = 1, n
            write ( *, '(2g14.6)' ) x1(i), y1(i)
        end do
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '      X2,         Y2'
        write ( *, '(a)' ) ' '
        do i = 1, n
            write ( *, '(2g14.6)' ) x2(i), y2(i)
        end do

    end do

    return
    end
    subroutine test220
    !
    !*******************************************************************************
    !
    !! TEST220 tests VECTOR_DIRECTIONS_2D;
    !
    implicit none
    !
    integer, parameter :: ntest = 5
    !
    real ax
    real ax_deg
    real ay
    real ay_deg
    integer i
    real x1
    real xtest(ntest)
    real y1
    real ytest(ntest)
    !
    xtest(1) = 1.0E+00
    ytest(1) = 0.0E+00

    xtest(2) =   sqrt ( 3.0E+00 )
    ytest(2) =   1.0E+00

    xtest(3) = - sqrt ( 3.0E+00 )
    ytest(3) =   1.0E+00

    xtest(4) = - sqrt ( 3.0E+00 )
    ytest(4) = - 1.0E+00

    xtest(5) =   sqrt ( 3.0E+00 )
    ytest(5) = - 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST220'
    write ( *, '(a)' ) '  VECTOR_DIRECTIONS_2D computes the angles'
    write ( *, '(a)' ) '  that a vector makes with the X, Y and Z axes.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     X        Y       AX       AY   ' // &
        '    AX       AY'
    write ( *, '(a)' ) '                     (__Radians___)' // &
        '  (___Degrees___)'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x1 = xtest(i)
        y1 = ytest(i)

        call vector_directions_2d ( x1, y1, ax, ay )

        ax_deg = radians_to_degrees ( ax )
        ay_deg = radians_to_degrees ( ay )

        write ( *, '(9f9.3)') x1, y1, ax, ay, ax_deg, ay_deg

    end do

    return
    end
    subroutine test221
    !
    !*******************************************************************************
    !
    !! TEST221 tests VECTOR_DIRECTIONS_3D;
    !
    implicit none
    !
    integer, parameter :: ntest = 3
    !
    real ax
    real ax_deg
    real ay
    real ay_deg
    real az
    real az_deg
    integer i
    real x1
    real xtest(ntest)
    real y1
    real ytest(ntest)
    real z1
    real ztest(ntest)
    !
    xtest(1) = 1.0E+00
    ytest(1) = 0.0E+00
    ztest(1) = 0.0E+00

    xtest(2) = 1.0E+00
    ytest(2) = 2.0E+00
    ztest(2) = 3.0E+00

    xtest(3) = 0.0E+00
    ytest(3) = 0.0E+00
    ztest(3) = 1.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST221'
    write ( *, '(a)' ) '  VECTOR_DIRECTIONS_3D computes the angles'
    write ( *, '(a)' ) '  that a vector makes with the X, Y and Z axes.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    X       Y       Z      AX      AY      AZ   ' // &
        '   AX      AY      AZ   '
    write ( *, '(a)' ) '  			 (_____Radians_______)' // &
        ' (_______Degrees_______)'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x1 = xtest(i)
        y1 = ytest(i)
        z1 = ztest(i)

        call vector_directions_3d ( x1, y1, z1, ax, ay, az )

        ax_deg = radians_to_degrees ( ax )
        ay_deg = radians_to_degrees ( ay )
        az_deg = radians_to_degrees ( az )

        write ( *, '(9f8.3)') x1, y1, z1, ax, ay, az, ax_deg, ay_deg, az_deg

    end do

    return
    end
    subroutine test222
    !
    !*******************************************************************************
    !
    !! TEST222 tests VECTOR_ROTATE_2D;
    !
    implicit none
    !
    integer, parameter :: ntest = 3
    !
    real angle
    real atest(ntest)
    integer i
    real x1
    real x2
    real xtest(ntest)
    real y1
    real y2
    real ytest(ntest)
    !
    xtest(1) = 1.0E+00
    ytest(1) = 0.0E+00
    atest(1) = 30.0E+00

    xtest(2) = 0.0E+00
    ytest(2) = 2.0E+00
    atest(2) = -45.0E+00

    xtest(3) = 1.0E+00
    ytest(3) = 1.0E+00
    atest(3) = 270.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST222'
    write ( *, '(a)' ) '  VECTOR_ROTATE_2D rotates a vector through'
    write ( *, '(a)' ) '  a given angle around the origin.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    X1      Y1   Angle      X2      Y2'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x1 = xtest(i)
        y1 = ytest(i)

        angle = degrees_to_radians ( atest(i) )

        call vector_rotate_2d ( x1, y1, angle, x2, y2 )

        write ( *, '(5f8.3)') x1, y1, atest(i), x2, y2

    end do

    return
    end
    subroutine test223
    !
    !*******************************************************************************
    !
    !! TEST223 tests VECTOR_ROTATE_BASE_2D;
    !
    implicit none
    !
    integer, parameter :: ntest = 4
    !
    real angle
    real atest(ntest)
    integer i
    real x1
    real x2
    real xb
    real xtest(ntest)
    real y1
    real y2
    real yb
    real ytest(ntest)
    !
    xb = 10.0E+00
    yb = 5.0E+00

    xtest(1) = xb + 1.0E+00
    ytest(1) = yb + 0.0E+00
    atest(1) = 30.0E+00

    xtest(2) = xb + 0.0E+00
    ytest(2) = yb + 2.0E+00
    atest(2) = -45.0E+00

    xtest(3) = xb + 1.0E+00
    ytest(3) = yb + 1.0E+00
    atest(3) = 270.0E+00

    xtest(4) = xb
    ytest(4) = yb
    atest(4) = 20.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST223'
    write ( *, '(a)' ) '  VECTOR_ROTATE_BASE_2D rotates a vector (X1,Y1)'
    write ( *, '(a)' ) '  through an angle around a base point (XB,YB).'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    X1      Y1      XB      YB   Angle      X2      Y2'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        x1 = xtest(i)
        y1 = ytest(i)

        angle = degrees_to_radians ( atest(i) )

        call vector_rotate_base_2d ( x1, y1, xb, yb, angle, x2, y2 )

        write ( *, '(7f8.3)') x1, y1, xb, yb, atest(i), x2, y2

    end do

    return
    end
    subroutine test224
    !
    !*******************************************************************************
    !
    !! TEST224 tests VECTOR_SEPARATION_3D;
    !
    implicit none
    !
    integer, parameter :: ntest = 5
    !
    integer i
    integer j
    real theta
    real theta_deg
    real v1(3)
    real v2(3)
    real xtest(ntest)
    real ytest(ntest)
    real ztest(ntest)
    !
    xtest(1) = 1.0E+00
    ytest(1) = 0.0E+00
    ztest(1) = 0.0E+00

    xtest(2) = 1.0E+00
    ytest(2) = 2.0E+00
    ztest(2) = 3.0E+00

    xtest(3) = 0.0E+00
    ytest(3) = 0.0E+00
    ztest(3) = 1.0E+00

    xtest(4) = -3.0E+00
    ytest(4) =  2.0E+00
    ztest(4) = -1.0E+00

    xtest(5) = -2.0E+00
    ytest(5) = -4.0E+00
    ztest(5) = -6.0E+00

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST224'
    write ( *, '(a)' ) '  VECTOR_SEPARATION_3D computes the separation angle'
    write ( *, '(a)' ) '  between two vectors.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    -----Vector 1-----      -----Vector 2-----  ' // &
        '   Radians    Degrees'
    write ( *, '(a)' ) ' '

    do i = 1, ntest

        v1(1:3) = (/ xtest(i), ytest(i), ztest(i) /)

        do j = i+1, ntest

            v2(1:3) = (/ xtest(j), ytest(j), ztest(j) /)

            call vector_separation_3d ( v1, v2, theta )

            theta_deg = radians_to_degrees ( theta )

            write ( *, '(6f8.3,f8.3,5x,f8.3)') v1(1:3), v2(1:3), theta, theta_deg

        end do

    end do

    return
    end
    subroutine test225
    !
    !*******************************************************************************
    !
    !! TEST225 tests VOXEL_LINE_3D.
    !
    implicit none
    !
    integer x1
    integer x2
    integer y1
    integer y2
    integer z1
    integer z2
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST225'
    write ( *, '(a)' ) '  VOXEL_LINE_3D prints the voxels on a line in 3D.'

    x1 = 0
    y1 = 0
    z1 = 0

    x2 = 4
    y2 = 2
    z2 = 7

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Start:'
    write ( *, '(3i6)' ) x1, y1, z1
    write ( *, '(a)' ) 'End:'
    write ( *, '(3i6)' ) x2, y2, z2
    write ( *, '(a)' ) ' '

    call voxel_line_3d ( x1, y1, z1, x2, y2, z2 )

    return
    end
    subroutine test226
    !
    !*******************************************************************************
    !
    !! TEST226 tests VOXEL_REGION_3D.
    !
    !
    !  The test region is 8 by 9 by 1 voxels:
    !
    !    123456789
    !  1 .........
    !  2 ...11.1..
    !  3 ..11111..
    !  4 ...11.1..
    !  5 ......1..
    !  6 .11..11..
    !  7 ..1......
    !  8 .......1.
    !
    implicit none
    !
    integer, parameter :: maxlist = 100
    integer, parameter :: nx = 8
    integer, parameter :: ny = 9
    integer, parameter :: nz = 1
    !
    integer i
    integer iregion
    integer ishow(nx,ny,nz)
    integer j
    integer k
    integer l
    integer list(maxlist)
    integer nelements
    integer nlist
    integer nregion
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST226'
    write ( *, '(a)' ) '  VOXEL_REGION_3D groups voxels into regions.'

    ishow(1:nx,1:ny,1:nz) = 0

    ishow(2,4,1) = 1
    ishow(2,5,1) = 1
    ishow(2,7,1) = 1

    ishow(3,3,1) = 1
    ishow(3,4,1) = 1
    ishow(3,5,1) = 1
    ishow(3,6,1) = 1
    ishow(3,7,1) = 1

    ishow(4,4,1) = 1
    ishow(4,5,1) = 1
    ishow(4,7,1) = 1

    ishow(5,7,1) = 1

    ishow(6,2,1) = 1
    ishow(6,3,1) = 1
    ishow(6,6,1) = 1
    ishow(6,7,1) = 1

    ishow(7,3,1) = 1

    ishow(8,8,1) = 1

    call voxel_region_3d ( ishow, list, maxlist, nlist, nregion, nx, ny, nz )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i6)' ) '  Number of regions found = ', nregion
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The nonzero ISHOW array elements are:'
    write ( *, '(a)' ) ' '

    do i = 1, nx
        do j = 1, ny
            do k = 1, nz
                l = ishow(i,j,k)
                if ( l /= 0 ) then
                    write ( *, '(4i6)' ) i, j, k, l
                end if
            end do
        end do
    end do

    if ( nlist > maxlist ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  The stack-based list of regions is unusable.'

    else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  The stack-based list of regions is:'
        write ( *, '(a)' ) ' '

        iregion = nregion

        do while ( nlist > 0 )

            nelements = list(nlist)
            nlist = nlist - 1

            write ( *, '(a)' ) ' '
            write ( *, '(a,i6,a,i6,a)' ) &
                '  Region ', iregion, ' includes ', nelements, ' voxels:'
            write ( *, '(a)' ) ' '

            do l = 1, nelements
                k = list(nlist)
                nlist = nlist - 1
                j = list(nlist)
                nlist = nlist - 1
                i = list(nlist)
                nlist = nlist - 1
                write ( *, '(3i6)' ) i, j, k
            end do

            iregion = iregion - 1

        end do

    end if

    return
    end
    subroutine test227
    !
    !*******************************************************************************
    !
    !! TEST227 tests VOXEL_STEP_3D.
    !
    implicit none
    !
    integer i
    integer i1
    integer i2
    integer inc
    integer j1
    integer j2
    integer jnc
    integer k1
    integer k2
    integer knc
    !
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST227'
    write ( *, '(a)' ) '  VOXEL_STEP_3D steps along a line from'
    write ( *, '(a)' ) '    one voxel to another.'

    i1 = 1
    j1 = 1
    k1 = 5

    inc = 7
    jnc = 3
    knc = -1

    i2 = i1
    j2 = j1
    k2 = k1

    write ( *, '(3i6)' ) i2, j2, k2

    do i = 1, 10
        call voxel_step_3d ( i1, j1, k1, i2, j2, k2, inc, jnc, knc )
        write ( *, '(3i6)' ) i2, j2, k2
    end do

    write ( *, '(a)' ) ' '

    i1 = i2
    j1 = j2
    k1 = k2

    inc = -inc
    jnc = -jnc
    knc = -knc

    i2 = i1
    j2 = j1
    k2 = k1

    write ( *, '(3i6)' ) i2, j2, k2
    do i = 1, 10
        call voxel_step_3d ( i1, j1, k1, i2, j2, k2, inc, jnc, knc )
        write ( *, '(3i6)' ) i2, j2, k2
    end do

    return
    end

    end module geometry_prb
