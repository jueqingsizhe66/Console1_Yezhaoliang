    module geometry
    implicit none
    contains
    subroutine angle_contains_ray_2d ( x1, y1, x2, y2, x3, y3, x, y, inside )
    !
    !*******************************************************************************
    !
    !! ANGLE_CONTAINS_RAY_2D determines if an angle contains a ray, in 2D.
    !
    !
    !  Discussion:
    !
    !    The angle is defined by the sequence of points (X1,Y1), (X2,Y2)
    !    and (X3,Y3).
    !
    !    The ray is defined by the sequence of points (X2,Y2), (X,Y).
    !
    !  Modified:
    !
    !    17 March 2000
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real X1, Y1, X2, Y2, X3, Y3, the X and Y coordinates of
    !    the angle.
    !
    !    Input, real X, Y, the end point of the ray to be checked.
    !    The ray is assumed to have an origin at (X2,Y2).
    !
    !    Output, logical INSIDE, is .TRUE. if the ray is inside
    !    the angle or on its boundary, and .FALSE. otherwise.
    !
    implicit none
    !
    real a1
    real a2
    logical inside
    real x
    real x1
    real x2
    real x3
    real y
    real y1
    real y2
    real y3
    !
    a1 = angle_deg_2d ( x1, y1, x2, y2, x, y )
    a2 = angle_deg_2d ( x1, y1, x2, y2, x3, y3 )

    if ( a1 <= a2 ) then
        inside = .true.
    else
        inside = .false.
    end if

    return
    end
    real function angle_deg_2d ( x1, y1, x2, y2, x3, y3 )
    !
    !*******************************************************************************
    !
    !! ANGLE_DEG_2D returns the angle swept out between two rays in 2D.
    !
    !
    !  Discussion:
    !
    !    Except for the zero angle case, it should be true that
    !
    !      ANGLE_DEG_2D(X1,Y1,X2,Y2,X3,Y3)
    !    + ANGLE_DEG_2D(X3,Y3,X2,Y2,X1,Y1) = 360.0
    !
    !  Modified:
    !
    !    14 July 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real X1, Y1, X2, Y2, X3, Y3, define the rays
    !    ( X1-X2, Y1-Y2 ) and ( X3-X2, Y3-Y2 ) which in turn define the
    !    angle, counterclockwise from ( X1-X2, Y1-Y2 ).
    !
    !    Output, real ANGLE_DEG_2D, the angle swept out by the rays, measured
    !    in degrees.  0 <= ANGLE_DEG_2D < 360.  If either ray has zero length,
    !    then ANGLE_DEG_2D is set to 0.
    !
    implicit none
    !
    real x
    real x1
    real x2
    real x3
    real y
    real y1
    real y2
    real y3
    !
    x = ( x1 - x2 ) * ( x3 - x2 ) + ( y1 - y2 ) * ( y3 - y2 )
    y = ( x1 - x2 ) * ( y3 - y2 ) - ( y1 - y2 ) * ( x3 - x2 )

    if ( x == 0.0E+00 .and. y == 0.0E+00 ) then

        angle_deg_2d = 0.0E+00

    else

        !angle_rad_2d = atan2 ( y, x )

        !if (  < 0.0E+00 ) then
        !    !angle_rad_2d = angle_rad_2d + 2.0E+00 * r_pi ( )
        !    angle_deg_2d = 2.0E+00
        !end if

        angle_deg_2d = 1.0+2.0

    end if

    return
    end

    logical function TriangleTest(a,b,c)
    IMPLICIT NONE
    REAL, INTENT(IN) :: a, b, c
    logical :: test1,test2
    test1 = (a > 0.0) .AND. (b > 0.0) .AND. (c > 0.0)
    test2 = (a + b > c) .AND. (a + c > b) .AND. (b + c > a)
    TriangleTest = test1 .AND. test2
    ! both must be .TRUE.
    END FUNCTION TriangleTest

    REAL function Area(a,b,c) result(triangleArea)
    implicit none
    REAL,INTENT(IN) :: a,b,c
    REAL :: s
    s=(a+b+c) /2.0
    triangleArea=sqrt(s*(s-a)*(s-b)*(s-c))
    end


    end module geometry
