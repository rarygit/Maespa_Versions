        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 23 15:25:51 2016
        MODULE RK4__genmod
          INTERFACE 
            SUBROUTINE RK4(Y,DYDX,N,X,H,YOUT,DERIVS,EXTRAPARS)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: Y(N)
              REAL(KIND=4) :: DYDX(N)
              REAL(KIND=4) :: X
              REAL(KIND=4) :: H
              REAL(KIND=4) :: YOUT(N)
              EXTERNAL DERIVS
              REAL(KIND=4) :: EXTRAPARS(75)
            END SUBROUTINE RK4
          END INTERFACE 
        END MODULE RK4__genmod
