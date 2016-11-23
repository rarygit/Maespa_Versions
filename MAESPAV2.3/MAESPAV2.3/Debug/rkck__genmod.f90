        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 23 16:29:58 2016
        MODULE RKCK__genmod
          INTERFACE 
            SUBROUTINE RKCK(Y,DYDX,N,X,H,YOUT,YERR,DERIVS,EXTRAPARS)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: Y(N)
              REAL(KIND=4) :: DYDX(N)
              REAL(KIND=4) :: X
              REAL(KIND=4) :: H
              REAL(KIND=4) :: YOUT(N)
              REAL(KIND=4) :: YERR(N)
              EXTERNAL DERIVS
              REAL(KIND=4) :: EXTRAPARS(75)
            END SUBROUTINE RKCK
          END INTERFACE 
        END MODULE RKCK__genmod
