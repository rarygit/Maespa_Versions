        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:32 2016
        MODULE RKQS__genmod
          INTERFACE 
            SUBROUTINE RKQS(Y,DYDX,N,X,HTRY,EPS,YSCAL,HDID,HNEXT,DERIVS,&
     &EXTRAPARS)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: Y(N)
              REAL(KIND=4) :: DYDX(N)
              REAL(KIND=4) :: X
              REAL(KIND=4) :: HTRY
              REAL(KIND=4) :: EPS
              REAL(KIND=4) :: YSCAL(N)
              REAL(KIND=4) :: HDID
              REAL(KIND=4) :: HNEXT
              EXTERNAL DERIVS
              REAL(KIND=4) :: EXTRAPARS(75)
            END SUBROUTINE RKQS
          END INTERFACE 
        END MODULE RKQS__genmod
