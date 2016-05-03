        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:32 2016
        MODULE ODEINT__genmod
          INTERFACE 
            SUBROUTINE ODEINT(YSTART,NVAR,X1,X2,EPS,H1,HMIN,NOK,NBAD,   &
     &DERIVS,EXTRAPARS)
              INTEGER(KIND=4) :: NVAR
              REAL(KIND=4) :: YSTART(NVAR)
              REAL(KIND=4) :: X1
              REAL(KIND=4) :: X2
              REAL(KIND=4) :: EPS
              REAL(KIND=4) :: H1
              REAL(KIND=4) :: HMIN
              INTEGER(KIND=4) :: NOK
              INTEGER(KIND=4) :: NBAD
              EXTERNAL DERIVS
              REAL(KIND=4) :: EXTRAPARS(75)
            END SUBROUTINE ODEINT
          END INTERFACE 
        END MODULE ODEINT__genmod
