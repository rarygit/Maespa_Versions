        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 23 15:25:51 2016
        MODULE ZBRENT__genmod
          INTERFACE 
            FUNCTION ZBRENT(FUNC,X1,X2,TOLZ,EXTRAPARS,EXTRAINT)
              REAL(KIND=4) :: FUNC
              EXTERNAL FUNC
              REAL(KIND=4) :: X1
              REAL(KIND=4) :: X2
              REAL(KIND=4) :: TOLZ
              REAL(KIND=4) :: EXTRAPARS(75)
              INTEGER(KIND=4) :: EXTRAINT(10)
              REAL(KIND=4) :: ZBRENT
            END FUNCTION ZBRENT
          END INTERFACE 
        END MODULE ZBRENT__genmod
