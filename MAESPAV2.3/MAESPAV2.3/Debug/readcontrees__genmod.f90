        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:41:00 2016
        MODULE READCONTREES__genmod
          INTERFACE 
            SUBROUTINE READCONTREES(UFILE,NOALLTREES,DX,DY,XMAX,YMAX,   &
     &NOTREESI,NOTARGETSI,ITARGETSI,IPLOTSHAPE,WEIGHTSI)
              INTEGER(KIND=4) :: UFILE
              INTEGER(KIND=4) :: NOALLTREES
              REAL(KIND=4) :: DX(5000)
              REAL(KIND=4) :: DY(5000)
              REAL(KIND=4) :: XMAX
              REAL(KIND=4) :: YMAX
              INTEGER(KIND=4) :: NOTREESI
              INTEGER(KIND=4) :: NOTARGETSI
              INTEGER(KIND=4) :: ITARGETSI(5000)
              INTEGER(KIND=4) :: IPLOTSHAPE
              REAL(KIND=4) :: WEIGHTSI(5000)
            END SUBROUTINE READCONTREES
          END INTERFACE 
        END MODULE READCONTREES__genmod
