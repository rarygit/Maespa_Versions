        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:30 2016
        MODULE READXYZ__genmod
          INTERFACE 
            SUBROUTINE READXYZ(UFILE,NOALLTREES,X0,Y0,XMAX,YMAX,XSLOPE, &
     &YSLOPE,DX,DY,DZ)
              INTEGER(KIND=4) :: UFILE
              INTEGER(KIND=4) :: NOALLTREES
              REAL(KIND=4) :: X0
              REAL(KIND=4) :: Y0
              REAL(KIND=4) :: XMAX
              REAL(KIND=4) :: YMAX
              REAL(KIND=4) :: XSLOPE
              REAL(KIND=4) :: YSLOPE
              REAL(KIND=4) :: DX(5000)
              REAL(KIND=4) :: DY(5000)
              REAL(KIND=4) :: DZ(5000)
            END SUBROUTINE READXYZ
          END INTERFACE 
        END MODULE READXYZ__genmod
