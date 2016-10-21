        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:28 2016
        MODULE POINTSNEW2__genmod
          INTERFACE 
            SUBROUTINE POINTSNEW2(NOLAY,PPLAY,JLEAF,JSHAPE,SHAPE,RXNTR, &
     &RYNTR,RZNTR,ZBCNTR,DXTNTR,DYTNTR,DZTNTR,FOLNTR,PROPC,PROPP,BPT,   &
     &NOAGEC,NOAGEP,XL,YL,ZL,VL,DLT,DLI,LGP,FOLLAY)
              INTEGER(KIND=4) :: NOLAY
              INTEGER(KIND=4) :: PPLAY
              INTEGER(KIND=4) :: JLEAF
              INTEGER(KIND=4) :: JSHAPE
              REAL(KIND=4) :: SHAPE
              REAL(KIND=4) :: RXNTR
              REAL(KIND=4) :: RYNTR
              REAL(KIND=4) :: RZNTR
              REAL(KIND=4) :: ZBCNTR
              REAL(KIND=4) :: DXTNTR
              REAL(KIND=4) :: DYTNTR
              REAL(KIND=4) :: DZTNTR
              REAL(KIND=4) :: FOLNTR
              REAL(KIND=4) :: PROPC(3)
              REAL(KIND=4) :: PROPP(3)
              REAL(KIND=4) :: BPT(8,3)
              INTEGER(KIND=4) :: NOAGEC
              INTEGER(KIND=4) :: NOAGEP
              REAL(KIND=4) :: XL(5000)
              REAL(KIND=4) :: YL(5000)
              REAL(KIND=4) :: ZL(5000)
              REAL(KIND=4) :: VL(5000)
              REAL(KIND=4) :: DLT(5000)
              REAL(KIND=4) :: DLI(3,5000)
              INTEGER(KIND=4) :: LGP(5000)
              REAL(KIND=4) :: FOLLAY(15)
            END SUBROUTINE POINTSNEW2
          END INTERFACE 
        END MODULE POINTSNEW2__genmod
