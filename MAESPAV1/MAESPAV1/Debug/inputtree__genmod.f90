        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:46 2016
        MODULE INPUTTREE__genmod
          INTERFACE 
            SUBROUTINE INPUTTREE(XSLOPE,YSLOPE,BEAR,X0,Y0,XMAX,YMAX,    &
     &PLOTAREA,STOCKING,ZHT,Z0HT,ZPD,NOALLTREES,NOTREES,NOTARGETS,      &
     &ITARGETS,SHADEHT,NOXDATES,NOYDATES,NOZDATES,NOTDATES,NOLADATES,   &
     &NODDATES,DATESX,DATESY,DATESZ,DATEST,DATESLA,DATESD,DX,DY,DZ,R1,R2&
     &,R3,TRUNK,FLT,TOTLAITABLE,DIAMA,IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,APP&
     &,EXPAN,WEIGHTS,NSPECIES,ISPECIES)
              REAL(KIND=4) :: XSLOPE
              REAL(KIND=4) :: YSLOPE
              REAL(KIND=4) :: BEAR
              REAL(KIND=4) :: X0
              REAL(KIND=4) :: Y0
              REAL(KIND=4) :: XMAX
              REAL(KIND=4) :: YMAX
              REAL(KIND=4) :: PLOTAREA
              REAL(KIND=4) :: STOCKING
              REAL(KIND=4) :: ZHT
              REAL(KIND=4) :: Z0HT
              REAL(KIND=4) :: ZPD
              INTEGER(KIND=4) :: NOALLTREES
              INTEGER(KIND=4) :: NOTREES
              INTEGER(KIND=4) :: NOTARGETS
              INTEGER(KIND=4) :: ITARGETS(4500)
              REAL(KIND=4) :: SHADEHT
              INTEGER(KIND=4) :: NOXDATES
              INTEGER(KIND=4) :: NOYDATES
              INTEGER(KIND=4) :: NOZDATES
              INTEGER(KIND=4) :: NOTDATES
              INTEGER(KIND=4) :: NOLADATES
              INTEGER(KIND=4) :: NODDATES
              INTEGER(KIND=4) :: DATESX(30)
              INTEGER(KIND=4) :: DATESY(30)
              INTEGER(KIND=4) :: DATESZ(30)
              INTEGER(KIND=4) :: DATEST(30)
              INTEGER(KIND=4) :: DATESLA(30)
              INTEGER(KIND=4) :: DATESD(30)
              REAL(KIND=4) :: DX(4500)
              REAL(KIND=4) :: DY(4500)
              REAL(KIND=4) :: DZ(4500)
              REAL(KIND=4) :: R1(30,4500)
              REAL(KIND=4) :: R2(30,4500)
              REAL(KIND=4) :: R3(30,4500)
              REAL(KIND=4) :: TRUNK(30,4500)
              REAL(KIND=4) :: FLT(30,4500)
              REAL(KIND=4) :: TOTLAITABLE(30)
              REAL(KIND=4) :: DIAMA(30,4500)
              INTEGER(KIND=4) :: IFLUSH
              REAL(KIND=4) :: DT1
              REAL(KIND=4) :: DT2
              REAL(KIND=4) :: DT3
              REAL(KIND=4) :: DT4
              REAL(KIND=4) :: EXPTIME
              REAL(KIND=4) :: APP
              REAL(KIND=4) :: EXPAN
              REAL(KIND=4) :: WEIGHTS(4500)
              INTEGER(KIND=4) :: NSPECIES
              INTEGER(KIND=4) :: ISPECIES(4500)
            END SUBROUTINE INPUTTREE
          END INTERFACE 
        END MODULE INPUTTREE__genmod
