        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:23 2016
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
              INTEGER(KIND=4) :: ITARGETS(5000)
              REAL(KIND=4) :: SHADEHT
              INTEGER(KIND=4) :: NOXDATES
              INTEGER(KIND=4) :: NOYDATES
              INTEGER(KIND=4) :: NOZDATES
              INTEGER(KIND=4) :: NOTDATES
              INTEGER(KIND=4) :: NOLADATES
              INTEGER(KIND=4) :: NODDATES
              INTEGER(KIND=4) :: DATESX(1000)
              INTEGER(KIND=4) :: DATESY(1000)
              INTEGER(KIND=4) :: DATESZ(1000)
              INTEGER(KIND=4) :: DATEST(1000)
              INTEGER(KIND=4) :: DATESLA(1000)
              INTEGER(KIND=4) :: DATESD(1000)
              REAL(KIND=4) :: DX(5000)
              REAL(KIND=4) :: DY(5000)
              REAL(KIND=4) :: DZ(5000)
              REAL(KIND=4) :: R1(1000,5000)
              REAL(KIND=4) :: R2(1000,5000)
              REAL(KIND=4) :: R3(1000,5000)
              REAL(KIND=4) :: TRUNK(1000,5000)
              REAL(KIND=4) :: FLT(1000,5000)
              REAL(KIND=4) :: TOTLAITABLE(1000)
              REAL(KIND=4) :: DIAMA(1000,5000)
              INTEGER(KIND=4) :: IFLUSH
              REAL(KIND=4) :: DT1
              REAL(KIND=4) :: DT2
              REAL(KIND=4) :: DT3
              REAL(KIND=4) :: DT4
              REAL(KIND=4) :: EXPTIME
              REAL(KIND=4) :: APP
              REAL(KIND=4) :: EXPAN
              REAL(KIND=4) :: WEIGHTS(5000)
              INTEGER(KIND=4) :: NSPECIES
              INTEGER(KIND=4) :: ISPECIES(5000)
            END SUBROUTINE INPUTTREE
          END INTERFACE 
        END MODULE INPUTTREE__genmod
