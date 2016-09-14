        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 06 12:06:54 2016
        MODULE INTERPOLATET__genmod
          INTERFACE 
            SUBROUTINE INTERPOLATET(IDAY,ISTART,IHOUR,NOXDATES,DATESX,  &
     &RXTABLE,NOYDATES,DATESY,RYTABLE,NOZDATES,DATESZ,RZTABLE,NOTDATES, &
     &DATEST,ZBCTABLE,NODDATES,DATESD,DIAMTABLE,NOLADATES,DATESLA,      &
     &FOLTABLE,TOTLAITABLE,NOTREES,RX,RY,RZ,ZBC,FOLT,TOTLAI,DIAM,       &
     &STOCKING,IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN,NEWCANOPY,      &
     &CANOPYDIMS)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: ISTART
              INTEGER(KIND=4) :: IHOUR
              INTEGER(KIND=4) :: NOXDATES
              INTEGER(KIND=4) :: DATESX(45)
              REAL(KIND=4) :: RXTABLE(45,5000)
              INTEGER(KIND=4) :: NOYDATES
              INTEGER(KIND=4) :: DATESY(45)
              REAL(KIND=4) :: RYTABLE(45,5000)
              INTEGER(KIND=4) :: NOZDATES
              INTEGER(KIND=4) :: DATESZ(45)
              REAL(KIND=4) :: RZTABLE(45,5000)
              INTEGER(KIND=4) :: NOTDATES
              INTEGER(KIND=4) :: DATEST(45)
              REAL(KIND=4) :: ZBCTABLE(45,5000)
              INTEGER(KIND=4) :: NODDATES
              INTEGER(KIND=4) :: DATESD(45)
              REAL(KIND=4) :: DIAMTABLE(45,5000)
              INTEGER(KIND=4) :: NOLADATES
              INTEGER(KIND=4) :: DATESLA(45)
              REAL(KIND=4) :: FOLTABLE(45,5000)
              REAL(KIND=4) :: TOTLAITABLE(45)
              INTEGER(KIND=4) :: NOTREES
              REAL(KIND=4) :: RX(5000)
              REAL(KIND=4) :: RY(5000)
              REAL(KIND=4) :: RZ(5000)
              REAL(KIND=4) :: ZBC(5000)
              REAL(KIND=4) :: FOLT(5000)
              REAL(KIND=4) :: TOTLAI
              REAL(KIND=4) :: DIAM(5000)
              REAL(KIND=4) :: STOCKING
              INTEGER(KIND=4) :: IFLUSH
              REAL(KIND=4) :: DT1
              REAL(KIND=4) :: DT2
              REAL(KIND=4) :: DT3
              REAL(KIND=4) :: DT4
              REAL(KIND=4) :: EXPTIME
              REAL(KIND=4) :: APP
              REAL(KIND=4) :: EXPAN
              INTEGER(KIND=4) :: NEWCANOPY
              REAL(KIND=4) :: CANOPYDIMS(6)
            END SUBROUTINE INTERPOLATET
          END INTERFACE 
        END MODULE INTERPOLATET__genmod
