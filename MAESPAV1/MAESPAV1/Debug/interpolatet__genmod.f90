        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:46 2016
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
              INTEGER(KIND=4) :: DATESX(30)
              REAL(KIND=4) :: RXTABLE(30,4500)
              INTEGER(KIND=4) :: NOYDATES
              INTEGER(KIND=4) :: DATESY(30)
              REAL(KIND=4) :: RYTABLE(30,4500)
              INTEGER(KIND=4) :: NOZDATES
              INTEGER(KIND=4) :: DATESZ(30)
              REAL(KIND=4) :: RZTABLE(30,4500)
              INTEGER(KIND=4) :: NOTDATES
              INTEGER(KIND=4) :: DATEST(30)
              REAL(KIND=4) :: ZBCTABLE(30,4500)
              INTEGER(KIND=4) :: NODDATES
              INTEGER(KIND=4) :: DATESD(30)
              REAL(KIND=4) :: DIAMTABLE(30,4500)
              INTEGER(KIND=4) :: NOLADATES
              INTEGER(KIND=4) :: DATESLA(30)
              REAL(KIND=4) :: FOLTABLE(30,4500)
              REAL(KIND=4) :: TOTLAITABLE(30)
              INTEGER(KIND=4) :: NOTREES
              REAL(KIND=4) :: RX(4500)
              REAL(KIND=4) :: RY(4500)
              REAL(KIND=4) :: RZ(4500)
              REAL(KIND=4) :: ZBC(4500)
              REAL(KIND=4) :: FOLT(4500)
              REAL(KIND=4) :: TOTLAI
              REAL(KIND=4) :: DIAM(4500)
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
