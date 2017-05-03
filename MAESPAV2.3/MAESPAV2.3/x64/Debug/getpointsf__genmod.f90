        !COMPILER-GENERATED INTERFACE MODULE: Tue Dec 06 10:14:11 2016
        MODULE GETPOINTSF__genmod
          INTERFACE 
            SUBROUTINE GETPOINTSF(NUMTESTPNT,XL,YL,ZL,X0,Y0,XMAX,YMAX,  &
     &CTITLE,TTITLE,MTITLE,STITLE,VTITLE)
              INTEGER(KIND=4) :: NUMTESTPNT
              REAL(KIND=4) :: XL(5000)
              REAL(KIND=4) :: YL(5000)
              REAL(KIND=4) :: ZL(5000)
              REAL(KIND=4) :: X0
              REAL(KIND=4) :: Y0
              REAL(KIND=4) :: XMAX
              REAL(KIND=4) :: YMAX
              CHARACTER(LEN=256) :: CTITLE
              CHARACTER(LEN=256) :: TTITLE
              CHARACTER(LEN=256) :: MTITLE
              CHARACTER(LEN=256) :: STITLE
              CHARACTER(LEN=256) :: VTITLE
            END SUBROUTINE GETPOINTSF
          END INTERFACE 
        END MODULE GETPOINTSF__genmod
