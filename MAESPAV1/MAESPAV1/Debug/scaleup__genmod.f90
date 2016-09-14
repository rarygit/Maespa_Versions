        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:43 2016
        MODULE SCALEUP__genmod
          INTERFACE 
            SUBROUTINE SCALEUP(IHOUR,USESTAND,NOTARGETS,NOALLTREES,FOLT,&
     &ITARGETS,ISPECIES,NOSPEC,TOTLAI,STOCKING,SCLOSTTREE,THRAB,RADABV, &
     &FH2O,PLOTAREA,DOWNTHTREE,RGLOBABV,RGLOBUND,RADINTERC,FRACAPAR,    &
     &ISIMUS,FH2OUS,THRABUS,PARUSMEAN,SCLOSTTOT,GSCAN,WIND,ZHT,Z0HT,ZPD,&
     &PRESS,TAIR,VPD,ETMM,ETUSMM,ETMMSPEC)
              INTEGER(KIND=4) :: IHOUR
              INTEGER(KIND=4) :: USESTAND
              INTEGER(KIND=4) :: NOTARGETS
              INTEGER(KIND=4) :: NOALLTREES
              REAL(KIND=4) :: FOLT(4500)
              INTEGER(KIND=4) :: ITARGETS(4500)
              INTEGER(KIND=4) :: ISPECIES(4500)
              INTEGER(KIND=4) :: NOSPEC
              REAL(KIND=4) :: TOTLAI
              REAL(KIND=4) :: STOCKING
              REAL(KIND=4) :: SCLOSTTREE(4500,3)
              REAL(KIND=4) :: THRAB(4500,96,3)
              REAL(KIND=4) :: RADABV(96,3)
              REAL(KIND=4) :: FH2O(4500,96)
              REAL(KIND=4) :: PLOTAREA
              REAL(KIND=4) :: DOWNTHTREE(4500)
              REAL(KIND=4) :: RGLOBABV
              REAL(KIND=4) :: RGLOBUND
              REAL(KIND=4) :: RADINTERC
              REAL(KIND=4) :: FRACAPAR
              INTEGER(KIND=4) :: ISIMUS
              REAL(KIND=4) :: FH2OUS
              REAL(KIND=4) :: THRABUS
              REAL(KIND=4) :: PARUSMEAN
              REAL(KIND=4) :: SCLOSTTOT
              REAL(KIND=4) :: GSCAN(4500,96)
              REAL(KIND=4) :: WIND
              REAL(KIND=4) :: ZHT
              REAL(KIND=4) :: Z0HT
              REAL(KIND=4) :: ZPD
              REAL(KIND=4) :: PRESS
              REAL(KIND=4) :: TAIR
              REAL(KIND=4) :: VPD
              REAL(KIND=4) :: ETMM
              REAL(KIND=4) :: ETUSMM
              REAL(KIND=4) :: ETMMSPEC(4)
            END SUBROUTINE SCALEUP
          END INTERFACE 
        END MODULE SCALEUP__genmod
