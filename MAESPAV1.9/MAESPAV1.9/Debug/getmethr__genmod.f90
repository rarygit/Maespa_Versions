        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:31 2016
        MODULE GETMETHR__genmod
          INTERFACE 
            SUBROUTINE GETMETHR(IDATE,ZEN,NOMETCOLS,METCOLS,CAK,PRESSK, &
     &SWMIN,SWMAX,DELTAT,ALAT,DEC,DAYL,WINDAH,TSOIL,TAIR,RADABV,FBEAM,RH&
     &,VPD,VMFD,CA,PRESS,PPT,SOILMOIST,SOILDATA,TSOILDATA,ETMEAS)
              INTEGER(KIND=4) :: IDATE
              REAL(KIND=4) :: ZEN(300)
              INTEGER(KIND=4) :: NOMETCOLS
              INTEGER(KIND=4) :: METCOLS(21)
              REAL(KIND=4) :: CAK
              REAL(KIND=4) :: PRESSK
              REAL(KIND=4) :: SWMIN
              REAL(KIND=4) :: SWMAX
              REAL(KIND=4) :: DELTAT(12)
              REAL(KIND=4) :: ALAT
              REAL(KIND=4) :: DEC
              REAL(KIND=4) :: DAYL
              REAL(KIND=4) :: WINDAH(300)
              REAL(KIND=4) :: TSOIL(300)
              REAL(KIND=4) :: TAIR(300)
              REAL(KIND=4) :: RADABV(300,3)
              REAL(KIND=4) :: FBEAM(300,3)
              REAL(KIND=4) :: RH(300)
              REAL(KIND=4) :: VPD(300)
              REAL(KIND=4) :: VMFD(300)
              REAL(KIND=4) :: CA(300)
              REAL(KIND=4) :: PRESS(300)
              REAL(KIND=4) :: PPT(300)
              REAL(KIND=4) :: SOILMOIST(300)
              INTEGER(KIND=4) :: SOILDATA
              INTEGER(KIND=4) :: TSOILDATA
              REAL(KIND=4) :: ETMEAS(300)
            END SUBROUTINE GETMETHR
          END INTERFACE 
        END MODULE GETMETHR__genmod
