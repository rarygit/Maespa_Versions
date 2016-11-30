        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:40:54 2016
        MODULE GETMETHR__genmod
          INTERFACE 
            SUBROUTINE GETMETHR(IDATE,ZEN,NOMETCOLS,METCOLS,CAK,PRESSK, &
     &SWMIN,SWMAX,DELTAT,ALAT,DEC,DAYL,WINDAH,TSOIL,TAIR,RADABV,FBEAM,RH&
     &,VPD,VMFD,CA,PRESS,PPT,SOILMOIST,SOILDATA,TSOILDATA,ETMEAS,EMSKY)
              INTEGER(KIND=4) :: IDATE
              REAL(KIND=4) :: ZEN(96)
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
              REAL(KIND=4) :: WINDAH(96)
              REAL(KIND=4) :: TSOIL(96)
              REAL(KIND=4) :: TAIR(96)
              REAL(KIND=4) :: RADABV(96,3)
              REAL(KIND=4) :: FBEAM(96,3)
              REAL(KIND=4) :: RH(96)
              REAL(KIND=4) :: VPD(96)
              REAL(KIND=4) :: VMFD(96)
              REAL(KIND=4) :: CA(96)
              REAL(KIND=4) :: PRESS(96)
              REAL(KIND=4) :: PPT(96)
              REAL(KIND=4) :: SOILMOIST(96)
              INTEGER(KIND=4) :: SOILDATA
              INTEGER(KIND=4) :: TSOILDATA
              REAL(KIND=4) :: ETMEAS(96)
              REAL(KIND=4) :: EMSKY(96)
            END SUBROUTINE GETMETHR
          END INTERFACE 
        END MODULE GETMETHR__genmod
