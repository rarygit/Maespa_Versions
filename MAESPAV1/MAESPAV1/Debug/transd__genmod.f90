        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:41 2016
        MODULE TRANSD__genmod
          INTERFACE 
            SUBROUTINE TRANSD(IDAY,NEWCANOPY,IPROG,NT,XSLOPE,YSLOPE,NZEN&
     &,DIFZEN,NAZ,NUMPNT,DEXTT,DIFSKY,XL,YL,ZL,RX,RY,RZ,DXT,DYT,DZT,XMAX&
     &,YMAX,SHADEHT,FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,SHAPET, &
     &NEWTUTD,TU,TD,RELDF,DEXT)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: NEWCANOPY
              INTEGER(KIND=4) :: IPROG
              INTEGER(KIND=4) :: NT
              REAL(KIND=4) :: XSLOPE
              REAL(KIND=4) :: YSLOPE
              INTEGER(KIND=4) :: NZEN
              REAL(KIND=4) :: DIFZEN(20)
              INTEGER(KIND=4) :: NAZ
              INTEGER(KIND=4) :: NUMPNT
              REAL(KIND=4) :: DEXTT(4500,20)
              REAL(KIND=4) :: DIFSKY
              REAL(KIND=4) :: XL(4500)
              REAL(KIND=4) :: YL(4500)
              REAL(KIND=4) :: ZL(4500)
              REAL(KIND=4) :: RX(4500)
              REAL(KIND=4) :: RY(4500)
              REAL(KIND=4) :: RZ(4500)
              REAL(KIND=4) :: DXT(4500)
              REAL(KIND=4) :: DYT(4500)
              REAL(KIND=4) :: DZT(4500)
              REAL(KIND=4) :: XMAX
              REAL(KIND=4) :: YMAX
              REAL(KIND=4) :: SHADEHT
              REAL(KIND=4) :: FOLT(4500)
              REAL(KIND=4) :: ZBC(4500)
              INTEGER(KIND=4) :: JLEAFT(4500)
              REAL(KIND=4) :: BPTT(8,3,4500)
              INTEGER(KIND=4) :: NOAGECT(4500)
              REAL(KIND=4) :: PROPCT(3,4500)
              INTEGER(KIND=4) :: JSHAPET(4500)
              REAL(KIND=4) :: SHAPET(4500)
              INTEGER(KIND=4) :: NEWTUTD
              REAL(KIND=4) :: TU(4500)
              REAL(KIND=4) :: TD(4500)
              REAL(KIND=4) :: RELDF(4500)
              REAL(KIND=4) :: DEXT(20)
            END SUBROUTINE TRANSD
          END INTERFACE 
        END MODULE TRANSD__genmod
