        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:40:58 2016
        MODULE TRANSB__genmod
          INTERFACE 
            SUBROUTINE TRANSB(IHOUR,IPROG,ZENITH,AZMTH,XSLOPE,YSLOPE,   &
     &FBEAM,BEXTT,XPT,YPT,ZPT,RX,RY,RZ,DXT,DYT,DZT,XMAX,YMAX,SHADEHT,   &
     &FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,SHAPET,NT,SLA,BEXT,   &
     &BEXTANGT,BEXTANG)
              INTEGER(KIND=4) :: IHOUR
              INTEGER(KIND=4) :: IPROG
              REAL(KIND=4) :: ZENITH
              REAL(KIND=4) :: AZMTH
              REAL(KIND=4) :: XSLOPE
              REAL(KIND=4) :: YSLOPE
              REAL(KIND=4) :: FBEAM(96,3)
              REAL(KIND=4) :: BEXTT(5000)
              REAL(KIND=4) :: XPT
              REAL(KIND=4) :: YPT
              REAL(KIND=4) :: ZPT
              REAL(KIND=4) :: RX(5000)
              REAL(KIND=4) :: RY(5000)
              REAL(KIND=4) :: RZ(5000)
              REAL(KIND=4) :: DXT(5000)
              REAL(KIND=4) :: DYT(5000)
              REAL(KIND=4) :: DZT(5000)
              REAL(KIND=4) :: XMAX
              REAL(KIND=4) :: YMAX
              REAL(KIND=4) :: SHADEHT
              REAL(KIND=4) :: FOLT(5000)
              REAL(KIND=4) :: ZBC(5000)
              INTEGER(KIND=4) :: JLEAFT(5000)
              REAL(KIND=4) :: BPTT(8,3,5000)
              INTEGER(KIND=4) :: NOAGECT(5000)
              REAL(KIND=4) :: PROPCT(3,5000)
              INTEGER(KIND=4) :: JSHAPET(5000)
              REAL(KIND=4) :: SHAPET(5000)
              INTEGER(KIND=4) :: NT
              REAL(KIND=4) :: SLA
              REAL(KIND=4) :: BEXT
              REAL(KIND=4) :: BEXTANGT(5000,20)
              REAL(KIND=4) :: BEXTANG(20)
            END SUBROUTINE TRANSB
          END INTERFACE 
        END MODULE TRANSB__genmod
