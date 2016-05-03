        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 28 17:30:21 2016
        MODULE TREDST__genmod
          INTERFACE 
            SUBROUTINE TREDST(IFLAG,IPROG,IRAD,DZ,DAZ,XPT,YPT,ZPT,RX,RY,&
     &RZ,DXT,DYT,DZT,FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,SHAPET,&
     &EXTC,NT,S,S1,EFFK,EXTCANG,EFFKANG)
              INTEGER(KIND=4) :: IFLAG
              INTEGER(KIND=4) :: IPROG
              INTEGER(KIND=4) :: IRAD
              REAL(KIND=4) :: DZ
              REAL(KIND=4) :: DAZ
              REAL(KIND=4) :: XPT
              REAL(KIND=4) :: YPT
              REAL(KIND=4) :: ZPT
              REAL(KIND=4) :: RX(5000)
              REAL(KIND=4) :: RY(5000)
              REAL(KIND=4) :: RZ(5000)
              REAL(KIND=4) :: DXT(5000)
              REAL(KIND=4) :: DYT(5000)
              REAL(KIND=4) :: DZT(5000)
              REAL(KIND=4) :: FOLT(5000)
              REAL(KIND=4) :: ZBC(5000)
              INTEGER(KIND=4) :: JLEAFT(5000)
              REAL(KIND=4) :: BPTT(8,3,5000)
              INTEGER(KIND=4) :: NOAGECT(5000)
              REAL(KIND=4) :: PROPCT(3,5000)
              INTEGER(KIND=4) :: JSHAPET(5000)
              REAL(KIND=4) :: SHAPET(5000)
              REAL(KIND=4) :: EXTC(5000)
              INTEGER(KIND=4) :: NT
              REAL(KIND=4) :: S
              REAL(KIND=4) :: S1
              REAL(KIND=4) :: EFFK
              REAL(KIND=4) :: EXTCANG(5000,20)
              REAL(KIND=4) :: EFFKANG(20)
            END SUBROUTINE TREDST
          END INTERFACE 
        END MODULE TREDST__genmod
