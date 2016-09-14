        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:41 2016
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
              REAL(KIND=4) :: RX(4500)
              REAL(KIND=4) :: RY(4500)
              REAL(KIND=4) :: RZ(4500)
              REAL(KIND=4) :: DXT(4500)
              REAL(KIND=4) :: DYT(4500)
              REAL(KIND=4) :: DZT(4500)
              REAL(KIND=4) :: FOLT(4500)
              REAL(KIND=4) :: ZBC(4500)
              INTEGER(KIND=4) :: JLEAFT(4500)
              REAL(KIND=4) :: BPTT(8,3,4500)
              INTEGER(KIND=4) :: NOAGECT(4500)
              REAL(KIND=4) :: PROPCT(3,4500)
              INTEGER(KIND=4) :: JSHAPET(4500)
              REAL(KIND=4) :: SHAPET(4500)
              REAL(KIND=4) :: EXTC(4500)
              INTEGER(KIND=4) :: NT
              REAL(KIND=4) :: S
              REAL(KIND=4) :: S1
              REAL(KIND=4) :: EFFK
              REAL(KIND=4) :: EXTCANG(4500,20)
              REAL(KIND=4) :: EFFKANG(20)
            END SUBROUTINE TREDST
          END INTERFACE 
        END MODULE TREDST__genmod
