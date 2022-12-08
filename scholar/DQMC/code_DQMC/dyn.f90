      SUBROUTINE DYN( UST, UL, UR, ULR,ULRINV, XMEAN_DYN, XMAX_DYN)
        Use Blockc
        Use Block_obs
        Use MyMats
        Implicit Real (KIND=8) (A-G,O-Z)
        Implicit Integer (H-N)
!#define DEC
        Interface
           SUBROUTINE CALCGR( UL, UR, ULRINV)
             COMPLEX (Kind=8), Dimension(:,:) :: UL, UR, ULRINV
           END SUBROUTINE CALCGR
           SUBROUTINE PROPR (GT0UP,NT1)
             COMPLEX (Kind=8), Dimension(:,:) :: GT0UP
             INTEGER :: NT1
           END SUBROUTINE PROPR
           SUBROUTINE PROPRM1(GT0UP,NT1)
             COMPLEX (Kind=8), Dimension(:,:) :: GT0UP
             INTEGER :: NT1
           END SUBROUTINE PROPRM1
           SUBROUTINE MMTHR(A)
             COMPLEX (Kind=8), Dimension(:,:) :: A
           END SUBROUTINE MMTHR
           SUBROUTINE MMUUR(A, NF, NT, NFLAG)
             COMPLEX (Kind=8), Dimension(:,:) :: A
             INTEGER :: NF, NT, NFLAG
           END SUBROUTINE MMUUR
           SUBROUTINE ORTHO(A,I)
             COMPLEX (Kind=8), Dimension(:,:) :: A
             INTEGER :: I
           END SUBROUTINE ORTHO
           SUBROUTINE OBSERT(NT,GT0UP,G0TUP,GTTUP,G00UP)
             COMPLEX (Kind=8), Dimension(:,:) :: G00UP,G0TUP,GT0UP,GTTUP
             Integer :: NT
           END SUBROUTINE OBSERT
        End Interface
        ! Storage is full with U^{<} (left) propagations.
        Complex (Kind=8), Dimension(:,:,:) :: UST
        Complex (Kind=8), Dimension(:,:) :: UL, UR, ULR, ULRINV
        Real (Kind=8) :: XMEAN_DYN, XMAX_DYN
! Local.
        Complex (Kind=8) :: DETZ, ZK, DET1(2)
        Complex (Kind=8), Dimension(:,:), Allocatable :: GRUPB, G00UP, G0TUP, &
             & GT0UP, GTTUP, TEMP
        ALLOCATE ( GRUPB(Ndim,Ndim),  G00UP(Ndim,Ndim), G0TUP(Ndim,Ndim), &
             & GT0UP(Ndim,Ndim), GTTUP(Ndim,Ndim), TEMP(Ndim,Ndim) )
        ! WRITE(6,*) 'Starting Dyn'
        DO NT = NTAUIN, NTAUIN + NTDM -1
           ! UR is on time slice NT
           NTAU = NT - NTAUIN
           IF ( MOD(NT,NWRAP).EQ.0) THEN
              NTAU = NT - NTAUIN
              NT_ST = NT/NWRAP
              IF (NTAU.GT.0) THEN
                 DO NL = 1,NE
                    DO I = 1,Ndim
                       UL(NL,I) = UST(I,NL,NT_ST)
                    ENDDO
                 ENDDO
              ENDIF
              CALL MMULT(ULR,UL,UR)
              CALL INV (ULR,ULRINV,DET1)
              ! Compute Green functions.
              CALL CALCGR(UL, UR, ULRINV)
              !write(6,*) 'computed Green: ', NT
              !You have: GRUP (I,J) = <c_i c^+_j >
              DO I = 1,Ndim
              DO J = 1,Ndim
                 ZK = DCMPLX(0.D0,0.D0)
                 IF (I.EQ.J) ZK = DCMPLX(1.D0,0.D0)
                 GRUPB(I,J) = ZK - GRUP(I,J)
              ENDDO
              ENDDO
              IF (NTAU.EQ.0) THEN
                 G00UP = GRUP
                 GTTUP = GRUP
                 GT0UP = GRUP
                 G0TUP = GRUPB
                 NT1 = 0
                 CALL OBSERT (NT1,GT0UP,G0TUP,GTTUP,G00UP)
              ELSE
                 XMAX = 0.D0
                 XMEAN = 0.D0
                 CALL COMPARE (GTTUP, GRUP, XMAX,XMEAN)
                 IF (XMAX.GT.XMAX_DYN) XMAX_DYN = XMAX
                 XMEAN_DYN = XMEAN_DYN + XMEAN
                 GTTUP = GRUP
                 CALL MMULT(TEMP,GRUP,GT0UP)
                 GT0UP = TEMP
                 CALL MMULT(TEMP,G0TUP,GRUPB)
                 G0TUP = TEMP
              ENDIF
           ENDIF ! Ortho.
           ! Now propagate to Ntau + 1 and call OBSERT.
           NT1 = NT + 1
           CALL PROPR (GT0UP,NT1)
           CALL PROPRM1(G0TUP,NT1)
           CALL PROPRM1(GTTUP,NT1)
           CALL PROPR (GTTUP,NT1)
           NTAU1 = NT1 - NTAUIN
           ! WRITE(6,*) 'Dyn: calling obsetT: ', NTAU1
           CALL OBSERT (NTAU1,GT0UP,G0TUP, GTTUP,G00UP)
           ! Wrap UR.
           CALL MMTHR(UR)
           If (RJ > Zero) then
              DO NF = 1,NFAM
                 NFLAG = 2
                 CALL MMUUR(UR, NF, NT1, NFLAG)
                 NFLAG = 1
                 CALL MMUUR(UR, NF, NT1, NFLAG)
              ENDDO
           Endif
           NFLAG = 3 ! Hubbard
           CALL MMUUR(UR, NF, NT1, NFLAG)
           IF (MOD(NT1,NWRAP).EQ.0 .AND. NT1.NE. (NTAUIN + NTDM) ) THEN
              NCON = 0
              CALL ORTHO(UR,NCON)
              !Write(6,*) 'Dyn Ortho on: ', NTAUIN, NTAUIN+NTDM, NT1
           ENDIF
        ENDDO
        DEALLOCATE (GRUPB, G00UP, G0TUP, GT0UP, GTTUP, TEMP )
        RETURN
      END SUBROUTINE DYN
