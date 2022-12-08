   Program SuNF
   Use Blockc
   Use Block_obs
! Use Block_tau
   Use MyMats
   Implicit Real (KIND=8) (A-G,O-Z)
   Implicit Integer (H-N)
! T=0 SU(N) t-U-J model. Flux phases t-U-J model. With large-N, Honeycomb lattice.
! L is the linear size of square lattice.
! LQ is the number of unit cells.
! Norb number of orbitals in unit cell.
! Ndim = Norb*LQ
! Trotter break-up. There are 6 families.
! e^(J_3) e^(J_2) e^(J_1) e^(K_3) e^(K_2) e^(K_1) e^(T)
! Since observables are rather expensive to compute we will calculate
! them on a maximum of LTROT_ME time slices.
   Interface
      SUBROUTINE MMUUR(A, NF, NT ,NFLAG)
        COMPLEX (Kind=8), Dimension(:,:) :: A
        INTEGER :: NF, NT, NFLAG
      END SUBROUTINE MMUUR
      SUBROUTINE MMUURM1(A, NF, NTAU, NFLAG)
        COMPLEX (KIND=8), Dimension(:,:) :: A
        Integer :: NF, NTAU, NFLAG
      END SUBROUTINE MMUURM1
      SUBROUTINE MMUUL(A, NF,NTAU,NFLAG)
        COMPLEX (Kind=8), Dimension(:,:) :: A
        Integer :: NF,NTAU,NFLAG
      END SUBROUTINE MMUUL
      SUBROUTINE MMUULM1(A, NF, NTAU, NFLAG)
        COMPLEX (KIND=8), Dimension(:,:) :: A
        Integer :: NF, NTAU, NFLAG
      END SUBROUTINE MMUULM1
      SUBROUTINE ORTHO(A,I)
        COMPLEX (Kind=8), Dimension(:,:) :: A
        INTEGER :: I
      END SUBROUTINE ORTHO
      SUBROUTINE CALCGR(UL, UR, ULRINV)     
        COMPLEX (Kind=8), Dimension(:,:) :: UL, UR, ULRINV
      END SUBROUTINE CALCGR
      SUBROUTINE UPGRADEU(NTAU,ISEED,  UL,UR, ULRINV)
        COMPLEX (Kind=8), Dimension(:,:) :: UL, UR, ULRINV
        INTEGER :: NTAU, ISEED
      END SUBROUTINE UPGRADEU
      SUBROUTINE UPGRADEJ(NTAU,NF,ISEED,UL,UR,ULRINV)
        COMPLEX (Kind=8), Dimension(:,:) :: UL, UR, ULRINV
        Integer :: NTAU,NF,ISEED
      END SUBROUTINE UPGRADEJ
      SUBROUTINE MMTHR(A)
        COMPLEX (Kind=8), Dimension(:,:) :: A
      END SUBROUTINE MMTHR
      SUBROUTINE MMTHRM1(A)
        COMPLEX (Kind=8), Dimension(:,:) :: A
      END SUBROUTINE MMTHRM1
      SUBROUTINE MMTHL(A)
        COMPLEX (Kind=8), Dimension(:,:) :: A
      END SUBROUTINE MMTHL
      SUBROUTINE MMTHLM1(A)
        COMPLEX (Kind=8), Dimension(:,:) :: A
      END SUBROUTINE MMTHLM1
      SUBROUTINE DYN( UST, UL, UR, ULR,ULRINV, XMEAN_DYN, XMAX_DYN)
         Complex (Kind=8), Dimension(:,:,:) :: UST
         Complex (Kind=8), Dimension(:,:) :: UL, UR, ULR, ULRINV
         Real (Kind=8) :: XMEAN_DYN, XMAX_DYN
      END SUBROUTINE DYN
   END Interface
!#define DEC
   INCLUDE 'mpif.h'
   INTEGER STATUS(MPI_STATUS_SIZE)
! Space for storage.
   COMPLEX (KIND=8), Dimension(:,:), Allocatable :: UL ,UR , ULR, ULRINV
   COMPLEX (KIND=8), Dimension(:,:), Allocatable :: UL_1,UR_1, ULRINV_1,ULR1,ULRINV1
   COMPLEX (KIND=8), Dimension(:,:,:),Allocatable :: UST
   COMPLEX (KIND=8) :: DETZ
   COMPLEX (Kind=8) :: DET1(2)
   NCON = 0
   CALL MPI_INIT(IERR)
   CALL MPI_COMM_SIZE(MPI_COMM_WORLD,ISIZE,IERR)
   CALL MPI_COMM_RANK(MPI_COMM_WORLD,IRANK,IERR)
   IF (IRANK == 0) THEN
      OPEN (UNIT=50,FILE='info',STATUS='UNKNOWN')
      OPEN ( UNIT=20, FILE='paramC_sets',STATUS='UNKNOWN' )
      READ(20,*) BETA, LTROT, LTROT_quench, NWRAP, RT1, RHUB1, RHUB2,  RJ 
      READ(20,*) NBIN, NSWEEP, LTAU, NTDM
      READ(20,*) NLX, NLY, Itwist, TwistX, N_SUN, NE
      CLOSE(20)
   ENDIF
      L_Trot_hop = .false.
      NB_Field = 0
   CALL MPI_BCAST(BETA ,1,MPI_REAL8,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(RJ ,1,MPI_REAL8,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(RHUB1 ,1,MPI_REAL8,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(RHUB2 ,1,MPI_REAL8,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(RT1 ,1,MPI_REAL8,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(TwistX ,1,MPI_REAL8,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(Itwist ,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(NLX ,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(NLY ,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(NE ,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(LTROT ,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(LTROT_quench ,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(NWRAP ,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(N_SUN ,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(NBIN ,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(NSWEEP ,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(LTAU ,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_BCAST(NTDM ,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL Allocate_Blockc
   CALL Allocate_obs( LQ, Norb, LTROT )
   IF (LTAU == 1) THEN
      CALL Allocate_obs_tau(LQ,Norb,LTROT)
      ALLOCATE( UL_1(NE,Ndim), UR_1(Ndim,NE), ULRINV_1(NE,NE) )
   ENDIF
   NST = LTROT/NWRAP
   ALLOCATE(UL (NE,NDIM),UR(NDIM,NE), ULR(NE,NE), ULRINV(NE,NE), UST(NDIM,NE,NST))
   ALLOCATE(ULR1(NE,NE),ULRINV1(NE,NE))
! Write parameters.
   IF (IRANK == 0) THEN
      WRITE(50,*) '========================='
      WRITE(50,*) 'SU(N) t-U-J, Projector '
      WRITE(50,*) 'Linear lenght    :',NLX,NLY
      WRITE(50,*) 'N: SU(N)         :',N_SUN
      WRITE(50,*) 'Particle #       :',NE
      WRITE(50,*) 'Hopping t        :',RT1
      WRITE(50,*) 'Twist            :', Itwist, TwistX
      WRITE(50,*) 'J                :',RJ
      WRITE(50,*) 'Hubbard          :',RHUB1, RHUB2
      WRITE(50,*) 'Theta            :',BETA
      WRITE(50,*) 'Trotter number   :',LTROT 
      WRITE(50,*) 'Quench number   :',LTROT_quench
      WRITE(50,*) '=>Dtau           :',DTAU
      WRITE(50,*) 'N_Ortho          :',NWRAP
      WRITE(50,*) '# Bins           :',NBIN
      WRITE(50,*) '# Sweeps/Bin     :',NSWEEP
      WRITE(50,*) 'Measuring on the ',LTROT_ME,'  slice'
      IF (LTAU == 1) THEN
         WRITE(50,*) 'TAU_max            :', DBLE(NTDM)*DTAU
         WRITE(50,*) 'Effective proj.    :', DBLE(NTAUIN)*DTAU
      ENDIF
      WRITE(50,*) '# of families    :', NFAM
      WRITE(50,*) 'Length of fam.   :', LFAM
      WRITE(50,*) 'Twist in x-direction phi/phi_0 : ', TwistX
      if ( L_Trot_hop ) then
         Write(50,*) ' Trotter decomposition for hopping'
      else
         Write(50,*) ' No Trotter decomposition for hopping'
      endif
      WRITE(50,*) '# Nodes         :',ISIZE
   ENDIF
! Check parameters.
   IF (LTAU == 1) THEN
      IF (MOD(NTAUIN,NWRAP).NE.0) THEN
         WRITE(50,*) 'NTAUIN is not a multiple of NOFRE'
         STOP
      ENDIF
      IF (MOD(NTDM,NWRAP).NE.0) THEN
         WRITE(50,*) 'NTDM is not a multiple of NOFRE'
         STOP
      ENDIF
   ENDIF
! Setup lists and hopping matrix.
   CALL SLI ! Setup lattice
   RHUB = RHUB1
   CALL SALPH ! Calculate DTAU and ALPHA - precalc factors for updates & measurements
   CALL SPROJ(DEGEN,EN_FREE) ! Setup Projector PROJ and TWF, compute charge gap/degeneracy
   CALL STHOP ! Setup & exponentiate hopping matrix
   IF (IRANK == 0) THEN
      WRITE(50,*) 'Trial: Degen, En:  ', DEGEN, EN_FREE
   ENDIF
   CALL INCONFC(ISEED) ! Setup random sigma configuration or read it from unit 10
   CALL SYSTEM_CLOCK(COUNT_RATE=N_P_SEC)
   CALL SYSTEM_CLOCK(COUNT=ICPU_1)
!-------Fill up right storage and set the phase.
   DO NL = 1,NE
      DO I = 1,Ndim
         UR(I,NL) = PROJ(I,NL)
      ENDDO
   ENDDO
   DO NT = 1,LTROT
      RHUB = RHUB1
      if(nt<= LTROT_ME.and.nt>LTROT_ST) RHUB = RHUB1 + (RHUB2-RHUB1)* dble(nt-LTROT_ST)/dble(LTROT_quench)
      if(nt> LTROT_ME.and.nt<LTROT_EN) RHUB = RHUB1 + (RHUB2-RHUB1)*dble(LTROT_EN-nt)/dble(LTROT_quench)
      CALL SALPH 
      CALL MMTHR(UR)
      IF (RJ > Zero) THEN
         DO NF = 1,NFAM
            NFLAG = 2
            CALL MMUUR(UR, NF, NT, NFLAG)
            NFLAG = 1
            CALL MMUUR(UR, NF, NT, NFLAG)
         ENDDO
      ENDIF
      if(abs(RHUB)>zero) then
          NFLAG = 3 ! Hubbard
          CALL MMUUR(UR, NF, NT, NFLAG)
      endif
      IF (MOD(NT,NWRAP) == 0) THEN
         CALL ORTHO(UR,NCON)
! Write in Store (NT,NF+1) with ortho.
         NT_ST = NT/NWRAP
         DO NL = 1,NE
            DO NR = 1,Ndim
               UST(NR,NL,NT_ST) = UR(NR,NL)
            ENDDO
         ENDDO
      ENDIF
   ENDDO
! UR is now on time slice LTROT.
! Set UL
   DO NL = 1,NE
      DO I = 1,Ndim
         UL(NL,I) = DCONJG(PROJ(I,NL))
      ENDDO
   ENDDO
! Set phase.
   CALL MMULT (ULR,UL,UR)
   CALL INV (ULR,ULRINV,DET1)

! Test is OK.
!************** Start Sweeps. *********************************
   XMAXP = 0.D0
   ACC_JK = 0.D0
   ACC_U = 0.D0
   AXMAX_DYN = 0.D0
   XMEAN_DYN= 0.D0
   DO NBC = 1, NBIN
! Here, you have the green functions on time slice 1.
! Set bin observables to zero.
      CALL Init_Obs
      If (Ltau == 1) Call Init_obs_tau
      OBS = CMPLX(0.d0,0.d0)
      NOBS = 0
      NOBST = 0
      SIGNT = 0.D0
      DO NSW = 1, NSWEEP
! Set UL, UST is full with right propagations.
         DO NL = 1,NE
            DO I = 1,Ndim
               UL(NL,I) = DCONJG(PROJ(I,NL))
            ENDDO
         ENDDO
         DO NT = LTROT, 1, -1
! UR, UL on time slice Ltrot.
             RHUB = RHUB1
             if(nt<= LTROT_ME.and.nt>LTROT_ST) RHUB = RHUB1 + (RHUB2-RHUB1)* dble(nt-LTROT_ST)/dble(LTROT_quench)
             if(nt> LTROT_ME.and.nt<LTROT_EN) RHUB = RHUB1 + (RHUB2-RHUB1)*dble(LTROT_EN-nt)/dble(LTROT_quench)
             CALL SALPH          
             IF (MOD(NT,NWRAP) == 0 ) THEN
! Read UR.              
                 NT_ST = NT/NWRAP               
                 DO NL = 1,NE                 
                     DO I = 1,Ndim                   
                         UR(I,NL) = UST(I,NL,NT_ST)               
                     ENDDO          
                 ENDDO
! Ortho of UL and recalc of ulrinv
                IF (NT.NE.LTROT) CALL ORTHO(UL,NCON)
                CALL MMULT(ULR,UL,UR)
                CALL INV (ULR,ULRINV,DET1)
! Store UL.
                NT_ST = NT/NWRAP
                DO NL = 1,NE
                    DO I = 1,Ndim
                        UST(I,NL,NT_ST) = UL(NL,I)
                    ENDDO  
                ENDDO
             ENDIF
! Update Auxiliary field for Hubbard U   
             IF (abs(RHUB) > ZERO) CALL UPGRADEU(NT,ISEED, UL,UR, ULRINV)
! Compute observables   
             IF (NT == LTROT_ME) THEN
                    Call CALCGR(UL,UR,ULRINV)
                    CALL OBSER
                    NOBS = NOBS + 1   
             ENDIF
! Propagate UL and UR by multiplying U-matrix              
             if(abs(RHUB)>zero) then                  
                 NFLAG = 3 ! Hubbard                  
                 CALL MMUUL ( UL,NF,NT,NFLAG )                 
                 CALL MMUURM1( UR,NF,NT,NFLAG )               
             endif                
             if (abs(RJ) > Zero) THEN                   
                 DO NF = NFAM,1,-1                       
                     NFLAG = 2                       
                     CALL MMUUL (UL,NF,NT,NFLAG)                       
                     CALL MMUURM1(UR,NF,NT,NFLAG)                       
                     CALL UPGRADEJ(NT,NF,ISEED,UL,UR,ULRINV)                       
                     NFLAG = 1                       
                     CALL MMUUL (UL,NF,NT,NFLAG)                   
                     CALL MMUURM1(UR,NF,NT,NFLAG)                 
                 ENDDO               
             endif                  
             CALL MMTHL (UL)           
             CALL MMTHRM1(UR)   
         ENDDO
! Set UR, UST is full with left propagations. UL is on time slice 0.
         UR(1:Ndim,1:NE) = PROJ(1:Ndim,1:NE)
         CALL ORTHO(UL,NCON)
         CALL MMULT(ULR,UL,UR)
         CALL INV (ULR,ULRINV,DET1)

         DO NT = 1,LTROT
! You start the loop on time slice NT-1
             RHUB = RHUB1    
             if(nt<= LTROT_ME.and.nt>LTROT_ST) RHUB = RHUB1 + (RHUB2-RHUB1)* dble(nt-LTROT_ST)/dble(LTROT_quench)
             if(nt> LTROT_ME.and.nt<LTROT_EN) RHUB = RHUB1 + (RHUB2-RHUB1)*dble(LTROT_EN-nt)/dble(LTROT_quench)
             CALL SALPH 
             CALL MMTHR (UR)
             CALL MMTHLM1(UL)
        
             IF (abs(RJ) > Zero) THEN
                 DO NF = 1,NFAM
                     NFLAG = 2
                     CALL MMUUR (UR, NF, NT, NFLAG)
                     CALL MMUULM1 (UL, NF, NT, NFLAG)
                     CALL UPGRADEJ(NT,NF,ISEED,UL,UR,ULRINV)
                     NFLAG = 1
                     CALL MMUUR (UR, NF, NT, NFLAG)
                     CALL MMUULM1(UL, NF, NT, NFLAG)
                 ENDDO
             ENDIF               
             
             if(abs(RHUB)>zero) then               
                 NFLAG = 3 ! Hubbard
                 CALL MMUUR (UR, NF, NT, NFLAG)
                 CALL MMUULM1(UL, NF, NT, NFLAG)
             endif           
             IF (abs(RHUB) > ZERO) CALL UPGRADEU(NT,ISEED, UL,UR, ULRINV)            
             
             IF (NT == LTROT_ME) THEN
                 Call CALCGR(UL,UR,ULRINV)
                 CALL OBSER
                 NOBS = NOBS + 1
             ENDIF
            
             IF (MOD(NT,NWRAP) == 0) THEN
                 NT_ST = NT/NWRAP
                 DO NL = 1,NE
                     DO NR = 1,Ndim
                         UL(NL,NR) = UST(NR,NL,NT_ST)
                     ENDDO
                 ENDDO
                 CALL ORTHO(UR,NCON)
                 CALL MMULT(ULR,UL,UR)
                 CALL INV (ULR,ULRINV,DET1)           
                 DO NL = 1,NE             
                     DO NR = 1,Ndim                  
                         UST(NR,NL,NT_ST) = UR(NR,NL)                 
                     ENDDO          
                 ENDDO
             ENDIF
            
             IF (LTAU == 1 .AND. NT == NTAUIN ) THEN        
                 UL_1 = UL
                 UR_1 = UR
                 ULRINV_1 = ULRINV
                 CALL DYN(UST,  UL_1, UR_1, ULR,ULRINV_1, XMEAN_DYN, XMAX_DYN)
                 NOBST = NOBST + 1
             ENDIF
         ENDDO
      ENDDO
      CALL PREQ (NOBS,NBC)
      IF (LTAU == 1) THEN
         CALL PRTAU(NOBST)
      ENDIF
      IF (DBLE(OBS(30)) > ZERO) ACC_JK = ACC_JK + DBLE(OBS(29)/OBS(30))
      IF (DBLE(OBS(28)) > ZERO) ACC_U = ACC_U + DBLE(OBS(27)/OBS(28))
   ENDDO
   CALL OUTCONFC(ISEED)

!!$ IF (LTAU == 1) XMEAN_DYN = XMEAN_DYN/(DBLE(NOBST)*DBLE(NTDM/NWRAP))
   XMAX_DYN1 = 0.0
   CALL MPI_REDUCE(XMAX_DYN,XMAX_DYN1,1,MPI_REAL8,MPI_MAX,0,MPI_COMM_WORLD,IERR)
   XMAX_DYN = XMAX_DYN1
   XMEAN_DYN1 = 0.D0
   CALL MPI_REDUCE(XMEAN_DYN,XMEAN_DYN1,1,MPI_REAL8,MPI_SUM,0,MPI_COMM_WORLD,IERR)
   XMEAN_DYN = XMEAN_DYN1/DBLE(ISIZE)
   CALL MPI_REDUCE(XMAXP,XMAXP_1,1,MPI_REAL8,MPI_MAX,0,MPI_COMM_WORLD,IERR)
   XMAXP = XMAXP_1
   CALL SYSTEM_CLOCK(COUNT=ICPU_2)
   CPUT = 0.D0
   CPUT = DBLE(ICPU_2 - ICPU_1)/DBLE(N_P_SEC)
   CPUT_1 = 0.D0
   CALL MPI_REDUCE(CPUT,CPUT_1,1,MPI_REAL8,MPI_SUM,0,MPI_COMM_WORLD,IERR)
   CPUT = CPUT_1/DBLE(ISIZE)
   IF (IRANK == 0) THEN
      WRITE(50,*) ' Max  diff Ph :', XMAXP
      IF (LTAU == 1) WRITE(50,*) ' Dynamics Mean,Max: ', XMEAN_DYN, XMAX_DYN
      WRITE(50,*) ' Accep_J    :',ACC_JK/DBLE(NBIN)
      WRITE(50,*) ' Accep_U    :',ACC_U /DBLE(NBIN)
      WRITE(50,*) 'Tot CPU time:', CPUT
   ENDIF
   CALL MPI_FINALIZE(IERR)
    END PROGRAM SuNF
