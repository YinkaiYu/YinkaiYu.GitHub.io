      SUBROUTINE UPGRADEU(NTAU,ISEED, UL,UR, ULRINV)

        Use Blockc
        Implicit none
        !Arguments.
        COMPLEX (Kind=8), Dimension(:,:) :: UL, UR, ULRINV
        INTEGER :: NTAU, ISEED
        
        !Space for tests
        COMPLEX (Kind=8), Dimension(:), Allocatable ::  V1, U1, VHLP1, UHLP1
        COMPLEX (Kind=8)  G44UP, RATIOUP, RATIOTOT, DENOM, DEL44
        
        ! Local
        Real (Kind=8) :: ACCM, Ratio_Re_abs,  Ratio_Re, Random, Weight
        Real (Kind=8), external :: Ranf
        Integer, external :: nranf
        Integer :: I4, nrflip, NL, NL1, NL2
        
 
        ALLOCATE(V1(NE), U1(NE), VHLP1(NE), UHLP1(NE))

	ACCM  = 0.D0
	DO I4 = 1,Ndim
	   nrflip = nranf(iseed,3) 

	   DEL44   =  DELTA_U( NSIGL_U(I4,NTAU), NRFLIP )
	   
	   DO NL = 1,NE
	      VHLP1(NL) = DEL44*UR(I4,NL)
	      UHLP1(NL) = UL(NL,I4)
	   ENDDO
	   DO NL = 1,NE
	      V1(NL) = DCMPLX(0.D0,0.D0)
	      U1(NL) = DCMPLX(0.D0,0.D0)
	   ENDDO
	   DO NL = 1,NE
	      DO NL1 = 1,NE
		 V1(NL) = V1(NL) + VHLP1(NL1)*ULRINV(NL1,NL)
	      ENDDO
	   ENDDO
	   G44UP = DCMPLX(0.D0,0.D0)
	   DO NL = 1,NE
	      G44UP = G44UP + V1(NL)*UHLP1(NL)    
       ENDDO

	   RATIOUP = (DCMPLX(1.D0,0.D0) + G44UP)* DETA_U( NSIGL_U(I4,NTAU), NRFLIP )

	   RATIOTOT = abs( RATIOUP)**(DBLE(N_SUN))
           !if (I4 == NDim/2 ) Write(6,*) RATIOTOT
	   
	   RATIO_RE = DGAML(NSIGL_U(I4,NTAU),NRFLIP )*&
                &   DBLE( RATIOTOT  )

	   ! WEIGHT_TMP = DGAML(NSIGL_U(I4,NTAU),NRFLIP )*SQRT(DBLE(RATIOTOT*DCONJG(RATIOTOT)))
           ! X  = ABS(RATIO_RE - WEIGHT_TMP) 
           ! write(6,*) X

	   RATIO_RE_ABS = RATIO_RE
	   IF (RATIO_RE .LT. 0.D0 )  RATIO_RE_ABS = - RATIO_RE 
           Random =  RANF(ISEED)
	   !Write(6,*) Random, nrflip
           IF ( RATIO_RE_ABS.GT.Random ) THEN
           ! IF (  WEIGHT_TMP .GT. Random ) THEN
              !	 WRITE(50,*) 'Accepted!'
	      ! Upgrade the inverse.

	      ACCM  = ACCM + 1.D0
              ! Upgrade Phase.
	      WEIGHT = SQRT(DBLE(RATIOTOT*CONJG(RATIOTOT)))

	      DO NL  = 1,NE
              DO NL1 = 1,NE
                  U1(NL) = U1(NL) + ULRINV(NL,NL1)*UHLP1(NL1)
              ENDDO
	      ENDDO

	      DENOM =   DCMPLX(1.D0,0.D0)/ (DCMPLX(1.D0,0.D0) + G44UP)
	      DO NL = 1,NE
	         V1(NL) = V1(NL)*DENOM
	      ENDDO 

	      DO NL1 = 1,NE
              DO NL2 = 1,NE
                  ULRINV(NL1,NL2) = ULRINV(NL1,NL2)  -   U1(NL1)*V1(NL2)
              ENDDO
          ENDDO
              ! Upgrade  UR
              
          DO NL = 1,NE
              UR(I4,NL) = UR(I4,NL) +    DEL44*UR(I4,NL)    
          ENDDO

              !	      Flip:              
          NSIGL_U(I4,NTAU) =  NFLIPL(NSIGL_U(I4,NTAU), NRFLIP)

           ENDIF
	   
           !WRITE(50,*)
	   
	ENDDO
	OBS(27) = OBS(27) + DCMPLX(ACCM/DBLE(Ndim),0.D0)
	OBS(28) = OBS(28) + DCMPLX(1.D0,0.D0)
	!WRITE(6,*) 'UpgradeU: Acc: ', ACCM/DBLE(Ndim)

        DEALLOCATE(V1, U1, VHLP1, UHLP1)
	 
      END SUBROUTINE UPGRADEU
