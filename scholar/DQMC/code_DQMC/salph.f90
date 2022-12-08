	SUBROUTINE SALPH
!       Calculate DTAU and ALPHA

          Use Blockc
          Implicit Real (KIND=8) (A-G,O-Z)
          Implicit Integer (H-N)

          REAL (Kind=8) ::  XTH(4)
          COMPLEX (Kind = 8) :: Z, ALPHA_U
          COMPLEX (Kind = 8) :: UHLP(4,4)


          !For J term.
          ETAL(-2)= - SQRT(2.D0 * ( 3.D0 + SQRT(6.D0) ) )
          ETAL(-1)= - SQRT(2.D0 * ( 3.D0 - SQRT(6.D0) ) )
          ETAL( 1)=   SQRT(2.D0 * ( 3.D0 - SQRT(6.D0) ) )
          ETAL( 2)=   SQRT(2.D0 * ( 3.D0 + SQRT(6.D0) ) )
          
          GAML(-2)= 1.D0 - SQRT(6.D0)/3.D0
          GAML( 2)= 1.D0 - SQRT(6.D0)/3.D0
          GAML(-1)= 1.D0 + SQRT(6.D0)/3.D0
          GAML( 1)= 1.D0 + SQRT(6.D0)/3.D0
          
          ALPHA  =  SQRT(DTAU*RJ/(DBLE(N_SUN)*4.D0))
          
          ! Same for current and for kinetic.
          XSIGP2(-2) = EXP( 1.D0* ALPHA * ETAL(-2) )
          XSIGP2(-1) = EXP( 1.D0* ALPHA * ETAL(-1) )
          XSIGP2( 1) = EXP( 1.D0* ALPHA * ETAL( 1) )
          XSIGP2( 2) = EXP( 1.D0* ALPHA * ETAL( 2) )
          
          XSIGM2(-2) = EXP(-1.D0* ALPHA * ETAL(-2) )
          XSIGM2(-1) = EXP(-1.D0* ALPHA * ETAL(-1) )
          XSIGM2( 1) = EXP(-1.D0* ALPHA * ETAL( 1) )
          XSIGM2( 2) = EXP(-1.D0* ALPHA * ETAL( 2) )
                 
          ! For flipping 
          NFLIPL(-2,1) = -1
          NFLIPL(-2,2) =  1
          NFLIPL(-2,3) =  2
          
          NFLIPL(-1,1) =  1
          NFLIPL(-1,2) =  2
          NFLIPL(-1,3) = -2
          
          NFLIPL( 1,1) =  2
          NFLIPL( 1,2) = -2
          NFLIPL( 1,3) = -1
          
          NFLIPL( 2,1) = -2
          NFLIPL( 2,2) = -1
          NFLIPL( 2,3) =  1
          
          DO NL = -2,2
             IF (NL.NE.0) THEN
                DO I  = 1,3
                   NLN = NFLIPL(NL,I)
                   DELLP2(NL,I) =  (XSIGP2(NLN)/XSIGP2(NL)) - 1.D0
                   DELLM2(NL,I) =  (XSIGM2(NLN)/XSIGM2(NL)) - 1.D0                
                   DGAML (NL,I) =  GAML(NLN)/GAML(NL)
                ENDDO
             ENDIF
          ENDDO
          
          !	Pair-hopping. Kinetic.
          DO M = 1,2
             DO N = 1,2
                UR_K(M,N) = DCMPLX(0.D0,0.D0)
             ENDDO
          ENDDO
          UR_K(1,1) =   DCMPLX( 1.D0/SQRT(2.D0),0.D0 )
          UR_K(1,2) =   DCMPLX( 1.D0/SQRT(2.D0),0.D0 )
          UR_K(2,1) =   DCMPLX( 1.D0/SQRT(2.D0),0.D0 )
          UR_K(2,2) =   DCMPLX(-1.D0/SQRT(2.D0),0.D0 )
          
          DO M = 1,2
             DO N = 1,2
                URT_K(M,N) = DCONJG(UR_K(N,M))
             ENDDO
          ENDDO
          
          
          !****** For Hubbard. 
          if (RHUB .ge. 0.d0) then
              ALPHA_U  = CMPLX(0.d0,SQRT(DTAU*RHUB/(DBLE(N_SUN))))
          else if (RHUB .le. 0.d0) then
              ALPHA_U  = CMPLX(SQRT(DTAU*abs(RHUB)/DBLE(N_SUN)),0.D0)
          end if

          XSIGMA_U(-2) = EXP(ALPHA_U*DCMPLX( ETAL(-2),0.D0))
          XSIGMA_U(-1) = EXP(ALPHA_U*DCMPLX( ETAL(-1),0.D0))
          XSIGMA_U( 1) = EXP(ALPHA_U*DCMPLX( ETAL( 1),0.D0))
          XSIGMA_U( 2) = EXP(ALPHA_U*DCMPLX( ETAL( 2),0.D0))
          
          RHO = 0.5
          DO NL = -2,2
             IF (NL.NE.0) THEN
                DO I  = 1,3
                   NLN = NFLIPL(NL,I)
                   DELTA_U(NL,I)=(XSIGMA_U(NLN)/XSIGMA_U(NL)) -  DCMPLX(1.D0,0.D0)
                   DETA_U (NL,I)=  EXP(-ALPHA_U*DCMPLX(ETAL(NLN)*RHO,0.D0)) / EXP(-ALPHA_U*DCMPLX(ETAL(NL )*RHO,0.D0)) 
                ENDDO
             ENDIF
          ENDDO
          
          RETURN
        END SUBROUTINE SALPH
