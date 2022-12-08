	SUBROUTINE MMUUL(A, NF,NTAU,NFLAG)
	
          !In A  Out A* EXP(D(NF)) * UT(NF)  if NFLAG = 1
          !In A  Out A* U(NF)                if NFLAG = 2

          Use Blockc
          Implicit Real (KIND=8) (A-G,O-Z)
          Implicit Integer (H-N)

          !Arguments:
          COMPLEX (Kind=8), Dimension(:,:) :: A
          Integer :: NF,NTAU,NFLAG

          !	Local
          COMPLEX (Kind=8), Dimension(:),  Allocatable :: V1, V2
          COMPLEX (Kind=8) :: UT(2,2), U(2,2)

          N = Size(A,1)
          ALLOCATE (V1(N), V2(N))
          IF (NFLAG.EQ.3.and.RHUB.GT.zero) THEN
             DO NL = 1,N
                DO J  = 1,Ndim
                   A(NL,J) = A(NL,J) * XSIGMA_U(NSIGL_U(J,NTAU))
                ENDDO
             ENDDO
          ENDIF
          
          !Kinetic.
          NF1 = NF
          DO I = 1,2
              DO J = 1,2
                  U(I,J)  = UR_K (I,J)
                  UT(I,J) = URT_K(I,J)
              ENDDO   
          ENDDO
          
          IF (NFLAG.EQ.2 .AND. RJ.GT.ZERO) THEN
             DO I = 1,LFAM
                I1 = L_bonds(I,0  ) 
                I2 = L_bonds(I,nf1) 
                DO J = 1,N
                   V1(J)   =  A(J,I1) * U(1,1) + A(J,I2) * U(2,1) 
                   V2(J)   =  A(J,I1) * U(1,2) + A(J,I2) * U(2,2) 
                ENDDO
                DO J = 1,N
                   A(J,I1) = V1(J)
                   A(J,I2) = V2(J)
                ENDDO
                
             ENDDO
          ENDIF
          
          IF (NFLAG.EQ.1 .AND. RJ.GT. ZERO) THEN
             DO I = 1,LFAM
                I1 = L_bonds(I,0  ) 
                I2 = L_bonds(I,nf1) 
                ! Kenitic
                DO J = 1,N
                    A(J,I1) = DCMPLX(XSIGP2(NSIGL_K(I,nf1,NTAU)),0.D0)*A(J,I1)
                    A(J,I2) = DCMPLX(XSIGM2(NSIGL_K(I,nf1,NTAU)),0.D0)*A(J,I2)   
                ENDDO
                DO J = 1,N
                   V1(J) = A(J,I1) * UT(1,1) +  A(J,I2) * UT(2,1)
                   V2(J) = A(J,I1) * UT(1,2) +  A(J,I2) * UT(2,2)
                ENDDO
                DO J = 1,N
                   A(J,I1) = V1(J)
                   A(J,I2) = V2(J)
                ENDDO
             ENDDO
          ENDIF

          DEALLOCATE (V1, V2)

          RETURN
    END SUBROUTINE MMUUL
    
