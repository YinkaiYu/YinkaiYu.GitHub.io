	SUBROUTINE MMUURM1(A, NF, NTAU,  NFLAG)
          !	In A Out U(NF) * A                if NFLAG = 1
          !	In A Out EXP(D(NF)) * UT(NF) * A  if NFLAG = 2

          Use Blockc
          Implicit Real (KIND=8) (A-G,O-Z)
          Implicit Integer (H-N)
          COMPLEX (KIND=8), Dimension(:,:) :: A
          Integer :: NF, NTAU,  NFLAG

          !Local
          COMPLEX (KIND=8), Dimension(:), Allocatable ::  V1, V2
          COMPLEX (KIND=8) :: UT(2,2), U(2,2)

          N = Size(A,2)

          Allocate (V1(N), V2(N))
             
          !          Kinetic.
          NF1 = NF
          DO I = 1,2
              DO J = 1,2
                  U(I,J)  = UR_K (I,J)
                  UT(I,J) = URT_K(I,J)
              ENDDO   
          ENDDO
          !WRITE(6,*) 'MMUUR: ',NF, NFLAG, NF1, NN

          IF (NFLAG.EQ.3.and.RHUB.GT.zero) THEN
             DO NL= 1,N
                DO I = 1,Ndim
                   A(I,NL) =  A(I,NL) / XSIGMA_U( NSIGL_U(I,NTAU))
                ENDDO
             ENDDO
          ENDIF
          
          IF (NFLAG.EQ.2 .AND.  RJ .GT. ZERO ) THEN
             DO I = 1,LFAM
                I1 = L_Bonds(I,0  )
                I2 = L_Bonds(I,nf1)
                DO J = 1,N
                   V1(J)   =  UT(1,1) * A(I1,J) + UT(1,2) * A(I2,J)
                   V2(J)   =  UT(2,1) * A(I1,J) + UT(2,2) * A(I2,J) 
                ENDDO
                DO J = 1,N
                   A(I1,J) = V1(J)
                   A(I2,J) = V2(J)
                ENDDO
                
             ENDDO
          ENDIF
          
          IF (NFLAG.EQ.1 .AND. RJ .GT. ZERO) THEN
             DO I = 1,LFAM
                I1 = L_Bonds(I,0  )
                I2 = L_Bonds(I,nf1)
                ! Kinetic 
                DO J = 1,N
                    A(I1,J) =  A(I1,J) / DCMPLX(XSIGP2(NSIGL_K(I,Nf1,NTAU)),0.D0)   
                    A(I2,J) =  A(I2,J) / DCMPLX(XSIGM2(NSIGL_K(I,Nf1,NTAU)),0.D0)
                ENDDO
                DO J = 1,N
                   V1(J)   =  U(1,1) * A(I1,J) +  U(1,2) * A(I2,J) 
                   V2(J)   =  U(2,1) * A(I1,J) +  U(2,2) * A(I2,J) 
                ENDDO
                DO J = 1,N
                   A(I1,J) = V1(J)
                   A(I2,J) = V2(J)
                ENDDO
             ENDDO
          ENDIF
          
          Deallocate (V1, V2)
          
          Return
    END SUBROUTINE MMUURM1
        
