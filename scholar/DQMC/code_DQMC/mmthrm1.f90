	SUBROUTINE MMTHRM1(A)

          !In  A 
          !Out EXP(DTAU*T1)*EXP( DTAU*T2) * EXP( DTAU*T3) * A

          Use Blockc
          Use  MyMats
          
          Implicit Real (KIND=8) (A-G,O-Z)
          Implicit Integer (H-N)

          !Local
          COMPLEX (Kind=8), Dimension(:,:) ::  A
          Complex (Kind=8), Allocatable :: A1(:,:)
          
          N = SIZE(A,2)
          Allocate  ( A1(Ndim,N)       )
          Call Mmult( A1, URTM1_tot, A ) 
          A = A1    
          deallocate (A1)
          
        END SUBROUTINE MMTHRM1
        
