	SUBROUTINE MMTHLM1(A)

          !	In  A 
          !	Out A* (EXP(-DTAU*T3) * EXP(-DTAU*T2) * EXP(-DTAU*T1))^{-1}
          Use Blockc
          Use MyMats

          Implicit Real (KIND=8) (A-G,O-Z)
          Implicit Integer (H-N)
        
          COMPLEX (Kind=8), Dimension(:,:) :: A
          Complex (Kind=8), Allocatable :: A1(:,:)
 
          N = SIZE(A,1)
          Allocate  ( A1(N,Ndim)   )
          Call mmult(A1, A, URTM1_tot)
          A = A1    
          deallocate ( A1 )   
         
                
	END SUBROUTINE MMTHLM1
