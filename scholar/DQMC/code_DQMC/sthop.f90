        SUBROUTINE STHOP
        
          Use Blockc
          use MyMats
          Implicit Real (KIND=8) (A-G,O-Z)
          Implicit Integer (H-N)
          Interface 
             Subroutine  SetH(HLP2)
               Complex (Kind=8), Dimension(:,:) :: HLP2
             end Subroutine SetH
            end Interface
          
          Parameter (NCH = 2)
          COMPLEX (kind=8) ::  Z, Z0, Z1
          COMPLEX (Kind=8), allocatable ::  HLP2(:,:), HLP1(:,:)
          REAL    (Kind=8), allocatable ::   WC(:)
          Integer Info
          
             
          Allocate ( HLP2(NDIM,NDIM), HLP1(NDIM,NDIM), WC(NDIM)  )
          Call SetH(HLP2)
          Call DIAG(HLP2,HLP1,WC)
          DO I = 1,Ndim
              DO J = 1,Ndim
                  Z0 = DCMPLX(0.D0,0.D0)
                  Z1 = DCMPLX(0.D0,0.D0)
                  DO M = 1,Ndim
                      Z0 = Z0 +  HLP1(I,M) * &
                      &   DCMPLX(EXP(-DTAU *WC(M)),0.D0) * &
                      &    DCONJG(HLP1(J,M))
                      Z1 = Z1 +  HLP1(I,M)  *&
                      &  DCMPLX(EXP( DTAU *WC(M)),0.D0) * &
                      &   DCONJG(HLP1(J,M))
                  ENDDO
                  URT_tot  (I,J) = Z0
                  URTM1_tot(I,J) = Z1
              ENDDO
          ENDDO
          deallocate ( HLP2, HLP1, WC  )
          RETURN    
    END SUBROUTINE STHOP
        
