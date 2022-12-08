    SUBROUTINE obsert(NT, GR00_TAU, GRTT_TAU, GR0T_TAU, GRT0_TAU)
        Use Blockc
        Use Block_obs
        Implicit Real (KIND=8) (A-G,O-Z)
        Implicit Integer (H-N)
        complex(kind=8),dimension(:,:) :: GR00_tau, GRTT_tau, GR0T_tau, GRT0_tau
        complex(kind=8),dimension(:,:), Allocatable :: GR00_TAUd, GRTT_taud, GR0T_taud, GRT0_taud
       
        allocate( GR00_TAUd(NDIM,NDIM), GRTT_taud(NDIM,NDIM), GR0T_taud(NDIM,NDIM), GRT0_taud(NDIM,NDIM))
        
        !G0T_(i,j) =  - <c^{dagger}(tau)_j c_i>
        !GT0_(i,j) = <c_i(tau) c^{dagger}_j>

        
        DO J = 1,Ndim
           XJ = 1.d0
           IF (NList(J,3) == 1 ) XJ= -1.d0
           DO I = 1,Ndim
              XI = 1.d0
              IF (NList(I,3) == 1 ) XI = -1.d0
              GRT0_taud(I,J)  = -cmplx(XI*XJ,0.d0)*CONJG( GR0T_tau(J,I) )
              GR0T_taud(I,J)  = -cmplx(XI*XJ,0.d0)*CONJG( GRT0_tau(J,I) ) 
              GRTT_taud(I,J)  = cmplx(XI*XJ,0.d0)* ( ZKRON(I,J) - CONJG( GRTT_tau(J,I) ) )
              GR00_taud(I,J)  = cmplx(XI*XJ,0.d0)* ( ZKRON(I,J) - CONJG( GR00_tau(J,I) ) )
           Enddo
        Enddo
        ! accoring to this formala of computing spin-down green's function, repulsive hubbard U isn't allowed in this programme

        NT1 = NT + 1
        !	Arguments.
        do LX1 = 1,NLX
            do LX2 = 1,NLX
                do LY1 = 1,NLY
                    do LY2 = 1,NLY
                            lx = npbcx(LX1-LX2)
                            ly = npbcy(LY1-LY2)
                            imj = invlist(lx,ly)
                            do no1 = 1,norb
                                do no2 = 1,norb
                                    i = invnlist(lx1,ly1,no1,1)
                                    j = invnlist(lx2,ly2,no2,1)
                                    green_tau(imj,no1,no2, NT1) = green_tau(imj,no1,no2, NT1) + &
                                    &   ( GRT0_TAU(i, j) + GRT0_TAUD(i, j) )
                                    green1_tau(imj,no1,no2, NT1) = green1_tau(imj,no1,no2, NT1) + &
                                    &   ( -GR0T_TAU(j, i) - GR0T_TAUD(j, i) )
                                    onspair_tau(imj,no1,no2, NT1) = onspair_tau(imj,no1,no2, NT1) + &
                                    &   ( GRT0_TAU(i, j) * GRT0_TAUD(i, j) )
                                    spin_tau(imj,no1,no2, NT1) = spin_tau(imj,no1,no2, NT1) + &
                                    &   ( ( - GRTT_TAU(i, i) + GRTT_TAUD(i, i)) * ( - GR00_TAU(j ,j) + GR00_TAUD(j, j)) &
                                    &   - GR0T_TAU(j, i) * GRT0_TAU(i, j) - GR0T_TAUD(j, i) * GRT0_TAUD(i, j) )
                                    spinpm_tau(imj,no1,no2, NT1) = spinpm_tau(imj,no1,no2, NT1) + &
                                    &   ( - GR0T_TAU(j, i) * GRT0_TAUD(i, j) - GR0T_TAUD(j, i) * GRT0_TAU(i, j) )
                                    den_tau(imj,no1,no2, NT1) = den_tau(imj,no1,no2, NT1) + &
                                    &   ( (GRTT_TAU(i, i) + GRTT_TAUD(i, i)) * (GR00_TAU(j ,j) + GR00_TAUD(j, j)) &
                                    &   - GR0T_TAU(j, i) * GRT0_TAU(i, j) - GR0T_TAUD(j, i) * GRT0_TAUD(i, j) &
                                    &   - GRTT_TAU(i, i) - GRTT_TAUD(i, i) - GR00_TAU(j, j) - GR00_TAUD(j, j) + 1.00)
                                enddo
                            enddo
                    enddo    
                enddo
           enddo
        enddo
                
        deallocate(GR00_TAUd, GRTT_taud, GR0T_taud, GRT0_taud)
        
      END SUBROUTINE OBSERT
      
