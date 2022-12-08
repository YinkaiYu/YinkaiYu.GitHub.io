    SUBROUTINE OBSER
        Use Blockc
        Use Block_obs
        Use MyMats
        Implicit Real (KIND=8) (A-G,O-Z)
        Implicit Integer (H-N)
        
        complex(kind=8) :: Z1
        COMPLEX (Kind=8), Dimension(:) , Allocatable :: TMP_D, TMP_S
        COMPLEX (Kind=8), Dimension(:,:) , Allocatable :: GRDO, GRDOC
!#define DEC
        allocate( TMP_D(Ndim), TMP_S(NDIM),GRDO(NDIM,NDIM),GRDOC(NDIM,NDIM) )
        
        do I = 1,NDIM
           XI = 1.d0
           if (NList(I,3) == 1 ) XI = -1.d0
           do J = 1,NDIM
              XJ = 1.d0
              if (NList(J,3) == 1 ) XJ= -1.d0
              GRDO (I,J) = CMPLX(XI*XJ, 0.d0) * CONJG ( GRUPC(I,J) )
              GRDOC(I,J) = CMPLX(XI*XJ, 0.d0) * CONJG ( GRUP (I,J) )
           enddo
        enddo
        ! accoring to this formala of computing spin-down green's function, repulsive hubbard U isn't allowed in this programme
        
        do LX1 = 1, NLX
            do LY1 = 1, NLY
                II = invlist(LX1,LY1)
                i0 = L_bonds(II,0)
                i1 = L_bonds(II,1)
                i2 = L_bonds(II,2)
                I3 = L_bonds(II,3)
                
                density = density + real(GRUPC(I0,I0)+GRDOC(I0,I0)+GRUPC(I1,I1)+GRDOC(I1,I1))/dble(2*LQ)
                double_ocupy = double_ocupy + real( GRUPC(I0,I0)*GRDOC(I0,I0)+GRUPC(I1,I1)*GRDOC(I1,I1))/dble(2*LQ)
                kinetic = kinetic + (-RT1)*real(GRUPC(I0,I1)+GRUPC(I0,I2)+GRUPC(I0,I3)) + &
                &  (-RT1)*real(GRUPC(I1,I0)+GRUPC(I2,I0)+GRUPC(I3,I0))
            enddo
        enddo
        
        
        do i = 1, NDIM
            TMP_D(i) = GRUPC(i, i) + GRdoC(i, i)
            TMP_S(i) = GRUPC(i, i) - GRdoC(i, i)
        enddo
        
        do ix = 1, nlx
            do  iy = 1, nly
                i = invlist(ix,iy)
                do no1 = 1,norb
                    do jx = 1, nlx
                        do jy = 1, nly                  
                            j = invlist(jx,jy)
                            do no2 = 1,norb
                                II = invnlist(ix,iy,no1,1)
                                JJ = invnlist(jx,jy,no2,1)
                                imj1 = npbcx(ix-jx)
                                imj2 = npbcy(iy-jy)
                                imj = invlist(imj1,imj2)
                                den(imj,no1,no2) = den(imj,no1,no2) + &
                                &   ( TMP_D(ii)* TMP_D(jj) + GRUPC(ii, jj)* GRUP(ii, jj) + GRdoC(ii, jj)* GRdo(ii, jj) - TMP_D(ii) - TMP_D(jj) + 1.00)/dble(LQ)
                                spin(imj,no1,no2) = spin(imj,no1,no2) + &
                                &   ( TMP_S(ii)* TMP_S(jj) + GRUPC(ii, jj)* GRUP(ii, jj) + GRdoC(ii, jj)* GRdo(ii, jj) )/dble(LQ)
                                spinpm(imj,no1,no2) = spinpm(imj,no1,no2) + &
                                &   ( GRUPC(ii, jj)* GRdo(ii, jj) + GRdoC(ii, jj)* GRUP(ii, jj) )/dble(LQ)
                                onswave(imj,no1,no2) = onswave(imj,no1,no2) + & ! on-site s-wave pairing
                                &   ( GRUP(ii, jj)* GRdo(ii, jj) )/dble(LQ)
                            enddo
                        enddo
                    enddo        
                enddo    
            enddo
        enddo
                        
        deallocate(TMP_D, TMP_S, GRDO, GRDOC)
     
      END SUBROUTINE OBSER
