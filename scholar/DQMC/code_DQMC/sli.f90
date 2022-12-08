    SUBROUTINE SLI
        Use Blockc
        Implicit Real (KIND=8) (A-G,O-Z)
        Implicit Integer (H-N)
     
        LIST = 0; INVLIST = 0; NNLIST = 0

! nlist: Index_site -> (nx,ny)
        NCOUNT = 0
	    DO NX = 1,NLX
	        DO NY = 1,NLY
	            NCOUNT = NCOUNT + 1
	            LIST(NCOUNT,1) = NX
	            LIST(NCOUNT,2) = NY
	            INVLIST(NX,NY) = NCOUNT
	        ENDDO
        ENDDO

! nlist: Index_dimen -> (nx,ny,n_orbital, n_spin)
	    NCOUNT = 0	
        DO nx = 1, NLX
            DO ny = 1, NLY
                DO NO = 1, Norb
                    DO NS = 1 ,nspin
                        ncount = ncount + 1
                        NLIST(NCOUNT,1) = NX
                        NLIST(NCOUNT,2) = NY
                        NLIST(NCOUNT,3) = NO
                        NLIST(NCOUNT,4) = NS
                        INVNLIST(NX,NY,NO,NS) = NCOUNT
                    ENDDO
                ENDDO
            ENDDO
        ENDDO
	
!define the nearest neighbor bonds
         do ly = 1, NLY
            do lx = 1, NLX               
            ll = invlist(lx,ly)
            L_bonds(ll,0) = invnlist(lx,ly,1,1)
            L_bonds(ll,1) = invnlist(lx,ly,2,1)
            lln1 = invnlist(npbcx(lx+1),ly,2,1)
            L_bonds(ll,2) = lln1
            lln2 = invnlist(lx,npbcy(ly+1),2,1)
            L_bonds(ll,3) = lln2
            enddo
         enddo

! define the unite matrix
        ZKRON = DCMPLX(0.D0,0.D0)
	    DO I = 1,NDIM
	        ZKRON(I,I) = DCMPLX(1.D0,0.D0)
	    ENDDO
        
	    RETURN
    END SUBROUTINE SLI
    
    integer FUNCTION Iscalar(vec1,vec2)
        Use Blockc
        Iscalar = vec1(1)*vec2(1) + vec1(2)*vec2(2)       
        return    
    end function Iscalar
    
      
    INTEGER FUNCTION NPBCX(NR)
        use blockc
        NPBCX = NR
        IF (NR.GT.NLX) NPBCX = NR - NLX
        IF (NR.LT.1) NPBCX = NR + NLX
        RETURN
    END FUNCTION NPBCX
    
    INTEGER FUNCTION NPBCY(NR)
        use blockc
        NPBCY = NR
        IF (NR.GT.NLY) NPBCY = NR - NLY
        IF (NR.LT.1) NPBCY = NR + NLY
        RETURN
    END FUNCTION NPBCY
    
     
    INTEGER FUNCTION NPBC_tau(NR)
        use blockc
        NPBC_tau = NR
        IF (NR.GT.LTROT) NPBC_tau = NR - LTROT
        IF (NR.LT.1) NPBC_tau = NR + LTROT
        RETURN  
    END FUNCTION NPBC_tau


    