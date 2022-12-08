        Subroutine  SetH(HLP2)

          Use Blockc
          Use MyMats
          Implicit Real (KIND=8) (A-G,O-Z)
          Implicit Integer (H-N)
          

          Complex (Kind=8), Dimension(:,:) :: HLP2 
          Complex (Kind=8) :: Z1


          !  nearest bond
          IseedHop = 3958195
          HLP2 = CMPLX(0.D0,0.D0)
          do I = 1,LQ
             i_0 = L_Bonds(i,0)
             do nf = 1,Nbond
                i_n = L_Bonds(i,nf)
                random = ranf(IseedHop)
                Z1 = CMPLX(-RT1,0.D0) + TwistX*(random - 0.5d0)
                HLP2(i_0,i_n)  =  Z1
                HLP2(i_n,i_0)  = conjg(Z1)
             enddo
          enddo
        

        end Subroutine SetH
