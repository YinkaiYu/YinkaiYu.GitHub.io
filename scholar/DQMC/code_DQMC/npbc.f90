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
