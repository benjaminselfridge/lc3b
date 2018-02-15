        0x3400

        ;; 1) test unconditional branch.
START:  BR LBR
        BR DONE

        ;; 3) test branch zero.
LBRp:   XOR R1 R1 R1
        BRp DONE
        BRn DONE
        BRz LBRz

        ;; 6.1) test branch np.
LBRnz2: ADD R3 R3 0x1a
        BRz DONE
        BRnp LBRnp1
        
        ;; 2) test branch positive.
LBR:    ADD R0 R0 0x1
        BRn DONE
        BRz DONE
        BRnz DONE
        BRp LBRp

        ;; 5.2) test branch nz.
LBRnz1: XOR R2 R2 R2
        BRp DONE
        BRnz LBRnz2

        ;; done
LBRzp2: ADD R6 R6 0xb
        BRnzp DONE
        
        ;; 6.2) test branch np.
LBRnp1: ADD R4 R4 0xa
        BRz DONE
        BRnp LBRnp2
        
        ;; 5.1) test branch nz.
LBRn:   ADD R2 R2 0x1f
        BRp DONE
        BRnz LBRnz1

        ;; 7.2) test branch zp
LBRzp1: ADD R3 R4 0x1f
        BRn DONE
        BRzp LBRzp2
        
        ;; 4) test branch negative.
LBRz:   ADD R1 R1 0x1f          ; R1 should be -1.
        BRp DONE
        BRz DONE
        BRn LBRn

        ;; 7.1) test branch zp
LBRnp2: AND R5 R5 0x0
        BRn DONE
        BRzp LBRzp1
        
DONE:   TRAP 0x0
