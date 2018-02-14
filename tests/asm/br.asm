        0x3400
        BR      L1
        ADD     R0      R0      0x1
L1:     ADD     R0      R0      0x1 ;R0 contains 1
        BRn     DONE
        BRp     L2

        ;; comment?
        
        ADD     R1      R1      0x1
L2:     ADD     R1      R1      0x1f ;R1 contains -1
        BRp     DONE
        ADD     R1      R1      0x1f ;R1 contains -2
DONE:   TRAP    0x0
