        0x5000

        LEA     R0      ADDR
        LDW     R0      R0      0x0
        ADD     R1      R1      0x003
        STW     R1      R0      0x0
        LDW     R2      R0      0x0
        TRAP    0x0

ADDR:   0x5100
