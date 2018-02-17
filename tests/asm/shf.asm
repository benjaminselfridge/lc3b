        0x4500

        ADD     R0      R0      0x1F
        LSHF    R1      R0      4
        RSHFL   R2      R1      3
        RSHFA   R3      R1      4

        TRAP    0x0
        
