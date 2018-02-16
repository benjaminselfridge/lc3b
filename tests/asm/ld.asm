        0x3010
        LEA     R0      DATA1
        LDW     R1      R0      0x0
        LDB     R2      R0      0x0
        LDB     R3      R0      0x1
        LDW     R4      R0      0x0
        LDW     R4      R0      0x1
        TRAP 0x0


DATA1:  0x1234
