        0x3000
        AND R0  R0  0x0
        ADD R0  R0  0x5
        AND R1  R1  0x0
        ADD R1  R1  0x4
        ADD R2  R1  R0          ; R0 should contain 9
        TRAP 0x0
