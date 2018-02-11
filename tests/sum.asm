        0x3000
START:  AND  R0 R0 0x0          ; 0x5020
        ADD  R0 R0 0x10         ; 0x1030
        AND  R1 R1 0x0          ; 0x5260
LOOP:   AND  R0 R0 R0           ; 0x5000
        BRnz END                ; 0x0C03
        ADD  R1 R1 R0           ; 0x1240
        ADD  R0 R0 0xFFFF       ; 0x103F
        BR   LOOP               ; 0x0FFB
END:    TRAP 0x0                ; 0xF000
