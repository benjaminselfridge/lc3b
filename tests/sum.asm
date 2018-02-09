        0x3000
START:  AND  R0 R0 0x0          ; R0 <- 0x0
        ADD  R0 R0 0x10         ; R0 <- 0x10
        AND  R1 R1 0x0          ; R1 <- 0 (sum)
LOOP:   AND  R0 R0 R0           ; Set NZP on R0
        BRnz END                ; If R0 <= 0, jump to end
        ADD  R1 R1 R0           ; R1 += R0
        ADD  R0 R0 0xFFFF       ; R0 -= 1
        JMP LOOP
END:    TRAP 0x0
