        0x3000
L1:     ADD R0 R0 R1
        AND R1 R2 R3
L3:     ADD R0 R0 R0 ; this is a comment
        JMP L1       ; jump
L2:     TRAP
