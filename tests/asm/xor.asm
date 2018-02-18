        0x3400

        ;; Load R0 with 0xAAAA
        ADD     R0      R0      0xA
        LSHF    R0      R0      4
        ADD     R0      R0      0xA
        LSHF    R0      R0      4
        ADD     R0      R0      0xA
        LSHF    R0      R0      4
        ADD     R0      R0      0xA

        NOT     R1      R0

        XOR     R2      R1      -1

        TRAP    0x0
