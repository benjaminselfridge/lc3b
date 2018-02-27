        0x3000

        ;; This program sums up the contents of an array at location 0x4000 and
        ;; stores the result in register R0.

        ;; First, we do a bunch of set up:

        ;; R1 will be the index register, storing the address of the next array
        ;; location to add.

START:  LEA     R1      ARR
        LDW     R1      R1      0x0

        ;; Ensure R0 is zeroed out (not strictly necessary since registers are
        ;; initialized to zero, but being anal is a good thing).

        AND     R0      R0      0x0

        ;; R2 will be the number of array elements left to sum. Note that since
        ;; this is the final operation we are doing before looping, we can check
        ;; the condition codes based on this load at the top of the loop.
        LEA     R2      SIZE
        LDW     R2      R2      0x0

        ;; Main loop. We assume at the top of the loop that R1 contains the next
        ;; address to be read, that R2 contains the number of array elements
        ;; left to sum, and that R0 contains the sum accrued thus far.

        ;; Do the "end of loop" check here. If R2 <= 0, we are done and jump to
        ;; exit.
LOOP:   BRnz    EXIT

        ;; Loop body.
        ;; First, we read the next integer from memory.
        LDW     R3      R1      0x0

        ;; Next, we add that value to the current total.
        ADD     R0      R3      R0

        ;; Finally, we increment the next read address and decrement the loop
        ;; counter (number of remaining elements in the array).
        ADD     R1      R1      0x2
        ADD     R2      R2      -1

        ;; Jump to top of loop.
        BR      LOOP

EXIT:   TRAP    0x0

SIZE:   0x3FFE
ARR:    0x4000
