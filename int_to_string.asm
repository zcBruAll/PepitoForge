section .text
    global int_to_string

int_to_string:
    ; Input: rdi = integer to convert
    ; Output: rsi = pointer to the resulting string (buffer)

    ; Save registers that will be modified
    push rbx
    push rdx

    mov rbx, 10           ; RCX will be used as the divisor for division by 10
    lea rsi, [rdi + 19]
    mov byte [rsi], 0

divide_loop:
    xor rdx, rdx       ; Clear RDX to ensure high bits are zero for division
    div rbx            ; Divide RAX by RBX; quotient in RAX, remainder in RDX

    add dl, '0'        ; Convert the remainder to ASCII
    dec rsi            ; Move buffer pointer back
    mov [rsi], dl      ; Store the ASCII character in buffer

    test rax, rax
    jnz divide_loop    ; If the quotient is not zero, continue the loop

    pop rdx
    pop rbx
    ret