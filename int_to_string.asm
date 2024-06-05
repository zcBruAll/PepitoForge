section .text
global int_to_string

int_to_string:
    ; Input: rdi = integer to convert
    ; Output: rsi = pointer to the resulting string (buffer)

    ; Save registers that will be modified
    push rax
    push rbx
    push rcx
    push rdx

    mov rcx, 10           ; RCX will be used as the divisor for division by 10
    mov rbx, rdi          ; Store the value of RDI in RBX
    xor rdx, rdx          ; Clear RDX before the division

    ; Check if the number is zero
    cmp rbx, 0
    jnz convert_loop
    mov byte [rsi], '0'
    inc rsi
    jmp convert_done

convert_loop:
    xor rdx, rdx          ; Clear RDX for division
    div rcx               ; Divide RAX by RCX, quotient in RAX, remainder in RDX
    add dl, '0'           ; Convert remainder to ASCII
    dec rsi               ; Move pointer to the left
    mov [rsi], dl         ; Store the ASCII character
    test rax, rax         ; Check if the quotient is zero
    jnz convert_loop      ; If not, continue dividing

    inc rsi               ; Adjust RSI to point to the start of the string

convert_done:
    ; Restore registers
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret                   ; Use ret instead of syscall to return to caller
