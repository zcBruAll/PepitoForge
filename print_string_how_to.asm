section .data
    newline db 0xA     ; Newline character
    var1 dq 5
    var2 dq 10

section .bss
    buffer resb 20     ; Buffer for storing ASCII digits (including null terminator)

section .text
    extern int_to_string
    global _start

_start:
    mov rax, [var1]
    mov rbx, [var2]
    mul rbx
    
    ;mov rax, [number]  ; Load the number into RAX register
    lea rdi, [buffer]  ; Load the address of buffer into RDI

    call int_to_string

    ; RDI now points to the start of the ASCII string
    ; Print the ASCII string (assuming Linux x86-64 environment)
    mov rax, 1         ; sys_write system call number (1)
    mov rdi, 1         ; File descriptor 1 (stdout)
    lea rsi, [buffer]  ; Pointer to the string
    lea rdx, [buffer + 20]
    sub rdx, rdi
    syscall            ; Make the system call

    ; Print newline character
    mov rax, 1         ; sys_write system call number (1)
    mov rdi, 1         ; File descriptor 1 (stdout)
    lea rsi, [newline] ; Pointer to the newline character
    mov rdx, 1         ; Length of the string
    syscall            ; Make the system call

    ; Exit the program
    mov rax, 60        ; sys_exit system call number (60)
    xor rdi, rdi       ; Exit code 0
    syscall