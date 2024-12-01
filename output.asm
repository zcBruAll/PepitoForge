section .data
x dd 0
y dd 0

section .text
	global _start

_start:
	mov dword [x], 10
	mov dword [y], 20
	mov rax, [x]
	add rax, [y]
	; evaluate rax

	mov rax, 60
	xor rdi, rdi
	syscall
