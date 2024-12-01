section .data
x dd 0
y dd 0
z dd 0

section .text
	global _start

_start:
	mov dword [x], 10

	mov dword [y], 20

	mov eax, [x]
	add eax, [y]
	mov dword [z], eax

	mov eax, [y]
	imul eax, [z]
	push eax
	mov eax, [x]
	pop ebx
	mov edx, 0
	div ebx
	sub eax, [x]
	add eax, [y]
	; evaluate eax

	mov eax, 60
	xor edi, edi
	syscall
