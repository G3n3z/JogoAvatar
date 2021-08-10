;------------------------------------------------------------------------
;	Base para TRABALHO PRATICO - TECNOLOGIAS e ARQUITECTURAS de COMPUTADORES
;   
;	ANO LECTIVO 2020/2021
;
;	Daniel Fernandes 	- a2020116565
;	Hugo Jorge			- a2020116988
;
;--------------------------------------------------------------
; Demostra��o da navega��o do Ecran com um avatar
;
;		arrow keys to move 
;		press ESC to exit
;
;--------------------------------------------------------------

.8086
.model small
.stack 2048

dseg	segment para public 'data'


		STR12	 			DB 		"            "	; String para 12 digitos
		DDMMAAAA 			db		"                     "
		
		Horas				dw		0				; Vai guardar a HORA actual
		Minutos				dw		0				; Vai guardar os minutos actuais
		Segundos			dw		0				; Vai guardar os segundos actuais
		Old_seg				dw		0				; Guarda os �ltimos segundos que foram lidos
		Tempo_init			dw		000				; Guarda O Tempo de inicio do jogo
		Tempo_j				dw		0				; Guarda O Tempo que decorre o  jogo
		Tempo_limite		dw		100				; tempo m�ximo de Jogo
		String_TJ			db		"    /10$"
		
		String_num 			db 		"  0 $"
        String_nome  		db	    "          $"	
		Construir_nome		db	    "          $"	
		Dim_nome			db		5	; Comprimento do Nome
		indice_nome			dw		0	; indice que aponta para Construir_nome
		
		Fim_Ganhou			db	    " Ganhou $"	
		Fim_Perdeu			db	    " Perdeu $"	

        Erro_Open       	db      'Erro ao tentar abrir o ficheiro$'
        Erro_Ler_Msg    	db      'Erro ao tentar ler do ficheiro$'
        Erro_Close      	db      'Erro ao tentar fechar o ficheiro$'
        Fich         		db      'labi.TXT',0
		FichMenu			db		'menu.TXT',0
        HandleFich      	dw      0
        car_fich       		db      ?
		stri				db	"               $"
		string				db		"Teste pr�tico de T.I",0
		Car					db	32	; Guarda um caracter do Ecran 
		Cor					db	7	; Guarda os atributos de cor do caracter
		POSy				db		3	; a linha pode ir de [1 .. 25]
		POSx				db		3	; POSx pode ir [1..80]	
		POSya				db		3	; Posi��o anterior de y
		POSxa				db		3	; Posi��o anterior de x
		nivel				db		1	; Nivel do Jogo
		nivel2 				db 		2
		nivel3				db		3		
		nivel4				db		4
		nivel5				db		5
		string_1			db		"ISEC$"
		string_2 			db		"TACPL$"
		string_3			db		"AVATAR$"
		string_4			db		"JOGOTAC$"
		string_5			db		"TRABALHO$"
		pontuacaoJog		dw		0	
		lim					db		"   $"
		temp_lim			db		"       $"
		temp_lim1			db		"/100 $"
		temp_lim2			db		"/90 $"
		temp_lim3			db		"/80 $"
		temp_lim4			db		"/70 $"
		temp_lim5			db		"/60 $"
		string_ganhou		db		"Ganhou$"
		string_perdeu		db		"Perdeu$"
		string_final		db		"Parabens terminou$"
		vazio				db		"          $"
		pont				db		"   $"
		ganhou				db		0
		PosMx				db		37
		PosMy				db		4
		menu				db		?
		FichJogo			db		'descj.TXT',0
		stringSair			db		"Sair do Jogo$"
		FtextTop			db		'desct.TXT',0
		FtextS				db		'descs.TXT',0
		FichPont			db		'top.TXT',0
		posPont				dw 		407
		posNome				dw		396
		posPontnome			db		"   $"
		msgErrorCreate		db	"Ocorreu um erro na criacao do ficheiro!$"
		msgErrorWrite		db	"Ocorreu um erro na escrita para ficheiro!$"
		msgErrorClose		db	"Ocorreu um erro no fecho do ficheiro$"
		ESCAPE				db		 "Prima Escape para Sair ...$"
		para				db       "Parabens entrou para o score$"
		score				db		 " Digite 3 caracteres para o score: $"
		PosNy				db		24
		PosNx				db		39
		ultimo_num_aleat	dw		0
		top10				db		16 dup("                                                                        ")
		
dseg	ends

cseg	segment para public 'code'
assume		cs:cseg, ds:dseg



;########################################################################
goto_xy	macro		POSx,POSy
		mov		ah,02h
		mov		bh,0		; numero da p�gina
		mov		dl,POSx
		mov		dh,POSy
		int		10h
endm

;########################################################################
; MOSTRA - Faz o display de uma string terminada em $

MOSTRA MACRO STR 
MOV AH,09H
LEA DX,STR 
INT 21H
ENDM

; FIM DAS MACROS



;ROTINA PARA APAGAR ECRAN

apaga_ecran	proc
			mov		ax,0B800h
			mov		es,ax
			xor		bx,bx
			mov		cx,25*80
		
apaga:		mov		byte ptr es:[bx],' '
			mov		byte ptr es:[bx+1],7
			inc		bx
			inc 	bx
			loop	apaga
			ret
apaga_ecran	endp
;########################################################################
; Calculo de numero aleatorio
CalcAleat proc near 

	push  cx
	MOV AH,2CH ;VAI BUSCAR A HORA DO SISTEMA
	INT 21H

	MOV ultimo_num_aleat,dx    ;Guarda na variavel os segundos (DH) os milisegundos (DL)

	pop cx
	
	RET
CalcAleat endp
;########################################################################
; Verificação de posição gerada aleatóriamente

verificaAleatorio PROC
	push ax
	push bx
	push dx
	push cx
	pushf

ciclo:
	call	CalcAleat				; Função que calcula um numero aleatorio
	xor     dx, dx
	mov     ax, ultimo_num_aleat	
	mov     bx, 16
	div     bx
	add     dx, 2					; Formula de calculo de numero aleatorio entre a e b

	cmp		dx, 2
	jl		ciclo					; Verificação do numero se realmente está dentro do labirinto
	cmp     dx, 18
	ja		ciclo
	mov ch, dl						; Guardar em ch a nova POSy
ciclo2:
	call	CalcAleat				; Função que calcula um numero aleatorio
	xor     dx, dx
	mov     ax, ultimo_num_aleat
	mov     bx, 76					
	div     bx
	add     dx, 2					; Formula de calculo de numero aleatorio entre a e b
	
	cmp    	dx, 2
	jl		ciclo2
	cmp     dx, 76
	ja		ciclo2					; Verificação do numero se realmente está dentro do labirinto

	
	mov cl, dl						; Guardar em ch a nova POSx
	goto_xy cl, ch					; Mover o cursor para  a nova posição
	mov ah, 08h
	mov bh, 0
	int 10h							; Coloca em al o caracter por baixo do cursor
	
	cmp al, 32
	jne ciclo						; Se por baixo do cursor não estiver um espaço em branco volta a calcular uma posição aleatória
	
	mov POSx, cl					;Guardar as novas posições nas variáveis
	mov Posy, ch

	goto_xy	POSx,POSy	


	popf
	pop CX
	pop DX
	pop BX
	pop AX


	RET
verificaAleatorio endp
;########################################################################
; IMP_FICH

IMP_FICH	PROC

		;abre ficheiro
        mov     ah,3dh
        
        int     21h
        jc      erro_abrir
        mov     HandleFich,ax
        jmp     ler_ciclo

erro_abrir:
        mov     ah,09h
        lea     dx,Erro_Open
        int     21h
        jmp     sai_f

ler_ciclo:
        mov     ah,3fh
        mov     bx,HandleFich
        mov     cx,1
        lea     dx,car_fich
        int     21h
		jc		erro_ler
		cmp		ax,0		;EOF?
		je		fecha_ficheiro
        mov     ah,02h
		mov		dl,car_fich
		int		21h
		jmp		ler_ciclo

erro_ler:
        mov     ah,09h
        lea     dx,Erro_Ler_Msg
        int     21h

fecha_ficheiro:
        mov     ah,3eh
        mov     bx,HandleFich
        int     21h
        jnc     sai_f

        mov     ah,09h
        lea     dx,Erro_Close
        Int     21h
sai_f:	
		RET
		
IMP_FICH	endp		


;********************************************************************************
;********************************************************************************
; HORAS  - LE Hora DO SISTEMA E COLOCA em tres variaveis (Horas, Minutos, Segundos)
; CH - Horas, CL - Minutos, DH - Segundos
;********************************************************************************	

Ler_TEMPO PROC	
 
		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX
	
		PUSHF
		
		MOV AH, 2CH             ; Buscar a hORAS
		INT 21H                 
		
		XOR AX,AX
		MOV AL, DH              ; segundos para al
		mov Segundos, AX		; guarda segundos na variavel correspondente
		
		XOR AX,AX
		MOV AL, CL              ; Minutos para al
		mov Minutos, AX         ; guarda MINUTOS na variavel correspondente
		
		XOR AX,AX
		MOV AL, CH              ; Horas para al
		mov Horas,AX			; guarda HORAS na variavel correspondente
 
		POPF
		POP DX
		POP CX
		POP BX
		POP AX
 		RET 
Ler_TEMPO   ENDP 

;********************************************************************************
;********************************************************************************	
;-------------------------------------------------------------------
; HOJE - LE DATA DO SISTEMA E COLOCA NUMA STRING NA FORMA DD/MM/AAAA
; CX - ANO, DH - MES, DL - DIA
;-------------------------------------------------------------------
HOJE PROC	

		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX
		PUSH SI
		PUSHF
		
		MOV AH, 2AH             ; Buscar a data
		INT 21H                 
		PUSH CX                 ; Ano-> PILHA
		XOR CX,CX              	; limpa CX
		MOV CL, DH              ; Mes para CL
		PUSH CX                 ; Mes-> PILHA
		MOV CL, DL				; Dia para CL
		PUSH CX                 ; Dia -> PILHA
		XOR DH,DH                    
		XOR	SI,SI
; DIA ------------------ 
; DX=DX/AX --- RESTO DX   
		XOR DX,DX               ; Limpa DX
		POP AX                  ; Tira dia da pilha
		MOV CX, 0               ; CX = 0 
		MOV BX, 10              ; Divisor
		MOV	CX,2
DD_DIV:                         
		DIV BX                  ; Divide por 10
		PUSH DX                 ; Resto para pilha
		MOV DX, 0               ; Limpa resto
		loop dd_div
		MOV	CX,2
DD_RESTO:
		POP DX                  ; Resto da divisao
		ADD DL, 30h             ; ADD 30h (2) to DL
		MOV DDMMAAAA[SI],DL
		INC	SI
		LOOP DD_RESTO            
		MOV DL, '/'             ; Separador
		MOV DDMMAAAA[SI],DL
		INC SI
; MES -------------------
; DX=DX/AX --- RESTO DX
		MOV DX, 0               ; Limpar DX
		POP AX                  ; Tira mes da pilha
		XOR CX,CX               
		MOV BX, 10				; Divisor
		MOV CX,2
MM_DIV:                         
		DIV BX                  ; Divisao or 10
		PUSH DX                 ; Resto para pilha
		MOV DX, 0               ; Limpa resto
		LOOP MM_DIV
		MOV CX,2 
MM_RESTO:
		POP DX                  ; Resto
		ADD DL, 30h             ; SOMA 30h
		MOV DDMMAAAA[SI],DL
		INC SI		
		LOOP MM_RESTO
		
		MOV DL, '/'             ; Character to display goes in DL
		MOV DDMMAAAA[SI],DL
		INC SI
 
;  ANO ----------------------
		MOV DX, 0               
		POP AX                  ; mes para AX
		MOV CX, 0               ; 
		MOV BX, 10              ; 
 AA_DIV:                         
		DIV BX                   
		PUSH DX                 ; Guarda resto
		ADD CX, 1               ; Soma 1 contador
		MOV DX, 0               ; Limpa resto
		CMP AX, 0               ; Compara quotient com zero
		JNE AA_DIV              ; Se nao zero
AA_RESTO:
		POP DX                  
		ADD DL, 30h             ; ADD 30h (2) to DL
		MOV DDMMAAAA[SI],DL
		INC SI
		LOOP AA_RESTO
		POPF
		POP SI
		POP DX
		POP CX
		POP BX
		POP AX
 		RET 
HOJE   ENDP 

;********************************************************************************
;********************************************************************************
; Imprime o tempo e a data no monitor

Trata_Horas PROC

		PUSHF
		PUSH AX
		PUSH BX
		PUSH CX
		PUSH DX		

		CALL 	Ler_TEMPO				; Horas MINUTOS e segundos do Sistema
		
		MOV		AX, Segundos
		cmp		AX, Old_seg			; VErifica se os segundos mudaram desde a ultima leitura
		je		fim_horas			; Se a hora n�o mudou desde a �ltima leitura sai.
		mov		Old_seg, AX			; Se segundos s�o diferentes actualiza informa��o do tempo
		
		call 		tempolimite		;Se houver mudança nos segundos, esta função atualiza o tempo de jogo

		 
		
		mov 	ax,Horas
		MOV		bl, 10     
		div 	bl
		add 	al, 30h				; Caracter Correspondente �s dezenas
		add		ah,	30h				; Caracter Correspondente �s unidades
		MOV 	STR12[0],al			; 
		MOV 	STR12[1],ah
		MOV 	STR12[2],'h'		
		MOV 	STR12[3],'$'
		GOTO_XY 2,0
		MOSTRA STR12 		
        
		mov 	ax,Minutos
		MOV 	bl, 10     
		div 	bl
		add 	al, 30h				; Caracter Correspondente �s dezenas
		add		ah,	30h				; Caracter Correspondente �s unidades
		MOV 	STR12[0],al			; 
		MOV 	STR12[1],ah
		MOV 	STR12[2],'m'		
		MOV 	STR12[3],'$'
		GOTO_XY	6,0
		MOSTRA	STR12 		
		
		mov 	ax,Segundos
		MOV 	bl, 10     
		div 	bl
		add 	al, 30h				; Caracter Correspondente �s dezenas
		add		ah,	30h				; Caracter Correspondente �s unidades
		MOV 	STR12[0],al			; 
		MOV 	STR12[1],ah
		MOV 	STR12[2],'s'		
		MOV 	STR12[3],'$'
		GOTO_XY	10,0
		MOSTRA	STR12 		
        
		CALL 	HOJE				; Data de HOJE
		MOV 	al ,DDMMAAAA[0]	
		MOV 	STR12[0], al	
		MOV 	al ,DDMMAAAA[1]	
		MOV 	STR12[1], al	
		MOV 	al ,DDMMAAAA[2]	
		MOV 	STR12[2], al	
		MOV 	al ,DDMMAAAA[3]	
		MOV 	STR12[3], al	
		MOV 	al ,DDMMAAAA[4]	
		MOV 	STR12[4], al	
		MOV 	al ,DDMMAAAA[5]	
		MOV 	STR12[5], al	
		MOV 	al ,DDMMAAAA[6]	
		MOV 	STR12[6], al	
		MOV 	al ,DDMMAAAA[7]	
		MOV 	STR12[7], al	
		MOV 	al ,DDMMAAAA[8]	
		MOV 	STR12[8], al
		MOV 	al ,DDMMAAAA[9]	
		MOV 	STR12[9], al		
		MOV 	STR12[10],'$'
		GOTO_XY	68,0
		MOSTRA	STR12 	
		
						
fim_horas:		
		goto_xy	POSx,POSy			; Volta a colocar o cursor onde estava antes de actualizar as horas
		
		POPF
		POP DX		
		POP CX
		POP BX
		POP AX
fim:
		RET		
			
Trata_Horas ENDP
;########################################################################
; Leitura do ficheiro das pontuacoes
leFich PROC
		
;abre ficheiro
        xor 	si, si
		mov     ah,3dh
        mov 	al,2 
		lea 	dx, FichPont
        int     21h
        jc      erro_abrir
        mov     HandleFich,ax
        jmp     ler_ciclo

erro_abrir:
        mov     ah,09h
        lea     dx,Erro_Open
        int     21h
        jmp     sai_f

ler_ciclo:
        mov     ah,3fh
        mov     bx,HandleFich
        mov     cx,1
        lea     dx,top10[si]		; Vai guardando na variavel top os caracteres do ficheiro, byte a byte
        int     21h
		jc		erro_ler
		cmp		ax,0		;EOF?
		je		fecha_ficheiro
        inc 	si
		jmp		ler_ciclo

erro_ler:
        mov     ah,09h
        lea     dx,Erro_Ler_Msg
        int     21h

fecha_ficheiro:
        mov     ah,3eh
        mov     bx,HandleFich
        int     21h
        jnc     sai_f

        mov     ah,09h
        lea     dx,Erro_Close
        Int     21h
sai_f:	
		RET
		


leFich endp
;########################################################################
; Escrita do ficheiro de pont
escrevefich PROC
		mov		ah, 3ch				; Abrir o ficheiro para escrita
		mov		cx, 00H				; Define o tipo de ficheiro ??
		lea		dx, FichPont		; DX aponta para o nome do ficheiro 
		int		21h					; Abre efectivamente o ficheiro (AX fica com o Handle do ficheiro)
		jnc		escreve				; Se não existir erro escreve no ficheiro
	
		mov		ah, 09h
		lea		dx, msgErrorCreate
		int		21h
	
		jmp		fim

escreve:
		mov		bx, ax				; Coloca em BX o Handle
    	mov		ah, 40h				; indica que é para escrever
    	
		lea		dx, top10			; DX aponta para a infromação a escrever
    	mov		cx, 1150			; CX fica com o numero de bytes a escrever
		int		21h					; Chama a rotina de escrita
		jnc		close				; Se não existir erro na escrita fecha o ficheiro
	
		mov		ah, 09h
		lea		dx, msgErrorWrite
		int		21h
close:
		mov		ah,3eh				; fecha o ficheiro
		int		21h
		jnc		fim
	
		mov		ah, 09h
		lea		dx, msgErrorClose
		int		21h
fim:
		ret
escrevefich endp

;########################################################################
; Mostra Tempo Limite

mostra_temp_lim PROC
	
	push ax
	push bx
	push cx
	xor ah, ah
	mov si, 2
	mov cl, 10	

ciclo:	
	div cl			;Devisoes sucessivas por 10 para obter os digitos da pontuação separadamente
	add ah, 48		;Adiciona 48 para passar ao respetivo caracter
	mov lim[si], ah	
	dec si
	xor ah, ah
	cmp ax, 0
	JNE ciclo
	
	goto_xy  57,0 	;Mostra o tempo atualizado
	MOSTRA lim
	goto_xy	POSx,POSy
	pop cx
	pop bx
	pop ax
	ret
mostra_temp_lim endp
;########################################################################
; Trata Tempo Limite

tempolimite PROC
	push ax
	pushf

	mov ax, Tempo_j
	inc ax			;Incremento do tempo limite
	mov Tempo_j, ax	
	call mostra_temp_lim	;Função que passa o tempo para uma string e imprime

	popf
	pop ax

	ret

tempolimite endp

;########################################################################
; LE UMA TECLA	

LE_TECLA	PROC
		
		push BX
		pushf
sem_tecla:

		
		call Trata_Horas		;Atualização da hora
		mov ax, Tempo_j
		mov bx, Tempo_limite	
		cmp ax,bx				;Comparação do tempo para saber se o tempo chegou ao fim
		jne continua
		mov cl,1				;Se o tempo tiver excedido, poe o valor de cl a 1
		jmp SAI_TECLA
		
continua:
		MOV	AH,0BH
		INT 21h
		cmp AL,0
		je	sem_tecla



		mov		ah,08h
		int		21h
		mov		ah,0
		cmp		al,0
		jne		SAI_TECLA
		mov		ah, 08h
		int		21h
		mov		ah,1
SAI_TECLA:
		popf
		pop BX
	
	RET
LE_TECLA	endp

;########################################################################
; Verifica se o caracter por baixo do cursor � uma letra	

VERIFICA_CHAR PROC
		push AX
		pushf
		goto_xy	POSx,POSy	;Move o cursor para a nova posição
		mov ah, 08h
		mov bh,0			; numero da p�gina
		int 10h

		xor Bl, Bl			;Verificação do caracter
		CMP AL, 32
		JE  final
		CMP AL, 'A'			;Caso o caracter seja diferente de espaço, menor que 'A' e maior que 'z' incrementa o bl
		jl salto			
    	CMP AL, 'z'
		ja salto
		jmp final

salto:	         			
        INC Bl
		goto_xy	POSxa,POSya	; Se o caracter não é uma letra ou espaço em branco atualiza o cursor para a posição anterior
final:           

		popf
		pop AX
		RET

VERIFICA_CHAR endp

;########################################################################
; Copia palavra-chave para variavel string	

copiaString  PROC
		push ax
		xor di, di
		xor ch, ch
copianov:
		mov al, [SI]		
		mov string_nome[DI], al		; Copia palavra-chave para variavel string	
	 	inc DI
		inc SI
		inc ch
		CMP al, '$'
		JNE copianov
		mov Dim_nome, ch 	;Atualiza o tamanho da palavra-chave
		
		pop ax
		RET

copiaString endp

;########################################################################
; Verifica se o caracter de baixo do cursor pretense à palavra-chave

verificaPalavra PROC

		push ax
		push bx
		pushf

		xor si, si
		mov al, car

repete:
		
		mov ah, string_nome[si]
		INC si
		cmp ah, '$'
		je  acaba		;Se chega ao fim da string
		cmp al, ah		; Compara se os caracteres sao iguais
		JNE repete
		
		mov ah , Construir_nome[si-1]
		cmp ah, al
		je  acaba
		mov Construir_nome[si-1], al  ; Copiar para Construir_nome o caracter
		goto_xy 9, 21
		MOSTRA Construir_nome
		goto_xy POSx, POSy
		xor bx, bx
		mov bx, pontuacaoJog
		add bx, 5
		mov pontuacaoJog, bx
		jmp repete

acaba:
		

		popf
		pop BX
		pop AX
		
		RET

verificaPalavra endp

;########################################################################
; Verifica se cocluiu a palavra

verificaFim PROC
	push ax
	push bx
	push si
	pushf

	xor si, si
	xor bl, bl
igual:
	mov al, Construir_nome[si]
	mov ah, string_nome[si]	
	cmp al, '$'			;Compara se chegou ao fim da string
	je salta
	
	inc si		
	inc bl     ;Contador para verificar o tamanho do construir nome
	cmp al, ah  ;Compara se são iguais
	je igual
	
salta:  
	mov cl, Dim_nome
	cmp cl, bl		 
	JA fim    	; Se o tamanho de construir nome (bl) for menor que a dimensao da palavra-chave salta fora
	mov cl, ganhou
	inc cl		; Condicao de termino do jogo
	mov ganhou, cl	
fim:

	popf
	pop SI
	pop BX
	pop AX

	RET	



verificaFim endp

;########################################################################
; Copia duas strings
copia2strings PROC
		push ax
		pushf
ciclo:
		mov al, [di]
		mov [si], al      ;copia para[si] [di]
		inc si
		inc di
		cmp al,'$'		;copia até encontrar $ 
		jne ciclo
		
		popf
		pop AX
		ret
copia2strings endp
;########################################################################
; Passa numero a string
num2string PROC
	push ax
	push bx
	pushf	


    mov bx, 10

	
ciclo:
	cmp ax, 0
	je fim
	xor dx, dx
	div bx
	add dl,48			
	mov [si], dl
	dec si
	jmp ciclo
	
fim:
	popf
	pop bx
	pop AX

	RET
num2string endp
;########################################################################
; Coloca uma string apenas com caracteres de espaço

limpaString PROC
	push ax
	mov al, 32			; Coloca em al o valor do caracter ' '
ciclo:

	mov ah, [si]	
	cmp ah, '$'			; Enquanto não chegar ao fim da string
	je fim
	
	mov [si], al		; Coloca na string o caracter ' ' 
	inc si				; Incremento do ponteiro
	jmp ciclo			

fim:
	pop ax
	RET

limpaString endp
;########################################################################
; Novo nivel
novonivel proc
	pushf
	push ax
	push dx
	push si
	
	call		apaga_ecran
	goto_xy		0,0
	mov     al,0
	lea     dx,Fich
	call		IMP_FICH
	
	
	xor ax, ax
	goto_xy		45,0	;Mostra o nivel em que esta
	mov dl, nivel
	add dl, 30h
	mov ah, 02h
	int 21h

	xor ax, ax			; Tempo limite a 0
	mov Tempo_j, ax

	mov al, nivel
	cmp al, 1
	je  nivel1

	mov ax, Tempo_limite
	sub ax, 10				; decrementar tempo limite
	mov Tempo_limite, ax 
	jmp  fnivel1

nivel1:
	mov Tempo_limite, 100 	;Atualiza os tempos
	mov pontuacaoJog, 0		;Repoe a pontuação a 0
	lea si, pont			
	call limpaString
	

fnivel1:

	
	lea si, lim
	call limpaString

	lea si, Construir_nome
	call limpaString

	xor cl, cl
	mov ganhou, cl
	
	pop si
	pop dx
	pop ax
	popf
	ret
novonivel endp


;########################################################################
; Pede nome ao utilizdor
pedeNome PROC
	push bx
	push si
	pushf
	
	goto_xy 5,24
	Mostra para		; Impressão de mensagens informativas
	goto_xy 5,25
	Mostra score
	mov bh, 40
	xor si, si
ciclo:	

	call LE_TECLAMenu		;Lê uma tecla do teclado
	mov posPontnome[si], al
	mov dl, al
	mov ah, 02				
	int 21h
	inc si
	inc bh
	cmp si, 3				;Faz este processo 3 vezes (número e letras permitidas no top10)
	jne ciclo

	popf
	pop si
	pop bx

	ret

pedeNome endp
;########################################################################
; Atualizção do Top 10
atualizatop proc
	
	mov si, posPont
	xor di, di
	mov cx, 10
	
ciclo1:

	
	sub si, di  ;Retirar ao si as vezes que incrementou no ciclo2
	push cx		; Coloca na pilha o valor de cx para uso no loop ciclo1
	mov cx, 3
	xor di, di

ciclo2:
	
	mov al, pont[di] ; move para al o digito da pontuação efetuada
	mov ah, top10[si] ;move para al o digito da pontuação guardada em ficheiro
	inc di
	inc si
	cmp ah, al
	ja  continua	;Se ah maior que al salta fora do ciclo2 para passar para o proximo ranking
	jl  salto  ; Se ah for menor que al, salta fora do ciclo, significa que encontrou algum score menor
	loop ciclo2

continua:

	add si, 72   ; Incrementa o index para a proxima linha
	pop cx		; Recupera da pilha o valor de cx para o uso do loop ciclo1
	loop ciclo1
	jmp fim

salto:
	
	call pedeNome	;Pede o nome ao utilizdor
	
	mov ax, si  
	mov dx, posPont
	sub ax, dx
	mov bl, 72		; Calculos para determinar quantas iterações são necessárias para atualizar o top10 no loop altera
	div bl
	mov bl, 9
	sub bl, al
	mov cl, bl
	xor ch, ch
	
	mov bx, si		; Guarda a posição em que se insere o novo valor
	sub bx, di		; Subtrai ao index a quantidade de casas que incrementou para a comparação feita anteriormente

	mov si, 1055	; Coloca si a apontar para a score do rank 10 do top
	xor di, di
	mov bp, 1044	; Coloca bp a apontar para o nome do rank 10 do top

altera:
	sub si, di   ; Voltar ao inicio da pontuacao 
	xor di, di	 ;Subtrai ao index a quantidade de casas que incrementou para a comparação feita no ciclo3

ciclo3:

	mov al,top10[si-72] ; Copia caracter a caracter começando a copiar do rank 9 para o rank 10
	mov top10[si], al
	mov ah, top10[bp-72]
	mov top10[bp], ah
	inc si
	inc di
	inc bp
	cmp di, 3
	jne ciclo3

	sub si, 72
	sub bp, 72
	sub bp, di
	
	loop altera
	
	xor di, di

ciclo4:
	mov al, pont[di]
	mov top10[bx], al
	mov ah, posPontnome[di]	; Atualização do score e do nome na posição descoberta anteriormente 
	mov top10[bp], ah

	inc di
	inc bx
	inc bp
	cmp di, 3
	jne ciclo4

	call escrevefich		; Atualiza o ficheiro das pontuações
	xor ax, ax

fim:
	
	ret
atualizatop endp
;########################################################################
; Avatar

AVATAR	PROC
			MOV nivel, 1
			

Inicio:			


			mov		ax,0B800h
			mov		es,ax

			call novonivel     ; Chama a função que trata e alguns aspetos de ser um novo nivel

			goto_xy 	9, 21
			MOSTRA 		vazio  ; Limpeza da impressao do jogo anterior
			mov 	al, nivel	
			cmp 	al, 1
			jne	nivel_2			; verificação do nivel

nivel_1:		
			Lea	SI, string_1		
			call 	copiaString		; Copia para a string da palavra chave a palavra isec guardada em string_1
			LEA DI, temp_lim1		; Ponteiro para a string do tempo limite do nivel 1
				
			jmp jogar				; Salto para o inicio do jogo
			
nivel_2:
			

			cmp al, 2
			jne	nivel_3				; verificação do nivel
			LEA SI, string_2	
			call 	copiaString		; Copia para a string da palavra chave a palavra guardada em string_2
			LEA DI, temp_lim2		; Ponteiro para a string do tempo limite do nivel 2
			jmp jogar				; Salto para o inicio do jogo

nivel_3:
			
			cmp al, 3				
			jne	nivel_4				; verificação do nivel
			LEA SI, string_3
			call 	copiaString		; Copia para a string da palavra chave a palavra guardada em string_3
			LEA DI, temp_lim3		; Ponteiro para a string do tempo limite do nivel 3
			jmp jogar				; Salto para o inicio do jogo

nivel_4:
			
			cmp al, 4				
			jne	nivel_5				; verificação do nivel
			LEA SI, string_4		
			call 	copiaString		; Copia para a string da palavra chave a palavra guardada em string_4
			LEA DI, temp_lim4		; Ponteiro para a string do tempo limite do nivel 4
			jmp jogar				; Salto para o inicio do jogo


nivel_5:
			
			cmp al, 5
			jne	fim					; verificação do nivel, se nao for 5, signfica que consegui concluir todos os niveis
			LEA SI, string_5		
			call 	copiaString		; Copia para a string da palavra chave a palavra guardada em string_5
			LEA DI, temp_lim5		; Ponteiro para a string do tempo limite do nivel 4

jogar:
			LEA SI, temp_lim
			call  	copia2strings	; Copia para o tempo limite uma string no qual o ponteiro DI usado na função e atribuido conforme o nivel
			goto_xy  60,0
			MOSTRA 	temp_lim   ;Impressao /100 

			goto_xy  9 ,20 
			MOSTRA    string_nome	;Impressão no ecra da palavra chave definida
			    
			
			push cx
			call verificaAleatorio
			pop cx
			goto_xy	POSx,POSy		; Vai para nova possi��o

		
CICLO:			
			goto_xy	POSxa,POSya		; Vai para a posi��o anterior do cursor
			mov		ah, 02h
			mov		dl, Car			; Repoe Caracter guardado 
			int		21H		

			goto_xy	POSx,POSy		; Vai para nova possi��o
			mov 	ah, 08h
			mov		bh,0			; numero da p�gina
			int		10h		
			mov		Car, al			; Guarda o Caracter que est� na posi��o do Cursor
			mov		Cor, ah			; Guarda a cor que est� na posi��o do Cursor
			
			
		
			goto_xy	78,0			; Mostra o caractr que estava na posi��o do AVATAR
			mov		ah, 02h			; IMPRIME caracter da posi��o no canto
			mov		dl, Car	
			int		21H
			
						
				

	
			goto_xy	POSx,POSy		; Vai para posi��o do cursor

IMPRIME:	mov		ah, 02h
			mov		dl, 190	; Coloca AVATAR
			int		21H	
			goto_xy	POSx,POSy	; Vai para posi��o do cursor
	
			mov		al, POSx	; Guarda a posi��o do cursor
			mov		POSxa, al
			mov		al, POSy	; Guarda a posi��o do cursor
			mov 	POSya, al
			call    verificaPalavra

			xor 		AX, AX
			mov 		AX,pontuacaoJog		;pontuaçao do jogo 
			lea 		si, pont			; ponteiro para a string onde é colocado a pontuação
			add 		si, 2				; Incremento de 2 unidades para começar a traduzir e numero para caracter pelas unidades 
			call 		num2string			;Função que passa um numero a string
			goto_xy 	29,0 
			Mostra 		pont				;Mostrar a pontuação
			call 	verificaFim				; Verifica se acabou
			mov 	cl, ganhou				; Variavel para armazenar se ganhou
			cmp 	cl, 1	
			jl		LER_SETA				; Se for menor que 1 significa que ainda não chegou ao fim
			mov 	POSx, 0  				;Poe posicao inicial
			mov		POSy, 0					
			je 		ciclo					;Se for igual volta a ir ao ciclo sem pedir tecla para imprimir a letra em que está o avatar
			ja		fimGanhou				; Se é maior, já imprimiu o ultimo caracter e acabou de passar de nivel


LER_SETA:	call 	LE_TECLA				; Função que lê tecla do teclado
			cmp 		cl,1
			je		fimPerdeu				; Se cl, for igual a 1 significa que acabou o tempo
			cmp		ah, 1					
			je		ESTEND					; Se ah = 1 é uma tecla estendida e salta para  verificação de tecla estendida
			CMP 	AL, 27	; ESCAPE		;Se for =0 e o al=27 é a tecla escape e sai do jogo
			JE		fimESC
			jmp		LER_SETA				; Se chegou ao fim a tecla introduzida não é valida
		
ESTEND:		cmp 	al,48h					;Se a tecla for diferente de 48h salta para verificação de tecla para baixo
			jne		BAIXO
			dec		POSy					;Decrementa a posição do y do cursor
			call 		VERIFICA_CHAR		;Verifica se o que está por baixo do cursor é um caracter valido
			cmp		bl,0					; Se bl for igual a 0 , valor colocado na função verifica_char, significa que o caracter de baixo do cursor não é uma parede
			je               CICLO			; E continua o jogo
			inc		POSy					; Caso contrario repoe o valor de y e poe o cursor na posição anterior
			goto_xy	POSxa,POSya
			jmp		LER_SETA				; E volta a ler outra tecla do teclado
				              
					

BAIXO:		cmp		al,50h					;Se a tecla for diferente de 50h salta para verificação de tecla para a esquerda
			jne		ESQUERDA				
			inc 	POSy					;Incrementa a posição do y do cursor
			call 		VERIFICA_CHAR		;Verifica se o que está por baixo do cursor é um caracter valido
			cmp		bl,0
			je               CICLO			 ;Se bl for igual a 0 , valor colocado na função verifica_char, significa que o caracter de baixo do cursor não é uma parede
			dec		POSy
			jmp		LER_SETA				; Caso contrario repoe o valor de y e poe o cursor na posição anterior E volta a ler outra tecla do teclado
			

ESQUERDA:
			cmp		al,4Bh					;Se a tecla for diferente de 4Bh salta para verificação de tecla para a direita
			jne		DIREITA					
			dec		POSx					;Decrementa a posição do x do cursor
			call 	VERIFICA_CHAR			;Verifica se o que está por baixo do cursor é um caracter valido
			cmp		bl,0					; Se bl for igual a 0 , valor colocado na função verifica_char, significa que o caracter de baixo do cursor não é uma parede
			je      CICLO					; Caso contrario repoe o valor de x e poe o cursor na posição anterior
			inc		POSx
			jmp		LER_SETA

DIREITA:
			cmp		al,4Dh					;Se a tecla for diferente de 4Dh volta a ler outra tecla do teclado
			jne		LER_SETA 			
			inc		POSx					;Incrementa a posição do x do cursor
			call 	VERIFICA_CHAR			;Verifica se o que está por baixo do cursor é um caracter valido
			cmp		bl,0
			je      CICLO					; Se bl for igual a 0 , valor colocado na função verifica_char, significa que o caracter de baixo do cursor não é uma parede
			dec		POSx
			jmp		LER_SETA				; Caso contrario repoe o valor de x e poe o cursor na posição anterior
			

fimGanhou:

			mov 	 	ax, Tempo_limite		; Se passou de nivel vai adicionar à pontuação o tempo restante 
			mov 	 	bx, Tempo_j
			SUB 	 	AX, BX
			mov 		bx, pontuacaoJog
			add		 	ax, bx
			mov			pontuacaoJog, ax
			goto_xy 	60,20
			mostra   	string_ganhou
			goto_xy 	POSx, POSy

			inc 		nivel				;Incrementa o nivel e volta ao inicio do jogo
			jmp 		Inicio	

fimPerdeu:		
			goto_xy 		60,20			
			mostra   	string_perdeu		
			goto_xy 		POSx, POSy
			
fim:

			mov 	 ax, Tempo_limite		;Quando acaba o jogo volta a adicionar à pontuação do a diferença do tempo limite e do tempo decorrido
			mov 	 bx, Tempo_j
			SUB 	 AX, BX
			mov 	bx, pontuacaoJog
			add		 ax, bx
			mov		pontuacaoJog, ax		;Atualiza a variavel da pontuação
			goto_xy 		60,20
			mostra   	string_final	
			call        leFich				; le o ficheiro das pontuações
			call atualizatop				; Atualiza as pontuações

fimESC:				

			call		apaga_ecran
			
			
			goto_xy		0,0	
			
			lea     	dx,FichPont
			mov 		al,0
			call		IMP_FICH			; Mostra as pontuações
			xor         Ax, ax
			xor 		bx, bx
			call        MenuPontuacoes

			RET
AVATAR		endp

LE_TECLAMenu	PROC
		
sem_tecla:

		mov		ah,08h
		int		21h
		mov		ah,0
		cmp		al,0
		jne		SAI_TECLA
		mov		ah, 08h
		int		21h
		mov		ah,1

SAI_TECLA:	RET
LE_TECLAMenu	endp



;########################################################################

menuPrincipal PROC
			push AX
			push BX
			push dx
			pushf

CICLO:		
			goto_xy	PosMx,PosMy    ;Poe o cursor na posicao nova
			mov bl, PosMy 		 	
			cmp bl, 4				;Compara a posicao no eixo dos yy
			jne  textoPont			; Se nao for igual a 4 significa que não está em cima do jogar
									;Caso contrario:
			goto_xy  1,12			; Coloca o cursor na posicao de escrever o ficheiro com as regras do jogo
			
			mov     al,0
			lea 	dx,Fichjogo
			call  	IMP_FICH
			mov     menu, 1 
			goto_xy	PosMx,PosMy    ; Volta a colocar o cursor na posição em que estava
			jmp LER_SETA			; Volta a ler uma entrada do teclado
			
textoPont:		
			cmp bl, 6				
			jne  Sair				; Se nao for igual a 6 significa que não está em cima do Top10
			goto_xy  1,12			; Coloca o cursor na posicao de escrever o ficheiro com uma mensagem informativa

			lea 	dx, FtextTop	
			mov     al,0
			call  	IMP_FICH
			mov		menu,2  
			goto_xy	PosMx,PosMy		; Volta a colocar o cursor na posição em que estava
			jmp LER_SETA			; Volta a ler uma entrada do teclado
Sair:
			goto_xy  0,12			; Se chegar aqui significa que esta em 'Sair'			
			lea 	dx, FtextS		
			mov     al,0
			call  	IMP_FICH
			mov		menu,3 		
			goto_xy	PosMx,PosMy		; Volta a colocar o cursor na posição em que estava

LER_SETA:	call 	LE_TECLAMenu	; Chama a funcao que lê do teclado
			cmp		ah, 1			
			je		ESTEND			; Se ah = 1, é uma tecla estendida
			jmp		TeclaEnter		; Caso contrario vamos verificar se é enter
		
		
ESTEND:			
			cmp 	al,48h			; se al for igual a 48h significa que é a tecla com a seta para cima
			jne		BAIXO			; Caso contrário vamos verificar se é a seta para baixo
			SUB		PosMy,2 		;subtrai-se à posição do y 2 para ir para a a opção de cima
			mov 	bl, POSMY		
			cmp 	bl, 4			; Se for menor que 4 significa que estamos no cimo do menu e não interessa mudar a posicão do cursor 
			jnl		ciclo			; Se não for menor vamos para o ciclo para atualizar o ecra
			mov		PosMy, 8		; Move para a o posição y o valor da ultima opção
			jmp		ciclo    		; Mandamos ler outra tecla sem mudar a posiçãao
			


BAIXO:		
			cmp		al,50h			; se al for igual a 50h significa que é a tecla com a seta para baixo
			jne		LER_SETA		; Caso contrário vamos ler outra vez do teclado, pois não foi inserida nenhuma tecla permitida
			add 	POSMy, 2		;Adiciona-se à posição do y 2 para ir para a a opção de cima
			mov 	bl, POSMY
			cmp 	bl, 8			; Se for maior que 8 significa que estamos no fundo do menu e não interessa mudar a posicão do cursor 
			jna		ciclo			; Se não for maior vamos para o ciclo para atualizar o ecra
			mov		PosMy, 4		; Move para a o posição y o valor da primeira opção
			jmp		ciclo		; Mandamos ler outra tecla sem mudar a posiçãao

TeclaEnter:
			cmp 	al, 13			
			jne		LER_SETA		;Se não for a tecla enter, 13 em codido ASCII, vamos ler outra tecla

			
FIM:
			popf
			pop DX
			pop BX
			pop AX
			RET
menuPrincipal endp
;########################################################################
;Gestão do menu de pontuações
MenuPontuacoes proc

		push ax
		goto_xy  5,17		
		MOSTRA ESCAPE	; Impressão de mensagem de informação
		

TECLA:
		call LE_TECLAMenu	; Ler tecla do teclado
		cmp		ah, 0		; Se nao for uma tecla estendida, volta a ler outra tecla do teclado
		jne		TECLA
		CMP 	AL, 27	
		JE		FIM			; Se for a tecla ESCAPE vai para o fim da função
		jmp		TECLA		; Caso contraio volta a ler outra tecla do teclado

FIM:	
		pop AX
		RET

MenuPontuacoes endp
;########################################################################
Main  proc
			mov			ax, dseg
			mov			ds,ax
			
Inicio:
			mov			ax,0B800h
			mov			es,ax
			
					
			call		apaga_ecran   ; Apaga o ecra
			goto_xy		0,0
			mov     	al,0
			lea     	dx,FichMenu
			call		IMP_FICH		; Importa o ficheiro Menu

			

			
			call 		menuPrincipal	; Chama a função que faz a gestao do menu principal
			
			call		apaga_ecran		; Apaga o ecra
			goto_xy		0,0
		
			cmp 		menu, 1			;Menu = 1, significa que é para jogar
			jne			pontuacoes		; Se o menu não for igual a um vai ver se é igual o pretendido é aceder ao top10
		
			lea     	dx,Fich			
			mov     	al,0
			call		IMP_FICH		; Importa o labirinto
			call 		AVATAR			; Função onde decorre o jogo
			jmp 		Inicio			; Quando acabar o jogo volta ao inicio para voltar ao menu

pontuacoes:
			cmp			menu, 2			;Menu = 1, significa que é para aceder ao top10
			jne			fim				;Se nao for igual significa que a opção é 'Sair'
			lea     	dx,FichPont
			mov 		al,0
			call		IMP_FICH		; Importa o top10
			call        MenuPontuacoes	; Função que gere o top10
			jmp 		Inicio			; Quando sair do top 10 volta ao menu
fim:
			goto_xy		0,22	
			mov			ah,4CH
			INT			21H
Main	endp
Cseg	ends
end	Main


		
