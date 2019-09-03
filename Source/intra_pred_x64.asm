; ******************************************************************************
; intra_pred_x64.asm
; Copyright (c) 2013-2018 David Pethes
;
; This file is part of Fev.
;
; Fev is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Fev is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY;  without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with Fev.  If not, see <http://www.gnu.org/licenses/>.
;
; ******************************************************************************

%include "x64inc.asm"

SECTION .rodata
ALIGN 16
vec_w_8x1:
  times 8 dw 1
vec_w_1_to_8:
  dw 1, 2, 3, 4, 5, 6, 7, 8
vec_w_minus7_to_0:
  dw -7, -6, -5, -4, -3, -2, -1, 0


SECTION .text

cglobal predict_top16_sse2
cglobal predict_left16_ssse3
cglobal predict_left16_ssse3.loop
cglobal predict_plane16_sse2
cglobal predict_plane16_sse2.loop

; I16 prediction modes
; src is pixcache:
;      {   0,17 - top left pixel
;         1..16 - pixels from top row
;        18..33 - pixels from left column
;      }              

; predict_top16(src, dst: uint8_p)
ALIGN 16
predict_top16_sse2:
    movdqu xmm0, [r1+1]  ; top row  
    %assign i 0
    %rep 16
        movdqa [r2 + i], xmm0
    %assign i i + 16
    %endrep
    ret

; predict_left16(src, dst: uint8_p)
ALIGN 16
predict_left16_ssse3:
    add r1, 18  ; left column
    mov r3, 8
    pxor xmm2, xmm2
.loop:
    movzx   r0, byte [r1]
    movd    xmm0, r0
    pshufb  xmm0, xmm2

    movzx   r4, byte [r1+1]
    add     r1, 2
    movd    xmm1, r4
    pshufb  xmm1, xmm2
    
    movdqu    [r2   ], xmm0
    movdqu    [r2+16], xmm1
    add     r2, 16*2
    dec r3
    jnz .loop
    ret

; (5 * h|V + 32) >> 6
; 1 - mmreg h/v, 2 - reg dest
; 64b movd = movq, so null the upper part
%macro scale_result 2
    xor  rax, rax
    movd eax, %1
    imul eax, 5
    add  eax, 32
    sar  eax, 6
    mov  %2, rax
%endmacro

; swap word ordering in xmmreg (0..7 -> 7..0)
%macro reorder_8w 1
    pshufd  %1, %1, 27     
    pshuflw %1, %1, 177
    pshufhw %1, %1, 177
%endmacro

; 1 - xmmreg src/res, 2 - vec_w_8x1, 3 - scratch
%macro HADDW 3
    pxor %3, %3
    pmaddwd  %1, %2 
    HADDD  %1, %3
%endmacro

; load B/C constant
; 1 - reg source, 2 - reg dest, 3 - offset
%macro load_constant 3
    movq xmm0, [%1 + %3]          ;0..7
    movq xmm1, [%1 + %3 + 9]      ;9..16
    punpcklbw xmm0, xmm7
    punpcklbw xmm1, xmm7
    reorder_8w xmm0
    psubw  xmm1, xmm0  ;diff = [7..0] - [9..16]
    pmullw xmm1, xmm4  ;diff * (i + 1) ; i = <1..8>
    HADDW  xmm1, xmm5, xmm6
    scale_result xmm1, %2
%endmacro

; predict_plane16(src, dst: uint8_p)
ALIGN 16
predict_plane16_sse2:
    %define regA r4  ; A
    %define regB r10 ; B
    %define regC r11 ; C
    PUSH_XMM_REGS 2
    pxor xmm7, xmm7
    movdqu xmm4, [vec_w_1_to_8]
    movdqu xmm5, [vec_w_8x1]
; get the B constant from top row
    load_constant r1, regB, 0
; get the C constant from left column
    load_constant r1, regC, 17
; get the A constant
    movzx regA, byte [r1 + 16]
    movzx r1,   byte [r1 + 17 + 16]
    add regA, r1
    shl regA, 4
    add regA, 16
; precalculate b * (x - 7);
    movd    xmm0, regB
    pshufd  xmm0, xmm0, 0
    packssdw  xmm0, xmm0 ; b
    movdqa  xmm1, xmm0
    pmullw  xmm0, [rel vec_w_minus7_to_0] ;b * (x - 7)  [x=-7..0]
    pmullw  xmm1, [rel vec_w_1_to_8]      ;b * (x - 7)  [x=1..8]
 
    mov  r0, 16
    mov  r3, -7     ; y = 0; d0 = (y-7) * c,
    imul r3, regC 
    add  r3, regA
.loop:
    movd    xmm2, r3
    pshufd  xmm2, xmm2, 0
    packssdw  xmm2, xmm2  ; d
    movdqa  xmm3, xmm2
    paddw   xmm2, xmm0    ; b * (x - 7) + d = e
    paddw   xmm3, xmm1    ; b * (x - 7) + d = e
    psraw   xmm2, 5       ; e >> 5
    psraw   xmm3, 5       ; e >> 5
    packuswb  xmm2, xmm3      
    movdqu  [r2], xmm2
    add     r2, MB_STRIDE
    add  r3, regC   ; + c
    dec  r0
    jnz  .loop
    POP_XMM_REGS 2
    ret
