; ******************************************************************************
; motion_comp_x64.asm
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
vec_w_8x32:
  times 8 dw 32  
  

SECTION .text

; all mc functions assume that dest is aligned with 16 byte stride
cglobal mc_chroma_8x8_sse2
cglobal mc_chroma_8x8_sse2.loop


; move any byte from reg to mmreg
; 1 - mmreg dest, 2 - reg src, 3 - reg byte position[0..3], 4 - reg scratch
%macro get_coef 4
    mov   %4, %2
%if %3 > 0    
    shr   %4, 8 * %3
%endif
    and   %4, 0xff
    movd  %1, %4
%endmacro

; spread 1xDW value in mmreg to 8xW values
%macro spread_coef 2
    pshufd    %1, %1, 0
    pshufd    %2, %2, 0
    packssdw  %1, %1
    packssdw  %2, %2
%endmacro

; procedure mc_chroma_8x8_sse2 (src, dst: pbyte; const stride: integer; coef: pbyte);
; coef: array[4] of byte
; treats xmm0-7 as volatile!
; todo - use one more xmm
ALIGN 16
mc_chroma_8x8_sse2:
    mov   r4, [r4]
    PUSH_XMM_REGS 2
    pxor  xmm7, xmm7
    get_coef  xmm0, r4, 0, r0
    get_coef  xmm1, r4, 1, r0
    get_coef  xmm2, r4, 2, r0
    get_coef  xmm3, r4, 3, r0   
    spread_coef xmm0, xmm1
    spread_coef xmm2, xmm3
    ; mc
    mov   r4, 8
.loop:
    ; A B
    movq      xmm4, [r1]    ; A
    movq      xmm5, [r1 +1] ; B
    movq      xmm6, [r1+r3] ; C   
    
    punpcklbw xmm4, xmm7
    punpcklbw xmm6, xmm7
    punpcklbw xmm5, xmm7
    
    pmullw    xmm4, xmm0
    pmullw    xmm5, xmm1
    pmullw    xmm6, xmm2
    
    paddw     xmm4, xmm5 ; A + B
    
    movq      xmm5, [r1+r3+1]  ; D
    paddw     xmm4, [vec_w_8x32] ; A + 32
    punpcklbw xmm5, xmm7
    pmullw    xmm5, xmm3
    paddw     xmm4, xmm6 ; A + C
    paddw     xmm4, xmm5 ; A + D

    add   r1, r3
    
    psraw     xmm4, 6
    packuswb  xmm4, xmm7
    movq      [r2], xmm4
    add   r2, MB_STRIDE 
    dec   r4
    jnz   .loop
    POP_XMM_REGS 2
    ret
