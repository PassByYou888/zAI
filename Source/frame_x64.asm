; ******************************************************************************
; frame_x64.asm
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
filter_coefs:
  dw 1, -5, 20, 20, -5, 1, 0, 0
vec_w_8x1:
  times 8 dw  1
vec_w_8x_5:
  times 8 dw -5
vec_w_8x16:
  times 8 dw 16
vec_w_8x20:
  times 8 dw 20
vec_d_4x16:
  times 4 dd 16
vec_d_4x512:
  times 4 dd 512


SECTION .text

cglobal filter_horiz_line_sse2
cglobal filter_vert_line_sse2
cglobal filter_hvtemp_line_sse2

; for profiling
cglobal filter_horiz_line_sse2.loop
cglobal filter_vert_line_sse2.loop
cglobal filter_hvtemp_line_sse2.loop


; filter macros

; param: 6x 16bit pixels in xmm reg
; in/out: xmm3
; tmp: xmm1
; xmm6 - filter coefs
%macro filter_horiz_w 1
    pmaddwd   %1, xmm6
    psrldq    xmm3, 4
    HADDD     %1, xmm1
    pslldq    %1, 12
    por       xmm3, %1
%endmacro


; param: add vector xmm reg, shift bits
; in/out: xmm3
; xmm7 - 0
%macro filter_scale_tmp 2
    paddd     xmm3, [%1]  ; rounding
    psrad     xmm3, %2    ; shift
    packssdw  xmm3, xmm3
    packuswb  xmm3, xmm7  ; clip
%endmacro


; procedure filter_horiz_line (src, dest: uint8_p; width: integer); cdecl;
ALIGN 16
filter_horiz_line_sse2:
    sub   r1, 2
    shr   r3, 2
    PUSH_XMM_REGS 2
    pxor      xmm7, xmm7  ; 0
    movdqa    xmm6, [filter_coefs]
.loop:
    pxor      xmm3, xmm3
    movq      xmm0, [r1]
    punpcklbw xmm0, xmm7
    movq      xmm2, [r1 + 1]
    punpcklbw xmm2, xmm7
    movq      xmm4, [r1 + 2]
    punpcklbw xmm4, xmm7
    movq      xmm5, [r1 + 3]
    punpcklbw xmm5, xmm7
    filter_horiz_w xmm0
    filter_horiz_w xmm2
    filter_horiz_w xmm4
    filter_horiz_w xmm5
    add       r1, 4

    filter_scale_tmp vec_d_4x16, 5
    movd      [r2], xmm3
    add       r2, 4
    dec       r3
    jnz .loop
    POP_XMM_REGS 2
    ret


; procedure filter_hvtemp_line_sse2 (src: int16_p; dest: uint8_p; width: integer); cdecl;
ALIGN 16
filter_hvtemp_line_sse2:
    sub   r1, 4
    shr   r3, 2
    PUSH_XMM_REGS 2
    pxor      xmm7, xmm7  ; 0
    movdqa    xmm6, [filter_coefs]
.loop:
    pxor      xmm3, xmm3
    movdqu    xmm0, [r1]
    filter_horiz_w  xmm0
    movdqu    xmm0, [r1 + 2]
    filter_horiz_w  xmm0
    movdqu    xmm0, [r1 + 4]
    filter_horiz_w  xmm0
    movdqu    xmm0, [r1 + 6]
    filter_horiz_w  xmm0
    add       r1, 8

    filter_scale_tmp vec_d_4x512, 10
    movd      [r2], xmm3
    add       r2, 4
    dec       r3
    jnz .loop
    POP_XMM_REGS 2
    ret


; param: 3x input for multiplying by hpel interp constants 1x20, 2x-5, 3x1
; xmm6 - 0
%macro madd_3bw 3
    punpcklbw %1, xmm6
    punpcklbw %2, xmm6
    punpcklbw %3, xmm6
    pmullw    %1, [vec_w_8x20]
    pmullw    %2, [vec_w_8x_5]
    pmullw    %3, [vec_w_8x1]
    paddw     %1, %2
    paddw     %1, %3
%endmacro

; procedure filter_vert_line_sse2 (src, dest: uint8_p; width: integer; stride: integer; tmpdest: psmallint); 
ALIGN 16
filter_vert_line_sse2:
; r1 src
; r2 dest
; r3 width - cycle counter
; r4 stride
; r0 src    
; r10 temp dest
    bind_param_5 r10
    shr   r3, 3          ; we are doing 8 values at time
    lea   r0, [r1 + r4]  ; positive offset source
    sub   r1, r4         ; negative offset source
    sub   r1, r4
    PUSH_XMM_REGS 1
    pxor      xmm6, xmm6  ; 0

.loop:
    ;0, -1, -2
    movq      xmm0, [r1 + 2 * r4]
    movq      xmm1, [r1 + r4]
    movq      xmm2, [r1]
    add       r1, 8
    madd_3bw  xmm0, xmm1, xmm2
    ;1, 2, 3
    movq      xmm3, [r0]
    movq      xmm1, [r0 + r4]
    movq      xmm2, [r0 + 2 * r4]
    add       r0, 8
    madd_3bw  xmm3, xmm1, xmm2
    paddw     xmm0, xmm3
    ;store to temp for filter_hvtemp_line_sse2
    movdqu    [r10], xmm0
    add       r10, 16
    
    paddw     xmm0, [vec_w_8x16]
    psraw     xmm0, 5
    packuswb  xmm0, xmm6  ; clip
    movq      [r2], xmm0
    add       r2, 8
;endloop
    dec       r3
    jnz .loop
    POP_XMM_REGS 1
    ret
