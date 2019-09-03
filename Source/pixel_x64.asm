; ******************************************************************************
; pixel_x64.asm
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
  times 8 dw  1


SECTION .text

cglobal sad_16x16_sse2
cglobal sad_8x8_mmx
cglobal sad_4x4_mmx
cglobal ssd_16x16_sse2
cglobal ssd_8x8_sse2
cglobal var_16x16_sse2

cglobal pixel_load_16x16_sse2
cglobal pixel_loadu_16x16_sse2
cglobal pixel_load_8x8_sse2
cglobal pixel_save_16x16_sse2
cglobal pixel_save_8x8_sse2

cglobal pixel_sub_4x4_mmx
cglobal pixel_add_4x4_mmx
cglobal pixel_avg_16x16_sse2

cglobal satd_4x4_mmx
cglobal satd_8x8_mmx
cglobal satd_16x16_sse2

; for profiling
cglobal ssd_16x16_sse2.loop
cglobal ssd_8x8_sse2.loop
cglobal var_16x16_sse2.loop
cglobal satd_8x8_mmx.loop
cglobal satd_16x16_sse2.loop


; SAD
; function sad_16x16_sse2(pix1, pix2: pbyte; stride: integer): integer;
ALIGN 16
sad_16x16_sse2:
    pxor  xmm4, xmm4
    pxor  xmm5, xmm5
%rep 8
    movdqu  xmm0, [r2]
    psadbw  xmm0, [r1]
    movdqu  xmm2, [r2 + r3]
    psadbw  xmm2, [r1 + 16]    
    add     r1, 32
    lea     r2, [r2 + 2 * r3]
    paddq   xmm5, xmm0
    paddq   xmm4, xmm2
%endrep
    paddq   xmm5, xmm4
    HADDQ   xmm5, xmm0
    movd    r0, xmm5
    ret

; function sad_8x8_mmx(pix1, pix2: pbyte; stride: integer): integer;
ALIGN 16
sad_8x8_mmx:
    pxor  mm5, mm5
%rep 4
    movq    mm0, [r2]
    movq    mm1, [r1]
    movq    mm2, [r2+r3]
    movq    mm3, [r1+MB_STRIDE]
    psadbw  mm0, mm1
    psadbw  mm2, mm3
    add     r1, 2*MB_STRIDE
    lea     r2, [r2 + 2 * r3]
    paddq   mm5, mm0
    paddq   mm5, mm2
%endrep
    movd    r0, mm5
    ret
    
; function sad_4x4_mmx(pix1, pix2: pbyte; stride: integer): integer;
ALIGN 16
sad_4x4_mmx:
    pxor  mm5, mm5
%rep 2
    movd    mm0, [r2]
    movd    mm1, [r1]
    movd    mm2, [r2+r3]
    movd    mm3, [r1+MB_STRIDE]
    psadbw  mm0, mm1
    psadbw  mm2, mm3
    add     r1, 2*MB_STRIDE
    lea     r2, [r2 + 2 * r3]
    paddq   mm5, mm0
    paddq   mm5, mm2
%endrep
    movd    r0, mm5
    ret
    
    
; SSD
; function ssd_16x16_sse2(pix1, pix2: pbyte; stride: integer): integer;
ALIGN 16
ssd_16x16_sse2:
    pxor  xmm4, xmm4    ; accum
    pxor  xmm5, xmm5    ; zero
    mov   r4, 16       ; counter
.loop:
    movdqa    xmm0, [r1]
    movdqa    xmm1, xmm0
    movdqu    xmm2, [r2]
    movdqa    xmm3, xmm2
    punpcklbw xmm0, xmm5
    punpckhbw xmm1, xmm5
    punpcklbw xmm2, xmm5
    punpckhbw xmm3, xmm5
    psubsw    xmm0, xmm2
    psubsw    xmm1, xmm3
    pmaddwd   xmm0, xmm0
    pmaddwd   xmm1, xmm1
    paddd     xmm4, xmm0
    paddd     xmm4, xmm1
    add   r1, 16
    add   r2, r3
    dec   r4
    jnz   .loop
    HADDD xmm4, xmm0
    movd  r0,  xmm4
    ret

; function ssd_8x8_sse2(pix1, pix2: pbyte; stride: integer): integer;
ALIGN 16
ssd_8x8_sse2:
    pxor  xmm4, xmm4    ; accum
    pxor  xmm5, xmm5    ; zero
    mov   r4, 8        ; counter
.loop:
    movq      xmm0, [r1]
    movq      xmm2, [r2]
    punpcklbw xmm0, xmm5
    punpcklbw xmm2, xmm5
    psubsw    xmm0, xmm2
    pmaddwd   xmm0, xmm0
    add       r1, 16
    add       r2, r3
    paddd     xmm4, xmm0
    dec       r4
    jnz .loop
    HADDD     xmm4, xmm0
    movd      r0, xmm4
    ret
    
; Variance
; function var_16x16_sse2(pixels: pbyte): integer;
ALIGN 16
var_16x16_sse2:
    mov   r2, 8
    pxor  xmm5, xmm5     ; sum
    PUSH_XMM_REGS 2
    pxor  xmm6, xmm6     ; sum squared
    pxor  xmm7, xmm7     ; zero
.loop:
    movdqa    xmm0, [r1]
    movdqa    xmm1, xmm0
    movdqa    xmm3, [r1+16]
    movdqa    xmm2, xmm0
    punpcklbw xmm0, xmm7
    movdqa    xmm4, xmm3
    punpckhbw xmm1, xmm7
    add       r1, 32
    punpckhbw xmm4, xmm7
    psadbw    xmm2, xmm7
    paddw     xmm5, xmm2
    movdqa    xmm2, xmm3
    punpcklbw xmm3, xmm7
    dec       r2
    psadbw    xmm2, xmm7
    pmaddwd   xmm0, xmm0
    paddw     xmm5, xmm2
    pmaddwd   xmm1, xmm1
    paddd     xmm6, xmm0
    pmaddwd   xmm3, xmm3
    paddd     xmm6, xmm1
    pmaddwd   xmm4, xmm4
    paddd     xmm6, xmm3
    paddd     xmm6, xmm4
    jnz  .loop
    HADDQ     xmm5, xmm0
    movd  r0, xmm5      ; sqr - sum * sum >> shift
    mul   r0
    HADDD     xmm6, xmm1
    shr   r0, 8
    mov   r1, r0
    movd  r0, xmm6
    sub   r0, r1
    POP_XMM_REGS 2
    ret


; Pixel Load & Store
; procedure pixel_load_16x16_sse2 (dest, src: uint8_p; stride: uint32_t);
ALIGN 16
pixel_load_16x16_sse2:
%rep 8
    movdqa  xmm0, [r2]
    movdqa  xmm1, [r2 + r3]
    lea     r2, [r2 + 2*r3]
    movdqa  [r1],    xmm0
    movdqa  [r1+16], xmm1
    add     r1, 32
%endrep
    ret
    
; pixel_loadu_16x16_sse2
ALIGN 16
pixel_loadu_16x16_sse2:
%rep 16
    movdqu xmm0, [r2]
    add    r2, r3
    movdqa [r1], xmm0
    add    r1, MB_STRIDE
%endrep
    ret

; pixel_load_8x8_sse2
ALIGN 16
pixel_load_8x8_sse2:
%rep 4
    movq  xmm0, [r2]
    movq  xmm1, [r2 + r3]
    lea     r2, [r2 + 2*r3]
    movq  [r1],    xmm0
    movq  [r1+16], xmm1
    add     r1, 32
%endrep
    ret

; procedure pixel_save_16x16_sse2 (src, dest: uint8_p; stride: uint32_t);
ALIGN 16
pixel_save_16x16_sse2:
%rep 8
    movdqa  xmm0, [r1]
    movdqa  xmm1, [r1+16]
    add     r1, 32
    movdqa  [r2],     xmm0
    movdqa  [r2+r3], xmm1
    lea     r2, [r2+2*r3]
%endrep
    ret

; pixel_save_8x8_sse2
ALIGN 16
pixel_save_8x8_sse2:
%rep 4
    movq  xmm0, [r1]
    movq  xmm1, [r1+16]
    add   r1, 32
    movq  [r2],    xmm0
    movq  [r2+r3], xmm1
    lea     r2, [r2+2*r3]
%endrep
    ret


; saturated subtraction and 8 -> 16 transport
; procedure pixel_sub_8x8_sse2(pix1, pix2: pbyte; diff: int16_p);
ALIGN 16
pixel_sub_4x4_mmx:
    pxor  mm5, mm5
%rep 4
    movd      mm0, [r1]
    movd      mm1, [r2]
    punpcklbw mm0, mm5
    punpcklbw mm1, mm5
    psubw     mm0, mm1
    add   r1, 16
    add   r2, 16
    movq      [r3], mm0
    add   r3, 8
%endrep
    ret

; saturated addition and 16 -> 8 transport
; procedure pixel_add_8x8_sse2(pix1, pix2: pbyte; diff: int16_p);
ALIGN 16
pixel_add_4x4_mmx:
    pxor  mm5, mm5
%rep 4
    movd      mm0, [r2]
    movq      mm1, [r3]
    punpcklbw mm0, mm5
    paddw     mm0, mm1
    add   r2, 16
    add   r3, 8
    packuswb  mm0, mm5
    movd      [r1], mm0
    add   r1, 16
%endrep
    ret

; procedure pixel_avg_16x16_sse2(src1, src2, dest: uint8_p; stride: integer);
ALIGN 16
pixel_avg_16x16_sse2:
%rep 8
    movdqu  xmm0, [r1]
    movdqu  xmm1, [r2]
    movdqu  xmm2, [r1+r4]
    movdqu  xmm3, [r2+r4]
    lea     r1, [r1 + 2*r4]    
    lea     r2, [r2 + 2*r4]  
    pavgb   xmm0, xmm1
    pavgb   xmm2, xmm3
    movdqa  [r3], xmm0
    movdqa  [r3 + MB_STRIDE], xmm2
    add     r3, 2*MB_STRIDE
%endrep
    ret
    
   
; ******************************************************************************
; SATD

;subtract two 4x4 blocks, return difference
; 1, 2 - pixel address
; 3 - stride
; 4 - address offset
; output:  m0..m3
; scratch: m5, m7
%macro mSUB4x4 4
    pxor      mm7, mm7
    mov_m2    mm0, [%1 + %4]
    mov_m2    mm5, [%2 + %4]
    punpcklbw mm0, mm7
    punpcklbw mm5, mm7
    mov_m2    mm1, [%1 + 16 + %4]
    mov_m2    mm6, [%2 + %3 + %4]
    punpcklbw mm1, mm7
    punpcklbw mm6, mm7
    psubw     mm0, mm5
    psubw     mm1, mm6

    mov_m2    mm2, [%1 +   32 + %4]
    mov_m2    mm5, [%2 + %3*2 + %4]
    add   %2, %3
    punpcklbw mm2, mm7
    punpcklbw mm5, mm7
    mov_m2    mm3, [%1 +   48 + %4]
    mov_m2    mm6, [%2 + %3*2 + %4]
    punpcklbw mm3, mm7
    punpcklbw mm6, mm7
    psubw     mm2, mm5
    psubw     mm3, mm6
    sub   %2, %3
%endmacro


; transpose 4x4 matrix of int16-s
; in/out: m0..3
; scratch: m5, m6
%macro TRANSPOSE_4x4_int16 0
    movq      mm5, mm0
    movq      mm6, mm2
    punpcklwd mm0, mm1
    punpcklwd mm2, mm3
    punpckhwd mm5, mm1
    punpckhwd mm6, mm3
    movq      mm1, mm0
    punpckldq mm0, mm2
    punpckhdq mm1, mm2
    movq      mm2, mm5
    movq      mm3, mm5
    punpckldq mm2, mm6
    punpckhdq mm3, mm6
%endmacro


;partially transpose one 4x4 matrix using xmmregs
; out: 2 rows merged in %1 / %3
%macro mTRANSPOSE4_xmm 4
    movdqa  %3, %1
    punpcklwd %1, %2
    punpckhwd %3, %2
    pshufd %1, %1, 216
    pshufd %3, %3, 216
%endmacro

;transpose 2x 4x4 matrix using xmmregs
; in/out: m0..3
; scratch: m5..7
%macro mTRANSPOSE4x2_xmm 0
    movdqa xmm5, xmm0
    movdqa xmm6, xmm1
    movdqa xmm7, xmm2
    
    punpckldq xmm0, xmm2
    punpckldq xmm1, xmm3
    mTRANSPOSE4_xmm xmm0, xmm1, xmm2, xmm3
    punpckhdq xmm5, xmm7
    punpckhdq xmm6, xmm3
    mTRANSPOSE4_xmm xmm5, xmm6, xmm7, xmm3
    
    movdqa xmm1, xmm0 
    punpcklqdq  xmm0, xmm5   ; b|a + b1|a1 -> a1|a  
    punpckhqdq  xmm1, xmm5
    movdqa xmm3, xmm2
    punpcklqdq  xmm2, xmm7
    punpckhqdq  xmm3, xmm7
%endmacro


;hadamard transform for 4x4 matrix
; in/out (in reverse order):  m0..m3
; scratch: m7
%macro mHADAMARD4 0
    mov_m  mm7, mm1
    paddw  mm1, mm3  ; e1 = c + a
    psubw  mm3, mm7  ; e2 = a - c
    mov_m  mm7, mm0
    paddw  mm0, mm2  ; f1 = d + b
    psubw  mm2, mm7  ; f2 = b - d
    mov_m  mm7, mm0
    paddw  mm0, mm1  ; g1 = f1 + e1
    psubw  mm1, mm7  ; g2 = e1 - f1
    mov_m  mm7, mm2
    paddw  mm2, mm3  ; h1 = f2 + e2
    psubw  mm3, mm7  ; h2 = e2 - f2
%endmacro


;absolute value
; 1, 2 - input regs
; scratch: m6, m7
%macro mPABS_2 2
    pxor   mm7, mm7
    pxor   mm6, mm6
    psubw  mm7, %1
    psubw  mm6, %2
    pmaxsw %1, mm7
    pmaxsw %2, mm6
%endmacro


;sum words to 2 doublewords
; in: m0..3
; out: m0
%macro mSUM 0
    mPABS_2 mm0, mm1
    paddw   mm0, mm1
    mPABS_2 mm2, mm3
    paddw   mm2, mm3
    paddw   mm0, mm2
    pmaddwd mm0, [vec_w_8x1]
%endmacro


;sum N dwords, move to result
; 1 - input mmreg, 2 - result reg
; scratch: m7
%macro SUM2DW 2
    movq  mm7, %1
    psrlq mm7, 32
    paddd %1, mm7
    movd  %2, %1
%endmacro

%macro SUM4DW 2
    HADDD %1, xmm7
    movd  %2, %1
%endmacro


; SATD mmx
%define mov_m  movq
%define mov_m2 movd

; function satd_4x4_mmx  (pix1, pix2: pbyte; stride: integer): integer;
ALIGN 16
satd_4x4_mmx:
    PUSH_XMM_REGS 2
    mSUB4x4 r1, r2, r3, 0
    mHADAMARD4
    TRANSPOSE_4x4_int16
    mHADAMARD4
    mSUM
    SUM2DW mm0, r0
    POP_XMM_REGS 2
    ret


; function satd_8x8_mmx  (pix1, pix2: pbyte; stride: integer): integer;
ALIGN 16
satd_8x8_mmx:
    mov   r4, 2
    pxor  mm4,mm4  ; sum
    PUSH_XMM_REGS 2
.loop:
    %assign i 0
    %rep 2
        mSUB4x4 r1, r2, r3, i
        mHADAMARD4
        TRANSPOSE_4x4_int16
        mHADAMARD4
        mSUM
        paddd   mm4, mm0
        %assign i i + 4
    %endrep
    lea   r1, [r1 + 4 * 16]
    lea   r2, [r2 + 4 * r3]
    dec   r4
    jnz   .loop
    SUM2DW mm4, r0
    POP_XMM_REGS 2
    ret


; function satd_16x16_sse2  (pix1, pix2: pbyte; stride: integer): integer;
%define mov_m  movdqa
%define mov_m2 movq
%define mm0 xmm0
%define mm1 xmm1
%define mm2 xmm2
%define mm3 xmm3
%define mm4 xmm4
%define mm5 xmm5
%define mm6 xmm6
%define mm7 xmm7

ALIGN 16
satd_16x16_sse2:
    mov   r4, 4
    pxor  mm4, mm4  ; sum
    PUSH_XMM_REGS 2
.loop:
    %assign i 0
    %rep 2
        mSUB4x4 r1, r2, r3, i
        mHADAMARD4
        mTRANSPOSE4x2_xmm
        mHADAMARD4
        mSUM
        paddd   mm4, mm0
        %assign i i + 8
    %endrep
    lea   r1, [r1 + 4 * 16]
    lea   r2, [r2 + 4 * r3]
    dec   r4
    jnz   .loop
    SUM4DW mm4, r0
    POP_XMM_REGS 2
    ret

