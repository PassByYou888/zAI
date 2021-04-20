; ******************************************************************************
; x64inc.asm
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

BITS 64
DEFAULT REL

%define WIN64 0
%ifidn __OUTPUT_FORMAT__,win64
    %define WIN64 1
%endif

; add underscore prefix to globals for C linkage
%macro cglobal 1
    global %1
%endmacro

; horizontal add double
; 1 - mmreg src/dest, 2 - mmreg scratch
%macro HADDD 2
    movhlps %2, %1
    paddd   %1, %2
    pshuflw %2, %1, 0xE
    paddd   %1, %2
    ; ssse3
    ;pshufd  %1, %1, 8  ; 0.0.3.0.
    ;phaddd  %1, %2
%endmacro

; horizontal add quad
; 1 - mmreg src/dest, 2 - mmreg scratch
%macro HADDQ 2
    movhlps %2, %1
    paddq   %1, %2
%endmacro

%define MB_STRIDE 16

; win64 function params: RCX, RDX, R8, R9
; linux64: RDI, RSI, RDX, RCX, R8, R9
; common volatile regs: RAX, R10, R11, XMM0..XMM5 
; return value: RAX/EAX
; 32b regcall: EAX, EDX, ECX
%define r0 rax
%ifdef WIN64
  %define r1 rcx
  %define r2 rdx
  %define r3 r8
  %define r4 r9
  ; xmm regs from xmm6 up aren't volatile on win64
  %macro PUSH_XMM_REGS 1
    movdqa [rsp +  8], xmm6
    %if %1 > 1
      movdqa [rsp + 24], xmm7
    %endif
    %if %1 > 2
      %error cannot push more than 2 XMM regs
    %endif
    %define PUSHED_XMM_REGS %1
  %endmacro
  %macro POP_XMM_REGS 1
    %ifnidn PUSHED_XMM_REGS, %1
      %error number of pushed and popped XMM regs does not match
    %endif
    movdqa xmm6, [rsp +  8]
    %if %1 > 1
      movdqa xmm7, [rsp + 24]
    %endif
    %if %1 > 2
      %error cannot pop more than 2 XMM regs
    %endif
    %undef PUSHED_XMM_REGS
  %endmacro
%else
  %define r1 rdi
  %define r2 rsi
  %define r3 rdx
  %define r4 rcx
  %macro PUSH_XMM_REGS 1
  %endmacro
  %macro POP_XMM_REGS 1
  %endmacro
%endif

; bind function parameter to desired register
; win64 5th param on stack
; linux64 5th param in r8
%macro bind_param_5 1
  %ifdef WIN64
    mov  %1, [rsp + 5*8]  ; skip 32B shadow space
  %else
    %define %1 r8
  %endif
%endmacro
