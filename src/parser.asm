; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>
;
; SPDX-License-Identifier: Apache-2.0

    INCLUDE "zos_sys.asm"
    INCLUDE "zos_err.asm"
    INCLUDE "assembler_macros_h.asm"


    MACRO CHECK_LABEL_OR_CONSTANT _
        LOCAL is_label
        cp ASSEMBLE_LABEL_NAME
        jp z, is_label
        cp ASSEMBLE_CONSTANT_N
        jp nz, _assemble_instr_operand_error
    is_label:
    ENDM

    MACRO NEW_INSTRUCTION name, operand_count, instruction_code, routine
        DEFS 4, name          ; instruction name, maximum 4 chars
        DEFB operand_count    ; operand count
        DEFW instruction_code ; instruction's first opcodes
        DEFW routine          ; routine to jump to
    ENDM

    DEFC ASSEMBLE_VARIABLE_COUNT = 0x80

    SECTION TEXT

    ; Convert the given instruction (string) to tokens that can be used
    ; for parsing.
    ; Writes in _assemble_instr_str and _assemble_reg*_str
    ; the proper strings. Returns B, the number of string written
    ; Parameters:
    ;   HL - Line to parse, must be NULL-terminated
    ; Returns:
    ;   B - Number of string written
parser_split_line:
    ; Remove the comment if there is any
    ld a, ';'
    call strsep
    ; Check if there is a label defined
    call parser_label_defined
    ; Return right now in case of an error
    ld a, b
    or a
    ret nz

    call strltrim
    ; Prepare the return value
    ld b, 0
    ; Check if we still have some characters in the string
    ld a, (hl)
    or a
    ret z
    ; Check if it is a directive
    cp '.'
    jp z, parse_directive_split_line
    call strtolower
    ; Separate the instruction into maximum 3 parts: INS R,R'
    ld a, ' '
    call strsep
    ; Save the current operand
    ld (_assemble_instr_str), hl
    ; If A is not 0, then the delimiter was not found
    inc b
    or a
    ret nz
    ; Look for the first operand
    ex de, hl   ; Put the next string into HL
    call strltrim   ; Trim it and cut where ',' is
    ld a, ','
    call strsep
    ; Save the second operand
    ld (_assemble_reg1_str), hl
    ; If A is not 0, then the delimiter was not found
    or a
    jr nz, _assemble_one_operand
    ; Trim the last operand
    ex de, hl
    call strltrim
    call strrtrim
    ld (_assemble_reg2_str), hl
_assemble_two_operand:
    inc b
_assemble_one_operand:
    inc b
_assemble_no_operand:
    ret


    ; Parse a line that contains a directive (starting with '.')
    ; Parameters:
    ;   HL - Directive name, including '.'
    ; Returns:
parse_directive_split_line:
    ; Split the directive from the parameter
    ld a, ' '
    call strsep
    ; If A is not 0, the directive is invalid, all the supported directives have a parameter
    or a
    jr nz, _invalid_directive
    ; DE contains the parameter.
    ; The directive was already left-trimmed by the caller, we also know that the first
    ; character was a '.', so jump to the second character
    call strrtrim
    call strtolower
    inc hl
    ld a, (hl)
    sub 'd'
    jr nz, _invalid_directive
    ; Make sure there is only one character remaining after it
    inc hl
    ld b, (hl)
    inc hl
    ; A is 0 is, for sure, make sure HL points to the end of the string
    or (hl)
    jr nz, _invalid_directive
    ; B contains the letter of the directive, trim the parameter before jumping to
    ; the directive routine
    ex de, hl
    call strltrim
    call strrtrim
    ; Check the directive
    ld a, b
    cp 'm'
    jr z, _assemble_direct_dm
    cp 'b'
    jr z, _assemble_direct_db
    cp 'w'
    jr z, _assemble_direct_dw
    cp 's'
    jr z, _assemble_direct_ds
    ; Unknown directive else
_invalid_directive:
    ld b, ASSEMBLE_UNKNOWN_DIRECTIVE
    ret


    ; DB directive, used to define a single byte
    ; Parameters:
    ;   HL - Parameter of the directive (trimmed by caller)
    ; Returns:
    ;   B - 0 on success, error code else
_assemble_direct_db:
    call parse_int
    ; A must be 0, else it's an invalid operand
    or a
    jr nz, _assemble_invalid_operand
    ; Make sure H is 0, else it's a 16-bit value
    or h
    ; Value is valid, we can add it to the binary
    ld a, l
    jp z, assemble_def_byte
_assemble_invalid_operand:
    ld b, ASSEMBLE_INVALID_OPERAND
    ret


    ; DW directive, used to define a 16-bit word
    ; Parameters:
    ;   HL - Parameter of the directive (trimmed by caller)
    ; Returns:
    ;   B - 0 on success, error code else
_assemble_direct_dw:
    call parse_int
    ; A must be 0, else it's an invalid operand
    or a
    ; HL contains the 16-bit word
    jp z, assemble_def_word
    jr _assemble_invalid_operand


    ; DM directive, used to define strings
    ; Parameters:
    ;   HL - Parameter of the directive (trimmed by caller)
    ; Returns:
    ;   B - 0 on success, error code else
_assemble_direct_dm:
    call parse_str
    ; A must be 0, else it's an invalid operand
    or a
    ; HL contains the 16-bit word
    jp z, assemble_def_message
    jr _assemble_invalid_operand


    ; DS directive, used to define space
    ; Parameters:
    ;   HL - Parameter of the directive (trimmed by caller)
    ; Returns:
    ;   B - 0 on success, error code else
_assemble_direct_ds:
    call parse_int
    ; A must be 0, else it's an invalid operand
    or a
    ; HL contains the 16-bit word
    jp z, assemble_def_space
    jr _assemble_invalid_operand


    ; Manage the ORG directive. This one is a bit special since it doesn't require the
    ; '.' in the front. It is treated as an instruction!
    ; Returns:
    ;   B - 0 on success, error code else
_assemble_direct_org:
    ld hl, (_assemble_reg1_str)
    call _assemble_operand_hl
    ;   C - First parameter code (if applicable)
    ;   DE - Constant (if applicable)
    ld a, c
    cp ASSEMBLE_CONSTANT_N
    jp z, assemble_set_org
    jp _assemble_instr_operand_error


    ; Check if a label was defined like "_label:" on the current line
    ; Parameters:
    ;   HL - Current line (string)
    ; Returns:
    ;   HL - Address of the string that follows the label
    ;   (_assemble_def_label) - Set to the address of the label name
    ;   B - 0 on success, ASSEMBLE_SYNTAX_ERROR on error
parser_label_defined:
    ld a, ':'
    call strsep
    ; A is 0 if we found ':', positive value else. We have to
    ; return A = 0 if we didn't find anything. HL was not altered.
    or a
    ld b, 0
    ret nz
    ; Check that the label is correct and doesn't have invalid character
    call strltrim
    call strrtrim
    push de ; Keep the "next" string, that follows the label
    call parser_label_is_valid
    pop de
    ; If B is not 0 (error), return
    ld a, b
    or a
    ret nz
    ; Check the length of the label
    call strlen
    ; B is 0 for sure here
    ld a, c
    cp ASSEMBLE_MAX_LABEL_LENGTH + 1
    jr nc, parser_label_too_long
    ; The label is valid, save it!
    ld (_assemble_def_label), hl
    ex de, hl
    ret
parser_label_too_long:
    ld b, ASSEMBLE_LABEL_TOO_LONG
    ret
parser_label_defined_invalid_syntax_pop:
    pop hl
parser_label_defined_invalid_syntax:
    ld b, ASSEMBLE_SYNTAX_ERROR
    ret


    ; Check if the label in HL is a valid label. A valid label respects the following Regex:
    ; [a-zA-Z_][a-zA-Z0-9_]+
    ; Parameters
    ;   HL - Address of the label (string)
    ; Returns:
    ;   B - 0 on success, error code else
    ; Alters:
    ;   A
parser_label_is_valid:
    push hl
    ; Make sure the string is not empty
    ld a, (hl)
    or a
    jr z, parser_label_defined_invalid_syntax_pop
    ; The first character must be an alpha or underscore
    cp '_'
    jr z, _parser_label_valid_loop
    call is_alpha
    ; On carry, the character is not alpha
    jr c, parser_label_defined_invalid_syntax_pop
_parser_label_valid_loop:
    inc hl
    ld a, (hl)
    or a
    jr z, _parser_label_valid_ret
    cp '_'
    jr z, _parser_label_valid_loop
    call is_alpha_numeric
    jp nc, _parser_label_valid_loop
    ; Character is not _ nor alpha numeric, return an error
    jr parser_label_defined_invalid_syntax_pop
_parser_label_valid_ret:
    ; A is 0 here, return B = 0 too
    pop hl
    ld b, a
    ret

    ; This function will parse the string-tokens contained in the
    ; _assemble_instr_str and _assemble_reg*_str RAM values.
    ; Parameters:
    ;   HL - Address of the line to parse
    ; Returns:
    ;   B - If positive, number of operands (e.g. 1 for "ldi", 2 for "inc b", 3 for "ld a, b")
    ;       If negative, an error occurred
    ; Alters:
    ;   A, BC, DE, HL
    PUBLIC parser_parse_line
parser_parse_line:
    ; Clean the label address, it is enough to clean the upper byte
    xor a
    ld (_assemble_def_label + 1), a
    ld (_assemble_ref_label + 1), a
    call parser_split_line
    ; If we haven't written any string, the line was empty (or a comment or a label)
    ; return directly. If there was an error, return directly too.
    ld a, b
    or a
    ret z
    cp ASSEMBLE_ERROR
    ret nc
    ; Get the current instruction in DE
    ld de, (_assemble_instr_str)
    ; Calculate the address of the first instruction to compare to
    ld a, (de)
    ; Make sure A is a-x (no instruction starts with y or z)
    cp 'a' - 1
    jr c, _assemble_parse_syntax_error
    cp 'y'
    jr nc, _assemble_parse_syntax_error
    ; First character is valid, look for the instructions label in the table
    ld hl, _assemble_instructions_table
    ; Calculate A = (A - 'a') * 2
    sub 'a'
    rlca
    add l
    ld l, a
    adc h
    sub l
    ld h , a
    ; Dereference the start address
    ld a, (hl)
    inc hl
    ld h, (hl)
    ; Make sure it's not 0
    ld l, a
    or h
    jr z, _assemble_parse_syntax_error
_assemble_parse_line_loop:
    ld a, 4
    call strncmp_opt
    or a
    ; A is 0, we just found the instruction!
    jr z, _assemble_parse_line_found
    ; If A is negative, HL is bigger, which means we didn't find the instruction
    jp m, _assemble_parse_syntax_error
    ; Go to the next entry
    ld a, ASSEMBLE_INSTR_ENTRY_SIZE
    add l
    ld l, a
    adc a, h
    sub l
    ld h, a
    jp _assemble_parse_line_loop
_assemble_parse_syntax_error:
    ld b, ASSEMBLE_SYNTAX_ERROR
    ret

_assemble_parse_line_found:
    ; Check the operand number for the instruction and then
    ; Jump to the instruction parser routine, which is 5 bytes away
    ; from the string address (in HL)
    inc hl
    inc hl
    inc hl
    inc hl
    ; Check operand count for the instruction if it is not variable (i.e. bit 7 = 1)
    ld a, (hl)
    bit 7, a
    jp nz, _assemble_skip_operand_check
    sub b
    jp nz, _assemble_invalid_operand_number
_assemble_skip_operand_check:
    ; Operand count is now in B, don't change it
    ; Load the instruction code (at most 2 bytes) in DE
    inc hl
    ld e, (hl)
    inc hl
    ld d, (hl)
    ; Load the routine address (16-bit load)
    inc hl
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    jp (hl)

_assemble_invalid_operand_number:
    ld b, ASSEMBLE_INVALID_OPERAND
    ret


    ; HLDE contains the instruction code:
    ;   If the instruction is only one byte long, the code will be in E
    ;   If the instruction is two bytes long, the code will be in DE
    ;   If the instruction is three bytes long, the code will be in LDE
    ;   If the instruction is four bytes long, the code will be in HLDE
    ; Parameters:
    ;   B  - 0
    ;   DE - 2 bytes of the instruction (provided in the R/O array)
    ; Returns:
    ;   B - Number of bytes for the instruction
    ;   HLDE - instruction bytes

    ; Routines for parsing LD instruction
_assemble_instr_ld:
    ld hl, (_assemble_reg1_str)
    call _assemble_operand_hl
    ; Backup the constant if necessary
    ld hl, _assemble_instr_ld_backup_constant
    ld (hl), e
    inc hl
    ld (hl), d
    ; C contains the code of the operand
    push bc
    ld hl, (_assemble_reg2_str)
    call _assemble_operand_hl
    ld h, c
    pop bc
    ld b, c
    ld c, h
    ; Check if the operand is an 8-bit register
    ld a, b
    ; A is < ASSEMBLE_REGISTER_MAX if the operand is a register
    cp ASSEMBLE_REGISTER_MAX
    jp c, _assemble_instr_ld_r

    ; Check if the operand is a 16-bit register
    and ASSEMBLE_REG16_MASK
    ld a, b
    jp nz, _assemble_instr_ld_rr

    ; Check if the operand is a memory load
    and ASSEMBLE_MEMACC_MASK
    jp nz, _assemble_instr_ld_mem

_assemble_instr_ld_error:
_assemble_instr_operand_error:
    ; Else... the operand is something else, return an error
    ld b, ASSEMBLE_INVALID_OPERAND
    ret

_assemble_instr_ld_r:
    ; B now contains the first operand, C contains the second
    ; Check if the first operand is a special register (I or R)
    ld a, b
    cp ASSEMBLE_REGISTER_SPECIAL
    jp nc, _assemble_instr_ld_special
    ; B (1st operand) is a regular register, check the 2nd operand
    ; First, check if it's an 8-bit register
    ld a, c
    cp ASSEMBLE_REGISTER_MAX
    jp c, _assemble_instr_ld_r_r
    ; The second operand is not an 8-bit register
    ; Check if it's an 8-bit constant
    cp ASSEMBLE_CONSTANT_N
    jp z, _assemble_instr_ld_r_n
    ; Check if it's a memory access
    bit ASSEMBLE_MEMACC_BIT, a
    jp nz, _assemble_instr_ld_r_mem
    jp _assemble_instr_operand_error

    ; Parser for LD r, r'
_assemble_instr_ld_r_r:
    ; Check if the second operand is a special register
    ld a, c
    cp ASSEMBLE_REGISTER_SPECIAL
    jp nc, _assemble_instr_ld_r_special
    ; It is not, it is a regular register!
    ld e, 0x40
    ld a, b ; Shift first operand 3 times
    add a
    add a
    add a
    or e    ; Put the opcode
    or c    ; Second operand
    ld e, a ; E = 0b01_rrr_r'r'r'
    ld b, 1 ; This instruction is one byte long, done!
    ret

    ; Parser for LD r, n
_assemble_instr_ld_r_n:
    ; N is contained in (D)E. Verify that N is less than 256!
    ld a, d
    or a
    jr nz, _assemble_instr_outofrange_error
    ; N is a byte, we can proceed. Shift the register index by 3
    ld a, b
    add a
    add a
    add a
    or 0x6  ; LD r, n = 0b00_rrr_110
    ld d, a ; D contains the opcode, E contains the byte
    ld b, 2 ; 2 bytes for this instruction
    ret
_assemble_instr_outofrange_error:
    ld b, ASSEMBLE_OUT_OF_RANGE
    ret

    ; Parser for LD r, (mem)
_assemble_instr_ld_r_mem:
    ; B and C contain the 1st and 2nd operand respectively
    ; If the second operand is (HL), (IX) or (IY), no need to check the 1st one
    ld a, c
    cp ASSEMBLE_REGISTER_MEMHL
    jp z, _assemble_instr_ld_r_memhl
    cp ASSEMBLE_REGISTER_MEMIX
    jp z, _assemble_instr_ld_r_memhl
    cp ASSEMBLE_REGISTER_MEMIY
    jp z, _assemble_instr_ld_r_memhl
    ; The second operand is not (HL), then the first one
    ; must be A
    ld a, b
    cp ASSEMBLE_REGISTER_A
    jp nz, _assemble_instr_operand_error
_assemble_instr_ld_a_mem:
    ; It is A indeed, the 2nd operand must be
    ; (BC), (DE) or (NNNN)
    ld a, c
    ; Set the instruction to 1 by default
    ld b, 1
    cp ASSEMBLE_REGISTER_MEMBC
    jp z, _assemble_instr_ld_a_membc
    cp ASSEMBLE_REGISTER_MEMDE
    jp z, _assemble_instr_ld_a_memde
    cp ASSEMBLE_REGISTER_MEMNN
    jp z, _assemble_instr_ld_a_memnn
    jp _assemble_instr_operand_error
_assemble_instr_ld_a_membc:
    ld e, 0x0a
    ret
_assemble_instr_ld_a_memde:
    ld e, 0x1a
    ret
_assemble_instr_ld_a_memnn:
    ld b, 3
    ; Instructions must be in HLDE
    ld l, 0x3a
    ; Little-endian, invert D and E
    ld a, d
    ld d, e
    ld e, a
    ret

    ; Parser for LD r, (HL/IX/IY)
_assemble_instr_ld_r_memhl:
    ; Get the 1st operand (register)
    ld a, b
    ; Generate the instruction from it byt shifting it
    ; and or-ing 0x46, as LD r, (HL) = 0b01_rrr_110
    add a
    add a
    add a
    or 0x46
    ; C still contains the 2nd operand, check if it's IX or IY
    ld h, e ; Store potential IX/IY parameter in H
    ld e, a
    ld a, c
    ; One byte only for if (HL) is the 2nd operand
    ld b, 1
    sub ASSEMBLE_REGISTER_MEMIX
    ret c
    ; Update b
    ld b, 3
    ; Set instruction's byte 0 and 1 before choosing byte 2
    ld l, 0xdd
    ld d, e
    ld e, h
    ret z
    ; IY, so update l register
    ld l, 0xfd
    ret

    ; Parser for LD A,I and LD A,R
_assemble_instr_ld_r_special:
    ; LD x,I/R is only possible if x is A
    ld a, b
    cp ASSEMBLE_REGISTER_A
    jp nz, _assemble_instr_operand_error
    ; The first operand is A!
    ld de, 0xed57   ; Code for LD A,I
    ld b, 2
    ; Modify the code if second operand is R
    ld a, c
    cp ASSEMBLE_REGISTER_R
    ret nz  ; We can return if the second operand is I
    ld e, 0x5f ; Opcode for LD R,A = 0xed4f
    ret

    ; Parser for LD I,A and LD R,A
_assemble_instr_ld_special:
    ; Trick: get instructions for LD A,I and LD R,A
    ; and then fix the resulted code
    ld a, b
    ld b, c
    ld c, a
    call _assemble_instr_ld_r_special
    bit 7, b
    ret nz
    ; Patch the code: subtract 16 to the last byte
    ld a, e
    sub 0x10
    ld e, a
    ret

    ; Parser for LD (mem), xxx
_assemble_instr_ld_mem:
    ; Check that we don't have a special register
    ; in the 2nd operand
    ld a, c
    cp ASSEMBLE_REGISTER_I
    jp z, _assemble_instr_operand_error
    cp ASSEMBLE_REGISTER_R
    jp z, _assemble_instr_operand_error

    ; Check if the 1st parameter is HL, IX or IY
    ld a, b
    cp ASSEMBLE_REGISTER_MEMHL
    jp z, _assemble_instr_ld_mem_r
    cp ASSEMBLE_REGISTER_MEMIX
    jp z, _assemble_instr_ld_mem_r
    cp ASSEMBLE_REGISTER_MEMIY
    jp z, _assemble_instr_ld_mem_r

    ; Check for A as a 2nd operand
    ld a, c
    cp ASSEMBLE_REGISTER_A
    jp z, _assemble_instr_ld_mem_a

    ; Check for (nn) as a 1st operand
    ld a, b
    cp ASSEMBLE_REGISTER_MEMNN
    jp z, _assemble_instr_ld_mem_nn
    ret

    ; Parser for LD (HL/IX/IY), r
_assemble_instr_ld_mem_r:
    ; Make sure the 2nd operand is not a constant, else jump
    ld a, c
    cp ASSEMBLE_CONSTANT_N
    jp z, _assemble_instr_ld_mem_n
    push bc ; Save operands
    ; The trick is to generate the instructions
    ; for LD R, (HL/IX/IY) and then correct the only
    ; instruction that needs to be corrected
    ld a, b
    ld c, b
    ld b, a
    call _assemble_instr_ld_r_memhl
    ; Instructions in HLDE, B number of bytes
    ld a, b
    pop bc
    ; We only need the 2nd operand which is in C
    ld b, a
    ld a, c
    or 0x70 ; LD (HL), r = 01110_rrr
    ld c, a
    ; If B is 1, replace E, if b is 3, replace D
    ld a, b
    dec a
    jp z, _assemble_instr_ld_mem_r_hl
    ld d, c
    ret
_assemble_instr_ld_mem_r_hl:
    ld e, c
    ret

    ; Parser for LD (HL|IX|IY), n
_assemble_instr_ld_mem_n:
    ld a, b
    cp ASSEMBLE_REGISTER_MEMHL
    jp z, _assemble_instr_ld_memhl_n
    cp ASSEMBLE_REGISTER_MEMIX ; Keep the flags
    ld b, 4
    ld hl, 0xdd36
    ld a, (_assemble_instr_ld_backup_constant)
    ld d, a
    ; E is already set
    ret z
    ; IY, update the highest byte
    ld h, 0xfd
    ret
_assemble_instr_ld_memhl_n:
    ld b, 2
    ld d, 0x36
    ; ld e, e ; the constant is already in E
    ret

    ; Parser for LD (mem), A
_assemble_instr_ld_mem_a:
    ; The trick here is the same as above:
    ; we can generate the code for LD A, (mem) and
    ; then patch it as we simply need to subtract 8 to the opcode
    ld a, b
    ld b, c
    ld c, a
    call _assemble_instr_ld_a_mem
    ; Check for error
    bit 7, b
    ret nz

    ld a, b
    dec a
    jp z, _assemble_instr_ld_mem_a_one_byte
    ; LD A, (nn)
    ld a, l
    sub 8
    ld l, a
    ret
_assemble_instr_ld_mem_a_one_byte:
    ; LD A, (DE|BC)
    ld a, e
    sub 8
    ld e, a
    ret

    ; Parser for LD rr, xx
_assemble_instr_ld_rr:
    ; If the 2nd operand is a register, the 1st
    ; must be SP
    bit ASSEMBLE_REG16_BIT, c
    jp nz, _assemble_instr_ld_rr_dd

    ld a, c
    ; Check for a 16-bit memory access
    cp ASSEMBLE_REGISTER_MEMNN
    jp z, _assemble_instr_ld_rr_memnn
    ; Check for a 16-bit constant
    CHECK_LABEL_OR_CONSTANT()
    ; Parser for LD dd,nn; LD IX, nn; LD IY,nn
_assemble_instr_ld_rr_nn:
    ; Invert the constant value (little-endian)
    ld a, d
    ld d, e
    ld e, a
    ; Check the 1st operand
    ld a, b
    cp ASSEMBLE_REGISTER_SP + 1
    jp c, _assemble_instr_ld_dd_nn
    cp ASSEMBLE_REGISTER_IY + 1
    jp c, _assemble_instr_ld_ixy_nn
    jp _assemble_instr_operand_error
_assemble_instr_ld_dd_nn:
    ; Generate the opcode
    and ASSEMBLE_REG16_MASK - 1
    rrca
    rrca
    rrca
    rrca    ; LD dd, nn = 00_dd_0001
    inc a
    ld l, a ; Load a into the returned register
    ld b, 3
    ret
_assemble_instr_ld_ixy_nn:
    ld b, 4
    ld h, 0xdd
    ld l, 0x21
    cp ASSEMBLE_REGISTER_IX
    ret z
    ld h, 0xfd
    ret

    ; Parser for:
    ; LD HL,(nn)
    ; LD dd,(nn)
    ; LD IX,(nn)
    ; LD IY,(nn)
_assemble_instr_ld_rr_memnn:
    ; Invert the constant value (little-endian)
    ld a, d
    ld d, e
    ld e, a
    ; Check the 1st operand
    ld a, b
    ; Load the final size now
    ld b, 3
    cp ASSEMBLE_REGISTER_HL
    jp z, _assemble_instr_ld_hl_memnn
    cp ASSEMBLE_REGISTER_SP + 1
    jp c, _assemble_instr_ld_dd_memnn
    cp ASSEMBLE_REGISTER_IY + 1
    jp c, _assemble_instr_ld_ixy_memnn
    jp _assemble_instr_operand_error
_assemble_instr_ld_hl_memnn:
    ld l, 0x2a
    ret
_assemble_instr_ld_dd_memnn:
    inc b
    ld h, 0xed
    and ASSEMBLE_REG16_MASK - 1
    rrca
    rrca
    rrca
    rrca    ; LD dd,(nn) = 01_dd_1011
    or 0x4b
    ld l, a
    ret
_assemble_instr_ld_ixy_memnn:
    inc b
    ld hl, 0xdd2a
    cp ASSEMBLE_REGISTER_IX
    ret z
    ld h, 0xfd
    ret

    ; Parser for:
    ; LD (nn),HL
    ; LD (nn),dd
    ; LD (nn),IX
    ; LD (nn),IY
_assemble_instr_ld_mem_nn:
    ; Let's use the same trick as above, invert
    ; the parameters, parse the instruction
    ; correct the bytes and return
    ld a, b
    ld b, c
    ld c, a
    ; nn has been erased if IX/IY was parsed as
    ; a 2nd operand, retrieve it now
    ld hl, _assemble_instr_ld_backup_constant
    ld e, (hl)
    inc hl
    ld d, (hl)
    call _assemble_instr_ld_rr_memnn
    bit 7, b
    ret nz
    ; Adjust the resulted byte(s)
    ld a, l
    sub 8
    ld l, a
    ret

    ; Parser for LD SP, HL|IX|IY
_assemble_instr_ld_rr_dd:
    ld a, b
    cp ASSEMBLE_REGISTER_SP
    jp nz, _assemble_instr_operand_error

    ; Prepare the return values
    ; Check for HL, IX or IY now
    ld b, 1
    ld de, 0xddf9
    ld a, c
    cp ASSEMBLE_REGISTER_HL
    ret z
    cp ASSEMBLE_REGISTER_IY
    jp z, _assemble_instr_ld_rr_iy
    cp ASSEMBLE_REGISTER_IX
    jp z, _assemble_instr_ld_rr_ix
    jp _assemble_instr_operand_error
_assemble_instr_ld_rr_iy:
    ld d, 0xfd
_assemble_instr_ld_rr_ix:
    inc b
    ret

    ; Routines for parsing PUSH and POP instruction
_assemble_instr_push:
    ld hl, (_assemble_reg1_str)
    call _assemble_operand_hl
    ; Result contained in C, check for the operand
    ; Exclude SP as we cannot push SP
    ld a, c
    cp ASSEMBLE_REGISTER_HL + 1
    jr c, _assemble_instr_push_qq
    ; But we can push AF
    cp ASSEMBLE_REGISTER_AF
    jp z, _assemble_instr_push_qq
    ; Check index registers also
    ld d, 0xdd
    ld e, 0xe5  ; Instructions for PUSH IX
    ld b, 2
    cp ASSEMBLE_REGISTER_IX
    ret z
    cp ASSEMBLE_REGISTER_IY
    jp nz, _assemble_instr_operand_error
    ; PUSH IY
    ld d, 0xfd
    ret
_assemble_instr_push_qq:
    and 3 ; Get the lowest two bits
    rlca
    rlca
    rlca
    rlca ; shift left by 5 <=> rotate right by 4
    or 0xc5 ; PUSH qq = 11_qq_0101
    ld e, a
    ld b, 1
    ret

_assemble_instr_pop:
    call _assemble_instr_push
    bit 7, b
    ret nz
    ld a, e
    ; The difference between PUSH and POP is that
    ; POP's LSB is PUSH's LSB minus 4
    sub 4
    ld e, a
    ret

    ; Parser for:
    ; EX DE,HL
    ; EX AF, AF'
    ; EX (SP), HL|IX|IY
_assemble_instr_ex:
    ; Store the 1st parsed operand in B
    ld hl, (_assemble_reg1_str)
    call _assemble_operand_hl
    ld b, c
    push bc

    ; Store the 2nd parsed operand in C
    ld hl, (_assemble_reg2_str)
    call _assemble_operand_hl
    ld a, c
    pop bc
    ld c, a

    ; Check the 1st operand
    ld a, b
    ld b, 1 ; prepare the final returned value
    cp ASSEMBLE_REGISTER_DE
    jp z, _assemble_instr_ex_de
    cp ASSEMBLE_REGISTER_AF
    jp z, _assemble_instr_ex_af
    cp ASSEMBLE_REGISTER_MEMSP
    jp z, _assemble_instr_ex_memsp
    jr _assemble_instr_ex_invalid_operand

    ; Parser for EX DE, HL
_assemble_instr_ex_de:
    ld a, c
    cp ASSEMBLE_REGISTER_HL
    jr nz, _assemble_instr_ex_invalid_operand
    ld e, 0xeb
    ret

_assemble_instr_ex_af:
    ld a, c
    cp ASSEMBLE_REGISTER_AFp
    jr nz, _assemble_instr_ex_invalid_operand
    ld e, 0x08
    ret

_assemble_instr_ex_memsp:
    ld a, c
    ld e, 0xe3
    cp ASSEMBLE_REGISTER_HL
    ret z
    inc b
    ld d, 0xdd
    cp ASSEMBLE_REGISTER_IX
    ret z
    ld d, 0xfd
    cp ASSEMBLE_REGISTER_IY
    ret z
_assemble_instr_ex_invalid_operand:
    ld b, ASSEMBLE_INVALID_OPERAND
    ret



    ; Parser for 8-bit arithmetic operations where first parameter is optional
    ; Parameters:
    ;   B - Byte to add to the resulted opcode if 8-bit expression
    ;   C - Number of parameters given to the instruction
    ; Returns:
    ;   B - 0: Error
    ;       1-4: Number of bytes of the instruction
    ;       > 4: Code for the first parameter when 16-bit instruction
    ;   C - Code for the second parameter (when B > 4)
    ;   HLDE - Instruction bytes
_assemble_instr_parse_8bit_s_optional_1st:
    ; Check if we only have 2 operands in total (including the instruction)
    ; If this is the case, it is a shortcut for INSTR A,s
    ; It is contained in C
    ld a, c
    cp 2
    ; B already contains the byte to add to the resulted opcode
    jr z, _assemble_instr_parse_8bit_s_group
    ; There must be in total 2 or 3 operands, not less not more
    cp 3
    jr nz, _assemble_instr_syntax_error
    ; Parse the first and second operand
    push bc
    call _assemble_operands
    ld a, b
    cp ASSEMBLE_REGISTER_A
    ; Flag is set already, we can modify A
    ld h, b
    ld l, c
    pop bc
    ld c, l
    jr z, _assemble_instr_parse_8bit_s_group_bypass_parse
    ; Check the operands, both should be 16 bit registers
    bit ASSEMBLE_REG16_BIT, h
    jr z, _assemble_instr_ex_invalid_operand
    bit ASSEMBLE_REG16_BIT, l
    jr z, _assemble_instr_ex_invalid_operand
    ld b, h
    ld c, l
    ret
_assemble_instr_syntax_error:
    ld b, ASSEMBLE_SYNTAX_ERROR
    ret
_assemble_instr_parse_16bit_s_optional:
    ld a, l
    cp ASSEMBLE_REGISTER_SP
    jr nc, _assemble_instr_ex_invalid_operand
    and ASSEMBLE_REG16_MASK - 1
    rrca
    rrca
    rrca
    rrca    ; ADD HL,ss = 00_ss_1001
    or 9
    xor b
    ld e, a
    ld b, 1
    ret


    ; Parse the 8-bit s operand that is used in all 8-bit arithmetic instructions
    ; Parameters:
    ;   B - Constant we have to add to the ADD's opcodes
    ;       For example, ADD A,(HL) opcode is 0x86 but
    ;       ADC A,(HL)'s is 0x8E, the difference is 8, so,
    ;       8 should be passed.
_assemble_instr_parse_8bit_s_group:
    ld hl, (_assemble_reg1_str)
    push bc ; Keep B
    call _assemble_operand_hl
    ld a, c
    pop bc
    ld c, a
    ; Result is in C
    ;   C - First parameter code (if applicable)
    ;   DE - Constant (if applicable)
_assemble_instr_parse_8bit_s_group_bypass_parse:
    ld a, c
    cp ASSEMBLE_REGISTER_SPECIAL
    jp c, _assemble_instr_parse_8bit_s_group_reg
    cp ASSEMBLE_CONSTANT_N
    jp z, _assemble_instr_parse_8bit_s_group_const
    cp ASSEMBLE_REGISTER_MEMHL
    jp z, _assemble_instr_parse_8bit_s_group_memhl
    cp ASSEMBLE_REGISTER_MEMIX
    ; Optimize a bit for IX and IY: pre-load the register code
    ld l, 0xdd
    jp z, _assemble_instr_parse_8bit_s_group_memix
    cp ASSEMBLE_REGISTER_MEMIY
    jp z, _assemble_instr_parse_8bit_s_group_memiy
    jp _assemble_instr_operand_error

    ; Register in S group
_assemble_instr_parse_8bit_s_group_reg:
    or 0x80
    add b   ; add the given constant
    ; Prepare return values
    ld e, a
    ld b, 1
    ret
_assemble_instr_parse_8bit_s_group_const:
    ; Constant value is already in E
    ld a, 0xC6
    add b
    ld d, a
    ld b, 2 ; 2 bytes for this instruction
    ret
_assemble_instr_parse_8bit_s_group_memhl:
    ld a, 0x86
    add b
    ld e, a
    ld b, 1
    ret
_assemble_instr_parse_8bit_s_group_memiy:
    ld l, 0xfd
_assemble_instr_parse_8bit_s_group_memix:
    ld a, 0x86
    add b
    ld d, a
    ; E already has the constant value
    ld b, 3
    ret

    ; Parser for ADD _,s
_assemble_instr_add:
    ; Copy the number of operands parsed into C, as required by _assemble_instr_parse_8bit_s_optional_1st
    ld c, b
    ld b, 0 ; No byte to add to the resulted opcode as they are based on ADD already
    call _assemble_instr_parse_8bit_s_optional_1st
    ; Check if the returned value is between 0 and 4 (error)
    ld a, b
    cp 5
    ret c
    ; Check for HL as 1st operand
    cp ASSEMBLE_REGISTER_HL
    jp z, _assemble_instr_add_hl_ss
    ld d, 0xdd
    cp ASSEMBLE_REGISTER_IX
    jp z, _assemble_instr_add_ix_ss
    cp ASSEMBLE_REGISTER_IY
    jp z, _assemble_instr_add_iy_ss
    jp _assemble_instr_operand_error
_assemble_instr_add_hl_ss:
    ; Generate code for ADD HL,ss
    ; Make sure the second parameter is a regular 16-bit register
    ld a, c
    cp ASSEMBLE_REGISTER_SP + 1
    jp nc, _assemble_instr_operand_error
    and ASSEMBLE_REG16_MASK - 1
    rrca
    rrca
    rrca
    rrca
    or 9
    ld e, a
    ld b, 1
    ret
_assemble_instr_add_iy_ss:
    ld d, 0xfd
_assemble_instr_add_ix_ss:
    ; Load second operand
    ld a, c
    ; The second operand CANNOT be HL
    ; But it can be IX|IY
    cp ASSEMBLE_REGISTER_HL
    jp z, _assemble_instr_operand_error
    cp b    ; Check if both parameters are the same (IX,IX or IY,IY)
    jp nz, _assemble_instr_add_ix_ss_not_fix
    ld a, 2 ; IX and IY is that case replace HL
_assemble_instr_add_ix_ss_not_fix:
    rrca
    rrca
    rrca
    rrca    ; ADD IX|Y,rr = 00_rr_1001
    or 9
    ld e, a
    ld b, 2
    ret

_assemble_instr_adc:
    ld c, b
    ld b, 8 ; Constant to add to the resulted opcode is 0
    call _assemble_instr_parse_8bit_s_optional_1st
    ; Check if the returned value is between 0 and 4 (error)
    ld a, b
    cp 5
    ret c
    ; Check for HL
    cp ASSEMBLE_REGISTER_HL
    jp nz, _assemble_instr_operand_error
    ; Generate code for ADC HL,ss
    ld a, c
    ; Make sure the second parameter is a standard 16-bit reg
    cp ASSEMBLE_REGISTER_SP + 1
    jp nc, _assemble_instr_operand_error
    and ASSEMBLE_REG16_MASK - 1
    rrca
    rrca
    rrca
    rrca
    or 0x4a
    ld d, 0xed
    ld e, a
    ld b, 2
    ret

_assemble_instr_sbc:
    ld c, b
    ld b, 0x18
    call _assemble_instr_parse_8bit_s_optional_1st
    ; Check if the returned value is between 0 and 4 (error)
    ld a, b
    cp 5
    ret c
    ; Check for HL
    cp ASSEMBLE_REGISTER_HL
    jp nz, _assemble_instr_operand_error
    ; Generate code for ADC HL,ss
    ld a, c
    cp ASSEMBLE_REGISTER_SP + 1
    jp nc, _assemble_instr_operand_error
    and ASSEMBLE_REG16_MASK - 1
    rrca
    rrca
    rrca
    rrca
    or 0x42
    ld d, 0xed
    ld e, a
    ld b, 2
    ret

_assemble_instr_sub:
    ld b, 0x10
    jp _assemble_instr_parse_8bit_s_group

_assemble_instr_and:
    ld b, 0x20
    jp _assemble_instr_parse_8bit_s_group

_assemble_instr_xor:
    ld b, 0x28
    jp _assemble_instr_parse_8bit_s_group

_assemble_instr_or:
    ld b, 0x30
    jp _assemble_instr_parse_8bit_s_group

_assemble_instr_cp:
    ld b, 0x38
    jp _assemble_instr_parse_8bit_s_group


    ; Parse the operands for INC/DEC, it accepts r, (HL), (IX+d), (IY+d), rr
    ; Parameters:
    ;   B - Constant to be add to the INC 8-bit opcodes
    ;       Constant that will be x8 to INC 16-bit opcodes
_assemble_instr_inc_dec_parameters:
    ld hl, (_assemble_reg1_str)
    push bc ; Keep B
    call _assemble_operand_hl
    ld a, c
    pop bc

    cp ASSEMBLE_REGISTER_SPECIAL
    jp c, _assemble_instr_inc_dec_r
    bit ASSEMBLE_REG16_BIT, a
    jp nz, _assemble_instr_inc_dec_rr
    cp ASSEMBLE_REGISTER_MEMHL
    jp z, _assemble_instr_inc_dec_memhl

    ; Optimize a bit for IX and IY: pre-load the register code
    ld l, 0xdd
    ; E already contains the constant for IX
    cp ASSEMBLE_REGISTER_MEMIX
    jp z, _assemble_instr_inc_dec_group_memix
    cp ASSEMBLE_REGISTER_MEMIY
    jp z, _assemble_instr_inc_dec_group_memiy
    jp _assemble_instr_operand_error
_assemble_instr_inc_dec_r:
    rlca
    rlca
    rlca
    or 4
    add b
    ld e, a
    ld b, 1
    ret
_assemble_instr_inc_dec_rr:
    cp ASSEMBLE_REGISTER_IY + 1
    jp nc, _assemble_instr_operand_error
    cp ASSEMBLE_REGISTER_SP + 1
    jp c, _assemble_instr_inc_dec_16reg
    cp ASSEMBLE_REGISTER_IX
    ld d, 0xdd
    jp z, _assemble_instr_inc_dec_ix
    ; Register is IY
    ld d, 0xfd
_assemble_instr_inc_dec_ix:
    ld a, b
    rlca
    rlca
    rlca
    add 0x23
    ld e, a
    ld b, 2
    ret
_assemble_instr_inc_dec_16reg:
    and ASSEMBLE_REG16_MASK - 1
    rlca
    rlca
    rlca
    rlca
    or 3
    ; B has to be multiplied by 8!
    sla b
    sla b
    sla b
    add b
    ld e, a
    ld b, 1
    ret
_assemble_instr_inc_dec_memhl:
    ld a, 0x34
    add b
    ld e, a
    ld b, 1
    ret
_assemble_instr_inc_dec_group_memiy:
    ld l, 0xfd
_assemble_instr_inc_dec_group_memix:
    ld a, 0x34
    add b
    ld d, a
    ld b, 3
    ret

_assemble_instr_inc:
    ld b, 0
    jr _assemble_instr_inc_dec_parameters

_assemble_instr_dec:
    ld b, 1
    jr _assemble_instr_inc_dec_parameters

_assemble_instr_im:
    ld hl, (_assemble_reg1_str)
    call _assemble_operand_hl
    ld a, c
    ; IM is only valid if the parameter is a constant with N < 3
    cp ASSEMBLE_CONSTANT_N
    jp nz, _assemble_instr_operand_error
    ld a, e
    ld d, 0xed
    ld e, 0x46
    ld b, 2
    ; IM 0
    or a
    ret z
    ; IM 1
    ld e, 0x56
    dec a
    ret z
    ; IM 2
    ld e, 0x5e
    dec a
    ret z
    jp _assemble_instr_operand_error


    ; Parse the operands for rotate and shift, it accepts r, (HL), (IX+d), (IY+d)
    ; Parameters:
    ;   B - Constant to be add to the INC 8-bit opcodes
    ;       Constant that will be x8 to INC 16-bit opcodes
_assemble_instr_rot_parameters:
    ld hl, (_assemble_reg1_str)
    push bc ; Keep B
    call _assemble_operand_hl
    ld a, c
    pop bc

    ; Prefix for all the instructions
    ld d, 0xcb
    cp ASSEMBLE_REGISTER_SPECIAL
    jp c, _assemble_instr_rot_r
    cp ASSEMBLE_REGISTER_MEMHL
    jp z, _assemble_instr_rot_memhl

    ; Optimize a bit for IX and IY: pre-load the register code
    ld h, 0xdd
    ld l, d
    ld d, e
    ld e, 0x6
    ; E already contains the constant for IX
    cp ASSEMBLE_REGISTER_MEMIX
    jp z, _assemble_instr_rot_memix
    cp ASSEMBLE_REGISTER_MEMIY
    jp z, _assemble_instr_rot_memiy
    jp _assemble_instr_operand_error
_assemble_instr_rot_r:
    add b
    ld e, a
    ld b, 2
    ret
_assemble_instr_rot_memhl:
    ld a, 0x6
    add b
    ld e, a
    ld b, 2
    ret
_assemble_instr_rot_memiy:
    ld h, 0xfd
_assemble_instr_rot_memix:
    ld a, e
    add b
    ld e, a
    ld b, 4
    ret

_assemble_instr_rlc:
    ld b, 0
    jp _assemble_instr_rot_parameters

_assemble_instr_rrc:
    ld b, 0x8
    jp _assemble_instr_rot_parameters

_assemble_instr_rl:
    ld b, 0x10
    jp _assemble_instr_rot_parameters

_assemble_instr_rr:
    ld b, 0x18
    jp _assemble_instr_rot_parameters

_assemble_instr_sla:
    ld b, 0x20
    jp _assemble_instr_rot_parameters

_assemble_instr_sra:
    ld b, 0x28
    jp _assemble_instr_rot_parameters

_assemble_instr_srl:
    ld b, 0x38
    jp _assemble_instr_rot_parameters



_assemble_instr_bit_op:
    ; Parse the first and second operand
    push bc
    call _assemble_operands
    ld a, b
    ; Flag is set already, we can modify A
    cp ASSEMBLE_CONSTANT_N
    ld l, c
    ld a, (_assemble_instr_ld_backup_constant)
    ld d, a
    pop bc
    ; B contains the constant to add to the opcode
    ; L contains the 2nd parameter code
    ; D contains the bit constant
    ; E contains IX|Y's index constant
    ; If the first parameter is not a constant, return an error
    jp nz, _assemble_instr_operand_error
    ; The constant shall be between 0 and 7. A already contains the
    ; constant.
    cp 8
    jp nc, _assemble_instr_operand_error
    ; Test the 2nd parameter
    ld a, l
    cp ASSEMBLE_REGISTER_SPECIAL
    jr c, _assemble_instr_bit_reg
    cp ASSEMBLE_REGISTER_MEMHL
    jr z, _assemble_instr_bit_memhl
    cp ASSEMBLE_REGISTER_MEMIX
    jr z, _assemble_instr_bit_memix
    cp ASSEMBLE_REGISTER_MEMIY
    jr z, _assemble_instr_bit_memiy
    jp _assemble_instr_operand_error
_assemble_instr_bit_reg:
    ; Load the bit number
    ld a, d
    rlca
    rlca
    rlca    ; BIT b, r = 01_bbb_rrr
    or 0x40
    or l    ; concatenate the register number
    add b   ; Add the constant as a parameter
    ld d, 0xcb
    ld e, a
    ld b, 2
    ret
_assemble_instr_bit_memhl:
    ld a, d
    rlca
    rlca
    rlca    ; BIT b, (HL) = 01_bbb_110
    or 0x46
    add b   ; Add the constant as a parameter
    ld d, 0xcb
    ld e, a
    ld b, 2
    ret
_assemble_instr_bit_memix:
    ld hl, 0xddcb
    ld a, d
    rlca
    rlca
    rlca    ; BIT b, (IY+d) = 01_bbb_110
    or 0x46
    add b   ; Add the constant as a parameter
    ld d, e
    ld e, a
    ld b, 4
    ret
_assemble_instr_bit_memiy:
    call _assemble_instr_bit_memix
    ld h, 0xfd
    ret

    ; Parser for BIT b,n
_assemble_instr_bit:
    ld b, 0
    jp _assemble_instr_bit_op

    ; Parser for SET b,n
_assemble_instr_set:
    ld b, 0x80
    jp _assemble_instr_bit_op

    ; Parser for RES b,n
_assemble_instr_res:
    ld b, 0x40
    jp _assemble_instr_bit_op


    ; This routine will convert a C register into a C flag.
    ; This is due to the lexer/parser that cannot differentiate
    ; between C the register and C the flag.
    ; Parameters:
    ;   B - Code of the parsed parameter
    ; Returns:
    ;   B - Code of C flag if it was C register at first
    ; Alters:
    ;   A
_assemble_fix_c_flag:
    ld a, b
    cp ASSEMBLE_REGISTER_C
    ret nz
    ld b, ASSEMBLE_FLAG_C
    ret

    ; Parser for JP [cc,] nn
    ; Parameters:
    ;   B - Number of parameters given to the instruction
_assemble_instr_jp:
    ld a, b
    cp 2
    jr z, _assemble_instr_jp_1param
    cp 3
    jr z, _assemble_instr_jp_2params
    jp _assemble_instr_operand_error
_assemble_instr_jp_1param:
    ld hl, (_assemble_reg1_str)
    call _assemble_operand_hl
    ld a, c ; Check the type of the parameter
    cp ASSEMBLE_CONSTANT_N
    jr z, _assemble_instr_jp_nn
    cp ASSEMBLE_LABEL_NAME
    jr z, _assemble_instr_jp_nn
    ; Prepare for (HL), (IX) or (IY)
    ld b, 1
    ld de, 0xdde9
    cp ASSEMBLE_REGISTER_MEMHL
    ret z
    ; check for (IX) or (IY)
    ld b, 2
    cp ASSEMBLE_REGISTER_MEMIX
    ret z
    ; Check for (IY)
    ld d, 0xfd
    cp ASSEMBLE_REGISTER_MEMIY
    ret z
    jp _assemble_instr_operand_error
_assemble_instr_jp_nn:
    ld b, 3
    ld l, 0xc3
    ; DE is already set by the lexer/parser, but in big endian, invert it
    ld a, d
    ld d, e
    ld e, a
    ret
_assemble_instr_jp_2params:
    ; The first parameter must be a flag, the second must be a constant
    call _assemble_operands
    call _assemble_fix_c_flag
    ld a, c
    CHECK_LABEL_OR_CONSTANT()
    ld a, b
    bit ASSEMBLE_FLAG_BIT, a
    jp z, _assemble_instr_operand_error
    and ASSEMBLE_FLAG_MASK - 1
    ; B is indeed a flag, DE already contains the address, but in big-endian
    ; JP ccn,nn = 11_ccc_010
    rlca
    rlca
    rlca
    or 0xc2
    ld l, a
    ld a, d
    ld d, e
    ld e, a
    ld b, 3
    ret

    ; Parser for JR [cc,] nn
    ; Parameters:
    ;   B - Number of parameters given to the instruction
_assemble_instr_jr:
    ld a, b
    cp 2
    jr z, _assemble_instr_jr_1param
    cp 3
    jr z, _assemble_instr_jr_2params
    jp _assemble_instr_operand_error
_assemble_instr_jr_1param:
    ld hl, (_assemble_reg1_str)
    call _assemble_operand_hl
    ld a, c
    ; Check the type of the parameter, JR e only accepts constants
    CHECK_LABEL_OR_CONSTANT()
    ld a, d
    or a
    ; The parameter must be a byte
    jp nz, _assemble_instr_operand_error
    ld a, e
    sub 2
    ld e, a
    ld d, 0x18
    ld b, 2
    ret
_assemble_instr_jr_2params:
    ; The first parameter must be a flag, the second must be a constant
    call _assemble_operands
    call _assemble_fix_c_flag
    ld a, c
    CHECK_LABEL_OR_CONSTANT()
    ld a, d
    or a
    ; The constant must be a byte
    jp nz, _assemble_instr_operand_error
    ; Check for the flags
    ld a, b
    bit ASSEMBLE_FLAG_BIT, a
    jp z, _assemble_instr_operand_error
    ; Only NZ, Z, C and NC flags are accepted
    cp ASSEMBLE_FLAG_C + 1
    jp nc, _assemble_instr_operand_error
    ; Get rid of the flag bit
    and ASSEMBLE_FLAG_MASK - 1
    ; B is indeed a flag, DE already contains the address, but in big-endian
    ; JP ccn,nn = 11_ccc_010
    rlca
    rlca
    rlca
    or 0x20
    ld d, a
    ld a, e
    sub 2
    ld e, a
    ld b, 2
    ret

    ; Parser for DJNZ e
    ; Parameters:
    ;   None
_assemble_instr_djnz:
    ld hl, (_assemble_reg1_str)
    call _assemble_operand_hl
    ld a, c
    ; Check the type of the parameter, JR e only accepts constants
    CHECK_LABEL_OR_CONSTANT()
    ld a, d
    or a
    ; The parameter must be a byte
    jp nz, _assemble_instr_operand_error
    ld a, e
    sub 2
    ld e, a
    ld d, 0x10
    ld b, 2
    ret


    ; Parser for CALL [cc,] nn
    ; Parameters:
    ;   B - Number of parameters given to the instruction
_assemble_instr_call:
    ld a, b
    cp 2
    jr z, _assemble_instr_call_1param
    cp 3
    jr z, _assemble_instr_call_2params
    jp _assemble_instr_operand_error
_assemble_instr_call_1param:
    ld hl, (_assemble_reg1_str)
    call _assemble_operand_hl
    ld a, c ; Check the type of the parameter
    CHECK_LABEL_OR_CONSTANT()
    ld b, 3
    ld l, 0xcd
    ; DE is already set by the lexer/parser, but in big endian, invert it
    ld a, d
    ld d, e
    ld e, a
    ret
_assemble_instr_call_2params:
    ; The first parameter must be a flag, the second must be a constant
    call _assemble_operands
    call _assemble_fix_c_flag
    ld a, c
    CHECK_LABEL_OR_CONSTANT()
    ld a, b
    bit ASSEMBLE_FLAG_BIT, a
    jp z, _assemble_instr_operand_error
    and ASSEMBLE_FLAG_MASK - 1
    ; B is indeed a flag, DE already contains the address, but in big-endian
    ; CALL cc,nn = 11_ccc_100
    rlca
    rlca
    rlca
    or 0xc4
    ld l, a
    ld a, d
    ld d, e
    ld e, a
    ld b, 3
    ret

    ; Parser for RET [cc]
    ; Parameters:
    ;   B - Number of parameters given to the instruction
_assemble_instr_ret:
    ld a, b
    dec a
    jr z, _assemble_instr_ret_no_param
    dec a
    jr z, _assemble_instr_ret_1param
    jp _assemble_instr_operand_error
_assemble_instr_ret_no_param:
    ; E has already been set by the caller
    ld b, 1
    ret
_assemble_instr_ret_1param:
    ld hl, (_assemble_reg1_str)
    call _assemble_operand_hl
    ld b, c
    call _assemble_fix_c_flag
    ld a, b ; Check the type of the parameter
    bit ASSEMBLE_FLAG_BIT, a
    jp z, _assemble_instr_operand_error
    and ASSEMBLE_FLAG_MASK - 1
    ; RET cc = 11_ccc_000
    rlca
    rlca
    rlca
    or 0xc0
    ld e, a
    ld b, 1
    ret

    ; Parser for RET [cc]
_assemble_instr_rst:
    ld hl, (_assemble_reg1_str)
    call _assemble_operand_hl
    ld a, c
    sub ASSEMBLE_CONSTANT_N
    jp nz, _assemble_instr_operand_error
    ; The constant must be a byte, so D must be 0
    or d
    jp nz, _assemble_instr_operand_error
    ld a, e
    ; RST only accepts $00, $08, $10, $18, $20, $28, $30, $38
    ; Check the lowest bits
    and 7
    jp nz, _assemble_instr_operand_error
    ld a, e
    ; In theory, we need to divide A by 8, but at the end, we would still
    ; need to shift the result to the left 3 times, so we only need set the
    ; lowest 3 bits as the previous check already guarantees us the lowest
    ; 3 bits are 0.
    or 7
    ; A must however have the two highest bits set to 0
    cp 0x40
    jp nc, _assemble_instr_operand_error
    or 0xc0
    ld e, a
    ld b, 1
    ret

    ; Generate opcodes for any instruction which parameters
    ; must be A,(n) or r,(c)
    ; Parameters:
    ;   H - Constant to subtract to the generated opcode for A,(n)
    ;   L - 1 is parameter must be inverted [(n), A], 0 else
    ; Returns:
    ;   B - Number of generated instructions (bytes)
    ;   DE - Code of the instructions
_assemble_instr_inout:
    push hl
    call _assemble_operands
    pop hl
    ld a, l
    or a
    jr z, _assemble_instr_inout_no_invert
    ld a, b
    ld b, c
    ld c, a
_assemble_instr_inout_no_invert:
    ; Check the second parameter
    ld a, c
    cp ASSEMBLE_REGISTER_MEMNN
    jp z, _assemble_instr_inout_memnn
    cp ASSEMBLE_REGISTER_MEMC
    jp z, _assemble_instr_inout_memc
    jp _assemble_instr_operand_error
_assemble_instr_inout_memnn:
    ; Parsed constant must be a byte
    ld a, d
    or d
    jp nz, _assemble_instr_operand_error
    ; 1st parameter must be A
    ld a, b
    cp ASSEMBLE_REGISTER_A
    jp nz, _assemble_instr_operand_error
    ; We can finally proceed to the opcode generation
    ld a, 0xdb
    sub h
    ld d, a
    ; E has already been set by the lexer/parser
    ld b, 2
    ret
_assemble_instr_inout_memc:
    ; 1st parameter msut be a regular register
    ld a, b
    cp ASSEMBLE_REGISTER_SPECIAL
    jp nc, _assemble_instr_operand_error
    ; Rotate/Shift A left three times
    rlca
    rlca
    rlca
    or 0x40
    ; If the parameters need to be inverted, the opcode to generate
    ; is OUT. OUT needs incrementing the resulted opcode for (C),r
    add l
    ld d, 0xed
    ld e, a
    ld b, 2
    ret

_assemble_instr_in:
    ld hl, 0
    jp _assemble_instr_inout

_assemble_instr_out:
    ld hl, 0x0801
    jp _assemble_instr_inout


    ; Following instructions don't take any operand
    ; We only need to increment B if they are made of 2 bytes (CPI for example)
    ; Else, they don't need further modification.
    ; Parameters:
    ;   B - Size of the instruction in byte (1)
    ; Returns:
    ;   B - Size of the instruction in byte (1 or 2)
_assemble_instr_cpd:
_assemble_instr_cpdr:
_assemble_instr_cpi:
_assemble_instr_cpir:
_assemble_instr_ind:
_assemble_instr_indr:
_assemble_instr_ini:
_assemble_instr_inir:
_assemble_instr_ldd:
_assemble_instr_lddr:
_assemble_instr_ldi:
_assemble_instr_ldir:
_assemble_instr_neg:
_assemble_instr_otdr:
_assemble_instr_otir:
_assemble_instr_outd:
_assemble_instr_outi:
_assemble_instr_reti:
_assemble_instr_retn:
_assemble_instr_rld:
_assemble_instr_rrd:
    inc b   ; two bytes for these instructions
    ; Fall-through
_assemble_instr_ccf:
_assemble_instr_cpl:
_assemble_instr_daa:
_assemble_instr_di:
_assemble_instr_ei:
_assemble_instr_exx:
_assemble_instr_halt:
_assemble_instr_nop:
_assemble_instr_rla:
_assemble_instr_rlca:
_assemble_instr_rra:
_assemble_instr_rrca:
_assemble_instr_scf:
    ret


    ; The following routines will call _assemble_operand_hl
    ; for the two operands stored in _assemble_reg1_str and _assemble_reg2_str
    ;
    ; Parameters:
    ;   None
    ; Returns:
    ;   B - Code of the first operand
    ;   C - Code of the second operand
    ;   _assemble_instr_ld_backup_constant: backup of the first operand constant (if applicable)
    ; Alters:
    ;   HL, DE, BC
_assemble_operands:
    ld hl, (_assemble_reg1_str)
    call _assemble_operand_hl
    ; Backup the constant if necessary
    ld hl, _assemble_instr_ld_backup_constant
    ld (hl), e
    inc hl
    ld (hl), d

    ; C contains the code of the operand
    push bc
    ld hl, (_assemble_reg2_str)
    call _assemble_operand_hl
    ld h, c
    pop bc
    ld b, c
    ld c, h
    ret

    ; The following routines are helpers for parsing the parameters
    ; Parse the operand which is pointed by HL
    ; Parameters:
    ;   HL - Address of the string (HL[0] is the first character)
    ; Returns:
    ;   C - Code of the operand (check the macros)
    ;   DE - Value of the operand (if applicable)
    ; Alters:
    ;   HL, A, BC, DE
_assemble_operand_hl:
    ; Check if the operand starts with (
    ld a, (hl)
    cp '('
    jp z, _assemble_operand_mem_access
    ; Check if the operand is one character long
    ld c, a
    inc hl
    ld a, (hl)
    or a
    jp z, _assemble_operand_one_char
    ; The operand length is > 1
    ld b, c
    ld c, a ; BC = (operand[0] << 8) | operand [1]
    inc hl
    ld a, (hl)
    or a
    jp z, _assemble_operand_two_chars
    ; The operand length is > 2, it can be a label, or a constant
    ; Restore HL before jumping
    dec hl
    dec hl
    jp _assemble_operand_n_chars

_assemble_operand_hl_error_ixy_index:
_assemble_operand_hl_error:
    ld c, ASSEMBLE_UNKNOWN_OPERAND
    ret

_assemble_operand_mem_access:
    inc hl ; Go after the (
    ; Look for the closing parenthesis
    call strlen
    ; Trim leading spaces
    ld a, ' '
    call strnltrim
    ld a, ')'
    call memsep
    ; It must be present, so A must be 0
    or a
    jp nz, _assemble_operand_hl_error
    ; DE contains the address of the next characters
    ; it size, after trimming must be 0
    ex de, hl
    call strnltrim
    ex de, hl
    ; BC must be 0!
    ld a, b
    or c
    jp nz, _assemble_operand_hl_error
    ; HL is now pointing to the content within the parenthesis
    call strlen
    call strnrtrim
    ; If the length is 0, error
    ld a, b
    or a
    jp nz, _assemble_operand_mem_access_n_chars
    or c
    jp z, _assemble_operand_hl_error
    dec a   ; test if the size is 1
    jp z, _assemble_operand_mem_access_one_char
    dec a   ; test if the size is 2
    jp z, _assemble_operand_mem_access_two_chars

_assemble_operand_mem_access_n_chars:
    ; N chars!
    call _assemble_operand_n_chars
    ; Here is what this function returns
    ;   - An integer value
    ;   - AF' register
    ;   - IX+c register
    ;   - IY+c register
    ;   - A label
    ld a, c
    cp ASSEMBLE_UNKNOWN_OPERAND
    jp z, _assemble_operand_hl_error
    cp ASSEMBLE_REGISTER_AFp
    jp z, _assemble_operand_hl_error
    cp ASSEMBLE_LABEL_NAME
    jr z, _assemble_operand_mem_access_one_char_constant
    cp ASSEMBLE_CONSTANT_N
    jr z, _assemble_operand_mem_access_one_char_constant
    ; It can only be IX or IY at that point, convert it directly
    add ASSEMBLE_MEMACC_MASK - ASSEMBLE_REG16_MASK
    ld c, a
    ret

_assemble_operand_mem_access_one_char:
    ld c, (hl)
    call _assemble_operand_one_char
    ; The only possible operations with 1 char in memory access are
    ; (C) and (N)
    ld a, c
    cp ASSEMBLE_REGISTER_C
    jp z, _assemble_operand_mem_access_one_char_c_reg
    cp ASSEMBLE_CONSTANT_N
    jp nz, _assemble_operand_hl_error
_assemble_operand_mem_access_one_char_constant:
    ld c, ASSEMBLE_REGISTER_MEMNN
    ret
_assemble_operand_mem_access_one_char_c_reg:
    ld c, ASSEMBLE_REGISTER_MEMC
    ret

_assemble_operand_mem_access_two_chars:
    ld b, (hl)
    inc hl
    ld c, (hl)
    inc hl
    call _assemble_operand_two_chars
    ; Check if the return value is known, it should NOT be a flag neither
    ld a, c
    cp ASSEMBLE_FLAG_MASK
    jp nc, _assemble_operand_hl_error
    cp ASSEMBLE_CONSTANT_N
    jp z, _assemble_operand_mem_access_one_char_constant
    ; (AF) doesn't exist
    cp ASSEMBLE_REGISTER_AF
    jp z, _assemble_operand_hl_error
    ; Convert 16-bit register to 16-bit memory access
    add ASSEMBLE_MEMACC_MASK - ASSEMBLE_REG16_MASK
    ld c, a
    ret

_assemble_operand_one_char:
    ; The character is in C
    ; Check for constants first
    ld a, c
    call is_digit
    ; If NC, it means that C is a digit
    jp nc, _assemble_operand_one_char_digit
    ; The character is not a digit, check if it is in the token list
    ld a, c
    ld hl, _assemble_operand_size1_str
    ld bc, _assemble_operand_size1_val - _assemble_operand_size1_str
    cpir    ; look for the character in the list
    ; Check if Bc is 0, if that's the case, the character is not valid
    ld a, b
    or c
    jp z, _assemble_operand_hl_error
    ; Else, BC contains the length - index of the element
    ld hl, _assemble_operand_size1_val - _assemble_operand_size1_str - 1
    sbc hl, bc
    ; HL contains the index of the element
    ld bc, _assemble_operand_size1_val
    add hl, bc
    ld c, (hl)
    ret
_assemble_operand_one_char_digit:
    ld a, c
    sub '0'
    ld d, 0
    ld e, a
    ld c, ASSEMBLE_CONSTANT_N
    ret

_assemble_operand_two_chars:
    ; The two characters are in BC
    ; we can also restore HL to have the characters in memory
    dec hl
    dec hl
    call parse_int
    or a
    jp z, _assemble_operand_two_chars_number
    ; The parameter is not a number, check if it's a register
    ; or a flag. We'll forbid 2-char long labels.
    ; BC contains the two characters of the given instruction.
    ; Copy it to DE and use C as a counter
    ld de, bc
    ld c, 0xff  ; C will be incremented right after, thus it'll be 0
    ld hl, _assemble_operand_size2_str - 2 ; HL is going to be incremented
_assemble_operand_two_chars_loop_2inc:
    inc hl
_assemble_operand_two_chars_loop_1inc:
    inc c
    inc hl
    ld a, (hl)
    or a
    ; If A is 0, we've reached the end of the array, the operand is unknown
    jp z, _assemble_operand_hl_error
    sub d
    jp nz, _assemble_operand_two_chars_loop_2inc
    ; First character is the same, check the following one
    inc hl
    ld a, (hl)
    sub e
    jp nz, _assemble_operand_two_chars_loop_1inc
    ; We've just found our token! C is the index
    ; Set B to 0, to simplify the 16-bit addition
    ld b, a
    ; Set DE to 0, when the register is IX/IY, the index is 0 by default
    ld d, a
    ld e, a
    ld hl, _assemble_operand_size2_val
    add hl, bc
    ld c, (hl)
    ret
_assemble_operand_two_chars_number:
    ld de, hl
    ld c, ASSEMBLE_CONSTANT_N
    ret

_assemble_operand_n_chars:
    ; HL contains the string representing the operand to parse,
    ; It can be:
    ;   - An integer value
    ;   - AF' register
    ;   - IX+c register
    ;   - IY+c register
    ;   - A label
    ; First, try parsing it into an integer
    ld d, h
    ld e, l
    call parse_int
    or a
    ; Parsing was successful, it is then a constant
    jp z, _assemble_operand_n_chars_constant
    ; HL was not a constant
    ; Check if it's AF' register
    ld hl, _assemble_operand_afp_str
    call strcmp
    or a
    jp z, _assemble_operand_n_chars_afp
    ; Put the operand back in HL
    ld h, d
    ld l, e
    ; Check if it's IX or IY + c value
    ld a, (hl)
    inc hl
    cp 'i'
    jp nz, _assemble_operand_n_chars_label
    ld a, (hl)
    inc hl
    cp 'x'
    jp z, _assemble_operand_n_chars_ixy
    cp 'y'
    jp z, _assemble_operand_n_chars_ixy
    jp _assemble_operand_n_chars_label
_assemble_operand_n_chars_ixy:
    call strlen
    call strnltrim
    ld a, (hl)
    cp '+'
    jr nz, _assemble_operand_n_chars_label
    inc hl
    dec bc
    call strnltrim
    call strnrtrim
    call parse_int
    ; A shall be 0 and the constant must be a byte (H == 0), so A | H == 0
    or h
    jp nz, _assemble_operand_hl_error_ixy_index
    ld c, ASSEMBLE_REGISTER_IX
    ; Check if we need to store ASSEMBLE_REGISTER_IY instead of ASSEMBLE_REGISTER_IX
    ; check the original operand's second byte
    inc de
    ld a, (de)
    ; Store the final result, before the test!
    ld d, 0
    ld e, l
    ; Test the register name
    cp 'y'
    ret nz
    ld c, ASSEMBLE_REGISTER_IY
    ret
_assemble_operand_n_chars_constant:
    ld c, ASSEMBLE_CONSTANT_N
    ; Constant value should be in DE, not HL, switch them
    ex de, hl
    ret
_assemble_operand_n_chars_afp:
    ld c, ASSEMBLE_REGISTER_AFp
    ret
_assemble_operand_n_chars_label:
    ; Retrieve the original label back
    ; Store 0 in DE so that it is used as a placeholder in the binary.
    ; Do not use a 16-bit value because jr/djnz require an 8-bit value
    ld hl, 0
    ex de, hl
    ld (_assemble_ref_label), hl
    ; Check that it only contains [0-9a-zA-Z_]
    call parser_label_is_valid
    ld a, b
    or a
    jr nz, _assemble_operand_syntax_error
    ; Check if the label is not too long
    call strlen
    ld a, c
    cp ASSEMBLE_MAX_LABEL_LENGTH + 1
    jp nc, _assemble_operand_label_too_long
    ld c, ASSEMBLE_LABEL_NAME
    ret
_assemble_operand_label_too_long:
    ld c, ASSEMBLE_LABEL_TOO_LONG
    ret
_assemble_operand_syntax_error:
    ld c, ASSEMBLE_SYNTAX_ERROR
    ret


    ; Space used to store the address of a label defined on the current line
    PUBLIC _assemble_def_label
_assemble_def_label: DEFS 2

    ; Space used to store the address of a label referenced by the current instruction
    PUBLIC _assemble_ref_label
_assemble_ref_label: DEFS 2

_assemble_instr_str: DEFS 2
_assemble_reg1_str:  DEFS 2
_assemble_reg2_str:  DEFS 2
    ; Backup the operand value if constant (16-bit)
_assemble_instr_ld_backup_constant: DEFS 2

_assemble_operand_afp_str: DEFM "af'", 0
_assemble_operand_size1_str:
    DEFM "a", "b", "c", "d", "e", "h", "l", "i", "r" ; Registers
    DEFM "z", "m", "p", "c"                          ; Flags
_assemble_operand_size1_val:
    DEFM ASSEMBLE_REGISTER_A, ASSEMBLE_REGISTER_B, ASSEMBLE_REGISTER_C
    DEFM ASSEMBLE_REGISTER_D, ASSEMBLE_REGISTER_E, ASSEMBLE_REGISTER_H
    DEFM ASSEMBLE_REGISTER_L, ASSEMBLE_REGISTER_I, ASSEMBLE_REGISTER_R
    DEFM ASSEMBLE_FLAG_Z, ASSEMBLE_FLAG_M, ASSEMBLE_FLAG_P, ASSEMBLE_FLAG_C

_assemble_operand_size2_str:
    DEFM "bc", "de", "hl", "sp", "af", "ix", "iy"  ; Registers
    DEFM "nz", "nc", "po", "pe", 0                 ; Flags
_assemble_operand_size2_val:
    DEFM ASSEMBLE_REGISTER_BC, ASSEMBLE_REGISTER_DE, ASSEMBLE_REGISTER_HL
    DEFM ASSEMBLE_REGISTER_SP, ASSEMBLE_REGISTER_AF, ASSEMBLE_REGISTER_IX
    DEFM ASSEMBLE_REGISTER_IY
    DEFM ASSEMBLE_FLAG_NZ, ASSEMBLE_FLAG_NC, ASSEMBLE_FLAG_PO, ASSEMBLE_FLAG_PE


_assemble_instructions_table:
    DEFW _assemble_instructions_a
    DEFW _assemble_instructions_b
    DEFW _assemble_instructions_c
    DEFW _assemble_instructions_d
    DEFW _assemble_instructions_e
    DEFW 0
    DEFW 0
    DEFW _assemble_instructions_h
    DEFW _assemble_instructions_i
    DEFW _assemble_instructions_j
    DEFW 0
    DEFW _assemble_instructions_l
    DEFW 0
    DEFW _assemble_instructions_n
    DEFW _assemble_instructions_o
    DEFW _assemble_instructions_p
    DEFW 0
    DEFW _assemble_instructions_r
    DEFW _assemble_instructions_s
    DEFW 0
    DEFW 0
    DEFW 0
    DEFW 0
    DEFW _assemble_instructions_x


_assemble_instructions_str:
    ; Each entry is composed as:
    ; byte instruction_name[4]; // Offset 0
    ; byte operand_count;       // Offset 4 (bit 7 is 1 if count is variable)
    ; word instruction_code;    // Offset 5
    ; word parser_routine;      // Offset 7
_assemble_instructions_a:
    NEW_INSTRUCTION("adc",  ASSEMBLE_VARIABLE_COUNT | 0x3, 0x0000, _assemble_instr_adc)
    NEW_INSTRUCTION("add",  0x83, 0x0000, _assemble_instr_add)
    NEW_INSTRUCTION("and",  2,    0x0000, _assemble_instr_and)
_assemble_instructions_b:
    NEW_INSTRUCTION("bit",  3,    0x0000, _assemble_instr_bit)
_assemble_instructions_c:
    NEW_INSTRUCTION("call", 0x83, 0x0000, _assemble_instr_call)
    NEW_INSTRUCTION("ccf",  1,    0x003f, _assemble_instr_ccf)
    NEW_INSTRUCTION("cp",   2,    0x0000, _assemble_instr_cp)
    NEW_INSTRUCTION("cpd",  1,    0xeda9, _assemble_instr_cpd)
    NEW_INSTRUCTION("cpdr", 1,    0xedb9, _assemble_instr_cpdr)
    NEW_INSTRUCTION("cpi",  1,    0xeda1, _assemble_instr_cpi)
    NEW_INSTRUCTION("cpir", 1,    0xedb1, _assemble_instr_cpir)
    NEW_INSTRUCTION("cpl",  1,    0x002f, _assemble_instr_cpl)
_assemble_instructions_d:
    NEW_INSTRUCTION("daa",  1,    0x0027, _assemble_instr_daa)
    NEW_INSTRUCTION("dec",  2,    0x0000, _assemble_instr_dec)
    NEW_INSTRUCTION("di",   1,    0x00f3, _assemble_instr_di)
    NEW_INSTRUCTION("djnz", 2,    0x0000, _assemble_instr_djnz)
_assemble_instructions_e:
    NEW_INSTRUCTION("ei",   1,    0x00fb, _assemble_instr_ei)
    NEW_INSTRUCTION("ex",   3,    0x0000, _assemble_instr_ex)
    NEW_INSTRUCTION("exx",  1,    0x00d9, _assemble_instr_exx)
_assemble_instructions_h:
    NEW_INSTRUCTION("halt", 1,    0x0076, _assemble_instr_halt)
_assemble_instructions_i:
    NEW_INSTRUCTION("im",   2,    0x0000, _assemble_instr_im)
    NEW_INSTRUCTION("in",   3,    0x0000, _assemble_instr_in)
    NEW_INSTRUCTION("inc",  2,    0x0000, _assemble_instr_inc)
    NEW_INSTRUCTION("ind",  1,    0xedaa, _assemble_instr_ind)
    NEW_INSTRUCTION("indr", 1,    0xedba, _assemble_instr_indr)
    NEW_INSTRUCTION("ini",  1,    0xeda2, _assemble_instr_ini)
    NEW_INSTRUCTION("inir", 1,    0xedb2, _assemble_instr_inir)
_assemble_instructions_j:
    NEW_INSTRUCTION("jp",   0x83, 0x0000, _assemble_instr_jp)
    NEW_INSTRUCTION("jr",   0x83, 0x0000, _assemble_instr_jr)
_assemble_instructions_l:
    NEW_INSTRUCTION("ld",   3,    0x0000, _assemble_instr_ld)
    NEW_INSTRUCTION("ldd",  1,    0xeda8, _assemble_instr_ldd)
    NEW_INSTRUCTION("lddr", 1,    0xedb8, _assemble_instr_lddr)
    NEW_INSTRUCTION("ldi",  1,    0xeda0, _assemble_instr_ldi)
    NEW_INSTRUCTION("ldir", 1,    0xedb0, _assemble_instr_ldir)
_assemble_instructions_n:
    NEW_INSTRUCTION("neg",  1,    0xed44, _assemble_instr_neg)
    NEW_INSTRUCTION("nop",  1,    0x0000, _assemble_instr_nop)
_assemble_instructions_o:
    NEW_INSTRUCTION("or",   2,    0x0000, _assemble_instr_or)
    NEW_INSTRUCTION("org",  2,    0x0000, _assemble_direct_org)
    NEW_INSTRUCTION("otdr", 1,    0xedbb, _assemble_instr_otdr)
    NEW_INSTRUCTION("otir", 1,    0xedb3, _assemble_instr_otir)
    NEW_INSTRUCTION("out",  3,    0x0000, _assemble_instr_out)
    NEW_INSTRUCTION("outd", 1,    0xedab, _assemble_instr_outd)
    NEW_INSTRUCTION("outi", 1,    0xeda3, _assemble_instr_outi)
_assemble_instructions_p:
    NEW_INSTRUCTION("pop",  2,    0x0000, _assemble_instr_pop)
    NEW_INSTRUCTION("push", 2,    0x0000, _assemble_instr_push)
_assemble_instructions_r:
    NEW_INSTRUCTION("res",  3,    0x0000, _assemble_instr_res)
    NEW_INSTRUCTION("ret",  0x82, 0x00c9, _assemble_instr_ret)
    NEW_INSTRUCTION("reti", 1,    0xed4d, _assemble_instr_reti)
    NEW_INSTRUCTION("retn", 1,    0xed45, _assemble_instr_retn)
    NEW_INSTRUCTION("rl",   2,    0x0000, _assemble_instr_rl)
    NEW_INSTRUCTION("rla",  1,    0x0017, _assemble_instr_rla)
    NEW_INSTRUCTION("rlc",  2,    0x0000, _assemble_instr_rlc)
    NEW_INSTRUCTION("rlca", 1,    0x0007, _assemble_instr_rlca)
    NEW_INSTRUCTION("rld",  1,    0xed6f, _assemble_instr_rld)
    NEW_INSTRUCTION("rr",   2,    0x0000, _assemble_instr_rr)
    NEW_INSTRUCTION("rra",  1,    0x001f, _assemble_instr_rra)
    NEW_INSTRUCTION("rrc",  2,    0x0000, _assemble_instr_rrc)
    NEW_INSTRUCTION("rrca", 1,    0x000f, _assemble_instr_rrca)
    NEW_INSTRUCTION("rrd",  1,    0xed67, _assemble_instr_rrd)
    NEW_INSTRUCTION("rst",  2,    0x0000, _assemble_instr_rst)
_assemble_instructions_s:
    NEW_INSTRUCTION("sbc",  0x83, 0x0000, _assemble_instr_sbc)
    NEW_INSTRUCTION("scf",  1,    0x0037, _assemble_instr_scf)
    NEW_INSTRUCTION("set",  3,    0x0000, _assemble_instr_set)
    NEW_INSTRUCTION("sla",  2,    0x0000, _assemble_instr_sla)
    NEW_INSTRUCTION("sra",  2,    0x0000, _assemble_instr_sra)
    NEW_INSTRUCTION("srl",  2,    0x0000, _assemble_instr_srl)
    NEW_INSTRUCTION("sub",  2,    0x0000, _assemble_instr_sub)
_assemble_instructions_x:
    NEW_INSTRUCTION("xor",  2,    0x0000, _assemble_instr_xor)
    DEFB 'z'    ; must be > than word "xor"