; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>
;
; SPDX-License-Identifier: Apache-2.0

    INCLUDE "zos_sys.asm"
    INCLUDE "zos_err.asm"
    INCLUDE "assembler_macros_h.asm"

    DEFC DEFAULT_ORG = 0x4000

;******************************************************************************
;*                            LINKER SECTION                                  *
;******************************************************************************

    ORG 0x4000
    SECTION TEXT
    SECTION DATA
    SECTION BINARY_OUTPUT

;******************************************************************************
;*                             CODE  SECTION                                  *
;******************************************************************************

    SECTION TEXT

    EXTERN data_get
    EXTERN data_insert
    EXTERN data_list_new_entry
    EXTERN data_list_prepend_entry
    EXTERN data_list_get_value
    EXTERN data_list_get_next
    EXTERN file_open_input
    EXTERN file_read_input_line
    EXTERN file_open_output
    EXTERN file_write_output
    EXTERN file_close_input_output
    EXTERN parser_parse_line
    EXTERN _assemble_def_label
    EXTERN _assemble_ref_label
    EXTERN word_to_ascii


    ; Parameters:
    ;   BC - 0 if no argument, 1 else
    ;   DE - Address of the string parameter when BC is 1
assemble_main:
    ld a, b
    or c
    jr z, _version_usage
    ; Separate input file from output file
    call assemble_detect_parameters
    ; Check if an error occurred
    or a
    jr nz, _usage
    ; HL contains the input file name, DE contains the output file name
    ; Open both files before even starting assembling
    push de
    call file_open_input
    or a
    jr nz, _error_ret_exit
    pop hl  ; Pop output file name
    call file_open_output
    or a
    jr nz, _error_exit
    ; Assemble the opened file
    call assemble_file
    ; If an error occurred we have to close the file and exit
    cp ERR_NO_MORE_ENTRIES
    jr nz, _error_close_ret_exit
    ; Replace the labels references found
    call assemble_replace_references
    or a
    jr nz, _error_close_ret_exit
    ; Writing the assembled code to the output file
    call assemble_write_output
    ; In both cases we have to close the files
    push af
    call file_close_input_output
    pop af
    or a
    jr nz, _error_exit
    jr _exit

_error_close_ret_exit:
    call file_close_input_output
    jr _error_exit
    ; Pop the output file name from the stack and return an error
_error_ret_exit:
    pop de
_error_exit:
    S_WRITE3(DEV_STDOUT, error_str, error_str_end - error_str)
_exit:
    EXIT()

_version_usage:
    S_WRITE3(DEV_STDOUT, version_str, version_str_end - version_str)
_usage:
    S_WRITE3(DEV_STDOUT, usage_str, usage_str_end - usage_str)
    jr _exit

error_str: DEFM "error occurred\n", 0
error_str_end:
version_str:
    DEFM "ver. "
    INCBIN "version.txt"
    DEFM "\n"
version_str_end:
usage_str: DEFM "usage: zealasm <input.asm> <output.bin>\n", 0
usage_str_end:


    ; Parse the one-line string parameter into two parameters:
    ; the input file and the output file
    ; Parameters:
    ;   DE - Address of the string to split/parse
    ; Returns:
    ;   HL - String of the input file
    ;   DE - String of the output file
assemble_detect_parameters:
    ex de, hl
    call strltrim
    ld a, ' '
    call strsep
    ; If A is not 0, the delimiter was not found, return directly
    or a
    ret nz
    ; Else, DE is pointing to the second string, trim it and make sure there
    ; is not third parameter
    push hl
    ex de, hl
    call strltrim
    call strrtrim
    ld a, ' '
    call strsep
    or a
    ld a, 1
    ; If A is 0, there is a third operand, treat it as an error
    jr z, _assemble_detect_parameters_error
    ; Restore HL and DE and return
    ex de, hl
    xor a
_assemble_detect_parameters_error:
    pop hl
    ret


    ; Assemble the file that was opened as "input" file.
    ; This shall be done line and line while storing the bytes generated.
    ; There will be two passes:
    ;   - The first one wil assemble the instructions while recording all the
    ;     labels defines and label references.
    ;   - The second one will go through all the missing referenced labels
    ;     and look for their actual address in the final binary.
    ; The final binary will be buffered instead of being written to a temporary file.
    ; Writing and reading to and from a file is rather slow, we want to avoid that.
    ; Parameters:
    ;   -
    ; Returns:
    ;   A - ERR_SUCCESS on success, error code else (message will be printed)
    ; Alters:
    ;   A, BC, DE, HL
assemble_file:
    ld hl, (current_line)
    inc hl
    ld (current_line), hl
    call file_read_input_line
    or a
    ret nz
    ; Ignore empty lines
    or c
    jr z, assemble_file
    ; Take string in parameter 1 to parse
    call parser_parse_line
    ; B is the error code or the number of bytes
    ; HLDE contains the instruction code
    ld a, b
    ; Check for errors
    cp ASSEMBLE_ERROR
    jr nc, assemble_file_error
    ld b, h
    ld c, l
    ld hl, (assemble_binary)
    ; If A is 1, E is set
    ; If A is 2, D E are set
    ; If A is 3, C D E are set
    ; If A is 4, B C D E are set
    ; If A is 0, an error occurred
    ; In all cases, do NOT alter A
    or a
    jr z, assemble_file_0
    cp 1
    jr z, assemble_file_1
    cp 2
    jr z, assemble_file_2
    cp 3
    jr z, assemble_file_3
    ; Default: 4 parameters
    ld (hl), b
    inc hl
assemble_file_3:
    ld (hl), c
    inc hl
assemble_file_2:
    ld (hl), d
    inc hl
assemble_file_1:
    ld (hl), e
    inc hl
    ld (assemble_binary), hl
assemble_file_0:
    ; Keep the number of bytes of the current instruction in C
    ld c, a
    ; Check if the instruction contain a reference to a label
    ; -------------------------------------------------------
    ; Store the next instruction address in DE
    ex de, hl
    ld hl, (_assemble_ref_label)
    ld a, h
    or a
    ; A is 0 is no reference was present in the current instruction
    jp z, assemble_check_label_ref
    ; A reference to a label was present! Store it for later. Associate the virtual address - 2
    ; to it. If the instruction needs a relative jump (one byte displacement), we will only need
    ; to check the byte at [virtual_address - 2].
    dec de
    dec de
    call data_list_new_entry
    ret nz
    ; Prepend the current entry to the one we already have
    ld hl, (list_ref)
    ex de, hl
    call data_list_prepend_entry
    ld (list_ref), hl
    ; Check if there is any label defined
    ; -----------------------------------
assemble_check_label_ref:
    ; Store the label string in DE
    ld de, (_assemble_def_label)
    ld hl, (current_addr)
    ; Check if there is a label
    ld a, d
    or a
    call nz, assemble_add_label
    ; In all cases, A should be 0 here
    or a
    ret nz
    ; Update the current address in the binary, C still contains the number of bytes in the
    ; current instruction
    ld b, a ; A is 0, set B to 0
    ; Check for a directive
    ld a, (directive_detected)
    or a
    jp z, adjust_current_addr
    ; Directive was detected, reset the flag and get its size
    xor a
    ld (directive_detected), a
    ld bc, (directive_size)
adjust_current_addr:
    add hl, bc
    ld (current_addr), hl
    jp assemble_file


    ; Print an assembly error
    ; Parameters:
    ;   B - Assembly error code
    ; Returns:
    ;   A - ERR_FAILURE
assemble_file_error:
    ld a, b
    sub ASSEMBLE_ERROR
    rlca    ; multiply by 2 because each entry is 2-word big
    ld hl, assemble_error_table
    add l
    ld l, a
    adc h
    sub l
    ld h, a
    ; Dereference the string address
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    ; Save it on the stack for later
    push hl
    ; Printing the line number
    ld hl, (current_line)
    ld de, line_str_num
    call word_to_ascii
    ld (hl), ':'
    inc hl
    ld (hl), ' '
    ld hl, line_str
    call strlen
    ex de, hl
    S_WRITE1(DEV_STDOUT)
    ; Pop back the error message from the stack
    pop hl
    call strlen
    ex de, hl
    S_WRITE1(DEV_STDOUT)
    ld a, ERR_FAILURE
    ret

line_str: DEFM "line "
line_str_num: DEFS 5    ; at most 5 digits

assemble_error_table:
    DEFW unknown_error_str
    DEFW invalid_operand_str
    DEFW syntax_error_str
    DEFW out_of_range_str
    DEFW label_too_long_str
    DEFW org_already_set_str
    DEFW unknown_direct_str

unknown_error_str:   DEFM "unknown operand\n", 0
invalid_operand_str: DEFM "invalid operand\n", 0
syntax_error_str:    DEFM "syntax error\n", 0
out_of_range_str:    DEFM "out of range\n", 0
label_too_long_str:  DEFM "label name too long\n", 0
org_already_set_str: DEFM "ORG already set\n", 0
unknown_direct_str:  DEFM "unknown directive\n", 0


    ; Jump to this branch when a label is defined on the current line
    ; Parameters:
    ;   HL - Current address in the generated binary
    ;   DE - Label address (string)
    ;   B - Number of bytes in the current instruction
    ; Alters:
    ;   A, DE
assemble_add_label:
    push bc
    push hl
    ; Insert the label and its address in the hashmap
    ex de, hl
    call data_insert
    pop hl
    pop bc
    ret


    ; Routine used to replace the labels references found in the code.
    ; Currently, the addresses to replace have placeholders (0x0000).
    ; Parameters:
    ;   -
    ; Returns:
    ;   A - ERR_SUCCESS on success, ERR_NO_SUCH_ENTRY if there is an undefined reference.
    ; Alters:
    ;   A, BC, DE, HL
assemble_replace_references:
    ; Browse the list of references until we found NULL pointer
    ld hl, (list_ref)
assemble_replace_references_loop:
    ld a, h
    or l
    ret z
    ; HL points to the label name (string). Look for it in the hashmap
    push hl
    call data_get
    pop hl
    ; On error, return directly
    or a
    ret nz
    ; DE contains the address of the label in the binary, get the place where we need to put
    ; this value
    ; HL contains the current label reference, get the place where the reference was in the code.
    push hl
    call data_list_get_value
    ; The value is the virtual address (in the current program) to patch
    ld a, (hl)
    or a
    ; If A is NOT 0, the instruction is a relative jump
    jr nz, assemble_replace_relative
    ; The reference was absolute, replace the content
    ld (hl), e
    inc hl
    ld (hl), d
assemble_replace_references_next:
    ; Go to the next node
    pop hl
    call data_list_get_next
    jp assemble_replace_references_loop
    ; Parameters:
    ;   HL - Pointer to the relative instruction (displacement in HL + 1)
    ;   DE - Target address of the label (virtual address, in the binary file)
assemble_replace_relative:
    ; We have to find the virtual address of the current instruction:
    ; Vhl = (HL - assemble_binary_data) + ORGa
    push hl
    ld bc, -assemble_binary_data
    add hl, bc
    ld bc, (org_addr)
    add hl, bc
    ; Then, calculate the difference between DE and Vhl:
    ; delta = DE - Vhl - 2
    ; -2 because djnz/jr instruction require the code to subtract 2
    scf
    ex de, hl
    inc de
    sbc hl, de
    ; HL must be a signed 8-bit value. So, the highest bit of L must be 1 if H is 0xFF, or 0 if
    ; H is 0, else, it's an error because the displacement is too big.
    add hl, hl
    ld a, h
    jp c, assemble_relative_check_negative
    ; Check positive value, H (A) must be 0
    or a
    jr nz, assemble_too_far
    jp assemble_relative_valid
assemble_relative_check_negative:
    ; Check negative value, H (A) must be 0xFF
    inc a
    jr nz, assemble_too_far
assemble_relative_valid:
    ; Restore the carry in the displacement
    ld a, l
    rra
    ; Store it in the binary
    pop hl
    inc hl
    ld (hl), a
    jp assemble_replace_references_next


assemble_too_far:
    pop hl  ; virtual address
    pop hl  ; node containing the current label reference
    call strlen
    ex de, hl
    S_WRITE1(DEV_STDOUT)
    S_WRITE3(DEV_STDOUT, too_far_str, too_far_str_end - too_far_str)
    ld a, ERR_FAILURE
    ret
too_far_str: DEFM " label target is too far\n", 0
too_far_str_end:


    ; This routine will write the binary data generated to the output file.
    ; Parameters:
    ;   -
    ; Returns:
    ;   A - ERR_SUCCESS on success, error code else.
    ; Alters;
    ;   A, BC, DE, HL
assemble_write_output:
    ; Calculate the number of bytes of the binary file
    ld de, assemble_binary_data
    ld hl, (assemble_binary)
    or a
    sbc hl, de
    ; Store the number of bytes in BC
    ld b, h
    ld c, l
    ; DE contains the beginning of the binary file
    ex de, hl
    jp file_write_output


    ; Set the origin of the address for the program
    ; Parameters:
    ;   DE - new ORG program address
    ; Returns:
    ;   B - 0 on success, error code else
    PUBLIC assemble_set_org
assemble_set_org:
    ; Only set the ORG if the it is set before any instruction
    ld hl, (current_addr)
    ld bc, (org_addr)
    xor a
    sbc hl, bc
    jr nz, assemble_set_org_error
    ; Valid, we can save it!
    ex de, hl
    ld (current_addr), hl
    ld (org_addr), hl
    ld b, 0
    ret
assemble_set_org_error:
    ld b, ASSEMBLE_ORG_ALREADY_SET
    ret


    ; Define a message. In other words, store raw bytes in the binary
    ; file.
    ; Parameters:
    ;   HL - Byte sequence to store in the file
    ;   BC - Length of the sequence
    ; Returns:
    ;   B - 0 on success, error code else
    PUBLIC assemble_def_message
assemble_def_message:
    ; If BC is 0, return as a success
    ld a, b
    or c
    ret z
    ; Save the directive size here so that we don't need to save BC on the stack
    ld (directive_size), bc
    ld de, (assemble_binary)
    ldir
    ; Save the current binary cursor
    ld (assemble_binary), de
    ; The current virtual address will be updated by the caller (because of the
    ; potential presence of a label at the current line)
    ; Set the directive flag to 1
    ld a, 1
    ld (directive_detected), a
    ; B is already 0 here because of LDIR
    ret


    ; Define a byte. In other words, store one raw byte in the binary
    ; Parameters:
    ;   A - Byte to store in the file
    ; Returns:
    ;   B - 0 on success, error code else
    PUBLIC assemble_def_byte
assemble_def_byte:
    ld hl, (assemble_binary)
    ld (hl), a
    inc hl
    ld (assemble_binary), hl
    ; The current virtual address will be updated by the caller (because of the
    ; potential presence of a label at the current line)
    ; Set the directive flag to 1
    ld bc, 1
assemble_def_set_size:
    ld (directive_size), bc
    ld a, 1
    ld (directive_detected), a
    ret


    ; Define a word (16-bit value). In other words, store a word in the binary
    ; Parameters:
    ;   HL - Word to store in the file
    ; Returns:
    ;   B - 0 on success, error code else
    PUBLIC assemble_def_word
assemble_def_word:
    ex de, hl
    ld hl, (assemble_binary)
    ld (hl), e
    inc hl
    ld (hl), d
    inc hl
    ld (assemble_binary), hl
    ; The current virtual address will be updated by the caller (because of the
    ; potential presence of a label at the current line)
    ; Set the directive flag to 1
    ld bc, 2
    jr assemble_def_set_size


    ; Define an empty space
    ; Parameters:
    ;   HL - Size of the empty space to allocate
    ; Returns:
    ;   B - 0 on success, error code else
    PUBLIC assemble_def_space
assemble_def_space:
    ld b, h
    ld c, l
    ; Check if the size is 0
    ld a, b
    or c
    ret z
    ; Copy BC empty bytes
    push bc
    ld hl, (assemble_binary)
    ld (hl), 0
    ld d, h
    ld e, l
    inc de
    dec bc
    ; If BC is 0 after decrementing, do not execute ldir
    ld a, b
    or c
    jr z, assemble_def_space_empty
    ldir
assemble_def_space_empty:
    ld (assemble_binary), de
    pop bc
    jr assemble_def_set_size


    SECTION DATA
    ; Store the current line number in the file, very convenient in case of error
current_line: DEFW 0
    ; Current address in the binary file
current_addr: DEFW DEFAULT_ORG
    ; Virtual address where the program starts
org_addr:     DEFW DEFAULT_ORG
    ; Address of the list containing all the references found in the program
list_ref: DEFW 0
    ; Mark whether a special directive was used and how big it was
directive_detected: DEFB 0
directive_size: DEFW 0


    SECTION BINARY_OUTPUT
assemble_binary: DEFW assemble_binary_data
assemble_binary_data: