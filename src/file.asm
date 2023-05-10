; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>
;
; SPDX-License-Identifier: Apache-2.0

    INCLUDE "zos_err.asm"
    INCLUDE "zos_sys.asm"

    DEFC LINE_BUFFER_LENGTH = 80
    DEFC LINE_MAX_LENGTH = 255

    EXTERN memsep

    SECTION TEXT

    ; Open the given file name as input
    ; Parameters:
    ;   HL - Filename
    ; Returns:
    ;   A - ERR_SUCCESS on success, error code else
    ; Alters:
    ;   A, BC, HL
    PUBLIC file_open_input
file_open_input:
    ld b, h
    ld c, l
    ; The input file only needs to be opened in read mode
    ld h, O_RDONLY
    OPEN()
    ; An error occurred if A is negative, in any case, save it
    ld (file_input_dev), a
    or a
    jp m, file_open_error
    ; Success, we can return 0
    xor a
    ret
file_open_error:
    neg
    ret


    ; Open the given file as the output file
    ; Parameters;
    ;   HL - Name of the output file. If it doesn't exist it will be created,
    ;        if it already exists, it will overwritten.
    ; Returns:
    ;   A - ERR_SUCCESS on success, error code else
    ; Alters:
    ;   A, BC, HL
    PUBLIC file_open_output
file_open_output:
    ld b, h
    ld c, l
    ld h, O_WRONLY | O_TRUNC | O_CREAT
    OPEN()
    ld (file_output_dev), a
    or a
    jp m, file_open_error
    ; Success, we can return 0
    xor a
    ret


    ; Read the next line of the input file, the \n character will be replaced with \0
    ; If the end of the file has been reached, the returned size will be 0
    ; Parameters:
    ;   -
    ; Returns:
    ;   HL - Address of the buffer storing the line. The buffer content can be altered
    ;        as long as the modifications remain within the length.
    ;    C - Length of the line (without NULL-terminator nor \n)
    ;    A - ERR_SUCCESS on success, error code else.
    ; Alters:
    ;   A, BC, DE, HL
    PUBLIC file_read_input_line
file_read_input_line:
    ASSERT(LINE_MAX_LENGTH < 256)
    ; Fill the given buffer with the current line available
    ld c, 0xFF ; Number of bytes read
    ld de, file_cur_line - 1
_file_read_input_line_loop:
    inc de
    inc c
_file_read_input_line_loop_no_inc:
    ; Get the next character of the file
    call _file_read_next_char
    ; Check if we reached the end of file
    cp ERR_NO_MORE_ENTRIES
    jr z, _file_read_input_line_no_more_entries
    ; Else, simply check for an error
    or a
    ret nz
    ; B contains the next character read
    ld a, b
    cp '\n'
    jr z, _file_read_input_line_end
    ; Not a newline, we can store and continue
    ld (de), a
    ; Only increment DE and C if we still have space in the buffer
    ld a, LINE_MAX_LENGTH
    sub c
    jp nz, _file_read_input_line_loop
    jr _file_read_input_line_loop_no_inc
_file_read_input_line_no_more_entries:
    ; Jumped here if there are no more characters in the file. If C is not 0,
    ; the current line is not empty (file doesn't file an ending \n)
    ; Return if C is 0
    inc c
    dec c
    ret z
    ; Else, fall-through, act as if a \n was received
_file_read_input_line_end:
    ; No more characters in the file or \n encountered
    xor a
    ld (de), a
    ld hl, file_cur_line
    ret


    ; Read next available character from the input file.
    ; Register C is preserved.
    ; Parameters:
    ;   -
    ; Returns:
    ;   A - ERR_SUCCESS on success, ERR_NO_MORE_ENTRIES if end of file, error code else
    ;   B - Character received
    ; Alters:
    ;   A, HL, B
_file_read_next_char:
    ld hl, file_buffer_remaining
    ld a, (hl)
    or a
    jr z, _file_read_next_char_reload
    ; The buffer still has some bytes, decrement the remaining count
    dec (hl)
    ; And retrieve the next char
    ld hl, (file_buffer_remaining_addr)
    ld b, (hl)
    inc hl
    ld (file_buffer_remaining_addr), hl
    xor a
    ret
_file_read_next_char_reload:
    push de
    push bc
    ; The buffer doesn't have any more bytes, read from the file
    ld de, file_buffer
    ld bc, LINE_BUFFER_LENGTH
    ld a, (file_input_dev)
    ld h, a
    READ()
    ; Check if any error occurred
    or a
    ret nz
    ; If no byte was read from the file, return ERR_NO_MORE_ENTRIES
    ASSERT(LINE_BUFFER_LENGTH < 256)
    or c
    jr z, _file_read_next_char_end_of_file
    ; Decrement the remaining bytes and store it
    dec c
    ld a, c
    ld (file_buffer_remaining), a
    ; Store the next byte in B, store the "remaining" buffer address
    ld a, (de)
    inc de
    ld (file_buffer_remaining_addr), de
    ; Pop the values on the stack and set the returned values
    pop bc
    pop de
    ; Return success
    ld b, a
    xor a
    ret
_file_read_next_char_end_of_file:
    pop bc
    pop de
    ld a, ERR_NO_MORE_ENTRIES
    ret


    ; Close both the input and the output file. If any of them is not opened,
    ; it will be ignored.
    ; Parameters:
    ;   None
    ; Returns:
    ;   None
    PUBLIC file_close_input_output
file_close_input_output:
    ; Load both descriptor a the same time since the syscall will not alter
    ; BC
    ld bc, (file_input_dev)
    ld h, b
    bit 7, h
    jr nz, _file_close_input_output_next
    CLOSE()
_file_close_input_output_next:
    ld h, c
    bit 7, h
    ret nz
    CLOSE()
    ret


    ; Write the given buffer to the output file. If the buffer is crossing virtual
    ; page boundary, the write request will be split into several ones.
    ; Parameters:
    ;   HL - Buffer to write to the file
    ;   BC - Size of the buffer
    ; Returns:
    ;   A - ERR_SUCCESS on success, error code else
    ; Alters:
    ;   A, BC, DE, HL
    PUBLIC file_write_output
file_write_output:
    ; If BC is 0, return success directly
    ld a, b
    or c
    ret z
    ; Check if the buffer goes beyond one virtual page (the kernel would refuse it else)
    push hl
    ld a, h
    ; Get the virtual page of HL and store it in D
    and 0xc0
    ld d, a
    ; Get the address of the last byte of the buffer
    dec hl
    add hl, bc
    ; Get the virtual page of this new address
    ld a, h
    and 0xc0
    ld h, a
    ; If it is the same as the former one, the buffer doesn't cross-boundary, we have to call
    ; WRITE syscall only once
    cp d
    jr z, _file_write_output_same_page
    ; The buffer crosses boundaries, at most 3, the first address to write is the original one, but
    ; we need to modify the size.
    pop hl  ; Original buffer address
    ; Calculate the boundary address out of the virtual page count
    ld a, d
    add 0x40    ; the virtual page is not 3 for sure, so increment it by 1
    ld d, a
    ld e, 0
    push de     ; save next virtual size to write
    ; DE contains the end of page address, calculate the new size
    push bc ; Save original size
    ex de, hl
    or a
    sbc hl, de
    ; Size is in HL
    ld b, h
    ld c, l
    ; Buffer is in DE, write now
    call _file_write_output_same_page_de
    ; Calculate remaining size to write
    pop hl
    or a
    sbc hl, bc
    ; Put the remaining size in BC
    ld b, h
    ld c, l
    ; Pop the next buffer to write
    pop hl
    ; If there was no error, continue writing
    or a
    jp z, file_write_output
    ret
    ; Parameters:
    ;   [SP] - Address of the buffer to print
    ;   BC - Size of the buffer to print
    ; Returns:
    ;   BC - Number of bytes written
_file_write_output_same_page:
    ; Buffer to write to file in DE
    pop de
_file_write_output_same_page_de:
    ; Opened descriptor in H
    ld a, (file_output_dev)
    ld h, a
    WRITE()
    ret


    SECTION DATA
file_input_dev:  DEFS 1
file_output_dev: DEFS 1
file_cur_line:   DEFS LINE_MAX_LENGTH + 1 ;NULL-terminator
file_buffer:     DEFS LINE_BUFFER_LENGTH

    ; Number of char remaining to process on the file_buffer
file_buffer_remaining:      DEFW 0
file_buffer_remaining_addr: DEFW 0
