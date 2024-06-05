; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>
;
; SPDX-License-Identifier: Apache-2.0

    SECTION TEXT

    ; Trim leading spaces and tabs characters from a string pointed by HL
    ; Parameters:
    ;   HL - String to trim leading spaces from
    ; Returns:
    ;   HL - Address of the non-space character from the string
    ;   A - First non-space character
    ; Alters:
    ;   A
    PUBLIC strltrim
strltrim:
    dec hl
_strltrim_loop:
    inc hl
    ld a, (hl)
    cp ' '
    jp z, _strltrim_loop
    cp '\t'
    jp z, _strltrim_loop
    ret


    ; Trim leading space character from a string pointed by HL
    ; Parameters:
    ;   HL - String to trim leading spaces from
    ;   BC - Length of the string
    ; Returns:
    ;   HL - Address of the non-space character from the string
    ;   BC - Length of the remaining string
    ; Alters:
    ;   A
    PUBLIC strnltrim
strnltrim:
    dec hl
    inc bc
_strnltrim_loop:
    inc hl
    dec bc
    ; Return if BC is 0 now
    ld a, b
    or c
    ret z
    ld a, ' '
    cp (hl)
    jp z, _strnltrim_loop
    ret

    ; Trim trailing space character from a string pointed by HL
    ; Parameters:
    ;   HL - String to trim leading spaces from
    ;   BC - Length of the string
    ; Returns:
    ;   HL - Address of the non-space character from the string
    ;   BC - Length of the remaining string
    ; Alters:
    ;   A
    PUBLIC strnrtrim
strnrtrim:
    push hl
    add hl, bc
    inc bc
_strnrtrim_loop:
    ; Decrement BC and check if it is 0
    dec bc
    ld a, b
    or c
    jr z, _strnrtrim_end
    dec hl
    ld a, ' '
    cp (hl)
    jr z, _strnrtrim_loop
    inc hl
    ld (hl), 0
_strnrtrim_end:
    pop hl
    ret


    ; Trim trailing space character from a string pointed by HL.
    ; A \0 will be placed after the last non-space character
    ; Parameters:
    ;   HL - String to trim trailing spaces from
    ; Returns:
    ;   -
    ; Alters:
    ;   A
    PUBLIC strrtrim
strrtrim:
    push hl
    push de
    ; Make DE always point to the last non-space character
    dec hl
    ld d, h
    ld e, l
_strrtrim_loop:
    inc hl
    ld a, (hl)
    or a
    jr z, _strrtrim_end
    ; Check if we encountered a space or tab
    cp ' '
    jr z, _strrtrim_loop
    cp '\t'
    jr z, _strrtrim_loop
    ; Not a tab nor a loop, update HL and DE
    ld d, h
    ld e, l
    jp _strrtrim_loop
_strrtrim_end:
    ; End of the string, place the \0 after the last non-space char
    inc de
    ld (de), a  ; A is 0 here
    pop de
    pop hl
    ret



    ; Compare two NULL-terminated strings.
    ; Parameters:
    ;   HL - First NULL-terminated string address
    ;   DE - Second NULL-terminated string address
    ; Returns:
    ;   A - 0 if strings are identical
    ;       > 0 if DE is greater than HL
    ;       < 0 if HL is greater than DE
    ; Alters:
    ;       A
    PUBLIC strcmp
strcmp:
    push hl
    push de
    dec hl
    dec de
_strcmp_compare:
    inc hl
    inc de
    ld a, (de)
    sub (hl)
    jr nz, _strcmp_end
    ; Check if both strings have reached the end
    ; If this is the case, or (hl) will reset in zero flag to be set
    ; In that case, no need to continue, we can return, with flag Z set
    or (hl)
    jr nz, _strcmp_compare
_strcmp_end:
    pop de
    pop hl
    ret


    ; Same as strcmp, but at most BC bytes will be read.
    ; Parameters:
    ;   HL - First NULL-terminated string address
    ;   DE - Second NULL-terminated string address
    ;   BC - Maximum number of char to compare
    ; Returns:
    ;   A - 0 if strings are identical
    ;       > 0 if DE is greater than HL
    ;       < 0 if HL is greater than DE
    ; Alters:
    ;       A
    PUBLIC strncmp
strncmp:
    push hl
    push de
    push bc
    dec hl
    dec de
    inc bc
_strncmp_compare:
    dec bc
    inc hl
    inc de
    ld a, b
    or c
    jr z, _strncmp_end
    ld a, (de)
    sub (hl)
    jr nz, _strncmp_end
    ; Check if both strings have reached the end
    ; If this is the case, or (hl) will reset in zero flag to be set
    ; In that case, no need to continue, we can return, with flag Z set
    or (hl)
    jr nz, _strncmp_compare
_strncmp_end:
    pop bc
    pop de
    pop hl
    ret


    ; Same as strncmp, but optimized to test A bytes.
    ; Parameters:
    ;   HL - First NULL-terminated string address
    ;   DE - Second NULL-terminated string address
    ;   A  - Maximum number of char to compare (must not be 0)
    ; Returns:
    ;   A - 0 if strings are identical
    ;       > 0 if DE is greater than HL
    ;       < 0 if HL is greater than DE
    ; Alters:
    ;       A
    PUBLIC strncmp_opt
strncmp_opt:
    push hl
    push de
    push bc
    ld b, a
_strncmp_opt_compare:
    ld a, (de)
    sub (hl)
    jr nz, _strncmp_opt_end
    ; Check if we still have bytes to test
    dec b
    jr z, _strncmp_opt_end
    ; Check if both strings have reached the end
    ; A is 0 here, check if (HL) is also 0
    or (hl)
    inc de
    inc hl
    jp nz, _strncmp_opt_compare
_strncmp_opt_end:
    pop bc
    pop de
    pop hl
    ret


    ; Look for the delimiter A in the memory pointed by HL
    ; Once it finds it, the token is replace by \0
    ; Parameters:
    ;       HL - Address of the string
    ;       BC - Size of the string
    ;       A - Delimiter
    ; Returns:
    ;       HL - Original string address
    ;       DE - Address of the next string (address of the token found +1)
    ;       BC - Length of the remaining string
    ;       A - 0 if the delimiter was found, non-null value else
    ; Alters:
    ;       A, DE, BC
    PUBLIC memsep
memsep:
    ld d, h
    ld e, l
    cpir
    ; Regardless whether BC is 0 is not, we have to check the last character
    ; and replace it. This is due to the fact that if the separator is the
    ; last character of the string, BC will still be 0, even though we've
    ; found it.
    dec hl
    sub (hl)
    jr nz, _memsep_not_set
    ld (hl), a
_memsep_not_set:
    inc hl
    ex de, hl
    ret


    ; Look for the delimiter A in the string pointed by HL
    ; Once it finds it, the token is replace by \0.
    ; Parameters:
    ;       HL - Address of the string
    ;       A - Delimiter
    ; Returns:
    ;       DE - Address of the next string (address of the token found +1)
    ;       A - 0 if the delimiter was found, non-null value else
    ; Alters:
    ;       A, BC, DE
    PUBLIC strsep
strsep:
    push hl
    ld d, a
_strsep_loop:
    ld a, (hl)
    cp d
    jr z, _strsep_found
    ; Next character
    inc hl
    or a
    jp nz, _strsep_loop
    ; End of the string, A is 0, increment it and restore HL
    inc a
    pop hl
    ret
_strsep_found:
    xor a
    ld (hl), a
    inc hl  ; token + 1
    ex de, hl
    ; Restore original HL
    pop hl
    ret


    ; Calculate the length of a NULL-terminated string
    ; Parameters:
    ;       HL - Address of the string
    ; Returns:
    ;       BC - Size of the string
    ; Alters:
    ;       A
    PUBLIC strlen
strlen:
    push hl
    xor a
    ld b, a
    ld c, a
_strlen_loop:
    cp (hl)
    jr z, _strlen_end
    inc hl
    inc bc
    jr _strlen_loop
_strlen_end:
    pop hl
    ret

    ; Copy a NULL-terminated string into a given address, including the terminating NULL-byte.
    ; Parameters:
    ;       HL - Source string address
    ;       DE - Destination address
    ; Alters
    ;       A
    PUBLIC strcpy
strcpy:
    push hl
    push bc
    push de
    ld bc, 0xffff
_strcpy_loop:
    ld a, (hl)
    ; Copy byte into de, even if it's null-byte
    ldi
    ; Test null-byte here
    or a
    jp nz, _strcpy_loop
    pop de
    pop bc
    pop hl
    ret


    ; Same as strcpy but if the source address is smaller than the given size,
    ; the destination buffer will be filled with NULL (\0) byte.
    ; Parameters:
    ;       HL - Source string address
    ;       DE - Destination string address
    ;       BC - Maximum number of bytes to write
    ; Returns:
    ;       DE - Destination string address + maximum bytes
    ; Alters:
    ;       A, BC, HL, DE
    PUBLIC strncpy_unsaved
strncpy_unsaved:
    ; Make sure that BC is not 0, else, nothing to copy
    ld a, b
    or c
    ret z
    ; Size is not 0, we can proceed
_strncpy_loop:
    ; Read the src byte, to check null-byte
    ld a, (hl)
    ; We cannot use ldir here as we need to check the null-byte in src
    ldi
    or a
    jp z, _strncpy_zero
    ld a, b
    or c
    jp nz, _strncpy_loop
    ret
_strncpy_zero:
    ; Here too, we have to test whether BC is 0 or not
    ld a, b
    or c
    ret z
    ; 0 has just been copied to dst (DE), we can reuse this null byte to fill
    ; the end of the buffer using LDIR
    ld h, d
    ld l, e
    ; Make HL point to the null-byte we just copied
    dec hl
    ; Perform the copy
    ldir
    ret


    ; Convert all characters of the given string to lowercase
    ; Parameters:
    ;       HL - Address of the string to convert
    ; Alters:
    ;       A
    PUBLIC strtolower
strtolower:
    push hl
_strtolower_loop:
    ld a, (hl)
    or a
    jr z, _strtolower_end
    call to_lower
    ld (hl), a
    inc hl
    jr nz, _strtolower_loop
_strtolower_end:
    pop hl
    ret


    ; Parse string into a 16-bit integer. Hexadecimal string can start with
    ; 0x or $, decimal number start with any valid digit
    ; Parameters:
    ;       HL - NULL-terminated string to parse
    ; Returns:
    ;       HL - Parsed value
    ;       A - 0 if the string was parsed successfully
    ;           1 if the string represents a value bigger than 16-bit
    ;           2 if the string presents non-digit character(s)
    ; Alters:
    ;       A, HL
    PUBLIC parse_int
parse_int:
    ld a, (hl)
    cp '$'
    jr z, _parse_hex_prefix
    cp '0'
    jr nz, parse_dec
    inc hl
    ld a, (hl)
    cp 'x'
    jr z, _parse_hex_prefix
    dec hl
    jr parse_dec

    PUBLIC parse_hex
_parse_hex_prefix:
    inc hl  ; Go past prefix ($, 0x)
parse_hex:
    push de
    ex de, hl
    ld h, 0
    ld l, 0
    ld a, (de)
    or a
    jp z, _parse_hex_incorrect
_parse_hex_loop:
    call parse_hex_digit
    jr c, _parse_hex_incorrect
    ; Left shift HL 4 times
    add hl, hl
    jp c, _parse_hex_too_big
    add hl, hl
    jp c, _parse_hex_too_big
    add hl, hl
    jp c, _parse_hex_too_big
    add hl, hl
    jp c, _parse_hex_too_big
    or l
    ld l, a
    ; Go to next character and check whether it is the end of the string or not
    inc de
    ld a, (de)
    or a
    jp z, _parse_hex_end
    jp _parse_hex_loop
_parse_hex_too_big:
    ld a, 1
    pop de
    ret
_parse_hex_incorrect:
    ld a, 2
_parse_hex_end:
    pop de
    ret

    PUBLIC parse_dec
parse_dec:
    push de ; DE wil contain the string to parse
    push bc ; BC will be a temporary register, for multiplying HL by 10
    ex de, hl
    ld h, 0
    ld l, 0
    ld a, (de)
    or a
    jp z, _parse_dec_incorrect
_parse_dec_loop:
    call parse_dec_digit
    jr c, _parse_dec_incorrect
    ; Multiple HL by 10!
    add hl, hl  ; HL = HL * 2
    jr c, _parse_dec_too_big
    push hl     ; HL * 2 pushed on the stack
    add hl, hl  ; HL = HL * 4
    jr c, _parse_dec_too_big_pushed
    add hl, hl  ; HL = HL * 8
    jr c, _parse_dec_too_big_pushed
    pop bc      ; BC contains HL * 2
    add hl, bc  ; HL = 2 * HL + 8 * HL = 10 * HL
    jr c, _parse_dec_too_big
    ld b, 0
    ld c, a
    ; Add the new digit to the result
    add hl, bc
    jr c, _parse_dec_too_big
    ; Go to next character and check whether it is the end of the string or not
    inc de
    ld a, (de)
    or a
    jp z, _parse_dec_end
    jp _parse_dec_loop
_parse_dec_too_big_pushed:
    ; We have to pop the saved 2*HL
    pop bc
_parse_dec_too_big:
    ld a, 1
    ; Pop back BC real value
    pop bc
    pop de
    ret
_parse_dec_incorrect:
    ld a, 2
_parse_dec_end:
    pop bc
    pop de
    ret

parse_dec_digit:
    cp '0'
    jp c, _parse_not_dec_digit
    cp '9' + 1
    jp nc, _parse_not_dec_digit
    ; A is between '0' and '9'
    sub '0' ; CY will be reset
    ret
_parse_not_dec_digit:
    scf
    ret

parse_hex_digit:
    cp '0'
    jp c, _parse_not_hex_digit
    cp '9' + 1
    jp c, _parse_hex_dec_digit
    cp 'A'
    jp c, _parse_not_hex_digit
    cp 'F' + 1
    jp c, _parse_upper_hex_digit
    cp 'a'
    jp c, _parse_not_hex_digit
    cp 'f' + 1
    jp nc, _parse_not_hex_digit
_parse_lower_hex_digit:
    ; A is a character between 'a' and 'f'
    sub 'a' - 10 ; CY will be reset
    ret
_parse_upper_hex_digit:
    ; A is a character between 'A' and 'F'
    sub 'A' - 10 ; CY will be reset
    ret
_parse_hex_dec_digit:
    ; A is a character between '0' and '9'
    sub '0' ; CY will be reset
    ret
_parse_not_hex_digit:
    scf
    ret


    ; Parse the parameter for as a string which is surrounded with ""
    ; Parameters:
    ;   HL - Parameter to parse as a string
    ; Returns:
    ;   HL - Modified string (because of escaped characters, and "" removed)
    ;   BC - Length of the final string
    ;   A - 0 on success, positive value else
    PUBLIC parse_str
parse_str:
    ld a, (hl)
    cp '"'
    jr nz, parse_str_error
    ; Save HL on the stack, DE will contain the pointer on the string that we will
    push hl
    ld d, h
    ld e, l
    ld bc, 0
    inc hl
_parse_str_loop:
    ld a, (hl)
    cp '"'
    jr z, _parse_str_end
    cp 0x5c ; Backslash character
    jr z, _parse_str_esc
    ; Character is valid, increment BC and continue the loop
_parse_str_copy_a:
    ld (de), a
    inc hl
    inc de
    inc bc
    jp _parse_str_loop
_parse_str_end:
    ; We reached the end with '"', make sure it followed by a 0!
    inc hl
    ld a, (hl)
    ld (de), a  ; If A is 0, it will be stored at the end
    pop hl
    ret
_parse_str_esc:
    ; Check the character following the '\'
    inc hl
    ld a, (hl)
    cp 0x5c
    jr z, _parse_str_copy_a
    cp '"'
    jr z, _parse_str_copy_a
    push bc
    cp '0'
    ld b, 0
    jr z, _parse_str_copy_b
    cp 'a'
    ld b, 7
    jr z, _parse_str_copy_b
    inc b
    cp 'b'
    jr z, _parse_str_copy_b
    inc b
    cp 't'
    jr z, _parse_str_copy_b
    inc b
    cp 'n'
    jr z, _parse_str_copy_b
    inc b
    cp 'v'
    jr z, _parse_str_copy_b
    inc b
    cp 'f'
    jr z, _parse_str_copy_b
    inc b
    cp 'r'
    jr z, _parse_str_copy_b
    pop bc
    ; Not an escape character, copy \ back and current char
    ld a, 0x5c
    ld (de), a
    inc de
    ld a, (hl)
    ld (de), a
    inc de
    inc hl
    inc bc
    inc bc
    jp _parse_str_loop
_parse_str_copy_b:
    ; B contains the character to copy to the destination buffer, the stack contains
    ; the original value of BC
    ld a, b
    pop bc
    jr _parse_str_copy_a
parse_str_error:
    ld a, 1
    ret


    ; Check if character in A is a lower case letter [a-z]
    ; Parameters:
    ;   A - ASCII character
    ; Returns:
    ;   carry flag - Not a lower char
    ;   not carry flag - Is a lower char
is_lower:
    cp 'a'
    ret c
    cp 'z' + 1         ; +1 because p flag is set when result is 0
    ccf
    ret

    ; Check if character in A is an upper case letter [A-Z]
    ; Parameters:
    ;   A - ASCII character
    ; Returns:
    ;   carry flag - Not an upper char
    ;   not carry flag - Is an upper char
is_upper:
    cp 'A'
    ret c   ; Return if carry because we shouldn't have a carry here
    cp 'Z' + 1         ; +1 because p flag is set when result is 0
    ccf
    ret


    ; Check if character in A is a digit [0-9]
    ; Parameters:
    ;   A - ASCII character
    ; Returns:
    ;   carry flag - Not a digit
    ;   not carry flag - Is a digit
    PUBLIC is_digit
is_digit:
    cp '0'
    ret c
    cp '9' + 1         ; +1 because if A = '9', p flag would be set
    ccf
    ret


    ; Check if character in A is a letter [A-Za-z]
    ; Parameters:
    ;   A - ASCII character
    ; Returns:
    ;   carry flag - Not an alpha char
    ;   not carry flag - Is an alpha char
    PUBLIC is_alpha
is_alpha:
    call is_lower
    ret nc   ; Return on success
    jp is_upper


    ; Check if character in A is alpha numeric [A-Za-z0-9]
    ; Parameters:
    ;   A - ASCII character
    ; Returns:
    ;   carry flag - Not an alpha numeric
    ;   not carry flag - Is an alpha numeric
    PUBLIC is_alpha_numeric
is_alpha_numeric:
    call is_alpha
    ret nc  ; Return on success
    jr is_digit


    ; Subroutine converting a character to a lower case
    ; Parameters:
    ;   A - ASCII character
    ; Returns:
    ;   A - Lower case character on success, same character else
    PUBLIC to_lower
to_lower:
    cp 'A'
    jp c, _to_lower_not_char
    cp 'Z' + 1         ; +1 because p flag is set when result is 0
    jp nc, _to_lower_not_char
    add 'a' - 'A'
    ret
_to_lower_not_char:
    ret


    ; Convert a 16-bit value to decimal (ASCII) and store it in the
    ; buffer pointed by DE
    ; Parameters:
    ;   DE - Buffer to store the result in
    ;   HL - 16-bit value to convert
    ; Returns:
    ;   HL - Address of the last digit + 1
    PUBLIC word_to_ascii
word_to_ascii:
    ld c, 10    ; Constant, not modified by the code
    push de
    pop ix      ; Move DE in IX
    ld d, 0     ; Number of digits
word_to_ascii_loop:
    call divide_hl_c
    ; Remainder in A, convert to ASCII
    add '0'
    push af
    inc d
    ; Check if HL is 0
    ld a, h
    or l
    jp nz, word_to_ascii_loop
    ; We have D digits, iterate over all the digits
    ld b, d
    push ix
    pop hl
word_to_ascii_pop_loop:
    pop af
    ld (hl), a
    inc hl
    djnz word_to_ascii_pop_loop
    ret

    ; Divide a 16-bit value by an 8-bit value
    ; Parameters:
    ;   HL - Number of divide
    ;   C  - Divider
    ; Returns:
    ;   HL - Result
    ;   A  - Remainder
divide_hl_c:
   xor a
   ld b, 16
divide_hl_c_loop:
   add hl, hl
   rla
   jp c, divide_hl_carry
   cp c
   jp c, divide_hl_next
divide_hl_carry:
   sub c
   inc l
divide_hl_next:
   djnz divide_hl_c_loop
   ret
