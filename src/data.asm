; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>
;
; SPDX-License-Identifier: Apache-2.0

    INCLUDE "zos_err.asm"

    EXTERN strncpy_unsaved
    EXTERN strncmp_opt

    ; Make the assumption that the stack will be set to 0xFFFF
    DEFC PROGRAM_STACK_ADDRESS = 0xFFFF

    ; Allocate 1024 bytes for the stack, it should be enough
    DEFC DATA_ALLOCATED_STACK = 1024

    ; Bottom of the heap, if the head reaches this value, send no more memory error
    DEFC DATA_BOTTOM_HEAP_MSB = 0xC0
    DEFC DATA_BOTTOM_HEAP = DATA_BOTTOM_HEAP_MSB << 8

    ; Define the list entry format
    DEFVARS 0 {
        list_entry_content_t      DS.B 0
        list_entry_key_t          DS.B 16
        list_entry_value_t        DS.B 2  ; resulted binary should not exceed 64KB
        list_entry_content_end_t  DS.B 0
        list_entry_next_t         DS.B 2
        list_entry_end_t          DS.B 0
    }

    DEFC LIST_ENTRY_KEY_SIZE     =  list_entry_value_t - list_entry_key_t
    DEFC LIST_ENTRY_CONTENT_SIZE = list_entry_content_end_t - list_entry_content_t
    DEFC LIST_ENTRY_SIZE         = list_entry_end_t


    SECTION TEXT

;******************************************************************************
;*                            PUBLIC  ROUTINES                                *
;******************************************************************************

    ; Get the value associated to a key (string) in the hashmap
    ; Parameters:
    ;   HL - Key for the hashmap (NULL-Terminated string)
    ; Returns:
    ;   DE - Value associated to the key
    ;   A - ERR_SUCCESS if the entry was found
    ;       ERR_NO_SUCH_ENTRY if the entry was not found
    ; Alters;
    ;   A, BC, DE, HL
    PUBLIC data_get
data_get:
    ; Put the key (string) in DE as it will be preserved
    ld d, h
    ld e, l
    call data_hashmap_get_list
    ld a, h
    or l
    jr z, _data_get_not_found
    ; Search for the index where the key (string) should be placed in the list
    call data_list_search
    ; If A is 0, the string was found
    or a
    jr nz, _data_get_not_found
    ; HL contains the entry structure, retrieve the value
    ld de, list_entry_value_t
    add hl, de
    ; Dereference the 16-bit value
    ld e, (hl)
    inc hl
    ld d, (hl)
    xor a
    ret
_data_get_not_found:
    ld a, ERR_NO_SUCH_ENTRY
    ret


    ; Insert a string in the hashmap if it doesn't already exist!
    ; Parameters:
    ;   HL - String to add to the hashmap
    ;   DE - Value to store with the string
    ; Returns:
    ;   A - ERR_SUCCESS - the data have been inserted successfully
    ;       ERR_NO_MORE_MEMORY - no more heap memory
    ;       ERR_ALREADY_EXIST - the string was already inserted
    ; Alters:
    ;   A, BC, DE, HL
    PUBLIC data_insert
data_insert:
    push hl
    call data_hashmap_get_list
    ; Check if the list is empty
    ld a, h
    or l
    jr z, _data_store_str_first
    ; Allocate a new entry. Retrieve the string from the stack and save the
    ; returned list address.
    ex (sp), hl
    call data_list_allocate_entry
    ret nz
    ; Put the list address in HL and the new entry in DE.
    ; BC contains the a pointer to the first entry, in case the new entry
    ; has to be insert at the beginning of the list.
    pop hl
    call data_list_search
    or a
    ; If A is 0, the entry was found, this is an error
    jr z, _data_insert_already_exist
    ; Entry was not found, we can insert it by putting its address (DE) in the
    ; previous nodes' next field (BC).
    ld a, e
    ld (bc), a
    inc bc
    ld a, d
    ld (bc), a
    ; Make our entry point to its "next" field
    ex de, hl
    ld bc, list_entry_next_t
    add hl, bc
    ; Store the new next node in our next field
    ld (hl), e
    inc hl
    ld (hl), d
    ; Success!
    xor a
    ret
_data_store_str_first:
    ; Retrieve the string from the stack
    pop hl
    ; Allocate a new node and store it as the first list entry
    call data_list_allocate_entry
    ; Save the newly created entry (DE)
    ld h, b
    ld l, c
    ld (hl), e
    inc hl
    ld (hl), d
    ; Return success
    xor a
    ret
_data_insert_already_exist:
    ld a, ERR_ALREADY_EXIST
    ret


    ; Prepend a list entry to a list. In other words, add an entry to the
    ; beginning of the list.
    ; Parameters:
    ;   HL - List entry to prepend
    ;   DE - List address (first list entry)
    ; Returns:
    ;   HL - New list address (original HL)
    ; Alters:
    ;   A
    PUBLIC data_list_prepend_entry
data_list_prepend_entry:
    push hl
    ld a, list_entry_next_t
    ; HL += A
    add l
    ld l, a
    adc h
    sub l
    ld h, a
    ld (hl), e
    inc hl
    ld (hl), d
    pop hl
    ret


    ; Get the value of the list entry.
    ; Parameters:
    ;   HL - List entry
    ; Returns:
    ;   HL - Value of the entry
    ; Alters:
    ;   A, BC, HL
    PUBLIC data_list_get_value
data_list_get_value:
    ld bc, list_entry_value_t
    add hl, bc
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    ret


    ; Get the next node of the given entry.
    ; Parameters:
    ;   HL - List entry
    ; Returns:
    ;   HL - Next node entry
    ; Alters:
    ;   HL, BC
    PUBLIC data_list_get_next
data_list_get_next:
    ld bc, list_entry_next_t
    add hl, bc
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    ret


;******************************************************************************
;*                            PRIVATE ROUTINES                                *
;******************************************************************************

    ; Search the place of new entry in the given sorted list.
    ; Parameters:
    ;   HL - Address of the list (MUST NOT BE NULL)
    ;   DE - Address of the entry to search (address of the string)
    ;   BC - Pointer to the first entry (in case the element must be inserted at the beginning)
    ; Returns:
    ;   A - ERR_SUCCESS if the entry was found
    ;       ERR_NO_SUCH_ENTRY if the entry was not found
    ;   HL - Address of the last entry tested. If the entry was not found, this pointer represents
    ;        the next node of the new entry.
    ;   BC - Address of the previous node's "next" address
    ; Alters:
    ;   A, BC, HL
data_list_search:
    ; HL points to the first entry's key (string), we can compared it with DE.
_data_list_search_loop:
    ld a, LIST_ENTRY_KEY_SIZE
    ; Routine strncmp_opt doesn't alter BC
    call strncmp_opt
    ; If A is 0, both strings are equal
    or a
    ret z
    ; If A is negative, then DE is smaller than HL, so we found its place!
    jp m, _data_list_search_place_here
    ; Else, HL is smaller, we have to go to the next node, HL += list_entry_next_t
    ; We can re-use BC since we will overwrite it right after
    ld bc, list_entry_next_t
    add hl, bc
    ; Store HL (next node address) in BC
    ld b, h
    ld c, l
    ; Dereference HL to make it point to the entry
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    ; If HL is not 0, we can continue going through the list
    or h
    jp nz, _data_list_search_loop
    ; Fall-through
    ; We couldn't find the entry but we've found where to place it
_data_list_search_place_here:
    ; HL already points to the entry that would be the "next" entry of the one
    ; pointed by DE.
    ; BC is also pointing to the previous entry's "next node", so we can return
    ; safely.
    ld a, ERR_NO_SUCH_ENTRY
    ret


    ; Allocate a new list entry from the given data.
    ; Parameters:
    ;   HL - String to store in the new entry
    ;   DE - 16-bit data to store in the new entry
    ; Returns:
    ;   A - 0 on success, positive value in case of error
    ;   Z flag - set if A is 0
    ;   DE - Address of the new entry
    ; Alters:
    ;   A, DE
    PUBLIC data_list_new_entry
data_list_new_entry:
data_list_allocate_entry:
    push hl
    ld hl, (data_heap_head)
    ; Subtract LIST_ENTRY_SIZE from HL
    ld a, -LIST_ENTRY_SIZE
    dec h   ; A is negative
    add l
    ld l, a
    adc h
    sub l
    ld h, a
    ; HL is the newly allocated pointer, check if we reached the end of the heap
    ; It is faster to directly check the value of H (which is also in A).
    ; A must be bigger or equal to DATA_BOTTOM_HEAP_MSB, so A - DATA_BOTTOM_HEAP_MSB >= 0
    cp DATA_BOTTOM_HEAP_MSB
    jr c, _data_list_allocate_entry_no_mem
    ; HL is valid, we can populate the structure and return a positive value
    ; Save the new heap head
    ld (data_heap_head), hl
    ; Get the string from the stack and save current DE value
    ex de, hl
    ex (sp), hl
    ; DE contains the newly allocated pointer, HL contains the string to copy
    push de ; Save the new pointer on the stack
    push bc
    ld bc, LIST_ENTRY_KEY_SIZE
    call strncpy_unsaved
    pop bc
    ; DE points to the end of the string, so new_pointer + LIST_ENTRY_KEY_SIZE
    ; We can store the value, it is in SP + 2, so we need to pop and exchange
    ; the stack values
    pop hl
    ex (sp), hl
    ; HL contains the value, DE contains the value address
    ex de, hl
    ld (hl), e
    inc hl
    ld (hl), d
    ; Success, we can return the newly allocated pointer
    pop de
    xor a
    ret
_data_list_allocate_entry_no_mem:
    pop hl
    ld a, ERR_NO_MORE_MEMORY
    or a
    ret


    ; Get the list which index is the hash value of the given string.
    ; Parameters:
    ;   HL - NULL-terminated string
    ; Returns:
    ;   HL - Address of first entry
    ;   BC - Address of the pointer to the first item list. Useful if the entry is 0.
    ; Alters:
    ;   A, HL, BC
data_hashmap_get_list:
    ; Get the hash value for the string and retrieve its list
    call data_hash_str
    ; Hash in B, A is 0 for sure here
    ld h, a
    ld l, b
    ; List = Table[hash * 2]
    add hl, hl
    ld bc, data_hashmap
    add hl, bc
    ; Dereference HL into HL, save it in BC before as we need to return it
    ld c, l
    ld b, h
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    ret


    ; Calculate the 8-bit hash value of a given NULL-terminated string
    ; Parameters:
    ;   HL - Address of the string
    ; Returns:
    ;   A - 0
    ;   B - Hash value of the string
    ; Alters:
    ;   A, BC, HL
data_hash_str:
    ; Result will be stored in B
    ; C contains the mask to use with A at each iteration
    ld bc, 0x0580
_data_hash_str_loop:
    ld a, (hl)
    or a
    ret z
    ; Calculate hash = (hash << 7) + hash + char
    ld a, b
    rrca
    and c
    add b
    add (hl)
    inc hl
    ld b, a
    jp _data_hash_str_loop


    SECTION DATA
    ; The hashmap is composed of 256 2-byte entries, these entries are lists, in C it would give us:
    ; struct hashmap_t {
    ;   list_entry_t* entries[256];
    ; }
    ; where each entry is as follow:
    ; struct list_entry_t {
    ;   uint8_t  content[LIST_ENTRY_CONTENT_SIZE];
    ;   uint16_t next;
    ; }
data_hashmap:
    DEFS 256 * 2

    ; The heap is implemented very simply with a pointer to the end of the memory that will decrease
data_heap_head:
    DEFW PROGRAM_STACK_ADDRESS + 1 - DATA_ALLOCATED_STACK