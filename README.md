<h2 align="center">Zealasm Z80 assembler</h2>
<p align="center">
    <a href="https://opensource.org/licenses/Apache-2.0">
        <img src="https://img.shields.io/badge/License-Apache_2.0-blue.svg" alt="Licence" />
    </a>
    <p align="center">Presenting <b>zealasm</b>: a Z80 assembler entirely written in Z80 assembly, for Zeal 8-bit OS</p>
</p>


## About zealasm

The goal of zealasm is to provide a way to assemble Z80 code directly in [Zeal 8-bit OS](https://github.com/Zeal8bit/Zeal-8-bit-OS), without the need of another computer.

## Features and limitations

zealasm is still in alpha, here are the features implemented:

* Less than **6KB** of code once compiled!
* Use Zeal 8-bit OS API for file read and write, so completely portable
* Support `ORG` to relocate final binary
* Support a final binary up to 40KB
* Support labels up to **16 characters**, forward usage before declaration supported
* Support all official instructions from [Z80 User Manual](https://www.zilog.com/docs/z80/um0080.pdf)
* Support directives: `dm`, `ds`, `db`, `dw`
* Support comments starting with `;`
* Support upper or lower case instructions and operands

Regarding the current limitations:

* Doesn't support memory paging, so the maximum output file size is limited by the amount of RAM mapped at once: around 40KB
* No support for macros/defines
* No support for constant calculation (`label1` - `label2`)

## Building the project

### Requirements

At the moment, the project has only been assembled on Linux (Ubuntu 20.04 and 22.04), it should be compatible with Mac OS and Windows as long as you have:

* bash
* git (to clone this repo)
* make
* Zeal 8-bit OS source code. Only the `kernel_headers` directory is required
* z88dk v2.2 (or later). Only its assembler, `z80asm`, is strictly required. The latest version of `z80asm` must be used as earlier versions don't have support for `MACRO`.

To install z88dk, please [check out their Github project](https://github.com/z88dk/z88dk).

### Building

To build the program, define the path to Zeal 8-bit OS, this will let us find the header files used to assemble `zealasm`:
```
export ZOS_PATH=/your/path/to/Zeal8bitOS
```
Then simply use the command:
```
make
```

After compiling, the folder `bin/` should contain the binary `zealasm.bin`. This file can be then loaded to Zeal 8-bit OS through UART thanks to the `load` command.

The binary can also be embedded within the `romdisk` that will contain both the OS and a read-only file system. For example:
```
cd $ZOS_PATH
export EXTRA_ROMDISK_FILES="/path/to/zealasm/bin/zealasm.bin"
make
```

More info about compiling Zeal 8-bit OS [here](https://github.com/Zeal8bit/Zeal-8-bit-OS#getting-started).

The resulted ROM image can then be provided to an emulator or directly flashed to the computer's ROM that will use it.

## Usage

To assemble source file `source.asm` into `binary.bin`, use:
```
zealasm.bin source.asm binary.bin
```

If you are using Zeal 8-bit OS default romdisk, you can use `exec` command to execute `zealasm.bin` or directly use `./`, followed by the source/destination file:
```
A:/>exec zealasm.bin source_file.asm B:/final_binary.bin
```
or
```
A:/>./zealasm.bin source_file.asm B:/final_binary.bin
```

Make sure you specify a destination disk that is **write-read**! (`A:/` disk isn't by default)

## Directives

zealasm supports several directives that can be used in the source file to assemble:
* `org` to set the origin address of the assembled binary. Make sure you use this directive **before** any instruction. This directive can only be used once per file. For example:
```
    org 0x4000

_start:
    ; HL will contain the value 0x4000
    ld hl, _start
    ; Rest of the code
label:
```
* `.dm` to define a string in the final binary. Escape characters **are** supported. For example
```
    ld hl, my_str
    ; Rest of the code
my_str: .dm "Hello world, this is a message\n"
```
* `.db` to define a single byte in the final binary. The value must not be greater than `0xFF`. A hexadecimal value can start with `0x` or `$`, else it will be considered a decimal value. For example:
```
    .db 0x4f
    .db $8f
    .db 19
```
* `.dw` to define a 16-bit word in the final binary. It will be written as a little-endian value. The value must not be greater than `0xFFFF`. A hexadecimal value can start with `0x` or `$`, else it will be considered a decimal value. For example:
```
    .dw 0x4f2a
    .dw $f1fe
    .dw 1337
```
* `.ds` to define a (zero-initialized) space in the final binary. The parameter is the size of the space, which can also be a decimal or a hexadecimal value. For example:
```
    .ds 10   ; defines a space of 10 bytes
    .db 0x20 ; defines a space of 32 bytes
    .db $20  ; defines a space of 32 bytes
```

## 8-bit arithmetic and operands

Despite the fact that the following instructions are described (in the official [Z80 User Manual](https://www.zilog.com/docs/z80/um0080.pdf)) as taking A as the first operand, `zealasm` will also accept that the second operand is given:
* `add`. For example: both `add a, b` and `add b` will be accepted
* `adc`. For example: both `adc a, b` and `adc b` will be accepted
* `sbc`. For example: both `sbc a, b` and `sbc b` will be accepted

The following instructions will **only** accept a **single** operand:
* `sub`. For example, `sub b` is valid, but `sub a,b` isn't
* `and`
* `or`
* `xor`
* `cp`

## 16-bit arithmetic and operand

As the `add`, `adc` and `sbc` instructions will refer to `A` if only a single operand is given, `HL` must be specified to refer to a 16-bit operation.
For example, `add hl, bc` is a valid instruction, but `add bc` is **not**.

## Contact

For any suggestion or request, you can contact me at contact [at] zeal8bit [dot] com

Or, you can join Zeal 8-bit projects [Discord server](https://discord.gg/UzEjwRvBBb).

For features requests, you can also open an issue or a pull request.

## Contributing

Contributions are welcome! Feel free to fix any bug that you may see or encounter, or implement any feature that you find important.

To contribute:
  * Fork the Project
  * Create your feature Branch (*optional*)
  * Commit your changes. Please make a clear and concise commit message (*)
  * Push to the branch
  * Open a Pull Request


(*) A good commit message is as follow:
```
Module: add/fix/remove a from b

Explanation on what/how/why
```
For example:
```
Flash: add the possibility to flash programs bigger than 48KB
```

## License

Distributed under the Apache 2.0 License. See `LICENSE` file for more information.

You are free to use it for personal and commercial use, the boilerplate present in each file must not be removed.
