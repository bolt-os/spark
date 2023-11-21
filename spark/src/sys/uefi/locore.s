// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

.pushsection .text

.section .pe_headers

mz_header:
        .ascii  "MZ"                            // MZ magic
        .4byte  0x7ff0006f                      // j 0x1000
        .zero   54
        .4byte  pe_header                       // offset to PE header

.p2align 3
pe_header:
        .ascii  "PE\0\0"                        // PE magic
        .2byte  0x5064                          // machine (IMAGE_FILE_MACHINE_RISCV64)
        .2byte  section_count                   // num sections
        .4byte  0                               // creation timestamp
        .4byte  0                               // symbol table offset
        .4byte  0                               // num symbols
        .2byte  section_table - opt_header      // optional header size
        .2byte  0x0226                          // characteristics
                                                //   IMAGE_FILE_EXECUTABLE_IMAGE
                                                //   IMAGE_FILE_LINE_NUMS_STRIPPED
                                                //   IMAGE_FILE_LARGE_ADDRESS_AWARE
                                                //   IMAGE_FILE_DEBUG_STRIPPED

opt_header:
        .2byte  0x020b                          // magic number (PE32+)
        .byte   0x02, 0x26                      // maj,min linker version
        .4byte  __text_size                     // size of code
        .4byte  __data_size_init                // size of initialized data section
        .4byte  0                               // size of uninitialized data section
        .4byte  _start                          // entry point (relative to __image_base)
        .4byte  __text                          // code base (relative to __image_base)
        .8byte  __image_base                    // image base
        .4byte  0x1000                          // section alignment
        .4byte  0x1000                          // file alignment
        .2byte  0, 0                            // maj,min os version
        .2byte  0, 0                            // maj,min image version
        .2byte  0, 0                            // maj,min subsys version
        .4byte  0                               // win32 version (must be 0)
        .4byte  __image_size                    // size of image
        .4byte  __text                          // size of headers (multiple of file alignment)
        .4byte  0                               // checksum
        .2byte  10                              // subsystem (IMAGE_SUBSYSTEM_EFI_APPLICATION)
        .2byte  0x8160                          // dll characteristics
                                                //   IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA
                                                //   IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE
                                                //   IMAGE_DLLCHARACTERISTICS_NX_COMPAT
                                                //   IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE
        .8byte  0                               // size of stack reserve
        .8byte  0                               // size of stack commit
        .8byte  0                               // size of heap reserve
        .8byte  0                               // size of heap commit
        .4byte  0                               // loader flags
        .4byte  data_dirs_count                 // num data dir entries

data_dirs:
        .4byte  0, 0                            // export table
        .4byte  0, 0                            // import table
        .4byte  0, 0                            // resource table
        .4byte  0, 0                            // exception table
        .4byte  0, 0                            // certificate table
        .4byte  reloc, 12                       // base relocation table
.set data_dirs_count, (. - data_dirs) / 8

section_table:
        .ascii  ".text\0\0\0"                   // name
        .4byte  __text_size                     // memory size
        .4byte  __text                          // virt address
        .4byte  __text_size                     // file size
        .4byte  __text                          // file offset
        .4byte  0                               // relocations
        .4byte  0                               // line numbers
        .2byte  0                               // num relocations
        .2byte  0                               // num line numbers
        .4byte  0x60000020                      // characteristics
                                                //   IMAGE_SCN_CNT_CODE
                                                //   IMAGE_SCN_MEM_EXECUTE
                                                //   IMAGE_SCN_MEM_READ

        .ascii  ".rdata\0\0"
        .4byte  __rodata_size
        .4byte  __rodata
        .4byte  __rodata_size
        .4byte  __rodata
        .4byte  0
        .4byte  0
        .2byte  0
        .2byte  0
        .4byte  0x40000040                      // characteristics
                                                //   IMAGE_SCN_CNT_INITIALIZED_DATA
                                                //   IMAGE_SCN_MEM_READ

        .ascii  ".reloc\0\0"
        .4byte  12
        .4byte  reloc
        .4byte  0x1000
        .4byte  reloc
        .4byte  0
        .4byte  0
        .2byte  0
        .2byte  0
        .4byte  0x40000040                      // characteristics
                                                //   IMAGE_SCN_CNT_INITIALIZED_DATA
                                                //   IMAGE_SCN_MEM_READ

        .ascii  ".sbat\0\0\0"
        .4byte  __sbat_sizev
        .4byte  __sbat
        .4byte  __sbat_size
        .4byte  __sbat
        .4byte  0
        .4byte  0
        .2byte  0
        .2byte  0
        .4byte  0x40000000                      // characteristics
                                                //      IMAGE_SCN_MEM_READ

        .ascii  ".data\0\0\0"
        .4byte  __data_size
        .4byte  __data
        .4byte  __data_size_init
        .4byte  __data
        .4byte  0
        .4byte  0
        .2byte  0
        .2byte  0
        .4byte  0xc0000040                      // characteristics
                                                //      IMAGE_SCN_CNT_INITIALIZED_DATA
                                                //      IMAGE_SCN_MEM_READ
                                                //      IMAGE_SCN_MEM_WRITE
.set section_count, (. - section_table) / 40

.section .data.reloc
reloc:
        .4byte          0x1000
        .4byte          12
        .2byte          0

.section .note.GNU-stack,"",@progbits

.section .text._start,"ax",@progbits
.global _start
.type _start,@function
_start:

.option push
.option norelax
        lla             gp, __global_pointer$
.option pop

        mv              t0, sp
        lla             sp, __boot_stackp

        addi            sp, sp, -(8 * 4)
        sd              s0, 0x00(sp)
        sd              s1, 0x08(sp)
        sd              s2, 0x10(sp)
        sd              s3, 0x18(sp)

        mv              s0, a0
        mv              s1, a1
        mv              s2, ra
        mv              s3, sp

        /*
         * We may or may not have been loaded to our linked address.
         */
        lla             a0, __image_base
        lla             a1, _DYNAMIC
        call            _relocate
        bnez            a0, error

        mv              a0, s0
        mv              a1, s1
        mv              fp, zero
        call            spark_main

error:
        mv              ra, s2
        mv              sp, s3
        ld              s0, 0x00(sp)
        ld              s1, 0x08(sp)
        ld              s2, 0x10(sp)
        ld              s3, 0x18(sp)
        addi            sp, sp, (8 * 4)

        ret
.size _start, . - _start

.popsection
