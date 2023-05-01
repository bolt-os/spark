
SECTIONS {
    . = 0;
    PROVIDE(__image_base = .);
    PROVIDE(__spark_start = .);

    .text                   : {
        KEEP(*(.pe_headers))
        . = ALIGN(CONSTANT(MAXPAGESIZE));

        PROVIDE(__text = .);
        *(.text._start)
        *(.text .text.*)
    }
    .plt                    : { *(.plt .plt.*) }
    PROVIDE(__etext = .);
    . = ALIGN(CONSTANT(MAXPAGESIZE));
    PROVIDE(__text_size = .);

    PROVIDE(__rodata = .);
    .hash                   : { *(.hash) }
    .gnu.hash               : { *(.gnu.hash) }
    .dynsym                 : { *(.dynsym) }
    .dynstr                 : { *(.dynstr) }
    .rela                   : { *(.rela*) }
    .rodata                 : { *(.rodata .rodata.*) }
    .note.gnu.build-id      : { KEEP(*(.note.gnu.build-id)) }
    .eh_frame_hdr           : { KEEP(*(.eh_frame_hdr)) }
    PROVIDE(__eh_frame = .);
    .eh_frame               : { KEEP(*(.eh_frame)) }
    . = ALIGN(CONSTANT(MAXPAGESIZE));
    PROVIDE(__rodata_size = . - __rodata);

    PROVIDE(__sbat = .);
    .data.sbat              : { KEEP(*(.sbat)) }
    PROVIDE(__sbat_sizev = 1);
    . = ALIGN(CONSTANT(MAXPAGESIZE));
    PROVIDE(__sbat_size = . - __sbat);

    PROVIDE(__reloc = .);
    .data.reloc             : { KEEP(*(.data.reloc)) }
    . = ALIGN(CONSTANT(MAXPAGESIZE));
    PROVIDE(__reloc_size = . - __reloc);

    PROVIDE(__data = .);
    .tdata                  : { *(.tdata .tdata.*) }
    .tbss                   : { *(.tbss .tbss.*) }
    .data.rel.ro            : { *(.data.rel.ro .data.rel.ro.*) }
    .dynamic                : { *(.dynamic) }
    . = DATA_SEGMENT_RELRO_END(0, .);
    .got                    : { *(.got .got.*) } 
    .got.plt                : { *(.got.plt .got.plt.*) } 
    .data                   : { *(.data .data.*) *(.sdata .sdata.*) } 
    PROVIDE(__data_size_init = . - __data);
    PROVIDE(__global_pointer$ = .);
    PROVIDE(__bss = .);
    .bss                    :
    {
        *(.sbss .sbss.*)
        *(.bss .bss.*)
        *(.dynbss)
        . += 0x100000;
        . = ALIGN(CONSTANT(MAXPAGESIZE));
        PROVIDE(__boot_stackp = .);
    }
    PROVIDE(__ebss = .);
    PROVIDE(__data_size = . - __data);
    PROVIDE(__image_size = . - __image_base);
    PROVIDE(__spark_end = .);
}
