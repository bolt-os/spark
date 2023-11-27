
SECTIONS {
    . = 0;
    PROVIDE(__executable_start = .);
    PROVIDE(__image_base = .);

    PROVIDE(__text = .);
    .text : {
        *(.text._start)
        *(.text .text.*)
    }
    PROVIDE(__etext = .);

    .rodata                 : { *(.rodata .rodata.*) }
    .eh_frame_hdr           : { KEEP(*(.eh_frame_hdr)) }
    .eh_frame               : { PROVIDE(__eh_frame = .); KEEP(*(.eh_frame)) }
    .dynsym                 : { *(.dynsym) }
    .dynstr                 : { *(.dynstr) }
    .rela                   : { *(.rela*) }
    .dynamic                : { *(.dynamic) }
    .got                    : { *(.got) }
    .data.rel.ro            : { *(.data.rel.ro*) }
    .data                   : {
        *(.data .data.*)
        *(.sdata .sdata.*)
    }
    PROVIDE(__global_pointer$ = .);
    PROVIDE(__bss = .);
    .bss                    : {
        *(.sbss .sbss.*)
        *(.dynbss)
        *(.bss .bss.*)
        *(COMMON)
        . += 0x100000;
        . = ALIGN(128);
        PROVIDE(__boot_stackp = .);
    }
    PROVIDE(__ebss = .);
    PROVIDE(__image_size = . - __image_base);
}
