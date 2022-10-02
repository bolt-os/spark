
SECTIONS {
    . = 0;
    PROVIDE(__executable_start = .);
    PROVIDE(__spark_start = .);
    PROVIDE(__image_base = .);

    PROVIDE(__text = .);
    .text : {
        *(.text._start)
        *(.text .text.*)
    }
    PROVIDE(__etext = .);

    .rodata                 : {
        *(.rodata .rodata.*)
        *(.data.rel.ro*)
    }
    .eh_frame_hdr           : { KEEP(*(.eh_frame_hdr)) }
    PROVIDE(__eh_frame = .);
    .eh_frame               : { KEEP(*(.eh_frame)) }
    .got                    : { *(.got) }
    .dynamic                : { *(.dynamic) }
    .dynsym                 : { *(.dynsym) }
    .dynstr                 : { *(.dynstr) }
    .rela                   : { *(.rela*) }
    .data                   : {
        *(.data .data.*)
        PROVIDE(__device_drivers = .);
        KEEP(*(.device_drivers))
        PROVIDE(__end_device_drivers = .);
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
    PROVIDE(__spark_end = .);
}
