ENTRY(_start)
SECTIONS {
    . = 0x82000000;
    PROVIDE(__executable_start = .);
    PROVIDE(__spark_start = .);
    .text               :
    {
        *(.text._start)
        *(.text .text.*)
    }
    PROVIDE(__etext = .);
    .rodata             : { *(.rodata .rodata.*) }
    PROVIDE(__symtab_start = .);
    .symtab             : { *(.symtab) }
    PROVIDE(__symtab_end = .);
    PROVIDE(__strtab_start = .);
    .strtab             : { *(.strtab) }
    PROVIDE(__strtab_end = .);
    PROVIDE(__eh_frame_hdr = .);
    .eh_frame_hdr       : { KEEP(*(.eh_frame_hdr)) }
    PROVIDE(__eh_frame_hdr_end = .);
    PROVIDE(__eh_frame = .);
    .eh_frame           : { KEEP(*(.eh_frame)) }
    .gcc_except_table   : { KEEP(*(.gcc_except_table .gcc_except_table.*)) }
    .data               : { *(.data .data.*) }
    .sdata              : { *(.sdata .sdata.*) }
    __global_pointer$ = .;
    PROVIDE(__bss_start = .);
    .sbss               : { *(.sbss .sbss.*) }
    .bss                :
    {
        *(.bss .bss.*)
        *(COMMON)
        . += 0x100000;
        . = ALIGN(128);
        PROVIDE(__boot_stackp = .);
    }
    PROVIDE(__bss_end = .);
    PROVIDE(__spark_end = .);
}
