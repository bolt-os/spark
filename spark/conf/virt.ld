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
    .eh_frame_hdr       : { KEEP(*(.eh_frame_hdr)) }
    PROVIDE(__eh_frame = .);
    .eh_frame           : { KEEP(*(.eh_frame)) }
    .gcc_except_table   : { KEEP(*(.gcc_except_table .gcc_except_table.*)) }

    PROVIDE(__tdata = .);
    .tdata              : { *(.tdata .tdata.*) }
    PROVIDE(__tbss = .);
    .tbss               : { *(.tbss .tbss.*) }
    PROVIDE(__etbss = .);

    .data               :
    {
        *(.data .data.*)
        PROVIDE(__device_drivers = .);
        KEEP(*(.device_drivers))
        PROVIDE(__end_device_drivers = .);
    }
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
