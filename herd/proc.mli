(** Information about a process. 

    Also known as a thread (in CPP11) or a work-item (in OpenCL).
    In CPP11 and CPU memory models, the process is just
    an integer. In OpenCL and PTX, the process is additionally
    identified by a work-group, device, etc.
*)

type proc

val proc_compare : proc -> proc -> int
val proc_eq : proc -> proc -> bool
val pp_proc : proc -> string
val proc_to_int : proc -> int
val int_to_proc : int -> proc
