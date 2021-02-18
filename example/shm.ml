let rand = Random.State.make_self_init ()

let with_memory_fd ~size f =
  let rec aux = function
    | 0 -> failwith "Failed to find an unused temporary file name!"
    | i ->
      let rnd = Random.State.bits rand land 0xFFFFFF in
      let path = Printf.sprintf "/dev/shm/wl_shm-%06x" rnd in
      match Unix.openfile path Unix.[O_CREAT; O_RDWR; O_CLOEXEC; O_EXCL] 0o600 with
      | exception Unix.Unix_error(Unix.EEXIST, _, _) -> aux (i - 1)
      | fd ->
        Fun.protect ~finally:(fun () -> Unix.close fd)
          (fun () ->
             Unix.unlink path;
             Unix.ftruncate fd size;
             f fd
          )
  in
  aux 100
