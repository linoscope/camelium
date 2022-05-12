(lang dune 2.8)

(generate_opam_files true)

(package
 (name camelium)
 (depends
   base
   lwt
   lwt_ppx
   logs
   eio
   domainslib
   ppx_jane
))
