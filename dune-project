(lang dune 2.8)

(generate_opam_files true)

(package
 (name camelium)
 (depends
   core
   lwt
   lwt_ppx
   logs
   eio
   domainslib
   ppx_jane
   hacl-star
   secp256k1-internal
   base64
))
