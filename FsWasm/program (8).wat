(module
 (table 0 anyfunc)
 (memory $0 1)
 (data (i32.const 16) "hello\00")
 (data (i32.const 24) "\10\00\00\00")
 (export "memory" (memory $0))
 (export "append" (func $append))
 (export "main" (func $main))
 (func $append (; 0 ;) (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (block $label$0
   (br_if $label$0
    (i32.eqz
     (tee_local $2
      (i32.load8_u
       (get_local $1)
      )
     )
    )
   )
   (set_local $1
    (i32.add
     (get_local $1)
     (i32.const 1)
    )
   )
   (loop $label$1
    (i32.store8
     (get_local $0)
     (get_local $2)
    )
    (set_local $0
     (i32.add
      (get_local $0)
      (i32.const 1)
     )
    )
    (set_local $2
     (i32.load8_u
      (get_local $1)
     )
    )
    (set_local $1
     (i32.add
      (get_local $1)
      (i32.const 1)
     )
    )
    (br_if $label$1
     (get_local $2)
    )
   )
  )
  (get_local $0)
 )
 (func $main (; 1 ;) (result i32)
  (local $0 i32)
  (local $1 i32)
  (local $2 i32)
  (local $3 i32)
  (local $4 i32)
  (block $label$0
   (br_if $label$0
    (i32.eqz
     (tee_local $3
      (i32.load8_u
       (tee_local $4
        (i32.load offset=24
         (i32.const 0)
        )
       )
      )
     )
    )
   )
   (set_local $2
    (i32.add
     (get_local $4)
     (i32.const 1)
    )
   )
   (set_local $4
    (i32.const 32)
   )
   (loop $label$1
    (i32.store8
     (get_local $4)
     (get_local $3)
    )
    (set_local $4
     (i32.add
      (get_local $4)
      (i32.const 1)
     )
    )
    (set_local $3
     (i32.load8_u
      (get_local $2)
     )
    )
    (set_local $2
     (i32.add
      (get_local $2)
      (i32.const 1)
     )
    )
    (br_if $label$1
     (get_local $3)
    )
   )
   (set_local $3
    (i32.const 0)
   )
   (br_if $label$0
    (i32.eqz
     (tee_local $2
      (i32.load8_u
       (tee_local $0
        (i32.load offset=24
         (i32.const 0)
        )
       )
      )
     )
    )
   )
   (set_local $0
    (i32.add
     (get_local $0)
     (i32.const 1)
    )
   )
   (loop $label$2
    (i32.store8
     (i32.add
      (get_local $4)
      (get_local $3)
     )
     (get_local $2)
    )
    (set_local $2
     (i32.add
      (get_local $0)
      (get_local $3)
     )
    )
    (set_local $3
     (tee_local $1
      (i32.add
       (get_local $3)
       (i32.const 1)
      )
     )
    )
    (br_if $label$2
     (tee_local $2
      (i32.load8_u
       (get_local $2)
      )
     )
    )
   )
   (br_if $label$0
    (i32.eqz
     (tee_local $3
      (i32.load8_u
       (tee_local $2
        (i32.load offset=24
         (i32.const 0)
        )
       )
      )
     )
    )
   )
   (set_local $4
    (i32.add
     (get_local $4)
     (get_local $1)
    )
   )
   (set_local $2
    (i32.add
     (get_local $2)
     (i32.const 1)
    )
   )
   (loop $label$3
    (i32.store8
     (get_local $4)
     (get_local $3)
    )
    (set_local $4
     (i32.add
      (get_local $4)
      (i32.const 1)
     )
    )
    (set_local $3
     (i32.load8_u
      (get_local $2)
     )
    )
    (set_local $2
     (i32.add
      (get_local $2)
      (i32.const 1)
     )
    )
    (br_if $label$3
     (get_local $3)
    )
   )
   (br_if $label$0
    (i32.eqz
     (tee_local $3
      (i32.load8_u
       (tee_local $2
        (i32.load offset=24
         (i32.const 0)
        )
       )
      )
     )
    )
   )
   (set_local $2
    (i32.add
     (get_local $2)
     (i32.const 1)
    )
   )
   (loop $label$4
    (i32.store8
     (get_local $4)
     (get_local $3)
    )
    (set_local $4
     (i32.add
      (get_local $4)
      (i32.const 1)
     )
    )
    (set_local $3
     (i32.load8_u
      (get_local $2)
     )
    )
    (set_local $2
     (i32.add
      (get_local $2)
      (i32.const 1)
     )
    )
    (br_if $label$4
     (get_local $3)
    )
   )
   (br_if $label$0
    (i32.eqz
     (tee_local $3
      (i32.load8_u
       (tee_local $2
        (i32.load offset=24
         (i32.const 0)
        )
       )
      )
     )
    )
   )
   (set_local $2
    (i32.add
     (get_local $2)
     (i32.const 1)
    )
   )
   (loop $label$5
    (i32.store8
     (get_local $4)
     (get_local $3)
    )
    (set_local $4
     (i32.add
      (get_local $4)
      (i32.const 1)
     )
    )
    (set_local $3
     (i32.load8_u
      (get_local $2)
     )
    )
    (set_local $2
     (i32.add
      (get_local $2)
      (i32.const 1)
     )
    )
    (br_if $label$5
     (get_local $3)
    )
   )
  )
  (i32.const 42)
 )
)
