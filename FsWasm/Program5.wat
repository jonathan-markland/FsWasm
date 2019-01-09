(module
 (type $FUNCSIG$v (func))
 (type $FUNCSIG$vi (func (param i32)))
 (table 5 5 anyfunc)
 (elem (i32.const 0) $__wasm_nullptr $Write1 $WriteN $Write2 $WriteM)
 (memory $0 1)
 (data (i32.const 16) "Write1()\00")
 (data (i32.const 32) "Write2()\00")
 (data (i32.const 48) "NNNNNNNNN\00")
 (data (i32.const 64) "MMMMMMMMM\00")
 (data (i32.const 80) "Hello\00")
 (data (i32.const 96) "This is the bottom line of the screen\00")
 (data (i32.const 144) "**********\00")
 (data (i32.const 160) "AAAAAAAAAA\00")
 (export "memory" (memory $0))
 (export "ClearScreen" (func $ClearScreen))
 (export "WriteText" (func $WriteText))
 (export "Write1" (func $Write1))
 (export "Write2" (func $Write2))
 (export "WriteN" (func $WriteN))
 (export "WriteM" (func $WriteM))
 (export "WaitKey" (func $WaitKey))
 (export "main" (func $main))
 (func $ClearScreen (; 0 ;)
  (local $0 i32)
  (i32.store offset=12
   (tee_local $0
    (i32.sub
     (i32.load offset=4
      (i32.const 0)
     )
     (i32.const 16)
    )
   )
   (i32.const 753664)
  )
  (block $label$0
   (loop $label$1
    (br_if $label$0
     (i32.gt_s
      (i32.load offset=12
       (get_local $0)
      )
      (i32.const 761855)
     )
    )
    (i32.store16
     (i32.load offset=12
      (get_local $0)
     )
     (i32.const 7968)
    )
    (i32.store offset=12
     (get_local $0)
     (i32.add
      (i32.load offset=12
       (get_local $0)
      )
      (i32.const 2)
     )
    )
    (br $label$1)
   )
  )
 )
 (func $WriteText (; 1 ;) (param $0 i32) (param $1 i32) (param $2 i32)
  (local $3 i32)
  (i32.store offset=12
   (tee_local $3
    (i32.sub
     (i32.load offset=4
      (i32.const 0)
     )
     (i32.const 16)
    )
   )
   (get_local $0)
  )
  (i32.store offset=8
   (get_local $3)
   (get_local $1)
  )
  (i32.store offset=4
   (get_local $3)
   (get_local $2)
  )
  (i32.store
   (get_local $3)
   (i32.add
    (i32.add
     (i32.mul
      (i32.load offset=8
       (get_local $3)
      )
      (i32.const 160)
     )
     (i32.load offset=12
      (get_local $3)
     )
    )
    (i32.const 753664)
   )
  )
  (block $label$0
   (loop $label$1
    (br_if $label$0
     (i32.eqz
      (i32.load8_u
       (i32.load offset=4
        (get_local $3)
       )
      )
     )
    )
    (i32.store8
     (i32.load
      (get_local $3)
     )
     (i32.load8_u
      (i32.load offset=4
       (get_local $3)
      )
     )
    )
    (i32.store8
     (i32.add
      (i32.load
       (get_local $3)
      )
      (i32.const 1)
     )
     (i32.const 47)
    )
    (i32.store
     (get_local $3)
     (i32.add
      (i32.load
       (get_local $3)
      )
      (i32.const 2)
     )
    )
    (i32.store offset=4
     (get_local $3)
     (i32.add
      (i32.load offset=4
       (get_local $3)
      )
      (i32.const 1)
     )
    )
    (br $label$1)
   )
  )
 )
 (func $Write1 (; 2 ;) (type $FUNCSIG$v)
  (call $WriteText
   (i32.const 20)
   (i32.const 10)
   (i32.const 16)
  )
 )
 (func $Write2 (; 3 ;) (type $FUNCSIG$v)
  (call $WriteText
   (i32.const 20)
   (i32.const 10)
   (i32.const 32)
  )
 )
 (func $WriteN (; 4 ;) (type $FUNCSIG$vi) (param $0 i32)
  (local $1 i32)
  (i32.store offset=4
   (i32.const 0)
   (tee_local $1
    (i32.sub
     (i32.load offset=4
      (i32.const 0)
     )
     (i32.const 16)
    )
   )
  )
  (i32.store offset=12
   (get_local $1)
   (get_local $0)
  )
  (call $WriteText
   (i32.const 20)
   (get_local $0)
   (i32.const 48)
  )
  (i32.store offset=4
   (i32.const 0)
   (i32.add
    (get_local $1)
    (i32.const 16)
   )
  )
 )
 (func $WriteM (; 5 ;) (type $FUNCSIG$vi) (param $0 i32)
  (local $1 i32)
  (i32.store offset=4
   (i32.const 0)
   (tee_local $1
    (i32.sub
     (i32.load offset=4
      (i32.const 0)
     )
     (i32.const 16)
    )
   )
  )
  (i32.store offset=12
   (get_local $1)
   (get_local $0)
  )
  (call $WriteText
   (i32.const 20)
   (get_local $0)
   (i32.const 64)
  )
  (i32.store offset=4
   (i32.const 0)
   (i32.add
    (get_local $1)
    (i32.const 16)
   )
  )
 )
 (func $WaitKey (; 6 ;) (result i32)
  (i32.load8_s offset=786432
   (i32.const 0)
  )
 )
 (func $main (; 7 ;) (result i32)
  (local $0 i32)
  (local $1 i32)
  (i32.store offset=4
   (i32.const 0)
   (tee_local $1
    (i32.sub
     (i32.load offset=4
      (i32.const 0)
     )
     (i32.const 32)
    )
   )
  )
  (i32.store offset=28
   (get_local $1)
   (i32.const 0)
  )
  (call $ClearScreen)
  (call $WriteText
   (i32.const 10)
   (i32.const 2)
   (i32.const 80)
  )
  (call $WriteText
   (i32.const 0)
   (i32.const 24)
   (i32.const 96)
  )
  (i32.store offset=24
   (get_local $1)
   (i32.const 4)
  )
  (block $label$0
   (loop $label$1
    (br_if $label$0
     (i32.gt_s
      (i32.load offset=24
       (get_local $1)
      )
      (i32.const 13)
     )
    )
    (call $WriteText
     (i32.const 20)
     (i32.load offset=24
      (get_local $1)
     )
     (i32.const 144)
    )
    (i32.store offset=24
     (get_local $1)
     (i32.add
      (i32.load offset=24
       (get_local $1)
      )
      (i32.const 1)
     )
    )
    (br $label$1)
   )
  )
  (i32.store offset=20
   (get_local $1)
   (i32.const 1)
  )
  (i32.store offset=16
   (get_local $1)
   (i32.const 2)
  )
  (block $label$2
   (loop $label$3
    (i32.store offset=12
     (get_local $1)
     (tee_local $0
      (call $WaitKey)
     )
    )
    (br_if $label$2
     (i32.eq
      (get_local $0)
      (i32.const 27)
     )
    )
    (i32.store offset=8
     (get_local $1)
     (i32.const 4)
    )
    (loop $label$4
     (br_if $label$3
      (i32.gt_s
       (i32.load offset=8
        (get_local $1)
       )
       (i32.const 13)
      )
     )
     (block $label$5
      (br_if $label$5
       (i32.gt_u
        (tee_local $0
         (i32.add
          (i32.load offset=12
           (get_local $1)
          )
          (i32.const -65)
         )
        )
        (i32.const 15)
       )
      )
      (block $label$6
       (block $label$7
        (block $label$8
         (block $label$9
          (block $label$10
           (block $label$11
            (block $label$12
             (block $label$13
              (block $label$14
               (block $label$15
                (block $label$16
                 (block $label$17
                  (block $label$18
                   (block $label$19
                    (block $label$20
                     (block $label$21
                      (br_table $label$21 $label$20 $label$19 $label$18 $label$17 $label$16 $label$15 $label$14 $label$13 $label$12 $label$11 $label$10 $label$9 $label$8 $label$7 $label$6 $label$21
                       (get_local $0)
                      )
                     )
                     (call $WriteText
                      (i32.const 20)
                      (i32.load offset=8
                       (get_local $1)
                      )
                      (i32.const 160)
                     )
                     (i32.store offset=20
                      (get_local $1)
                      (i32.const 1)
                     )
                     (br $label$5)
                    )
                    (call $WriteText
                     (i32.const 20)
                     (i32.load offset=8
                      (get_local $1)
                     )
                     (i32.const 160)
                    )
                    (i32.store offset=20
                     (get_local $1)
                     (i32.const 3)
                    )
                    (br $label$5)
                   )
                   (call $WriteText
                    (i32.const 20)
                    (i32.load offset=8
                     (get_local $1)
                    )
                    (i32.const 160)
                   )
                   (br $label$5)
                  )
                  (call $WriteText
                   (i32.const 20)
                   (i32.load offset=8
                    (get_local $1)
                   )
                   (i32.const 160)
                  )
                  (br $label$5)
                 )
                 (call $WriteText
                  (i32.const 20)
                  (i32.load offset=8
                   (get_local $1)
                  )
                  (i32.const 160)
                 )
                 (br $label$5)
                )
                (call $WriteText
                 (i32.const 20)
                 (i32.load offset=8
                  (get_local $1)
                 )
                 (i32.const 160)
                )
                (br $label$5)
               )
               (call $WriteText
                (i32.const 20)
                (i32.load offset=8
                 (get_local $1)
                )
                (i32.const 160)
               )
               (i32.store offset=16
                (get_local $1)
                (i32.const 2)
               )
               (br $label$5)
              )
              (call $WriteText
               (i32.const 20)
               (i32.load offset=8
                (get_local $1)
               )
               (i32.const 160)
              )
              (i32.store offset=16
               (get_local $1)
               (i32.const 4)
              )
              (br $label$5)
             )
             (call $WriteText
              (i32.const 20)
              (i32.load offset=8
               (get_local $1)
              )
              (i32.const 160)
             )
             (br $label$5)
            )
            (call $WriteText
             (i32.const 20)
             (i32.load offset=8
              (get_local $1)
             )
             (i32.const 160)
            )
            (br $label$5)
           )
           (call $WriteText
            (i32.const 20)
            (i32.load offset=8
             (get_local $1)
            )
            (i32.const 160)
           )
           (br $label$5)
          )
          (call $WriteText
           (i32.const 20)
           (i32.load offset=8
            (get_local $1)
           )
           (i32.const 160)
          )
          (br $label$5)
         )
         (call $WriteText
          (i32.const 20)
          (i32.load offset=8
           (get_local $1)
          )
          (i32.const 160)
         )
         (br $label$5)
        )
        (call $WriteText
         (i32.const 20)
         (i32.load offset=8
          (get_local $1)
         )
         (i32.const 160)
        )
        (br $label$5)
       )
       (call $WriteText
        (i32.const 20)
        (i32.load offset=8
         (get_local $1)
        )
        (i32.const 160)
       )
       (br $label$5)
      )
      (call $WriteText
       (i32.const 20)
       (i32.load offset=8
        (get_local $1)
       )
       (i32.const 160)
      )
     )
     (call_indirect (type $FUNCSIG$v)
      (i32.load offset=20
       (get_local $1)
      )
     )
     (call_indirect (type $FUNCSIG$v)
      (i32.load offset=16
       (get_local $1)
      )
     )
     (i32.store offset=8
      (get_local $1)
      (i32.add
       (i32.load offset=8
        (get_local $1)
       )
       (i32.const 1)
      )
     )
     (br $label$4)
    )
   )
  )
  (i32.store offset=4
   (i32.const 0)
   (i32.add
    (get_local $1)
    (i32.const 32)
   )
  )
  (i32.const 0)
 )
 (func $__wasm_nullptr (; 8 ;) (type $FUNCSIG$v)
  (unreachable)
 )
)
