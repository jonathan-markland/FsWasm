(module
 (table 0 anyfunc)
 (memory $0 2)
 (export "memory" (memory $0))
 (export "_Z6CircleP7Surfaceiiic" (func $_Z6CircleP7Surfaceiiic))
 (export "_Z4Demov" (func $_Z4Demov))
 (export "main" (func $main))
 (func $_Z6CircleP7Surfaceiiic (; 0 ;) (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32)
  (local $5 i32)
  (i32.store offset=4
   (i32.const 0)
   (tee_local $5
    (i32.sub
     (i32.load offset=4
      (i32.const 0)
     )
     (i32.const 48)
    )
   )
  )
  (i32.store offset=44
   (get_local $5)
   (get_local $0)
  )
  (i32.store offset=40
   (get_local $5)
   (get_local $1)
  )
  (i32.store offset=36
   (get_local $5)
   (get_local $2)
  )
  (i32.store offset=32
   (get_local $5)
   (get_local $3)
  )
  (i32.store8 offset=31
   (get_local $5)
   (get_local $4)
  )
  (set_local $3
   (call $_ZN22PrivateRasterCollectorC2EP7Surfacec
    (i32.add
     (get_local $5)
     (i32.const 8)
    )
    (i32.load offset=44
     (get_local $5)
    )
    (i32.load8_s offset=31
     (get_local $5)
    )
   )
  )
  (call $_ZN8lib80GFX6System9ToRasters22BresenhamFilledEllipseIi22PrivateRasterCollectorEEvT_S4_S4_S4_RT0_
   (i32.sub
    (tee_local $2
     (i32.load offset=40
      (get_local $5)
     )
    )
    (tee_local $4
     (i32.load offset=32
      (get_local $5)
     )
    )
   )
   (i32.sub
    (tee_local $1
     (i32.load offset=36
      (get_local $5)
     )
    )
    (get_local $4)
   )
   (i32.add
    (get_local $2)
    (get_local $4)
   )
   (i32.add
    (get_local $1)
    (get_local $4)
   )
   (get_local $3)
  )
  (i32.store offset=4
   (i32.const 0)
   (i32.add
    (get_local $5)
    (i32.const 48)
   )
  )
 )
 (func $_ZN22PrivateRasterCollectorC2EP7Surfacec (; 1 ;) (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
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
  (i32.store8 offset=7
   (get_local $3)
   (get_local $2)
  )
  (i32.store8
   (tee_local $1
    (i32.load offset=12
     (get_local $3)
    )
   )
   (get_local $2)
  )
  (i32.store offset=4
   (get_local $1)
   (tee_local $2
    (i32.load offset=8
     (get_local $3)
    )
   )
  )
  (i32.store offset=8
   (get_local $1)
   (i32.load offset=8
    (get_local $2)
   )
  )
  (i32.store offset=12
   (get_local $1)
   (i32.load offset=12
    (i32.load offset=8
     (get_local $3)
    )
   )
  )
  (get_local $1)
 )
 (func $_ZN8lib80GFX6System9ToRasters22BresenhamFilledEllipseIi22PrivateRasterCollectorEEvT_S4_S4_S4_RT0_ (; 2 ;) (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32)
  (local $5 i32)
  (i32.store offset=4
   (i32.const 0)
   (tee_local $5
    (i32.sub
     (i32.load offset=4
      (i32.const 0)
     )
     (i32.const 80)
    )
   )
  )
  (i32.store offset=76
   (get_local $5)
   (get_local $0)
  )
  (i32.store offset=72
   (get_local $5)
   (get_local $1)
  )
  (i32.store offset=68
   (get_local $5)
   (get_local $2)
  )
  (i32.store offset=64
   (get_local $5)
   (get_local $3)
  )
  (i32.store offset=60
   (get_local $5)
   (get_local $4)
  )
  (block $label$0
   (br_if $label$0
    (i32.ge_s
     (i32.load offset=76
      (get_local $5)
     )
     (i32.load offset=68
      (get_local $5)
     )
    )
   )
   (br_if $label$0
    (i32.ge_s
     (i32.load offset=72
      (get_local $5)
     )
     (i32.load offset=64
      (get_local $5)
     )
    )
   )
   (br_if $label$0
    (i32.ge_s
     (i32.load offset=76
      (get_local $5)
     )
     (i32.load offset=8
      (i32.load offset=60
       (get_local $5)
      )
     )
    )
   )
   (br_if $label$0
    (i32.lt_s
     (i32.load offset=68
      (get_local $5)
     )
     (i32.const 1)
    )
   )
   (br_if $label$0
    (i32.ge_s
     (i32.load offset=72
      (get_local $5)
     )
     (i32.load offset=12
      (i32.load offset=60
       (get_local $5)
      )
     )
    )
   )
   (br_if $label$0
    (i32.lt_s
     (i32.load offset=64
      (get_local $5)
     )
     (i32.const 1)
    )
   )
   (i32.store offset=56
    (get_local $5)
    (i32.shr_s
     (i32.add
      (i32.load offset=76
       (get_local $5)
      )
      (i32.load offset=68
       (get_local $5)
      )
     )
     (i32.const 1)
    )
   )
   (i32.store offset=52
    (get_local $5)
    (i32.shr_s
     (i32.add
      (i32.load offset=72
       (get_local $5)
      )
      (i32.load offset=64
       (get_local $5)
      )
     )
     (i32.const 1)
    )
   )
   (block $label$1
    (block $label$2
     (br_if $label$2
      (i32.ge_s
       (i32.load offset=76
        (get_local $5)
       )
       (i32.load offset=68
        (get_local $5)
       )
      )
     )
     (set_local $4
      (i32.sub
       (i32.load offset=68
        (get_local $5)
       )
       (i32.load offset=56
        (get_local $5)
       )
      )
     )
     (br $label$1)
    )
    (set_local $4
     (i32.sub
      (i32.load offset=76
       (get_local $5)
      )
      (i32.load offset=56
       (get_local $5)
      )
     )
    )
   )
   (i32.store offset=48
    (get_local $5)
    (get_local $4)
   )
   (block $label$3
    (block $label$4
     (br_if $label$4
      (i32.ge_s
       (i32.load offset=72
        (get_local $5)
       )
       (i32.load offset=64
        (get_local $5)
       )
      )
     )
     (set_local $4
      (i32.sub
       (i32.load offset=64
        (get_local $5)
       )
       (i32.load offset=52
        (get_local $5)
       )
      )
     )
     (br $label$3)
    )
    (set_local $4
     (i32.sub
      (i32.load offset=72
       (get_local $5)
      )
      (i32.load offset=52
       (get_local $5)
      )
     )
    )
   )
   (i32.store offset=44
    (get_local $5)
    (get_local $4)
   )
   (i32.store8 offset=43
    (get_local $5)
    (i32.const 1)
   )
   (block $label$5
    (br_if $label$5
     (i32.ge_s
      (i32.load offset=48
       (get_local $5)
      )
      (i32.load offset=44
       (get_local $5)
      )
     )
    )
    (i32.store offset=36
     (get_local $5)
     (i32.load offset=48
      (get_local $5)
     )
    )
    (i32.store offset=48
     (get_local $5)
     (i32.load offset=44
      (get_local $5)
     )
    )
    (i32.store offset=44
     (get_local $5)
     (i32.load offset=36
      (get_local $5)
     )
    )
    (i32.store8 offset=43
     (get_local $5)
     (i32.const 0)
    )
   )
   (i32.store offset=32
    (get_local $5)
    (i32.sub
     (i32.const 3)
     (i32.shl
      (i32.load offset=48
       (get_local $5)
      )
      (i32.const 1)
     )
    )
   )
   (i32.store offset=28
    (get_local $5)
    (i32.const 0)
   )
   (i32.store offset=24
    (get_local $5)
    (i32.load offset=48
     (get_local $5)
    )
   )
   (i32.store offset=20
    (get_local $5)
    (i32.load offset=44
     (get_local $5)
    )
   )
   (i32.store offset=16
    (get_local $5)
    (i32.const 0)
   )
   (i32.store offset=12
    (get_local $5)
    (i32.shr_s
     (i32.load offset=48
      (get_local $5)
     )
     (i32.const 1)
    )
   )
   (i32.store offset=8
    (get_local $5)
    (i32.shr_s
     (i32.load offset=48
      (get_local $5)
     )
     (i32.const 1)
    )
   )
   (i32.store offset=4
    (get_local $5)
    (i32.const 6)
   )
   (i32.store
    (get_local $5)
    (i32.sub
     (i32.const 10)
     (i32.shl
      (i32.load offset=48
       (get_local $5)
      )
      (i32.const 2)
     )
    )
   )
   (loop $label$6
    (br_if $label$0
     (i32.gt_s
      (i32.load offset=28
       (get_local $5)
      )
      (i32.load offset=24
       (get_local $5)
      )
     )
    )
    (block $label$7
     (block $label$8
      (br_if $label$8
       (i32.eqz
        (i32.and
         (i32.load8_u offset=43
          (get_local $5)
         )
         (i32.const 1)
        )
       )
      )
      (call $_ZN22PrivateRasterCollectorclEiii
       (i32.load offset=60
        (get_local $5)
       )
       (i32.sub
        (tee_local $4
         (i32.load offset=56
          (get_local $5)
         )
        )
        (tee_local $3
         (i32.load offset=28
          (get_local $5)
         )
        )
       )
       (i32.add
        (get_local $4)
        (get_local $3)
       )
       (i32.add
        (i32.load offset=52
         (get_local $5)
        )
        (i32.load offset=20
         (get_local $5)
        )
       )
      )
      (call $_ZN22PrivateRasterCollectorclEiii
       (i32.load offset=60
        (get_local $5)
       )
       (i32.sub
        (tee_local $4
         (i32.load offset=56
          (get_local $5)
         )
        )
        (tee_local $3
         (i32.load offset=24
          (get_local $5)
         )
        )
       )
       (i32.add
        (get_local $4)
        (get_local $3)
       )
       (i32.add
        (i32.load offset=52
         (get_local $5)
        )
        (i32.load offset=16
         (get_local $5)
        )
       )
      )
      (call $_ZN22PrivateRasterCollectorclEiii
       (i32.load offset=60
        (get_local $5)
       )
       (i32.sub
        (tee_local $4
         (i32.load offset=56
          (get_local $5)
         )
        )
        (tee_local $3
         (i32.load offset=28
          (get_local $5)
         )
        )
       )
       (i32.add
        (get_local $4)
        (get_local $3)
       )
       (i32.sub
        (i32.load offset=52
         (get_local $5)
        )
        (i32.load offset=20
         (get_local $5)
        )
       )
      )
      (call $_ZN22PrivateRasterCollectorclEiii
       (i32.load offset=60
        (get_local $5)
       )
       (i32.sub
        (tee_local $4
         (i32.load offset=56
          (get_local $5)
         )
        )
        (tee_local $3
         (i32.load offset=24
          (get_local $5)
         )
        )
       )
       (i32.add
        (get_local $4)
        (get_local $3)
       )
       (i32.sub
        (i32.load offset=52
         (get_local $5)
        )
        (i32.load offset=16
         (get_local $5)
        )
       )
      )
      (br $label$7)
     )
     (call $_ZN22PrivateRasterCollectorclEiii
      (i32.load offset=60
       (get_local $5)
      )
      (i32.sub
       (tee_local $4
        (i32.load offset=56
         (get_local $5)
        )
       )
       (tee_local $3
        (i32.load offset=20
         (get_local $5)
        )
       )
      )
      (i32.add
       (get_local $4)
       (get_local $3)
      )
      (i32.add
       (i32.load offset=52
        (get_local $5)
       )
       (i32.load offset=28
        (get_local $5)
       )
      )
     )
     (call $_ZN22PrivateRasterCollectorclEiii
      (i32.load offset=60
       (get_local $5)
      )
      (i32.sub
       (tee_local $4
        (i32.load offset=56
         (get_local $5)
        )
       )
       (tee_local $3
        (i32.load offset=16
         (get_local $5)
        )
       )
      )
      (i32.add
       (get_local $4)
       (get_local $3)
      )
      (i32.add
       (i32.load offset=52
        (get_local $5)
       )
       (i32.load offset=24
        (get_local $5)
       )
      )
     )
     (call $_ZN22PrivateRasterCollectorclEiii
      (i32.load offset=60
       (get_local $5)
      )
      (i32.sub
       (tee_local $4
        (i32.load offset=56
         (get_local $5)
        )
       )
       (tee_local $3
        (i32.load offset=20
         (get_local $5)
        )
       )
      )
      (i32.add
       (get_local $4)
       (get_local $3)
      )
      (i32.sub
       (i32.load offset=52
        (get_local $5)
       )
       (i32.load offset=28
        (get_local $5)
       )
      )
     )
     (call $_ZN22PrivateRasterCollectorclEiii
      (i32.load offset=60
       (get_local $5)
      )
      (i32.sub
       (tee_local $4
        (i32.load offset=56
         (get_local $5)
        )
       )
       (tee_local $3
        (i32.load offset=16
         (get_local $5)
        )
       )
      )
      (i32.add
       (get_local $4)
       (get_local $3)
      )
      (i32.sub
       (i32.load offset=52
        (get_local $5)
       )
       (i32.load offset=24
        (get_local $5)
       )
      )
     )
    )
    (i32.store offset=8
     (get_local $5)
     (tee_local $4
      (i32.add
       (i32.load offset=8
        (get_local $5)
       )
       (i32.load offset=44
        (get_local $5)
       )
      )
     )
    )
    (block $label$9
     (br_if $label$9
      (i32.le_s
       (get_local $4)
       (i32.load offset=48
        (get_local $5)
       )
      )
     )
     (i32.store offset=16
      (get_local $5)
      (i32.add
       (i32.load offset=16
        (get_local $5)
       )
       (i32.const 1)
      )
     )
     (i32.store offset=8
      (get_local $5)
      (i32.sub
       (i32.load offset=8
        (get_local $5)
       )
       (i32.load offset=48
        (get_local $5)
       )
      )
     )
    )
    (block $label$10
     (br_if $label$10
      (i32.ge_s
       (i32.load offset=32
        (get_local $5)
       )
       (i32.const 0)
      )
     )
     (i32.store offset=32
      (get_local $5)
      (i32.add
       (i32.load offset=32
        (get_local $5)
       )
       (i32.load offset=4
        (get_local $5)
       )
      )
     )
     (i32.store offset=4
      (get_local $5)
      (i32.add
       (i32.load offset=4
        (get_local $5)
       )
       (i32.const 4)
      )
     )
     (i32.store
      (get_local $5)
      (i32.add
       (i32.load
        (get_local $5)
       )
       (i32.const 4)
      )
     )
     (i32.store offset=28
      (get_local $5)
      (i32.add
       (i32.load offset=28
        (get_local $5)
       )
       (i32.const 1)
      )
     )
     (br $label$6)
    )
    (i32.store offset=32
     (get_local $5)
     (i32.add
      (i32.load offset=32
       (get_local $5)
      )
      (i32.load
       (get_local $5)
      )
     )
    )
    (i32.store offset=4
     (get_local $5)
     (i32.add
      (i32.load offset=4
       (get_local $5)
      )
      (i32.const 4)
     )
    )
    (i32.store
     (get_local $5)
     (i32.add
      (i32.load
       (get_local $5)
      )
      (i32.const 8)
     )
    )
    (i32.store offset=28
     (get_local $5)
     (i32.add
      (i32.load offset=28
       (get_local $5)
      )
      (i32.const 1)
     )
    )
    (i32.store offset=24
     (get_local $5)
     (i32.add
      (i32.load offset=24
       (get_local $5)
      )
      (i32.const -1)
     )
    )
    (i32.store offset=12
     (get_local $5)
     (tee_local $4
      (i32.add
       (i32.load offset=12
        (get_local $5)
       )
       (i32.load offset=44
        (get_local $5)
       )
      )
     )
    )
    (br_if $label$6
     (i32.le_s
      (get_local $4)
      (i32.load offset=48
       (get_local $5)
      )
     )
    )
    (i32.store offset=20
     (get_local $5)
     (i32.add
      (i32.load offset=20
       (get_local $5)
      )
      (i32.const -1)
     )
    )
    (i32.store offset=12
     (get_local $5)
     (i32.sub
      (i32.load offset=12
       (get_local $5)
      )
      (i32.load offset=48
       (get_local $5)
      )
     )
    )
    (br $label$6)
   )
  )
  (i32.store offset=4
   (i32.const 0)
   (i32.add
    (get_local $5)
    (i32.const 80)
   )
  )
 )
 (func $_Z4Demov (; 3 ;)
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
   (i32.const 16)
  )
  (block $label$0
   (loop $label$1
    (br_if $label$0
     (i32.ge_u
      (i32.load offset=28
       (get_local $1)
      )
      (i32.const 81936)
     )
    )
    (i32.store8
     (tee_local $0
      (i32.load offset=28
       (get_local $1)
      )
     )
     (i32.const 0)
    )
    (i32.store offset=28
     (get_local $1)
     (i32.add
      (get_local $0)
      (i32.const 1)
     )
    )
    (br $label$1)
   )
  )
  (i32.store offset=12
   (get_local $1)
   (i32.const 320)
  )
  (i32.store offset=8
   (get_local $1)
   (i32.const 16)
  )
  (i32.store offset=16
   (get_local $1)
   (i32.const 320)
  )
  (i32.store offset=20
   (get_local $1)
   (i32.const 256)
  )
  (call $_Z6CircleP7Surfaceiiic
   (i32.add
    (get_local $1)
    (i32.const 8)
   )
   (i32.const 160)
   (i32.const 100)
   (i32.const 80)
   (i32.const -1)
  )
  (call $_Z6CircleP7Surfaceiiic
   (i32.add
    (get_local $1)
    (i32.const 8)
   )
   (i32.const 160)
   (i32.const 100)
   (i32.const 40)
   (i32.const -64)
  )
  (i32.store offset=4
   (i32.const 0)
   (i32.add
    (get_local $1)
    (i32.const 32)
   )
  )
 )
 (func $main (; 4 ;) (result i32)
  (local $0 i32)
  (i32.store offset=4
   (i32.const 0)
   (tee_local $0
    (i32.sub
     (i32.load offset=4
      (i32.const 0)
     )
     (i32.const 16)
    )
   )
  )
  (i32.store offset=12
   (get_local $0)
   (i32.const 0)
  )
  (call $_Z4Demov)
  (i32.store offset=4
   (i32.const 0)
   (i32.add
    (get_local $0)
    (i32.const 16)
   )
  )
  (i32.const 0)
 )
 (func $_ZN22PrivateRasterCollectorclEiii (; 5 ;) (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32)
  (local $4 i32)
  (i32.store offset=28
   (tee_local $4
    (i32.sub
     (i32.load offset=4
      (i32.const 0)
     )
     (i32.const 32)
    )
   )
   (get_local $0)
  )
  (i32.store offset=24
   (get_local $4)
   (get_local $1)
  )
  (i32.store offset=20
   (get_local $4)
   (get_local $2)
  )
  (i32.store offset=16
   (get_local $4)
   (get_local $3)
  )
  (block $label$0
   (br_if $label$0
    (i32.lt_s
     (get_local $3)
     (i32.const 0)
    )
   )
   (br_if $label$0
    (i32.ge_s
     (i32.load offset=16
      (get_local $4)
     )
     (i32.load offset=12
      (tee_local $2
       (i32.load offset=28
        (get_local $4)
       )
      )
     )
    )
   )
   (br_if $label$0
    (i32.ge_s
     (i32.load offset=24
      (get_local $4)
     )
     (i32.load offset=20
      (get_local $4)
     )
    )
   )
   (br_if $label$0
    (i32.ge_s
     (i32.load offset=24
      (get_local $4)
     )
     (i32.load offset=8
      (get_local $2)
     )
    )
   )
   (br_if $label$0
    (i32.lt_s
     (i32.load offset=20
      (get_local $4)
     )
     (i32.const 1)
    )
   )
   (block $label$1
    (br_if $label$1
     (i32.gt_s
      (i32.load offset=24
       (get_local $4)
      )
      (i32.const -1)
     )
    )
    (i32.store offset=24
     (get_local $4)
     (i32.const 0)
    )
   )
   (block $label$2
    (br_if $label$2
     (i32.le_s
      (i32.load offset=20
       (get_local $4)
      )
      (i32.load offset=8
       (get_local $2)
      )
     )
    )
    (i32.store offset=20
     (get_local $4)
     (i32.load offset=8
      (get_local $2)
     )
    )
   )
   (i32.store offset=12
    (get_local $4)
    (tee_local $3
     (i32.add
      (i32.add
       (i32.load
        (tee_local $3
         (i32.load offset=4
          (get_local $2)
         )
        )
       )
       (i32.mul
        (i32.load offset=4
         (get_local $3)
        )
        (i32.load offset=16
         (get_local $4)
        )
       )
      )
      (i32.load offset=24
       (get_local $4)
      )
     )
    )
   )
   (i32.store offset=8
    (get_local $4)
    (i32.add
     (get_local $3)
     (i32.sub
      (i32.load offset=20
       (get_local $4)
      )
      (i32.load offset=24
       (get_local $4)
      )
     )
    )
   )
   (loop $label$3
    (br_if $label$0
     (i32.ge_u
      (i32.load offset=12
       (get_local $4)
      )
      (i32.load offset=8
       (get_local $4)
      )
     )
    )
    (i32.store8
     (tee_local $3
      (i32.load offset=12
       (get_local $4)
      )
     )
     (i32.load8_u
      (get_local $2)
     )
    )
    (i32.store offset=12
     (get_local $4)
     (i32.add
      (get_local $3)
      (i32.const 1)
     )
    )
    (br $label$3)
   )
  )
 )
)
