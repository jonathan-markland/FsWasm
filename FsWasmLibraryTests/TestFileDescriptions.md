
The WasmFiddle Source Files
===========================



program (1).wasm
----------------

int main() { 
  return 42;
}

(module
 (table 0 anyfunc)
 (memory $0 1)
 (export "memory" (memory $0))
 (export "main" (func $main))
 (func $main (; 0 ;) (result i32)
  (i32.const 42)
 )
)




program (2).wasm
----------------

int WriteToAddressThenRead(int n)
{
  *(int *)n = 0x12345678;
  return *(int *)0xAABBCCDD;
}

int main() { 
  return WriteToAddressThenRead(0x88776655);
}

(module
 (table 0 anyfunc)
 (memory $0 1)
 (export "memory" (memory $0))
 (export "WriteToAddressThenRead" (func $WriteToAddressThenRead))
 (export "main" (func $main))
 (func $WriteToAddressThenRead (; 0 ;) (param $0 i32) (result i32)
  (i32.store
   (get_local $0)
   (i32.const 305419896)
  )
  (i32.load offset=2864434397
   (i32.const 0)
  )
 )
 (func $main (; 1 ;) (result i32)
  (call $WriteToAddressThenRead
   (i32.const -2005440939)
  )
 )
)




program (3).wasm
----------------

#include "stdio.h"

int main() { 
  printf ("%s", "Hello there, this is a test!");
  return 100;
}

(module
 (type $FUNCSIG$ii (func (param i32) (result i32)))
 (type $FUNCSIG$iii (func (param i32 i32) (result i32)))
 (import "env" "printf" (func $printf (param i32 i32) (result i32)))
 (table 0 anyfunc)
 (memory $0 1)
 (data (i32.const 16) "%s\00")
 (data (i32.const 32) "Hello there, this is a test!\00")
 (export "memory" (memory $0))
 (export "main" (func $main))
 (func $main (; 1 ;) (result i32)
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
  (i32.store
   (get_local $0)
   (i32.const 32)
  )
  (drop
   (call $printf
    (i32.const 16)
    (get_local $0)
   )
  )
  (i32.store offset=4
   (i32.const 0)
   (i32.add
    (get_local $0)
    (i32.const 16)
   )
  )
  (i32.const 100)
 )
)





program (4).wasm
----------------

#include "stdio.h"

int main() { 
  for(int i=0; i<10; i++)
  {
    printf ("%s %d", "Loop iteration: ", i);
  }
  return 0;
}

(module
 (type $FUNCSIG$ii (func (param i32) (result i32)))
 (type $FUNCSIG$iii (func (param i32 i32) (result i32)))
 (import "env" "printf" (func $printf (param i32 i32) (result i32)))
 (table 0 anyfunc)
 (memory $0 1)
 (data (i32.const 16) "%s %d\00")
 (data (i32.const 32) "Loop iteration: \00")
 (export "memory" (memory $0))
 (export "main" (func $main))
 (func $main (; 1 ;) (result i32)
  (local $0 i32)
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
  (set_local $0
   (i32.const 0)
  )
  (loop $label$0
   (i32.store
    (get_local $1)
    (i32.const 32)
   )
   (i32.store offset=4
    (get_local $1)
    (get_local $0)
   )
   (drop
    (call $printf
     (i32.const 16)
     (get_local $1)
    )
   )
   (br_if $label$0
    (i32.ne
     (tee_local $0
      (i32.add
       (get_local $0)
       (i32.const 1)
      )
     )
     (i32.const 10)
    )
   )
  )
  (i32.store offset=4
   (i32.const 0)
   (i32.add
    (get_local $1)
    (i32.const 16)
   )
  )
  (i32.const 0)
 )
)





program (5).wasm
----------------

unsigned short ScreenMemory[80*25];

void UncheckedPaintRectangle(unsigned short *topLeft, int width, int height, int strafe, unsigned short paintValue)
{
  unsigned short *rowStart = topLeft;
  while(height > 0)
  {
    int w = width;
    unsigned short *d = rowStart;
    while(w > 0)
    {
      *d = paintValue;
      ++d;
      --w;
    }
    --height;
    rowStart += strafe;
  }
}


int main() { 
  UncheckedPaintRectangle(ScreenMemory, 80,25, 80, 0x1f41);
  UncheckedPaintRectangle(ScreenMemory + 10 + (80*3), 40,5, 80, 0x4f42);
  return 0;
}

(module
 (table 0 anyfunc)
 (memory $0 1)
 (export "memory" (memory $0))
 (export "UncheckedPaintRectangle" (func $UncheckedPaintRectangle))
 (export "main" (func $main))
 (func $UncheckedPaintRectangle (; 0 ;) (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (block $label$0
   (br_if $label$0
    (i32.lt_s
     (get_local $2)
     (i32.const 1)
    )
   )
   (set_local $5
    (i32.add
     (get_local $1)
     (i32.const 1)
    )
   )
   (set_local $6
    (i32.shl
     (get_local $3)
     (i32.const 1)
    )
   )
   (loop $label$1
    (block $label$2
     (br_if $label$2
      (i32.lt_s
       (get_local $1)
       (i32.const 1)
      )
     )
     (set_local $7
      (get_local $5)
     )
     (set_local $3
      (get_local $0)
     )
     (loop $label$3
      (i32.store16
       (get_local $3)
       (get_local $4)
      )
      (set_local $3
       (i32.add
        (get_local $3)
        (i32.const 2)
       )
      )
      (br_if $label$3
       (i32.gt_s
        (tee_local $7
         (i32.add
          (get_local $7)
          (i32.const -1)
         )
        )
        (i32.const 1)
       )
      )
     )
    )
    (set_local $0
     (i32.add
      (get_local $0)
      (get_local $6)
     )
    )
    (set_local $3
     (i32.gt_s
      (get_local $2)
      (i32.const 1)
     )
    )
    (set_local $2
     (i32.add
      (get_local $2)
      (i32.const -1)
     )
    )
    (br_if $label$1
     (get_local $3)
    )
   )
  )
 )
 (func $main (; 1 ;) (result i32)
  (call $UncheckedPaintRectangle
   (i32.const 16)
   (i32.const 80)
   (i32.const 25)
   (i32.const 80)
   (i32.const 8001)
  )
  (call $UncheckedPaintRectangle
   (i32.const 516)
   (i32.const 40)
   (i32.const 5)
   (i32.const 80)
   (i32.const 20290)
  )
  (i32.const 0)
 )
)





program (6).wasm
----------------


void DoSomething(float f, double d, int i, long long l)
{
  *(float     *)  0x100 = 0.5f + f - (f / 2);
  *(double    *)  0x108 = 123.456 + d - (d / 2);
  *(int       *)  0x110 = 123456 + i - (i / 2);
  *(long long *)  0x118 = 123456789123456 + l - (l / 2);
}


int main() {
  DoSomething(1.0f, 2.0, 3, 4);
  return 42;
}


(module
 (table 0 anyfunc)
 (memory $0 1)
 (export "memory" (memory $0))
 (export "DoSomething" (func $DoSomething))
 (export "main" (func $main))
 (func $DoSomething (; 0 ;) (param $0 f32) (param $1 f64) (param $2 i32) (param $3 i64)
  (f64.store offset=264
   (i32.const 0)
   (f64.add
    (f64.add
     (get_local $1)
     (f64.const 123.456)
    )
    (f64.mul
     (get_local $1)
     (f64.const -0.5)
    )
   )
  )
  (f32.store offset=256
   (i32.const 0)
   (f32.sub
    (f32.add
     (get_local $0)
     (f32.const 0.5)
    )
    (f32.mul
     (get_local $0)
     (f32.const 0.5)
    )
   )
  )
  (i32.store offset=272
   (i32.const 0)
   (i32.add
    (i32.add
     (get_local $2)
     (i32.div_s
      (get_local $2)
      (i32.const -2)
     )
    )
    (i32.const 123456)
   )
  )
  (i64.store offset=280
   (i32.const 0)
   (i64.add
    (i64.add
     (get_local $3)
     (i64.div_s
      (get_local $3)
      (i64.const -2)
     )
    )
    (i64.const 123456789123456)
   )
  )
 )
 (func $main (; 1 ;) (result i32)
  (call $DoSomething
   (f32.const 1)
   (f64.const 2)
   (i32.const 3)
   (i64.const 4)
  )
  (i32.const 42)
 )
)


