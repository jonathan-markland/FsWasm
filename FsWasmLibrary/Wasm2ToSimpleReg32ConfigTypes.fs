module Wasm2ToSimpleReg32ConfigTypes

type BarrierConfig = WithBarriers | WithoutBarriers
type OptimisationConfig = FullyOptimised | NoOptimisation
type WriteOutFunctionConfig = WriteOutFunctionConfig of BarrierConfig * OptimisationConfig
