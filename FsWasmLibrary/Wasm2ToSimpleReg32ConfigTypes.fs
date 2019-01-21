module Wasm2ToSimpleReg32ConfigTypes

type BarrierConfig = WithBarriers | WithoutBarriers
type OptimisationConfig = FullyOptimised | NoOptimisation
type OutputOrderConfig = DebugOutputOrder | FinalOutputOrder
type WriteOutFunctionConfig = WriteOutFunctionConfig of BarrierConfig * OptimisationConfig * OutputOrderConfig

