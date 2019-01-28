module BWToCRMConfigurationTypes

type BarrierConfig = WithBarriers | WithoutBarriers
type OptimisationConfig = FullyOptimised | NoOptimisation
type OutputOrderConfig = DebugOutputOrder | FinalOutputOrder
type WriteOutFunctionConfig = TranslationConfiguration of BarrierConfig * OptimisationConfig * OutputOrderConfig

