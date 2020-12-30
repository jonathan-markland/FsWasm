module BWToCRMConfigurationTypes


type BarrierConfig = 

    /// The generation should include the "register barrier" indicators.
    /// These are boundaries over which no general registers contain needed values.
    | WithBarriers 

    /// The generation should omit the "register barrier" indicators for tidyness.
    | WithoutBarriers


type OptimisationConfig = 

    /// All defined peephole optimisations are applied.
    | FullyOptimised 

    /// No optimisations are applied.  Instead, the verbose primary stack 
    /// machine-style generation is shown.
    | NoOptimisation


type WriteOutFunctionConfig = 
    TranslationConfiguration of BarrierConfig * OptimisationConfig

