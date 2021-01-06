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


type EntryPointConfig =

    /// Use the following named export function as the entry point.
    /// The WASM module needs to export a function under this name.
    | ForceEntryPoint of exportName:string

    /// Use the WASM Start record as the entry point (if present).
    /// TODO: This does NOT generate the initialisation pathway that ForceEntryPoint does,
    /// because this case needs design consideration.
    | WasmStartEntryPointIfPresent
    

type WriteOutFunctionConfig =  // TODO: better name
    TranslationConfiguration of BarrierConfig * OptimisationConfig * EntryPointConfig

