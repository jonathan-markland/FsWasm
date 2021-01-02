module ArmSupportLibrary



type Operand2SignClass = 

    /// Use the positive logic version of the instruction.
    | Operand2Positive 
    
    /// Use the partnering negative logic version of the instruction.
    | Operand2Negative



/// Can constant 'c' be expressed as an 8-bit number left-shifted
/// by an even number of places?  If so, returns the constant in
/// the form of an 8-bit value and a left-shift amount, if not,
/// returns None.
let ConstantToOperand2Form c =

    let inline fitsWithin maskArea constant =
        ((constant ||| maskArea) ^^^ maskArea) = 0u

    let rec tryShiftOf shift =
        if shift <= 24 then 
            let maskArea = 255u <<< shift
            if c |> fitsWithin maskArea then
                Some (c >>> shift, shift)
            else
                tryShiftOf (shift + 2)
        else
            None

    tryShiftOf 0



/// Can constant 'c' be expressed as an 8-bit number left-shifted
/// by an even number of places, or the complement of that?
let ConstantToOperand2FormConsideringNegation c =

    match ConstantToOperand2Form c with
        | Some (const8,shift) -> 
            Some ( Operand2Positive, const8, shift )
        | None ->
            match ConstantToOperand2Form (~~~c) with
                | Some (const8, shift) -> 
                    Some ( Operand2Negative, const8, shift )
                | None -> 
                    None


/// Would the given constant require the MOVT instruction?
let inline RequiresMovT c =
    c > 65535u


/// Return an ARM instruction sequence that loads a constant 
/// into the given register without memory references, or 
/// altering any other registers.
let LoadConstantInto targetRegister value =

    match ConstantToOperand2FormConsideringNegation value with

        | Some (Operand2Positive, const8, shift) ->
            [ sprintf "mov %s,#%d  ; mov %d LSL %d" targetRegister value const8 shift ]

        | Some (Operand2Negative, const8, shift) ->
            [ sprintf "mov %s,#%d  ; mvn %d LSL %d" targetRegister value const8 shift ]

        | None ->
            let movw = sprintf "movw %s,%d" targetRegister (value &&& 0xFFFFu)
            if (value |> RequiresMovT) then
                let movt = sprintf "movt %s,%d" targetRegister (value >>> 16)
                [ movw ; movt ]
            else
                [ movw ]



/// Perform mathematical operation on a register altering
/// its value, eg: add-to, sub-from, compare-with, orr-with, eor-with (etc).
/// The tempRegister MAY be used to form the constant.
let MathsWithConstant armInstruction targetRegister value tempRegister =

    let optReg2 = if (armInstruction = "cmp") then "" else (targetRegister + ",")

    match ConstantToOperand2FormConsideringNegation value with

        | Some (Operand2Positive, const8, shift) ->
            [ sprintf "%s %s,%s#%d ; %s" armInstruction targetRegister optReg2 value armInstruction ]

        | Some (Operand2Negative, const8, shift) ->
            [ sprintf "%s %s,%s#%d ; Expecting %s's negative-partner" armInstruction targetRegister optReg2 value armInstruction ]

        | None ->
            (LoadConstantInto tempRegister value) @
            [ sprintf "%s %s,%s%s" armInstruction targetRegister optReg2 tempRegister ]



/// Data types for load/store instructions LDR and STR.
type ArmTypeSize = ArmWord | ArmHalfword | ArmByte



/// Describes how an immediate offset value is to be handled.
type ArmOffsetStrategy =

    /// The offset is zero anyway, so no offset field is needed at all.   [...]
    | NoOffset

    /// The offset can be done with the immediate field:  [..., #offset]
    | ImmediateOffset of offset:uint32

    /// The offset must can't fit the immediate field, so need temporary register.
    | OffsetUsingTempRegister of armReg:string * offset:uint32 // [..., Rtemp]



/// Returns a value indicating how to handle the given offset
/// constant for load/store operations.
let ArmOffsetHandlingStrategyFor fetchStoreType offsetDesired offsetTempRegister =

    let maxRangeFor = function
        | ArmWord      ->  4095u
        | ArmHalfword  ->   255u
        | ArmByte      ->  4095u

    if offsetDesired = 0u then
        NoOffset

    else if offsetDesired <= (maxRangeFor fetchStoreType) then
        ImmediateOffset offsetDesired

    else
        OffsetUsingTempRegister (offsetTempRegister, offsetDesired) 



/// Returns a load constant instruction sequence (if one is needed) for 
/// the given ArmOffsetStrategy.
let OffsetLoadInstructionFor = function
    | NoOffset -> []
    | ImmediateOffset _ -> []
    | OffsetUsingTempRegister (armReg, offset) -> LoadConstantInto armReg offset
    


/// Returns an immediate offset component for use in addressing, which
/// may involve use of a temporary register for large offsets.
let ArmOffset = function
    | NoOffset -> ""
    | ImmediateOffset o -> ", #" + (o.ToString())
    | OffsetUsingTempRegister (armReg, _) -> ", " + armReg



/// Return an instruction sequence for load or store, with optional 
/// immediate offset addressing.  May require an additional temporary
/// register for the immediate offset, if it is too large for the 
/// instruction fields.
let LoadStoreRegOffset fetchStoreType loadStoreInstruction addressRegister offsetDesired offsetTempRegister = 

    let offsetStrategy = 
        ArmOffsetHandlingStrategyFor fetchStoreType offsetDesired offsetTempRegister 

    (OffsetLoadInstructionFor offsetStrategy)
        @ [ sprintf "%s R0,[%s%s]" loadStoreInstruction addressRegister (ArmOffset offsetStrategy) ]



/// Returns an instruction sequence for storing a constant.
/// May require up to two temporary registers for the constant and immediate 
/// offset, if these values are too large for the instruction fields.
let StoreConstant fetchStoreType armStoreInstruction addressRegister offsetDesired value offsetTempRegister constantTempRegister = 
    
    let offsetStrategy = 
        ArmOffsetHandlingStrategyFor fetchStoreType offsetDesired offsetTempRegister 
    
    (LoadConstantInto constantTempRegister value) @
    (OffsetLoadInstructionFor offsetStrategy) @
    [ sprintf "%s %s,[%s%s]" armStoreInstruction constantTempRegister addressRegister (ArmOffset offsetStrategy) ] 
