namespace FP

module SDF =
    type BiOperation = float->float->float
    
    type UOperation = float -> float
    
    type Operation =
        | Unary of UOperation
        | Binary of BiOperation
        
    type Operand =
        
        
    type BinaryAgent() =
        let agent =
            MailboxProcessor.Start(fun inbox ->
                let rec loop graph:Graph =
                    async {
                        
                    })