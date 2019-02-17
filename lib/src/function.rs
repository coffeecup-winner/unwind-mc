#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
pub enum FunctionStatus {
    Created,
    BoundsResolved,
    BoundsNotResolvedInvalidAddress,
    BoundsNotResolvedIncompleteGraph,
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
pub enum CallingConvention {
    Unknown,
    Stdcall,
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
pub struct Function {
    pub address: u64,
    pub status: FunctionStatus,
    pub calling_convention: CallingConvention,
    pub arguments_size: Option<u16>,
}
