import {
    AddressType,
    ArrayType,
    BoolType,
    BytesType,
    DataLocation,
    FixedBytesType,
    IntType,
    PointerType,
    StringType
} from "solc-typed-ast";

export const bool = new BoolType();
export const address = new AddressType(false);
export const bytes1 = new FixedBytesType(1);
export const bytes2 = new FixedBytesType(2);
export const bytes3 = new FixedBytesType(3);
export const bytes4 = new FixedBytesType(4);
export const bytes5 = new FixedBytesType(5);
export const bytes21 = new FixedBytesType(21);
export const bytes32 = new FixedBytesType(32);

export const uint8 = new IntType(8, false);
export const uint16 = new IntType(16, false);
export const uint24 = new IntType(24, false);
export const uint64 = new IntType(64, false);

export const int8 = new IntType(8, true);
export const int16 = new IntType(16, true);
export const int32 = new IntType(32, true);
export const int48 = new IntType(48, true);
export const int128 = new IntType(128, true);
export const int256 = new IntType(256, true);

export const bytes = new BytesType();
export const bytesCalldata = new PointerType(bytes, DataLocation.CallData);

export const string = new StringType();
export const stringCalldata = new PointerType(string, DataLocation.CallData);

export const int16Arr = new PointerType(new ArrayType(int16), DataLocation.CallData);
export const bytes22DArr = new PointerType(
    new ArrayType(new PointerType(new ArrayType(bytes2), DataLocation.CallData)),
    DataLocation.CallData
);
export const int8x4 = new PointerType(new ArrayType(int8, 4n), DataLocation.CallData);
export const int8x4x2 = new PointerType(
    new ArrayType(new PointerType(new ArrayType(int8, 4n), DataLocation.CallData), 2n),
    DataLocation.CallData
);

export const int8xNx2 = new PointerType(
    new ArrayType(new PointerType(new ArrayType(int8), DataLocation.CallData), 2n),
    DataLocation.CallData
);
export const int8x4xN = new PointerType(
    new ArrayType(new PointerType(new ArrayType(int8, 4n), DataLocation.CallData)),
    DataLocation.CallData
);
