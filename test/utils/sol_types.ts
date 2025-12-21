import {
    AddressTypeId,
    ArrayTypeId,
    BoolTypeId,
    BytesTypeId,
    DataLocation,
    FixedBytesTypeId,
    IntTypeId,
    PointerTypeId,
    StringTypeId
} from "solc-typed-ast";

export const bool = new BoolTypeId();
export const address = new AddressTypeId(false);
export const bytes1 = new FixedBytesTypeId(1);
export const bytes2 = new FixedBytesTypeId(2);
export const bytes3 = new FixedBytesTypeId(3);
export const bytes4 = new FixedBytesTypeId(4);
export const bytes5 = new FixedBytesTypeId(5);
export const bytes21 = new FixedBytesTypeId(21);
export const bytes32 = new FixedBytesTypeId(32);

export const uint8 = new IntTypeId(8, false);
export const uint16 = new IntTypeId(16, false);
export const uint24 = new IntTypeId(24, false);
export const uint64 = new IntTypeId(64, false);
export const uint112 = new IntTypeId(112, false);
export const uint120 = new IntTypeId(120, false);
export const uint128 = new IntTypeId(128, false);
export const uint136 = new IntTypeId(136, false);
export const uint144 = new IntTypeId(144, false);
export const uint248 = new IntTypeId(248, false);
export const uint256 = new IntTypeId(256, false);

export const int8 = new IntTypeId(8, true);
export const int16 = new IntTypeId(16, true);
export const int32 = new IntTypeId(32, true);
export const int48 = new IntTypeId(48, true);
export const int120 = new IntTypeId(120, true);
export const int128 = new IntTypeId(128, true);
export const int136 = new IntTypeId(136, true);
export const int256 = new IntTypeId(256, true);

export const bytes = new BytesTypeId();
export const bytesCalldata = new PointerTypeId(bytes, DataLocation.CallData, true);

export const string = new StringTypeId();
export const stringCalldata = new PointerTypeId(string, DataLocation.CallData, true);

export const int16Arr = new PointerTypeId(new ArrayTypeId(int16), DataLocation.CallData, true);
export const bytes22DArr = new PointerTypeId(
    new ArrayTypeId(new PointerTypeId(new ArrayTypeId(bytes2), DataLocation.CallData, true)),
    DataLocation.CallData,
    true
);
export const int8x4 = new PointerTypeId(new ArrayTypeId(int8, 4n), DataLocation.CallData, true);
export const int8x4x2 = new PointerTypeId(
    new ArrayTypeId(new PointerTypeId(new ArrayTypeId(int8, 4n), DataLocation.CallData, true), 2n),
    DataLocation.CallData,
    true
);

export const int8xNx2 = new PointerTypeId(
    new ArrayTypeId(new PointerTypeId(new ArrayTypeId(int8), DataLocation.CallData, true), 2n),
    DataLocation.CallData,
    true
);
export const int8x4xN = new PointerTypeId(
    new ArrayTypeId(new PointerTypeId(new ArrayTypeId(int8, 4n), DataLocation.CallData, true)),
    DataLocation.CallData,
    true
);
