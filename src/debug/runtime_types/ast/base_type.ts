/**
 * Base class for runtime types. Runtime Types are a more convenient
 * set of types for representing runtime values in memory/storage/stack than raw Solidity types.
 *
 * They replace high-level types with their simplified lower-level version:
 *   - contracts -> address
 *   - enums -> int
 *   - user-defined value types -> underyling value type
 *   - user-defined structs -> structs with fields expanded
 *
 * Also they explicitly account for missing typing information.
 */
export abstract class BaseRuntimeType {
    abstract pp(): string;
}
