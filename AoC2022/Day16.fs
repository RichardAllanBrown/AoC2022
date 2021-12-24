module Day16

type Bits = char list

module Bits =
    let toString(bits: Bits): string =
        bits
        |> List.toArray 
        |> fun c -> new System.String(c)

    let toInt(bits: Bits): int =
        System.Convert.ToInt32(toString(bits), 2)

    let parseFromHex(s: string): Bits =
        s.ToCharArray() 
        |> Array.map(fun c -> System.Convert.ToInt32(c.ToString(), 16))
        |> Array.map(fun c -> System.Convert.ToString(c, 2))
        |> Array.collect(fun c -> c.ToCharArray())
        |> Array.toList

type IPacket = interface end
type Literal = { version: int; value: int } interface IPacket
type Operator = { version: int; packets: IPacket list } interface IPacket
type Packet = 
    | Literal of Literal
    | Operator of Operator

module PacketF =
    let literal(bits: Bits) =        
        let version = bits |> List.take(3) |> Bits.toInt
        let limit(b: Bits list): Bits list =
            let lastPacketIndex = b |> List.findIndex(fun s -> List.head(s) = '0')
            b |> List.take(lastPacketIndex + 1)
        let value = 
            bits 
            |> List.skip(6) 
            |> List.chunkBySize(5)
            |> limit
            |> List.collect id
            |> Bits.toInt
        { version = version; value = value }

    let operator(bits: Bits) = 
        let version = bits |> List.take(3) |> Bits.toInt
        { version = version; packets = List.empty }

let (|Literal|_|) (bits: Bits) =
    let typeId = bits |> List.skip(3) |> List.take(3) |> Bits.toInt
    if (typeId = 4) then PacketF.literal(bits) |> Some else None

let (|Operator|_|) (bits: Bits) =
    let typeId = bits |> List.skip(3) |> List.take(3) |> Bits.toInt
    if (typeId = 4) then None else PacketF.operator(bits) |> Some

let decode(bits: Bits) = 
    match bits with
    | Literal l -> l.version
    | Operator o -> o.version
    | _ -> 0