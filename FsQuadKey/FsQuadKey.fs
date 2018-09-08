module Adacola.FsQuadKey

(*
参考サイト
http://d.hatena.ne.jp/maachang/20150404/1428139492
http://msdn.microsoft.com/en-us/library/bb259689.aspx
*)

open System
open FSharp.Data.UnitSystems.SI.UnitSymbols

[<Measure>]
type deg

[<Struct>]
type QuadKey = QuadKey of int64

let private minDetail = 1
let private maxDetail = 23
let private sinLatPi = Math.PI / 180.0<deg>
let private pi2 = Math.PI * 2.0
let private pi4 = Math.PI * 4.0
let private distanceTable : int<m>[] =
    [5; 10; 19; 39; 77; 153; 306; 612; 1223; 2446; 4892; 9784; 19568; 39136; 78272; 156543; 313086; 626172; 1252344; 2504689; 5009377; 10018754]
    |> Seq.map LanguagePrimitives.Int32WithMeasure |> Seq.toArray

[<Struct>]
type private Detail = Detail of int
let private validateDetail detail = if minDetail <= detail && detail <= maxDetail then Detail detail else sprintf "[%d, %d]の範囲で指定してください" minDetail maxDetail |> invalidArg "detail"

[<Struct>]
type private Latitude = Latitude of float<deg>
let private validateLatitude latitude = if -90.0<deg> <= latitude && latitude <= 90.0<deg> then Latitude latitude else invalidArg "latitude" "[-90, 90]の範囲で指定してください"

[<Struct>]
type private Longitude = Longitude of float<deg>
let private validateLongitude longitude = if -180.0<deg> <= longitude && longitude <= 180.0<deg> then Longitude longitude else invalidArg "longitude" "[-180, 180]の範囲で指定してください"

let private detailToMapSize detail = 256L <<< detail |> float

let private latLonToXy (Detail detail) (Latitude latitude) (Longitude longitude) =
    let sLat = sin (latitude * sinLatPi)
    let x = (longitude + 180.0<deg>) / 360.0<deg>
    let y = 0.5 - log ((1.0 + sLat) / (1.0 - sLat)) / pi4
    let mapSize = detailToMapSize detail
    let f z = int (z * mapSize + 0.5) >>> 8
    f x, f y

let private xyToLatLon (Detail detail) x y =
    let mapSize = detailToMapSize detail
    let x' = float (x <<< 8) / mapSize - 0.5
    let y' = 0.5 - (float (y <<< 8) / mapSize)
    let latitude = 90.0<deg> - 360.0<deg> * atan (exp (-y' * pi2)) / Math.PI
    let longitude = 360.0<deg> * x'
    Latitude latitude, Longitude longitude

let private xyToQuadKey (Detail detail) x y =
    ((0L, detail <<< 1), seq { detail .. -1 .. 1 }) ||> Seq.fold (fun (ret, cnt) i ->
        let n = i - 1
        let mask = 1 <<< n
        let cnt = cnt - 2
        let ret = ret ||| (int64 (((x &&& mask) >>> n) + (((y &&& mask) >>> n) <<< 1)) <<< cnt)
        ret, cnt)
    |> fst |> QuadKey

let private quadKeyToXy (Detail detail) (QuadKey key) =
    (((0, 0), detail <<< 1), seq { detail .. -1 .. 1}) ||> Seq.fold (fun ((x, y), cnt) i ->
        let cnt = cnt - 2
        let mask = 1 <<< (i - 1)
        let x, y =
            match int ((key &&& (3L <<< cnt)) >>> cnt) with
            | 1 -> x ||| mask, y
            | 2 -> x, y ||| mask
            | 3 -> x ||| mask, y ||| mask
            | _ -> x, y
        (x, y), cnt)
    |> fst

/// 緯度経度からQuadKeyを計算
let private latLonToQuadKey detail latitude longitude =
    latLonToXy detail latitude longitude ||> xyToQuadKey detail

/// QuadKeyから緯度経度を計算
let private quadKeyToLatLon detail quadKey =
    quadKeyToXy detail quadKey ||> xyToLatLon detail

let private quadKeyToMinMaxQuadKey (Detail detail) (QuadKey key) =
    let shift = (maxDetail - detail) <<< 1
    let minKey = key <<< shift
    let maxKey = if detail = maxDetail then minKey else minKey ||| (0x7fffffffffffffffL &&& ((1L <<< shift) - 1L))
    QuadKey minKey, QuadKey maxKey

/// 範囲検索用データの作成
let private searchCode detail latitude longitude =
    let xyToMinMaxQuadKey x y = xyToQuadKey detail x y |> quadKeyToMinMaxQuadKey detail
    let x, y = latLonToXy detail latitude longitude
    [for dx in -1 .. 1 do for dy in -1 .. 1 do yield xyToMinMaxQuadKey (x + dx) (y + dy)]

let private endKey ((Detail det) as detail) quadKey =
    if det = maxDetail then quadKey else quadKeyToMinMaxQuadKey detail quadKey |> snd

let private getDetail maybeDetail maybeDistance =
    maybeDetail |> Option.map validateDetail |> Option.defaultWith (fun() ->
        maybeDistance |> Option.map (fun distance ->
            let binarySearchResult = Array.BinarySearch(distanceTable, distance)
            let index = if binarySearchResult < 0 then ~~~binarySearchResult else binarySearchResult
            distanceTable.Length - index + 1)
        |> Option.defaultValue maxDetail |> Detail)

type QuadKey with
    /// 指定距離からdetailを取得
    static member distanceToDetail distance =
        let (Detail detail) = getDetail None (Some distance)
        detail

    /// 緯度経度からQuadKeyを計算
    static member ofLatLon(latitude, longitude, ?detail, ?distance) =
        let latitude = latitude |> validateLatitude
        let longitude = longitude |> validateLongitude
        let detail = getDetail detail distance
        latLonToQuadKey detail latitude longitude

    /// QuadKeyから緯度経度を計算
    static member toLatLon quadKey =
        let (Latitude lat), (Longitude lon) = quadKeyToLatLon (Detail maxDetail) quadKey
        lat, lon

    /// QuadKeyの終端キーを取得
    static member getEndKey(quadKey, ?detail, ?distance) =
        let detail = getDetail detail distance
        endKey detail quadKey

    /// 範囲検索用のデータを作成
    static member getSearchCode(latitude, longitude, ?detail, ?distance) =
        let latitude = latitude |> validateLatitude
        let longitude = longitude |> validateLongitude
        let detail = getDetail detail distance
        searchCode detail latitude longitude
