module Adacola.FsQuadKey

(*
参考サイト
http://d.hatena.ne.jp/maachang/20150404/1428139492
http://msdn.microsoft.com/en-us/library/bb259689.aspx
*)

open System

[<Struct>]
type QuadKey = QuadKey of int64

[<Measure>]
type private deg

let private maxDetail = 23
let private sinLatPi = Math.PI / 180.0<deg>
let private pi2 = Math.PI * 2.0
let private pi4 = Math.PI * 4.0

let private detailToMapSize detail = 256L <<< detail |> float

let private xyToQuadKey detail x y =
    ((0L, detail <<< 1), seq { detail .. -1 .. 1 }) ||> Seq.fold (fun (ret, cnt) i ->
        let n = i - 1
        let mask = 1 <<< n
        let cnt = cnt - 2
        let ret = ret ||| (int64 (((x &&& mask) >>> n) + (((y &&& mask) >>> n) <<< 1)) <<< cnt)
        ret, cnt)
    |> fst |> QuadKey

/// 緯度経度からQuadKeyを計算
let private latLonToQuadKey detail (latitude : float<deg>) (longitude : float<deg>) =
    let sLat = sin (latitude * sinLatPi)
    let x = (longitude + 180.0<deg>) / 360.0<deg>
    let y = 0.5 - log ((1.0 + sLat) / (1.0 - sLat)) / pi4
    let mapSize = detailToMapSize detail
    let xx = int (x * mapSize + 0.5) >>> 8
    let yy = int (x * mapSize + 0.5) >>> 8
    xyToQuadKey xx yy

/// QuadKeyから緯度経度を計算
let private quadKeyToLatLon detail (QuadKey quadKey) =
    let xx, yy, _ =
        ((0, 0, detail <<< 1), seq { detail .. -1 .. 1}) ||> Seq.fold (fun (xx, yy, cnt) i ->
            let cnt = cnt - 2
            let mask = 1 <<< (i - 1)
            let xx, yy =
                match int ((quadKey &&& (3L <<< cnt)) >>> cnt) with
                | 1 -> xx ||| mask, yy
                | 2 -> xx, yy ||| mask
                | 3 -> xx ||| mask, yy ||| mask
                | _ -> xx, yy
            xx, yy, cnt)
    let mapSize = detailToMapSize detail
    let x = float (xx <<< 8) / mapSize - 0.5
    let y = 0.5 - (float (yy <<< 8) / mapSize)
    let latitude = 90.0<deg> - 360.0<deg> * atan (exp (-y * pi2)) / Math.PI
    let longitude = 360.0<deg> * x
    latitude, longitude

// TODO