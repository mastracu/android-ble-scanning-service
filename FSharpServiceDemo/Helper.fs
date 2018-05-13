
module helper

open System
open Android.Content
open Android.App
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
open System.Text
open System.IO
open System.Net
open System
open FSharp.Data.HttpRequestHeaders
open FSharp.Data.HttpContentTypes

let buildRequestLocationingPermissionsDialog (ctx:Activity) (dialog:AlertDialog.Builder) =
        dialog.SetMessage("Locationing permissions are currently not granted to the application")
            .SetPositiveButton("Request Permissions" , new EventHandler<DialogClickEventArgs> (fun s dArgs -> 
                ctx.RequestPermissions ([|Android.Manifest.Permission.AccessCoarseLocation; 
                                            Android.Manifest.Permission.AccessFineLocation|], 0)
                ) )
            .SetNegativeButton("Cancel" , new EventHandler<DialogClickEventArgs> (fun s dArgs -> ()) )

// http://techdocs.zebra.com/datawedge/6-5/guide/settings/
let Asset2DWAutoImport  filename path (context:Context) =
    let fromStream = context.Assets.Open filename
    let toFileStream =  File.Create (path + filename)
    // I create the file - RW for owner only, not visibile to DW
    do fromStream.CopyTo toFileStream
    do toFileStream.Close ()
    do fromStream.Close ()
    // once it is copied, I give RW access to everyone in order for DW to process it and then remove it.  
    // let javaFile =  new Java.IO.FileOutputStream (path + filename)
    let javaFile = new Java.IO.File (path + filename)
    do javaFile.SetWritable (true,false) |> ignore
    do javaFile.SetReadable (true,false) |> ignore


//http://fsharp.github.io/FSharp.Data/library/Http.html
//data sent in JSON format
//https://stackoverflow.com/questions/16652014/async-exception-handling-in-f
let AsyncSendRegionNotification url jsonData cont1 cont2 =
    let asyncWorkflowSend =  
       async {  try
                    let! completor1 =  Async.StartChild (FSharp.Data.Http.AsyncRequestString (url, 
                                                            headers = [ ContentType Json ], 
                                                            body = FSharp.Data.TextRequest jsonData), millisecondsTimeout = 5000)
                    let! result1 = completor1
                    return (cont1 result1)
                with error -> 
                    return (cont2 error) }       
    asyncWorkflowSend |> Async.Start 

type RegionEventType = 
 | EnterRegion
 | ExitRegion

type RegionEvent =
   { 
      regionName : string;
      deviceID : string;
      timeStamp : int64;
      eventType : RegionEventType;
   }

//I could have used JSON.net library, but it's got dozens of dependencies - a bit of an overkill for what I need to do.
let serialize regionName deviceID (timeStamp:int64) eventType = 
    let regionEventIdx = match eventType with 
                          | EnterRegion -> 0
                          | ExitRegion -> 1
    sprintf """{ "regionName":"%s", "deviceID":"%s", "timeStamp":%i, "eventType":{"_tag":%d} }""" regionName deviceID timeStamp regionEventIdx



let epoch2timestamp (millisec:int64) = 
    let startTime = new DateTime (1970,1,1)
    let time = TimeSpan.FromMilliseconds (float millisec) 
    ((startTime.Add time).ToLocalTime()).ToString("hh:mm:ss.fff")

let convertmsecstoHumanReadeable (msecs:int64) =
    let ms = msecs % 1000L
    let seconds = msecs / 1000L
    sprintf "%i.%i sec" seconds ms

let isValidBeaconAddress (candidateBeaconAddr:string) =
    if candidateBeaconAddr.Length <> 17 then
       false
    else
       let hexDigits = ['0'..'9'] @ ['a'..'f'] @ ['A'..'F']  
       let separatorList = [for i in [0..4] do yield candidateBeaconAddr.[3*i+2] ]
       let hexDigitList = [for i in [0..5] do yield! candidateBeaconAddr.[3*i..3*i+1]]
       List.forall (fun c -> c = ':') separatorList && List.forall (fun c -> List.exists (fun elem -> elem = c) hexDigits) hexDigitList

[<StructuredFormatDisplay("{PrettyPrint}")>]
type BeaconObservation = 
   {ObservationTimestamp:int64; BeaconAddress:string; SignalStrength: int; DeviceName: string} 
   member x.PrettyPrint = sprintf "%s\t%s\t%d" (epoch2timestamp x.ObservationTimestamp) x.BeaconAddress  x.SignalStrength
   member x.SendBroadcast (cont:Context) action =
       let int = new Intent ()
       do  int.SetAction action |> ignore
       do  int.PutExtra (action + ".ObservationTimestamp", x.ObservationTimestamp) |> ignore
       do  int.PutExtra (action + ".BeaconAddress", x.BeaconAddress) |> ignore
       do  int.PutExtra (action + ".SignalStrength", x.SignalStrength) |> ignore
       do  cont.SendBroadcast int

[<StructuredFormatDisplay("{PrettyPrint}")>]
type BeaconStatistics = 
   {Address: string; LastSeen:int64; MaxRSSI:int; BiggestGap:int64; Count:int64 }
   member x.PrettyPrint = sprintf "\nAddress = %s\nLastSeen = %s\nMaxRSSI = %d\nBiggestGap = %s\nCount = %i\n"
                                     x.Address (epoch2timestamp x.LastSeen) x.MaxRSSI (convertmsecstoHumanReadeable x.BiggestGap) x.Count

let rec ListwIncrCountElement (bo:BeaconObservation) (bsl:BeaconStatistics list) =
    match bsl with 
    | [] -> []
    | a :: xs -> if (a.Address = bo.BeaconAddress) then 
                    {Address = a.Address
                     LastSeen = bo.ObservationTimestamp
                     MaxRSSI = max bo.SignalStrength a.MaxRSSI
                     BiggestGap = max a.BiggestGap (bo.ObservationTimestamp - a.LastSeen)
                     Count = a.Count + 1L } :: xs
                 else
                    a :: (ListwIncrCountElement bo xs) 

let rec gapsFinal (bsl:BeaconStatistics list) now =
    match bsl with 
    | [] -> []
    | a :: xs ->  
                   {Address = a.Address
                    LastSeen = a.LastSeen
                    MaxRSSI = a.MaxRSSI
                    BiggestGap = max a.BiggestGap (now - a.LastSeen)
                    Count = a.Count } :: gapsFinal xs now

[<StructuredFormatDisplay("{PrettyPrint}")>]
type SessionStatistics = 
   {SessionStart:int64 option; SessionEnd:int64 option; BeaconList:BeaconStatistics list}
   static member Empty = {SessionStart = None; SessionEnd = None; BeaconList = [] }
   member x.IsKnownBeacon address = 
      List.exists (fun beacStat -> beacStat.Address = address) x.BeaconList
   member x.UpdateWith (bo:BeaconObservation) = 
      { 
      SessionEnd = x.SessionEnd
      SessionStart = match x.SessionStart with
                     | None -> Some bo.ObservationTimestamp
                     | Some t -> Some t
      BeaconList = if x.IsKnownBeacon bo.BeaconAddress then
                      ListwIncrCountElement bo x.BeaconList
                   else
                      {Address = bo.BeaconAddress; LastSeen = bo.ObservationTimestamp 
                       MaxRSSI = bo.SignalStrength; BiggestGap=0L; Count=1L} :: x.BeaconList}
   member x.PrettyPrint = 
      match x.SessionStart with | None -> "" | Some t -> sprintf "Session Start = %s\n" (epoch2timestamp t)
      + match x.SessionEnd with | None -> "" | Some t -> sprintf "Session End = %s\n" (epoch2timestamp t)
      + List.fold (fun s bs -> s + sprintf "%A" bs) "" x.BeaconList


// MailboxProcessor class 
// from https://en.wikibooks.org/wiki/F_Sharp_Programming/MailboxProcessor

type SessionStatisticMsg = 
    | Exit
    | Empty
    | CloseSession of Int64
    | UpdateWith of BeaconObservation
    | IsKnownBeacon of String * AsyncReplyChannel<Boolean>
    | FetchReport of AsyncReplyChannel<String>

type SessionStatisticsProcessor() =
    let sessionStatisticsMailboxProcessor =
        MailboxProcessor.Start(fun inbox ->
            let rec sessionStatisticsLoop sessionStatisticsInstance =
                async { let! msg = inbox.Receive()
                        match msg with
                        | Exit -> return ()
                        | Empty -> return! sessionStatisticsLoop SessionStatistics.Empty
                        | CloseSession now -> 
                           return! sessionStatisticsLoop 
                              {SessionEnd = Some now
                               SessionStart = sessionStatisticsInstance.SessionStart
                               BeaconList = gapsFinal sessionStatisticsInstance.BeaconList now} 
                        | UpdateWith bo -> return! sessionStatisticsLoop (sessionStatisticsInstance.UpdateWith bo)
                        | IsKnownBeacon (addr, replyChannel) -> 
                            replyChannel.Reply (sessionStatisticsInstance.IsKnownBeacon addr)
                            return! sessionStatisticsLoop sessionStatisticsInstance
                        | FetchReport replyChannel ->
                            replyChannel.Reply (sprintf "%A" sessionStatisticsInstance)
                            return! sessionStatisticsLoop sessionStatisticsInstance 
                      }
            sessionStatisticsLoop SessionStatistics.Empty
        )
    member this.Exit() = sessionStatisticsMailboxProcessor.Post(Exit)
    member this.Empty() = sessionStatisticsMailboxProcessor.Post(Empty)
    member this.UpdateWith bo = sessionStatisticsMailboxProcessor.Post(UpdateWith bo)
    member this.IsKnownBeacon addr = sessionStatisticsMailboxProcessor.PostAndReply((fun reply -> IsKnownBeacon(addr,reply)), timeout = 2000)
    member this.FetchReport() = sessionStatisticsMailboxProcessor.PostAndReply((fun reply -> FetchReport(reply)), timeout = 2000)
    member this.CloseSession now = sessionStatisticsMailboxProcessor.Post(CloseSession now)
    

let DELAY_BETWEEN_LOG_MESSAGES = 5000
let SERVICE_RUNNING_NOTIFICATION_ID = 10000
let SERVICE_STARTED_KEY = "has_service_been_started"
let BROADCAST_MESSAGE_KEY = "broadcast_message";
let NOTIFICATION_BROADCAST_ACTION = "BLEService.Notification.Action"
let ACTION_START_SERVICE = "BLEService.action.START_SERVICE"
let ACTION_STOP_SERVICE = "BLEService.action.STOP_SERVICE"
let ACTION_MUTE_SERVICE = "BLEService.action.MUTE_SERVICE"
let ACTION_UNMUTE_SERVICE = "BLEService.action.UNMUTE_SERVICE"
let ACTION_MAIN_ACTIVITY = "BLEService.action.MAIN_ACTIVITY"
let ACTION_RSSI_REPORT = "BLEService.action.RSSI_REPORT"
let ACTION_COUNT_REPORT = "BLEService.action.COUNT_REPORT"
let EXTRA_SERVICE_LANGUAGE = "BLEService.extra.SERVICE_LANGUAGE"
let EXTRA_SERVICE_FILTER = "BLEService.extra.SERVICE_FILTER"
let EXTRA_SERVICE_SCANMODE = "BLEService.extra.SERVICE_SCANMODE"
let EXTRA_SERVICE_REGIONTRACKING = "BLEService.extra.SERVICE_REGIONTRACKING"
let EXTRA_SERVICE_RSSITHRESHOLD = "BLEService.extra.SERVICE_RSSITHRESHOLD"
let EXTRA_SERVICE_REGION_TIMEOUT = "BLEService.extra.SERVICE_REGION_TIMEOUT"
let EXTRA_SERVICE_NOTIFICATION = "BLEService.extra.SERVICE_NOTIFICATION"
let EXTRA_SERVICE_NOTIFICATION_URL = "BLEService.extra.SERVICE_NOTIFICATION_URL"
let EXTRA_SERVICE_BEACON_INTENT = "BLEService.extra.SERVICE_BEACON_INTENT"

let REGION_NOTIFICATION_URL = "http://www.mastracu.it/regionEvent"

