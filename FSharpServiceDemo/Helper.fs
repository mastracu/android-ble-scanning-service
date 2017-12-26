
module helper

open System

let epoch2timestamp (millisec:int64) = 
    let startTime = new DateTime (1970,1,1)
    let time = TimeSpan.FromMilliseconds (float millisec) 
    (startTime.Add time).ToLocalTime().ToLongTimeString()   

let isValidBeaconAddress (candidateBeaconAddr:string) =
    if candidateBeaconAddr.Length <> 17 then
       false
    else
       let hexDigits = ['0'..'9'] @ ['a'..'f'] @ ['A'..'F']  
       let separatorList = [for i in [0..4] do yield candidateBeaconAddr.[3*i+2] ]
       let hexDigitList = [for i in [0..5] do yield! candidateBeaconAddr.[3*i..3*i+1]]
       List.forall (fun c -> c = ':') separatorList && List.forall (fun c -> List.exists (fun elem -> elem = c) hexDigits) hexDigitList

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
