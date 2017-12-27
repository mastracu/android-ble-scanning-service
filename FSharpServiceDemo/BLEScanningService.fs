namespace BleScanningService

open System
open Android.App
open Android.Util
open Android.Content
open Android.OS
open Android.Bluetooth
open Android.Speech.Tts
open Android.Support.V4


// https://github.com/xamarin/samples-mnoodroid/tree/master/ApplicationFundamentals/ServiceSamples/ForegroundServiceDemo

type Resources = FSharpServiceDemo.Resource

// TODO: best practice for multi-language resources/apps in Android
// TODO: work-around Nougat 30 minutes BLE scanning timeout

// For future use - currently not used
// type BeaconCount = { addr: string; count:int}

type BeaconObservation = 
   {ObservationTimestamp:int64; BeaconAddress:string; SignalStrength: int; DeviceName: string} 
   override x.ToString() = 
      helper.epoch2timestamp x.ObservationTimestamp + " " + x.BeaconAddress  + "  " + x.DeviceName + " @ " + x.SignalStrength.ToString()
   member x.SendBroadcast (cont:Context) action extraName =
       let int = new Intent ()
       do  int.SetAction action |> ignore
       do  int.PutExtra (extraName, x.ToString()) |> ignore
       do  cont.SendBroadcast int

type UMScanCallback (e: Event<BeaconObservation>) =
   inherit LE.ScanCallback ()
   let reportResult (sResult:LE.ScanResult) = 
      let newObservation:BeaconObservation = 
         { ObservationTimestamp = Java.Lang.JavaSystem.CurrentTimeMillis(); BeaconAddress = sResult.Device.Address 
           SignalStrength =  sResult.Rssi; DeviceName = sResult.ScanRecord.DeviceName }
      e.Trigger newObservation
   override this.OnScanResult (cbType, sResult) = do sResult |> reportResult
   override this.OnBatchScanResults (sResults) =  do sResults |> Seq.iter reportResult
   override this.OnScanFailed errorCode = do Log.Debug ("UMBLE", "LE Scan Failed") |> ignore


[<Service>]
[<IntentFilter [|"BLEService.action.START_SERVICE"; "BLEService.action.STOP_SERVICE"; 
                 "BLEService.action.MUTE_SERVICE"; "BLEService.action.UNMUTE_SERVICE";
                 "BLEService.action.RSSI_REPORT"; "BLEService.action.COUNT_REPORT"|] >]
type BleScanningService() =
   inherit Service()

   static let TAG = typeof<BleScanningService>.FullName

   let mutable isStarted = false
   let mutable muted = false
   let mutable isRssiReport = false
   let mutable mBluetoothAdapter = Unchecked.defaultof<BluetoothAdapter>
   let mutable mBLuetoothLeScanner = Unchecked.defaultof<LE.BluetoothLeScanner>
   
   let mutable ObservationsCountInPeriod = 0
   let mutable perBeaconCountInPeriod = Array.zeroCreate 64
   let mutable beaconCountInPeriod = 0
   let mutable totalRssiInPeriod = 0
   let mutable topRssiInPeriod = -150
   let mutable topRssiBeaconInPeriod = Unchecked.defaultof<string>
   let mutable localeForSpeech = Unchecked.defaultof<Java.Util.Locale>
   let mutable beacon2Observe = Unchecked.defaultof<String seq>
   let mutable notificationAction1 = Unchecked.defaultof<App.NotificationCompat.Action>
   let mutable notificationAction2 = Unchecked.defaultof<App.NotificationCompat.Action>
   let mutable bleScanMode = Unchecked.defaultof<LE.ScanMode>
   let mutable regionTrackingMode = Unchecked.defaultof<Boolean>
   let mutable rssiThreshold = Unchecked.defaultof<int>
   let updateTimer = new System.Timers.Timer(float 10000)
   let regionTimeoutTimer = new System.Timers.Timer(float 5000)
   let mutable textToSpeech = Unchecked.defaultof<TextToSpeech>
   let mutable regionAddress = Unchecked.defaultof<Option<string>>

   let eventObservation = new Event<BeaconObservation> ()
   let mBLuetoothLeCallback = new UMScanCallback(eventObservation)
   

   // This interface needs to be implemented for TTS
   interface TextToSpeech.IOnInitListener with
       member this.OnInit status =
           if status = OperationResult.Success then
               do textToSpeech.SetLanguage localeForSpeech |> ignore
           else
               ()
           end   

   member this.obsAction bo = 
        if (Seq.length beacon2Observe = 0 || (Seq.exists (fun e -> e = bo.BeaconAddress) beacon2Observe) )  then
            if regionTrackingMode then 
                let enterRegionifAboveThreshold () =
                    if bo.SignalStrength > rssiThreshold then
                        // entro in nuova regione
                        do regionAddress <- Some bo.BeaconAddress
                        do this.RefreshNotification ()
                        do regionTimeoutTimer.Stop()
                        do regionTimeoutTimer.Start()
                        
                        if not muted then
                           use param = new Bundle ()
                           do param.PutString (TextToSpeech.Engine.KeyParamUtteranceId, "")
                           if localeForSpeech = Java.Util.Locale.Italy then 
                              do textToSpeech.Speak ("Regione " + string bo.BeaconAddress.[bo.BeaconAddress.Length - 2] + " " +
                                   string bo.BeaconAddress.[bo.BeaconAddress.Length - 1], QueueMode.Add, param, "UniqueID") |> ignore
                           elif localeForSpeech = Java.Util.Locale.Uk then
                              do textToSpeech.Speak ("Region " + string bo.BeaconAddress.[bo.BeaconAddress.Length - 2] + " " +
                                   string bo.BeaconAddress.[bo.BeaconAddress.Length - 1], QueueMode.Add, param, "UniqueID") |> ignore
                           else
                              do textToSpeech.Speak ("Región " + string bo.BeaconAddress.[bo.BeaconAddress.Length - 2] + " " +
                                   string bo.BeaconAddress.[bo.BeaconAddress.Length - 1], QueueMode.Add, param, "UniqueID") |> ignore                                                         
                match regionAddress with
                | None -> 
                        enterRegionifAboveThreshold ()
                | Some regAdd -> 
                        if bo.BeaconAddress = regAdd then
                            do regionTimeoutTimer.Stop()
                            do regionTimeoutTimer.Start()                                                    
                        else
                            enterRegionifAboveThreshold ()
            else
                if (bo.SignalStrength > topRssiInPeriod) then
                    topRssiBeaconInPeriod <- bo.BeaconAddress
                    topRssiInPeriod <- bo.SignalStrength
                else
                    ()
                do ObservationsCountInPeriod <-(ObservationsCountInPeriod + 1)
                let beaconIndex = Array.FindIndex (perBeaconCountInPeriod, (fun e -> bo.BeaconAddress = e) )
                if (beaconIndex = -1) then
                    Array.set perBeaconCountInPeriod beaconCountInPeriod bo.BeaconAddress 
                    do beaconCountInPeriod <- beaconCountInPeriod + 1
                else
                    ()
            do bo.SendBroadcast this "com.zebra.newBLEObservation" "com.zebra.newBLEObservation.ToString"
        else
            () 

   member this.pararametersReport () =
      match bleScanMode with 
      | LE.ScanMode.LowLatency -> "LowLatency scanning\n" 
      | LE.ScanMode.LowPower -> "LowPower scanning\n" 
      | _ -> "Unknown scanning mode " 
      +
      if regionTrackingMode then
         "Region mode ON : threshold " + rssiThreshold.ToString() + "\nRegion Timeout : " + (regionTimeoutTimer.Interval).ToString() + " msec" +
            match regionAddress with
            | None -> "\nOut of region\n"
            | Some reg -> "\nRegion " +  reg + "\n"
      else
         "Region mode OFF\n" + if isRssiReport then "RSSI " else "COUNT " 
          + "report mode Freq 10 secs" 

   member this.notificationBuilder = 
   // TODO: read and understand https://developer.android.com/training/notify-user/expanded.html
   // TODO: https://developer.android.com/guide/topics/ui/notifiers/notifications.html#BigNotify
      (new App.NotificationCompat.Builder(this))
                            .SetStyle(
                               (new App.NotificationCompat.BigTextStyle())
                                  .BigText(this.pararametersReport ())
                             ) 
                            .SetContentTitle(this.GetString(Resources.String.app_name))
                            .SetContentText(this.GetString(Resources.String.notification_text))
                            .SetSmallIcon(Resources.Drawable.ic_stat_tap_and_play)
                            .SetContentIntent(this.BuildIntentToShowMainActivity())
                            .SetOngoing(true)

   member this.BuildIntentToShowMainActivity() =
      let notificationIntent = (new Intent (this, typeof<FSharpServiceDemo.MainActivity>))
                                   .SetAction(helper.ACTION_MAIN_ACTIVITY)
                                   .SetFlags(ActivityFlags.SingleTop ||| ActivityFlags.ClearTask)
                                   .PutExtra(helper.SERVICE_STARTED_KEY, true);

      PendingIntent.GetActivity(this, 0, notificationIntent, PendingIntentFlags.UpdateCurrent);

   member this.BuildMuteBlescanAction =
      let muteIntent = (new Intent(this, this.GetType()))
                                   .SetAction(helper.ACTION_MUTE_SERVICE)
      let mutePendingIntent = PendingIntent.GetService(this, 0, muteIntent, PendingIntentFlags.UpdateCurrent)
      let builder = new App.NotificationCompat.Action.Builder (Resources.Drawable.ic_stat_volume_off, 
                                                        this.GetString(Resources.String.mute_blescan_service_button_text), 
                                                        mutePendingIntent)
      builder.Build()

   member this.BuildUnmuteBlescanAction =
      let unmuteIntent = (new Intent(this, this.GetType()))
                                   .SetAction(helper.ACTION_UNMUTE_SERVICE)
      let unmutePendingIntent = PendingIntent.GetService(this, 0, unmuteIntent, PendingIntentFlags.UpdateCurrent)
      let builder = new App.NotificationCompat.Action.Builder (Resources.Drawable.ic_stat_volume_up, 
                                                        this.GetString(Resources.String.unmute_blescan_service_button_text), 
                                                        unmutePendingIntent)
      builder.Build()

   member this.BuildRssiBlescanAction =
      let rssiIntent = (new Intent(this, this.GetType()))
                                   .SetAction(helper.ACTION_RSSI_REPORT)
      let rssiPendingIntent = PendingIntent.GetService(this, 0, rssiIntent, PendingIntentFlags.UpdateCurrent)
      let builder = new App.NotificationCompat.Action.Builder (Resources.Drawable.restart, 
                                                        this.GetString(Resources.String.rssi_blescan_service_button_text), 
                                                        rssiPendingIntent)
      builder.Build()

   member this.BuildCountBlescanAction =
      let countIntent = (new Intent(this, this.GetType()))
                                   .SetAction(helper.ACTION_COUNT_REPORT)
      let countPendingIntent = PendingIntent.GetService(this, 0, countIntent, PendingIntentFlags.UpdateCurrent)
      let builder = new App.NotificationCompat.Action.Builder (Resources.Drawable.restart, 
                                                        this.GetString(Resources.String.count_blescan_service_button_text), 
                                                        countPendingIntent)
      builder.Build()

   member this.BuildNotification notificationAction1 notificationAction2 =
      do this.notificationBuilder.MActions.Clear()
      this.notificationBuilder
         .AddAction(notificationAction1)
         .AddAction(notificationAction2)
         .Build()

   member this.RefreshNotification () =
       use notification = this.BuildNotification notificationAction1 notificationAction2
       use notificationManager = this.GetSystemService(Context.NotificationService) :?> NotificationManager 
       do notificationManager.Notify (helper.SERVICE_RUNNING_NOTIFICATION_ID,notification)      
      
   member this.BuildStopServiceAction =
      let stopServiceIntent = (new Intent(this, this.GetType()))
                                  .SetAction(helper.ACTION_STOP_SERVICE)
      let stopServicePendingIntent = PendingIntent.GetService(this, 0, stopServiceIntent, PendingIntentFlags.UpdateCurrent)
      let builder = new App.NotificationCompat.Action.Builder(Android.Resource.Drawable.IcMediaPause,
                                                        this.GetText(Resources.String.stop_service),
                                                        stopServicePendingIntent)
      builder.Build()

   member this.RegisterForegroundService (showRssiCountSwitch:Boolean) =
      notificationAction1 <- this.BuildStopServiceAction
      notificationAction2 <- if showRssiCountSwitch then this.BuildRssiBlescanAction else this.BuildMuteBlescanAction
      let notification = this.BuildNotification notificationAction1 notificationAction2
      do this.StartForeground (helper.SERVICE_RUNNING_NOTIFICATION_ID,notification)

   member this.StartScan() = 
      let scanSettingsBuilder = new LE.ScanSettings.Builder()
      let scanSettings = (scanSettingsBuilder.SetReportDelay 0L).SetScanMode(bleScanMode).Build()

      do mBLuetoothLeScanner.StartScan(null, scanSettings, mBLuetoothLeCallback )
      do Log.Debug (TAG, "StartScan") |> ignore 

   member this.StopScan() =
      do mBLuetoothLeScanner.FlushPendingScanResults mBLuetoothLeCallback
      do mBLuetoothLeScanner.StopScan mBLuetoothLeCallback
      do Log.Debug (TAG, "StopScan") |> ignore 
   
   member this.timerElapsedHandler args  =
      if (ObservationsCountInPeriod > 0) && (not muted) then
         use param = new Bundle ()
         do param.PutString (TextToSpeech.Engine.KeyParamUtteranceId, "")
         if (isRssiReport) then
            let averageRssiInPeriod = totalRssiInPeriod / ObservationsCountInPeriod
            if   localeForSpeech = Java.Util.Locale.Italy then 
               do textToSpeech.Speak (" Segnale massimo " + topRssiInPeriod.ToString()+ " da indirizzo " +
                                   string topRssiBeaconInPeriod.[topRssiBeaconInPeriod.Length - 2] + " " +
                                   string topRssiBeaconInPeriod.[topRssiBeaconInPeriod.Length - 1], QueueMode.Add, param, "UniqueID") |> ignore
            elif localeForSpeech = Java.Util.Locale.Uk then
               do textToSpeech.Speak (" Maximum signal " + topRssiInPeriod.ToString()+ " from beacon " +
                                   string topRssiBeaconInPeriod.[topRssiBeaconInPeriod.Length - 2] + " " +
                                   string topRssiBeaconInPeriod.[topRssiBeaconInPeriod.Length - 1], QueueMode.Add, param, "UniqueID") |> ignore
            else
               do textToSpeech.Speak (" Nivel máximo de señal " + topRssiInPeriod.ToString()+ " desde posición " +
                                   string topRssiBeaconInPeriod.[topRssiBeaconInPeriod.Length - 2] + " " +
                                   string topRssiBeaconInPeriod.[topRssiBeaconInPeriod.Length - 1], QueueMode.Add, param, "UniqueID") |> ignore
         else
            do textToSpeech.Speak ((ObservationsCountInPeriod.ToString() + 
                                    if    localeForSpeech = Java.Util.Locale.Italy then " rilevazioni da " +  beaconCountInPeriod.ToString() + "indirizzi"
                                    elif  localeForSpeech = Java.Util.Locale.Uk    then " advertisements from " +  beaconCountInPeriod.ToString() + "beacons"
                                    else " señales desde " + beaconCountInPeriod.ToString() + "balizas")
                 , QueueMode.Add, param, "UniqueID") |> ignore
      else
         ()
      do ObservationsCountInPeriod <- 0
      do topRssiInPeriod <- -150
      do beaconCountInPeriod <- 0
      do Array.Clear(perBeaconCountInPeriod, 0, 64)

   member this.regionTimeoutHandler args  =
        use param = new Bundle ()
        do param.PutString (TextToSpeech.Engine.KeyParamUtteranceId, "")
        match regionAddress with
        | None -> ()
        | Some add -> 
            Log.Debug ("RegionTimeoutHandler", "Timeout expired") |> ignore
            do regionAddress <- None
            do this.RefreshNotification()

            if not muted then
                if localeForSpeech = Java.Util.Locale.Italy then 
                    do textToSpeech.Speak ("Uscita regione " + string add.[add.Length - 2] + " " +
                        string add.[add.Length - 1], QueueMode.Add, param, "UniqueID") |> ignore
                elif localeForSpeech = Java.Util.Locale.Uk then
                    do textToSpeech.Speak ("Exit region " + string add.[add.Length - 2] + " " +
                        string add.[add.Length - 1], QueueMode.Add, param, "UniqueID") |> ignore
                else
                    do textToSpeech.Speak ("Salida región " + string add.[add.Length - 2] + " " +
                        string add.[add.Length - 1], QueueMode.Add, param, "UniqueID") |> ignore                                                         

   override this.OnCreate () =
      base.OnCreate()

      let btManager = Application.Context.GetSystemService(Context.BluetoothService) :?> BluetoothManager
      do mBluetoothAdapter <- btManager.Adapter
      do mBLuetoothLeScanner <- mBluetoothAdapter.BluetoothLeScanner
      do updateTimer.AutoReset <- true
      do updateTimer.Elapsed.Add this.timerElapsedHandler
      do regionTimeoutTimer.AutoReset <- false
      do regionTimeoutTimer.Elapsed.Add this.regionTimeoutHandler
      do textToSpeech <- new TextToSpeech (this,this)

      do Event.add (this.obsAction) eventObservation.Publish
      
   override this.OnStartCommand (intent, flags, startId) =
      match intent.Action with 
      | "BLEService.action.START_SERVICE" -> 
         if isStarted then
               Log.Info(TAG, "OnStartCommand: The service is already running.") |> ignore
         else
               Log.Info(TAG, "OnStartCommand: The service is starting.") |> ignore
               do localeForSpeech <- match intent.GetStringExtra (helper.EXTRA_SERVICE_LANGUAGE) with
                                        | "IT" ->  Java.Util.Locale.Italy
                                        | "ES" ->  new Java.Util.Locale ("es", "ES")
                                        | _ -> Java.Util.Locale.Uk
               do beacon2Observe <- Seq.cast (intent.GetStringArrayListExtra (helper.EXTRA_SERVICE_FILTER))
               do bleScanMode <- match intent.GetStringExtra (helper.EXTRA_SERVICE_SCANMODE) with
                                    | "LOWLATENCY" -> LE.ScanMode.LowLatency
                                    | "LOWPOWER" -> LE.ScanMode.LowPower
                                    | _ -> LE.ScanMode.LowPower
               do regionTrackingMode <- intent.GetBooleanExtra (helper.EXTRA_SERVICE_REGIONTRACKING, false)
               if regionTrackingMode then
                   do rssiThreshold <- intent.GetIntExtra (helper.EXTRA_SERVICE_RSSITHRESHOLD, -90)
                   do regionTimeoutTimer.Enabled <- true
                   do regionTimeoutTimer.Interval <- float (intent.GetIntExtra (helper.EXTRA_SERVICE_REGION_TIMEOUT, 5) * 1000)
                   do regionTimeoutTimer.Enabled <- false
               else
                   do updateTimer.Start()

               this.RegisterForegroundService (not regionTrackingMode) 
               this.StartScan()
               do isStarted <- true

      | "BLEService.action.STOP_SERVICE" -> 
         Log.Info(TAG, "OnStartCommand: The service is stopping.") |> ignore
         do this.StopForeground(true)
         do this.StopSelf()
         do isStarted <- false
      | "BLEService.action.MUTE_SERVICE" -> 
         Log.Info(TAG, "OnStartCommand: Muting service") |> ignore
         do notificationAction2 <- this.BuildUnmuteBlescanAction
         do muted <- true      
         do this.RefreshNotification ()
      | "BLEService.action.UNMUTE_SERVICE" -> 
         Log.Info(TAG, "OnStartCommand: Unmuting service") |> ignore
         do notificationAction2 <- this.BuildMuteBlescanAction
         do muted <- false
         do this.RefreshNotification ()
      | "BLEService.action.RSSI_REPORT" -> 
         Log.Info(TAG, "OnStartCommand: Switching to RSSI report") |> ignore
         do notificationAction2 <- this.BuildCountBlescanAction
         do isRssiReport <- true
         do this.RefreshNotification ()
      | "BLEService.action.COUNT_REPORT" -> 
         Log.Info(TAG, "OnStartCommand: Switching to Count report") |> ignore
         do notificationAction2 <- this.BuildRssiBlescanAction
         do isRssiReport <- false
         do this.RefreshNotification ()

      | _ -> ()

      StartCommandResult.Sticky

   override this.OnBind (intent) =
      null

   override this.OnDestroy () =
      Log.Info(TAG, "OnDestroy: The started service is shutting down.") |> ignore
      do (if regionTrackingMode then regionTimeoutTimer else updateTimer).Stop()
      // Stop the blescan.
      this.StopScan()
      do textToSpeech.Stop() |> ignore
      do textToSpeech.Shutdown()

      // Remove the notification from the status bar.
      let notificationManager = Application.Context.GetSystemService(Context.NotificationService) :?> NotificationManager
      notificationManager.Cancel helper.SERVICE_RUNNING_NOTIFICATION_ID

      base.OnDestroy()

   