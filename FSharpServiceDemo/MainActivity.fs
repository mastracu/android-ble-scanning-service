﻿namespace FSharpServiceDemo

open System
open System.IO  // File.Create

open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
open Android.Preferences
open Android.Bluetooth
open Android.Util

open helper


type Resources = FSharpServiceDemo.Resource

//TODO: support multiple display sizes (convertion to dpi) - tablet 
//TODO: check bluetooth adapter enable state - move to button
//https://blog.xamarin.com/requesting-runtime-permissions-in-android-marshmallow/
//https://developer.xamarin.com/guides/android/application_fundamentals/permissions/
//https://developer.xamarin.com/guides/android/application_fundamentals/understanding_android_api_levels/
//TODO: option menu entry to BLE print barcode label for beacon (low priority since all MPACT beacons have one already)
//also com.symbol.rxlogger.intent.action.BACKUP_NOW - build a dialog that also contains a log of the different actions (timestamp and power scan type) - sebd broadcast intents
//TODO: make HTTP POST notification parameters persistent wth shared preferences 
//TODO: app crashes on TC20 when trying to build DW configuration.
//http://fsharp.github.io/FSharp.Data/library/Http.html


//Enable BLE option menu entry
//Run-time request permission option menu entry

// Launcher icon generator: http://romannurik.github.io/AndroidAssetStudio/


type barcodeReceiver (dwIntentAction: String, barcodeProcessor: String -> String -> String -> Unit) = 
   inherit BroadcastReceiver()               
   let mutable decodedSource = ""
   let mutable decodedData = ""
   let mutable decodedLabelType = ""
   let mutable activeProfile = ""
   override this.OnReceive (context, intent) =
      let action = intent.Action
      let b = intent.Extras
      match action = dwIntentAction  with 
      | true ->
         do decodedSource <- b.GetString "com.symbol.datawedge.source"
         do decodedData <- b.GetString "com.symbol.datawedge.data_string"
         do decodedLabelType <- b.GetString "com.symbol.datawedge.label_type"
         do barcodeProcessor decodedSource decodedData decodedLabelType
      | false ->
         ()

[<Activity (Label = "bleScan", MainLauncher = true, Icon = "@mipmap/ic_launcher")>]
[<IntentFilter [| "BLEService.action.MAIN_ACTIVITY" |]>]
type MainActivity () =
    inherit Activity ()

    let mutable beaconcbArray = Unchecked.defaultof<CheckBox []>
    let mutable regionTrackingcb = Unchecked.defaultof<CheckBox>
    let mutable beaconIntentBool = false
    let mutable regionThreshold = Unchecked.defaultof<int>
    let mutable regionTimeout = Unchecked.defaultof<int>
    let mutable regionNotificationEnableBool = false
    let mutable regionNotificationUrlStr = helper.REGION_NOTIFICATION_URL

    let updateTextregionTrackingcb () =  
       regionTrackingcb.Text <- "RegionTracking (" + regionThreshold.ToString() + " dbm, " + regionTimeout.ToString() + " secs)" 

    member this.showBarcodeToast (text:string) = 
       this.RunOnUiThread( fun() -> 
          let barcodeToast = (Android.Widget.Toast.MakeText(this, text, Android.Widget.ToastLength.Long))
          do barcodeToast.Show() )

    member this.barcodeBroadcastReceiver = new barcodeReceiver ("com.zebra.newbeacon", this.replaceBeacon)

    member this.replaceBeacon a newBeacon c =         
       let hexDigits = ['0'..'9'] @ ['a'..'f'] @ ['A'..'F']  
       if (newBeacon.Length = 12) then
          let newFormattedBeacon = newBeacon.[0..1] + ":" + newBeacon.[2..3] + ":" + newBeacon.[4..5] + ":" + newBeacon.[6..7]
                                   + ":" + newBeacon.[8..9] + ":" + newBeacon.[10..11]
          if helper.isValidBeaconAddress newFormattedBeacon then 
             let mutable beaconIndex = 0
             let alert = new AlertDialog.Builder(this)
             do (alert.SetTitle (newFormattedBeacon + " to replace:")) |> ignore
             do alert.SetCancelable false |> ignore
             do alert.SetSingleChoiceItems ( [| for cb in beaconcbArray do yield cb.Text |], 0, 
                    new EventHandler<DialogClickEventArgs> (fun s dArgs -> beaconIndex <- dArgs.Which)) |> ignore
             do alert.SetPositiveButton ("OK", 
                    new EventHandler<DialogClickEventArgs> (fun s dArgs -> beaconcbArray.[beaconIndex].Text <- newFormattedBeacon) ) |> ignore
             do alert.SetNegativeButton ("Cancel", 
                    new EventHandler<DialogClickEventArgs> (fun s dArgs -> ()) ) |> ignore
             let dialog = alert.Create()
             do dialog.Show() |> ignore
          else
             this.showBarcodeToast ("Scanned invalid beacon address " + newBeacon)             
       else
          this.showBarcodeToast ("Scanned barcode length is invalid " + newBeacon)

    member this.selectedLanguage () =
        let enrbLanguage = this.FindViewById<RadioButton>(Resources.Id.en_language)
        let itrbLanguage = this.FindViewById<RadioButton>(Resources.Id.it_language)
        let esrbLanguage = this.FindViewById<RadioButton>(Resources.Id.es_language)
        let ptrbLanguage = this.FindViewById<RadioButton>(Resources.Id.pt_language)

        if    enrbLanguage.Checked then "EN" 
        elif  itrbLanguage.Checked then "IT"
        elif  ptrbLanguage.Checked then "PT"
        else  "ES"

    member this.selectedScanMode () =
       let rbLowLatencyMode = this.FindViewById<RadioButton>(Resources.Id.LowLatencyMode)
       if rbLowLatencyMode.Checked then "LOWLATENCY"
       else "LOWPOWER"


    override this.OnCreate (bundle) =

        base.OnCreate (bundle)
        // Set our view from the "main" layout resource
        this.SetContentView (Resources.Layout.Main)

        // let startServiceIntent = new Intent(this, typeof<BleScanningService.BleScanningService>)
        let startServiceIntent = new Intent()
        do startServiceIntent.SetAction(helper.ACTION_START_SERVICE) |> ignore
        do startServiceIntent.SetPackage(this.PackageName) |> ignore

        // let stopServiceIntent = new Intent(this, typeof<BleScanningService.BleScanningService>);
        let stopServiceIntent = new Intent()
        do stopServiceIntent.SetAction(helper.ACTION_STOP_SERVICE) |> ignore
        do stopServiceIntent.SetPackage(this.PackageName) |> ignore

        let stopServiceButton = this.FindViewById<Button>(Resources.Id.stop_blescan_service_button)
        let startServiceButton = this.FindViewById<Button>(Resources.Id.start_blescan_service_button)
        let BLEScanLoggingButton = this.FindViewById<Button>(Resources.Id.blescan_logging)

        do beaconcbArray <- [|this.FindViewById<CheckBox>(Resources.Id.btadd1_filter); 
                              this.FindViewById<CheckBox>(Resources.Id.btadd2_filter);
                              this.FindViewById<CheckBox>(Resources.Id.btadd3_filter)|]
        let enrbLanguage = this.FindViewById<RadioButton>(Resources.Id.en_language)
        let itrbLanguage = this.FindViewById<RadioButton>(Resources.Id.it_language)
        let esrbLanguage = this.FindViewById<RadioButton>(Resources.Id.es_language)
        let ptrbLanguage = this.FindViewById<RadioButton>(Resources.Id.pt_language)
        let lowpowerrbScanMode = this.FindViewById<RadioButton>(Resources.Id.LowPowerMode)
        let lowlatencyrbScanMode = this.FindViewById<RadioButton>(Resources.Id.LowLatencyMode)
        do regionTrackingcb <- this.FindViewById<CheckBox>(Resources.Id.region_tracking)

// https://stackoverflow.com/questions/26668509/how-do-i-use-sharedpreferences-in-xamarin-android
        let prefs = PreferenceManager.GetDefaultSharedPreferences this

        do beaconcbArray.[0].Checked <- prefs.GetBoolean ("btadd1_selected", false) 
        do beaconcbArray.[1].Checked <- prefs.GetBoolean ("btadd2_selected", false) 
        do beaconcbArray.[2].Checked <- prefs.GetBoolean ("btadd3_selected", false) 

        do beaconcbArray.[0].Text <- prefs.GetString ("btadd1_filter", "beacon #1")
        do beaconcbArray.[1].Text <- prefs.GetString ("btadd2_filter", "beacon #2")
        do beaconcbArray.[2].Text <- prefs.GetString ("btadd3_filter", "beacon #3")

        do regionTrackingcb.Checked <- prefs.GetBoolean ("region_tracking", false) 

        do match prefs.GetString ("selected_language", "EN") with
           | "EN" -> enrbLanguage.Checked <- true
           | "ES" -> esrbLanguage.Checked <- true
           | "PT" -> ptrbLanguage.Checked <- true
           | "IT" -> itrbLanguage.Checked <- true 
           | _ -> ()

        do match prefs.GetString ("selected_scanmode", "LOWPOWER") with
           | "LOWPOWER" -> lowpowerrbScanMode.Checked <- true
           | "LOWLATENCY" -> lowlatencyrbScanMode.Checked <- true
           | _ -> ()

        do regionThreshold <- prefs.GetInt ("threshold_value", -90)
        do regionTimeout <- prefs.GetInt ("region_timeout", 5)

        do regionNotificationEnableBool <- prefs.GetBoolean ("region_notification_enable", false)
        do regionNotificationUrlStr <- prefs.GetString ("region_notification_url", helper.REGION_NOTIFICATION_URL)
        
        do updateTextregionTrackingcb ()

        startServiceButton.Click.Add (fun args -> 
           let selectedBeacons = [ for cb in beaconcbArray do if cb.Checked then yield cb.Text ]
           if List.forall (fun c -> c=true) (List.map helper.isValidBeaconAddress selectedBeacons) then
               if regionTrackingcb.Checked && selectedBeacons.IsEmpty then
                   this.showBarcodeToast "At least one beacon has to be specified when region tracking is enabled"
               else    
                   startServiceIntent.PutExtra (helper.EXTRA_SERVICE_SCANMODE, this.selectedScanMode() ) |>  ignore
                   startServiceIntent.PutExtra (helper.EXTRA_SERVICE_LANGUAGE, this.selectedLanguage() ) |>  ignore
                   // https://stackoverflow.com/questions/6543811/intent-putextra-list    
                   startServiceIntent.PutStringArrayListExtra (helper.EXTRA_SERVICE_FILTER, List.toArray selectedBeacons ) |> ignore
                   startServiceIntent.PutExtra (helper.EXTRA_SERVICE_REGIONTRACKING, regionTrackingcb.Checked) |> ignore
                   if regionTrackingcb.Checked then
                       startServiceIntent.PutExtra (helper.EXTRA_SERVICE_RSSITHRESHOLD, regionThreshold) |> ignore 
                       startServiceIntent.PutExtra (helper.EXTRA_SERVICE_REGION_TIMEOUT, regionTimeout) |> ignore 
                       startServiceIntent.PutExtra (helper.EXTRA_SERVICE_BEACON_INTENT, beaconIntentBool) |> ignore 
                       startServiceIntent.PutExtra (helper.EXTRA_SERVICE_NOTIFICATION, regionNotificationEnableBool) |> ignore
                       if regionNotificationEnableBool then
                            startServiceIntent.PutExtra (helper.EXTRA_SERVICE_NOTIFICATION_URL, regionNotificationUrlStr) |> ignore 
                       else
                            ()
                   else
                       ()
                   this.StartService (startServiceIntent) |> ignore
            else
               this.showBarcodeToast "Invalid beacon address selected"
        )

        stopServiceButton.Click.Add (fun args -> 
           do this.StopService (stopServiceIntent) |> ignore
        )

        BLEScanLoggingButton.Click.Add (fun args ->
           do this.StartActivity typedefof<BLELog.BLELogActivity>
//https://developer.android.com/guide/components/activities/tasks-and-back-stack.html
//When the current activity starts another, the new activity is pushed on the top of the stack and takes focus. The previous activity remains in the stack, but is stopped. When an activity stops, the system retains the current state of its user interface. When the user presses the Back button, the current activity is popped from the top of the stack (the activity is destroyed) and the previous activity resumes (the previous state of its UI is restored). 
        )

        let filter = new IntentFilter "com.zebra.newbeacon"
        do filter.AddCategory "android.intent.category.DEFAULT"
        do this.RegisterReceiver (this.barcodeBroadcastReceiver, filter) |> ignore

    override this.OnRequestPermissionsResult (requestcode, permissions, grantRes) =
        let text = if grantRes.[0] = PM.Permission.Granted then "Permission Granted" else "Permission denied"
        use permGrantedToast = 
            (Android.Widget.Toast.MakeText(this, text, Android.Widget.ToastLength.Long))
        do permGrantedToast.Show() 

    override this.OnResume () =
        do base.OnResume()
        let dialog = (new AlertDialog.Builder(this)).SetTitle("Run-time permissions request")
        if (int Build.VERSION.SdkInt >= 23) then 
            let permission = Android.Manifest.Permission.AccessFineLocation
            if this.CheckSelfPermission permission <> PM.Permission.Granted then
                let dialog2 = buildRequestLocationingPermissionsDialog this dialog
                do this.RunOnUiThread (fun () -> dialog2.Create()
                                                        .Show() ) 

        let btManager = Application.Context.GetSystemService(Context.BluetoothService) :?> BluetoothManager
        let ba = btManager.Adapter
        if (not ba.IsEnabled) then
            let alDialogBuild = (new AlertDialog.Builder(this))
                                            .SetTitle("BLE adapter is off")
                                            .SetMessage("Service requires ble adapter to be on.")
                                            .SetNegativeButton("Skip" , new EventHandler<DialogClickEventArgs> (fun s dArgs -> ()) )
                                            .SetPositiveButton("Turn ble on" , 
                                                    new EventHandler<DialogClickEventArgs> (fun s dArgs -> 
                                                            use int = new Intent ()
                                                            do  int.SetAction BluetoothAdapter.ActionRequestEnable |> ignore
                                                            do this.StartActivity int  
                                                        ) )
                                            .Create()
            do alDialogBuild.Show() 
                                         
        
    override this.OnPause () =

        do base.OnPause()
        let prefs = PreferenceManager.GetDefaultSharedPreferences this
        let editor = prefs.Edit ()
        do editor.PutString ("btadd1_filter", beaconcbArray.[0].Text) |> ignore
        do editor.PutBoolean ("btadd1_selected", beaconcbArray.[0].Checked) |> ignore
        do editor.PutString ("btadd2_filter", beaconcbArray.[1].Text) |> ignore
        do editor.PutBoolean ("btadd2_selected", beaconcbArray.[1].Checked) |> ignore
        do editor.PutString ("btadd3_filter", beaconcbArray.[2].Text) |> ignore
        do editor.PutBoolean ("btadd3_selected", beaconcbArray.[2].Checked) |> ignore
        do editor.PutString ("selected_language", this.selectedLanguage()) |> ignore
        do editor.PutString ("selected_scanmode", this.selectedScanMode()) |> ignore
        do editor.PutBoolean ("region_tracking", regionTrackingcb.Checked ) |> ignore
        do editor.PutInt ("threshold_value", regionThreshold) |> ignore
        do editor.PutInt ("region_timeout", regionTimeout) |> ignore
        do editor.PutBoolean ("region_notification_enable", regionNotificationEnableBool) |> ignore
        do editor.PutString ("region_notification_url", regionNotificationUrlStr) |> ignore

        do editor.Apply ()

    override this.OnCreateOptionsMenu menu =
        let inflater = new MenuInflater (this) 
        do inflater.Inflate (Resources.Menu.option, menu)
        true

    override this.OnOptionsItemSelected item = 
        if item.ItemId = Resources.Id.datawedgeRebuild then
            let dialog = 
                (new AlertDialog.Builder(this))
                        .SetTitle("bleService DW Profile")
                        .SetMessage("Existing bleService DW profile will be overridden")
                        .SetNegativeButton("Cancel" , new EventHandler<DialogClickEventArgs> (fun s dArgs -> ()) )
                        .SetPositiveButton("Apply" , new EventHandler<DialogClickEventArgs> 
                                (fun s dArgs -> 
                                    do try Asset2DWAutoImport  "dwprofile_Bleservice.db" "/enterprise/device/settings/datawedge/autoimport/" this  
                                       with | :? System.Exception as exn -> 
                                                   let errToast = Android.Widget.Toast.MakeText(this, exn.Message, Android.Widget.ToastLength.Long)
                                                   do errToast.Show()
                                    )
                              )
                        .Create()
            do dialog.Show()
            true
        elif item.ItemId = Resources.Id.regionTrackingParameters then
            let inflater = this.GetSystemService(Context.LayoutInflaterService) :?> LayoutInflater
            let layout = inflater.Inflate (Resources.Layout.regionTrackingParms, this.FindViewById<ViewGroup>(Resources.Id.layoutDialog) )
            let text1 = layout.FindViewById<TextView>(Resources.Id.textView1)
            let seekBar1 = layout.FindViewById<SeekBar>(Resources.Id.seekBar1)
            let text2 = layout.FindViewById<TextView>(Resources.Id.textView2)
            let seekBar2 = layout.FindViewById<SeekBar>(Resources.Id.seekBar2)
            let defaultButton = layout.FindViewById<Button>(Resources.Id.defaultButton)
            let cancelButton = layout.FindViewById<Button>(Resources.Id.cancelButton)
            let applyButton = layout.FindViewById<Button>(Resources.Id.applyButton)
            let beaconIntentCheckBox = layout.FindViewById<CheckBox>(Resources.Id.cbObservationsIntents)

            do seekBar1.ProgressChanged.Add (fun e -> text1.Text <- "Enter Region RSSI Threshold: " + (e.Progress-110).ToString() + " dbm" ) 
            do seekBar2.ProgressChanged.Add (fun e -> text2.Text <- "Region Exit Timeout: " + e.Progress.ToString() + " secs" )

            let alert = (new Dialog(this))
            do alert.SetTitle "Region Tracking Parms"
            do alert.SetContentView layout
            do alert.SetCancelable false

            do defaultButton.Click.Add (fun dArgs -> seekBar1.Progress <- 20; seekBar2.Progress <- 5)
            do cancelButton.Click.Add (fun dArgs -> alert.Dismiss())
            do applyButton.Click.Add (fun dArgs -> 
                regionTimeout <- seekBar2.Progress
                regionThreshold <- seekBar1.Progress-110
                beaconIntentBool <- beaconIntentCheckBox.Checked
                do updateTextregionTrackingcb () 
                alert.Dismiss())

            do seekBar1.Progress <- regionThreshold + 110
            do seekBar2.Progress <- regionTimeout 
            do beaconIntentCheckBox.Checked <- beaconIntentBool
            do alert.Show()
            true
        elif item.ItemId = Resources.Id.BLECapabilities then
            use btManager = Application.Context.GetSystemService(Context.BluetoothService) :?> BluetoothManager
//https://developer.android.com/reference/android/bluetooth/BluetoothAdapter.html 
//see isMultipleAdvertisementSupported (API21), isOffloadedScanBatchingSupported and isOffloadedFilteringSupported (API 21)
            use ba = btManager.Adapter
            let text = sprintf "MultiAdvertSupport: %s\nScanBatchSupport: %s\nOffloadFilterSupport: %s" 
                            (ba.IsMultipleAdvertisementSupported.ToString()) 
                            (ba.IsOffloadedScanBatchingSupported.ToString()) 
                            (ba.IsOffloadedFilteringSupported.ToString())
            let dialog = (new AlertDialog.Builder(this))
                            .SetTitle("ble adapter capabilities")
                            .SetMessage(text)
                            .SetNeutralButton("OK" , new EventHandler<DialogClickEventArgs> (fun s dArgs -> ()) )
                            .Create()
            do dialog.Show()
            true
        elif item.ItemId = Resources.Id.notificationsPost then
            let inflater = this.GetSystemService(Context.LayoutInflaterService) :?> LayoutInflater
            let layout = inflater.Inflate (Resources.Layout.notificationsSettings, this.FindViewById<ViewGroup>(Resources.Id.layoutNotificationDialog) )
            let notificationUrl = layout.FindViewById<EditText>(Resources.Id.notificationUrl)
            let enableNotifications = layout.FindViewById<CheckBox>(Resources.Id.enableNotifications)
            let defaultButton = layout.FindViewById<Button>(Resources.Id.defaultButtonNotification)
            let cancelButton = layout.FindViewById<Button>(Resources.Id.cancelButtonNotification)
            let applyButton = layout.FindViewById<Button>(Resources.Id.applyButtonNotification)
            let testButton = layout.FindViewById<Button>(Resources.Id.buttonTestNotification)
            let testResult = layout.FindViewById<TextView>(Resources.Id.textViewTestNotification)

            let alert = (new Dialog(this))
            do alert.SetTitle "Region Notifications"
            do alert.SetContentView layout
            do alert.SetCancelable false

            do defaultButton.Click.Add (fun dArgs -> notificationUrl.Text <- helper.REGION_NOTIFICATION_URL ; enableNotifications.Checked <- false)
            do cancelButton.Click.Add (fun dArgs -> alert.Dismiss())
            do applyButton.Click.Add (fun dArgs -> 
                                         regionNotificationUrlStr <- notificationUrl.Text
                                         regionNotificationEnableBool <- enableNotifications.Checked
                                         alert.Dismiss() )
            do testButton.Click.Add (fun dArgs ->
                                         this.RunOnUiThread ( fun () -> testResult.Text <- "Test in progress...")
                                         //TODO: critical is to retrieve ESN and pass it over as one string joined with Build.Model.  Build.Serial throws an exception

                                         let jsonString = serialize "FAKEREGION" Build.Model (Java.Lang.JavaSystem.CurrentTimeMillis()) ExitRegion
                                         AsyncSendRegionNotification notificationUrl.Text jsonString 
                                              (fun s -> this.RunOnUiThread(fun () -> testResult.Text <- "Result: PASSED")) 
                                              (fun exn -> this.RunOnUiThread(fun () -> testResult.Text <- "Result: TIMEOUT EXPRIRED"))
                                        )


            do notificationUrl.Text <- regionNotificationUrlStr
            do enableNotifications.Checked <- regionNotificationEnableBool

            do alert.Show()
            true
        elif item.ItemId = Resources.Id.enableRXLogger then
            let mutable intentSelectIdx = 0
            let dialog = 
                (new AlertDialog.Builder(this))
                        .SetTitle("Send RxLogger intent")
                        .SetSingleChoiceItems( Resources.Array.rxlogger_intents, intentSelectIdx, 
                                new EventHandler<DialogClickEventArgs> (fun s dArgs -> intentSelectIdx <- dArgs.Which) )
                        .SetNegativeButton("Cancel" , new EventHandler<DialogClickEventArgs> (fun s dArgs -> ()) )
                        .SetPositiveButton("Apply" , 
                                new EventHandler<DialogClickEventArgs> (fun s dArgs -> 
                                let action = match intentSelectIdx with
                                                | 0 -> "com.symbol.rxlogger.intent.action.ENABLE"
                                                | 1 -> "com.symbol.rxlogger.intent.action.DISABLE"
                                                | 2 -> "com.symbol.rxlogger.intent.action.BACKUP_NOW"
                                                | _ -> ""
                                use int = new Intent ()
                                do  int.SetAction action |> ignore
                                do this.SendBroadcast int
                                do this.RunOnUiThread( fun() -> 
                                    use intentToast = 
                                        (Android.Widget.Toast.MakeText(this, "INTENT SENT: " + action, Android.Widget.ToastLength.Long))
                                    do intentToast.Show() )))
                        .Create()
            do dialog.Show()
            true
        elif item.ItemId = Resources.Id.requestPermissions then
            let dialog = (new AlertDialog.Builder(this))
                            .SetTitle("Run-time permissions request")
            let dialog2 = if (int Build.VERSION.SdkInt >= 23) then 
                              let permission = Android.Manifest.Permission.AccessFineLocation
                              if this.CheckSelfPermission permission <> PM.Permission.Granted then
                                  buildRequestLocationingPermissionsDialog this dialog                              
                              else
                                  dialog.SetMessage("Locationing permissions are granted to the application. No action required")
                                        .SetNeutralButton("OK" , new EventHandler<DialogClickEventArgs> (fun s dArgs -> ()) )
                          else
                              dialog.SetMessage("No Run-time permissions required below Marshmallow. No action required")
                                    .SetNeutralButton("OK" , new EventHandler<DialogClickEventArgs> (fun s dArgs -> ()) )
            do dialog2.Create()
                      .Show() 
            true
        else
            true