namespace BLELog

open System
open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
open Android.Bluetooth
open Android.Util

open helper

type Resources = FSharpServiceDemo.Resource

type ObservationReceiver (extraBase:string, func: BeaconObservation -> Unit) = 
   inherit BroadcastReceiver()
   let mutable mfunc = func
   member this.func with set (value) = mfunc <- value               
   override this.OnReceive (context, intent) =
       { ObservationTimestamp = intent.GetLongExtra (extraBase + ".ObservationTimestamp", 0L) 
         BeaconAddress = intent.GetStringExtra (extraBase + ".BeaconAddress")
         SignalStrength = intent.GetIntExtra (extraBase + ".SignalStrength", 0)
         DeviceName = "" } |> mfunc
        

// https://developer.xamarin.com/guides/android/advanced_topics/working_with_androidmanifest.xml/
[<Activity (Label = "BLEScanLog")>]
type BLELogActivity () =
    inherit Activity ()
          
    let zebraLog = new Android.Runtime.JavaList<string> ()
    let sessionStatistics = new SessionStatisticsProcessor ()
    let observationReceiver = new ObservationReceiver("com.zebra.newBLEObservation", fun bo -> ())

    let mutable listViewStatusLog:ListView = null
    let mutable logAdapter:ArrayAdapter = null
    let mutable isStoppedReceiver = true

    let clearSession (context:Activity) =
        let dialog = 
            (new AlertDialog.Builder(context))
                .SetTitle("Clear Session")
                .SetMessage("Please realize this will wipe out all collected data")
                .SetNegativeButton("Cancel" , new EventHandler<DialogClickEventArgs> (fun s dArgs -> ()) )
                .SetPositiveButton("Clear" , new EventHandler<DialogClickEventArgs> 
                        (fun s dArgs -> 
                            do sessionStatistics.Empty()
                            do zebraLog.Clear()
                            do context.RunOnUiThread (fun () -> do logAdapter.NotifyDataSetChanged())) )
                .Create()
        do dialog.Show()
       
    let statsShow context =
        let dialog = (new AlertDialog.Builder(context))
                         .SetTitle("Session Statistics")
                         .SetMessage(sessionStatistics.FetchReport())
                         .SetNeutralButton("OK" , new EventHandler<DialogClickEventArgs> (fun s dArgs -> ()) )
                         .Create()
        do dialog.Show()
   
    let StopLogEntry (context:Activity) now =
        do zebraLog.Insert(0, sprintf "%s\t--- LOGGING STOPPED ---" (epoch2timestamp now))
        do context.RunOnUiThread (fun () -> do logAdapter.NotifyDataSetChanged()) 
            
    member this.NewLogEntry newObservation =
        do sessionStatistics.UpdateWith newObservation 
        do zebraLog.Insert(0, sprintf "%A" newObservation)
        do this.RunOnUiThread (fun () -> do logAdapter.NotifyDataSetChanged()) 
   
    override this.OnResume () =
        base.OnResume ()

    override this.OnPause () =
        base.OnPause ()
        
    override this.OnCreate (bundle) =
        base.OnCreate (bundle)

        // Set our view from the "main" layout resource
        this.SetContentView (Resources.Layout.BLELogActivity)
        
        do listViewStatusLog <- this.FindViewById<ListView>(Resources.Id.listView1)
        do logAdapter <- new ArrayAdapter<string>(this, Android.Resource.Layout.TestListItem, zebraLog )
        do listViewStatusLog.Adapter <- logAdapter
        let buttonStopLog = this.FindViewById<Button>(Resources.Id.stop_receiver_button)
        let buttonClearLog = this.FindViewById<Button>(Resources.Id.clear_log_button)
        
        observationReceiver.func <- this.NewLogEntry
        let filter = new IntentFilter("com.zebra.newBLEObservation")
        do filter.AddCategory "android.intent.category.DEFAULT"
        do this.RegisterReceiver (observationReceiver, filter) |> ignore
        do isStoppedReceiver <- false

        do buttonStopLog.Click.Add (fun args -> 
           if isStoppedReceiver then 
               do this.RegisterReceiver (observationReceiver, filter) |> ignore               
               do buttonStopLog.Text <- "Stop Receiver"
               do isStoppedReceiver <- false
           else
               let now = Java.Lang.JavaSystem.CurrentTimeMillis() 
               do sessionStatistics.CloseSession now
               do this.UnregisterReceiver (observationReceiver) |> ignore
               do buttonStopLog.Text <- "Start Receiver"
               do StopLogEntry this now
               do isStoppedReceiver <- true
               do statsShow this
        )

        do buttonClearLog.Click.Add (fun args -> 
           do clearSession this
        )

    override this.OnCreateOptionsMenu menu =
        let inflater = new MenuInflater (this) 
        do inflater.Inflate (Resources.Menu.log, menu)
        true

    override this.OnOptionsItemSelected item = 
        if item.ItemId = Resources.Id.logAnalytics then
           do statsShow this   
        else
           ()
        true