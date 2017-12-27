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

type Resources = FSharpServiceDemo.Resource

type BeaconObservation = 
   {ObservationTimestamp:int64; BeaconAddress:string; SignalStrength: int; DeviceName: string} 
   override x.ToString() = 
      helper.epoch2timestamp x.ObservationTimestamp + "    " + x.BeaconAddress  + "    " + x.SignalStrength.ToString()
   member x.SendBroadcast (cont:Context) action =
       let int = new Intent ()
       do  int.SetAction action |> ignore
       do  int.PutExtra (action + ".ObservationTimestamp", x.ObservationTimestamp) |> ignore
       do  int.PutExtra (action + ".BeaconAddress", x.BeaconAddress) |> ignore
       do  int.PutExtra (action + ".SignalStrength", x.SignalStrength) |> ignore
       do  cont.SendBroadcast int

type ObservationReceiver (extraBase:string, e:Event<BeaconObservation>) = 
   inherit BroadcastReceiver()               
   override this.OnReceive (context, intent) =
       { ObservationTimestamp = intent.GetLongExtra (extraBase + ".ObservationTimestamp", 0L) 
         BeaconAddress = intent.GetStringExtra (extraBase + ".BeaconAddress")
         SignalStrength = intent.GetIntExtra (extraBase + ".SignalStrength", 0)
         DeviceName = "" } |> e.Trigger
        
// https://developer.xamarin.com/guides/android/advanced_topics/working_with_androidmanifest.xml/
[<Activity (Label = "BLEScanLog")>]
type BLELogActivity () =
    inherit Activity ()
          
    let bleNotification = new Event<BeaconObservation> ()

    let mutable listViewStatusLog:ListView = null
    let mutable logAdapter:ArrayAdapter = null
    let zebraLog = new Android.Runtime.JavaList<string> ()

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

        use filter = new IntentFilter("com.zebra.newBLEObservation")
        do filter.AddCategory "android.intent.category.DEFAULT"
        let obsRec = new ObservationReceiver("com.zebra.newBLEObservation", bleNotification)
        do this.RegisterReceiver (obsRec, filter) |> ignore

        do Event.add  (fun newObservation -> 
           do zebraLog.Insert(0, newObservation.ToString())
           do this.RunOnUiThread (fun () -> do logAdapter.NotifyDataSetChanged()) ) bleNotification.Publish

        do buttonStopLog.Click.Add (fun args -> 
           do this.UnregisterReceiver (obsRec) |> ignore
           do buttonStopLog.Enabled <- false
        )

        do buttonClearLog.Click.Add (fun args -> 
           do zebraLog.Clear()
           do this.RunOnUiThread (fun () -> do logAdapter.NotifyDataSetChanged())
        )