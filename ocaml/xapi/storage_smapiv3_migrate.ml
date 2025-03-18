module type SMAPIv2_MIRROR = Storage_interface.MIRROR

module D = Debug.Make (struct let name = "storage_smapiv3_migrate" end)

module Unixext = Xapi_stdext_unix.Unixext
open Storage_interface
open Storage_task
module State = Storage_migrate_helper.State
module SXM = Storage_migrate_helper.SXM
open Xmlrpc_client
open D
open Storage_migrate_helper
module Local = Storage_migrate_helper.Local

let u x = raise Storage_interface.(Storage_error (Errors.Unimplemented x))

let export_nbd_proxy ~remote_url ~mirror_vm ~sr ~vdi ~dp ~verify_dest =
  debug "%s spawning exporting nbd proxy" __FUNCTION__ ;
  let path =
    Printf.sprintf "/var/run/nbdproxy/export/%s" (Vm.string_of mirror_vm)
  in
  let proxy_srv = Fecomms.open_unix_domain_sock_server path in
  try
    let uri =
      Printf.sprintf "/services/SM/nbdproxy/import/%s/%s/%s/%s"
        (Vm.string_of mirror_vm) (Sr.string_of sr) (Vdi.string_of vdi) dp
    in

    let dest_url = Http.Url.set_uri (Http.Url.of_string remote_url) uri in
    debug "%s now waiting for connection at %s" __FUNCTION__ path ;
    let nbd_client, _addr = Unix.accept proxy_srv in
    debug "%s connection accepted" __FUNCTION__ ;
    let request =
      Http.Request.make
        ~query:(Http.Url.get_query_params dest_url)
        ~version:"1.0" ~user_agent:"export_nbd_proxy" Http.Put uri
    in
    debug "%s making request to dest %s" __FUNCTION__
      (Http.Url.to_string dest_url) ;
    let verify_cert = if verify_dest then Stunnel_client.pool () else None in
    let transport = Xmlrpc_client.transport_of_url ~verify_cert dest_url in
    with_transport ~stunnel_wait_disconnect:false transport
      (with_http request (fun (_response, s) ->
           debug "%s starting proxy" __FUNCTION__ ;
           Unixext.proxy (Unix.dup s) (Unix.dup nbd_client)
       )
      ) ;
    Unix.close proxy_srv
  with e ->
    D.debug "%s did not get connection due to %s, closing" __FUNCTION__
      (Printexc.to_string e) ;
    Unix.close proxy_srv ;
    raise e

let mirror_checker dbg mirror_id sr vdi vm key =
  let rec mirror_checker_rec () =
    let alm_opt = State.find_active_local_mirror mirror_id in
    Option.iter
      (fun (alm : State.Send_state.t) ->
        Option.iter
          (fun _handle ->
            let {failed; _} : Mirror.status =
              Local.DATA.stat dbg sr vdi vm key
            in
            if failed then (
              error "%s QEMU mirroring has failed" __FUNCTION__ ;
              Updates.add (Dynamic.Mirror mirror_id) updates
            ) ;
            alm.watchdog <-
              Some
                (Scheduler.one_shot scheduler (Scheduler.Delta 5)
                   "qemu_watchdog" mirror_checker_rec
                )
          )
          alm.watchdog
      )
      alm_opt
  in
  mirror_checker_rec ()

let mirror_wait ~dbg ~sr ~vdi ~vm ~mirror_id mirror_key =
  let rec mirror_wait_rec key =
    let {failed; complete; progress} : Mirror.status =
      Local.DATA.stat dbg sr vdi vm key
    in
    if complete then (
      Option.fold ~none:()
        ~some:(fun p -> info "%s progress is %f" __FUNCTION__ p)
        progress ;
      info "%s completed" __FUNCTION__
    ) else if failed then (
      Option.iter
        (fun (snd_state : State.Send_state.t) -> snd_state.failed <- true)
        (State.find_active_local_mirror mirror_id) ;
      info "%s failed" __FUNCTION__ ;
      raise
        (Storage_interface.Storage_error
           (Migration_mirror_failure "Mirror failed")
        )
    ) else (
      Option.fold ~none:()
        ~some:(fun p -> info "%s progress is %f" __FUNCTION__ p)
        progress ;
      mirror_wait_rec key
    )
  in

  match mirror_key with
  | Storage_interface.Mirror.CopyV1 _ ->
      ()
  | Storage_interface.Mirror.MirrorV1 _ ->
      debug "%s waiting for mirroring to be done" __FUNCTION__ ;
      mirror_wait_rec mirror_key

module MIRROR : SMAPIv2_MIRROR = struct
  let send_start _ctx ~dbg ~task_id:_ ~dp ~sr ~vdi ~mirror_vm ~mirror_id
      ~local_vdi:_ ~copy_vm:_ ~live_vm ~url ~remote_mirror ~dest_sr ~verify_dest
      =
    let nbd_proxy_path =
      Printf.sprintf "/var/run/nbdproxy/export/%s" (Vm.string_of mirror_vm)
    in
    match remote_mirror with
    | Mirror.Vhd_mirror _ ->
        raise
          (Storage_error
             (Migration_preparation_failure
                "Incorrect remote mirror format for SMAPIv3"
             )
          )
    | Mirror.QCOW2_mirror {nbd_export; mirror_datapath; mirror_vdi} -> (
      try
        let nbd_uri =
          Uri.make ~scheme:"nbd+unix" ~host:"" ~path:nbd_export
            ~query:[("socket", [nbd_proxy_path])]
            ()
          |> Uri.to_string
        in
        let _ : Thread.t =
          Thread.create
            (fun () ->
              export_nbd_proxy ~remote_url:url ~mirror_vm ~sr:dest_sr
                ~vdi:mirror_vdi.vdi ~dp:mirror_datapath ~verify_dest
            )
            ()
        in
        info "%s nbd_proxy_path: %s nbd_uri %s" __FUNCTION__ nbd_proxy_path
          nbd_uri ;
        (* TODO need to move the mirror wait to somewhere else, no need to wait it here *)
        let mk = Local.DATA.mirror dbg sr vdi live_vm nbd_uri in

        debug "%s Updating active local mirrors: id=%s" __FUNCTION__ mirror_id ;
        let alm =
          State.Send_state.
            {
              url
            ; source_sr= sr
            ; dest_sr
            ; remote_info=
                Some
                  {dp= mirror_datapath; vdi= mirror_vdi.vdi; url; verify_dest}
            ; local_dp= dp
            ; tapdev= None
            ; failed= false
            ; watchdog= None
            ; vdi
            ; live_vm
            ; mirror_key= Some mk
            }
        in
        State.add mirror_id (State.Send_op alm) ;
        debug "%s Updated mirror_id %s in the active local mirror" __FUNCTION__
          mirror_id ;
        mirror_wait ~dbg ~sr ~vdi ~vm:live_vm ~mirror_id mk
      with e ->
        error "%s some other failure: %s" __FUNCTION__ (Printexc.to_string e) ;
        raise
          (Storage_interface.Storage_error
             (Migration_mirror_failure (Printexc.to_string e))
          )
    )

  let receive_start _ctx ~dbg:_ ~sr:_ ~vdi_info:_ ~id:_ ~similar:_ =
    u "DATA.MIRROR.receive_start"

  let receive_start2 _ctx ~dbg ~sr ~vdi_info ~mirror_id ~similar:_ ~vm ~url
      ~verify_dest =
    debug "%s dbg: %s sr: %s vdi: %s id: %s vm: %s url: %s verify_dest: %B"
      __FUNCTION__ dbg (s_of_sr sr)
      (string_of_vdi_info vdi_info)
      mirror_id (s_of_vm vm) url verify_dest ;
    let module Remote = StorageAPI (Idl.Exn.GenClient (struct
      let rpc =
        Storage_utils.rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2"
          (Storage_utils.connection_args_of_uri ~verify_dest url)
    end)) in
    let on_fail : (unit -> unit) list ref = ref [] in
    try
      (* We drop cbt_metadata VDIs that do not have any actual data *)
      let (vdi_info : vdi_info) =
        {vdi_info with sm_config= [("base_mirror", mirror_id)]}
      in
      let leaf_dp = Remote.DP.create dbg Uuidx.(to_string (make ())) in
      let leaf = Remote.VDI.create dbg sr vdi_info in
      info "Created leaf VDI for mirror receive: %s" (string_of_vdi_info leaf) ;
      on_fail := (fun () -> Remote.VDI.destroy dbg sr leaf.vdi) :: !on_fail ;
      let dummy = Remote.VDI.snapshot dbg sr leaf in
      on_fail := (fun () -> Remote.VDI.destroy dbg sr dummy.vdi) :: !on_fail ;
      let backend = Remote.VDI.attach3 dbg leaf_dp sr leaf.vdi vm true in
      let nbd_export =
        match nbd_export_of_attach_info backend with
        | None ->
            raise
              (Storage_error
                 (Migration_preparation_failure "Cannot parse nbd uri")
              )
        | Some export ->
            export
      in
      debug "%s activating dp %s sr: %s vdi: %s vm: %s" __FUNCTION__ leaf_dp
        (s_of_sr sr) (s_of_vdi leaf.vdi) (s_of_vm vm) ;
      Remote.VDI.activate3 dbg leaf_dp sr leaf.vdi vm ;
      let remote_mirror =
        Mirror.QCOW2_mirror
          {Mirror.mirror_vdi= leaf; mirror_datapath= leaf_dp; nbd_export}
      in
      debug "%s updating receiving state lcoally to id: %s vm: %s vdi_info: %s"
        __FUNCTION__ mirror_id (s_of_vm vm)
        (string_of_vdi_info vdi_info) ;
      State.update_mirror_recv_state ~sr ~vdi_info ~mirror_id ~vm ~url
        ~verify_dest remote_mirror ;
      remote_mirror
    with e ->
      List.iter
        (fun op ->
          try op ()
          with e ->
            debug "Caught exception in on_fail: %s performing cleaning up"
              (Printexc.to_string e)
        )
        !on_fail ;
      raise e

  let receive_finalize _ctx ~dbg:_ ~id:_ = u "DATA.MIRROR.receive_finalize"

  let receive_finalize2 _ctx ~dbg ~mirror_id ~sr ~url ~verify_dest =
    debug "%s dbg:%s id: %s sr: %s url: %s verify_dest: %B" __FUNCTION__ dbg
      mirror_id (s_of_sr sr) url verify_dest ;
    let (module Remote) =
      Storage_migrate_helper.get_remote_backend url verify_dest
    in
    let open State.Receive_state in
    let recv_state = State.find_active_receive_mirror mirror_id in
    Option.iter
      (fun r ->
        Remote.DP.destroy2 dbg r.leaf_dp r.sr r.leaf_vdi r.mirror_vm false ;
        Remote.VDI.remove_from_sm_config dbg r.sr r.leaf_vdi "base_mirror"
      )
      recv_state ;
    State.remove_receive_mirror mirror_id

  let stop _ctx ~dbg:_ ~id:_ = u "DATA.MIRROR.stop"

  let is_mirror_failed _ctx ~dbg ~mirror_id ~sr =
    match State.find_active_local_mirror mirror_id with
    | Some ({mirror_key= Some mk; vdi; live_vm; _} : State.Send_state.t) ->
        let {failed; _} : Mirror.status =
          Local.DATA.stat dbg sr vdi live_vm mk
        in
        failed
    | _ ->
        false

  let pre_deactivate_hook ctx ~dbg ~dp ~sr ~vdi =
    debug "%s dbg: %s dp: %s sr: %s vdi: %s" __FUNCTION__ dbg dp (s_of_sr sr)
      (s_of_vdi vdi) ;
    let mirror_id = State.mirror_id_of (sr, vdi) in
    D.debug "%s looking for final stats" __FUNCTION__ ;
    State.find_active_local_mirror mirror_id
    |> Option.iter (fun (s : State.Send_state.t) ->
           if is_mirror_failed ctx ~dbg ~mirror_id ~sr:s.source_sr then (
             D.error "%s QEMU reports mirroring failed" __FUNCTION__ ;
             s.failed <- true
           ) ;
           Option.iter (Scheduler.cancel scheduler) s.watchdog ;
           s.watchdog <- None
       )

  let stat _ctx ~dbg:_ ~id:_ = u "DATA.MIRROR.stat"

  let receive_cancel _ctx ~dbg:_ ~id:_ = u "DATA.MIRROR.receive_cancel"

  let receive_cancel2 _ctx ~dbg:_ ~id:_ = u "DATA.MIRROR.receive_cancel2"

  let list _ctx ~dbg:_ = u "DATA.MIRROR.list"
end
