(*
 * Copyright (C) 2011 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module D = Debug.Make (struct let name = "storage_migrate" end)

open D
module Listext = Xapi_stdext_std.Listext
open Xapi_stdext_pervasives.Pervasiveext
module Unixext = Xapi_stdext_unix.Unixext
open Storage_interface
open Storage_task
open Storage_migrate_helper

module type SMAPIv2 = module type of Local

module type SMAPIv2_MIRROR = Storage_interface.MIRROR

let choose_backend dbg sr =
  debug "%s dbg: %s choosing backend for sr :%s" __FUNCTION__ dbg (s_of_sr sr) ;
  match Storage_mux_reg.migrate_version_of_sr sr with
  | Smapiv1 ->
      (module Storage_smapiv1_migrate.MIRROR : SMAPIv2_MIRROR)
  | Smapiv3 ->
      (module Storage_smapiv3_migrate.MIRROR : SMAPIv2_MIRROR)
  | Smapiv2 ->
      failwith "unsupported smapi version smapiv2"

(** module [MigrateRemote] is similar to [MigrateLocal]. Although conceptually
these functions should execute on the remote side, in practice they are more subtle
than that. *)
module MigrateRemote = struct
  let receive_finalize2 ~dbg ~mirror_id ~sr ~url ~verify_dest =
    let (module Migrate_Backend) = choose_backend dbg sr in
    Migrate_Backend.receive_finalize2 () ~dbg ~mirror_id ~sr ~url ~verify_dest

  let receive_cancel2 ~dbg ~id ~url ~verify_dest =
    let module Remote = StorageAPI (Idl.Exn.GenClient (struct
      let rpc =
        Storage_utils.rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2"
          (Storage_utils.connection_args_of_uri ~verify_dest url)
    end)) in
    let receive_state = State.find_active_receive_mirror id in
    let open State.Receive_state in
    Option.iter
      (fun r ->
        log_and_ignore_exn (fun () -> Remote.DP.destroy dbg r.leaf_dp false) ;
        List.iter
          (Option.iter (fun v ->
               log_and_ignore_exn (fun () -> Remote.VDI.destroy dbg r.sr v)
           )
          )
          [r.dummy_vdi; Some r.leaf_vdi; r.parent_vdi]
      )
      receive_state ;
    State.remove_receive_mirror id
end

(** This module [MigrateLocal] consists of the concrete implementations of the 
migration part of SMAPI. Functions inside this module are sender driven, which means
they tend to be executed on the sender side. although there is not a hard rule
on what is executed on the sender side, this provides some heuristics. *)
module MigrateLocal = struct
  (* TODO move this to helper *)
  let find_local_vdi ~dbg ~sr ~vdi =
    (* Find the local VDI *)
    let vdis, _ = Local.SR.scan2 dbg sr in
    try List.find (fun x -> x.vdi = vdi) vdis
    with Not_found -> failwith "Local VDI not found"

  (** [similar_vdis dbg sr vdi] returns a list of content_ids of vdis
  which are similar to the input [vdi] in [sr] *)
  let similar_vdis ~dbg ~sr ~vdi =
    let similar_vdis = Local.VDI.similar_content dbg sr vdi in
    let similars =
      List.filter_map
        (function
          | {content_id; _} when content_id = "" ->
              None
          | {content_id; _} ->
              Some content_id
          )
        similar_vdis
    in

    debug "Similar VDIs to = [ %s ]"
      (String.concat "; "
         (List.map
            (fun x ->
              Printf.sprintf "(vdi=%s,content_id=%s)"
                (Storage_interface.Vdi.string_of x.vdi)
                x.content_id
            )
            similar_vdis
         )
      ) ;
    similars

  let mirror_clean ~dbg ~sr ~vdi ~vdis =
    let local_vdi =
      try List.find (fun x -> x.vdi = vdi) vdis
      with Not_found ->
        failwith
          (Printf.sprintf "Local VDI %s not found"
             (Storage_interface.Vdi.string_of vdi)
          )
    in
    let local_vdi =
      Storage_migrate_helper.add_to_sm_config local_vdi "mirror" "null"
    in
    let local_vdi =
      Storage_migrate_helper.remove_from_sm_config local_vdi "base_mirror"
    in
    (* Disable mirroring on the local machine *)
    let snapshot = Local.VDI.snapshot dbg sr local_vdi in
    Local.VDI.destroy dbg sr snapshot.vdi

  let snapshot_clean ~dbg ~mirror_id ~sr ~vdis =
    let snap =
      try
        Some
          (List.find
             (fun x ->
               List.mem_assoc "base_mirror" x.sm_config
               && List.assoc "base_mirror" x.sm_config = mirror_id
             )
             vdis
          )
      with _ -> None
    in
    match snap with
    | Some s ->
        debug "Found snapshot VDI: %s" (Storage_interface.Vdi.string_of s.vdi) ;
        Local.VDI.destroy dbg sr s.vdi
    | None ->
        debug "Snapshot VDI already cleaned up"

  let stop ~dbg ~mirror_id =
    (* Find the local VDI *)
    let alm = State.find_active_local_mirror mirror_id in
    match alm with
    | Some alm ->
        ( match alm.State.Send_state.remote_info with
        | Some remote_info -> (
            let sr, vdi = State.of_mirror_id mirror_id in
            let vdis, _ = Local.SR.scan2 dbg sr in
            mirror_clean ~dbg ~sr ~vdi ~vdis ;
            (* Destroy the snapshot, if it still exists *)
            snapshot_clean ~dbg ~mirror_id ~sr ~vdis ;
            let remote_url =
              Storage_utils.connection_args_of_uri
                ~verify_dest:remote_info.State.Send_state.verify_dest
                remote_info.State.Send_state.url
            in
            let module Remote = StorageAPI (Idl.Exn.GenClient (struct
              let rpc =
                Storage_utils.rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2"
                  remote_url
            end)) in
            try Remote.DATA.MIRROR.receive_cancel dbg mirror_id with _ -> ()
          )
        | None ->
            ()
        ) ;
        State.remove_local_mirror mirror_id
    | None ->
        raise
          (Storage_interface.Storage_error (Does_not_exist ("mirror", mirror_id))
          )

  let prepare ~dbg ~sr ~vdi ~dest ~local_vdi ~mirror_id ~mirror_vm ~url
      ~verify_dest =
    try
      let (module Migrate_Backend) = choose_backend dbg sr in
      let similars = similar_vdis ~dbg ~sr ~vdi in
      (* We choose receive_start based on sr, but the actual sr passed to
         receive start would be the dest sr *)
      let remote_mirror =
        Migrate_Backend.receive_start2 () ~dbg ~sr:dest ~vdi_info:local_vdi
          ~mirror_id ~similar:similars ~vm:mirror_vm ~url ~verify_dest
      in

      remote_mirror
    with e ->
      error "%s Caught error %s while preparing for SXM" __FUNCTION__
        (Printexc.to_string e) ;
      raise
        (Storage_error (Migration_preparation_failure (Printexc.to_string e)))

  let start ~task ~dbg ~sr ~vdi ~dp ~mirror_vm ~copy_vm ~live_vm ~url ~dest
      ~verify_dest =
    SXM.info
      "%s sr:%s vdi:%s dp: %s mirror_vm: %s copy_vm: %s url:%s dest:%s \
       verify_dest:%B"
      __FUNCTION__
      (Storage_interface.Sr.string_of sr)
      (Storage_interface.Vdi.string_of vdi)
      dp
      (Storage_interface.Vm.string_of mirror_vm)
      (Storage_interface.Vm.string_of copy_vm)
      url
      (Storage_interface.Sr.string_of dest)
      verify_dest ;

    let local_vdi = find_local_vdi ~dbg ~sr ~vdi in

    let mirror_id = State.mirror_id_of (sr, local_vdi.vdi) in
    debug "%s: Adding to active local mirrors before sending: id=%s"
      __FUNCTION__ mirror_id ;
    let alm =
      State.Send_state.
        {
          url
        ; dest_sr= dest
        ; source_sr= sr
        ; remote_info= None
        ; local_dp= dp
        ; tapdev= None
        ; failed= false
        ; watchdog= None
        ; vdi
        ; live_vm
        ; mirror_key= None
        }
    in
    State.add mirror_id (State.Send_op alm) ;
    debug "%s Added mirror %s to active local mirrors" __FUNCTION__ mirror_id ;

    let (module Migrate_Backend) = choose_backend dbg sr in
    try
      let remote_mirror =
        prepare ~dbg ~sr ~vdi ~dest ~local_vdi ~mirror_id ~mirror_vm ~url
          ~verify_dest
      in
      Migrate_Backend.send_start () ~dbg ~task_id:task ~dp ~sr ~vdi ~mirror_vm
        ~mirror_id ~local_vdi ~copy_vm ~live_vm ~url ~remote_mirror
        ~dest_sr:dest ~verify_dest ;

      Some (Mirror_id mirror_id)
    with
    | Storage_error (Sr_not_attached sr_uuid) ->
        error " Caught exception %s:%s. Performing cleanup."
          Api_errors.sr_not_attached sr_uuid ;
        raise (Api_errors.Server_error (Api_errors.sr_not_attached, [sr_uuid]))
        (* TODO add different types of migration failures *)
    | ( Storage_error (Migration_mirror_fd_failure reason)
      | Storage_error (Migration_mirror_snapshot_failure reason) ) as e ->
        error "%s: Caught %s: during storage migration preparation" __FUNCTION__
          reason ;
        MigrateRemote.receive_cancel2 ~dbg ~id:mirror_id ~url ~verify_dest ;
        raise e
    | Storage_error (Migration_mirror_copy_failure reason) as e ->
        error "%s: Caught %s: during storage migration copy" __FUNCTION__ reason ;
        stop ~dbg ~mirror_id ;
        raise e
    | e ->
        error "Caught %s during SXM: " (Api_errors.to_string e) ;
        stop ~dbg ~mirror_id ;
        raise e

  (* TODO mux this *)
  let stat ~dbg ~id =
    let recv_opt = State.find_active_receive_mirror id in
    let send_opt = State.find_active_local_mirror id in
    let copy_opt = State.find_active_copy id in
    let open State in
    let failed =
      match send_opt with
      | Some send_state ->
          let (module Migrate_Backend) =
            choose_backend dbg send_state.source_sr
          in
          let failed =
            Migrate_Backend.is_mirror_failed () ~dbg ~mirror_id:id
              ~sr:send_state.source_sr
          in
          send_state.failed <- failed ;
          failed
      | None ->
          false
    in
    let state =
      (match recv_opt with Some _ -> [Mirror.Receiving] | None -> [])
      @ (match send_opt with Some _ -> [Mirror.Sending] | None -> [])
      @ match copy_opt with Some _ -> [Mirror.Copying] | None -> []
    in
    if state = [] then raise (Storage_error (Does_not_exist ("mirror", id))) ;
    let src, dst =
      match (recv_opt, send_opt, copy_opt) with
      | Some receive_state, _, _ ->
          ( receive_state.Receive_state.remote_vdi
          , receive_state.Receive_state.leaf_vdi
          )
      | _, Some send_state, _ ->
          let dst_vdi =
            match send_state.Send_state.remote_info with
            | Some remote_info ->
                remote_info.Send_state.vdi
            | None ->
                Storage_interface.Vdi.of_string ""
          in
          (snd (of_mirror_id id), dst_vdi)
      | _, _, Some copy_state ->
          (snd (of_copy_id id), copy_state.Copy_state.copy_vdi)
      | _ ->
          failwith "Invalid"
    in
    {Mirror.source_vdi= src; dest_vdi= dst; state; failed}

  let list ~dbg =
    let send_ops, recv_ops, copy_ops = State.map_of () in
    let get_ids map = List.map fst map in
    let ids =
      get_ids send_ops @ get_ids recv_ops @ get_ids copy_ops
      |> Listext.List.setify
    in
    List.map (fun id -> (id, stat ~dbg ~id)) ids

  let killall ~dbg =
    let send_ops, recv_ops, copy_ops = State.map_of () in
    List.iter
      (fun (mirror_id, send_state) ->
        debug "Send in progress: %s" mirror_id ;
        List.iter log_and_ignore_exn
          [
            (fun () -> stop ~dbg ~mirror_id)
          ; (fun () ->
              Local.DP.destroy dbg send_state.State.Send_state.local_dp true
            )
          ]
      )
      send_ops ;
    List.iter
      (fun (id, copy_state) ->
        debug "Copy in progress: %s" id ;
        List.iter log_and_ignore_exn
          [
            (fun () ->
              Local.DP.destroy dbg copy_state.State.Copy_state.leaf_dp true
            )
          ; (fun () ->
              Local.DP.destroy dbg copy_state.State.Copy_state.base_dp true
            )
          ] ;
        let remote_url =
          Storage_utils.connection_args_of_uri
            ~verify_dest:copy_state.State.Copy_state.verify_dest
            copy_state.State.Copy_state.remote_url
        in
        let module Remote = StorageAPI (Idl.Exn.GenClient (struct
          let rpc =
            Storage_utils.rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url
        end)) in
        List.iter log_and_ignore_exn
          [
            (fun () ->
              Remote.DP.destroy dbg copy_state.State.Copy_state.remote_dp true
            )
          ; (fun () ->
              Remote.VDI.destroy dbg copy_state.State.Copy_state.dest_sr
                copy_state.State.Copy_state.copy_vdi
            )
          ]
      )
      copy_ops ;
    List.iter
      (fun (id, (recv_state : State.Receive_state.t)) ->
        debug "Receive in progress: %s" id ;
        log_and_ignore_exn (fun () ->
            MigrateRemote.receive_cancel2 ~dbg ~id ~url:recv_state.url
              ~verify_dest:recv_state.verify_dest
        )
      )
      recv_ops ;
    State.clear ()
end

let nbd_handler req s ?(vm = "0") sr vdi dp =
  debug "%s: vm=%s sr=%s vdi=%s dp=%s" __FUNCTION__ vm sr vdi dp ;
  let sr, vdi = Storage_interface.(Sr.of_string sr, Vdi.of_string vdi) in
  req.Http.Request.close <- true ;
  let vm = Vm.of_string vm in
  let path =
    Storage_utils.transform_storage_exn (fun () ->
        Local.DATA.import_activate "nbd" dp sr vdi vm
    )
  in
  Http_svr.headers s (Http.http_200_ok () @ ["Transfer-encoding: nbd"]) ;
  let control_fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  finally
    (fun () ->
      Unix.connect control_fd (Unix.ADDR_UNIX path) ;
      let msg = dp in
      let len = String.length msg in
      let written = Unixext.send_fd_substring control_fd msg 0 len [] s in

      if written <> len then (
        error "Failed to transfer fd to %s" path ;
        Http_svr.headers s (Http.http_404_missing ~version:"1.0" ()) ;
        req.Http.Request.close <- true
      )
    )
    (fun () -> Unix.close control_fd)

(** nbd_proxy is a http handler but will turn the http connection into an nbd connection.
It proxies the connection between the sender and the generic nbd server, as returned
by [get_nbd_server dp sr vdi vm]. *)
let import_nbd_proxy req s vm sr vdi dp =
  debug "%s: vm=%s sr=%s vdi=%s dp=%s" __FUNCTION__ vm sr vdi dp ;
  let sr, vdi = Storage_interface.(Sr.of_string sr, Vdi.of_string vdi) in
  req.Http.Request.close <- true ;
  let vm = Vm.of_string vm in
  let path =
    Storage_utils.transform_storage_exn (fun () ->
        Local.DATA.get_nbd_server "nbd" dp sr vdi vm
    )
  in
  debug "%s got nbd server path %s" __FUNCTION__ path ;
  Http_svr.headers s (Http.http_200_ok () @ ["Transfer-encoding: nbd"]) ;
  let control_fd = Unixext.open_connection_unix_fd path in
  finally
    (fun () ->
      let s' = Unix.dup s in
      let control_fd' = Unix.dup control_fd in
      debug "%s: Connected; running proxy (between fds: %d and %d)" __FUNCTION__
        (Unixext.int_of_file_descr control_fd')
        (Unixext.int_of_file_descr s') ;
      Unixext.proxy s' control_fd' ;
      debug "%s: proxy exited" __FUNCTION__
    )
    (fun () -> Unix.close control_fd)

let with_task_and_thread ~dbg f =
  let task =
    Storage_task.add tasks dbg.Debug_info.log (fun task ->
        Storage_task.set_tracing task dbg.Debug_info.tracing ;
        try f task with
        | Storage_error (Backend_error (code, params))
        | Api_errors.Server_error (code, params) ->
            raise (Storage_error (Backend_error (code, params)))
        | Storage_error (Unimplemented msg) ->
            raise (Storage_error (Unimplemented msg))
        | e ->
            raise (Storage_error (Internal_error (Printexc.to_string e)))
    )
  in
  let _ =
    Thread.create
      (fun () ->
        Storage_task.run task ;
        signal (Storage_task.id_of_handle task)
      )
      ()
  in
  Storage_task.id_of_handle task

(* The following functions acts as wrappers of the migration part of SMAPIv2. Some of
   them are just direct calling of the functions inside the Migrate module. Leave it
   this way so that they all stay in one place rather than being spread around the
   file. *)

let with_dbg ~name ~dbg f =
  Debug_info.with_dbg ~with_thread:true ~module_name:__MODULE__ ~name ~dbg f

let copy ~dbg ~sr ~vdi ~vm ~url ~dest ~verify_dest =
  debug "%s dbg: %s sr: %s vdi:%s vm: %s url: %s dest: %s verify_dest: %B"
    __FUNCTION__ dbg (s_of_sr sr) (s_of_vdi vdi) (s_of_vm vm) url (s_of_sr dest)
    verify_dest ;
  with_dbg ~name:"DATA.copy" ~dbg @@ fun dbg ->
  with_task_and_thread ~dbg (fun task ->
      Storage_smapiv1_migrate.Copy.copy_into_sr ~task ~dbg:dbg.Debug_info.log
        ~sr ~vdi ~vm ~url ~dest ~verify_dest
  )

let start ~dbg ~sr ~vdi ~dp ~mirror_vm ~copy_vm ~live_vm ~url ~dest ~verify_dest
    =
  with_dbg ~name:"DATA.MIRROR.start" ~dbg @@ fun dbg ->
  with_task_and_thread ~dbg (fun task ->
      MigrateLocal.start
        ~task:(Storage_task.id_of_handle task)
        ~dbg:dbg.Debug_info.log ~sr ~vdi ~dp ~mirror_vm ~copy_vm ~live_vm ~url
        ~dest ~verify_dest
  )

(* XXX: PR-1255: copy the xenopsd 'raise Exception' pattern *)
let stop ~dbg ~id =
  debug "%s dbg: %s id: %s" __FUNCTION__ dbg id ;
  try MigrateLocal.stop ~dbg ~mirror_id:id with
  | Storage_error (Backend_error (code, params))
  | Api_errors.Server_error (code, params) ->
      raise (Storage_error (Backend_error (code, params)))
  | e ->
      raise e

let list ~dbg =
  debug "%s dbg: %s" __FUNCTION__ dbg ;
  MigrateLocal.list ~dbg

let killall ~dbg =
  debug "%s dbg: %s" __FUNCTION__ dbg ;
  MigrateLocal.killall ~dbg

let stat ~dbg =
  debug "%s dbg: %s" __FUNCTION__ dbg ;
  MigrateLocal.stat ~dbg

let receive_finalize2 ~dbg ~mirror_id ~sr ~url ~verify_dest =
  debug "%s dbg: %s mirror_id: %s sr: %s url: %s verify_dest: %B" __FUNCTION__
    dbg mirror_id (s_of_sr sr) url verify_dest ;
  MigrateRemote.receive_finalize2 ~dbg ~mirror_id ~sr ~url ~verify_dest

let receive_cancel2 ~dbg ~id ~url ~verify_dest =
  debug "%s dbg: %s id: %s url: %s verify_dest: %B" __FUNCTION__ dbg id url
    verify_dest ;
  MigrateRemote.receive_cancel2 ~dbg ~id ~url ~verify_dest

(* The remote end of this call, SR.update_snapshot_info_dest, is implemented in
 * the SMAPIv1 section of storage_migrate.ml. It needs to access the setters
 * for snapshot_of, snapshot_time and is_a_snapshot, which we don't want to add
 * to SMAPI. *)
let update_snapshot_info_src ~dbg ~sr ~vdi ~url ~dest ~dest_vdi ~snapshot_pairs
    ~verify_dest =
  info
    "SR.update_snapshot_info_src dbg:%s sr:%s vdi:%s url:%s dest:%s \
     dest_vdi:%s snapshot_pairs:%s"
    dbg (s_of_sr sr) (s_of_vdi vdi) url (s_of_sr dest) (s_of_vdi dest_vdi)
    (List.map
       (fun (local_snapshot, dest_snapshot) ->
         Printf.sprintf "local:%s, dest:%s" (s_of_vdi local_snapshot)
           (s_of_vdi dest_snapshot)
       )
       snapshot_pairs
    |> String.concat "; "
    |> Printf.sprintf "[%s]"
    ) ;

  let remote_url = Storage_utils.connection_args_of_uri ~verify_dest url in
  let module Remote = StorageAPI (Idl.Exn.GenClient (struct
    let rpc =
      Storage_utils.rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url
  end)) in
  let local_vdis = Local.SR.scan dbg sr in
  let find_vdi ~vdi ~vdi_info_list =
    try List.find (fun x -> x.vdi = vdi) vdi_info_list
    with Not_found ->
      raise (Storage_error (Vdi_does_not_exist (Vdi.string_of vdi)))
  in
  let snapshot_pairs_for_remote =
    List.map
      (fun (local_snapshot, remote_snapshot) ->
        (remote_snapshot, find_vdi ~vdi:local_snapshot ~vdi_info_list:local_vdis)
      )
      snapshot_pairs
  in
  Remote.SR.update_snapshot_info_dest dbg dest dest_vdi
    (find_vdi ~vdi ~vdi_info_list:local_vdis)
    snapshot_pairs_for_remote

let pre_deactivate_hook ~dbg ~dp ~sr ~vdi =

  let (module Migrate_Backend) = choose_backend dbg sr in
  Migrate_Backend.pre_deactivate_hook () ~dbg ~dp ~sr ~vdi

let post_deactivate_hook ~sr ~vdi ~dp:_ =
  let open State.Send_state in
  let mirror_id = State.mirror_id_of (sr, vdi) in
  State.find_active_local_mirror mirror_id
  |> Option.iter (fun r ->
         let verify_dest =
           Option.fold ~none:false
             ~some:(fun ri -> ri.verify_dest)
             r.remote_info
         in
         let remote_url =
           Storage_utils.connection_args_of_uri ~verify_dest r.url
         in
         let module Remote = StorageAPI (Idl.Exn.GenClient (struct
           let rpc =
             Storage_utils.rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2"
               remote_url
         end)) in
         debug "%s Calling receive_finalize2 on sr %s" __FUNCTION__
           (Sr.string_of sr) ;
         log_and_ignore_exn (fun () ->
             receive_finalize2 ~dbg:"Mirror-cleanup" ~mirror_id ~sr ~url:r.url
               ~verify_dest
         ) ;
         debug "Finished calling receive_finalize2" ;
         State.remove_local_mirror mirror_id ;
         debug "Removed active local mirror: %s" mirror_id
     )
