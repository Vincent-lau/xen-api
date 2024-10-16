(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(** The main callback function.

    @group API Messaging
*)

(** Actions module *)
module Actions = struct
  (** The DebugVersion throws a NotImplemented exception for everything
      		by default.  The ReleaseVersion is missing all the fields;
      		so server will not compile unless everything is overridden *)

  module Task = Xapi_task
  module Session = Xapi_session
  module Auth = Xapi_auth
  module Subject = Xapi_subject
  module Role = Xapi_role
  module Event = Xapi_event
  module Alert = Xapi_alert

  module VM = struct include Xapi_vm include Xapi_vm_migrate end

  module VM_metrics = struct end

  module VM_guest_metrics = struct end

  module VMPP = Xapi_vmpp
  module VMSS = Xapi_vmss
  module VM_appliance = Xapi_vm_appliance
  module VM_group = Xapi_vm_group
  module DR_task = Xapi_dr_task

  module LVHD = struct end

  module Host = Xapi_host
  module Host_crashdump = Xapi_host_crashdump
  module Pool = Xapi_pool
  module Pool_update = Xapi_pool_update
  module Pool_patch = Xapi_pool_patch
  module Host_patch = Xapi_host_patch

  module Host_metrics = struct end

  module Host_cpu = struct end

  module Network = Xapi_network
  module VIF = Xapi_vif

  module VIF_metrics = struct end

  module PIF = Xapi_pif

  module PIF_metrics = struct end

  module SR = Xapi_sr
  module SM = Xapi_sm
  (* module Sm_feature = struct end *)

  module VDI = struct
    include Xapi_vdi

    let pool_migrate = Xapi_vm_migrate.vdi_pool_migrate
  end

  module VBD = Xapi_vbd

  module VBD_metrics = struct end

  module Crashdump = Xapi_crashdump
  module PBD = Xapi_pbd

  module Data_source = struct end

  module VTPM = Xapi_vtpm

  let not_implemented x =
    raise (Api_errors.Server_error (Api_errors.not_implemented, [x]))

  module Console = struct
    let create ~__context ~other_config:_ = not_implemented "Console.create"

    let destroy ~__context ~self:_ = not_implemented "Console.destroy"
  end

  module Bond = Xapi_bond
  module VLAN = Xapi_vlan
  module User = Xapi_user
  module Blob = Xapi_blob
  module Message = Xapi_message
  module Secret = Xapi_secret
  module Tunnel = Xapi_tunnel
  module PCI = Xapi_pci
  module PGPU = Xapi_pgpu
  module GPU_group = Xapi_gpu_group
  module VGPU = Xapi_vgpu
  module VGPU_type = Xapi_vgpu_type
  module PVS_site = Xapi_pvs_site
  module PVS_server = Xapi_pvs_server
  module PVS_proxy = Xapi_pvs_proxy
  module PVS_cache_storage = Xapi_pvs_cache_storage

  module Feature = struct end

  module SDN_controller = Xapi_sdn_controller

  module Vdi_nbd_server_info = struct end

  module Probe_result = struct end

  module Sr_stat = struct end

  module PUSB = Xapi_pusb
  module USB_group = Xapi_usb_group
  module VUSB = Xapi_vusb
  module Network_sriov = Xapi_network_sriov
  module Cluster = Xapi_cluster
  module Cluster_host = Xapi_cluster_host
  module Certificate = Certificates
  module Diagnostics = Xapi_diagnostics
  module Repository = Repository
  module Observer = Xapi_observer
end

(** Use the server functor to make an XML-RPC dispatcher. *)
module Forwarder = Message_forwarding.Forward (Actions)

(** Here are the functions to forward calls made on the unix domain socket on a slave to a master *)
module D = Debug.Make (struct
  let name = "api_server"
end)

(** Forward a call to the master *)
let forward req call is_json =
  let open Xmlrpc_client in
  let transport =
    SSL
      ( SSL.make ~use_stunnel_cache:true ~verify_cert:(Stunnel_client.pool ()) ()
      , Pool_role.get_master_address ()
      , !Constants.https_port
      )
  in
  let rpc = if is_json then JSONRPC_protocol.rpc else XMLRPC_protocol.rpc in
  rpc ~srcstr:"xapi" ~dststr:"xapi" ~transport
    ~http:{req with Http.Request.frame= true}
    call

(* Whitelist of functions that do *not* get forwarded to the master (e.g. session.login_with_password) *)
(* !!! Note, this only blocks synchronous calls. As is it happens, all the calls we want to block right now are only
   synchronous. However, we'd probably want to change this is the list starts getting longer. *)
let whitelist =
  List.map
    (fun (obj, msg) -> Datamodel_utils.wire_name ~sync:true obj msg)
    Datamodel.whitelist

let emergency_call_list =
  List.map
    (fun (obj, msg) -> Datamodel_utils.wire_name ~sync:true obj msg)
    Datamodel.emergency_calls

let is_himn_req req =
  match req.Http.Request.host with
  | Some h -> (
    match Xapi_mgmt_iface.himn_addr () with
    | Some himn ->
        himn = h
    | None ->
        false
  )
  | None ->
      false

(* The API does not use the error.code and only retains it for compliance with
   the JSON-RPC v2.0 specs. We set this always to a non-zero value because
   some JsonRpc clients consider error.code 0 as no error*)
let error_code_lit = 1L

let json_of_error_object ?(data = None) code message =
  let data_json = match data with Some d -> [("data", d)] | None -> [] in
  Rpc.Dict
    ([("code", Rpc.Int code); ("message", Rpc.String message)] @ data_json)

(* debug(fmt "response = %s" response); *)

module Unixext = Xapi_stdext_unix.Unixext
