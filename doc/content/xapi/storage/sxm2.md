---
Title: Storage Xen Motion (SXM) Protocol
---

## High level overview

The core idea of storage migration is inherently simple: We have VDIs attached to a VM, and we wish to migrate these VDIs from one SR to another. This necessarily requires us to copy the data stored in these VDIs over to the new SR, which can be a long-running process if there are gigabytes or even petabytes of them. We wish to minimise the down time of this process to allow the VM to keep running as much as possible.

This process is illustrated below. Conceptually there will be two hosts involved in this process: client -> server (local -> remote). Throughout this document, these two terms will be used interchangeably. The local host initiates the migration process, and moves the VDIs over to the server side. In practice these two hosts can be the same host, different hosts in the same pool, or different hosts on different pools. What counts as a migration is that we are moving from one  SR to another.

Storage migration works as follows:
1. Take a snapshot of a VDI that is attached to VM1. This gives us an immutable copy of the current state of the VDI, with all the data until the point we took the snapshot. This is illustrated in the diagram as a VDI and its snapshot connecting to a shared parent, which stores the shared content for the snapshot and the writable VDI from which we took the snapshot. 
2. Mirror the writable VDI to the server hosts: this means that all writes that goes to the client VDI will also be written to the mirrored VDI on the remote host.
3. Copy the immutable snapshot from our local host to the remote. 
4. Compose the mirror and the snapshot to form a single VDI 
5. Destroy the snapshot on the local host

At this stage the VDI/storage is migrated, the rest of the work would be to migrate the VM itself. Do note that the mirroring connection is still up at this point, which means all the writes will still be replicated to the remote host. This is because we haven't finished migrating the VM yet, and we keep the mirror running as we mirror the VM.

The process of VM migration would be covered separately.


## Steps break down


### preparation

Before we can start our migration process, there are a number of preparations needed to gather the information.


### mirroring

- attach and activate the VDI to be mirrored to a special domain just for SXM
- keep track of the mirror with its id
  - A special table for tracking all the mirror tasks
- mirror the vdi
- (local) snaptshot the vdi
- copy the snapshot
- (remote) compose the mirror 
- (local) destroy the snapshot




### copying


### cleanup


## Handling failures