/*
 * Copyright (c) 2022 xvanc and contributors
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

//  00  NVM Command Set Identify Namespace data structure
//  01  Identify Controller data structure
//  02  Active Namespace ID list
//  03  Namespace Identification Descriptor list
//  04  NVM Set list
//  05  I/O Command Set specific Identify Namespace data structure
//  06  I/O Command Set specific Identify Controller data structure
//  07  I/O Command Set specific Active Namespace ID list
//  08  I/O Command Set independent Identify Namespace data structure
//  10  Allocated Namespace ID list
//  11  Identify Namespace data structure for an Allocated Namespace ID
//  12  Namespace Attached Controller list
//  13  Controller list
//  14  Primary Controller Capabilities data structure
//  15  Secondary Controller list
//  16  Namespace Granularity list
//  17  UUID list
//  18  Domain list
//  19  Endurance Group list
//  1a  I/O Command Set specific Allocated Namespace ID list
//  1b  I/O Command Set specific Identify Namespace data structure for an Allocated Namespace ID
//  1c  Identify I/O Command Set data structure

use super::{controller::Controller, queue::CompletionStatus};
use crate::{
    dev::block::nvme::controller::{AdminCommand, DataPtr},
    size_of,
};
use core::{marker::PhantomData, mem, ops};
use libsa::endian::{u16_le, u32_le, u64_le};
use uuid::Uuid;

/// Types which represent a data structure returned by the [`AdminCommand::Identify`] command.
pub trait Identify {
    /// Controller or Namespace Structure
    const CNS: u8;

    /// Controller Identifier
    const CNTID: u8 = 0;

    /// Command Set Identifier
    const CSI: u8 = 0;

    /// CNS Specific Identifier
    const CNSSI: u8 = 0;

    /// UUID Index
    const UUID_INDEX: u8 = 0;
}

pub fn identify<I: Identify>(
    ctlr: &mut Controller,
    nsid: Option<u32>,
) -> Result<Box<I>, CompletionStatus> {
    debug_assert!(size_of!(I) <= 4096);
    debug_assert!(mem::align_of::<I>() <= 4096);

    let mut uninit = Box::<I>::new_uninit();

    ctlr.admin_command(AdminCommand::Identify)
        .namespace_id(nsid.unwrap_or(0))
        .data_ptr(DataPtr::Prp(uninit.as_mut_ptr().addr() as u64, 0))
        .cdw10(I::CNS as u32 | ((I::CNTID as u32) << 16))
        .cdw11(((I::CSI as u32) << 24) | I::CNSSI as u32)
        .cdw14(I::UUID_INDEX as u32 & 0x7f)
        .execute()?;

    Ok(unsafe { uninit.assume_init() })
}

// ----------------------------------------------------------------------------

/// Identify Controller
#[repr(C)]
#[derive(Debug)]
pub struct IdentifyController {
    /// PCI Vendor ID
    vid: u16_le,
    /// PCI Subsystem Vendor ID
    ssvid: u16_le,
    /// Serial Number
    sn: [u8; 20],
    /// Model Number
    mn: [u8; 40],
    /// Firmware Revision
    fr: u64_le,
    /// Recommended Arbitration Burst
    rab: u8,
    /// IEEE OUI Identifier
    ieee: [u8; 3],
    /// Controller Multi-Path I/O and Namespace Sharing Capabilities
    cmic: u8,
    /// Maximum Data Transfer Size
    mdts: u8,
    /// Controller ID
    cntlid: u16_le,
    /// Version
    ver: u32_le,
    /// RTD3 Resume Latency
    rtd3r: u32_le,
    rtd3e: u32_le,
    /// Optional Asynnchronous Events Supported
    oaes: u32_le,
    /// Controller Attributes
    ctratt: u32_le,
    /// Read Recovery Levels Suupported
    rrls: u16_le,
    reserved0: [u8; 9],
    /// Controller Type
    cntrltype: u8,
    /// FRU Globally Unique Identifier
    fguid: [u8; 16],
    /// Command Retry Delay Time
    crdt: [u16_le; 3],
    reserved1: [u8; 106],
    reserved2: [u8; 13],
    /// NVM Subsystem Report
    nvmsr: u8,
    /// VPD Write Cycle Information
    vwci: u8,
    /// Management Endpoint Capabilites
    mec: u8,
    /// Optional Admin Command Support
    oacs: u16_le,
    /// Abort Command Limit
    acl: u8,
    /// Asynchronous Event Request Limit
    aerl: u8,
    /// Firmware Updates
    frmw: u8,
    /// Log Page Attributes
    lpa: u8,
    /// Error Log Page Entries
    elpe: u8,
    /// Number of Power States Support
    npss: u8,
    avscc: u8,
    apsta: u8,
    wctemp: u16_le,
    cctemp: u16_le,
    mtfa: u16_le,
    hmpre: u32_le,
    hmmin: u32_le,
    tnvmcap: [u8; 16],
    unvmcap: [u8; 16],
    rpmbs: u32_le,
    edstt: u16_le,
    dsto: u8,
    fwug: u8,
    kas: u16_le,
    hctma: u16_le,
    mntmt: u16_le,
    mxtmt: u16_le,
    sanicap: u32_le,
    hmminds: u32_le,
    hmmaxd: u16_le,
    nsetidmax: u16_le,
    endgidmax: u16_le,
    anatt: u8,
    anacap: u8,
    anagrpmax: u32_le,
    nanagrpid: u32_le,
    pels: u32_le,
    domain_identifier: u16_le,
    reserved3: [u8; 10],
    megcap: [u8; 16],
    reserved4: [u8; 128],
    sqes: u8,
    cqes: u8,
    maxcmd: u16_le,
    nn: u32_le,
    oncs: u16_le,
    fuses: u16_le,
    fna: u8,
    vwc: u8,
    awun: u16_le,
    awupf: u16_le,
    icsvscc: u8,
    nwpc: u8,
    acwu: u16_le,
    cdfs: u16_le,
    sgls: u32_le,
    mnan: u32_le,
    maxdna: [u8; 16],
    maxcna: u32_le,
    reserved5: [u8; 204],
    /// NVM Subsystem NVMe Qualified Name
    ///
    /// This field contains a null-terminated UTF-8 string.
    subnqn: [u8; 256],
    reserved6: [u8; 768],
    ioccsz: u32_le,
    iorcsz: u32_le,
    icdoff: u16_le,
    fcatt: u8,
    msdbd: u8,
    ofcs: u16_le,
    reserved7: [u8; 242],
    psd: [[u8; 32]; 32],
    vendor_specific_data: [u8; 1024],
}

const _: () = {
    assert!(size_of!(IdentifyController) == 4096);
};

impl Identify for IdentifyController {
    const CNS: u8 = 0x01;
}

// ----------------------------------------------------------------------------

#[repr(transparent)]
pub struct NamespaceList([u32_le; 1024]);

impl ops::Deref for NamespaceList {
    type Target = [u32_le];

    fn deref(&self) -> &Self::Target {
        let mut len = 0;
        while self.0[len].get() != 0 {
            len += 1;
        }
        &self.0[..len]
    }
}

// ----------------------------------------------------------------------------

/// Active Namepace ID list
pub struct ActiveNamespaceIdList(NamespaceList);

impl Identify for ActiveNamespaceIdList {
    const CNS: u8 = 0x02;
}

impl ops::Deref for ActiveNamespaceIdList {
    type Target = NamespaceList;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// ----------------------------------------------------------------------------

/// I/O Command Set independent Identify Namespace
#[repr(C)]
pub struct IdentifyNamespace {
    nsfeat: u8,
    nmic: u8,
    rescap: u8,
    fpi: u8,
    anagrpid: u32_le,
    nsattr: u8,
    reserved0: u8,
    nvmsetid: u16_le,
    endgid: u16_le,
    nstat: u8,
    reserved: [u8; 49081],
}

impl Identify for IdentifyNamespace {
    const CNS: u8 = 0x08;
}

// ----------------------------------------------------------------------------

/// Allocated Namepace ID list
pub struct AllocatedNamespaceIdList(NamespaceList);

impl Identify for AllocatedNamespaceIdList {
    const CNS: u8 = 0x10;
}

// ----------------------------------------------------------------------------
//  I/O Command Set specific data structures
//

/// An I/O Command Set
pub trait IoCommandSet {
    const CSI: u8;
}

/// An I/O Command Set which provides an I/O Command Set specific Identify Namespace data structure
pub trait IoCommandSetWithIdentifyNamespace: IoCommandSet {
    type Data: Sized;
}

/// The NVM I/O Command Set
pub struct NvmCommandSet;

impl IoCommandSet for NvmCommandSet {
    const CSI: u8 = 0x00;
}

// ----------------------------------------------------------------------------

/// I/O Command Set specific Active Namespace ID list
#[repr(transparent)]
pub struct IoCommandSetActiveNamespaceIdList<C: IoCommandSet> {
    list: NamespaceList,
    command_set: PhantomData<C>,
}

impl<C: IoCommandSet> Identify for IoCommandSetActiveNamespaceIdList<C> {
    const CNS: u8 = 0x07;
    const CSI: u8 = C::CSI;
}

impl<C: IoCommandSet> ops::Deref for IoCommandSetActiveNamespaceIdList<C> {
    type Target = NamespaceList;

    fn deref(&self) -> &Self::Target {
        &self.list
    }
}

// ----------------------------------------------------------------------------

#[repr(transparent)]
pub struct IoCommandSetIdentifyNamespace<C>
where
    C: IoCommandSetWithIdentifyNamespace,
{
    data: C::Data,
}

impl<C> Identify for IoCommandSetIdentifyNamespace<C>
where
    C: IoCommandSetWithIdentifyNamespace,
{
    const CNS: u8 = 0x05;
    const CSI: u8 = C::CSI;
}

impl<C> ops::Deref for IoCommandSetIdentifyNamespace<C>
where
    C: IoCommandSetWithIdentifyNamespace,
{
    type Target = C::Data;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

// ----------------------------------------------------------------------------
//      NVM Command Set

#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct LbaFormat(u32_le);

impl LbaFormat {
    pub fn metadata_size(self) -> usize {
        self.0.get() as usize & 0xffff
    }

    pub fn lba_data_size(self) -> usize {
        let lbads = (self.0.get() >> 16) as usize & 0xff;
        1 << lbads
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct ExtendedLbaFormat(u32_le);

#[repr(C)]
pub struct NvmCommandSetIdentifyNamespace {
    logical_block_storage_tag_mask: u64_le,
    protection_information_capabilities: u8,
    reserved0: [u8; 3],
    extended_lba_formats: [ExtendedLbaFormat; 64],
}

impl IoCommandSetWithIdentifyNamespace for NvmCommandSet {
    type Data = NvmCommandSetIdentifyNamespace;
}

// ----------------------------------------------------------------------------

bitflags::bitflags! {
    #[repr(transparent)]
    pub struct NamespaceFeatures : u8 {
        const THINP    = 1 << 0;
        const NSABP    = 1 << 1;
        const DAE      = 1 << 2;
        const UIDREUSE = 1 << 3;
        const OPTPERF  = 1 << 4;
    }
}

impl Identify for NvmIdentifyNamespace {
    const CNS: u8 = 0x00;
}

#[repr(C)]
#[derive(Debug)]
pub struct NvmIdentifyNamespace {
    /// Namespace Size
    ///
    /// Reports the total number of logical blocks in the namespace.
    pub nsze: u64_le,
    /// Namespace Capacity
    ///
    /// Reports the maximum number of logical blocks which may be allocated in the namespace.
    pub ncap: u64_le,
    /// Namespace Utilization
    ///
    /// Reports the number of logical blocks currently allocated in the namespace.
    pub nuse: u64_le,
    /// Namespace Features
    pub nsfeat: NamespaceFeatures,
    /// Number of LBA Formats
    ///
    ///
    pub nlbaf: u8,
    /// Formatted LBA Size
    pub flbas: u8,
    /// Metadata Capabilities
    pub mc: u8,
    /// End-to-end Data Protection Capabilities
    pub dpc: u8,
    /// End-to-end Data Protection Type Settings
    pub dps: u8,
    /// Namespace Multi-path I/O and Namespace Sharing Capabilities
    pub nmic: u8,
    /// Reservation Capabilities
    pub rescap: u8,
    /// Format Progress Indicator
    pub fpi: u8,
    /// Deallocate Logical Block Features
    pub dlfeat: u8,
    /// Namespace Atomic Write Unit Normal
    pub nawun: u16_le,
    /// Namespace Atomic Write Unit Power Fail
    pub nawupf: u16_le,
    /// Namespace Atomic Compare and Write Unit
    pub nacwu: u16_le,
    /// Namespace Atomic Boundary Normal Size
    pub nabsn: u16_le,
    /// Namespace Atomic Boundary Offset
    pub nabo: u16_le,
    /// Namespace Atomic Boundary Size Power Fail
    pub nabspf: u16_le,
    /// Namespace Optimal I/O Boundary
    pub noiob: u16_le,
    /// NVM Capacity
    pub nvmcap: [u64_le; 2],
    /// Namespace Preferred Write Granularity
    pub npwg: u16_le,
    /// Namespace Preferred Write Alignment
    pub npwa: u16_le,
    /// Namespace Preferred Deallocate Granularity
    pub npdg: u16_le,
    /// Namespace Preferred Deallocate Alignment
    pub npda: u16_le,
    /// Namespace Optimal Write Size
    pub nows: u16_le,
    /// Maximum Single Source Range Length
    pub mssrl: u16_le,
    /// Maximum Copy Length
    pub mcl: u32_le,
    /// Maximum Source Range Count
    pub msrc: u8,
    pub _rsvd0: [u8; 11],
    /// ANA Group Identifier
    pub anagrpid: u32_le,
    pub _rsvd1: [u8; 3],
    /// Namespace Attributes
    pub nsattr: u8,
    /// NVM Set Identifier
    pub nvmsetid: u16_le,
    /// Endurance Group Identifier (ENDGID)
    pub endgid: u16_le,
    /// Namespace Globally Unique Identifier (NGUID)
    pub nguid: Uuid,
    /// IEEE Extended Unique Identifier (EUI64)
    pub eui64: u64_le,
    /// LBA Format Support
    pub lbaf: [LbaFormat; 64],
}
