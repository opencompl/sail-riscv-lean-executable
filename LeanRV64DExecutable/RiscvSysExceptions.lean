import LeanRV64DExecutable.RiscvErrors
import LeanRV64DExecutable.RiscvSysRegs

set_option maxHeartbeats 1_000_000_000
set_option maxRecDepth 1_000_000
set_option linter.unusedVariables false
set_option match.ignoreUnusedAlts true

open Sail

namespace LeanRV64DExecutable.Functions

open zvkfunct6
open zvk_vaesem_funct6
open zvk_vaesef_funct6
open zvk_vaesdm_funct6
open zvk_vaesdf_funct6
open zicondop
open wxfunct6
open wvxfunct6
open wvvfunct6
open wvfunct6
open wrsop
open write_kind
open word_width
open wmvxfunct6
open wmvvfunct6
open vxsgfunct6
open vxmsfunct6
open vxmfunct6
open vxmcfunct6
open vxfunct6
open vxcmpfunct6
open vvmsfunct6
open vvmfunct6
open vvmcfunct6
open vvfunct6
open vvcmpfunct6
open vregno
open vregidx
open vmlsop
open vlewidth
open visgfunct6
open virtaddr
open vimsfunct6
open vimfunct6
open vimcfunct6
open vifunct6
open vicmpfunct6
open vfwunary0
open vfunary1
open vfunary0
open vfnunary0
open vext8funct6
open vext4funct6
open vext2funct6
open uop
open sopw
open sop
open seed_opst
open rounding_mode
open ropw
open rop
open rmvvfunct6
open rivvfunct6
open rfvvfunct6
open regno
open regidx
open read_kind
open pmpAddrMatch
open physaddr
open option
open nxsfunct6
open nxfunct6
open nvsfunct6
open nvfunct6
open nisfunct6
open nifunct6
open mvxmafunct6
open mvxfunct6
open mvvmafunct6
open mvvfunct6
open mmfunct6
open maskfunct3
open iop
open fwvvmafunct6
open fwvvfunct6
open fwvfunct6
open fwvfmafunct6
open fwvffunct6
open fwffunct6
open fvvmfunct6
open fvvmafunct6
open fvvfunct6
open fvfmfunct6
open fvfmafunct6
open fvffunct6
open fregno
open fregidx
open f_un_x_op_H
open f_un_x_op_D
open f_un_rm_xf_op_S
open f_un_rm_xf_op_H
open f_un_rm_xf_op_D
open f_un_rm_fx_op_S
open f_un_rm_fx_op_H
open f_un_rm_fx_op_D
open f_un_rm_ff_op_S
open f_un_rm_ff_op_H
open f_un_rm_ff_op_D
open f_un_op_x_S
open f_un_op_f_S
open f_un_f_op_H
open f_un_f_op_D
open f_madd_op_S
open f_madd_op_H
open f_madd_op_D
open f_bin_x_op_H
open f_bin_x_op_D
open f_bin_rm_op_S
open f_bin_rm_op_H
open f_bin_rm_op_D
open f_bin_op_x_S
open f_bin_op_f_S
open f_bin_f_op_H
open f_bin_f_op_D
open extop_zbb
open extension
open exception
open ctl_result
open csrop
open cregidx
open checked_cbop
open cbop_zicbom
open cbie
open bropw_zbb
open bropw_zba
open brop_zbs
open brop_zbkb
open brop_zbb
open brop_zba
open bop
open biop_zbs
open barrier_kind
open ast
open amoop
open agtype
open WaitReason
open TrapVectorMode
open Step
open SATPMode
open Register
open Privilege
open PmpAddrMatchType
open PTW_Error
open PTE_Check
open InterruptType
open HartState
open FetchResult
open Ext_PhysAddr_Check
open Ext_FetchAddr_Check
open Ext_DataAddr_Check
open Ext_ControlAddr_Check
open ExtStatus
open ExecutionResult
open ExceptionType
open Architecture
open AccessType

def ext_check_xret_priv (p : Privilege) : Bool :=
  true

def ext_fail_xret_priv (_ : Unit) : Unit :=
  ()

def handle_trap_extension (p : Privilege) (pc : (BitVec (2 ^ 3 * 8))) (u : (Option Unit)) : Unit :=
  ()

def prepare_trap_vector (p : Privilege) (cause : (BitVec (2 ^ 3 * 8))) : SailM (BitVec (2 ^ 3 * 8)) := do
  let tvec ← (( do
    match p with
    | Machine => readReg mtvec
    | Supervisor => readReg stvec
    | User => (internal_error "riscv_sys_exceptions.sail" 25 "Invalid privilege level") ) : SailM
    Mtvec )
  match (tvec_addr tvec cause) with
  | .some epc => (pure epc)
  | none => (internal_error "riscv_sys_exceptions.sail" 29 "Invalid tvec mode")

def get_xepc (p : Privilege) : SailM (BitVec (2 ^ 3 * 8)) := do
  match p with
  | Machine => (align_pc (← readReg mepc))
  | Supervisor => (align_pc (← readReg sepc))
  | User => (internal_error "riscv_sys_exceptions.sail" 45 "Invalid privilege level")

def set_xepc (p : Privilege) (value : (BitVec (2 ^ 3 * 8))) : SailM (BitVec (2 ^ 3 * 8)) := do
  let target := (legalize_xepc value)
  match p with
  | Machine => writeReg mepc target
  | Supervisor => writeReg sepc target
  | User => (internal_error "riscv_sys_exceptions.sail" 54 "Invalid privilege level")
  (pure target)

def prepare_xret_target (p : Privilege) : SailM (BitVec (2 ^ 3 * 8)) := do
  (get_xepc p)

def get_mtvec (_ : Unit) : SailM (BitVec (2 ^ 3 * 8)) := do
  readReg mtvec

def get_stvec (_ : Unit) : SailM (BitVec (2 ^ 3 * 8)) := do
  readReg stvec

def set_mtvec (value : (BitVec (2 ^ 3 * 8))) : SailM (BitVec (2 ^ 3 * 8)) := do
  writeReg mtvec (legalize_tvec (← readReg mtvec) value)
  readReg mtvec

def set_stvec (value : (BitVec (2 ^ 3 * 8))) : SailM (BitVec (2 ^ 3 * 8)) := do
  writeReg stvec (legalize_tvec (← readReg stvec) value)
  readReg stvec

