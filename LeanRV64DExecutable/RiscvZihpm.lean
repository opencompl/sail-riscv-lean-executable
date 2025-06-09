import LeanRV64DExecutable.Prelude
import LeanRV64DExecutable.RiscvErrors
import LeanRV64DExecutable.RiscvXlen
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
open ISA_Format
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

def undefined_HpmEvent (_ : Unit) : SailM (BitVec 64) := do
  (undefined_bitvector 64)

def Mk_HpmEvent (v : (BitVec 64)) : (BitVec 64) :=
  v

def _get_HpmEvent_MINH (v : (BitVec 64)) : (BitVec 1) :=
  (Sail.BitVec.extractLsb v 62 62)

def _update_HpmEvent_MINH (v : (BitVec 64)) (x : (BitVec 1)) : (BitVec 64) :=
  (Sail.BitVec.updateSubrange v 62 62 x)

def _update_CountSmcntrpmf_MINH (v : (BitVec 64)) (x : (BitVec 1)) : (BitVec 64) :=
  (Sail.BitVec.updateSubrange v 62 62 x)

def _set_HpmEvent_MINH (r_ref : (RegisterRef (BitVec 64))) (v : (BitVec 1)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_HpmEvent_MINH r v)

def _get_CountSmcntrpmf_MINH (v : (BitVec 64)) : (BitVec 1) :=
  (Sail.BitVec.extractLsb v 62 62)

def _set_CountSmcntrpmf_MINH (r_ref : (RegisterRef (BitVec 64))) (v : (BitVec 1)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_CountSmcntrpmf_MINH r v)

def _get_HpmEvent_OF (v : (BitVec 64)) : (BitVec 1) :=
  (Sail.BitVec.extractLsb v 63 63)

def _update_HpmEvent_OF (v : (BitVec 64)) (x : (BitVec 1)) : (BitVec 64) :=
  (Sail.BitVec.updateSubrange v 63 63 x)

def _set_HpmEvent_OF (r_ref : (RegisterRef (BitVec 64))) (v : (BitVec 1)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_HpmEvent_OF r v)

def _get_HpmEvent_SINH (v : (BitVec 64)) : (BitVec 1) :=
  (Sail.BitVec.extractLsb v 61 61)

def _update_HpmEvent_SINH (v : (BitVec 64)) (x : (BitVec 1)) : (BitVec 64) :=
  (Sail.BitVec.updateSubrange v 61 61 x)

def _update_CountSmcntrpmf_SINH (v : (BitVec 64)) (x : (BitVec 1)) : (BitVec 64) :=
  (Sail.BitVec.updateSubrange v 61 61 x)

def _set_HpmEvent_SINH (r_ref : (RegisterRef (BitVec 64))) (v : (BitVec 1)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_HpmEvent_SINH r v)

def _get_CountSmcntrpmf_SINH (v : (BitVec 64)) : (BitVec 1) :=
  (Sail.BitVec.extractLsb v 61 61)

def _set_CountSmcntrpmf_SINH (r_ref : (RegisterRef (BitVec 64))) (v : (BitVec 1)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_CountSmcntrpmf_SINH r v)

def _get_HpmEvent_UINH (v : (BitVec 64)) : (BitVec 1) :=
  (Sail.BitVec.extractLsb v 60 60)

def _update_HpmEvent_UINH (v : (BitVec 64)) (x : (BitVec 1)) : (BitVec 64) :=
  (Sail.BitVec.updateSubrange v 60 60 x)

def _update_CountSmcntrpmf_UINH (v : (BitVec 64)) (x : (BitVec 1)) : (BitVec 64) :=
  (Sail.BitVec.updateSubrange v 60 60 x)

def _set_HpmEvent_UINH (r_ref : (RegisterRef (BitVec 64))) (v : (BitVec 1)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_HpmEvent_UINH r v)

def _get_CountSmcntrpmf_UINH (v : (BitVec 64)) : (BitVec 1) :=
  (Sail.BitVec.extractLsb v 60 60)

def _set_CountSmcntrpmf_UINH (r_ref : (RegisterRef (BitVec 64))) (v : (BitVec 1)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_CountSmcntrpmf_UINH r v)

def _get_HpmEvent_VSINH (v : (BitVec 64)) : (BitVec 1) :=
  (Sail.BitVec.extractLsb v 59 59)

def _update_HpmEvent_VSINH (v : (BitVec 64)) (x : (BitVec 1)) : (BitVec 64) :=
  (Sail.BitVec.updateSubrange v 59 59 x)

def _update_CountSmcntrpmf_VSINH (v : (BitVec 64)) (x : (BitVec 1)) : (BitVec 64) :=
  (Sail.BitVec.updateSubrange v 59 59 x)

def _set_HpmEvent_VSINH (r_ref : (RegisterRef (BitVec 64))) (v : (BitVec 1)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_HpmEvent_VSINH r v)

def _get_CountSmcntrpmf_VSINH (v : (BitVec 64)) : (BitVec 1) :=
  (Sail.BitVec.extractLsb v 59 59)

def _set_CountSmcntrpmf_VSINH (r_ref : (RegisterRef (BitVec 64))) (v : (BitVec 1)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_CountSmcntrpmf_VSINH r v)

def _get_HpmEvent_VUINH (v : (BitVec 64)) : (BitVec 1) :=
  (Sail.BitVec.extractLsb v 58 58)

def _update_HpmEvent_VUINH (v : (BitVec 64)) (x : (BitVec 1)) : (BitVec 64) :=
  (Sail.BitVec.updateSubrange v 58 58 x)

def _update_CountSmcntrpmf_VUINH (v : (BitVec 64)) (x : (BitVec 1)) : (BitVec 64) :=
  (Sail.BitVec.updateSubrange v 58 58 x)

def _set_HpmEvent_VUINH (r_ref : (RegisterRef (BitVec 64))) (v : (BitVec 1)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_HpmEvent_VUINH r v)

def _get_CountSmcntrpmf_VUINH (v : (BitVec 64)) : (BitVec 1) :=
  (Sail.BitVec.extractLsb v 58 58)

def _set_CountSmcntrpmf_VUINH (r_ref : (RegisterRef (BitVec 64))) (v : (BitVec 1)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_CountSmcntrpmf_VUINH r v)

def _get_HpmEvent_event (v : (BitVec 64)) : (BitVec 32) :=
  (Sail.BitVec.extractLsb v 31 0)

def _update_HpmEvent_event (v : (BitVec 64)) (x : (BitVec 32)) : (BitVec 64) :=
  (Sail.BitVec.updateSubrange v 31 0 x)

def _set_HpmEvent_event (r_ref : (RegisterRef (BitVec 64))) (v : (BitVec 32)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_HpmEvent_event r v)

def hpmidx_from_bits (b : (BitVec 5)) : SailM Nat := do
  let index := (BitVec.toNat b)
  assert (index ≥b 3) "unreachable HPM index"
  (pure index)

def legalize_hpmevent (v : (BitVec 64)) : SailM (BitVec 64) := do
  (pure (_update_HpmEvent_event
      (_update_HpmEvent_VUINH
        (_update_HpmEvent_VSINH
          (_update_HpmEvent_UINH
            (_update_HpmEvent_SINH
              (_update_HpmEvent_MINH
                (_update_HpmEvent_OF (Mk_HpmEvent (zeros (n := 64)))
                  (← do
                    bif (← (currentlyEnabled Ext_Sscofpmf))
                    then (pure (_get_HpmEvent_OF v))
                    else (pure (0b0 : (BitVec 1)))))
                (← do
                  bif (← (currentlyEnabled Ext_Sscofpmf))
                  then (pure (_get_HpmEvent_MINH v))
                  else (pure (0b0 : (BitVec 1)))))
              (← do
                bif ((← (currentlyEnabled Ext_Sscofpmf)) && (← (currentlyEnabled Ext_S)))
                then (pure (_get_HpmEvent_SINH v))
                else (pure (0b0 : (BitVec 1)))))
            (← do
              bif ((← (currentlyEnabled Ext_Sscofpmf)) && (← (currentlyEnabled Ext_U)))
              then (pure (_get_HpmEvent_UINH v))
              else (pure (0b0 : (BitVec 1))))) (0b0 : (BitVec 1))) (0b0 : (BitVec 1)))
      (_get_HpmEvent_event v)))

/-- Type quantifiers: index : Nat, 3 ≤ index ∧ index ≤ 31 -/
def read_mhpmcounter (index : Nat) : SailM (BitVec (2 ^ 3 * 8)) := do
  (pure (Sail.BitVec.extractLsb (GetElem?.getElem! (← readReg mhpmcounter) index) (xlen -i 1) 0))

/-- Type quantifiers: index : Nat, 3 ≤ index ∧ index ≤ 31 -/
def read_mhpmcounterh (index : Nat) : SailM (BitVec 32) := do
  (pure (Sail.BitVec.extractLsb (GetElem?.getElem! (← readReg mhpmcounter) index) 63 32))

/-- Type quantifiers: index : Nat, 3 ≤ index ∧ index ≤ 31 -/
def read_mhpmevent (index : Nat) : SailM (BitVec (2 ^ 3 * 8)) := do
  (pure (Sail.BitVec.extractLsb (GetElem?.getElem! (← readReg mhpmevent) index) (xlen -i 1) 0))

/-- Type quantifiers: index : Nat, 3 ≤ index ∧ index ≤ 31 -/
def write_mhpmcounter (index : Nat) (value : (BitVec (2 ^ 3 * 8))) : SailM Unit := do
  bif ((BitVec.access sys_writable_hpm_counters index) == 1#1)
  then
    writeReg mhpmcounter (vectorUpdate (← readReg mhpmcounter) index
      (Sail.BitVec.updateSubrange (GetElem?.getElem! (← readReg mhpmcounter) index) (xlen -i 1) 0
        value))
  else (pure ())

/-- Type quantifiers: index : Nat, 3 ≤ index ∧ index ≤ 31 -/
def write_mhpmcounterh (index : Nat) (value : (BitVec 32)) : SailM Unit := do
  bif ((BitVec.access sys_writable_hpm_counters index) == 1#1)
  then
    writeReg mhpmcounter (vectorUpdate (← readReg mhpmcounter) index
      (Sail.BitVec.updateSubrange (GetElem?.getElem! (← readReg mhpmcounter) index) 63 32 value))
  else (pure ())

/-- Type quantifiers: index : Nat, 3 ≤ index ∧ index ≤ 31 -/
def write_mhpmevent (index : Nat) (value : (BitVec (2 ^ 3 * 8))) : SailM Unit := do
  bif ((BitVec.access sys_writable_hpm_counters index) == 1#1)
  then
    writeReg mhpmevent (vectorUpdate (← readReg mhpmevent) index
      (← (legalize_hpmevent
          (Mk_HpmEvent
            (← do
              match xlen with
              | 32 =>
                (pure ((Sail.BitVec.extractLsb (GetElem?.getElem! (← readReg mhpmevent) index) 63
                      32) ++ value))
              | 64 => (pure value)
              | _ => (internal_error "riscv_zihpm.sail" 223 "Unsupported xlen"))))))
  else (pure ())

