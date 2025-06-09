import LeanRV64DExecutable.Prelude

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

def undefined_seed_opst (_ : Unit) : SailM seed_opst := do
  (internal_pick [BIST, ES16, WAIT, DEAD])

/-- Type quantifiers: arg_ : Nat, 0 ≤ arg_ ∧ arg_ ≤ 3 -/
def seed_opst_of_num (arg_ : Nat) : seed_opst :=
  match arg_ with
  | 0 => BIST
  | 1 => ES16
  | 2 => WAIT
  | _ => DEAD

def num_of_seed_opst (arg_ : seed_opst) : Int :=
  match arg_ with
  | BIST => 0
  | ES16 => 1
  | WAIT => 2
  | DEAD => 3

def opst_code_forwards (arg_ : seed_opst) : (BitVec 2) :=
  match arg_ with
  | BIST => (0b00 : (BitVec 2))
  | WAIT => (0b01 : (BitVec 2))
  | ES16 => (0b10 : (BitVec 2))
  | DEAD => (0b11 : (BitVec 2))

def opst_code_backwards (arg_ : (BitVec 2)) : seed_opst :=
  let b__0 := arg_
  bif (b__0 == (0b00 : (BitVec 2)))
  then BIST
  else
    (bif (b__0 == (0b01 : (BitVec 2)))
    then WAIT
    else
      (bif (b__0 == (0b10 : (BitVec 2)))
      then ES16
      else DEAD))

def opst_code_forwards_matches (arg_ : seed_opst) : Bool :=
  match arg_ with
  | BIST => true
  | WAIT => true
  | ES16 => true
  | DEAD => true

def opst_code_backwards_matches (arg_ : (BitVec 2)) : Bool :=
  let b__0 := arg_
  bif (b__0 == (0b00 : (BitVec 2)))
  then true
  else
    (bif (b__0 == (0b01 : (BitVec 2)))
    then true
    else
      (bif (b__0 == (0b10 : (BitVec 2)))
      then true
      else
        (bif (b__0 == (0b11 : (BitVec 2)))
        then true
        else false)))

def read_seed_csr (_ : Unit) : SailM (BitVec (2 ^ 3 * 8)) := do
  let reserved_bits : (BitVec 6) := (0b000000 : (BitVec 6))
  let custom_bits : (BitVec 8) := (0x00 : (BitVec 8))
  let seed ← (( do (get_16_random_bits ()) ) : SailM (BitVec 16) )
  (pure (zero_extend (m := ((2 ^i 3) *i 8))
      ((opst_code_forwards ES16) ++ (reserved_bits ++ (custom_bits ++ seed)))))

def write_seed_csr (_ : Unit) : (BitVec (2 ^ 3 * 8)) :=
  (zeros (n := ((2 ^i 3) *i 8)))

