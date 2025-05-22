import LeanRV64D.RiscvPmpRegs

set_option maxHeartbeats 1_000_000_000
set_option maxRecDepth 1_000_000
set_option linter.unusedVariables false
set_option match.ignoreUnusedAlts true

open Sail

noncomputable section

namespace LeanRV64D.Functions

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
open pmpMatch
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

def pmpCheckRWX (ent : (BitVec 8)) (acc : (AccessType Unit)) : Bool :=
  match acc with
  | .Read _ => ((_get_Pmpcfg_ent_R ent) == (0b1 : (BitVec 1)))
  | .Write _ => ((_get_Pmpcfg_ent_W ent) == (0b1 : (BitVec 1)))
  | .ReadWrite _ =>
    (((_get_Pmpcfg_ent_R ent) == (0b1 : (BitVec 1))) && ((_get_Pmpcfg_ent_W ent) == (0b1 : (BitVec 1))))
  | .InstructionFetch () => ((_get_Pmpcfg_ent_X ent) == (0b1 : (BitVec 1)))

def undefined_pmpAddrMatch (_ : Unit) : SailM pmpAddrMatch := do
  (internal_pick [PMP_NoMatch, PMP_PartialMatch, PMP_Match])

/-- Type quantifiers: arg_ : Nat, 0 ≤ arg_ ∧ arg_ ≤ 2 -/
def pmpAddrMatch_of_num (arg_ : Nat) : pmpAddrMatch :=
  match arg_ with
  | 0 => PMP_NoMatch
  | 1 => PMP_PartialMatch
  | _ => PMP_Match

def num_of_pmpAddrMatch (arg_ : pmpAddrMatch) : Int :=
  match arg_ with
  | PMP_NoMatch => 0
  | PMP_PartialMatch => 1
  | PMP_Match => 2

/-- Type quantifiers: width : Nat, addr : Nat, end_ : Nat, begin : Nat, 0 ≤ begin, 0 ≤ end_, 0
  ≤ addr, 0 ≤ width -/
def pmpRangeMatch (begin : Nat) (end_ : Nat) (addr : Nat) (width : Nat) : pmpAddrMatch :=
  bif (((addr +i width) ≤b begin) || (end_ ≤b addr))
  then PMP_NoMatch
  else
    (bif ((begin ≤b addr) && ((addr +i width) ≤b end_))
    then PMP_Match
    else PMP_PartialMatch)

def pmpMatchAddr (typ_0 : physaddr) (width : (BitVec (2 ^ 3 * 8))) (ent : (BitVec 8)) (pmpaddr : (BitVec (2 ^ 3 * 8))) (prev_pmpaddr : (BitVec (2 ^ 3 * 8))) : SailM pmpAddrMatch := do
  let .Physaddr addr : physaddr := typ_0
  let addr := (BitVec.toNat addr)
  let width := (BitVec.toNat width)
  match (pmpAddrMatchType_of_bits (_get_Pmpcfg_ent_A ent)) with
  | OFF => (pure PMP_NoMatch)
  | TOR =>
    (bif (zopz0zKzJ_u prev_pmpaddr pmpaddr)
    then (pure PMP_NoMatch)
    else
      (pure (pmpRangeMatch ((BitVec.toNat prev_pmpaddr) *i 4) ((BitVec.toNat pmpaddr) *i 4) addr
          width)))
  | NA4 =>
    (do
      assert ((sys_pmp_grain ()) <b 1) "NA4 cannot be selected when PMP grain G >= 1."
      let begin := ((BitVec.toNat pmpaddr) *i 4)
      (pure (pmpRangeMatch begin (begin +i 4) addr width)))
  | NAPOT =>
    (let mask := (pmpaddr ^^^ (BitVec.addInt pmpaddr 1))
    let begin_words := (BitVec.toNat (pmpaddr &&& (Complement.complement mask)))
    let end_words := ((begin_words +i (BitVec.toNat mask)) +i 1)
    (pure (pmpRangeMatch (begin_words *i 4) (end_words *i 4) addr width)))

def undefined_pmpMatch (_ : Unit) : SailM pmpMatch := do
  (internal_pick [PMP_Success, PMP_Continue, PMP_Fail])

/-- Type quantifiers: arg_ : Nat, 0 ≤ arg_ ∧ arg_ ≤ 2 -/
def pmpMatch_of_num (arg_ : Nat) : pmpMatch :=
  match arg_ with
  | 0 => PMP_Success
  | 1 => PMP_Continue
  | _ => PMP_Fail

def num_of_pmpMatch (arg_ : pmpMatch) : Int :=
  match arg_ with
  | PMP_Success => 0
  | PMP_Continue => 1
  | PMP_Fail => 2

def pmpMatchEntry (addr : physaddr) (width : (BitVec (2 ^ 3 * 8))) (acc : (AccessType Unit)) (priv : Privilege) (ent : (BitVec 8)) (pmpaddr : (BitVec (2 ^ 3 * 8))) (prev_pmpaddr : (BitVec (2 ^ 3 * 8))) : SailM pmpMatch := do
  match (← (pmpMatchAddr addr width ent pmpaddr prev_pmpaddr)) with
  | PMP_NoMatch => (pure PMP_Continue)
  | PMP_PartialMatch => (pure PMP_Fail)
  | PMP_Match =>
    (bif ((pmpCheckRWX ent acc) || ((priv == Machine) && (not (pmpLocked ent))))
    then (pure PMP_Success)
    else (pure PMP_Fail))

def accessToFault (acc : (AccessType Unit)) : ExceptionType :=
  match acc with
  | .Read _ => (E_Load_Access_Fault ())
  | .Write _ => (E_SAMO_Access_Fault ())
  | .ReadWrite _ => (E_SAMO_Access_Fault ())
  | .InstructionFetch () => (E_Fetch_Access_Fault ())

/-- Type quantifiers: width : Nat, 0 < width ∧ width ≤ max_mem_access -/
def pmpCheck (addr : physaddr) (width : Nat) (acc : (AccessType Unit)) (priv : Privilege) : SailM (Option ExceptionType) := SailME.run do
  let width : xlenbits := (to_bits (l := ((2 ^i 3) *i 8)) width)
  let loop_i_lower := 0
  let loop_i_upper := 63
  let mut loop_vars := ()
  for i in [loop_i_lower:loop_i_upper:1]i do
    let () := loop_vars
    loop_vars ← do
      let prev_pmpaddr ← do
        bif (i >b 0)
        then (pmpReadAddrReg (i -i 1))
        else (pure (zeros (n := ((2 ^i 3) *i 8))))
      match (← (pmpMatchEntry addr width acc priv (GetElem?.getElem! (← readReg pmpcfg_n) i)
          (← (pmpReadAddrReg i)) prev_pmpaddr)) with
      | PMP_Success => SailME.throw (none : (Option ExceptionType))
      | PMP_Fail => SailME.throw ((some (accessToFault acc)) : (Option ExceptionType))
      | PMP_Continue => (pure ())
  (pure loop_vars)
  bif (priv == Machine)
  then (pure none)
  else (pure (some (accessToFault acc)))

def reset_pmp (_ : Unit) : SailM Unit := do
  assert (((sys_pmp_count ()) == 0) || ((((sys_pmp_count ()) == 16) || (((sys_pmp_count ()) == 64) : Bool)) : Bool)) "sys_pmp_count() must be 0, 16, or 64"
  let loop_i_lower := 0
  let loop_i_upper := 63
  let mut loop_vars := ()
  for i in [loop_i_lower:loop_i_upper:1]i do
    let () := loop_vars
    loop_vars ← do
      writeReg pmpcfg_n (vectorUpdate (← readReg pmpcfg_n) i
        (_update_Pmpcfg_ent_L
          (_update_Pmpcfg_ent_A (GetElem?.getElem! (← readReg pmpcfg_n) i)
            (pmpAddrMatchType_to_bits OFF)) (0b0 : (BitVec 1))))
  (pure loop_vars)

