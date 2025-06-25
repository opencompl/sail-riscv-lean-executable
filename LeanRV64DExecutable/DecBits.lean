import LeanRV64DExecutable.Sail.Sail
import LeanRV64DExecutable.Sail.BitVec
import LeanRV64DExecutable.Sail.IntRange
import LeanRV64DExecutable.Defs
import LeanRV64DExecutable.Specialization
import LeanRV64DExecutable.FakeReal
import LeanRV64DExecutable.RiscvExtrasExecutable

set_option maxHeartbeats 1_000_000_000
set_option maxRecDepth 1_000_000
set_option linter.unusedVariables false
set_option match.ignoreUnusedAlts true

open Sail

namespace LeanRV64DExecutable.Functions

open zvk_vsm4r_funct6
open zvk_vsha2_funct6
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

/-- Type quantifiers: k_n : Nat, k_n ≥ 0, k_n > 0 -/
def dec_bits_forwards (bv : (BitVec k_n)) : (Nat × String) :=
  ((Sail.BitVec.length bv), (Int.repr (BitVec.toNat bv)))

/-- Type quantifiers: k_n : Nat, k_n ≥ 0, k_n > 0 -/
def dec_bits_forwards_matches (bv : (BitVec k_n)) : Bool :=
  true

/-- Type quantifiers: tuple_0.1 : Nat, tuple_0.1 ≥ 0, tuple_0.1 > 0 -/
def dec_bits_backwards (tuple_0 : (Nat × String)) : (BitVec tuple_0.1) :=
  let (n, str) := tuple_0
  (parse_dec_bits n str)

/-- Type quantifiers: tuple_0.1 : Nat, tuple_0.1 ≥ 0, tuple_0.1 > 0 -/
def dec_bits_backwards_matches (tuple_0 : (Nat × String)) : Bool :=
  let (n, str) := tuple_0
  (valid_dec_bits n str)

def dec_bits_1_forwards (arg_ : (BitVec 1)) : SailM String := do
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (1, s) => (some s)
      | _ => none)
    else none)) with
  | .some result => (pure result)
  | _ =>
    (do
      assert false "Pattern match failure at unknown location"
      throw Error.Exit)

def dec_bits_1_backwards (arg_ : String) : (BitVec 1) :=
  match arg_ with
  | s => (dec_bits_backwards (1, s))

def dec_bits_1_forwards_matches (arg_ : (BitVec 1)) : Bool :=
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (1, s) => (some true)
      | _ => none)
    else none)) with
  | .some result => result
  | none =>
    (match head_exp_ with
    | _ => false)

def dec_bits_1_backwards_matches (arg_ : String) : Bool :=
  match arg_ with
  | s => true

def dec_bits_2_forwards (arg_ : (BitVec 2)) : SailM String := do
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (2, s) => (some s)
      | _ => none)
    else none)) with
  | .some result => (pure result)
  | _ =>
    (do
      assert false "Pattern match failure at unknown location"
      throw Error.Exit)

def dec_bits_2_backwards (arg_ : String) : (BitVec 2) :=
  match arg_ with
  | s => (dec_bits_backwards (2, s))

def dec_bits_2_forwards_matches (arg_ : (BitVec 2)) : Bool :=
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (2, s) => (some true)
      | _ => none)
    else none)) with
  | .some result => result
  | none =>
    (match head_exp_ with
    | _ => false)

def dec_bits_2_backwards_matches (arg_ : String) : Bool :=
  match arg_ with
  | s => true

def dec_bits_3_forwards (arg_ : (BitVec 3)) : SailM String := do
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (3, s) => (some s)
      | _ => none)
    else none)) with
  | .some result => (pure result)
  | _ =>
    (do
      assert false "Pattern match failure at unknown location"
      throw Error.Exit)

def dec_bits_3_backwards (arg_ : String) : (BitVec 3) :=
  match arg_ with
  | s => (dec_bits_backwards (3, s))

def dec_bits_3_forwards_matches (arg_ : (BitVec 3)) : Bool :=
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (3, s) => (some true)
      | _ => none)
    else none)) with
  | .some result => result
  | none =>
    (match head_exp_ with
    | _ => false)

def dec_bits_3_backwards_matches (arg_ : String) : Bool :=
  match arg_ with
  | s => true

def dec_bits_4_forwards (arg_ : (BitVec 4)) : SailM String := do
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (4, s) => (some s)
      | _ => none)
    else none)) with
  | .some result => (pure result)
  | _ =>
    (do
      assert false "Pattern match failure at unknown location"
      throw Error.Exit)

def dec_bits_4_backwards (arg_ : String) : (BitVec 4) :=
  match arg_ with
  | s => (dec_bits_backwards (4, s))

def dec_bits_4_forwards_matches (arg_ : (BitVec 4)) : Bool :=
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (4, s) => (some true)
      | _ => none)
    else none)) with
  | .some result => result
  | none =>
    (match head_exp_ with
    | _ => false)

def dec_bits_4_backwards_matches (arg_ : String) : Bool :=
  match arg_ with
  | s => true

def dec_bits_5_forwards (arg_ : (BitVec 5)) : SailM String := do
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (5, s) => (some s)
      | _ => none)
    else none)) with
  | .some result => (pure result)
  | _ =>
    (do
      assert false "Pattern match failure at unknown location"
      throw Error.Exit)

def dec_bits_5_backwards (arg_ : String) : (BitVec 5) :=
  match arg_ with
  | s => (dec_bits_backwards (5, s))

def dec_bits_5_forwards_matches (arg_ : (BitVec 5)) : Bool :=
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (5, s) => (some true)
      | _ => none)
    else none)) with
  | .some result => result
  | none =>
    (match head_exp_ with
    | _ => false)

def dec_bits_5_backwards_matches (arg_ : String) : Bool :=
  match arg_ with
  | s => true

def dec_bits_6_forwards (arg_ : (BitVec 6)) : SailM String := do
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (6, s) => (some s)
      | _ => none)
    else none)) with
  | .some result => (pure result)
  | _ =>
    (do
      assert false "Pattern match failure at unknown location"
      throw Error.Exit)

def dec_bits_6_backwards (arg_ : String) : (BitVec 6) :=
  match arg_ with
  | s => (dec_bits_backwards (6, s))

def dec_bits_6_forwards_matches (arg_ : (BitVec 6)) : Bool :=
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (6, s) => (some true)
      | _ => none)
    else none)) with
  | .some result => result
  | none =>
    (match head_exp_ with
    | _ => false)

def dec_bits_6_backwards_matches (arg_ : String) : Bool :=
  match arg_ with
  | s => true

def dec_bits_7_forwards (arg_ : (BitVec 7)) : SailM String := do
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (7, s) => (some s)
      | _ => none)
    else none)) with
  | .some result => (pure result)
  | _ =>
    (do
      assert false "Pattern match failure at unknown location"
      throw Error.Exit)

def dec_bits_7_backwards (arg_ : String) : (BitVec 7) :=
  match arg_ with
  | s => (dec_bits_backwards (7, s))

def dec_bits_7_forwards_matches (arg_ : (BitVec 7)) : Bool :=
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (7, s) => (some true)
      | _ => none)
    else none)) with
  | .some result => result
  | none =>
    (match head_exp_ with
    | _ => false)

def dec_bits_7_backwards_matches (arg_ : String) : Bool :=
  match arg_ with
  | s => true

def dec_bits_8_forwards (arg_ : (BitVec 8)) : SailM String := do
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (8, s) => (some s)
      | _ => none)
    else none)) with
  | .some result => (pure result)
  | _ =>
    (do
      assert false "Pattern match failure at unknown location"
      throw Error.Exit)

def dec_bits_8_backwards (arg_ : String) : (BitVec 8) :=
  match arg_ with
  | s => (dec_bits_backwards (8, s))

def dec_bits_8_forwards_matches (arg_ : (BitVec 8)) : Bool :=
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (8, s) => (some true)
      | _ => none)
    else none)) with
  | .some result => result
  | none =>
    (match head_exp_ with
    | _ => false)

def dec_bits_8_backwards_matches (arg_ : String) : Bool :=
  match arg_ with
  | s => true

def dec_bits_9_forwards (arg_ : (BitVec 9)) : SailM String := do
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (9, s) => (some s)
      | _ => none)
    else none)) with
  | .some result => (pure result)
  | _ =>
    (do
      assert false "Pattern match failure at unknown location"
      throw Error.Exit)

def dec_bits_9_backwards (arg_ : String) : (BitVec 9) :=
  match arg_ with
  | s => (dec_bits_backwards (9, s))

def dec_bits_9_forwards_matches (arg_ : (BitVec 9)) : Bool :=
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (9, s) => (some true)
      | _ => none)
    else none)) with
  | .some result => result
  | none =>
    (match head_exp_ with
    | _ => false)

def dec_bits_9_backwards_matches (arg_ : String) : Bool :=
  match arg_ with
  | s => true

def dec_bits_10_forwards (arg_ : (BitVec 10)) : SailM String := do
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (10, s) => (some s)
      | _ => none)
    else none)) with
  | .some result => (pure result)
  | _ =>
    (do
      assert false "Pattern match failure at unknown location"
      throw Error.Exit)

def dec_bits_10_backwards (arg_ : String) : (BitVec 10) :=
  match arg_ with
  | s => (dec_bits_backwards (10, s))

def dec_bits_10_forwards_matches (arg_ : (BitVec 10)) : Bool :=
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (10, s) => (some true)
      | _ => none)
    else none)) with
  | .some result => result
  | none =>
    (match head_exp_ with
    | _ => false)

def dec_bits_10_backwards_matches (arg_ : String) : Bool :=
  match arg_ with
  | s => true

def dec_bits_32_forwards (arg_ : (BitVec 32)) : SailM String := do
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (32, s) => (some s)
      | _ => none)
    else none)) with
  | .some result => (pure result)
  | _ =>
    (do
      assert false "Pattern match failure at unknown location"
      throw Error.Exit)

def dec_bits_32_backwards (arg_ : String) : (BitVec 32) :=
  match arg_ with
  | s => (dec_bits_backwards (32, s))

def dec_bits_32_forwards_matches (arg_ : (BitVec 32)) : Bool :=
  let head_exp_ := arg_
  match (match head_exp_ with
  | mapping0_ =>
    (bif (dec_bits_forwards_matches mapping0_)
    then
      (match (dec_bits_forwards mapping0_) with
      | (32, s) => (some true)
      | _ => none)
    else none)) with
  | .some result => result
  | none =>
    (match head_exp_ with
    | _ => false)

def dec_bits_32_backwards_matches (arg_ : String) : Bool :=
  match arg_ with
  | s => true

