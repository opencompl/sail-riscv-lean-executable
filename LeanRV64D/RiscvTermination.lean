import LeanRV64D.Sail.Sail
import LeanRV64D.Sail.BitVec
import LeanRV64D.Sail.IntRange
import LeanRV64D.Defs
import LeanRV64D.Specialization
import LeanRV64D.FakeReal
import LeanRV64D.RiscvExtras

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

def compressed_measure (instr : ast) : Int :=
  match instr with
  | .C_ADDI4SPN (rdc, nzimm) => 1
  | .C_LW (uimm, rsc, rdc) => 1
  | .C_LD (uimm, rsc, rdc) => 1
  | .C_SW (uimm, rsc1, rsc2) => 1
  | .C_SD (uimm, rsc1, rsc2) => 1
  | .C_ADDI (nzi, rsd) => 1
  | .C_JAL imm => 1
  | .C_ADDIW (imm, rsd) => 1
  | .C_LI (imm, rd) => 1
  | .C_ADDI16SP imm => 1
  | .C_LUI (imm, rd) => 1
  | .C_SRLI (shamt, rsd) => 1
  | .C_SRAI (shamt, rsd) => 1
  | .C_ANDI (imm, rsd) => 1
  | .C_SUB (rsd, rs2) => 1
  | .C_XOR (rsd, rs2) => 1
  | .C_OR (rsd, rs2) => 1
  | .C_AND (rsd, rs2) => 1
  | .C_SUBW (rsd, rs2) => 1
  | .C_ADDW (rsd, rs2) => 1
  | .C_J imm => 1
  | .C_BEQZ (imm, rs) => 1
  | .C_BNEZ (imm, rs) => 1
  | .C_SLLI (shamt, rsd) => 1
  | .C_LWSP (uimm, rd) => 1
  | .C_LDSP (uimm, rd) => 1
  | .C_SWSP (uimm, rs2) => 1
  | .C_SDSP (uimm, rs2) => 1
  | .C_JR rs1 => 1
  | .C_JALR rs1 => 1
  | .C_MV (rd, rs2) => 1
  | .C_EBREAK tt => 1
  | .C_ADD (rsd, rs2) => 1
  | .C_FLDSP (uimm, rd) => 1
  | .C_FSDSP (uimm, rs2) => 1
  | .C_FLD (uimm, rsc, rdc) => 1
  | .C_FSD (uimm, rsc1, rsc2) => 1
  | .C_FLWSP (imm, rd) => 1
  | .C_FSWSP (uimm, rs2) => 1
  | .C_FLW (uimm, rsc, rdc) => 1
  | .C_FSW (uimm, rsc1, rsc2) => 1
  | _ => 0

