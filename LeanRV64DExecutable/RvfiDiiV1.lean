import LeanRV64DExecutable.Prelude
import LeanRV64DExecutable.RvfiDii

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

def undefined_RVFI_DII_Execution_Packet_V1 (_ : Unit) : SailM (BitVec 704) := do
  (undefined_bitvector 704)

def Mk_RVFI_DII_Execution_Packet_V1 (v : (BitVec 704)) : (BitVec 704) :=
  v

def _get_RVFI_DII_Execution_Packet_V1_rvfi_rs1_data (v : (BitVec 704)) : (BitVec 64) :=
  (Sail.BitVec.extractLsb v 319 256)

def _update_RVFI_DII_Execution_Packet_V1_rvfi_rs1_data (v : (BitVec 704)) (x : (BitVec 64)) : (BitVec 704) :=
  (Sail.BitVec.updateSubrange v 319 256 x)

def _set_RVFI_DII_Execution_Packet_V1_rvfi_rs1_data (r_ref : (RegisterRef (BitVec 704))) (v : (BitVec 64)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_RVFI_DII_Execution_Packet_V1_rvfi_rs1_data r v)

def _get_RVFI_DII_Execution_Packet_V1_rvfi_rs2_data (v : (BitVec 704)) : (BitVec 64) :=
  (Sail.BitVec.extractLsb v 383 320)

def _update_RVFI_DII_Execution_Packet_V1_rvfi_rs2_data (v : (BitVec 704)) (x : (BitVec 64)) : (BitVec 704) :=
  (Sail.BitVec.updateSubrange v 383 320 x)

def _set_RVFI_DII_Execution_Packet_V1_rvfi_rs2_data (r_ref : (RegisterRef (BitVec 704))) (v : (BitVec 64)) : SailM Unit := do
  let r ← do (reg_deref r_ref)
  writeRegRef r_ref (_update_RVFI_DII_Execution_Packet_V1_rvfi_rs2_data r v)

def rvfi_get_exec_packet_v1 (_ : Unit) : SailM (BitVec 704) := do
  let v1_packet := (Mk_RVFI_DII_Execution_Packet_V1 (zeros (n := 704)))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_intr v1_packet
        (_get_RVFI_DII_Execution_Packet_InstMetaData_rvfi_intr (← readReg rvfi_inst_data))))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_halt v1_packet
        (_get_RVFI_DII_Execution_Packet_InstMetaData_rvfi_halt (← readReg rvfi_inst_data))))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_trap v1_packet
        (_get_RVFI_DII_Execution_Packet_InstMetaData_rvfi_trap (← readReg rvfi_inst_data))))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_insn v1_packet
        (_get_RVFI_DII_Execution_Packet_InstMetaData_rvfi_insn (← readReg rvfi_inst_data))))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_order v1_packet
        (_get_RVFI_DII_Execution_Packet_InstMetaData_rvfi_order (← readReg rvfi_inst_data))))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_pc_wdata v1_packet
        (_get_RVFI_DII_Execution_Packet_PC_rvfi_pc_wdata (← readReg rvfi_pc_data))))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_pc_rdata v1_packet
        (_get_RVFI_DII_Execution_Packet_PC_rvfi_pc_rdata (← readReg rvfi_pc_data))))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_rd_addr v1_packet
        (_get_RVFI_DII_Execution_Packet_Ext_Integer_rvfi_rd_addr (← readReg rvfi_int_data))))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_rs2_addr v1_packet
        (_get_RVFI_DII_Execution_Packet_Ext_Integer_rvfi_rs2_addr (← readReg rvfi_int_data))))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_rs1_addr v1_packet
        (_get_RVFI_DII_Execution_Packet_Ext_Integer_rvfi_rs1_addr (← readReg rvfi_int_data))))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_rd_wdata v1_packet
        (_get_RVFI_DII_Execution_Packet_Ext_Integer_rvfi_rd_wdata (← readReg rvfi_int_data))))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_rs2_data v1_packet
        (_get_RVFI_DII_Execution_Packet_Ext_Integer_rvfi_rs2_rdata (← readReg rvfi_int_data))))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_rs1_data v1_packet
        (_get_RVFI_DII_Execution_Packet_Ext_Integer_rvfi_rs1_rdata (← readReg rvfi_int_data))))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_mem_wmask v1_packet
        (Sail.BitVec.truncate
          (_get_RVFI_DII_Execution_Packet_Ext_MemAccess_rvfi_mem_wmask (← readReg rvfi_mem_data))
          8)))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_mem_rmask v1_packet
        (Sail.BitVec.truncate
          (_get_RVFI_DII_Execution_Packet_Ext_MemAccess_rvfi_mem_rmask (← readReg rvfi_mem_data))
          8)))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_mem_wdata v1_packet
        (Sail.BitVec.truncate
          (_get_RVFI_DII_Execution_Packet_Ext_MemAccess_rvfi_mem_wdata (← readReg rvfi_mem_data))
          64)))
  let v1_packet ← do
    (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_mem_rdata v1_packet
        (Sail.BitVec.truncate
          (_get_RVFI_DII_Execution_Packet_Ext_MemAccess_rvfi_mem_rdata (← readReg rvfi_mem_data))
          64)))
  (pure (_update_RVFI_DII_Execution_Packet_V1_rvfi_mem_addr v1_packet
      (_get_RVFI_DII_Execution_Packet_Ext_MemAccess_rvfi_mem_addr (← readReg rvfi_mem_data))))

