import LeanRV64D.RiscvDecodeExt

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

def rvfi_fetch (_ : Unit) : SailM FetchResult := do
  writeReg rvfi_inst_data (Sail.BitVec.updateSubrange (← readReg rvfi_inst_data) 63 0
    (← readReg minstret))
  writeReg rvfi_pc_data (Sail.BitVec.updateSubrange (← readReg rvfi_pc_data) 63 0
    (zero_extend (m := 64) (← (get_arch_pc ()))))
  writeReg rvfi_inst_data (Sail.BitVec.updateSubrange (← readReg rvfi_inst_data) 159 152
    (zero_extend (m := 8) (privLevel_to_bits (← readReg cur_privilege))))
  writeReg rvfi_inst_data (Sail.BitVec.updateSubrange (← readReg rvfi_inst_data) 167 160
    (zero_extend (m := 8) (_get_Misa_MXL (← readReg misa))))
  match (ext_fetch_check_pc (← readReg PC) (← readReg PC)) with
  | .Ext_FetchAddr_Error e => (pure (F_Ext_Error e))
  | .Ext_FetchAddr_OK use_pc =>
    (do
      let use_pc_bits := (bits_of_virtaddr use_pc)
      bif ((bne (BitVec.access use_pc_bits 0) 0#1) || ((bne (BitVec.access use_pc_bits 1) 0#1) && (not
               (← (currentlyEnabled Ext_Zca)))))
      then (pure (F_Error ((E_Fetch_Addr_Align ()), (← readReg PC))))
      else
        (do
          match (← (translateAddr use_pc (InstructionFetch ()))) with
          | .Err (e, _) => (pure (F_Error (e, (← readReg PC))))
          | .Ok (_, _) =>
            (do
              let i ← do
                (pure (_get_RVFI_DII_Instruction_Packet_rvfi_insn (← readReg rvfi_instruction)))
              writeReg rvfi_inst_data (Sail.BitVec.updateSubrange (← readReg rvfi_inst_data) 127
                64 (zero_extend (m := 64) i))
              bif ((Sail.BitVec.extractLsb i 1 0) != (0b11 : (BitVec 2)))
              then (pure (F_RVC (Sail.BitVec.extractLsb i 15 0)))
              else
                (do
                  let PC_hi ← do (pure (BitVec.addInt (← readReg PC) 2))
                  match (ext_fetch_check_pc (← readReg PC) PC_hi) with
                  | .Ext_FetchAddr_Error e => (pure (F_Ext_Error e))
                  | .Ext_FetchAddr_OK use_pc_hi =>
                    (do
                      match (← (translateAddr use_pc_hi (InstructionFetch ()))) with
                      | .Err (e, _) => (pure (F_Error (e, (← readReg PC))))
                      | .Ok (_, _) => (pure (F_Base i)))))))

