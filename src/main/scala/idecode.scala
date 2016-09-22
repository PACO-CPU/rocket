// See LICENSE for license details

package rocket

import Chisel._
import Instructions._
import uncore.constants.MemoryOpConstants._
import ALU._

abstract trait DecodeConstants
{
  val xpr64 = Y

  val decode_default: List[BitPat] =
                //               jal                                                               renf1             fence.i
                //               | jalr                                                            | renf2           |
                //         fp_val| | renx2                                                         | | renf3         |
                //         | rocc| | | renx1     s_alu1                          mem_val           | | | wfd         |
                //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | | div       |     alu_lut_sel
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | wxd     | fence |         lut_wr
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | csr   | | amo | lut_ex   |  lutl luts Precise
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | |     | | |   |  |       |  |     |  | 
                List(N,    X,X,X,X,X,X,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, X,X,X,X,X,X,CSR.X,X,X,X,  X, X,      X, X,    X, X)

  val table: Array[(BitPat, List[BitPat])]
}

class IntCtrlSigs extends Bundle {
  val legal = Bool()
  val fp = Bool()
  val rocc = Bool()
  val branch = Bool()
  val jal = Bool()
  val jalr = Bool()
  val rxs2 = Bool()
  val rxs1 = Bool()
  val sel_alu2 = Bits(width = A2_X.getWidth)
  val sel_alu1 = Bits(width = A1_X.getWidth)
  val sel_imm = Bits(width = IMM_X.getWidth)
  val alu_dw = Bool()
  val alu_fn = Bits(width = FN_X.getWidth)
  val mem = Bool()
  val mem_cmd = Bits(width = M_SZ)
  val mem_type = Bits(width = MT_SZ)
  val rfs1 = Bool()
  val rfs2 = Bool()
  val rfs3 = Bool()
  val wfd = Bool()
  val div = Bool()
  val wxd = Bool()
  val csr = Bits(width = CSR.SZ)
  val fence_i = Bool()
  val fence = Bool()
  val amo = Bool()
  val alu_lut_sel = Bool()
  val lut_ex = Bool()
  val lut_wr = Bool()
  val lutl = Bool()
  val luts = Bool()
  val precise = Bool()

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, XDecode.decode_default, table)
    val sigs = Seq(legal, fp, rocc, branch, jal, jalr, rxs2, rxs1, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd, mem_type,
                   rfs1, rfs2, rfs3, wfd, div, wxd, csr, fence_i, fence, amo, alu_lut_sel,lut_ex,lut_wr,lutl,luts, precise)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

object XDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                //               jal                                                               renf1             fence.i
                //               | jalr                                                            | renf2           |
                //         fp_val| | renx2                                                         | | renf3         |
                //         | rocc| | | renx1     s_alu1                          mem_val           | | | wfd         |
                //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | | div       |       alu_lut_sel
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | wxd     | fence |         lut_wr  luts 
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | csr   | | amo | lut_ex  |  lutl | 
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | |     | | |   |  |      |  |    |
    BNE->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SNE,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    BEQ->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SEQ,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    BLT->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLT,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    BLTU->      List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLTU,  N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    BGE->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGE,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    BGEU->      List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGEU,  N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,  N, N,     N, N,   N,Y),

    JAL->       List(Y,    N,N,N,Y,N,N,N,A2_FOUR,A1_PC,  IMM_UJ,DW_X,  FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    JALR->      List(Y,    N,N,N,N,Y,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    AUIPC->     List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),

    LB->        List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_B, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    LH->        List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_H, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    LW->        List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_W, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    LD->        List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_D, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    LBU->       List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_BU,N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    LHU->       List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_HU,N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    LWU->       List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_WU,N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SB->        List(Y,    N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_B, N,N,N,N,N,N,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SH->        List(Y,    N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_H, N,N,N,N,N,N,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SW->        List(Y,    N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_W, N,N,N,N,N,N,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SD->        List(xpr64,N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_D, N,N,N,N,N,N,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    LUTL->      List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,  Y, N,     N, Y,   N,Y),
    LUTE->      List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  Y, Y,     Y, N,   N,Y),
    LUTE2->     List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  Y, Y,     Y, N,   N,Y),
    LUTW->      List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,  Y, N,     Y, N,   N,Y),
    LUTW2->     List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,  Y, N,     Y, N,   N,Y),
    LUTS->      List(Y,    N,N,N,N,N,N,N,A2_ZERO,A1_ZERO,IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  Y, N,     N, N,   Y,Y),
    //LUTE3->     List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_X, N,N,N,N,N,N,CSR.N,N,N,N,  Y),

    AMOADD_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD, MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOXOR_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR, MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOSWAP_W-> List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOAND_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND, MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOOR_W->   List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,  MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOMIN_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN, MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOMINU_W-> List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOMAX_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX, MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOMAXU_W-> List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOADD_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD, MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOSWAP_D-> List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOXOR_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR, MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOAND_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND, MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOOR_D->   List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,  MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOMIN_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN, MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOMINU_D-> List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOMAX_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX, MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    AMOMAXU_D-> List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),

    LR_W->      List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,    MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    LR_D->      List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,    MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    SC_W->      List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,    MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),
    SC_D->      List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,    MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y,  N, N,     N, N,   N,Y),

    LUI->       List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    ADDI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SLTI ->     List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLT,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SLTIU->     List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLTU,  N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    ANDI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_AND,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    ORI->       List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_OR,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    XORI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_XOR,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SLLI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SRLI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SRAI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    ADD->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SUB->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SLT->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLT,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SLTU->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLTU,  N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    AND->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_AND,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    OR->        List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_OR,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    XOR->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_XOR,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SLL->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SL,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SRL->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SR,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SRA->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SRA,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),

    ADDIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_ADD,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SLLIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SL,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SRLIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SR,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SRAIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SRA,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    ADDW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_ADD,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    ADD_APPROX->List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,N),
    SUBW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SUB,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SUB_APPROX->List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,N),
    SLLW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SL,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SRLW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SR,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    SRAW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SRA,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),

    MUL->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MUL,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    MUL_APPROX->List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MUL,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,N),
    MULH->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULH,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    MULHU->     List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHU, N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    MULHSU->    List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHSU,N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    MULW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_MUL,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),

    DIV->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIV,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    DIVU->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIVU,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    REM->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REM,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    REMU->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REMU,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    DIVW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIV,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    DIVUW->     List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIVU,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    REMW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REM,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),
    REMUW->     List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REMU,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,  N, N,     N, N,   N,Y),

    FENCE->     List(Y,    N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,Y,N,  N, N,     N, N,   N,Y),
    FENCE_I->   List(Y,    N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,Y,N,N,  N, N,     N, N,   N,Y),

    SFENCE_VM-> List(Y,    N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.I,N,N,N,  N, N,     N, N,   N,Y),
    SCALL->     List(Y,    N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.I,N,N,N,  N, N,     N, N,   N,Y),
    SBREAK->    List(Y,    N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.I,N,N,N,  N, N,     N, N,   N,Y),
    SRET->      List(Y,    N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.I,N,N,N,  N, N,     N, N,   N,Y),
    MRTS->      List(Y,    N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.I,N,N,N,  N, N,     N, N,   N,Y),
    WFI->       List(Y,    N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.I,N,N,N,  N, N,     N, N,   N,Y),
    CSRRW->     List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.W,N,N,N,  N, N,     N, N,   N,Y),
    CSRRS->     List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.S,N,N,N,  N, N,     N, N,   N,Y),
    CSRRC->     List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.C,N,N,N,  N, N,     N, N,   N,Y),
    CSRRWI->    List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.W,N,N,N,  N, N,     N, N,   N,Y),
    CSRRSI->    List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.S,N,N,N,  N, N,     N, N,   N,Y),
    CSRRCI->    List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.C,N,N,N,  N, N,     N, N,   N,Y))
}

object FDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                //               jal                                                               renf1             fence.i
                //               | jalr                                                            | renf2           |
                //         fp_val| | renx2                                                         | | renf3         |
                //         | rocc| | | renx1     s_alu1                          mem_val           | | | wfd         |
                //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | | div       |
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | wxd     | fence
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | csr   | | amo
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | |     | | |
    FCVT_S_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_D_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,Y,N,N,CSR.N,N,N,N),
    FSGNJ_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSGNJ_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSGNJX_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSGNJX_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSGNJN_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSGNJN_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMIN_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMIN_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMAX_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMAX_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FADD_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FADD_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSUB_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSUB_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMUL_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMUL_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMADD_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FMADD_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FMSUB_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FMSUB_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FNMADD_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FNMADD_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FNMSUB_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FNMSUB_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FCLASS_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCLASS_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FMV_X_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FMV_X_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_W_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_W_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_WU_S-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_WU_D-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_L_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_L_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_LU_S-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_LU_D-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FEQ_S->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N),
    FEQ_D->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N),
    FLT_S->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N),
    FLT_D->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N),
    FLE_S->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N),
    FLE_D->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N),
    FMV_S_X->   List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FMV_D_X->   List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_S_W->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_D_W->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_S_WU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_D_WU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_S_L->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_D_L->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_S_LU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_D_LU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FLW->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_W, N,N,N,Y,N,N,CSR.N,N,N,N),
    FLD->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_D, N,N,N,Y,N,N,CSR.N,N,N,N),
    FSW->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_W, N,Y,N,N,N,N,CSR.N,N,N,N),
    FSD->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_D, N,Y,N,N,N,N,CSR.N,N,N,N))
}

object FDivSqrtDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FDIV_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FDIV_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSQRT_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSQRT_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N))
}

object RoCCDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                        //               jal                                                               renf1             fence.i
                        //               | jalr                                                            | renf2           |
                        //         fp_val| | renx2                                                         | | renf3         |
                        //         | rocc| | | renx1     s_alu1                          mem_val           | | | wfd         |
                        //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | | div       |
                        //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | wxd     | fence
                        //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | csr   | | amo
                        //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | |     | | |
    CUSTOM0->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM0_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM0_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM0_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM0_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM0_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM1->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM1_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM1_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM1_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM1_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM1_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM2->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM2_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM2_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM2_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM2_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM2_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM3->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM3_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM3_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM3_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM3_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM3_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N))
}
