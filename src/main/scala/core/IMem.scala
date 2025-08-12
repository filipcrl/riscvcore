package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline

/** Simple instruction memory: 32-bit words, combinational read, word addressing.
  * @param depthWords number of 32-bit words in memory
  * @param hexPath optional $readmemh file (one 32-bit hex word per line, no @ records)
  */
class IMem(depthWords: Int = 65536, hexPath: Option[String] = None) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(32.W))
    val data = Output(UInt(32.W))
  })

  val mem = Mem(depthWords, UInt(32.W))
  io.data := mem(io.addr >> 2)

  hexPath.foreach(p => loadMemoryFromFileInline(mem, p))
}