RISC-V RV32I Core with Caches, AXI, MIG Top, and Peripherals
===========================================================

Overview
- A simple RV32I CPU core built in Chisel with instruction/data caches and a 512‑bit AXI memory port.
- An FPGA top (`MigTop`) that wraps Xilinx MIG (DDR3) and exposes a 4‑bit LED peripheral.
- Peripherals are memory‑mapped when the highest address bit is 1; LED is at the least‑significant one‑hot slot.

Architecture
- Core: `src/main/scala/core/Core.scala`
  - RV32I pipeline with `MemPort` instruction/data interfaces.
  - Uses `Control`, `Alu`, `ImmGen`, `BrCond`, and a 32×32 register file.
- Cache + AXI: `src/main/scala/core/Cache.scala`
  - Blocking I$/D$ with line fill/beat write, 64‑byte lines, 512‑bit AXI.
  - `AxiArbiter` arbitrates I$/D$ to a single AXI master.
- Top: `src/main/scala/core/Top.scala`
  - Instantiates Core, I$, D$, AXI arbiter.
  - Routes D$ requests to either cache or peripherals based on `addr[31]`.
  - Exposes `io.leds` (4‑bit) from peripherals.
- Peripherals: `src/main/scala/core/Peripherials.scala`
  - `Peripherials` multiplexes devices by one‑hot `addr[10:3]` and forwards `addr[2:0]`.
  - `LEDPeripheral` drives 4 LEDs; RW at subaddress `000`.
- FPGA Top: `src/main/scala/core/MigTop.scala`
  - Wraps Xilinx MIG 7‑series (BlackBox), clocks/resets, AXI wiring, and exports `leds[3:0]`.

Peripheral Address Map
- Select peripherals: `addr[31] = 1`.
- One‑hot select: `addr[10:3]` (bit 0 = LED peripheral).
- Sub‑address to device: `addr[2:0]`.
- LED register (RW) at subaddr `000` → absolute base `0x80000008` (bit 31=1, one‑hot bit0=1, subaddr=000).
  - Write example (byte): store `0x0A` to `0x80000008` → LEDs show `1010`.

Build and Run
- Requirements: JDK 11+, sbt, Verilator (for tests with `svsim`).

- Run unit tests:
  - `sbt test`

- Generate FPGA RTL (MIG top):
  - `sbt "runMain core.EmitMigTop"`
  - Outputs SystemVerilog into `synthesized/`.

Vivado/FPGA Notes
- Instantiate `MigTop` as the toplevel. Connect DDR3, reference/system clocks, reset, and route `leds[3:0]` to board LED pins.

Repository Layout
- `src/main/scala/core/Core.scala` — CPU core and helpers.
- `src/main/scala/core/Cache.scala` — Cache, AXI arbiter, BRAM wrapper.
- `src/main/scala/core/Top.scala` — Core + caches + AXI + peripherals.
- `src/main/scala/core/Peripherials.scala` — Peripherals mux and LED device.
- `src/main/scala/core/MigTop.scala` — FPGA toplevel with MIG.
- `src/main/scala/core/Emit.scala` — Emit SystemVerilog entrypoint.
- `src/test/scala/core` — Tests for core and cache top.