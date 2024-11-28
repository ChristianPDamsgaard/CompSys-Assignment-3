import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))
  })

  //State enum and register
  val idle :: loadVals :: border::write::inc :: done  :: self :: left :: right :: up :: down :: adhoc = Enum (12)
  val stateReg = RegInit(idle)

  //Support registers
  val x = RegInit(0.U(32.W))
  val y = RegInit(0.U(32.W))

  val addressReg = RegInit(0.U(16.W))
  val trueAddressReg = RegInit(0.U(16.W))
  val dataReg = RegInit(0.U(32.W))

  //Default values
  io.writeEnable := false.B
  io.dataWrite := dataReg
  io.address := 0.U(16.W)
  io.done := false.B

  //FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.start) {
        addressReg := RegInit(0.U(16.W))
        stateReg := loadVals
      }
    }
    is(loadVals) {
      addressReg := (20.U * y) + x
      io.address := ((20.U * y) + x)(15,0)
      stateReg := border
    }

    is(border) {
      when(x === 0.U || x === 19.U || y === 0.U || y === 19.U) {
        io.dataWrite := 0.U(32.W)
        dataReg := 0.U(32.W)
        stateReg := write
      }
      when((x > 0.U && x < 19.U) && (y > 0.U && y < 19.U)) {
        trueAddressReg := addressReg //How memory works forces us to set a true Address Register
        addressReg := addressReg - 1.U(16.W) // Prepping the left neighbor
        io.address := addressReg - 1.U(16.W)
        dataReg := Cat(0.U(24.W), io.dataRead(7,0))
        stateReg := self
      }
    }
    is(self) {
      when(dataReg === 0.U(32.W)){
        io.dataWrite := 0.U(32.W)
        addressReg := trueAddressReg
        stateReg := write
      }
      when(dataReg === 255.U(32.W)){
        addressReg := trueAddressReg+1.U(16.W) //Preparing the coordinate for the right neighbor
        io.address := trueAddressReg+1.U(16.W)
        dataReg := Cat(0.U(24.W), io.dataRead(7,0))
        stateReg := left
      }
    }
    is(left){
      when(dataReg === 0.U(32.W)){
        io.dataWrite := 0.U(32.W)
        addressReg := trueAddressReg
        stateReg := write
      }
      when(dataReg === 255.U(32.W)){
        addressReg := trueAddressReg-20.U(16.W) // Preparing the coordinate for up
        io.address := trueAddressReg-20.U(16.W)
        dataReg := Cat(0.U(24.W), io.dataRead(7,0))
        stateReg := right
      }
    }
    is(right){
      when(dataReg === 0.U(32.W)){
        addressReg := trueAddressReg
        stateReg := write
      }
      when(dataReg === 255.U(32.W)){
        addressReg := trueAddressReg+20.U(16.W)
        io.address := trueAddressReg+20.U(16.W)
        dataReg := Cat(0.U(24.W), io.dataRead(7,0))
        stateReg := up
      }
    }
    is(up){
      when(dataReg === 0.U(32.W)){
        addressReg := trueAddressReg
        stateReg := write
      }
      when(dataReg === 255.U(32.W)){
        addressReg := trueAddressReg+20.U(16.W)
        io.address := trueAddressReg+20.U(16.W)
        dataReg := Cat(0.U(24.W), io.dataRead(7,0))
        stateReg := down
      }
    }
    is(down){
      when(dataReg === 0.U(32.W)){
        addressReg := trueAddressReg
        io.dataWrite := 0.U(32.W)
        stateReg := write
      }
      when(dataReg === 255.U(32.W)){
        io.address := trueAddressReg + 400.U(16.W)
        io.dataWrite := 255.U(32.W) /// White value
        stateReg := write
      }
    }


    is(write) {
      io.address := trueAddressReg + 400.U(16.W)
      io.writeEnable := true.B
      when(addressReg === 399.U(16.W)){
        stateReg := done
      }.otherwise{
        stateReg := inc
      }
    }
    is(inc){
      io.writeEnable := false.B
      when(x === 19.U){
        x := 0.U
        y := y+1.U
      }.otherwise{
        x := x+1.U
      }
      stateReg := loadVals
    }
    is(done){
      io.done := true.B
      stateReg := done
    }
  }

}
