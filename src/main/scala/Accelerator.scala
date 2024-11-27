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

  val dataMemory = Module(new DataMemory())

  //State enum and register
  val idle :: loadVals :: border::write::inc :: done  :: self :: left :: right :: up :: down :: adhoc = Enum (12)
  val stateReg = RegInit(idle)

  //Support registers
  val x = 0.U(32.W)
  val y = 0.U(32.W)

  val addressReg = RegInit(0.U(16.W))
  val trueAddressReg = RegInit(0.U(16.W))
  val dataReg = RegInit(0.U(32.W))

  //Default values
  dataMemory.io.writeEnable := false.B
  dataMemory.io.dataWrite := dataReg
  dataMemory.io.address := 0.U(16.W)
  io.done := false.B

  //FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := loadVals
        addressReg := RegInit(0.U(16.W))
      }
    }
    is(loadVals) {
      addressReg := (20.U * y) + x
      stateReg := border
    }

    is(border) {
      when(x === 0.U || x === 19.U || y === 0.U || y === 19.U) {
        //dataMemory.io.dataWrite := 0.U(32.W)
        //dataMemory.io.address := addressReg
        dataReg := 0.U(32.W)
        stateReg := write
      }
      when((x > 0.U && x < 19.U) && (y > 0.U && y < 19.U)) {
        trueAddressReg := addressReg //How memory works forces us to set a true Address Register
        addressReg := trueAddressReg-1.U(16.W) // Prepping the left neighbor
        dataReg := Cat(0.U(24.W), dataMemory.io.dataRead(7,0))
        stateReg := self
      }
    }
    is(self) {
      when(dataReg === 0.U(32.W)){
        stateReg := write
        addressReg := trueAddressReg
      }
      when(dataReg === 255.U(32.W)){
        addressReg := trueAddressReg+1.U(16.W) //Preparing the coordinate for the right neighbor
        dataReg := Cat(0.U(24.W), dataMemory.io.dataRead(7,0))
        stateReg := left
      }
    }
    is(left){
      when(dataReg === 0.U(32.W)){
        stateReg := write
        addressReg := trueAddressReg
      }
      when(dataReg === 255.U(32.W)){
        addressReg := trueAddressReg-20.U(16.W) // Preparing the coordinate for up
        dataReg := Cat(0.U(24.W), dataMemory.io.dataRead(7,0))
        stateReg := right
      }
    }
    is(right){
      when(dataReg === 0.U(16.W)){
      stateReg := write
      addressReg := trueAddressReg
      }
      when(dataReg === 255.U(16.W)){
        addressReg := trueAddressReg+20.U(16.W)
        dataReg := Cat(0.U(24.W), dataMemory.io.dataRead(7,0))
        stateReg := up
      }
    }
    is(up){
      when(dataReg === 0.U(16.W)){
        stateReg := write
        addressReg := trueAddressReg
      }
      when(dataReg === 255.U(16.W)){
        dataReg := Cat(0.U(24.W), dataMemory.io.dataRead(7,0))
        stateReg := down
      }
    }
    is(down){
      when(dataReg === 0.U(16.W)){
        addressReg := trueAddressReg
        dataReg := 0.U(32.W)
        stateReg := write
      }
      when(dataReg === 255.U(16.W)){
        addressReg := trueAddressReg /// White value
        stateReg := write
      }
    }


    is(write) {
      dataMemory.io.address := addressReg + 400.U(16.W)
      dataMemory.io.writeEnable := true.B
      when(addressReg === 399.U(16.W)){
        stateReg := done
      }.otherwise{
        stateReg := inc
      }
    }
    is(inc){
      dataMemory.io.writeEnable := false.B
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
