package pool;
import FIFO::*;
import datatypes::*;
import Vector::*;
import FixedPoint::*;


interface Pool;
        method Action send(DataType data);
        method ActionValue#(DataType) reduced;
	method Action clean;
endinterface

module mkPool#(Integer img)(Pool);

   // #################################### DATA Stuctructures ###############################
   FIFO#(DataType) outQ <- mkFIFO;
   Reg#(BramWidth) counter <- mkReg(0);
   Reg#(Bit#(1)) p0 <- mkReg(0);
   Reg#(Bit#(1)) c0 <- mkReg(0);
   Reg#(DataType) _red <- mkReg(0);
   Reg#(UInt#(1)) cStride <- mkReg(0);
   Reg#(UInt#(1)) rStride <- mkReg(0);
  // #######################################################################################

   rule l1 ((c0 ^ p0) == 1);
		c0 <= p0;
		if(counter == fromInteger(img) - 1) begin
			rStride <= rStride + 1;	
			counter <= 0;
		end
		else
			counter <= counter + 1;
		
		cStride <= cStride + 1;
		if(cStride == 0 && rStride == 0)
			outQ.enq(_red);
   endrule

   method Action send(DataType data);
		_red <= data;
		p0 <= ~p0;
   endmethod

  method ActionValue#(DataType) reduced;
	let d = outQ.first; outQ.deq;
	return d;
  endmethod 
  
  method Action clean;
                outQ.clear;
		p0 <= 0;
		c0 <= 0;
		cStride <= 0;
		rStride <= 0;
  endmethod

endmodule

endpackage

