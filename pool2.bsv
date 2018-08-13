package pool2;
import FIFO::*;
import datatypes::*;
import Vector::*;
import FixedPoint::*;


interface Pool2;
        method Action send(Vector#(2,DataType) data);
        method ActionValue#(DataType) reduced;
	method Action clean;
endinterface

(*synthesize*)
module mkPool2(Pool2);

   //#################################### DATA Stuctructures ###############################
   FIFO#(DataType) outQ <- mkSizedFIFO(1);
   Reg#(UInt#(4)) counter <- mkReg(0);
   Reg#(DataType) _reduction <- mkReg(0);
   //#######################################################################################
   
  method Action send(Vector#(2,DataType) data);
	DataType a = 0;
	DataType b = 0;

	if(data[0] > data[1])
		a = data[0];
	else
		a = data[1];
	
	if(_reduction > a)
		b = _reduction;
	else
		b = a;

			//$display(" received for pool %d %d @counter %d ",  fxptGetInt(data[0]), fxptGetInt(data[1]),counter);
	if(counter >= 2) begin
		outQ.enq(b);
		counter <= 0;
		_reduction <= 0;
	end
	
	else
		counter <= counter + 2;
   endmethod

  method ActionValue#(DataType) reduced;
	let d = outQ.first; outQ.deq;
	return d;
  endmethod 
  
  method Action clean;
                outQ.clear;
		_reduction <= 0;
		counter <= 0;
  endmethod

endmodule

endpackage

