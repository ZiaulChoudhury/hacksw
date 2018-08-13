package BRam;
import bram::*;
import FIFO::*;
import pulse::*;
import datatypes::*;
import FIFOF::*;
import Vector::*;

#define WIDTH 1024
#define B 4

interface Bram;
        method Action read (BramWidth _col);
        method Action latch;
	method Action clean;
	method Vector#(16,DataType) get;
        method Action write(Vector#(16,DataType) data, Vector#(16,BramLength) id, BramWidth _col);
endinterface:Bram


(*synthesize*)
module mkBram(Bram);
	
	FIFORand slice[B];
	Reg#(DataType) datas[B];
        Pulse addr[B];
        Reg#(BramWidth) col <- mkReg(0);
	Wire#(BramWidth) colr <- mkWire;
	Wire#(Bool) c0[B];
	Wire#(Bool) l <- mkWire;
	Reg#(Bit#(1)) p0 <- mkReg(0);
	for(int i=0;i<fromInteger(B);i= i +1) begin
		slice[i] <- mkBuffer;
		datas[i] <- mkReg(0);
		addr[i] <- mkPulse;
		c0[i] <- mkWire;
	end
	

	for(int i= 0;i<fromInteger(B); i = i + 1)
	rule _insert;
		addr[i].ishigh;
		slice[i].enq(datas[i],col);
	endrule

	
	for(int i= 0;i<fromInteger(B); i = i + 1)
	rule _read (c0[i]== True);
			slice[i].deq(colr);
	endrule

	for(int i= 0;i<fromInteger(B); i = i + 1) begin
	rule _latch (l == True);
				slice[i].latchData;
	endrule
	
	rule _clean (l == False);
				slice[i].clean;
	endrule
	end
	
	method Action latch;
			l <= True;
	endmethod
	
	method  Vector#(16,DataType) get;
				Vector#(16, DataType) d = newVector;
				for(int i=0; i<B; i = i + 1)
                        		d[i] = slice[i].get;
                        return d;
	endmethod
	
        method Action read(BramWidth _col);
			colr <= _col;
			for(int i=0;i <fromInteger(B); i = i + 1)
				c0[i] <= True;

	endmethod
	
        method Action write(Vector#(16,DataType) data, Vector#(16,BramLength) id, BramWidth _col);
				col <= _col;
				for(int i=0;i <fromInteger(B); i = i + 1)
					if(id[i] == 1) begin
						addr[i].send;
						datas[i] <= data[i];
					end
	endmethod

	method Action clean;
			l <= False;
        endmethod


endmodule
endpackage

