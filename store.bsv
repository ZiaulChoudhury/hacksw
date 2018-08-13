import BRAM::*;
import DefaultValue::*;
import FIFOF::*;
import FixedPoint::*;
import TubeHeader::*;
import datatypes::*;
import Vector::*;
import pulse::*;

#define SIZE 12322

(*synthesize*)
module mkStore(BFIFO2);
	Wire#(Bool) deqStarted <- mkReg(False);
	BRAM_Configure cfg = defaultValue;
	cfg.allowWriteResponseBypass = False;
	Integer size = 12322; //50; //99; //24643; //99; 32259; 
	cfg.memorySize = size;
	BRAM2Port#(Int#(20), Bit#(32)) memory <- mkBRAM2Server(cfg);
	Reg#(Int#(20)) rear <- mkReg(0);
	Reg#(Int#(20)) front <- mkReg(0);
	Reg#(Int#(20)) _Sz <- mkReg(fromInteger(size-1));
	FIFOF#(DataType) send <- mkFIFOF;

	Reg#(UInt#(1)) rPtr <- mkReg(0);
	FIFOF#(UInt#(1)) rx <- mkSizedFIFOF(4);
	Reg#(UInt#(1)) rPtr2 <- mkReg(0);
	Reg#(UInt#(1)) wPtr <- mkReg(0);
	Reg#(DataType) cache <- mkReg(0);
	Reg#(Bit#(32)) dataBlock <- mkReg(0);
	Pulse ddr <- mkPulse;
	Reg#(int) clk <- mkReg(0);
		

	function BRAMRequest#(Int#(20), Bit#(32)) makeRequest(Bool write, Int#(20) addr, Bit#(32) data);
        return BRAMRequest {
                write : write,
                responseOnWrite : False,
                address : addr,
                datain : data
        };
	endfunction


	rule _CLK;
		clk <= clk + 1;
	endrule
	
	rule deqRequester (rear != front);

		if(front < SIZE)
		memory.portB.request.put(makeRequest(False,front,0));

		if (front == _Sz) begin
                          front <= 0;
			  rPtr <= 0;
		end
                else begin
			if(rPtr == 1) 
                          front <= front+1;
			rPtr <= rPtr + 1;
		end
		
	endrule

	rule fillcache;
		if( front == _Sz)
			rPtr2 <= 0;
		else
			rPtr2 <= rPtr2 + 1;
		let d <- memory.portB.response.get;
		dataBlock <= d;
		ddr.send;
		Vector#(2, DataType) dx = unpack(d);
		send.enq(dx[rPtr2]);
	endrule


	method Action enqDDR(Vector#(2, DataType) data);

			memory.portA.request.put(makeRequest(True, rear, pack(data)));
			if (rear == _Sz) begin
                                rear <= 0;
                	end
                	else begin
                                rear <= rear +1;
                	end

	endmethod
	
	method Action enq(DataType data);
	
		Vector#(2, DataType) d = newVector;
		wPtr <= wPtr + 1;	

		if(wPtr == 0) begin
			cache <= data;
			d[0] = data;
		end
		else begin
			d[0] = cache;
			d[1] = data;
		end
	
		memory.portA.request.put(makeRequest(True, rear, pack(d)));
		
		if (rear == _Sz) begin
				rear <= 0;
		end
		else begin
			if(wPtr == 1)
				rear <= rear +1;
		end
	
	endmethod

	method ActionValue#(DataType) deq;
		let d = send.first; send.deq;
		return d;
	endmethod

	method ActionValue#(Vector#(2, DataType)) deqDDR;
			ddr.ishigh;
			return unpack(dataBlock);
	endmethod

	method Action clean;
		send.clear;
		rear <= 0;
		front <= 0;
		rPtr <= 0;
		wPtr <= 0;
	endmethod

	method Action reset(Int#(16) sx);
		_Sz <= extend(sx) - 1 ;
	endmethod

        
	
endmodule
