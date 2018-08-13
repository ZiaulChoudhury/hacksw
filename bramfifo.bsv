import BRAM::*;
import DefaultValue::*;
import FIFOF::*;
import FixedPoint::*;
import TubeHeader::*;
import datatypes::*;

module mkBramFifo#(Integer size)(BFIFO);
	Wire#(Bool) deqStarted <- mkReg(False);
	BRAM_Configure cfg = defaultValue;
	cfg.allowWriteResponseBypass = False;
	cfg.memorySize = size;
	BRAM2Port#(UInt#(14), DataType) memory <- mkBRAM2Server(cfg);
	Reg#(UInt#(14)) rear <- mkReg(0);
	Reg#(UInt#(14)) front <- mkReg(0);
	Reg#(DataType) cache <- mkReg(0);
	FIFOF#(DataType) send <- mkFIFOF;
	

	function BRAMRequest#(UInt#(14), DataType) makeRequest(Bool write, UInt#(14) addr, DataType data);
        return BRAMRequest {
                write : write,
                responseOnWrite : False,
                address : addr,
                datain : data
        };
	endfunction


	rule deqRequester (rear != front /*&& rear > 0*/ );
		memory.portB.request.put(makeRequest(False,front,0));
		if (front == fromInteger(size)-1)
                                        front <= 0;
                                else
                                        front <= front+1;
	endrule

	rule fillcache;
		let d <- memory.portB.response.get;
		send.enq(d);
	endrule


	method Action enq(DataType data);
		memory.portA.request.put(makeRequest(True, rear, data));
		if (rear == fromInteger(size)-1)
				rear <= 0; 
		else
			rear <= rear +1;
	
	endmethod

	method ActionValue#(DataType) deq;
		let d = send.first; send.deq;
		return d;
	endmethod

	method Action clean;
                send.clear;
                rear <= 0;
                front <= 0;
        endmethod
	

	
endmodule: mkBramFifo
