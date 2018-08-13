package memory;
import BRAM::*;
import DefaultValue::*;
import FIFO::*;
import FixedPoint::*;
import datatypes::*;
import TubeHeader::*;

module mkBuffer#(Integer width)(FIFORand);

	BRAM_Configure cfg = defaultValue;
	cfg.allowWriteResponseBypass = False;
	cfg.memorySize = width;
	BRAM2Port#(BramWidth, DataType) memory <- mkBRAM2Server(cfg);
	Reg#(DataType) _cache <- mkReg(0);

	function BRAMRequest#(BramWidth, DataType) makeRequest(Bool write, BramWidth  addr, DataType data);
        return BRAMRequest {
                write : write,
                responseOnWrite : False,
                address : addr,
                datain : data
        };
	endfunction
	
	method Action latchData;
		let d <- memory.portB.response.get;
		_cache <= d;
	endmethod


	method Action enq(DataType data, BramWidth c);
			memory.portA.request.put(makeRequest(True, c, data));
	endmethod

	
	method Action deq(BramWidth c);
		memory.portB.request.put(makeRequest(False, c, 0));

	endmethod


	method DataType get;
		return _cache;
	endmethod
	
endmodule: mkBuffer
endpackage
