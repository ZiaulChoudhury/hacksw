package TubeHeader;
import datatypes::*;
import Vector::*;

#define MAX_LIM 100

typedef struct{
	Integer row1;
	Integer row2; 
	Integer col1; 
	Integer col2;  
	Integer size;
} Relay deriving(Eq, Bits);

typedef struct{
        Integer k;
        Integer stencil;
        Integer img;
        Integer banks;
	Vector#(100,CoeffType) weights;
}Stencil deriving(Eq, Bits);

typedef (function DataType compute(Reg#(DataType) inputs[])) Compute;

Relay r = Relay{row1:0,row2:0,col1:0,col2:0};
Relay _samp[1] = {r};
Relay _dRelay[1][1] = {_samp};

interface Reducer;
        method Action send(Vector#(100,DataType) data);
        method ActionValue#(DataType) reduced;
endinterface:Reducer

interface Component;
        method Action send(DataType data, BramLength i);
	method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate);
        method ActionValue#(DataType) forwarded(BramLength i, UInt#(10) _rate);
        
	method ActionValue#(Vector#(32,DataType)) receiveVector;
	method ActionValue#(Vector#(32,DataType)) getVector(UInt#(10) _ID);
        method Action sendVector(Vector#(32, DataType) inx);
	
	method Action clean;
	method Action extra;
	method Action cleaned;
	method Action reset(Int#(16) param);
	method Action reset2(Int#(16) _dc);
endinterface: Component

interface Conv;
        method Action sendP(Vector#(32, DataType) datas);
        method Action sendF(Vector#(32, CoeffType) filter);
        method ActionValue#(DataType) result;
	method Action clean;
endinterface

interface BFIFO;
        method Action enq(DataType val);
        method ActionValue#(DataType) deq;
	method Action clean;
endinterface: BFIFO


interface BFIFO2;
        method Action enq(DataType val);
        method Action enqDDR(Vector#(2,DataType) val);
        method ActionValue#(DataType) deq;
	method ActionValue#(Vector#(2, DataType)) deqDDR;
        method Action clean;
	method Action reset(Int#(16) rx);
endinterface: BFIFO2


typedef struct {
   Bit #(1) valid;
   Bit #(128) data;
   Bit #(16) slot;
   Bit #(4) pad;
   Bit #(1) last;
} PCIE_PKT deriving (Bits, Eq, FShow);

 
endpackage: TubeHeader
