package conv5;
import FIFOF::*;
import datatypes::*;
import Vector::*;
import pulse::*;
import FixedPoint::*;
import mul::*;
import FIFOF::*;
import TubeHeader::*;

(*synthesize*)
module mkConv5(Conv);
	
	//################################## DataStructures  #########################	
	FIFOF#(Bit#(400)) _inputQ <- mkFIFOF; 
	Wire#(DataType) window[25];
	Wire#(Bool) wc[25];
	Reg#(Bool) w <- mkReg(False);
	Wire#(Bool) cl <- mkWire;
	Reg#(DataType) accumulator1[25];
	Reg#(DataType) accumulator2[9];
	Reg#(DataType) accumulator3[4];
	Reg#(DataType) accumulator4[2];
	Reg#(DataType) accumulator5 <- mkReg(0);
	Reg#(CoeffType) coeffs[25];
	Pulse a0 <- mkPulse;
	Pulse a1 <- mkPulse;
	Pulse a2 <- mkPulse;
	Pulse a3 <- mkPulse;
	Reg#(int) clk <- mkReg(0);
	Reg#(Bit#(1)) p0 <- mkReg(0);
	Reg#(Bit#(1)) c0 <- mkReg(0);
	Reg#(Bit#(1)) p3 <- mkReg(0);
        Reg#(Bit#(1)) c3 <- mkReg(0);
	Mult _PE[25];
	FIFOF#(DataType) _outQ <- mkFIFOF;
		for(int j= 0; j<25; j = j + 1) begin
			window[j] <- mkWire;
			/*if(j == 12)
				coeffs[j] <- mkReg(1);
			else*/	
				coeffs[j] <- mkReg(0);	
			_PE[j] <- mkMult;
			wc[j] <- mkWire;
			accumulator1[j] <- mkReg(0);
		end
		for(int k=0; k<9; k = k +1)
			accumulator2[k] <- mkReg(0);

		for(int k=0; k<4; k = k +1)
                        accumulator3[k] <- mkReg(0);

		for(int k=0; k<2; k = k +1)
                        accumulator4[k] <- mkReg(0);

	//##################################################################################

	rule _clk;
		clk <= clk + 1;
	endrule

	rule _input_decompose;
		let  packet = _inputQ.first; _inputQ.deq;
		Vector#(25, DataType) wb = unpack(packet);
		
		for(int _Elem = 0; _Elem < 25 ; _Elem  = _Elem + 1) begin
				window[_Elem] <= wb[_Elem];
				wc[_Elem] <= True;
		end	
	endrule

	
	
		for(int _Elem=0 ;_Elem< 25; _Elem = _Elem + 1) begin
				rule _pushMAC(wc[_Elem] == True);
					_PE[_Elem].a(window[_Elem]);
					_PE[_Elem].b(coeffs[_Elem]);
				endrule
				
				rule _ac1;
					let d <- _PE[_Elem].out;
					accumulator1[_Elem] <= d;
					if(_Elem == 0)
						a0.send;
				endrule
		end 

				rule _ac2;
					a0.ishigh;	
                                        accumulator2[0] <= fxptTruncate(fxptAdd(accumulator1[0],fxptAdd(accumulator1[3], accumulator1[4])));
                                        accumulator2[1] <= fxptTruncate(fxptAdd(accumulator1[1],fxptAdd(accumulator1[5], accumulator1[6])));
                                        accumulator2[2] <= fxptTruncate(fxptAdd(accumulator1[2],fxptAdd(accumulator1[7], accumulator1[8])));	
					accumulator2[3] <= fxptTruncate(fxptAdd(accumulator1[9],fxptAdd(accumulator1[10], accumulator1[11])));
                                        accumulator2[4] <= fxptTruncate(fxptAdd(accumulator1[12],fxptAdd(accumulator1[13], accumulator1[14])));
                                        accumulator2[5] <= fxptTruncate(fxptAdd(accumulator1[15],fxptAdd(accumulator1[16], accumulator1[17])));
					accumulator2[6] <= fxptTruncate(fxptAdd(accumulator1[18],fxptAdd(accumulator1[19], accumulator1[20])));
                                        accumulator2[7] <= fxptTruncate(fxptAdd(accumulator1[21],fxptAdd(accumulator1[22], accumulator1[23])));
                                        accumulator2[8] <= accumulator1[24];
					a1.send;
                                endrule

				rule _ac3;
					a1.ishigh;
					accumulator3[0] <= fxptTruncate(fxptAdd(accumulator2[0], accumulator2[1]));
					accumulator3[1] <= fxptTruncate(fxptAdd(accumulator2[2], accumulator2[3]));
					accumulator3[2] <= fxptTruncate(fxptAdd(accumulator2[4], accumulator2[5]));
					accumulator3[3] <= fxptTruncate(fxptAdd(accumulator2[6], fxptAdd(accumulator2[7], accumulator2[8])));
					a2.send;		
				endrule

				rule _ac4;
					a2.ishigh;
					accumulator4[0] <= fxptTruncate(fxptAdd(accumulator3[0], accumulator3[1]));
					accumulator4[1] <= fxptTruncate(fxptAdd(accumulator3[2], accumulator3[3]));
					a3.send;
				endrule

				rule _ac5;
					a3.ishigh;
					accumulator5 <= fxptTruncate(fxptAdd(accumulator4[0], accumulator4[1]));
					p0 <= ~p0; 					
				endrule
				
				rule _sums ((c0 ^ p0) == 1);
					c0 <= p0;
					_outQ.enq(accumulator5);
				endrule

				rule _clean (cl == True);
                                        a0.clean;
                                        a1.clean;
                                        a2.clean;
                                        a3.clean;
                                        for(int j= 0; j<25; j = j + 1)
                                                        _PE[j].clean;

                                        _outQ.clear;
                                        _inputQ.clear;
                                        p0 <= 0;
                                        c0 <= 0;
                                endrule

 
	
	
	method Action sendP(Vector#(32, DataType) datas);
		 Vector#(25, DataType) m = newVector;
                 for(int j=0; j<25; j= j + 1)
                                m[j] = datas[j];

		_inputQ.enq(pack(m));	
	endmethod

		
	method Action sendF(Vector#(32, CoeffType) filter);
			CoeffType zero = 0;
			for(int j=0; j<25; j= j + 1)
					coeffs[j] <= filter[j];				
	endmethod
	
	method ActionValue#(DataType) result;
			let d = _outQ.first; _outQ.deq;
			return d;
	endmethod

	method Action clean;
                cl <= True;
        endmethod
	
endmodule
endpackage
