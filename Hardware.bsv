package Hardware; 
import TubeHeader:: *; 
import FixedPoint:: *; 
import datatypes:: *; 
import Compose:: *; 
import Vector:: *; 
import FIFO:: *; 
import FIFOF:: *;

#define RECV  65536
#define SEND  63505
interface Stdin;

	method ActionValue#(Bit#(10)) get;
	method Action put(Bit#(10) datas);

endinterface

(*synthesize *) 
module mkHardware(Stdin);
FIFOF#(Bit#(10)) outQ[8][8]; 
Reg#(int) clk <- mkReg(0); 
Reg#(UInt#(20)) recv <- mkReg(0); 
Reg#(UInt#(20)) send <- mkReg(0);
Reg#(Bool) _c <- mkReg(False);
Reg#(Bool) _clear <- mkReg(False);
for (int i = 0; i < 8; i = i + 1)
	for (int j = 0; j < 8; j = j + 1)
		outQ[i][j] <- mkFIFOF;
Reg#(Bool) init <- mkReg(True);



Integer buffer_src[1]={32};
Component _src <- mkSource(1 , buffer_src , 1);


Integer buffer_tile[1]={32};
Relay r0= Relay{row1:2,row2: 254,col1: 2 , col2: 254,size: 33};
Relay relays[1] = {r0};
Component _tile <- mkRelayBuffer(_src , 0 , 5 , 5 , 1 , buffer_tile , 1 , relays , 256, 1 , 1, 1);


Integer buffer_blur[1]={32};
Component _blur <- mkConvolver(_tile , 0 , 5, 1 , buffer_blur , 1);


Integer buffer_sharpen[1]={32};
Component _inputs_sharpen[2]= {_tile , _blur };
Integer _inputId_sharpen[2]= {0 , 0 };
Bool _relay_sharpen[2]= {True,False};

function DataType func_sharpen(Reg#(DataType) in[]);
                 /*DataType two = 2;
                 DataType val = fxptTruncate(fxptSub(fxptMult(two,in[0]),in[1]));
                 DataType th = fxptTruncate(fxptSub(in[0],in[1]));
                 if ( th < 0) begin                     Bit#(16) data = pack(th);
                     Bit#(1) one = 1;
                     data = invert(data);
                     Bit#(16) res = truncate(data + zeroExtend(one));
                     th = unpack(res);
                 end                  if(th < 0.01)                                 return in[0];
                  else                                 return val;
		  */
				
		return in[0];
               
endfunction
Component _sharpen <- mkPointComponent(func_sharpen,_inputs_sharpen , 2 ,_relay_sharpen , _inputId_sharpen , 1 , buffer_sharpen , 1);


rule initialize(init == True);
		 init <= False;
		 Vector#(32,DataType) weights_0 = newVector;
		 weights_0[0]=0.04;
		 weights_0[1]=0.04;
		 weights_0[2]=0.04;
		 weights_0[3]=0.04;
		 weights_0[4]=0.04;
		 weights_0[5]=0.04;
		 weights_0[6]=0.04;
		 weights_0[7]=0.04;
		 weights_0[8]=0.04;
		 weights_0[9]=0.04;
		 weights_0[10]=0.04;
		 weights_0[11]=0.04;
		 weights_0[12]=0.04;
		 weights_0[13]=0.04;
		 weights_0[14]=0.04;
		 weights_0[15]=0.04;
		 weights_0[16]=0.04;
		 weights_0[17]=0.04;
		 weights_0[18]=0.04;
		 weights_0[19]=0.04;
		 weights_0[20]=0.04;
		 weights_0[21]=0.04;
		 weights_0[22]=0.04;
		 weights_0[23]=0.04;
		 weights_0[24]=0.04;
		_blur.sendVector(weights_0);
endrule

rule _pad(recv >= RECV && send < SEND);
	Vector  # (8,DataType) pad = replicate(0);
	for (BramLength i=0; i <1; i = i + 1)begin
		_src.send(pad[i],i);
	end

endrule
rule flushOut (send < SEND);
	for(UInt#(10) i=0; i<1; i = i + 1) begin
		let d0  <-_sharpen.receive(0,i);
		outQ[i][0].enq(truncate(pack(fxptGetInt(d0))));

	end
endrule
rule _ClearPipe(send == SEND && _c == False);
	for (BramLength i=0; i <1; i = i + 1)begin
		for (BramLength j=0; j <8 ; j = j + 1)
			outQ[i][j].clear;
	end
		_c <= True;
		_src.clean;
		_tile.clean;
		_blur.clean;
		_sharpen.clean;

endrule
rule _ResetDone (_c == True && _clear == False);
		 _clear <= True;
		_tile.cleaned;

endrule
rule _ResetDone2 (_c == True && _clear == True);
	_c <= False;
	 _clear <= False;
	send <= 0; recv <= 0;

endrule
method ActionValue#(Bit#(10)) get if(send < SEND);
		Vector#(1,Bit#(10)) data = newVector;
		for (BramLength i=0; i <1; i = i + 1)
		for (BramLength j=0; j <1; j = j + 1)begin
			 data[i*1+j] = outQ[i][j].first; outQ[i][j].deq;
 		end
			 send <= send + 1; 
	 return pack(data);

endmethod


method Action put(Bit#(10) datas) if(outQ[0][0].notFull && recv < RECV);
		Vector#(1,Bit#(10)) data = unpack(datas);
		for (BramLength i=0; i <1; i = i + 1) begin
				Int#(10) x = unpack(data[0+i*1]);
				_src.send(fromInt(x),i);
		end
			 recv <= recv + 1; 

endmethod


endmodule
endpackage

