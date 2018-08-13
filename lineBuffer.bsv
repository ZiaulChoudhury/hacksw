package lineBuffer;
import FIFOF::*;
import bram::*;
import TubeHeader::*;
import pulse::*;
import datatypes::*;
import FixedPoint::*;
import Vector::*;

interface LineBuffer;
	method Action put(Vector#(32, DataType) in);
	method ActionValue#(Vector#(32, DataType)) get;
	method ActionValue#(Vector#(32, DataType)) relay;
	method Action clean;
	method Action cleaned;
	method Action reboot(Int#(16) im);
endinterface

module mkLineBuffer#(Integer row, Integer col, Integer _img, Integer roof, Integer rstride, Integer cstride)(LineBuffer);

		FIFOF#(DataType)				instream[roof];
		FIFOF#(Vector#(32,DataType))			outQ <- mkFIFOF;
		Reg#(Vector#(32,DataType))			fQ <- mkRegU;
		Reg#(DataType) 					forwardingQ[roof];
		Pulse                                           _forEnable <- mkPulse;
		Reg#(BramLength)                                r2 <- mkReg(0);
                Reg#(BramLength)                                r1 <- mkReg(0);
                Reg#(BramWidth)                                 c2 <- mkReg(0);
                Reg#(BramWidth)                                 c1 <- mkReg(0);
                Reg#(BramWidth)                                 res <- mkReg(0);
                Reg#(DataType)                                  windowBuffer[row*col];
		FIFORand                                        inputFmap[row];
		Reg#(Bool) readEnable 				<- mkReg(False);
		Reg#(Bool) _rebut 				<- mkReg(False);
		Reg#(BramWidth)  img <- mkReg(fromInteger(_img));
		Pulse _bp0 <- mkPulse;
		Pulse _bp1 <- mkPulse;
		Pulse _ena <- mkPulse;
		Pulse _r <- mkPulse;
		Reg#(Bit#(1)) p0 <- mkReg(0);
		Reg#(Bit#(1)) c0[row];
		Pulse	      l[row];
		Pulse	      l1[row];
		Reg#(DataType) store[row];
		FIFOF#(DataType) data[row];
		FIFOF#(BramLength) 				_readIndex[row]; 
                Reg#(UInt#(4)) 					_FR <- mkReg(0);
                Reg#(int) 					clk <- mkReg(0);
		
		rule _CLK;
				clk <= clk + 1;
		endrule
		
		Reg#(UInt#(8)) cStride <- mkReg(0);
        	Reg#(UInt#(8)) rStride <- mkReg(0);

		for(int i=0; i<fromInteger(roof); i = i + 1) begin
			instream[i] <- mkFIFOF;
			forwardingQ[i] <- mkReg(0);
		end
		
		for(int i=0; i<fromInteger(row); i = i + 1) begin
			inputFmap[i] <- mkBuffer;
			c0[i] <- mkReg(0);
			l[i] <- mkPulse;
			l1[i] <- mkPulse;
			store[i] <- mkReg(0);
			data[i] <- mkFIFOF;
			_readIndex[i] <- mkSizedFIFOF(16);
		
		end
	
		for(int i=0; i<fromInteger(row*col); i = i + 1) 
				windowBuffer[i] <- mkReg(0);
		
		rule _DRAMFetch (_rebut == False);	
				 if(c1 == extend(img-1)) begin
                                 	c1 <= 0;
                                 	if(r1 + fromInteger(roof) >= fromInteger(row))
                                 		r1 <= 0;
                                 else 
                                         r1 <= r1 + fromInteger(roof);
                                 end
                                 else
                                 c1 <= c1 + 1;
				 
				 for(BramLength i = 0; i < fromInteger(roof); i = i +1) begin
				 	let d = instream[i].first; instream[i].deq;
					//$display(" receiving in line buffer %d ", fxptGetInt(d));
					let index = (r1 + i)%fromInteger(row);
					inputFmap[index].enq(d , c1);
				 end
	
				 if(r1 + fromInteger(roof) >= fromInteger(row) && c1 >= fromInteger(col))
				 	readEnable <= True;
			
				 if(readEnable == True) begin
				 	_ena.send;
				 end
				_bp0.send;
		endrule

		rule _BRAMfetch (_rebut == False);
			_ena.ishigh;
			_bp0.ishigh;

			  	if(c2 == extend(img-1)) begin
                                	c2 <= 0;
                          	if (r2 + fromInteger(roof) >= fromInteger(row))
                                        r2 <= 0;
                                else 
                                        r2 <= r2 + fromInteger(roof);
                          	end
                          	else
                                c2 <= c2 + 1;

			for(BramLength i=0; i<fromInteger(row); i = i + 1) begin
				inputFmap[i].deq(c2);
				let index = (r2 + i)%fromInteger(row);
				//$display(" reading from index %d ", index);
				_readIndex[i].enq(index);
                         end

			p0 <= ~p0;
		endrule
	
		for(int i=0; i<fromInteger(row); i = i + 1) begin		
		
			rule _latchData (_rebut == False  && (c0[i]^p0) == 1);
				c0[i] <= p0;
				inputFmap[i].latchData;
				l[i].send;
			endrule

			rule _storeData;
				l[i].ishigh;
				let d = inputFmap[i].get;
				store[i] <= d;
				l1[i].send;
			endrule

			rule _getData;
				l1[i].ishigh;
				let index = _readIndex[i].first; _readIndex[i].deq;
				data[i].enq(store[index]);
			endrule

		end
		rule collect (_rebut == False);
                        for (UInt#(8) i = fromInteger(row*col) - 1; i >= fromInteger(row); i = i-1)
                                windowBuffer[i-fromInteger(row)] <= windowBuffer[i];
			

                        for (UInt#(8) i = 0;i < fromInteger(row); i = i+1) begin
                                let d = data[i].first; data[i].deq;
				//$display(" row is %d ", fxptGetInt(d));
                                windowBuffer[fromInteger(row*col-row)+i] <= d;
                        end
			//$display(" ############################ [ %d ] C = %d r = %d  @clk %d IMG = %d ", res, cStride, rStride, clk, img);

			
                        if(res == extend(img-1)) begin
                                res <= 0;
				rStride <= ((rStride + 1) & (fromInteger(rstride-1)));
			end
                        else begin
                                res <= res + 1;

			end

                        if(res >= fromInteger(col)-1) begin
			
				if(res == extend(img-1))
					cStride <= 0;
				else	
					cStride <= ((cStride + 1) & (fromInteger(cstride-1)));
				
				if(res == fromInteger(col-1) && rStride == 0 )
					_r.send;
				else begin
					if((cStride == 0 && rStride == 0))
						_r.send;
				end
			end
                endrule

		rule _reboot (_rebut == True && _FR > 0);
                        _bp0.clean;
                        _bp1.clean;
                        for(int i=0; i<fromInteger(roof); i = i+1) begin
                                instream[i].clear;
                        end

			for(int i=0; i<fromInteger(row); i = i + 1) begin
                        	c0[i] <= 0;
                        	l[i].clean;
                        	l1[i].clean;
                        	data[i].clear;
                        	_readIndex[i].clear;
			end
		
			p0 <= 0;
                        c1 <= 0;
                        c2 <= 0;
                        r2 <= 0;
                        r1 <= 0;
                        readEnable <= False;
                        _ena.clean;
			rStride <= 0;
			cStride <= 0;
			outQ.clear;
			res <= 0;
			_r.clean;
                        _FR <= _FR - 1;
			_forEnable.clean;
                endrule

                rule _reboot_mem(_FR == 2  && _rebut == True);
				for(int i=0; i<fromInteger(row); i = i + 1)
                                	inputFmap[i].clean;
                endrule


		rule send (_rebut == False);
				_r.ishigh;
				//$display(" sending the collected row ");
				Vector#(32, DataType) dat = replicate(0);
				Vector#(32, DataType) f = replicate(0);
		
				for(int i = 0; i<fromInteger(row); i = i + 1)
					for(int j = 0; j<fromInteger(row*col); j = j + fromInteger(row))
						dat[i*fromInteger(col) + j/fromInteger(row)] = windowBuffer[i+j];

				outQ.enq(dat);

				for(Integer k = 0; k<fromInteger(row); k = k  + 1) begin
						f[k] = dat[((col)/2 + k*col)];
                                end
				_forEnable.send;
				fQ <= f;
				
		endrule
	
			
		method Action put (Vector#(32, DataType) in);
				for(int i= 0;i<fromInteger(roof); i = i + 1) begin
					instream[i].enq(in[i]);
				end
		endmethod
		
		
		method ActionValue#(Vector#(32, DataType)) get if(_rebut == False);
				let d = outQ.first; outQ.deq;
				return d;
		endmethod

		method ActionValue#(Vector#(32, DataType)) relay;
				_forEnable.ishigh;
				return fQ;
		endmethod

		method Action clean;
                                 _rebut <= True;
                                 _FR <= 2;
                endmethod

		method Action  cleaned if(_FR == 0);
                                _rebut <= False;
                endmethod
		
		method Action reboot(Int#(16) im);
			img <= truncate(unpack(pack(im)));
		endmethod


endmodule
endpackage
