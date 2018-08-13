package Stage;
import Vector::*;
import FIFO::*;
import FIFOF::*;
import pulse::*;
import FixedPoint::*;
import datatypes::*;
import TubeHeader::*;
import BRam::*;
import mul::*;
import conv3::*;
import conv5::*;

	module mkStage#(Integer roof, Integer stencil, Integer img, Integer banks, Vector#(100,CoeffType) weights)(Component);

		//################################################## STRUCTURES ################################################
		Reg#(DataType) windowBuffer[roof][stencil*stencil];
		Reg#(int) res[roof];
		Reg#(Int#(32)) clk <- mkReg(0);
		FIFO#(DataType) instream[banks];
		Reg#(DataType) data[roof][stencil];
		Reg#(DataType) store[banks];
		FIFOF#(DataType) forward[roof];
		Reg#(DataType) forwardingQ[roof];
		Conv _PE[roof];
		Reg#(CoeffType) coeffs[stencil*stencil];
		Reg#(Bit#(1)) _macPulse[roof];
		Reg#(Bit#(1)) _macP[roof];
		Pulse 	collPulse[roof];
		Pulse 	_ena <- mkPulse; 
		Pulse 	_l <- mkPulse; 
		Pulse                                   	_recvEnable[roof];
		Pulse                                   	_forEnable[roof];
		Reg#(DataType)                          	recvData[roof];
                Reg#(BramLength)                           	r2 <- mkReg(0);
                Reg#(BramLength)                           	r1 <- mkReg(0);
		Reg#(BramWidth) 				c2 <- mkReg(0);
		Reg#(BramWidth) 				c1 <- mkReg(0);
		Reg#(BramWidth) 				row <- mkReg(0);
		Reg#(Bool) 					init <- mkReg(True);
		Pulse                                           _bp[roof];
                Pulse                                           _bp0 <- mkPulse;
                Pulse                                           _bp1 <- mkPulse;
		Reg#(Bool) 					_rebut <- mkReg(False);
                Reg#(UInt#(4)) 					_FR <- mkReg(0);

	
		if(stencil == 3) begin
			for(int k = 0; k<fromInteger(roof); k = k+1)
				_PE[k] <- mkConv3;
		end 
		else if(stencil == 5) begin
			for(int k = 0; k<fromInteger(roof); k = k+1) 
                                _PE[k] <- mkConv5;
		end
		
		
		Bram 						inputFmap    <- mkBram; 					
		Reg#(Bool)                               	_latch       <- mkReg(False);
		Reg#(Bool)                               	strideFetch  <- mkReg(True);
		Reg#(Bool)                               	readEnable   <- mkReg(False);
		FIFOF#(BramLength) 				_readIndex[roof][stencil]; 

		//################################################################################################################
	

		for(Int#(8) i =0 ;i< fromInteger(stencil*stencil); i = i+1) begin
			coeffs[i] <- mkReg(weights[i]);
		end
			
		for(BramLength i=0; i < fromInteger(banks); i = i +1 ) begin
			instream[i] <- mkFIFO;
			store[i] <- mkReg(0);
		end

		for(int k = 0; k<fromInteger(roof); k = k+1) begin
			forward[k] <- mkSizedFIFOF(32);
			forwardingQ[k] <- mkReg(0);
			_forEnable[k] <- mkPulse;
			collPulse[k] <- mkPulse;
			res[k] <- mkReg(0);
			recvData[k] <- mkReg(0);
			_recvEnable[k] <- mkPulse;
			_macPulse[k] <- mkReg(0);
			_macP[k] <- mkReg(0);
			for(int i=0;i< fromInteger(stencil) ; i = i+1) begin
				data[k][i] <- mkReg(0);
				_readIndex[k][i] <- mkSizedFIFOF(16);
			end
		end

		for(int k = 0; k< fromInteger(roof); k = k+1)
		for(int i= 0;i <fromInteger(stencil*stencil); i = i+1) begin
				windowBuffer[k][i] <- mkReg(0);
		end

		rule _Init (init == True);
			Vector#(32, CoeffType) c = replicate(0);

			for(int i=0; i<fromInteger(stencil*stencil); i = i + 1)
				c[i] = coeffs[i];

			for(int i=0; i<fromInteger(roof); i = i + 1)
				_PE[i].sendF(c);

			init <= False;
		endrule

		rule _CLK (_rebut == False);
			clk <= clk + 1;
		endrule	
		
		rule _DRAMStrideFetch (_rebut == False);	
				 if(c1 == fromInteger(img)-1) begin
                                 	c1 <= 0;
                                 	if(r1 + fromInteger(roof) >= fromInteger(banks))
                                 		r1 <= 0;
                                 else 
                                         r1 <= r1 + fromInteger(roof);
                                 end
                                 else
                                 c1 <= c1 + 1;
				 
				 Vector#(16, DataType) data = newVector;
				 Vector#(16, BramLength) map = replicate(0);
				 for(BramLength i = 0; i < fromInteger(roof); i = i +1) begin
				 	let d = instream[i].first; instream[i].deq;
					//$display(" in line buffer %d @clk %d ", fxptGetInt(d), clk);
					let index = (r1 + i)%fromInteger(banks);
					map[index] = 1;
					data[index] = d;
				 end

				
				 inputFmap.write(data, map,c1);
				
				 if(r1 >= fromInteger(stencil)-1 && c1 >= fromInteger(stencil)) begin
				 readEnable <= True;
				 //$display(" setting read enable to true ");
				 end
			
				 if(readEnable == True)
				 _ena.send;
				_bp0.send;
		endrule

		rule _BRAMfetch (_rebut == False);
			_ena.ishigh;
			_bp0.ishigh;

			  	if(c2 == fromInteger(img)-1) begin
                                	c2 <= 0;
                          	if (r2 + fromInteger(roof) >= fromInteger(banks))
                                        r2 <= 0;
                                else 
                                        r2 <= r2 + fromInteger(roof);
                          	end
                          	else
                                c2 <= c2 + 1;

			inputFmap.read(c2);
			for(BramLength k = 0; k < fromInteger(roof); k = k + 1)
				for(BramLength i = 0; i < fromInteger(stencil); i = i +1) begin
					let index = (r2 + i + k)%fromInteger(banks);
					_readIndex[k][i].enq(index);
                         	end

			_bp1.send;
		endrule

		rule _latchData (_rebut == False);
			inputFmap.latch;
			_l.send;
			_bp1.ishigh;
		endrule

		rule _storeData;
			_l.ishigh;
			Vector#(16, DataType) g = inputFmap.get;
			for(BramLength i =0 ;i<fromInteger(banks); i = i+1)
				store[i] <= g[i];
			
			_latch <= True;
			for(int k = 0; k<fromInteger(roof); k = k+1)
                                _bp[k].send;
		endrule

		for (UInt#(10) k = 0 ; k <fromInteger(roof); k = k+1) begin
		rule getData (_latch== True);
				_bp[k].ishigh;
				for(Int#(8) i=0;i< fromInteger(stencil) ; i = i+1) begin
	                                let index =  _readIndex[k][i].first; _readIndex[k][i].deq; 
					let d = store[index];
					data[k][i] <= d;
				end
				collPulse[k].send;
		endrule
		

		rule collect; 
			collPulse[k].ishigh;
			for (UInt#(8) i = fromInteger(stencil*stencil) - 1; i >= fromInteger(stencil); i = i-1)
				windowBuffer[k][i-fromInteger(stencil)] <= windowBuffer[k][i];
			
		
			for (UInt#(8) i = 0;i < fromInteger(stencil); i = i+1) begin
				let d = data[k][i];
				windowBuffer[k][fromInteger(stencil*stencil-stencil)+i] <= d;
			end

			if(res[k] == fromInteger(img)-1)
				res[k] <= 0;
			else
				res[k] <= res[k] + 1;

			if(res[k] >= fromInteger(stencil)-1)
				 _macP[k] <= ~_macP[k];
		endrule


		rule pushMac(((_macP[k] ^ _macPulse[k]) == 1) && _rebut == False); 
			_macPulse[k] <= _macP[k];
			//$display(" convolution active at @clk %d ", clk);	
			Vector#(32, DataType)  _WB = newVector;
                        for(int i=0; i< fromInteger(stencil*stencil); i = i+1)
                        	_WB[i] = windowBuffer[k][i];
                        _PE[k].sendP(_WB);
			let id = fromInteger((stencil*stencil)/2);
			_forEnable[k].send;
			forwardingQ[k] <= windowBuffer[k][id];
		endrule

		rule popMAC (_rebut == False);
                        let d <- _PE[k].result;
			//$display(" getting result %d @clk %d ", fxptGetInt(d), clk);
                        forward[k].enq(d);
                endrule

		/*rule receivePort (_rebut == False);
                                  recvData[k] <= forward[k].first; forward[k].deq;
                                 _recvEnable[k].send;
                endrule*/

		end


		rule _reboot (_rebut == True && _FR > 0);
                        _latch <= False;
                        _bp0.clean;
                        _bp1.clean;
                        for(int i=0; i<fromInteger(roof); i = i+1) begin
                                instream[i].clear;
                                _bp[i].clean;
                                _macP[i] <= 0;
                                collPulse[i].clean;
                                res[i] <= 0;
                                for(int j=0;j<fromInteger(stencil); j = j + 1)
                                         _readIndex[i][j].clear;

				forward[i].clear;
                                _PE[i].clean;
                                _macPulse[i] <= 0;

                        end

                        c1 <= 0;
                        c2 <= 0;
                        r2 <= 0;
                        r1 <= 0;
                        readEnable <= False;
                        _ena.clean;
                        _l.clean;
                        _FR <= _FR - 1;
			clk <= 0;
                endrule

                rule _reboot_mem(_FR == 2  && _rebut == True);
                                inputFmap.clean;
                endrule


		method Action  cleaned if(_FR == 0);
                                _rebut <= False;
                endmethod

		method Action send(DataType dat, BramLength i);
				instream[i].enq(dat);	
		endmethod
                
		method ActionValue#(DataType) receive(BramLength i, UInt#(10) _rate) if(_rebut == False); 
					/*_recvEnable[i].ishigh;
					return recvData[i];*/
					let d = forward[i].first; forward[i].deq;
					return d;
                endmethod
		
		method ActionValue#(DataType) forwarded (BramLength i, UInt#(10) _rate);
					_forEnable[i].ishigh;
                                        return forwardingQ[i]; 
                endmethod

		method Action clean;
				 _rebut <= True;
                        	 _FR <= 2;
		endmethod

	  endmodule: mkStage

endpackage: Stage
                       
