package HardwareTestBench; 
import Hardware:: *; 
import FixedPoint:: *; 
import Vector:: *; 
import "BDPI" function Action initialize_image(); 
import "BDPI" function Int#(32) readPixel1(Int#(32) ri, Int#(32) cj, Int#(32) ch); 
import "BDPI" function Int#(32) readPixel2(Int#(32) ri, Int#(32) cj, Int#(32) ch);


#define IMG 1024
#define SEND 1044484
#define K 1


module mkHardwareTestBench();

		Reg#(int) clk <- mkReg(0);
		Reg#(int) rows  <- mkReg(0);
       		Reg#(int) cols  <- mkReg(0);
		Reg#(Bool) init <- mkReg(True);
		Reg#(int) c0 <- mkReg(0);
		Reg#(int) c1 <- mkReg(0);
		Stdin cnnR <- mkHardware;
		Reg#(int) test <- mkReg(0);

		rule init_rule (init) ;
                	initialize_image();
                	init <= False;
      		endrule

		rule update_clock;
                   	test <= test + 1;
			clk <= clk + 1;
      		endrule


		rule layerIn(clk>=1);
			if(cols == IMG - 1) begin
                                cols <= 0;
                                rows <= rows + K;
                        end
                        else
                        cols <= cols + 1;
			Vector#(K, Int#(10)) s1 = newVector;
			if(rows <= IMG-K) begin

					for(int i=0; i<K; i = i+1) begin
					//Int#(8) pixlR = truncate(((rows + extend(i))*cols + 10)%255);
					Int#(10) pixlR = truncate(readPixel1((rows + i), cols,0));
					s1[i] = pixlR;
					end
					cnnR.put(pack(s1));
			end
		endrule

         	rule layerOut;  //(clk %20 == 0); // (!(clk > 100 && clk <1000));
                                        if( c0 < SEND) begin
                                        let d <- cnnR.get;
                                                if( c1 == 0) begin
                                                UInt#(10) dx = unpack(d);
                                                        $display(" %d " , dx );
                                                end
                                        c0 <= c0 + 1;
                                        end
                                        else begin
                                        c0 <= 0;
                                        rows <= 0;
                                        if(c1 == 0)
                                                        $finish(0);
                                                else
                                                        c1 <= c1+1;
                                        end
                endrule

endmodule

endpackage


