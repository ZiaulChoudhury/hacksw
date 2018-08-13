# This Makefile can be used from each of the Part subdirectories
# For example:    'make s1'

BSC=bsc

# ----------------------------------------------------------------
# Bluesim targets

.PHONY: dag harris harSin

dag3:
	$(BSC)  -verilog -u -cpp  +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing DAG.bsv
	$(BSC)  -sim  -u -g mkTestBench  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  TestBench.bsv
	$(BSC)  -sim  -e mkTestBench  -o ram  *.ba image-utilities.cpp

dag2:
	#$(BSC)  -verilog -u -cpp +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing TestBench.bsv
	#$(BSC)  -verilog -o ver -e mkTestBench *.v  
	$(BSC)  -verilog -u -cpp  +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing DAGPipe.bsv		
	$(BSC)  -sim  -u -g mkTestBench  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  TestBench.bsv
	$(BSC)  -sim  -e mkTestBench  -o ram  *.ba image-utilities.cpp


harris:
	$(BSC)  -verilog -u -cpp  +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing Harris.bsv
	$(BSC)  -sim  -u -g mkHarrisTB  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  HarrisTB.bsv
	$(BSC)  -sim  -e mkHarrisTB -o har  *.ba image-utilities.cpp

harSin:
	#$(BSC)  -verilog -u -cpp  +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing HarrSingle.bsv
	#$(BSC)  -verilog -o ver -e mkHarrSingle *.v 
	$(BSC)  -sim  -u -g mkHarrTBSingle  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  HarrTBSingle.bsv
	$(BSC)  -sim  -e mkHarrTBSingle -o har  *.ba image-utilities.cpp


sobel:
	#$(BSC)  -verilog -u -cpp +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing SobelTB.bsv
	#$(BSC)  -verilog -o ver -e mkSobelTB *.v  
	#$(BSC)  -verilog -u -cpp  +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing Sobel.bsv
	$(BSC)  -sim  -u -g mkSobelTB  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  SobelTB.bsv
	$(BSC)  -sim  -e mkSobelTB  -o sob  *.ba image-utilities.cpp

mult:
	#$(BSC)  -verilog -u -cpp  +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing Multirare.bsv
	$(BSC)  -sim  -u -g mkMultirateTB  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  MultirateTB.bsv
	$(BSC)  -sim  -e mkMultirateTB  -o mul  *.ba image-utilities.cpp

sr:
	#$(BSC)  -verilog -u -cpp  +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing SingleRatePyramid.bsv
	$(BSC)  -sim  -u -g mkSingleRateTB  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  SingleRateTB.bsv
	$(BSC)  -sim  -e mkSingleRateTB  -o sin  *.ba image-utilities.cpp

pad:
	$(BSC)  -verilog -u -cpp  +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing Pad.bsv
	$(BSC)  -sim  -u -g mkPadTB  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  PadTB.bsv
	$(BSC)  -sim  -e mkPadTB  -o mul  *.ba image-utilities.cpp


camera:
	$(BSC)  -sim  -u -g mkcameraTB  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  cameraTB.bsv
	$(BSC)  -sim  -e mkcameraTB  -o cam  *.ba image-utilities.cpp


blur:
	#$(BSC)  -verilog -u -cpp +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing NeuralNet.bsv
	#$(BSC)  -verilog -o ver -e mkConvolve *.v  
	$(BSC)  -sim  -u -g mkBlurTB  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  BlurTB.bsv
	$(BSC)  -sim  -e mkBlurTB  -o blu  *.ba Host.cpp

test:
	#$(BSC)  -verilog -u -cpp +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing Blur3TB.bsv
	#$(BSC)  -verilog -o ver -e mkBlur3TB *.v  
	$(BSC)  -sim  -u -g mkBlur3TB +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  Blur3TB.bsv
	$(BSC)  -sim  -e mkBlur3TB  -o blu  *.ba image-utilities.cpp


lucas:
	$(BSC)  -verilog -u -cpp  +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing Lucas.bsv
	$(BSC)  -sim  -u -g mkTestBenchLucas  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  TestBenchLucas.bsv
	$(BSC)  -sim  -e mkTestBenchLucas  -o sob  *.ba image-utilities.cpp


net:
	#$(BSC)  -verilog -u -cpp +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing TestBench.bsv
	#$(BSC)  -verilog -o ver -e mkTestBench *.v     
	$(BSC)  -sim  -u -g mkXilibus  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -show-range-conflict -cpp  Xilibus.bsv
	$(BSC)  -sim  -e mkXilibus  -o ram  *.ba host.cpp

scr:
	$(BSC)  -sim  -u -g mkScratch +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp scratch.bsv
	$(BSC)  -sim  -e mkScratch  -o blu  *.ba image-utilities.cpp


gau:
	$(BSC)  -verilog -u -cpp +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing GaussBlur.bsv
	$(BSC)  -verilog -o ver -e mkGaussBlur *.v  
	#$(BSC)  -sim  -u -g mkGaussTB  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  GaussTB.bsv
	#$(BSC)  -sim  -e mkGaussTB  -o gauss  *.ba image-utilities.cpp



canny:
	$(BSC)  -verilog -u -cpp +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing cann.bsv
	#$(BSC)  -verilog -o ver -e mkCann *.v  
	#$(BSC)  -sim  -u -g mkCannyTB  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  CannyTB.bsv
	#$(BSC)  -sim  -e mkCannyTB  -o cann  *.ba image-utilities.cpp

usm:
	#$(BSC)  -verilog -u -cpp +RTS -K20M -RTS -parallel-sim-link 8 -no-warn-action-shadowing USM.bsv
	#$(BSC)  -verilog -o ver -e mkUSM *.v  
	$(BSC)  -sim  -u -g mkUSMTB  +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp  USMTB.bsv
	$(BSC)  -sim  -e mkUSMTB  -o blu  *.ba image-utilities.cpp

hash:
	$(BSC)  -sim  -u -g mkHashTable +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp Hash.bsv
	$(BSC)  -sim  -e mkHashTable  -o has  *.ba image-utilities.cpp

bil:
	$(BSC)  -sim  -u -g mkBilateralTB +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp BilateralTB.bsv
	$(BSC)  -sim  -e mkBilateralTB  -o has  *.ba image-utilities.cpp



hard:
	$(BSC)  -sim  -u -g mkHardwareTestBench +RTS -K20M -RTS -show-schedule -parallel-sim-link 8 -no-warn-action-shadowing -cpp HardwareTestBench.bsv
	$(BSC)  -sim  -e mkHardwareTestBench -o hardware  *.ba image-utilities.cpp

# -----------------------------------------------------------------

.PHONY: clean fullclean

# Clean all intermediate files
clean:
	rm -f  *~  *.bi  *.bo  *.ba  *.h  *.cxx  *.o

# Clean all intermediate files, plus Verilog files, executables, schedule outputs
fullclean:
	rm -rf  *~  *.bi  *.bo  *.ba  *.h  *.cxx  *.o *.v xsim* *.sched
	rm -rf  *.exe   *.so  *.sched  *.v  *.vcd xe* ve* xsim* *.sched

