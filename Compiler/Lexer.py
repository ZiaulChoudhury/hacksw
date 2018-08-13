from DAG import*
from expression import *
import re
import math
import sys

def hasNumbers(inputString):
    return all(char.isdigit() for char in inputString)

def formatBody(body):
    exp = re.compile('expr[(][^;]+[)][;]')
    L = exp.findall(body)
    L2 = []
    for e in L:
        y = e.replace("expr","").replace(";","")
        x = formatExpression(y)
        L2.append(x+";")

    for i in range(0,len(L)):
        body = body.replace(L[i],L2[i])

    body = body.replace(";", ";\n")
    return body

class stage:
    def __init__ (self,ty,params, bd):

        ###### use in code generation ################
        self.type = ty
        self.sources = []
        self.relayInputs = []
        self.sourceRelays = {}
        self.inputId = {}
        self.bufferSizes = {}
        self.numOutputs = 0
        self.isRelaying = False
        self.function = bd.replace("x[", "in[")
        ##############################################

        self.InImg = 0;
        self.OutImg = 0;
        self.delta = 0;
        self.delays = {}
        self.parameters = params.strip('[|]')
        self.parameters = self.parameters.split(',')
        if(self.type != "Image"):
           i=0
           while(not hasNumbers(self.parameters[i])):
            self.sources.append(self.parameters[i])
            i = i + 1

    def addSourceRelay(self,source):
        self.sourceRelays[source] = True

    def removeSourceRelays(self,source):
        self.sourceRelays.pop(source,None)

    def addRelay(self,source):
        self.relayInputs.append(source)

    def removeRelay(self,source):
        self.relayInputs.remove(source)

    def inferImageDimensions(self,symbolTable):
        if(self.type == "Image"):
            self.InImg = int(self.parameters[1])
            self.OutImg = int(self.parameters[1])
        else:
            inx = [symbolTable[parent].OutImg for parent in self.sources]
            self.InImg = min(inx)
            if(self.type == "Tile"):
                self.OutImg = self.InImg - (int(self.parameters[1])-1)
            else:
                self.OutImg = self.InImg

    def updateDelta(self):
        if(self.type == "Tile"):
            self.delta = self.InImg*(int(self.parameters[1]) - 1 ) + ((int)(self.parameters[1]))
        else:
            self.delta = 1;

    def calculateDelays(self,symbolTable):
        if(self.type == "Source"):
            self.delays[self.sources[0]] = 0;
        else:
            for parent in self.sources:
                Pnode = symbolTable[parent]
                E = ((Pnode.OutImg - self.InImg)/2)*Pnode.OutImg + ((Pnode.OutImg - self.InImg)/2)
                delta = Pnode.delta;
                maxD = max(list(Pnode.delays.values()))
                self.delays[parent] = int(E + delta + maxD)

    def calculateBufferSizes(self):
        if(self.type != "Image"):
            cp = max(list(self.delays.values()))
            for parent in self.sources:
                self.bufferSizes[parent] = int(cp - self.delays[parent])

        return 0

    def assignWires(self,switchBoard,relayBoard):
        for parent in self.sources:
            if parent in self.sourceRelays:
                self.inputId[parent] = relayBoard[parent]
                relayBoard[parent] = relayBoard[parent] + 1
            else:
                self.inputId[parent] = switchBoard[parent]
                switchBoard[parent] = switchBoard[parent] + 1

    def setNumOutputs(self,num):
        self.numOutputs = num

def halt(symp):
    return 0;

def controller(DB,Rate,file):

    l1 = "rule initialize(init == True);\n"

    l1 += "\t\t init <= False;\n"
    i = 0;
    for constantkey in DB.constants.keys():
            info = DB.constants[constantkey]
            if info[1].__contains__("Vector"):
                weights = info[0].replace("[",'').replace("]",'').split(',')
                l1 += "\t\t " + "Vector#(32,DataType) weights_" + str(i) + " = newVector;\n"
                j = 0
                for weight in weights:
                    l1 += "\t\t weights_" + str(i) + "[" + str(j) + "]=" + weight + ";\n"
                    j = j + 1

                for key in DB.SymTab.keys():
                    Stage = DB.SymTab[key]
                    if(constantkey in Stage.parameters):
                        l1+= "\t\t_" + key + ".sendVector(weights_"+str(i) + ");\n"

            i = i + 1

    l1 += "endrule\n"
    file.write(l1 + "\n")


    l1="rule _pad(recv >= RECV && send < SEND);\n"
    l5 = "\tVector  # (8,DataType) pad = replicate(0);\n"
    l2 = "\tfor (BramLength i=0; i <" +  str(Rate) +"; i = i + 1)begin\n"
    l3 = ''
    for sources in DB.Topoolgy[1]:
                l3 += "\t" + "_"+sources+".send(pad[i],i);\n"
    l3 = "\t" + l3 + "\tend\n"
    l4 = "\nendrule"
    file.write(l1+l5+l2+l3+l4+"\n")

    l1="rule flushOut (send < SEND);\n"
    l2="\tfor(UInt#(10) i=0; i<" + str(Rate) + "; i = i + 1) begin\n"
    l3=''
    i=0
    for nodes in DB.Topoolgy:
        for node in nodes:
            Stage = DB.SymTab[node]
            if (Stage.numOutputs == 0):
                l3 = l3+"\t\tlet d" +str(i) + "  <-_" +node+ ".receive(0,i);\n"
                l3 = l3+ "\t\toutQ[i][" + str(i) + "].enq(truncate(pack(fxptGetInt(d" + str(i) + "))));\n"
                i = i + 1
    file.write(l1+l2+l3+"\n")
    file.write("\tend\n");
    file.write("endrule\n");

    l1 = "rule _ClearPipe(send == SEND && _c == False);\n"
    l2 = "\tfor (BramLength i=0; i <" +  str(Rate) +"; i = i + 1)begin\n"
    l2 += "\t\tfor (BramLength j=0; j <8 ; j = j + 1)\n"
    l3 = "\t\t\toutQ[i][j].clear;\n\tend\n\t\t_c <= True;\n"
    l4 = ''
    for key in DB.SymTab.keys():
        if(DB.SymTab[key].type != "Image"):
            l4 = l4 + "\t\t_"+ key +".clean;\n"

    file.write(l1 + l2 + l3 + l4 + "\n")
    file.write("endrule\n");

    l1 = "rule _ResetDone (_c == True && _clear == False);\n"
    l2 = "\t\t _clear <= True;\n"
    l4 = ''
    for key in DB.SymTab.keys():
        if(DB.SymTab[key].type == "Tile"):
            l4 = l4 + "\t\t_" + key + ".cleaned;\n"

    file.write(l1 + l2 + l4 + "\n")
    file.write("endrule\n");

    l1 = "rule _ResetDone2 (_c == True && _clear == True);\n"
    l2 =  "\t_c <= False;\n\t _clear <= False;\n"
    l3 = "\tsend <= 0; recv <= 0;\n"
    file.write(l1 + l2 + l3 + "\n")
    file.write("endrule\n");

    totalOut = DB.OUTPUTS * int(Rate)* 10

    l1 = "method ActionValue#(Bit#(" + str(totalOut) + ")) get if(send < SEND);\n"
    l2 = "\t\tVector#(" + str(int(totalOut/10)) +",Bit#(10)) data = newVector;\n"
    l3 = "\t\tfor (BramLength i=0; i <" + str(Rate) + "; i = i + 1)\n"
    l3 += "\t\tfor (BramLength j=0; j <" + str(DB.OUTPUTS) +"; j = j + 1)begin\n"
    l3 += "\t\t\t data[i*" + str(DB.OUTPUTS) + "+j] = outQ[i][j].first; outQ[i][j].deq;\n \t\tend\n"
    l3 += "\t\t\t send <= send + 1; \n"
    l3 += "\t return pack(data);\n"
    file.write(l1 + l2 + l3 + "\n")
    file.write("endmethod\n\n\n");

    totalOut = DB.INPUTS * int(Rate) * 10
    l1 =  "method Action put(Bit#(" + str(totalOut) +") datas) if(outQ[0][0].notFull && recv < RECV);\n"
    l2 = "\t\tVector#(" + str(int(totalOut / 10)) + ",Bit#(10)) data = unpack(datas);\n"
    i = 0
    l3 = "\t\tfor (BramLength i=0; i <" + str(Rate) + "; i = i + 1) begin\n"
    for node in DB.Topoolgy[0]:
        l3 += "\t\t\t\tInt#(10) x = " + "unpack(data["+str(i) +"+i*" + str(DB.INPUTS)+"]);\n"
        l3 += "\t\t\t\t_"+DB.AdjList[node][0][0] + ".send(fromInt(x),i);\n"
        i = i + 1
    l3 +="\t\tend\n"
    l3 += "\t\t\t recv <= recv + 1; \n"
    file.write(l1 + l2 + l3 + "\n")
    file.write("endmethod\n\n\n");

def codeGenerate(DB):
    f = open("Hardware.bsv", "w")


    Imports = "package Hardware; \nimport TubeHeader:: *; \nimport FixedPoint:: *; \nimport datatypes:: *; \nimport Compose:: *; \nimport Vector:: *; \nimport FIFO:: *; \nimport FIFOF:: *;"
    f.write(Imports + "\n\n")
    RECV = 0
    for pipeSrc in DB.Topoolgy[0]:
            RECV = DB.SymTab[pipeSrc].InImg * DB.SymTab[pipeSrc].InImg
    SEND = 0
    RATE = 0
    for nodes in DB.Topoolgy:
        for node in nodes:
            Stage = DB.SymTab[node]
            if (Stage.numOutputs == 0):
                SEND = Stage.OutImg * Stage.OutImg
                RATE = Stage.parameters[-1]

    f.write("#define RECV  " + str(RECV) + "\n")
    f.write("#define SEND  " + str(SEND+1) + "\n")
    f.write("interface Stdin;\n\n")

    totalOut = DB.OUTPUTS * int(RATE) * 10
    l1 = "\tmethod ActionValue#(Bit#(" + str(totalOut) + ")) get;\n"
    totalOut = DB.INPUTS * int(RATE) * 10
    l2 = "\tmethod Action put(Bit#(" + str(totalOut) + ") datas);\n"
    f.write(l1+l2+"\nendinterface\n\n")
    f.write("(*synthesize *) \nmodule mkHardware(Stdin);\n")
    initblock = "FIFOF#(Bit#(10)) outQ[8][8]; \nReg#(int) clk <- mkReg(0); \nReg#(UInt#(22)) recv <- mkReg(0); \nReg#(UInt#(22)) send <- mkReg(0);\nReg#(Bool) _c <- mkReg(False);\nReg#(Bool) _clear <- mkReg(False);\nfor (int i = 0; i < 8; i = i + 1)\n\tfor (int j = 0; j < 8; j = j + 1)\n\t\toutQ[i][j] <- mkFIFOF;\nReg#(Bool) init <- mkReg(True);\n"
    f.write(initblock+"\n")

    for nodes in DB.Topoolgy:
        for node in nodes:
            #f.write("//############################################# \n ")
            Stage = DB.SymTab[node]
            children = [x[0] for x in DB.AdjList[node]]
            if(Stage.type == "Source"):
                sz=''
                for child in children:
                    sz += str(DB.SymTab[child].bufferSizes[node]+32) + ","
                buffer = "Integer " + "buffer_"+node+"["+str(Stage.numOutputs)+"]={" + sz
                buffer = buffer[:buffer.rfind(',')] + "};"
                f.write(buffer + "\n")
                Comp = "Component _" + node + " <- " + "mkSource(" + str(Stage.numOutputs) + " , " + "buffer_"+node + " , "
                Comp = Comp + str(Stage.parameters[-1]) + ");"
                f.write(Comp + "\n")

            if(Stage.type == "Tile"):
                sz = ''
                j = 0;
                for child in children:
                    if(j < Stage.numOutputs):
                        sz += str(DB.SymTab[child].bufferSizes[node] + 32) + ","
                    j = j + 1
                buffer = "Integer " + "buffer_" + node + "[" + str(Stage.numOutputs) + "]={" + sz
                buffer = buffer[:buffer.rfind(',')] + "};"
                f.write(buffer + "\n")
                Comp = ''
                if(Stage.isRelaying == True):
                    relay = ''
                    rs = ''
                    i = 0
                    for tonode in Stage.relayInputs:
                        dim1 = int((Stage.InImg - DB.SymTab[tonode].InImg)/2)
                        dim2 = int((Stage.InImg - dim1))
                        sz   =  max(list(Stage.delays.values())) + Stage.delta
                        sz   =  int((max(list(DB.SymTab[tonode].delays.values())) - sz)  + 32)
                        relay += "Relay r" + str(i) + "= Relay{row1:" + str(dim1) + "," + "row2: " + str(dim2) + "," + "col1: " + str(dim1) + " , " + "col2: " + str(dim2) + "," + "size: " + str(sz) + "};\n"
                        rs += "r" + str(i) + ","
                        i = i + 1
                    rs = rs[:rs.rfind(',')] + "};\n"
                    relay += "Relay relays[" + str(i) + "] = {" + rs
                    Comp = relay
                    Comp += "Component _" + node + " <- " + "mkRelayBuffer(" + "_" + Stage.sources[0] + " , " + str(Stage.inputId[Stage.sources[0]]) + " , "
                    Comp = Comp + Stage.parameters[1] + " , " + Stage.parameters[2] + " , "
                    Comp += str(Stage.numOutputs) + " , " + "buffer_"+node + " , " + str(i) + " , relays , " +  str(Stage.InImg) + ", "
                    Comp += Stage.parameters[3] + " , " + str(Stage.parameters[4]) + ", " + str(Stage.parameters[5]) + ");"
                else:
                    Comp = "Component _" + node + " <- " + "mkTile(" + "_"+Stage.sources[0] + " , " + str(Stage.inputId[Stage.sources[0]]) + " , "
                    Comp = Comp + Stage.parameters[1] + " , " + Stage.parameters[2] + " , " + str(Stage.InImg) + " , "
                    Comp = Comp + str(Stage.numOutputs) + " , " + "buffer_"+node + " , " + Stage.parameters[3] + " , "
                    Comp = Comp + str(Stage.parameters[4]) + ", " + str(Stage.parameters[5]) + ");"
                f.write(Comp + "\n")

            if (Stage.type == "Convolution"):
                sz = ''
                if(Stage.numOutputs == 0):
                    buffer = "Integer " + "buffer_" + node + "[1]={32};"
                    f.write(buffer + "\n")
                else:
                    for child in children:
                        sz += str(DB.SymTab[child].bufferSizes[node] + 32) + ","
                    buffer = "Integer " + "buffer_" + node + "[" + str(Stage.numOutputs) + "]={" + sz
                    buffer = buffer[:buffer.rfind(',')] + "};"
                    f.write(buffer + "\n")
                Comp = "Component _" + node + " <- " + "mkConvolver(" + "_"+Stage.sources[0] + " , " + str(Stage.inputId[Stage.sources[0]]) + " , "
                Comp = Comp + str(Stage.parameters[1]) + ", "
                if(Stage.numOutputs == 0):
                    Comp = Comp + "1" + " , " + "buffer_"+node + " , "+ str(Stage.parameters[2]) + ");"
                else:
                    Comp = Comp + str(Stage.numOutputs) + " , " + "buffer_" + node + " , " + str(Stage.parameters[-1]) + ");"
                f.write(Comp + "\n")

            if (Stage.type == "Point"):
                sz = ''
                if(Stage.numOutputs == 0):
                    buffer = "Integer " + "buffer_" + node + "[1]={32};"
                    f.write(buffer+"\n")
                else:
                    for child in children:
                        sz += str(DB.SymTab[child].bufferSizes[node] + 32) + ","
                    buffer = "Integer " + "buffer_" + node + "[" + str(Stage.numOutputs) + "]={" + sz
                    buffer = buffer[:buffer.rfind(',')] + "};"
                    f.write(buffer + "\n")

                Comp = "Component" +" _inputs_" + node + "[" + str(len(Stage.sources)) + "]= {"
                inp = ''
                for source in Stage.sources:
                    inp += "_" + source + " , "
                inp = inp[:inp.rfind(',')]
                Comp = Comp + inp + "};"
                f.write(Comp + "\n")

                Comp = "Integer" + " _inputId_" + node + "[" + str(len(Stage.sources)) + "]= {"
                inp = ''
                for source in Stage.sources:
                    inp +=  str(Stage.inputId[source])+ " , "
                inp = inp[:inp.rfind(',')]
                Comp = Comp + inp + "};"
                f.write(Comp + "\n")

                Comp = "Bool" + " _relay_" + node + "[" + str(len(Stage.sources)) + "]= {"
                inp = ''
                for source in Stage.sources:
                    if(source in  Stage.sourceRelays):
                        inp += "True" + ","
                    else:
                        inp += "False" + ","
                inp = inp[:inp.rfind(',')]
                Comp = Comp + inp + "};"
                f.write(Comp + "\n\n")

                Comp = "function DataType func_" + node + "(Reg#(DataType) in[]);"
                f.write(Comp + "\n")
                f.write(Stage.function + "\n")
                f.write("endfunction\n")

                Comp = "Component _" + node + " <- mkPointComponent(" + "func_" + node + ","
                Comp = Comp + "_inputs_" + node + " , "
                Comp = Comp + str(len(Stage.sources)) + " ,"
                Comp = Comp + "_relay_" + node + " , "
                Comp = Comp + "_inputId_" + node + " , "
                if(Stage.numOutputs == 0):
                    Comp = Comp + "1" + " , " + "buffer_" + node + " , " + str(Stage.parameters[-1]) + ");"
                else:
                    Comp = Comp + str(Stage.numOutputs) + " , " + "buffer_" + node + " , " + str(Stage.parameters[-1]) + ");"
                f.write(Comp + "\n")
            f.write("\n\n")
    controller(DB,RATE,f)
    f.write("endmodule\n")
    f.write("endpackage\n")


class lex:

    def __init__ (self, file):
        with open(file, 'r') as myfile:
            program = myfile.read()

        program = program.replace('\n', ' ').replace('\r', ' ')
        program = program.rstrip()
        symbolTable = {}
        adjList = {}
        switchBoard = {}
        self.relayBoard = {}
        self.constants = {}
        self.INPUTS = 0
        self.OUTPUTS = 0;

        Components = program.split(">;")
        if "" in Components: Components.remove("")
        if "\n" in Components: Components.remove("\n")

        for Comp in Components:
            name = Comp[:Comp.find("<=")].strip(';|\n').replace(" ","")
            type = Comp[Comp.rfind('<')+1:].strip(';').replace(" ","")
            switchBoard[name] = 0;
            self.relayBoard[name] = 0;
            body = ""
            if(not type.__contains__("Constant")):
                if(type == "Point"):
                    body = Comp[Comp.find('{')+1: Comp.rfind('}')]
                    Comp = Comp.replace(body, "")
                    body = formatBody(body)
                parameters = Comp[Comp.find('['): Comp.rfind(']')+1].strip(';').replace(" ","")
                s = stage(type,parameters,body)
                symbolTable[name]=s
            else:
                parameters = Comp[Comp.find('['): Comp.rfind(']') + 1].strip(';').replace(" ", "")
                self.constants[name] = (parameters,type)



        graph = {}
        for node in symbolTable.keys():
            adjList[node]= []
            if(len(symbolTable[name].sources) > 0):
                graph[node] = set(symbolTable[node].sources)


        ### Creating the topological sort of the DAG ##########
        for node in symbolTable.keys():
            for source in symbolTable[node].sources:
                adjList[source].append((node,0))

        topology = list(toposort(graph))

        #######################################################

        print(topology)
        optRelay = {}
        ### Getting the relaying edges in the DAG #########################
        for i in range(0,len(topology)-1):
            for node in topology[i]:
                childs = [n[0] for n in adjList[node]]
                s = topology[i+1].intersection(set(childs))
                x = set(childs).difference(s)
                for m in list(x):
                    symbolTable[node].addRelay(m)
                    symbolTable[m].addSourceRelay(node)
                    optRelay[node] = m
                    print(" relayin from " + node + " to " + m)
        ####################################################################

        #### Inferring the image dimensions of the nodes ###################
        for nodes  in topology:
            for node in nodes:
                symbolTable[node].inferImageDimensions(symbolTable)
        #####################################################################

        ################## update delta values ##############################
        for node in symbolTable.values():
            node.updateDelta()
        #####################################################################

        for nodes in topology[1]:
            symbolTable[nodes].calculateDelays(symbolTable)

        ################## update the edge delays ############################
        for i in range(2,len(topology)):
            for node in topology[i]:
                symbolTable[node].calculateDelays(symbolTable)
        ######################################################################

        ######################################################################
        for i in range(1,len(topology)):
            for node in topology[i]:
                symbolTable[node].assignWires(switchBoard,self.relayBoard)

        for node in symbolTable.keys():
            symbolTable[node].setNumOutputs(switchBoard[node])

        for node in symbolTable.keys():
            if(symbolTable[node].numOutputs == 0):
                self.OUTPUTS = self.OUTPUTS + 1
            if (symbolTable[node].type == "Image"):
                self.INPUTS = self.INPUTS + 1

        for node in symbolTable.values():
            node.calculateBufferSizes()
        #######################################################################

        self.SymTab = symbolTable
        self.Topoolgy = topology
        self.AdjList = adjList
        self.optRelay = optRelay


        ############ Add relays here #########################################
        for relaynode in self.optRelay.keys():
            if(self.SymTab[relaynode].type == "Tile"):
                self.SymTab[relaynode].isRelaying = True


        ######################################################################
        halt(self)


def generateTestBench(DB):
    f = open("HardwareTestBench.bsv", "w")
    Comp = "package HardwareTestBench; \nimport Hardware:: *; \nimport FixedPoint:: *; \nimport Vector:: *; \n"
    Comp += "import \"BDPI\" function Action initialize_image(); \nimport \"BDPI\" function Int#(32) readPixel1(Int#(32) ri, Int#(32) cj, Int#(32) ch); \nimport \"BDPI\" function Int#(32) readPixel2(Int#(32) ri, Int#(32) cj, Int#(32) ch);\n\n\n"

    IMG = 0
    for pipeSrc in DB.Topoolgy[0]:
            IMG = DB.SymTab[pipeSrc].InImg

    Comp += "#define IMG " + str(IMG) + "\n"

    SEND = 0
    RATE = 0
    for nodes in DB.Topoolgy:
        for node in nodes:
            Stage = DB.SymTab[node]
            if (Stage.numOutputs == 0):
                SEND = Stage.OutImg * Stage.OutImg
                RATE = Stage.parameters[-1]

    Comp += "#define SEND " + str(SEND) + "\n"
    Comp += "#define K " + str(RATE) + "\n\n\n"

    with open('testbench.txt', 'r') as myfile:
        program = myfile.read()
    Comp += program + "\n"
    f.write(Comp + "\n")
    return 0

file = sys.argv[1]
if file.endswith('.hw'):
    DataBase = lex(file)
    codeGenerate(DataBase)
    generateTestBench(DataBase)
else:
    print(" Unknown file extension ")
