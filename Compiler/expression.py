
from __future__ import division
import random

OPERATORS = set(['+', '-', '*', '/', '(', ')'])
PRIORITY = {'+':1, '-':1, '*':2, '/':2}

def evaluate_prefix(formula):
    exps = list(formula)
    body = {}
    id = 1;
    while len(exps) > 1:
        for i in range(len(exps)-2):
            if exps[i] in OPERATORS:
                if not exps[i+1] in OPERATORS and not exps[i+2] in OPERATORS:
                    op, a, b = exps[i:i+3]
                    #a,b = map(float, [a,b])
                    if(not a in body and  not b in body):
                        if(op == '+'):
                            body[id] = "fxptAdd(" + a + "," + b + ")"
                        if (op == '-'):
                            body[id] = "fxptSub(" + a + "," + b + ")"
                        if (op == '*'):
                            body[id] = "fxptMult(" + a + "," + b + ")"

                        if (op == '/'):
                            body[id] = "fxptQuot(" + a + "," + b + ")"


                    if( a in body and b in body):
                        if (op == '+'):
                            body[id] = "fxptAdd(" + body[a] + "," + body[b] + ")"
                        if (op == '-'):
                            body[id] = "fxptSub(" + body[a] + "," + body[b] + ")"
                        if (op == '*'):
                            body[id] = "fxptMult(" + body[a] + "," + body[b] + ")"
                        if (op == '/'):
                            body[id] = "fxptQuot(" + body[a] + "," + body[b] + ")"


                    if (a in body and (not b in body)):
                        if (op == '+'):
                            body[id] = "fxptAdd(" + body[a] + "," + b + ")"
                        if (op == '-'):
                            body[id] = "fxptSub(" + body[a] + "," + b + ")"
                        if (op == '*'):
                            body[id] = "fxptMult(" + body[a] + "," + b + ")"
                        if (op == '/'):
                            body[id] = "fxptQuot(" + body[a] + "," + b + ")"


                    if ((not a in body) and b in body):
                        if (op == '+'):
                            body[id] = "fxptAdd(" + a + "," + body[b] + ")"
                        if (op == '-'):
                            body[id] = "fxptSub(" + a + "," + body[b] + ")"
                        if (op == '*'):
                            body[id] = "fxptMult(" + a + "," + body[b] + ")"
                        if (op == '/'):
                            body[id] = "fxptQuot(" + a + "," + body[b] + ")"


                    exps = exps[:i] + [id] + exps[i+3:]
                    id = id + 1
                    break
    return body[id-1]

def formatExpression(formula):

    form = formula.replace(" ", "")
    reMap = {}
    index = 1
    i = 0
    while i < len(form):
        identifier = ''
        while not form[i] in OPERATORS:
            identifier += form[i]
            i = i + 1
            if (i == len(form)):
                break
        i = i + 1
        if (not identifier in reMap):
            reMap[identifier] = index
            index = index + 1

    if "" in reMap: del reMap[""]

    for key in reMap.keys():
        form = form.replace(key, str(reMap[key]))


    op_stack = []
    exp_stack = []
    for ch in form:
        if not ch in OPERATORS:
            exp_stack.append(ch)
        elif ch == '(':
            op_stack.append(ch)
        elif ch == ')':
            while op_stack[-1] != '(':
                op = op_stack.pop()
                a = exp_stack.pop()
                b = exp_stack.pop()
                exp_stack.append(op + b + a)
            op_stack.pop()  # pop '('
        else:
            while op_stack and op_stack[-1] != '(' and PRIORITY[ch] <= PRIORITY[op_stack[-1]]:
                op = op_stack.pop()
                a = exp_stack.pop()
                b = exp_stack.pop()
                exp_stack.append(op + b + a)
            op_stack.append(ch)

    # leftover
    while op_stack:
        op = op_stack.pop()
        a = exp_stack.pop()
        b = exp_stack.pop()
        exp_stack.append(op + b + a)

    expr = ''.join(exp_stack)
    expr = evaluate_prefix(expr)
    d2 = dict((v, k) for k, v in reMap.items())

    for key in d2.keys():
        expr = expr.replace(str(key),str(d2[key]))
    return "fxptTruncate(" + expr + ")"




#f =  "(_GX/_GY + _GX - _GY)"
#L = formatExpression(f)
#print(L)