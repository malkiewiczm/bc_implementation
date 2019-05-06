grammar bc;

@header {
    import java.util.Map;
    import java.util.TreeMap;
    import java.util.Deque;
    import java.util.LinkedList;
}

@members {
    Map<String, Integer> functionNames = new TreeMap<>();
    Deque<mJmp> breakJmp = new LinkedList<mJmp>();
    Deque<mJmp> continueJmp = new LinkedList<mJmp>();
}

parseAll: statementList EOF
        { Program.ops.add(new mHalt()); }
    ;

statementList: statement* ;

block
    : '{' statementList '}'
    | statement
    ;

statement
    : statementBase
    | expr
        { Program.ops.add(new mPop()); }
    ;

statementSilent
    : statementBase
    | expr
        { Program.ops.add(new mPopSilent()); }
    ;

statementBase locals [mJmp jmp, mJz jz, mJmp jmpFor0, mJmp jmpFor1, mPush push]
    : ';'
    | ID '=' expr
        { Program.ops.add(new mStore($ID.text)); }
    | 'if' '(' expr ')'
        { $jz = new mJz(-1); Program.ops.add($jz); }
        block
        { $jz.val = Program.here(); }
        (
            'else'
            {
                $jmp = new mJmp(-1);
                Program.ops.add($jmp);
                ++$jz.val;
            }
            block
            { $jmp.val = Program.here(); }
        )?

    |
        {
            $jmp = new mJmp(-1);
            $jz = new mJz(-1);
            $jmpFor0 = new mJmp(-1);
            $jmpFor1 = new mJmp(-1);
        }
        /* A */
        'for' '(' statementSilent? ';'
        {
            $jmpFor0.val = Program.here();
        }
        /* B */
        expr? ';'
        {
            Program.ops.add($jz);
            Program.ops.add($jmpFor1);
            $jmp.val = Program.here();
            breakJmp.push(new mJmp(-1));
            continueJmp.push(new mJmp(Program.here()));
        }
        /* C */
        statementSilent? ')'
        {
            Program.ops.add($jmpFor0);
            $jmpFor1.val = Program.here();
        }
        block
        {
            Program.ops.add($jmp);
            $jz.val = Program.here();
            breakJmp.pop().val = Program.here();
            continueJmp.pop();
        }
    | 'while'
        {
            $jmp = new mJmp(Program.here());
            breakJmp.push(new mJmp(-1)); 
            continueJmp.push(new mJmp(Program.here()));
        }
        '(' expr ')'
        {
            $jz = new mJz(-1);
            Program.ops.add($jz);
        }
        block
        {
            Program.ops.add($jmp);
            $jz.val = Program.here();
            breakJmp.pop().val = Program.here();
            continueJmp.pop();
        }
    |
        {
            $jmp = new mJmp(-1);
            Program.ops.add($jmp);
        }
        'define' fname=ID '(' args+=ID? ( ',' args+=ID )*  ')'
        {
            functionNames.put($fname.text, Program.here());
            int expectedArgs = $args.size();
            Program.ops.add(new mPushScope(expectedArgs));
            for (int i = expectedArgs - 1; i >= 0; --i) {
                Program.ops.add(new mStoreLocal($args.get(i).getText()));
            }
        }
        block
        {
            Program.ops.add(new mPush(0.0));
            Program.ops.add(new mReturn());
            $jmp.val = Program.here();
        }
    | 'return' expr
        { Program.ops.add(new mReturn()); }
    | 'return'
        {
            Program.ops.add(new mPush(0.0));
            Program.ops.add(new mReturn());
        }
    | 'break'
        {
            Program.fatalIf(breakJmp.size() == 0, "called 'break' while not in a loop");
            Program.ops.add(breakJmp.peek());
        }
    | 'continue'
        {
            Program.fatalIf(continueJmp.size() == 0, "called 'continue' while not in a loop");
            Program.ops.add(continueJmp.peek());
        }
    | 'halt'
        { Program.ops.add(new mHalt()); }
    | 'print'
        {
            $push = new mPush(-1);
            Program.ops.add($push);
        }
        exprList
        {
            Program.ops.add(new mPrint());
            $push.value = Program.here();
        }
    | 'print'
        {
            $push = new mPush(-1);
            Program.ops.add($push);
        }
        '(' exprList ')'
        {
            Program.ops.add(new mPrint());
            $push.value = Program.here();
        }
    ;

expr locals [mPush push]
    : op=('++' | '--') ID {
            switch ($op.text) {
            case "++":
            Program.ops.add(new mPreInc($ID.text));
            break;
            default:
            Program.ops.add(new mPreDec($ID.text));
            }
        }
    | ID op=('++' | '--') {
            switch ($op.text) {
            case "++":
            Program.ops.add(new mPostInc($ID.text));
            break;
            default:
                Program.ops.add(new mPostDec($ID.text));
            }
        }
    | '-' expr
        { Program.ops.add(new mNegate()); }
    | expr '^' expr
        { Program.ops.add(new mExponent()); }
    | expr op=('*' | '/' | '%') expr {
            switch ($op.text) {
            case "*":
            Program.ops.add(new mMult());
            break;
            case "/":
            Program.ops.add(new mDiv());
            break;
            default:
            Program.ops.add(new mMod());
            }
        }
    | expr op=('-' | '+') expr {
            switch ($op.text) {
            case "-":
            Program.ops.add(new mSub());
            break;
            default:
            Program.ops.add(new mAdd());
            }
        }
    | expr op=('<' | '>' | '<=' | '>=' | '==' | '!=') expr {
            switch ($op.text) {
            case "<":
            Program.ops.add(new mLt());
            break;
            case ">":
            Program.ops.add(new mGt());
            break;
            case "<=":
            Program.ops.add(new mLte());
            break;
            case ">=":
            Program.ops.add(new mGte());
            break;
            case "==":
            Program.ops.add(new mEq());
            break;
            default:
            Program.ops.add(new mNeq());
            }
        }
    | '!' expr
        { Program.ops.add(new mComplement()); }
    | expr '&&' expr
        { Program.ops.add(new mAnd()); }
    | expr '||' expr
        { Program.ops.add(new mOr()); }
    | fname=ID
        {
            $push = new mPush(-1);
            Program.ops.add($push);
        }
        '(' exprList ')'
        {
            Integer location = functionNames.get($fname.text);
            if (location == null) {
                Mne m = Program.getBuiltinCall($fname.text);
                if (m == null) {
                    Program.fatal("function '" + $fname.text + "' does not exist");
                }
                Program.ops.add(m);
            } else {
                Program.ops.add(new mJmp(location));
            }
            $push.value = Program.here();
        }
    | '(' expr ')'
    | FLOAT
        { Program.ops.add(new mPush(Double.parseDouble($FLOAT.text))); }
    | ID
        { Program.ops.add(new mPushVar($ID.text)); }
    ;

exprList: args+=expr? (',' args+=expr)*
        {
           Program.ops.add(new mPush((double)$args.size()));
        };

FLOAT
    : '-'? Digit+ '.' Digit*
    | '-'? '.' Digit+
    | '-'? Digit+
    ;

fragment Digit: [0-9] ;

ID: [A-Za-z]+[A-Za-z0-9_]*;
WS : [ \r\n\t]+ -> skip ;
COMMENT : '/*' .*? '*/' -> skip ;
