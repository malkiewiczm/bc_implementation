/*
This file contains all the mnemonics for my version of bc
 */

import java.util.Scanner;

public abstract class Mne {
	public abstract void exec();
	@Override
	public String toString() {
		return getClass().getName();
	}
}

abstract class MneUnary extends Mne {
	public void exec() {
		double arg = Program.stack.pop();
		Program.stack.push(exec(arg));
	}
	protected abstract double exec(double arg);
}

abstract class MneBinary extends Mne {
	public void exec() {
		double arg2 = Program.stack.pop();
		double arg1 = Program.stack.pop();
		Program.stack.push(exec(arg1, arg2));
	}
	protected abstract double exec(double arg1, double arg2);
}

abstract class MneWithString extends Mne {
	protected String str;
	public MneWithString(String str) {
		this.str = str;
	}
	@Override
	public String toString() {
		return super.toString() + " " + str;
	}
}

abstract class MneWithInt extends Mne {
	public int val;
	public MneWithInt(int val) {
		this.val = val;
	}
	@Override
	public String toString() {
		return super.toString() + " " + val;
	}
}

abstract class MneCall extends Mne {
	public void exec() {
		int argCount = Program.stack.pop().intValue();
		double[] args = new double[argCount];
		for (int i = 0; i < argCount; ++i) {
			args[i] = Program.stack.pop();
		}
		exec(args);
	}
	protected abstract void exec(double[] args);
}

abstract class MneCallOneArg extends MneCall {
	protected void exec(double[] args) {
		Program.fatalIf(args.length > 1, "too many arguments for " + toString());
		Program.fatalIf(args.length == 0, "missing argument for " + toString());
		Program.stack.push(exec(args[0]));
	}
	protected abstract double exec(double arg);
}

abstract class MneCallNoArg extends MneCall {
	protected void exec(double[] args) {
		Program.fatalIf(args.length != 0, "too many arguments for " + toString());
		Program.stack.push(execSub());
	}
	protected abstract double execSub();
}

class mStore extends MneWithString {
	public mStore(String str) {
		super(str);
	}
	public void exec() {
		double val = Program.stack.pop();
		Program.scope.set(str, val);
	}
}

class mStoreLocal extends MneWithString {
	public mStoreLocal(String str) {
		super(str);
	}
	public void exec() {
		double val = Program.stack.pop();
		Program.scope.setLocal(str, val);
	}
}

class mPreInc extends MneWithString {
	public mPreInc(String str) {
		super(str);
	}
	public void exec() {
		double val = Program.scope.get(str);
		Program.scope.set(str, val + 1);
		Program.stack.push(val + 1);
	}
}

class mPreDec extends MneWithString {
	public mPreDec(String str) {
		super(str);
	}
	public void exec() {
		double val = Program.scope.get(str);
		Program.scope.set(str, val - 1);
		Program.stack.push(val - 1);
	}
}

class mPostInc extends MneWithString {
	public mPostInc(String str) {
		super(str);
	}
	public void exec() {
		double val = Program.scope.get(str);
		Program.scope.set(str, val + 1);
		Program.stack.push(val);
	}
}

class mPostDec extends MneWithString {
	public mPostDec(String str) {
		super(str);
	}
	public void exec() {
		double val = Program.scope.get(str);
		Program.scope.set(str, val - 1);
		Program.stack.push(val);
	}
}

class mNegate extends MneUnary {
	protected double exec(double arg) {
		return -arg;
	}
}

class mExponent extends MneBinary {
	protected double exec(double arg1, double arg2) {
		return Math.pow(arg1, arg2);
	}
}

class mMult extends MneBinary {
	protected double exec(double arg1, double arg2) {
		return arg1 * arg2;
	}
}

class mDiv extends MneBinary {
	protected double exec(double arg1, double arg2) {
		return arg1 / arg2;
	}
}

class mMod extends MneBinary {
	protected double exec(double arg1, double arg2) {
		return arg1 % arg2;
	}
}

class mSub extends MneBinary {
	protected double exec(double arg1, double arg2) {
		return arg1 - arg2;
	}
}

class mAdd extends MneBinary {
	protected double exec(double arg1, double arg2) {
		return arg1 + arg2;
	}
}

class mLt extends MneBinary {
	protected double exec(double arg1, double arg2) {
		return (arg1 < arg2) ? 1.0 : 0.0;
	}
}

class mGt extends MneBinary {
	protected double exec(double arg1, double arg2) {
		return (arg1 > arg2) ? 1.0 : 0.0;
	}
}

class mLte extends MneBinary {
	protected double exec(double arg1, double arg2) {
		return (arg1 <= arg2) ? 1.0 : 0.0;
	}
}

class mGte extends MneBinary {
	protected double exec(double arg1, double arg2) {
		return (arg1 >= arg2) ? 1.0 : 0.0;
	}
}

class mEq extends MneBinary {
	protected double exec(double arg1, double arg2) {
		return (arg1 == arg2) ? 1.0 : 0.0;
	}
}

class mNeq extends MneBinary {
	protected double exec(double arg1, double arg2) {
		return (arg1 != arg2) ? 1.0 : 0.0;
	}
}

class mComplement extends MneUnary {
	protected double exec(double arg) {
		return (arg != 0) ? 0.0 : 1.0;
	}
}

class mAnd extends MneBinary {
	protected double exec(double arg1, double arg2) {
		boolean v = (arg1 != 0) && (arg2 != 0);
		return v ? 1.0 : 0.0;
	}
}

class mOr extends MneBinary {
	protected double exec(double arg1, double arg2) {
		boolean v = (arg1 != 0) || (arg2 != 0);
		return v ? 1.0 : 0.0;
	}
}

class mPush extends Mne {
	public double value;
	public mPush(double value) {
		this.value = value;
	}
	public void exec() {
		Program.stack.push(this.value);
	}
	@Override
	public String toString() {
		return super.toString() + " " + value;
	}
}

class mPop extends Mne {
	public void exec() {
		System.out.println(Program.stack.pop());
	}
}

class mPopSilent extends Mne {
	public void exec() {
		Program.stack.pop();
	}
}

class mPushVar extends MneWithString {
	public mPushVar(String str) {
		super(str);
	}
	public void exec() {
		double val = Program.scope.get(str);
		Program.stack.push(val);
	}
}

class mJmp extends MneWithInt {
	public mJmp(int val) {
		super(val);
	}
	public void exec() {
		Program.PC = val;
	}
}

class mJz extends MneWithInt {
	public mJz(int val) {
		super(val);
	}
	public void exec() {
		double cmp = Program.stack.pop();
		if (cmp == 0.0) {
			Program.PC = val;
		}
	}
}

class mPushScope extends MneWithInt {
	public mPushScope(int val) {
		super(val);
	}
	public void exec() {
		int passedArgs = Program.stack.pop().intValue();
		if (passedArgs != val) {
			Program.fatal("function expected " + val + " args but you gave it " + passedArgs);
		}
		Program.scope.push();
	}
}

class mHalt extends Mne {
	public void exec() {
		System.exit(0);
	}
}

class mReturn extends Mne {
	public void exec() {
		Program.scope.pop();
		double ret = Program.stack.pop();
		Program.PC = Program.stack.pop().intValue();
		Program.stack.push(ret);
	}
}

class mSin extends MneCallOneArg {
	protected double exec(double arg) {
		return Math.sin(arg);
	}
}

class mCos extends MneCallOneArg {
	protected double exec(double arg) {
		return Math.cos(arg);
	}
}

class mLog extends MneCallOneArg {
	protected double exec(double arg) {
		return Math.log(arg);
	}
}

class mExp extends MneCallOneArg {
	protected double exec(double arg) {
		return Math.exp(arg);
	}
}

class mSqrt extends MneCallOneArg {
	protected double exec(double arg) {
		return Math.sqrt(arg);
	}
}

class mRead extends MneCallNoArg {
	private static Scanner scanner = new Scanner(System.in);
	protected double execSub() {
		Program.fatalIf(! scanner.hasNextDouble(), "bad number format");
		return scanner.nextDouble();
	}
}

class mPrint extends MneCall {
	protected void exec(double[] args) {
		for (int i = args.length - 1; i >= 0; --i) {
			System.out.print(args[i] + " ");
		}
		System.out.println();
	}
}
