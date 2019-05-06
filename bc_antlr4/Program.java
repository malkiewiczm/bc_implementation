import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.CharStreams;
import java.util.List;
import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedList;

public class Program {
	public static List<Mne> ops = new ArrayList<>();
	public static int PC = 0;
	public static Deque<Double> stack = new LinkedList<>();
	public static Scope scope = new Scope();
	public static void main(String[] args) {
		boolean printMnemonics = args.length >= 2 && args[1].equalsIgnoreCase("debug");
		String fname = (args.length == 0) ? "-" : args[0];
		bcLexer lexer = null;
		try {
			if (fname.equals("-")) {
				lexer = new bcLexer(CharStreams.fromStream(System.in));
			} else {
				lexer = new bcLexer(CharStreams.fromFileName(fname));
			}
		} catch (java.io.IOException e) {}
		Program.fatalIf(lexer == null, "could not open input file");
        bcParser parser = new bcParser(new CommonTokenStream(lexer));
		parser.parseAll();
		if (printMnemonics) {
			for (int i = 0; i < ops.size(); ++i) {
				System.out.println(i + ": " + ops.get(i));
			}
			System.out.println("------");
		}
		for ( ; ; ) {
			ops.get(PC).exec();
			++PC;
		}
	}
	public static void fatal(String msg) {
		System.out.println("error: " + msg);
		System.exit(1);
	}
	public static void fatalIf(boolean what, String msg) {
		if (what) {
			fatal(msg);
		}
	}
	public static int here() {
		return ops.size() - 1;
	}
	public static Mne getBuiltinCall(String name) {
		/*
		  special functions:
		  s = sin(radians)
		  c = cos(radians)
		  l = log base e
		  e = exp base e
		  sqrt = sqrt
		  read = read from stdin
		  print = catenate and print to stdout
		*/
		switch (name) {
		case "s":
			return new mSin();
		case "c":
			return new mCos();
		case "l":
			return new mLog();
		case "e":
			return new mExp();
		case "sqrt":
			return new mSqrt();
		case "read":
			return new mRead();
		case "print":
			return new mPrint();
		default:
			return null;
		}
	}
}
