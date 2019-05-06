import java.util.TreeMap;
import java.util.Map;
import java.util.Deque;
import java.util.LinkedList;

public class Scope {
	private Deque<TreeMap<String, Double>> scopeStack;
	public Scope() {
		scopeStack = new LinkedList<TreeMap<String, Double>>();
		push();
	}
	public double get(String name) {
		for (Map<String, Double> map : scopeStack) {
			Double ret = map.get(name);
			if (ret != null)
				return ret;
		}
		return 0.0;
	}
	public void set(String name, double value) {
		Map<String, Double> first = scopeStack.peekFirst();
		Map<String, Double> last = scopeStack.peekLast();
		if (first.get(name) != null) {
			first.put(name, value);
		} else {
			first.put(name, value);
		}
	}
	public void setLocal(String name, double value) {
		scopeStack.peekFirst().put(name, value);
	}
	public void push() {
		scopeStack.push(new TreeMap<String, Double>());
	}
	public void pop() {
		scopeStack.pop();
	}
}
