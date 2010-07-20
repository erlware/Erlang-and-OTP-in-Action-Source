import com.ericsson.otp.erlang.*;

public class Test {
    public void main(String []args) throws Exception {

	OtpNode node = new OtpNode("myJavaNode");
	//OtpNode node = new OtpNode("myJavaNode", "secretcookie");

	OtpMbox named_mbox = node.createMbox("myNamedMbox");
	OtpMbox anon_mbox = node.createMbox();

	OtpErlangAtom   anAtom  = new OtpErlangAtom("some_atom");
	OtpErlangString aString = new OtpErlangString("Some string");
	OtpErlangInt    anInt   = new OtpErlangInt(22);
	
	OtpErlangTuple aTuple =
	    new OtpErlangTuple(new OtpErlangObject[]{anAtom, aString, anInt});
	
	anon_mbox.send("myNamedMbox", aTuple);

	OtpErlangObject msg = named_mbox.receive();
	
	OtpErlangTuple t = (OtpErlangTuple) msg;
	
	String theAtom   = ((OtpErlangAtom)   t.elementAt(0)).atomValue();
	String theString = ((OtpErlangString) t.elementAt(1)).stringValue();
	int    theInt    = ((OtpErlangInt)    t.elementAt(2)).intValue();
    }
}
