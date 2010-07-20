
import com.ericsson.otp.erlang.*;

public class JInterfaceExample {
  public static void main(String[] args) throws Exception {
    if (args.length != 3) {
      System.out.println("wrong number of arguments");
      System.out.println("expected: nodeName mailboxName cookie");
      return;
    }
    JInterfaceExample ex = new JInterfaceExample(args[0],args[1],args[2]);
    ex.process();
  }

  private OtpNode node;
  private OtpMbox mbox;

  public JInterfaceExample(String nodeName, String mboxName, String cookie)
  throws Exception {
    super();
    node = new OtpNode(nodeName, cookie);
    mbox = node.createMbox(mboxName);
  }

  private void process() {
    while (true) {
      try {
        OtpErlangObject msg = mbox.receive();
        OtpErlangTuple t = (OtpErlangTuple) msg;
        OtpErlangPid from = (OtpErlangPid) t.elementAt(0);
        String name = ((OtpErlangString) t.elementAt(1)).stringValue();
        String greeting = "Greetings from Java, " + name + "!";
        OtpErlangString replystr = new OtpErlangString(greeting);
        OtpErlangTuple outMsg =
          new OtpErlangTuple(new OtpErlangObject[]{mbox.self(),
                                                   replystr});
        mbox.send(from, outMsg);
      } catch (Exception e) {
          System.out.println("caught error: " + e);
      }
    }
  }

}
