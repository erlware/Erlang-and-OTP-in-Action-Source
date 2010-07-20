import com.ericsson.otp.erlang.*;

public class HBaseTask implements Runnable {
  private OtpMbox mbox;
  private HBaseConnector conn;
  private OtpErlangPid from;
  private OtpErlangRef ref;
  private String action;
  private byte[] key;
  private byte[] value;

  public HBaseTask(OtpMbox mbox, HBaseConnector conn,
                   OtpErlangPid from, OtpErlangRef ref,
                   String action, byte[] key, byte[] value) {
    super();
    this.mbox = mbox;
    this.conn = conn;
    this.from = from;
    this.ref = ref;
    this.action = action;
    this.key = key;
    this.value = value;
  }

  public void run() {
    try {
      if (action.equals("get")) {
        doGet();
      } else if (action.equals("put")) {
        doPut();
      } else if (action.equals("delete")) {
        doDelete();
      } else {
        System.out.println("invalid action: " + action);
      }
    } catch (Exception e) {
        System.out.println("caught error: " + e);
    }
  }

  private void doGet() throws Exception {
    OtpErlangObject result;
    try {
      result = new OtpErlangBinary(conn.get(key));
    } catch (NullPointerException e) {
      result = new OtpErlangAtom("not_found");
    }
    OtpErlangTuple reply = new OtpErlangTuple(new OtpErlangObject[] {
                                 new OtpErlangAtom("reply"), ref,
                                 result
                               });
    mbox.send(from, reply);
  }

  private void doPut() throws Exception {
    conn.put(key, value);
    OtpErlangTuple reply = new OtpErlangTuple(new OtpErlangObject[] {
                                 new OtpErlangAtom("reply"), ref, 
                                 new OtpErlangAtom("ok")
                               });
    mbox.send(from, reply);
  }

  private void doDelete() throws Exception {
    conn.delete(key);
    OtpErlangTuple reply = new OtpErlangTuple(new OtpErlangObject[] {
                                 new OtpErlangAtom("reply"), ref,
                                 new OtpErlangAtom("ok")
                               });
    mbox.send(from, reply);
  }
}
