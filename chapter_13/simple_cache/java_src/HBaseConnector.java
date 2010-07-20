
import org.apache.hadoop.hbase.HBaseConfiguration;
import org.apache.hadoop.hbase.client.*;
import java.util.NavigableMap;

public class HBaseConnector {
  private HTable table;

  public HBaseConnector() throws Exception {
    super();
    table = new HTable(new HBaseConfiguration(), "cache");
  }

  public byte[] get(byte[] key) throws Exception {
    // Throws null pointer exception if key is not found
    Result result = table.get(new Get(key));
    NavigableMap<byte[], NavigableMap<byte[], byte[]>> map =
        result.getNoVersionMap();
    return map.get("value".getBytes()).get("".getBytes());
  }

  public void put(byte[] key, byte[] value) throws Exception {
    Put put = new Put(key);
    put.add("value".getBytes(), "".getBytes(), value);
    table.put(put);
  }

  public void delete(byte[] key) throws Exception {
    Delete del = new Delete(key);
    table.delete(del);
  }
}
