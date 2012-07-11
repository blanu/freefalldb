package $package;

import android.util.Log;

import org.alexd.jsonrpc.*;
import org.alexd.jsonrpc.JSONRPCParams.Versions;

public class ${appname}Client
{
  #foreach($action in $actions)
  public void ${action.name}(${action.targs})
  {
    JSONRPCClient client = JSONRPCClient.create("http://${appname}.appspot.com/actions", Versions.VERSION_2);
//    JSONRPCClient client = JSONRPCClient.create("http://localhost:8080/actions", Versions.VERSION_2);
    client.setConnectionTimeout(2000);
    client.setSoTimeout(2000);
    try 
    {
      client.call("${action.name}", $action.args);
    }
    catch (JSONRPCException e)
    {
      Log.e("Client", e.toString());
      e.printStackTrace();
    }
  }
  #end
  
  #foreach($view in $views)
  public Object ${view.name}()
  {
    JSONRPCClient client = JSONRPCClient.create("http://${appname}.appspot.com/views", Versions.VERSION_2);
//    JSONRPCClient client = JSONRPCClient.create("http://localhost:8080/views", Versions.VERSION_2);
    client.setConnectionTimeout(2000);
    client.setSoTimeout(2000);
    try 
    {
      return client.call("${view.name}");
    }
    catch (JSONRPCException e)
    {
      Log.e("Client", e.toString());
      e.printStackTrace();
      return null;
    }
  }
  #end  
}
