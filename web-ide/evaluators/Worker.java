/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package javaevaluator;
import java.net.*;
import java.io.*;
import java.util.*;
import javax.tools.JavaCompiler;
import javax.tools.JavaCompiler.CompilationTask;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import org.json.*;

/**
 *
 * @author Kevin
 */
public class Worker extends Thread {
    private Socket clientSocket;

    private String function;
    private String functionCall;
    private String expectedOutput;
    private String smessage;
    private String fmessage;

    private static final long TIMEOUT = 5000; //5 second timeout

    private Date initDate;

    /***
     * Creates a Worker object and initializes the socket
     * @param s - socket to client
     */
    public Worker(Socket s)
    {
        clientSocket = s;
        initDate = new Date();
    }

    /**
     * Returns the time, in milliseconds, at which this thread can be terminated.
     * The client connection has, at this point, already timed out the connection.
     * This will most likely occur because of an infinite loop.
     * @return time at which this thread should be terminated.
     */
    public long getTerminationDate()
    {
        return initDate.getTime() + TIMEOUT;
    }

    /***
     * Main Worker function.  Creates working directory for the thread,
     * creates references to socket IO streams, parses input from client,
     * creates the java file, compiles it, runs it, verifies output, and
     * sends results back to client.  
     */
    @Override
    public void run()
    {
    	try {
    		// the first thing we need is an output stream on which to respond, if 
    		// an error occurs.
    		PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);

    		try {
    			BufferedReader in = new BufferedReader(
    					new InputStreamReader(clientSocket.getInputStream()));

    			try {
    	        	File dir = new File(this.getName());
    	        	Boolean dirCreated = dir.mkdir();
    	        	
    	        	if (dirCreated) {
    	        		parseInput(in);
    	        		File file = createJavaFile(dir);
    	        	
    	        		String output = makeOutput(compile(file));
    	        		out.write(output);
    	        	} else {
    	        		out.write(dirCreationFailureMsg);
    	        	}
    			} catch (RuntimeException exn){
    				out.write(runtimeFailureMsg);
    			}
    			in.close();
    		} catch (IOException exn){
    			// couldn't read, but maybe we can write...
    			out.write(readFailureMsg);
    		}
    		out.close();

    	} catch (IOException exn){
    		// failed to create the output stream, no chance to respond.
    		System.err.println("failed to create output stream.");
    		exn.printStackTrace();
    	}
		clientSocket.close();

    }

    /***
     * Finds and loads the class specified by the client.  Runs the class
     * and returns output.
     * @return true - run was successful.  false - unsuccessful.
     */
    private boolean runFile()
    {
        boolean retVal = false;
        try{
            URL classUrl;
            File f = new File("");
            classUrl = f.toURI().toURL();
            URL[] classUrls = { classUrl };
            URLClassLoader ucl = new URLClassLoader(classUrls);
            String load = this.getName() + ".testFile";
            Class c = ucl.loadClass(load); //
            retVal = (Boolean)c.getDeclaredMethod("run", null).invoke(c.newInstance(), null);
        }
        catch(Exception e)
        {
            System.out.println("Worker.runFile(File) - failed to load the class");
            e.printStackTrace();
        }
        return retVal;
    }

    /***
     * Creates a java file based on the client's input
     * @param dir directory in which to create the java file.
     * @return a File object referencing the newly created file.
     */
    private File createJavaFile(File dir)
    {
        File retVal = new File(dir.getAbsoluteFile() + File.separator + "testFile.java");
        try{
            FileWriter writer = new FileWriter(retVal);
            writer.write("package " + this.getName() + ";\npublic class testFile {\n\tpublic boolean run() {\nreturn " + 
            		this.functionCall + " == " + expectedOutput + ";\n}\n" + function + "}");
            writer.close();
        }
        catch(IOException e)
        {
            System.out.println("Worker.createJavaFile(File) - failed to write to file: ");
            e.printStackTrace();
        }
        return retVal;
    }

    /***
     * Reads input from client and parses out the 5 arguments
     */
    private void parseInput(BufferedReader in)
    {
        String inputString = new String("");
        int length = -1;
        try{
            while(length != 0)
            {
                if(length == -1)
                {
                    String temp = in.readLine();
                    //System.out.println(temp);
                    if(temp == null)
                    {
                        break; //EOF
                    }
                    else if(temp.contains("Content-Length: "))
                    {
                        temp = temp.substring("Content-Length: ".length());
                        length = Integer.parseInt(temp);
                        in.readLine(); //empty line following content-length
                        //System.out.println();
                    }
                }
                else
                {
                    char[] cbuf = new char[length + 10];
                    int read = in.read(cbuf);
                    if(read > length)
                    {
                        read = length;
                    }
                    inputString = inputString + new String(cbuf);
                    if(read == -1)
                    {
                        break;
                    }
                    else
                    {
                        length -= read;
                    }
                }
            }
            //System.out.println(inputString);
            
            inputString = inputString.substring(inputString.indexOf('%'));
            
            inputString = java.net.URLDecoder.decode(inputString, "UTF-8");
            //System.out.println();
            //System.out.println();
            //System.out.println(inputString);
        }
        catch(IOException e)
        {
        }
        try{
            JSONObject json = new JSONObject(inputString);
            JSONObject args = (JSONObject)json.get("args");
            JSONObject textfields = (JSONObject)json.get("textfields");

            function = args.getString("function");
            functionCall = args.getString("functionCall");
            expectedOutput = args.getString("expectedOutput");
            smessage = args.getString("smessage");
            fmessage = args.getString("fmessage");

            function = replaceSegmentValues(function, textfields);
            functionCall = replaceSegmentValues(functionCall, textfields);
            expectedOutput = replaceSegmentValues(expectedOutput, textfields);
        }
        catch(JSONException j)
        {
            System.out.println("Worker.parseInput() - failed to parse input string");
            //j.printStackTrace();
        }
    }

    /**
     * Replaces segment values in the target string with the supplied value
     * @param str - target string
     * @param textfields - JSONObject that holds key-value pairs
     * @return resulting string
     */
    private String replaceSegmentValues(String str, JSONObject textfields)
    {
        String[] values = JSONObject.getNames(textfields);
        for(int i = 0; i < values.length; i++)
        {
            try{
                str = str.replace('@' + values[i], textfields.getString(values[i]));
            }
            catch(JSONException j) {}
        }
        return str;
    }

    /***
     * Deletes the given directory and all its contents
     * @param dir - directory to delete
     */
    public void deleteDirectory(File dir)
    {
        File[] files = dir.listFiles();
        if(files != null){
            for(int i = 0; i < files.length; i++)
            {
                if(files[i].isDirectory())
                    deleteDirectory(files[i]);
                files[i].delete();
            }
        }
        dir.delete();
    }

    /***
     * Compiles the file specified by 'file'
     * @param file - the file to be compiled
     * @return true indicates success, false is failure
     */
    private boolean compile(File file)
    {
        //String sourceFile = file.getAbsolutePath();
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if(compiler == null)
              System.out.println("Compiler not found on this machine");
        StandardJavaFileManager fileManager = compiler.getStandardFileManager (null,null,null);

        // prepare the source file(s) to compile
        List<File> sourceFileList = new ArrayList <File> ();
        sourceFileList.add (file);
        Iterable<? extends JavaFileObject> compilationUnits =fileManager.getJavaFileObjectsFromFiles (sourceFileList);

        CompilationTask task = compiler.getTask (null,fileManager, null, null, null, compilationUnits);
        boolean result = task.call();
        try {
            fileManager.close ();
        } catch (IOException e) {
        }
        return result;
    }

    /**
     * Returns the appropriate status code
     * @param status - determines which code to return
     * @return the status code
     */
    private String getStatus(boolean status)
    {
        if(status)
            return "success";
        else
            return "failure";
    }

    /**
     * If the compile succeeds, this will run and evaluate the file.
     * Regardless, this will create an output String that can be sent out over
     * the socket.
     * @param compile - flag indicating the success of the compile.
     * @return output String.
     */
    private String makeOutput(boolean compile) {
        String output = "";
        StringBuilder headers = new StringBuilder();
        headers.append("HTTP/1.1 200 OK\r\n");
        headers.append("Content-Type: text/html; charset=UTF-8\r\n");
        if(!compile)
        {
            try{
                JSONObject json = new JSONObject();
                json.put("message", "Your code does not compile. Try checking your syntax.<br />");
                json.put("status", getStatus(false));
                output = json.toString();
            }
            catch(JSONException j) { }
        }
        else{
            boolean result = runFile();
            try{
                JSONObject json = new JSONObject();
                if(result)
                {
                    json.put("message", smessage);
                }
                else{
                    json.put("message", fmessage);
                }
                json.put("status", getStatus(result));
                output = json.toString();
            }
            catch(JSONException j) { }
        }
        headers.append("Content-Length: " + output.length() + "\r\n");
        headers.append("\r\n");
        headers.append(output);
        return headers.toString();
    }
}
