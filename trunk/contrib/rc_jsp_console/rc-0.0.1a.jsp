<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">

<HTML>
<HEAD>
<%@ page 
language="java"
contentType="text/html; charset=ISO-8859-1"
pageEncoding="ISO-8859-1"
%>
<%@ page import="java.net.*"%>
<%@ page import="java.io.*"%>
<%
	String reply = "";
	String command = request.getParameter("command");
	String value = request.getParameter("value");
	String host = request.getParameter("host");
	String port = request.getParameter("port");
	StringBuffer buffy = null;

	if (command == null){
		command = "summary";
	}
	if (value == null){
		value = "";
	}
	if (host == null){
		host = "localhost";
	}
	if (port == null){
		port = "9011";
	}
	
	buffy = new StringBuffer("<tr valign=\"top\"><td colspan=\"2\">");
	buffy.append("received command: [").append(command).append("]<br>");
	buffy.append("received value: [").append(value).append("]<hr>");
	buffy.append("</td></tr>");
	Socket s = null;
	InputStreamReader isr = null;
	BufferedReader br = null;
	String version = "";
	try{
		// connect to host
		s = new Socket(host, new Integer(port).intValue());
		
		isr = new InputStreamReader(s.getInputStream());
		br = new 	BufferedReader (isr);
		StringBuffer b = new StringBuffer();

		// read the version
		version = br.readLine();
		
		// write the command		
		s.getOutputStream().write( command.getBytes());
		if ((command.equals("ping") || command.equals("keys") || command.equals("speed") || command.equals("enqueue") || command.equals("diskfree") )&&  value != null){
			s.getOutputStream().write( " ".getBytes());
			s.getOutputStream().write( value.getBytes());
		}
		s.getOutputStream().write( "\r\n".getBytes());
		
		// read the reply length
		String length = br.readLine();
		// read the reply
		char[] chars = new char[new Integer(length).intValue()];
		br.read(chars);
		b.append(chars);
		
		// quit gracefully according to rc protocol :)
		s.getOutputStream().write("quit".getBytes());
		s.getOutputStream().write( "\r\n".getBytes());
		length = br.readLine();
		chars = new char[new Integer(length).intValue()];
		br.read(chars);

		reply = b.toString();
	}catch (IOException ioe){
		reply = ioe.toString();
	}finally{
	
		if (br != null)br.close();
		if (isr!= null)isr.close();
		if ( s!= null)s.close();
	}
	
	
%>
<META http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<META name="GENERATOR" content="IBM WebSphere Studio">
<TITLE><%= version %></TITLE>

</HEAD>
<BODY>
<form method="POST">
	<table>
	
	<tr valign="top">
		<td>host <INPUT type="text" name="host" value="<%= host %>"><BR>
		port <INPUT type="text" name="port" value="<%= port %>"><BR>
		</td>
		<td>&nbsp;</td>
		<td>command:<br>
			<INPUT type="radio" name="command" value="summary" <%= command.equals( "summary")?  "CHECKED" : "" %>>summary<BR>
			<INPUT type="radio" name="command" value="ping" <%= command.equals( "ping")?  "CHECKED" : "" %> >ping <b>(<font color="red">*</font>)</b><BR>
			<INPUT type="radio" name="command" value="keys" <%= command.equals( "keys")?  "CHECKED" : "" %>>keys <b>(<font color="red">*</font>)</b><BR>
			<INPUT type="radio" name="command" value="speed" <%= command.equals( "speed")?  "CHECKED" : "" %>>speed <b>(<font color="red">*</font>)</b><BR>
			<INPUT type="radio" name="command" value="enqueue" <%= command.equals( "enqueue")?  "CHECKED" : "" %>>enqueue <b>(<font color="red">*</font>)</b><BR>
			<INPUT type="radio" name="command" value="diskfree" <%= command.equals( "diskfree")?  "CHECKED" : "" %>>diskfree<b>(<font color="red">*</font>)</b><BR>
			<br>
			 <b>(<font color="red">*</font>)</b> Needs value : <INPUT type="text" name="value" value="<%= value %>"><BR>
		</td>
	</tr>
	<tr>
		<td>
			&nbsp;
		</td>
		<td>
			&nbsp;
		</td>
		<td>
			<INPUT type="submit"><BR>
		</td>
	</tr>
	<tr valign="top">
		<td>
			reply:
		</td>
		<td>
			&nbsp;
		</td>
		<td>
			<textarea cols="80" rows="20"><%= reply != null ? reply : "" %></textarea>
		</td>
	</tr>
	
</form>
</BODY>
</HTML>
