/* Get a system property. */

public class getsp {
    public static void main(String[] args) {
	if((args != null) && (args.length == 1))
	    System.out.println(System.getProperty(args[0]));
	else
	    throw new RuntimeException("invalid argument");
    }
}
