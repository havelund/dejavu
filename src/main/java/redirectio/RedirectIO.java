package redirectio;

import java.io.*;

public class RedirectIO {
    PrintStream console = System.out;

    public void toFile() throws FileNotFoundException {
        PrintStream file = new PrintStream(new File("bdd.dot"));
        System.setOut(file);
    }

    public void toConsole() {
        System.setOut(console);
    }

    public static void main(String arr[]) throws FileNotFoundException {
        RedirectIO redirect = new RedirectIO();
        System.out.println("This will be written on the console!");
        redirect.toFile();
        System.out.println("This will be written to the text file");
        redirect.toConsole();
        System.out.println("This will also be written on the console!");
    }
}