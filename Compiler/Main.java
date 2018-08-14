import java.awt.Canvas;
import java.awt.Color;
import java.awt.Graphics;
import java.io.*;
import java.util.Random;
import javax.swing.JFrame;

class ReadImagePixels {
    int[][][] pixels;
    int[][][] pixel2;
    int[][][] pixel3;
    int[][][] pixel4;
    public ReadImagePixels(int rows, int cols, int rate, int dim)
    {
        pixels = new int[rows][cols][3];
        pixel2 = new int[rows][cols][3];
        pixel3 = new int[rows][cols][3];
        pixel4 = new int[rows][cols][3];
        try{
            FileReader frR = new FileReader("out.txt");
            BufferedReader brR = new BufferedReader(frR);
            int RATE = rate;
            try {
                for (int i = 0; i < dim/RATE; i++) {
                    for (int j = 0; j < dim; j++) {

                        for(int k=0; k<RATE; k++)
                            for (int l = 0; l < 1; l++) {
                                String lineR = brR.readLine();
                                if (lineR != null) {
                                    int val = Integer.parseInt(lineR.trim());
                                    if (val <0)
                                        pixels[i * RATE + k][j*1 + l][0] = 0;
                                    else if (val >255)
                                        pixels[i * RATE + k][j*1 + l][0] = 255;
                                    else
                                        pixels[i * RATE + k][j*1 + l][0] = val;
                                } else
                                    System.out.println("#");
                            }
                    }
                }
            }
            catch(Exception e) {
                e.printStackTrace();
            }
        }
        catch(Exception e)
        {
            e.printStackTrace();
        }
    }

}

public class Main extends Canvas {
    public int WIDTH = 0;
    public int HEIGHT = 0;
    private int DIM = 0;
    public int rate = 0;
    private static final Random random = new Random();
    ReadImagePixels p = null;
    int im;
    public Main(int image,int row, int col, int dim, int ra)
    {
        im = image;
        WIDTH = col;
        HEIGHT = row;
        DIM = dim;
        rate = ra;
        p = new ReadImagePixels(HEIGHT,WIDTH,rate,DIM);

    }
    public void paint(Graphics g) {
        super.paint(g);
        for (int r = 0; r < HEIGHT; r++) {
            for (int c = 0; c < WIDTH; c++) {
                if (im == 0)
                    g.setColor(new Color(p.pixels[r][c][0], p.pixels[r][c][0], p.pixels[r][c][0]));
                if (im == 1)
                    g.setColor(new Color(p.pixel2[r][c][0], p.pixel2[r][c][0], p.pixel2[r][c][0]));
                if (im == 2)
                    g.setColor(new Color(p.pixel3[r][c][0], p.pixel3[r][c][0], p.pixel3[r][c][0]));
                if (im == 3)
                    g.setColor(new Color(p.pixel4[r][c][0], p.pixel4[r][c][0], p.pixel4[r][c][0]));
                g.drawLine(r, c, r, c);
            }
        }
    }
    public static void main(String[] args) {
        int row = Integer.parseInt(args[0]);
        int col = Integer.parseInt(args[1]);
        int rate = Integer.parseInt(args[3]);
        int dim = (int)Math.sqrt(Integer.parseInt(args[2])*rate);
        JFrame frame = new JFrame();
        Main m = new Main(0,row,col,dim,rate);
        frame.setSize((m.WIDTH+10), (m.HEIGHT+10));
        frame.setTitle("frame");
        frame.add(m);
        frame.setVisible(true);
    }
}




