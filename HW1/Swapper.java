//Jack Schneiderhan
//I pledge my honor that I have abided by the Stevens Honor System

public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    @Override
    public void run() {
    	int length = interval.getY() - interval.getX() + 1;
        for(int i = 0; i < length; i++) {
        	this.buffer[offset+i] = content.charAt(interval.getX()+i);
        }
    }
}