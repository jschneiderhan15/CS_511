//Jack Schneiderhan and Cindy Zhang
//I pledge my honor that I have abided by the Stevens Honor System.
// 10-6-2021
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd;
    private List<BreadType> shoppingCart;
    private int shopTime;
    private int checkoutTime;

    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery) {
        // TODO
        this.bakery = bakery;
        this.shoppingCart = new ArrayList<BreadType>();
        this.rnd = new Random();
        this.shopTime = rnd.nextInt(5);        
        this.checkoutTime = rnd.nextInt(5);
        fillShoppingCart();
    }

    /**
     * Run tasks for the customer
     */
    public void run() { 
        // TODO
        System.out.println("Customer " + this.hashCode() + " has started shopping!");
        try {
            for(int i = 0; i < shoppingCart.size(); i++){
                if(shoppingCart.get(i) == BreadType.RYE){
                    this.bakery.shelfType[0].acquire();
                    this.bakery.takeBread(BreadType.RYE);
                    System.out.println("Customer " + this.hashCode() + " just took RYE!");
                    this.bakery.addSales(BreadType.RYE.getPrice());
                    System.out.println("Customer " + this.hashCode() + " just bought RYE!");
                    this.bakery.shelfType[0].release();
                }
                else if(shoppingCart.get(i) == BreadType.SOURDOUGH){
                    this.bakery.shelfType[1].acquire();
                    this.bakery.takeBread(BreadType.SOURDOUGH);
                    System.out.println("Customer " + this.hashCode() + " just took SOURDOUGH!");
                    this.bakery.addSales(BreadType.SOURDOUGH.getPrice());
                    System.out.println("Customer " + this.hashCode() + " just bought SOURDOUGH!");
                    this.bakery.shelfType[1].release();
                }
                else{
                    this.bakery.shelfType[2].acquire();
                    this.bakery.takeBread(BreadType.WONDER);
                    System.out.println("Customer " + this.hashCode() + " just took WONDER!");
                    this.bakery.addSales(BreadType.WONDER.getPrice());
                    System.out.println("Customer " + this.hashCode() + " just bought WONDER!");
                    this.bakery.shelfType[2].release();
                }
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        System.out.println("Customer " + this.hashCode() + " is leaving, bye!");
    }

    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingCart=" + Arrays.toString(shoppingCart.toArray()) + ", shopTime=" + shopTime + ", checkoutTime=" + checkoutTime;
    }

    /**
     * Add a bread item to the customer's shopping cart
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingCart.size() >= 3) {
            return false;
        }
        shoppingCart.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping cart with 1 to 3 random breads
     */
    private void fillShoppingCart() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping cart
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingCart) {
            value += bread.getPrice();
        }
        return value;
    }
}