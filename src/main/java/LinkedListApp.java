/**
 * Created by piyushm on 5/9/15.
 */
public class LinkedListApp {

    public static void main(String args[]) {

        System.out.println("stating program");

        LinkedList list = new LinkedList();
        list.appendItemToFirst(1, 24);
        list.appendItemToFirst(3, 25);
        list.appendItemToFirst(5, 26);
        list.appendItemToFirst(7, 27);
        list.appendItemToFirst(8, 28);
        list.appendItemToFirst(9, 29);
        System.out.println(list.find(70));
        for (int i = 0; i < 7; i++) {
            list.deleteItemFromFirst();
        }

        list.appendItemToFirst(1, 24);
        list.displayList();
    }
}

class LinkedList {

    public Link first;

    public Boolean isEmpty() {
        return (first == null);
    }

    public Link appendItemToFirst(int key, int data) {
        Link current = new Link(key, data);
        current.next = first;
        first = current;
        return first;
    }

    public Link deleteItemFromFirst() {
        if (!this.isEmpty()) {
            first = first.next;
        } else System.out.println("Empty List");
        return first;
    }

    public Link find(int key) {
        Link search = first;
        while (search.key != key) {
            if (search.next == null) {
                System.out.println("did not found key");
               return null;
            } else search = search.next;

        }
        return search;
    }

    public Link delete(int key)
    {
        Link search = first;
        while (search.key != key) {
            if (search.next == null) {
                System.out.println("did not found key");
                 return null;
            } else search = search.next;

        }
        return search;
    }

    public void displayList() {
        Link current = first;
        while (current != null) {
            System.out.println(current.toString());
            current = current.next;
        }

    }

}

class Link {

    public int key;
    public int data;
    public Link next;

    public Link(int key, int data) {
        this.key = key;
        this.data = data;
    }

    @Override
    public String toString() {
        return "Key :[" + key + "] data [" + data + "]";
    }
}
