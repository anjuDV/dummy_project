items = []
total = 0
while True:
    userInput = input("Enter the item name and price separated by a space or press q to quit: \n")
    if userInput != 'q':
        item_name, item_price = userInput.split()
        item_price = float(item_price)
        items.append((item_name, item_price))
        total += item_price
        print(f"Added {item_name} for {item_price:.2f}. Order total so far: {total:.2f}")
    else:
        print("Your order includes:")
        for item in items:
            print(f"{item[0]}: {item[1]:.2f}")
        print(f"Your bill total is {total:.2f}. Thanks for shopping!")
        break
