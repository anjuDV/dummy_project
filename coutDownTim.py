# import the time module 
import time 

def countdown(t):
    while t:
        mins, secs = divmod(t, 60)
        timer = '{:02d}:{:02d}'.format(mins, secs)
        print(timer, end="\r")
        time.sleep(1)
        t -= 1

    print('good job!!') 
#input time in second
t = input("Enter time in second: ") 

#function call
countdown(int(t))

