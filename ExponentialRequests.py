import random
import sys
import math


def main(argv):
    f = open('happensEvents.lp', mode='w')
    f.write('\n'.join(generate(argv[0], argv[1], argv[2], argv[3], argv[4])))
    f.close()


def generate(no_customers, no_items, time_period, no_actions, seed):
    
    random.seed(seed)

    rate_parameter = float(no_actions)/time_period

    cur_time = max(1, next_time(rate_parameter))

    time_stamps = []

    while int(cur_time) < time_period - 3:
        
        time_stamps.append(int(cur_time))

        cur_time += next_time(rate_parameter)

    return ['happens(request(' +
            str(random.randint(1, no_customers)) + ' ,' +
            str(random.randint(1, no_items)) + '), ' +
            str(time_stamp) + '):-time(' + str(time_stamp) + ').'
            for time_stamp in time_stamps]


def next_time(rate_parameter):
    return -math.log(1.0 - random.random()) / rate_parameter

if __name__ == '__main__':
    #for x in sys.argv[1:]: print(x)
    main(list(map(int, sys.argv[1:])))
