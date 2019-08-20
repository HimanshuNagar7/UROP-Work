import random
import sys


def main(argv):
    f = open('happensEvents.lp', mode='w')
    f.write('\n'.join(generate(argv[0], argv[1], argv[2], argv[3], argv[4])))
    f.close()


def generate(no_customers, no_items, time_period, no_actions, seed):

    random.seed(seed)

    time_stamps = sorted([random.randint(1, time_period - 4) for _ in range(no_actions)])

    return ['happens(request(' +
            str(random.randint(1, no_customers)) + ' ,' +
            str(random.randint(1, no_items)) + '), ' +
            str(time_stamp) + '):-time(' + str(time_stamp) + ').'
            for time_stamp in time_stamps]


if __name__ == '__main__':
    main(list(map(int, sys.argv[1:])))
