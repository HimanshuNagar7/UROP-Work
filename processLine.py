
def main():
    with open('line.txt', 'r') as file:
        line = file.read()
        startIndex = line.find('Solving') + 9
        endIndex = startIndex + line[startIndex:].find('s')
        print(line[startIndex : endIndex])

if __name__ == '__main__':
    main()
