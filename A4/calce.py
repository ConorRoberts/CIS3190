import math

# Save the value of arr to file
def keepe(fileName,arr):
    with open(fileName, 'w') as f:
        for i in arr:
            f.write(str(i))


if (__name__ == '__main__'):
    fileName = input("Enter file name: ")

    n = int(input("Enter number of digits: "))

    m = 4
    test = (n+1) * 2.30258509

    while ((m * (math.log(m)-1)+0.5 * math.log(6.2831852 * m)) < test):
        m += 1

    # Initialize coef array to be all 1s
    coef = [1 for _ in range(0, m+1)]
    arr = []

    arr.append(2)

    for _ in range(1, n):
        carry = 0
        for j in range(m, 1, -1):
            tmp = coef[j] * 10 + carry

            # Use integer division
            carry = tmp//j

            coef[j] = int(tmp-carry*j)
        arr.append(int(carry))

    # Join elements of arr to form string
    arr.insert(1, '.')

    keepe(fileName, arr)
