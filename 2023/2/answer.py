import re

max_red   = 12
max_green = 13
max_blue  = 14

file = open("input.txt", "r")
green = re.compile("(\d+)\sgreen")
blue  = re.compile("(\d+)\sblue")
red   = re.compile("(\d+)\sred")

result_1 = 0
result_2 = 0
i = 1

while True:
    content = file.readline()

    if not content:
        break

    grn_m = max(list(map(int, green.findall(content))))
    ble_m = max(list(map(int, blue.findall(content))))
    red_m = max(list(map(int, red.findall(content))))

    if grn_m <= max_green and ble_m <= max_blue and red_m <= max_red:
           result_1 += i

    result_2 += (grn_m * ble_m * red_m)
    i += 1

file.close()

print("The sum of the IDs of those games for part 1 is:", result_1)
print("The sum of the powers for part 2 is:", result_2)
