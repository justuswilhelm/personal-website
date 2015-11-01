from timeit import timeit

setup = 'a = (i for i in range(100))'
print("Setup: {}".format(setup))

tuple_code = 'tuple(a)'
list_code = 'list(a)'
print("Tuple code: {}".format(tuple_code))
print("List code: {}".format(list_code))

tuple_time = timeit('tuple(a)', setup)
list_time = timeit('list(a)', setup)
print("Tuple creation: {} seconds".format(tuple_time))
print("List creation: {} seconds".format(list_time))

print("Tuple creation was {:.2f} times faster".format(
    float(list_time) / tuple_time, 2))
