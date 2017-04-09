def __print_f(item):
        print item
# (lambda x: x <= 0 if __print_f(x + 3) else (lambda y: __print_f(y))(x * 2))(4)
