def adda(a):
    def addb(b):
        return a + b
    return addb

a = adda(10)
print(a(15)) # 25
