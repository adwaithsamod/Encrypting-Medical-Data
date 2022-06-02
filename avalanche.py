def Aeffect(a,b):
# Python program to measure avalanche effect for cryptographic algorithms
   # a = 546313 # first_cipher
  # b = 37385 # second_cipher after chanage one bit
# print bitwise XOR operation
    a_xor_b = int(a,16) ^ int(b,16)
    # print("a ^ b =", a_xor_b)
# Base 2(binary)
    bin_a_xor_b = bin(a_xor_b)
    # print("the result in binary =", bin_a_xor_b)
# next steps to count 1s in binary number
    one_count = 0
    for i in bin_a_xor_b:
        if i == "1":
            one_count+=1
            #print ("the 1s numbers is=", one_count)
# next steps to calculate equation of avalanche effect
    len_a = len(bin(int(a,16))[2:0])
    len_b = len(bin(int(b,16))[2:0])
# if to ensure divide by the longest binary string
    if (len_a) >= (len_b):
        AVA = (one_count/ len (bin(int(a,16))[2:])) * 100
    else:
        AVA = (one_count/ len (bin(int(b,16))[2:])) * 100
    return AVA
 
# Aeffect("31093bced90d778e","bde5a9ad9f82cf06")

