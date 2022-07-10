import properties

with open("tables/sbox_1.txt", 'r') as table:
	look_up = table.read()
look_up = look_up.split()

rows = [ look_up[17:33], look_up[34:50], look_up[51:67], look_up[68:84] ] #Should be changed according to format of txt.

fn_map = properties.function_generate(rows)

for i in range(OUT):
    print ('X' + str(i) + '\n:')
    print (fn_map[i])
