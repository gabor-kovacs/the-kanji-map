filenames = [

	'IDS-CBETA.txt', 
	'IDS-CDP.txt',
	'IDS-CNS-1.txt',
	'IDS-CNS-2.txt',
	'IDS-CNS-3.txt',

	'IDS-Daikanwa-01.txt',
	'IDS-Daikanwa-02.txt',
	'IDS-Daikanwa-03.txt',
	'IDS-Daikanwa-04.txt',
	'IDS-Daikanwa-05.txt',
	'IDS-Daikanwa-06.txt',
	'IDS-Daikanwa-07.txt',
	'IDS-Daikanwa-08.txt',
	'IDS-Daikanwa-09.txt',
	'IDS-Daikanwa-10.txt',
	'IDS-Daikanwa-11.txt',
	'IDS-Daikanwa-12.txt',

	'IDS-Daikanwa-dx.txt',
	'IDS-Daikanwa-ho.txt',
	

	'IDS-UCS-Basic.txt', 
	'IDS-UCS-Compat.txt', 
	'IDS-UCS-Compat-Supplement.txt', 
	'IDS-UCS-Ext-A.txt', 
	'IDS-UCS-Ext-B-1.txt', 
	'IDS-UCS-Ext-B-2.txt', 
	'IDS-UCS-Ext-B-3.txt', 
	'IDS-UCS-Ext-B-4.txt', 
	'IDS-UCS-Ext-B-5.txt', 
	'IDS-UCS-Ext-B-6.txt', 
	'IDS-UCS-Ext-C.txt', 
	'IDS-UCS-Ext-D.txt', 
	'IDS-UCS-Ext-E.txt', 
	'IDS-UCS-Ext-F.txt', 
	'IDS-UCS-Ext-G.txt']

with open('./chise-ids/IDS_MERGED.txt', 'w') as outfile:
    for fname in filenames:
        with open(fname) as infile:
            for line in infile:
            	if line.startswith(';;') == False:
                	outfile.write(line)



"""
	'IDS-HZK01.txt',
	'IDS-HZK02.txt',
	'IDS-HZK03.txt',
	'IDS-HZK04.txt',
	'IDS-HZK05.txt',
	'IDS-HZK06.txt',
	'IDS-HZK07.txt',
	'IDS-HZK08.txt',
	'IDS-HZK09.txt',
	'IDS-HZK10.txt',
	'IDS-HZK11.txt',
	'IDS-HZK12.txt',
	'IDS-JIS-X0208-1990.txt',
"""