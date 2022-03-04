import io
import codecs

# OPEN CHISE IDS TX FILE FROM GITHUB    https://github.com/kandu/chise_ids
# THE OUTPUT JSON HAS TO BE VALIDATED TO REMOVE TRAILING COMMAS AND FIX WRONG QUOTES

from joyo import joyo
from jinmeiyo import jinmeiyo
from keep import keep
Ideographic_Description_Characters = ["⿰", "⿱", "⿲", "⿳", "⿴", "⿵", "⿶", "⿷", "⿸", "⿹", "⿺", "⿻"]

f = io.open("IDS_MERGED.txt", "r", encoding='utf8')
print("reading")
lines = f.readlines()

# load joyo and jinmeiyo list
# keep = joyo + jinmeiyo




# print(len(keep))

# # FIND WHICH LINES NEED TO BE KEPT from the 200 000+ to only include yoyo and jinmeiyo + the ones used for composition
# # nem rekurzív, brute force
# for index in range(8):
#     for line in lines:
#         try:
#             id = line.split()[0]
#             kanji = line.split()[1]
#             rest = line.split()[2]
#             # IF KANJI FOUND IN LIST
#             if any(x in kanji for x in keep):
#                 # print(kanji)
#                 while (len(rest) > 0):
#                     # if the first character is a layout char
#                     if any(x in rest[0] for x in Ideographic_Description_Characters):
#                         # structure.append(rest[0])
#                         rest = rest[1:]

#                     # if special character
#                     if ( len(rest) > 0 and rest[0] == '&'):
#                         i = 0
#                         while (';' not in rest[i]):
#                             i += 1
#                         special_char = rest[1:i]
#                         keep.append(special_char)


#                         rest = rest [i+1:]

#                     # falls trough to normal character
#                     if (len(rest) > 0 and rest[0] != '&'  and not (rest[0] in Ideographic_Description_Characters)):
#                         keep.append(rest[0])
#                         rest = rest [1:]
#         except:
#             pass

#     print("iteration: " + str(index))
#     # remove duplicates
#     keep = list(set(keep))
#     # fix unicode to find potential
#     keep = [w.replace(r'\U000', 'U+') for w in keep]

# print(len(keep))




# saved = io.open("keep.txt", 'w', encoding='utf8')
# saved.write(str(keep))



# CREATING JSON FILEs
formatted = "{ \n"

nodes = ""
links = ""


for line in lines:
    try:
        id = line.split()[0]
        kanji = line.split()[1]
        rest = line.split()[2]
        structure = []
        composition = []


        # if ("27FB7" in line):
        #     print("DOUNF IT")
        #     print(line)
        #     print(line.split())
        #     print(line.split()[1])
        #     print("𧾷 ezt keressük")
        #     if ("𧾷" in line.split()[1]):
        #         print("true")
        #     else:
        #         print("false")
        #     if ("𧾷" == line.split()[1]):
        #         print("true")
        #     else:
        #         print("false")
        #     if any(x in kanji for x in keep):
        #         print("true")
        #     else:
        #         print("false")  



        # IF KANJI FOUND IN LIST
        if any(x in kanji for x in keep):
            while (len(rest) > 0):
                # if the first character is a layout char
                if any(x in rest[0] for x in Ideographic_Description_Characters):
                    structure.append(rest[0])
                    rest = rest[1:]

                # if special character
                if ( len(rest) > 0 and rest[0] == '&'):
                    i = 0
                    while (';' not in rest[i]):
                        i += 1
                    special_char = rest[1:i]
                    composition.append(special_char)

                    if(special_char != kanji):
                        links += "{ 'source': '" + str(special_char) + "', 'target': '" + str(kanji) + "'},\n"

                    rest = rest [i+1:]

                # falls trough to normal character
                if (len(rest) > 0 and rest[0] != '&'  and not (rest[0] in Ideographic_Description_Characters)):
                    composition.append(rest[0])

                    if(rest[0] != kanji):
                        links += "{ 'source': '" + str(rest[0]) + "', 'target': '" + str(kanji) + "'},\n"

                    rest = rest [1:]
                    # print(rest)



            nodes += "{ 'id': '" + str(kanji) + "'},\n"

            formatted += " '" + str(kanji) + "': {\n"
            # formatted += "'id' : '" + str(id) + "',\n"
            formatted += "'kanji' : '" + str(kanji) + "',\n"
            formatted += "'structure' : " + str(structure) + ",\n"
            formatted += "'composition' : " + str(composition) + "\n"
            formatted += "},\n"


    except:
        pass



KEEPNODES = ""

for xx in keep:
    KEEPNODES += "{ 'id': '" + str(xx) + "'},\n"

formatted = formatted[:-2] #remove trailing comma
formatted += "\n}"

wholegraph = "{ 'nodes': [" + KEEPNODES + nodes + "], 'links': [" + links + "]}"

# make valid json
formatted = formatted.replace("'", '"')
wholegraph = wholegraph.replace("'", '"')


# fix formatting
formatted = formatted.replace(r"\U000", "u")
formatted = formatted.replace("U+", "u")
formatted = formatted.replace("&", "")
formatted = formatted.replace(";", "")

wholegraph = wholegraph.replace(r"\U000", "u")
wholegraph = wholegraph.replace("U+", "u")
wholegraph = wholegraph.replace("&", "")
wholegraph = wholegraph.replace(";", "")



wholegraph_file = io.open("wholegraph.json", 'w', encoding='utf8')
wholegraph_file.write(wholegraph)


out = io.open("composition.json", 'w', encoding='utf8')
out.write(formatted)
