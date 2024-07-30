with open("Custom_SL/SL.custom_caret.R", "r") as File:
    lines = File.readlines()

lines = lines[28:]
models = []
for line in lines:
    if '\"' in line:
        i = line.find('\"')
        j = line.rfind('\"')
        model = line[i+1:j]
        models.append(model)
print(models[65:])
