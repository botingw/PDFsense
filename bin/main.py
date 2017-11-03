import os

try:
    import Tkinter as tk
except:
    print "Tkinter module not found"

def submit():
    allradios = [i.get() for i in radios]
    allchecks = [[i.get() for i in j] for j in checks]
    alltexts = [i.get() for i in texts]

    #print allradios
    #print allchecks
    #print alltexts

    #Open config file
    configFile = file("./config1.txt","w")

    all_elements = allradios+allchecks+alltexts
    jobStr = ''.join([str(t) for t in all_elements])
    jobID = str(int(sum([(ord(jobStr[i])-45)*i for i in range(len(jobStr))])))
    configFile.write("Job ID (copy from the counter file): "+jobID+"\n")

    configFile.write("PDF set: "+pdfsets[allradios[0]]+"\n\n")

    configFile.write("Type:  1     2     3     4     5     6     7\n")
    configFile.write("Flag:  "+str([1 if i == allradios[1] else 0 for i in range(7)])[1:-1].replace(',','    ')+"\n\n")

    configFile.write("Expt. ID:   "+str(expids)[1:-1].replace(',','  ')+"\n")
    configFile.write("Expt. Flag:  "+str(allchecks[0])[1:-1].replace(',','    ')+"\n\n")

    configFile.write("Type:  "+'  '.join([i.replace(u'\u203e','b') for i in functions]).replace(',','  ')+"  user\n")
    configFile.write("Flag:  "+str([1 if i == allradios[4] else 0 for i in range(len(functions))])[1:-1].replace(',','  ')+"   "+str(allchecks[2][0])+"\n\n")

    configFile.write("Name: "+alltexts[4]+"\n")
    configFile.write("Values: "+alltexts[5]+"\n\n")

    xrangestr = "auto"
    if allchecks[3][0] != 1:
        xrangestr = alltexts[6]+" "+alltexts[7]
    configFile.write("xmin,   xmax:  "+xrangestr+"\n")
    murangestr = "auto"
    if allchecks[4][0] != 1:
        murangestr = alltexts[8]+" "+alltexts[9]
    configFile.write("mumin, mumax:      "+murangestr+"\n")
    nstr = "auto"
    if allchecks[5][0] != 1:
        nstr = alltexts[10]
    configFile.write("Number of bins: "+nstr+"\n")
    hxrangestr = "auto"
    if allchecks[6][0] != 1:
        hxrangestr = alltexts[12]+" "+alltexts[13]
    configFile.write("xmin, xmax: "+hxrangestr+"\n")
    configFile.write("ymin, ymax:  0 auto\n\n")

    configFile.write("Color by data percentage: 50 70 85\n\n")

    if len(alltexts[0]) == 0:
        alltexts[0] = 0
        alltexts[1] = 0
    if len(alltexts[2]) == 0:
        alltexts[2] = 0
        alltexts[3] = 0
    configFile.write("Type:  1     2     3     4     5     6     7\n")
    configFile.write("Mode:  "+str([allradios[2] for i in range(7)])[1:-1].replace(',','    ')+"\n")
    configFile.write("Mode 1 range: "+str([(float(alltexts[0]),float(alltexts[1])) for i in range(7)])[1:-1].replace(',',' ').replace('(',' ').replace(')',' ')+"\n")
    configFile.write("Mode 2 range: "+str([(float(alltexts[2]),float(alltexts[3])) for i in range(7)])[1:-1].replace(',',' ').replace('(',' ').replace(')',' ')+"\n\n")

    configFile.write("Size: "+pointsizes[allradios[3]].lower()+"\n\n")

    configFile.close()

    #os.system("cat ./mathscript_v17/bin/config1.txt")
    os.system("python2 genScript.py "+jobID+" &")

def reset():
    [i.set(0) for i in radios]
    [[i.set(0) for i in j] for j in checks[:-4]]
    [[i.set(1) for i in j] for j in checks[-4:]]
    [i.delete(0,tk.END) for i in texts]

def changeHL(one,two,three):
    global third
    third.grid_forget()
    third = tk.Frame(figuresCell)
    third.grid(row=2,column=1,pady="10")
    if hlmode.get() == 0:
        options = pointsizes
        var = pointsize
        for s in range(len(options)):
            tk.Label(third,text="Size of data points:").grid(row=0,column=0)
            tk.Radiobutton(third,text=pointsizes[s],variable=pointsize,value=s).grid(row=s+1,sticky="w")
    elif hlmode.get() == 1:
        tk.Label(third,text="Input range of values:").grid(row=0,column=0,columnspan=2)
        tk.Label(third,text="Min:").grid(row=1,column=0)
        rangeMin = tk.Entry(third)
        texts[0] = rangeMin
        rangeMin.insert(10,"0.0")
        rangeMin.grid(row=1,column=1)
        tk.Label(third,text="Max:").grid(row=2,column=0)
        rangeMax = tk.Entry(third)
        texts[1] = rangeMax
        rangeMax.insert(10,"0.0")
        rangeMax.grid(row=2,column=1)
    elif hlmode.get() == 2:
        tk.Label(third,text="Input range of percentages:").grid(row=0,column=0,columnspan=2)
        tk.Label(third,text="Min:").grid(row=1,column=0)
        rangeMin2 = tk.Entry(third)
        texts[2] = rangeMin2
        rangeMin2.insert(10,"0.0")
        rangeMin2.grid(row=1,column=1)
        tk.Label(third,text="Max:").grid(row=2,column=0)
        rangeMax2 = tk.Entry(third)
        texts[3] = rangeMax2
        rangeMax2.insert(10,"0.0")
        rangeMax2.grid(row=2,column=1)

def changeFunc(one,two,three):
    global subCell
    subCell.grid_forget()
    subCell = tk.Frame(functionsCell)
    subCell.grid(row=2,column=0)
    if user[0].get() == 0:
        for s in range(len(functions)):
            tk.Radiobutton(subCell,text=functions[s],variable=function,value=s).grid(row=int(s%(len(functions)/numcols)),column=2*int(s/(len(functions)/numcols)),columnspan=2,sticky='w')
    elif user[0].get() == 1:
        tk.Label(subCell,text="Name:").grid(row=0,column=0,sticky='w')
        funcName = tk.Entry(subCell)
        texts[4] = funcName
        funcName.grid(row=0,column=1)
        tk.Label(subCell,text="Enter 57 values:").grid(row=1,column=0,sticky='w')
        vals = tk.Entry(subCell)
        texts[5] = vals
        vals.grid(row=1,column=1)

def changeR1(one,two,three):
    global range1
    range1.grid_forget()
    range1 = tk.Frame(rangesCell,relief=tk.GROOVE,bd=2)
    range1.grid(row=1,column=0,padx="5",pady="5")
    tk.Label(range1,text="Data X:").grid(row=0)
    tk.Checkbutton(range1,text="Auto",variable=xRange[0]).grid(row=1,sticky='w')
    if xRange[0].get() == 0:
        tk.Label(range1,text="X-min:").grid(row=2,column=0,sticky='w')
        rmin1 = tk.Entry(range1,width=7)
        texts[6] = rmin1
        rmin1.insert(10,"0.00001")
        rmin1.grid(row=2,column=1)
        tk.Label(range1,text="X-max:").grid(row=3,column=0,sticky='w')
        rmax1 = tk.Entry(range1,width=7)
        texts[7] = rmax1
        rmax1.insert(10,"1.0")
        rmax1.grid(row=3,column=1)

def changeR2(one,two,three):
    global range2
    range2.grid_forget()
    range2 = tk.Frame(rangesCell,relief=tk.GROOVE,bd=2)
    range2.grid(row=1,column=1,padx="5",pady="5")
    tk.Label(range2,text=u"Data \u03bc:").grid(row=0)
    tk.Checkbutton(range2,text="Auto",variable=muRange[0]).grid(row=1,sticky='w')
    if muRange[0].get() == 0:
        tk.Label(range2,text=u"\u03bc-min:").grid(row=2,column=0,sticky='w')
        rmin2 = tk.Entry(range2,width=7)
        texts[8] = rmin2
        rmin2.insert(10,"1.0")
        rmin2.grid(row=2,column=1)
        tk.Label(range2,text=u"\u03bc-max:").grid(row=3,column=0,sticky='w')
        rmax2 = tk.Entry(range2,width=7)
        texts[9] = rmax2
        rmax2.insert(10,"2000.0")
        rmax2.grid(row=3,column=1)

def changeR3(one,two,three):
    global range3
    range3.grid_forget()
    range3 = tk.Frame(rangesCell,relief=tk.GROOVE,bd=2)
    range3.grid(row=1,column=2,padx="5",pady="5")
    tk.Label(range3,text="Histogram data:").grid(row=0,columnspan=3)
    tk.Checkbutton(range3,text="Auto",variable=hdRange[0]).grid(row=1,sticky='w')
    if hdRange[0].get() == 0:
        tk.Label(range3,text="Nbin:").grid(row=2,column=0,sticky='w')
        rmin3 = tk.Entry(range3,width=7)
        texts[10] = rmin3
        rmin3.insert(10,"20")
        rmin3.grid(row=2,column=1)
        #tk.Label(range3,text="X-max:").grid(row=3,column=0,sticky='w')
        #rmax3 = tk.Entry(range3,width=7)
        #texts[11] = rmax3
        #rmax3.insert(10,"1.0")
        #rmax3.grid(row=3,column=1)

def changeR4(one,two,three):
    global range4
    range4.grid_forget()
    range4 = tk.Frame(rangesCell,relief=tk.GROOVE,bd=2)
    range4.grid(row=1,column=3,padx="5",pady="5")
    tk.Label(range4,text="Histogram X:").grid(row=0,columnspan=3)
    tk.Checkbutton(range4,text="Auto",variable=hxRange[0]).grid(row=1,sticky='w')
    if hxRange[0].get() == 0:
        tk.Label(range4,text="X-min:").grid(row=2,column=0,sticky='w')
        rmin4 = tk.Entry(range4,width=7)
        texts[12] = rmin4
        rmin4.insert(10,"0.0")
        rmin4.grid(row=2,column=1)
        tk.Label(range4,text="X-max:").grid(row=3,column=0,sticky='w')
        rmax4 = tk.Entry(range4,width=7)
        texts[13] = rmax4
        rmax4.insert(10,"10.0")
        rmax4.grid(row=3,column=1)

def changeExp(one,two,three):
    global experiments
    if pdfset.get() == 0:
        expidFile = file("./exptidname_inconfig.txt","r")
        experiments = [i.split() for i in expidFile.readlines()]
        expids = [int(i[0]) for i in experiments if len(i) != 0]
        experiments = [i[1] for i in experiments if len(i) != 0]
    elif pdfset.get() == 1:
        expidFile = file("./exptidname_inconfig2.txt","r")
        experiments = [i.split() for i in expidFile.readlines()]
        expids = [int(i[0]) for i in experiments if len(i) != 0]
        experiments = [i[1] for i in experiments if len(i) != 0]
    expidFile.close()
    global experimentsCell
    experimentsCell.grid_forget()
    experimentsCell = tk.Frame(table,padx="8",pady="8")
    experimentsCell.grid(row=0,column=1,columnspan=3)
    tk.Label(experimentsCell,text="Experiments to include:").grid(row=0,columnspan=int(numcols))
    experiment = [tk.IntVar() for i in range(len(experiments))]
    checks.append(experiment)
    for e in range(len(experiments)):
        tk.Checkbutton(experimentsCell,text=experiments[e],variable=experiment[e]).grid(row=int(e%(len(experiments)/numcols)+1),column=int(e/(len(experiments)/numcols)))


#List labels and such
expidFile = file("./exptidname_inconfig.txt","r")

pdfsets = ["CT14NNLO","2017.1008.0954.-0500_CT14HERA2-jet.ev"]
global experiments
experiments = [i.split() for i in expidFile.readlines()]
expids = [int(i[0]) for i in experiments if len(i) != 0]
experiments = [i[1] for i in experiments if len(i) != 0]
figtypes = ["Experimental errors","Residuals","PDF errors on residuals","Sensitivity factor","Correlation"]
hlmodes = ["No highlighting","Value range","Percentage range"]
pointsizes = ["Tiny","Small","Medium","Large"]
functions = [u"b\u203e",u"c\u203e",u"s\u203e",u"d\u203e",u"u\u203e","g","u","d","s","c","b","q6","q7","q8"]

expidFile.close()

radios = []
checks = []
texts = []

#Initialize window
root = tk.Tk()
root.title("LHC Plotter")
root.option_add("*Font","default 12")
root.resizable(width=False,height=False)
root.configure(background='white')
root.tk_setPalette(background='white')
#root.geometry('648x906')

#Create header
tk.Label(root,text="LHC Particle Distributions",font="default 24",pady="8",padx="6",bg="#327800").pack(fill=tk.X,pady=5)
tk.Label(root,text="Southern Methodist University\nPhysics Department",font="default 18",pady="3",padx="4",bg="#C8F0C8").pack(fill=tk.X,pady=5)

#Create overarching grid structure
table = tk.Frame(root)
table.pack(fill=tk.BOTH,pady="5")

#Create first row
#Choose PDF set
pdfsetCell = tk.Frame(table,padx="5",pady="5")
pdfsetCell.grid(row=0,column=0)
tk.Label(pdfsetCell,text="Choose PDF set:").pack()
pdfset = tk.IntVar()
pdfset.trace("w",changeExp)
radios.append(pdfset)
for s in range(len(pdfsets)):
    tk.Radiobutton(pdfsetCell,text=pdfsets[s],variable=pdfset,value=s).pack()

#Experiments
numcols = 5.0
global experimentsCell
experimentsCell = tk.Frame(table,padx="8",pady="8")
experimentsCell.grid(row=0,column=1,columnspan=3)
tk.Label(experimentsCell,text="Experiments to include:").grid(row=0,columnspan=int(numcols))
experiment = [tk.IntVar() for i in range(len(experiments))]
checks.append(experiment)
for e in range(len(experiments)):
    tk.Checkbutton(experimentsCell,text=experiments[e],variable=experiment[e]).grid(row=int(e%(len(experiments)/numcols)+1),column=int(e/(len(experiments)/numcols)))

#Second row
#Figures to plot
figuresCell = tk.Frame(table,padx="8",pady="8")
figuresCell.grid(row=1,columnspan=4)
tk.Label(figuresCell,text="Figures to plot:").grid(row=0,column=0)
pointsOrNot = [tk.IntVar()]
checks.append(pointsOrNot)
tk.Checkbutton(figuresCell,text="Experimental\ndata points",variable=pointsOrNot[0]).grid(row=1,column=0)

first = tk.Frame(figuresCell)
first.grid(row=1,column=1)
figtype = tk.IntVar()
radios.append(figtype)
for s in range(len(figtypes)):
    tk.Radiobutton(first,text=figtypes[s],variable=figtype,value=s).grid(row=s,sticky="w")

second = tk.Frame(figuresCell)
second.grid(row=2,column=0,pady="10")
tk.Label(second,text="Highlight mode:").grid(row=0,column=0)
hlmode = tk.IntVar()
hlmode.trace('w',changeHL)
radios.append(hlmode)
for s in range(len(hlmodes)):
    tk.Radiobutton(second,text=hlmodes[s],variable=hlmode,value=s).grid(row=s+1,sticky="w")

global third
third = tk.Frame(figuresCell)
third.grid(row=2,column=1,pady="10")
#It's got multiple potential variables
pointsize = tk.IntVar()
radios.append(pointsize)
rangeMin = tk.Entry(third)
texts.append(rangeMin)
rangeMax = tk.Entry(third)
texts.append(rangeMax)
rangeMin2 = tk.Entry(third)
texts.append(rangeMin2)
rangeMax2 = tk.Entry(third)
texts.append(rangeMax2)
for s in range(len(pointsizes)):
    tk.Label(third,text="Size of data points:").grid(row=0,column=0)
    tk.Radiobutton(third,text=pointsizes[s],variable=pointsize,value=s).grid(row=s+1,sticky="w")

#Third row
#Functions to use
functionsCell = tk.Frame(table,padx="8",pady="8")
functionsCell.grid(row=2,columnspan=4)
tk.Label(functionsCell,text="Functions to use in correlations:").grid(row=0,column=0)
user = [tk.IntVar()]
user[0].trace('w',changeFunc)
checks.append(user)
tk.Checkbutton(functionsCell,text="user",variable=user[0]).grid(row=1,column=0,sticky='w')

global subCell
subCell = tk.Frame(functionsCell)
subCell.grid(row=2,column=0)
function = tk.IntVar()
radios.append(function)
funcName = tk.Entry(subCell)
texts.append(funcName)
vals = tk.Entry(subCell)
texts.append(vals)
for s in range(len(functions)):
    tk.Radiobutton(subCell,text=functions[s],variable=function,value=s).grid(row=int(s%(len(functions)/numcols)),column=2*int(s/(len(functions)/numcols)),columnspan=2,sticky='w')

#Fourth row
rangesCell = tk.Frame(table,width=648,padx="8",pady="8")#,relief=tk.GROOVE,bd=2)
#rangesCell.geometry("648x151")
rangesCell.grid(row=3,columnspan=4)

tk.Label(rangesCell,text="Ranges:").grid(row=0,sticky='w')

range1 = tk.Frame(rangesCell,relief=tk.GROOVE,bd=2)
range1.grid(row=1,column=0,padx="5",pady="5")
tk.Label(range1,text="Data X:").grid(row=0,sticky='w')
xRange = [tk.IntVar(value=1)]
xRange[0].trace('w',changeR1)
checks.append(xRange)
rmin1 = tk.Entry(range1)
texts.append(rmin1)
rmax1 = tk.Entry(range1)
texts.append(rmax1)
tk.Checkbutton(range1,text="Auto",variable=xRange[0]).grid(row=1,sticky='w')

range2 = tk.Frame(rangesCell,relief=tk.GROOVE,bd=2)
range2.grid(row=1,column=1,padx="5",pady="5")
tk.Label(range2,text=u"Data \u03bc:").grid(row=0,sticky='w')
muRange = [tk.IntVar(value=1)]
muRange[0].trace('w',changeR2)
checks.append(muRange)
rmin2 = tk.Entry(range2)
texts.append(rmin2)
rmax2 = tk.Entry(range2)
texts.append(rmax2)
tk.Checkbutton(range2,text="Auto",variable=muRange[0]).grid(row=1,sticky='w')

range3 = tk.Frame(rangesCell,relief=tk.GROOVE,bd=2)
range3.grid(row=1,column=2,padx="5",pady="5")
tk.Label(range3,text="Histogram data:").grid(row=0,sticky='w')
hdRange = [tk.IntVar(value=1)]
hdRange[0].trace('w',changeR3)
checks.append(hdRange)
rmin3 = tk.Entry(range3)
texts.append(rmin3)
rmax3 = tk.Entry(range3)
texts.append(rmax3)
tk.Checkbutton(range3,text="Auto",variable=hdRange[0]).grid(row=1,sticky='w')

range4 = tk.Frame(rangesCell,relief=tk.GROOVE,bd=2)
range4.grid(row=1,column=3,padx="5",pady="5")
tk.Label(range4,text="Histogram X:").grid(row=0,sticky='w')
hxRange = [tk.IntVar(value=1)]
hxRange[0].trace('w',changeR4)
checks.append(hxRange)
rmin4 = tk.Entry(range4)
texts.append(rmin4)
rmax4 = tk.Entry(range4)
texts.append(rmax4)
tk.Checkbutton(range4,text="Auto",variable=hxRange[0]).grid(row=1,sticky='w')

#Submit and reset buttons
buttonsCell = tk.Frame(root,padx="8",pady="5")
buttonsCell.pack()
tk.Button(buttonsCell,text="SUBMIT",command=submit).grid(row=0,column=0)
tk.Button(buttonsCell,text="RESET",command=reset).grid(row=0,column=1)

tk.mainloop()
