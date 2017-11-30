import os,sys

try:
    import Tkinter as tk
except:
    print("Tkinter module not found")
    sys.exit()

def submit():
    print(texts[0])
    print(texts[2])
    allradios = [i.get() for i in radios]
    allchecks = [[i.get() for i in j] for j in checks]
    alltexts = [i.get() for i in texts]

    #print allradios
    #print allchecks
    #print alltexts

    if not any(allchecks[0]):
        print "No options selected"
    else:

        #Open config file
        configFile = open("./config1.txt","w")

        all_elements = allradios+allchecks+alltexts
        jobStr = ''.join([str(t) for t in all_elements])
        jobID = str(int(sum([(ord(jobStr[i]))*i for i in range(len(jobStr))])))
        configFile.write("Job ID (copy from the counter file): "+jobID+"\n")

        configFile.write("PDF set: "+pdfsets[allradios[0]]+"\n\n")

        configFile.write("Type:  1     2     3     4     5     6     7\n")
        configFile.write("Flag:  "+str(allchecks[1][0])+'    '+str([1 if (i+1) == allradios[1] else 0 for i in range(6)])[1:-1].replace(',','    ')+"\n\n")

        configFile.write("Expt. ID:   "+str(expids)[1:-1].replace(',','  ')+"\n")
        configFile.write("Expt. Flag:  "+str(allchecks[0])[1:-1].replace(',','    ')+"\n\n")

        configFile.write("Type:  "+'  '.join([i.replace(u'\u203e','b') for i in functions]).replace(',','  ')+"  user\n")
        configFile.write("Flag:  "+str([1 if i == allradios[4] else 0 for i in range(len(functions))])[1:-1].replace(',','  ')+"   "+str(allchecks[2][0])+"\n\n")

        #configFile.write("Name: "+alltexts[4]+"\n")
        #configFile.write("Values: "+alltexts[5]+"\n\n")

        xrangestr = "auto  auto"
        if allchecks[3][0] != 1:
            xrangestr = alltexts[6]+" "+alltexts[7]
        configFile.write("xmin,   xmax:  "+xrangestr+"\n")
        murangestr = "auto  auto"
        if allchecks[4][0] != 1:
            murangestr = alltexts[8]+" "+alltexts[9]
        configFile.write("mumin, mumax:      "+murangestr+"\n")
        nstr = "auto"
        if allchecks[5][0] != 1:
            nstr = alltexts[10]
        configFile.write("Number of bins: "+nstr+"\n")
        hxrangestr = "auto  auto"
        if allchecks[6][0] != 1:
            hxrangestr = alltexts[12]+" "+alltexts[13]
        configFile.write("xmin, xmax: "+hxrangestr+"\n")
        configFile.write("ymin, ymax:  0 auto\n\n")

        configFile.write("Color by data percentage: 50 70 85\n\n")

##20171120 botingw: highlight input convention change, highlight mode = {{h1min,h1max},{h2min,h2max},...}
        if len(alltexts[0]) == 0:
            alltexts[0] = str("{{0,0}}")
            alltexts[1] = 0
        if len(alltexts[2]) == 0:
            alltexts[2] = str("{{0,0}}")
            alltexts[3] = 0
        configFile.write("Type:  1     2     3     4     5     6     7\n")
        configFile.write("Mode:  "+str([allradios[2] for i in range(7)])[1:-1].replace(',','    ')+"\n")

##20171120 botingw: highlight input convention change, highlight mode = {{h1min,h1max},{h2min,h2max},...}
#        configFile.write("Mode 1 range: "+str([(float(alltexts[0]),float(alltexts[1])) for i in range(7)])[1:-1].replace('(','{{ ').replace('),','}}; ').replace(')','}} ')+"\n")
#        configFile.write("Mode 2 range: "+str([(float(alltexts[2]),float(alltexts[3])) for i in range(7)])[1:-1].replace('(','{{ ').replace('),','}}; ').replace(')','}} ')+"\n\n")
        configFile.write("Mode 1 range: "+str([alltexts[0] for i in range(7)])[1:-1].replace('\',',';').replace('\'',' ')+"\n")
        configFile.write("Mode 2 range: "+str([alltexts[2] for i in range(7)])[1:-1].replace('\',',';').replace('\'',' ')+"\n\n")
        print(alltexts[0])
        print(alltexts[2])


        configFile.write("Size: "+pointsizes[allradios[3]].lower()+"\n\n")

        configFile.close()

        ##20171116 botingw: new way to run uesr define function is not inputting from python script   
        #Write user defined values file
#        userFile = file("./user_define_func.txt","w")
#        userFile.write("Name: "+alltexts[4]+"\n")
#        userFile.write("Values: "+alltexts[5]+"\n\n")
#        userFile.close()

        #os.system("cat ./config1.txt")
        os.system("python2 genScript.py "+jobID+" &")

def reset():
    #[i.set(0) for i in radios]
    [[i.set(0) for i in j] for j in checks[:-4]]
    [[i.set(1) for i in j] for j in checks[-4:]]
    [i.delete(0,tk.END) for i in texts[2:]]

def changeHL(one,two,three):
    global third
    third.grid_forget()
    third = tk.Frame(figuresCell)
    third.grid(row=1,column=3,pady="10")
    if hlmode.get() == 0:
        options = pointsizes
        var = pointsize
        for s in range(len(options)):
            tk.Label(third,text="Size of data points:").grid(row=0,column=0)
            tk.Radiobutton(third,text=pointsizes[s],variable=pointsize,value=s).grid(row=s+1,sticky="w")
    elif hlmode.get() == 1:
        tk.Label(third,text="Input range of values:").grid(row=0,column=0,columnspan=2)

##20171120 botingw: input convention of highlight mode = {{h1min,h1max},{h2min,h2max},...}
        HLrange1 = tk.Entry(third)
        texts[0] = HLrange1
        HLrange1.insert(10,"{{-100,-1},{1,100}}")
        HLrange1.grid(row=1,column=1)
        tk.Label(third,text="Range:").grid(row=2,column=0)
##to keep the length of text array, add a dummy variable
        texts[1]=dummy1
        '''
        tk.Label(third,text="Min:").grid(row=1,column=0)

        rangeMin = tk.Entry(third)
        texts[0] = rangeMin
        rangeMin.insert(10,"0.5, 0.1")
        rangeMin.grid(row=1,column=1)

        tk.Label(third,text="Max:").grid(row=2,column=0)
        rangeMax = tk.Entry(third)
        texts[1] = rangeMax
        rangeMax.insert(10,"0.0")
        rangeMax.grid(row=2,column=1)
        '''


    elif hlmode.get() == 2:
        tk.Label(third,text="Input range of percentages:").grid(row=0,column=0,columnspan=2)
##20171120 botingw: input convention of highlight mode = {{h1min,h1max},{h2min,h2max},...}
        HLrange2 = tk.Entry(third)
        texts[2] = HLrange2
        HLrange2.insert(10,"{{0,15},{85,100}}")
        HLrange2.grid(row=1,column=1)
        tk.Label(third,text="Range:").grid(row=2,column=0)
##to keep the length of text array, add a dummy variable
        texts[3]=dummy2
        '''
        tk.Label(third,text="Min:").grid(row=1,column=0)
        rangeMin2 = tk.Entry(third)
        texts[1] = rangeMin2
        rangeMin2.insert(10,"90.0, 100.0")
        rangeMin2.grid(row=1,column=1)

        tk.Label(third,text="Max:").grid(row=2,column=0)
        rangeMax2 = tk.Entry(third)
        texts[3] = rangeMax2
        rangeMax2.insert(10,"0.0")
        rangeMax2.grid(row=2,column=1)
        '''

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
    xmindefault = "0.00001"
    xmaxdefault = "1"
    range1.grid_forget()
    range1 = tk.Frame(rangesCell,relief=tk.GROOVE,bd=2)
    range1.grid(row=1,column=0,padx="5",pady="5")
    tk.Label(range1,text="Data X:").grid(row=0)
    tk.Checkbutton(range1,text="Auto",variable=xRange[0]).grid(row=1,sticky='w')
    if xRange[0].get() == 0:
        tk.Label(range1,text="X-min:").grid(row=2,column=0,sticky='w')
        rmin1 = tk.Entry(range1,width=7)
        texts[6] = rmin1
        rmin1.insert(10,xmindefault)
        rmin1.grid(row=2,column=1)
        tk.Label(range1,text="X-max:").grid(row=3,column=0,sticky='w')
        rmax1 = tk.Entry(range1,width=7)
        texts[7] = rmax1
        rmax1.insert(10,xmaxdefault)
        rmax1.grid(row=3,column=1)

def changeR2(one,two,three):
    global range2
    mumindefault = "1"
    mumaxdefault = "2000"
    range2.grid_forget()
    range2 = tk.Frame(rangesCell,relief=tk.GROOVE,bd=2)
    range2.grid(row=1,column=1,padx="5",pady="5")
    tk.Label(range2,text=u"Data \u03bc:").grid(row=0)
    tk.Checkbutton(range2,text="Auto",variable=muRange[0]).grid(row=1,sticky='w')
    if muRange[0].get() == 0:
        tk.Label(range2,text=u"\u03bc-min:").grid(row=2,column=0,sticky='w')
        rmin2 = tk.Entry(range2,width=7)
        texts[8] = rmin2
        rmin2.insert(10,mumindefault)
        rmin2.grid(row=2,column=1)
        tk.Label(range2,text=u"\u03bc-max:").grid(row=3,column=0,sticky='w')
        rmax2 = tk.Entry(range2,width=7)
        texts[9] = rmax2
        rmax2.insert(10,mumaxdefault)
        rmax2.grid(row=3,column=1)

def changeR3(one,two,three):
    global range3
    Nbindefault = "20"
    range3.grid_forget()
    range3 = tk.Frame(rangesCell,relief=tk.GROOVE,bd=2)
    range3.grid(row=1,column=2,padx="5",pady="5")
    tk.Label(range3,text="Histogram data:").grid(row=0,columnspan=3)
    tk.Checkbutton(range3,text="Auto",variable=hdRange[0]).grid(row=1,sticky='w')
    if hdRange[0].get() == 0:
        tk.Label(range3,text="Nbin:").grid(row=2,column=0,sticky='w')
        rmin3 = tk.Entry(range3,width=7)
        texts[10] = rmin3
        rmin3.insert(10,Nbindefault)
        rmin3.grid(row=2,column=1)

def changeR4(one,two,three):
    global range4
    histxmindefault = "-3"
    histxmaxdefault = "3"
    range4.grid_forget()
    range4 = tk.Frame(rangesCell,relief=tk.GROOVE,bd=2)
    range4.grid(row=1,column=3,padx="5",pady="5")
    tk.Label(range4,text="Histogram X:").grid(row=0,columnspan=3)
    tk.Checkbutton(range4,text="Auto",variable=hxRange[0]).grid(row=1,sticky='w')
    if hxRange[0].get() == 0:
        tk.Label(range4,text="X-min:").grid(row=2,column=0,sticky='w')
        rmin4 = tk.Entry(range4,width=7)
        texts[12] = rmin4
        rmin4.insert(10,histxmindefault)
        rmin4.grid(row=2,column=1)
        tk.Label(range4,text="X-max:").grid(row=3,column=0,sticky='w')
        rmax4 = tk.Entry(range4,width=7)
        texts[13] = rmax4
        rmax4.insert(10,histxmaxdefault)
        rmax4.grid(row=3,column=1)

def changeExp(one,two,three):
    global experiments
    global expids
    global selectall
    if pdfset.get() == 0:
        expidFile = open("./exptidname_inconfig.txt","r")
        experiments = [i.split() for i in expidFile.readlines()]
        expids = [int(i[0]) for i in experiments if len(i) != 0]
        experiments = [i[1] for i in experiments if len(i) != 0]
    elif pdfset.get() == 1:
        expidFile = open("./exptidname_inconfig2.txt","r")
        experiments = [i.split() for i in expidFile.readlines()]
        expids = [int(i[0]) for i in experiments if len(i) != 0]
        experiments = [i[1] for i in experiments if len(i) != 0]
    expidFile.close()
    global experimentsCell
    experimentsCell.grid_forget()
    experimentsCell = tk.Frame(table,padx="8",pady="8")
    experimentsCell.grid(row=0,column=1,columnspan=3)
    tk.Label(experimentsCell,text="Experiments to include:").grid(row=0,column=0,columnspan=2)
    experiment = [tk.IntVar() for i in range(len(experiments))]
    
    selectall = tk.IntVar()
    selectall.trace('w',selectAll)
    tk.Checkbutton(experimentsCell,text="Select all",variable=selectall).grid(row=0,column=2)

    checks[0] = experiment
    for e in range(len(experiments)):
        tk.Checkbutton(experimentsCell,text=experiments[e],variable=experiment[e]).grid(row=int(e%(len(experiments)/numcols)+1),column=int(e/(len(experiments)/numcols)))

def selectAll(one,two,three):
    if selectall.get() == 0:
        [i.set(0) for i in checks[0]]
    elif selectall.get() == 1:
        [i.set(1) for i in checks[0]]


#List labels and such
expidFile = open("./exptidname_inconfig.txt","r")

pdfsets = ["CT14NNLO","CT14HERA2-jet.ev"]
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
tk.Label(experimentsCell,text="Experiments to include:").grid(row=0,column=0,columnspan=2)
selectall = tk.IntVar()
selectall.trace('w',selectAll)
tk.Checkbutton(experimentsCell,text="Select all",variable=selectall).grid(row=0,column=2)
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
    tk.Radiobutton(first,text=figtypes[s],variable=figtype,value=s+1).grid(row=s,sticky="w")

second = tk.Frame(figuresCell)
second.grid(row=1,column=2,pady="10")
tk.Label(second,text="Highlight mode:").grid(row=0,column=0)
hlmode = tk.IntVar()
hlmode.trace('w',changeHL)
radios.append(hlmode)
for s in range(len(hlmodes)):
    tk.Radiobutton(second,text=hlmodes[s],variable=hlmode,value=s).grid(row=s+1,sticky="w")

global third
third = tk.Frame(figuresCell)
third.grid(row=1,column=3,pady="10")
#It's got multiple potential variables
pointsize = tk.IntVar()
radios.append(pointsize)
##20171120 botingw: input convention of highlight mode = {{h1min,h1max},{h2min,h2max},...}
HLrange1 = tk.Entry(third)
dummy1 = tk.Entry(third)
texts.append(HLrange1)
texts.append(dummy1)
HLrange2 = tk.Entry(third)
dummy2 = tk.Entry(third)
texts.append(HLrange2)
texts.append(dummy2)
'''
rangeMin = tk.Entry(third)
rangeMin.insert(10,"0.5, 0.1")
texts.append(rangeMin)
#rangeMax = tk.Entry(third)
#texts.append(rangeMax)
rangeMin2 = tk.Entry(third)
rangeMin2.insert(10,"90.0, 100.0")
texts.append(rangeMin2)

rangeMax2 = tk.Entry(third)
texts.append(rangeMax2)
'''

for s in range(len(pointsizes)):
    tk.Label(third,text="Size of data points:").grid(row=0,column=0)
    tk.Radiobutton(third,text=pointsizes[s],variable=pointsize,value=s).grid(row=s+1,sticky="w")

#Third row
#Functions to use
functionsCell = tk.Frame(table,padx="8",pady="8")
functionsCell.grid(row=2,columnspan=2)
tk.Label(functionsCell,text="Functions to use in correlations:").grid(row=0,column=0)
user = [tk.IntVar()]
#20171116 botingw: new way to set user define function is by writing comment in user_define_func.txt, not by the user inputs in python script
##user[0].trace('w',changeFunc)
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
#20171119 botingw: only show flavour =bbar ~ b
Nflavour=11
#for s in range(len(functions)):
for s in range(Nflavour):
    tk.Radiobutton(subCell,text=functions[s],variable=function,value=s).grid(row=int(s%(len(functions)/numcols)),column=2*int(s/(len(functions)/numcols)),columnspan=2,sticky='w')

#Fourth row
rangesCell = tk.Frame(table,width=648,padx="8",pady="8")#,relief=tk.GROOVE,bd=2)
#rangesCell.geometry("648x151")
rangesCell.grid(row=2,column=2,columnspan=2)

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
tk.Button(buttonsCell,text="EXIT",command=sys.exit).grid(row=0,column=2)

tk.mainloop()
