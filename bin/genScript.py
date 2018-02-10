import os,glob,sys

try:
    import Tkinter as tk
except:
    print("Tkinter module not found")
    import sys
    sys.exit()

from PIL import Image, ImageTk

if not os.path.exists("../plots/Jobs/"+sys.argv[1]):
    os.system("/usr/local/bin/math -script ./run_v4.m")
    #os.system("cat config1.txt")

s1 = 450
s2 = 400

def data():
    image = Image.open("../plots/Jobs/"+sys.argv[1]+"/exptname_table.png")
    image = image.resize((s1,s1*image.size[1]/image.size[0]),Image.ANTIALIAS)
    tmp = ImageTk.PhotoImage(image)
    l = tk.Label(frame,compound=tk.CENTER,image=tmp)
    l.image = tmp
    l.grid(row=0,column=1,columnspan=2)

    ExptxQfig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/xQbyexpt_xQ.png"))
    xQfig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*_xQ[+|-]1_*.png"))
    hist1fig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*_hist[+|-]1_*.png"))
    hist2fig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*_hist[+|-]2_*.png"))
    legendfig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*_legend.png"))
    orderfigs=hist1fig+hist2fig#+legendfig

    if len(xQfig) > 0:
        image = Image.open(xQfig[0])
        image = image.resize((s1,s1),Image.ANTIALIAS)
        tmp = ImageTk.PhotoImage(image)
        l = tk.Label(frame,compound=tk.CENTER,image=tmp)
        l.image = tmp
        l.grid(row=0,column=0)

    g = 0
    f = 0
    for ff in range(len(ExptxQfig)):
        f = g+ff
        if "exptname_table" not in ExptxQfig[ff]:
            image = Image.open(ExptxQfig[ff])
            image = image.resize((s2,s2),Image.ANTIALIAS)
            tmp = ImageTk.PhotoImage(image)
            l = tk.Label(frame,compound=tk.CENTER,image=tmp)
            l.image = tmp
            l.grid(row=(f/3)+1,column=f%3)
    g = f

    for ff in range(1,len(xQfig)):
        f = g+ff
        if "exptname_table" not in xQfig[ff]:
            image = Image.open(xQfig[ff])
            image = image.resize((s2,s2),Image.ANTIALIAS)
            tmp = ImageTk.PhotoImage(image)
            l = tk.Label(frame,compound=tk.CENTER,image=tmp)
            l.image = tmp
            l.grid(row=(f/3)+1,column=f%3)
    g = f+1

    for ff in range(len(orderfigs)):
        f = g+ff
        if "exptname_table" not in orderfigs[ff]:
            image = Image.open(orderfigs[ff])
            image = image.resize((s2,s2),Image.ANTIALIAS)
            tmp = ImageTk.PhotoImage(image)
            l = tk.Label(frame,compound=tk.CENTER,image=tmp)
            l.image = tmp
            l.grid(row=(f/3)+1,column=f%3)
    g = f

    l2 = tk.Label(frame,compound=tk.CENTER,text="All figures can be found at %s"%(os.getcwd().replace("bin","plots/Jobs/"+sys.argv[1])))
    l2.grid(row=(f/3)+2,column=0,columnspan=3)

def myfunction(event):
    canvas.configure(scrollregion=canvas.bbox("all"))

w = 1250
h = 600

root = tk.Tk()
root.title("LHC Plotter")
root.option_add("*Font","default 12")
root.resizable(width=False,height=False)
root.geometry('%dx%d'%(w+15,h))
#root.configure(background='white')
#root.tk_setPalette(background='white')

myframe=tk.Frame(root)
myframe.place(x=0,y=0)

canvas=tk.Canvas(myframe)
frame=tk.Frame(canvas)
myscrollbar=tk.Scrollbar(myframe,orient="vertical",command=canvas.yview)
canvas.configure(yscrollcommand=myscrollbar.set,width=w,height=h)

myscrollbar.pack(side="right",fill="y")
canvas.pack(side="left")
canvas.create_window((0,0),window=frame)
frame.bind("<Configure>",myfunction)
data()
root.mainloop()
