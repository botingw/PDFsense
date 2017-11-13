import os,glob,sys
import Tkinter as tk

from PIL import Image, ImageTk

os.system("/usr/local/bin/math -script ./run_v4.m")

#Initialize window
'''root = tk.Tk()
root.title("LHC Plotter")
root.option_add("*Font","default 12")
root.resizable(width=False,height=False)
root.configure(background='white')

scrollbar = tk.Scrollbar(root)
scrollbar.pack(side=tk.RIGHT,fill=tk.Y)

everything = tk.Frame(root)

image = Image.open("../plots/Jobs/56763/exptname_table.png")
tmp = ImageTk.PhotoImage(image)
l = tk.Label(everything,image=tmp)
l.image = tmp
l.pack()

for f in sorted(glob.glob("../plots/Jobs/*/*.png")):
    if ("expt_error_ratio_hist2" not in f) and ("exptname_table" not in f):
        print f
        image = Image.open(f)
        tmp = ImageTk.PhotoImage(image)
        l = tk.Label(everything,image=tmp)
        l.image = tmp
        l.pack()

everything.config(yscrollcommand=scrollbar.set)
scrollbar.config(command=everything.yview)

root.mainloop()
'''

def data():
    image = Image.open("../plots/Jobs/"+sys.argv[1]+"/exptname_table.png")
    tmp = ImageTk.PhotoImage(image)
    l = tk.Label(frame,compound=tk.CENTER,image=tmp)
    l.image = tmp
    l.pack(fill=tk.X)

    ##20171112 botingw set the order of plot: xQ, histogram 1
#    collectfigs=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*.png"))
    ExptxQfig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/xQbyexpt_xQ.png"))
    xQfig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*_xQ_*.png"))
    hist1fig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*_hist1_*.png"))
    hist2fig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*_hist2_*.png"))
    legendfig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*_legend_*.png"))
    orderfigs=ExptxQfig+xQfig+hist1fig+hist2fig+legendfig
    print(orderfigs)

#20171112 botingw show figures by order, and show both histogram 1, 2
    for f in orderfigs:
#    for f in sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*.png")):
        if ("exptname_table" not in f):
#        if ("expt_error_ratio_hist2" not in f) and ("exptname_table" not in f):
            print f
            image = Image.open(f)
            #20171112 botingw: resize the images
            if ("_legend_" not in f):             
                image=image.resize((750,750), Image.ANTIALIAS)
            tmp = ImageTk.PhotoImage(image)
            l = tk.Label(frame,compound=tk.CENTER,image=tmp)
            l.image = tmp
            l.pack(fill=tk.X)

def myfunction(event):
    canvas.configure(scrollregion=canvas.bbox("all"))


#20171112 botingw change width for figures
#w = 900
#h = 800
w=900
h=800

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
