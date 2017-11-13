import os,glob,sys

try:
    import Tkinter as tk
except:
    print("Tkinter module not found")
    import sys
    sys.exit()

from PIL import Image, ImageTk

os.system("/usr/local/bin/math -script ./run_v4.m")

def data():
    image = Image.open("../plots/Jobs/"+sys.argv[1]+"/exptname_table.png")
    tmp = ImageTk.PhotoImage(image)
    l = tk.Label(frame,compound=tk.CENTER,image=tmp)
    l.image = tmp
    l.pack(fill=tk.X)

    ExptxQfig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/xQbyexpt_xQ.png"))
    xQfig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*_xQ_*.png"))
    hist1fig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*_hist1_*.png"))
    hist2fig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*_hist2_*.png"))
    legendfig=sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*_legend_*.png"))
    orderfigs=ExptxQfig+xQfig+hist1fig+hist2fig#+legendfig

    for f in orderfigs:
        if "exptname_table" not in f:
            image = Image.open(f)
            image = image.resize((600,600),Image.ANTIALIAS)
            tmp = ImageTk.PhotoImage(image)
            l = tk.Label(frame,compound=tk.CENTER,image=tmp)
            l.image = tmp
            l.pack(fill=tk.X)

def myfunction(event):
    canvas.configure(scrollregion=canvas.bbox("all"))

w = 600
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
