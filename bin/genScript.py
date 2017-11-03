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

    for f in sorted(glob.glob("../plots/Jobs/"+sys.argv[1]+"/*.png")):
        if ("expt_error_ratio_hist2" not in f) and ("exptname_table" not in f):
            print f
            image = Image.open(f)
            tmp = ImageTk.PhotoImage(image)
            l = tk.Label(frame,compound=tk.CENTER,image=tmp)
            l.image = tmp
            l.pack(fill=tk.X)

def myfunction(event):
    canvas.configure(scrollregion=canvas.bbox("all"))

w = 900
h = 800

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
