#!/usr/local/bin/python3

import sys
import os
import glob
import shutil

def make_db(filename):
    db = {}
    with open(filename) as f:
        for line in f:
            l = line.strip().split()
            if len(l) > 0 and l[0] == "`define":
                db["`" + l[1]] = l[2]
    return db

def translate(st, db):
    for s, t in db.items():
        st = st.replace(s, t)
    return st

def replace(filename, db):
    res = []
    with open(filename) as f:
        for line in f:
            tmp = translate(line, db)
            ll = line.split()
            if len(ll) > 0 and ll[0] == "`define":
                tmp = ""
            res.append(tmp)
    return res

def mk_new_filename(filename):
    l = filename.split("/")
    fn = l[-1]
    nf = fn.split(".")[0] + "_pp.sv"
    ll = l[0:-1] + [nf]
    return "/".join(ll)

def rewrite(filename, data):
    new_filename = mk_new_filename(filename)
    with open(new_filename, "w") as f:
        f.writelines(data)

def move_pp(dirname):
    new_dir = dirname + "pp/"
    os.mkdir(new_dir)
    pp_sv_files = glob.glob(dirname + "*_pp.sv")
    for f in pp_sv_files:
        shutil.move(f, new_dir)




if __name__ == '__main__':
    dirname = sys.argv[1]
    sv_files = glob.glob(dirname + "*.sv")
    db = {}
    for svf in sv_files:
        ndb = make_db(svf)
        db = {**db, **ndb}
    for svf in sv_files:
        res = replace(svf, db)
        rewrite(svf, res)
    move_pp(dirname)
    
    
