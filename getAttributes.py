from netCDF4 import Dataset

def getAttributes (fname, textVar):
    netCDFfile = Dataset (fname, 'r')
    atts = dir(netCDFfile.variables[textVar])
    attr = []
    val = []
    for att in atts:
        if isinstance(att, unicode):
            v = getattr (netCDFfile.variables[textVar], att)
            attr.append(att)
            val.append (v)
            print att, ': ', v
    return (attr)
