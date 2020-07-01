from datetime import datetime

class KT(object):
    def __init__(self, year, month, day):
        delta = datetime(year, month, day) - datetime(year, 1, 1)
        self.year = year - 1970
        self.day = delta.days

def nsv_id(year, month, day):
    delta = datetime(year, month, day) - datetime(year, 1, 1)
    return "NSV-%04d-%04d" % (year, delta.days * 24 + 1000)
