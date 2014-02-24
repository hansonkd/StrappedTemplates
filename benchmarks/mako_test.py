from mako.template import Template
import time
import timeit

template = """
% for row in rows:
    % for row in rows:
        ${ row }
    % endfor
% endfor
"""

def render():
    return Template(template).render(rows=range(0,100))

if __name__ == '__main__':   
    timer = timeit.Timer(render)
    time1 = time.time()
    timer.timeit(100)
    print (time.time() - time1) * 1000

