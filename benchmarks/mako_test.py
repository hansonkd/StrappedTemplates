from mako.template import Template
import time
import timeit

template_string = """
% for row in rows:
    % for row in rows:
        ${ row }
    % endfor
% endfor
"""
template = Template(template_string)

def make_render(i):
    def render():
        return template.render(rows=range(0,i))
    return render

if __name__ == '__main__':   
    for i in [100,200,300,400,500,600]:
        timer = timeit.Timer(make_render(i))
        time1 = time.time()
        timer.timeit(100)
        print i, "mean:", (time.time() - time1)/100.0, "seconds"

