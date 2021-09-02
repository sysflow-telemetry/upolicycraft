# Use non-interactive backend, to enable operation on headless machines
import matplotlib.pyplot as plt

log = {"times": [], "cpu": [], "mem_real": []}

with open("activity.log", "r") as f:
    for line in f.readlines():
        parts = line.split()
        time = float(parts[0])
        cpu = float(parts[1]) 
        mem_real = float(parts[2])
        if time < 14.358 or time > 45.0:
            continue
         
        log["times"].append(time)
        log["cpu"].append(cpu)
        log["mem_real"].append(mem_real)

# Elapsed time   CPU (%)     Real (MB)   Virtual (MB) 
# 14.358 - 45.706

with plt.rc_context({'backend': 'Agg'}):

  fig = plt.figure()
  ax = fig.add_subplot(1, 1, 1)

  ax.plot(log['times'], log['cpu'], '-', lw=1, color='r')

  plt.title("MIDS Resource Utilization While Stress Testing nullhttpd")
  ax.set_ylabel('CPU (%)', color='r')
  ax.set_xlabel('time (s)')
  ax.set_ylim(0., max(log['cpu']) * 1.2)

  ax2 = ax.twinx()

  ax2.plot(log['times'], log['mem_real'], '-', lw=1, color='b')
  ax2.set_ylim(0., max(log['mem_real']) * 1.2)

  ax2.set_ylabel('Real Memory (MB)', color='b')

  ax.grid()

  fig.savefig("niceplot.eps")
