## Solving TSP with GA and ACO
- m5201146 Tomoya Furukawa
- m5201129 Masato Hashimoto



## Environment
- OS: Max OS X
- CPU: 3.1 GHz Intel Core i7
- RAM: 16 GB 1867 MHz DDR3



## Implementation
- Common Lisp (SBCL)



## Common Parameters
- Number of agents(salesman or ants): 10
- Number of node: 10
- Distance between nodes: [1,10]
- Number of generation: 1000



## Parameters of GA
- Cross over point: 7 or 8
- truncate point: 5 (= Number of agents / 2)  
  Five agents that have good fitness survive when go next generation.
- mutation rate: 10% or 15%
    - Selected points are randomly



## Parameters of ACO
- tau: 5
- alpha: 1
- beta 10
- rho: 0.5 or 0.95
- Q: 100 or 10



## Experiment 0 (Brute force)
- Shortest path: (0 7 5 4 2 1 8 3 9 6)
- Cost(length): 22



## Experiment 1 (GA)
- Cross over point: 7
- mutation rate: 15%
- End generation: 100



## Results 1
- Obtained shortest path: (0 4 5 7 8 1 3 9 2 6)
- Route length: 32
- Time: 0.013 seconds
- Memory: 5,409 kbytes
- Final generation:  
```txt
#<AGENT fitness: 40 route: (0 7 5 6 8 3 4 9 2 1)>
#<AGENT fitness: 40 route: (0 8 1 2 4 7 9 6 5 3)>
#<AGENT fitness: 40 route: (0 6 1 8 3 5 7 9 4 2)>
#<AGENT fitness: 42 route: (0 7 5 6 9 2 3 4 1 8)>
#<AGENT fitness: 44 route: (0 3 8 9 4 5 7 2 6 1)>
#<AGENT fitness: 47 route: (0 1 5 2 8 3 7 4 6 9)>
#<AGENT fitness: 48 route: (0 5 1 7 9 6 4 2 8 3)>
#<AGENT fitness: 54 route: (0 9 7 4 5 1 8 3 6 2)>
#<AGENT fitness: 61 route: (0 2 5 4 3 8 6 7 9 1)>
#<AGENT fitness: 64 route: (0 4 1 3 2 5 8 6 7 9)>
```


## 
<img src="graphs/ga_10_7_15_100/ga_10_7_15_100.gif" width="900" height="650" border="" align="center" hspace="10" vspace="10">



## Experiment 2 (GA)
- Cross over point: 8
- mutaito rate: 10%
- End generation: 100
- Change point (from experiment 1):  
  - Cross over point: 7 -> 8
  - mutation rate: 15% -> 10%



## Results 2
- Obtained shortest path: (0 4 5 7 8 1 3 9 2 6)
- Route length: 32
- Time: 0.011 seconds
- Memory: 5,273 kbytes
- Final generation:  
```txt
#<AGENT fitness: 40 route: (0 2 3 9 8 6 1 4 5 7)>
#<AGENT fitness: 43 route: (0 2 4 3 7 5 6 8 1 9)>
#<AGENT fitness: 43 route: (0 4 5 7 2 8 1 9 3 6)>
#<AGENT fitness: 44 route: (0 9 5 7 1 6 2 8 3 4)>
#<AGENT fitness: 44 route: (0 9 7 8 2 5 4 3 1 6)>
#<AGENT fitness: 48 route: (0 6 2 7 1 3 5 9 8 4)>
#<AGENT fitness: 49 route: (0 8 1 4 6 3 5 2 9 7)>
#<AGENT fitness: 51 route: (0 9 5 8 7 3 4 6 2 1)>
#<AGENT fitness: 55 route: (0 6 2 9 7 4 1 3 5 8)>
#<AGENT fitness: 57 route: (0 2 3 6 1 9 7 8 4 5)>
```


## 
<img src="graphs/ga_10_8_10_100/ga_10_8_10_100.gif" width="900" height="650" border="" align="center" hspace="10" vspace="10">



## Experiment 3 (GA)
- Cross over point: 8
- mutaito rate: 10%
- End generation: 1000
- Change point (from experiment 2):  
  - End generation: 100 -> 1000



## Results 3
- Obtained shortest path: (0 7 5 6 1 8 2 4 3 9)
- Route length: 27
- Time: seconds: 0.128 seconds
- Memory: 52,246 kbytes
- Final generation:  
```txt
#<AGENT fitness: 35 route: (0 5 9 6 1 8 3 4 2 7)>
#<AGENT fitness: 37 route: (0 5 6 9 3 1 7 8 2 4)>
#<AGENT fitness: 40 route: (0 5 8 3 1 4 2 9 6 7)>
#<AGENT fitness: 40 route: (0 7 4 8 9 3 6 5 2 1)>
#<AGENT fitness: 47 route: (0 1 3 2 4 5 6 8 9 7)>
#<AGENT fitness: 47 route: (0 2 8 4 5 3 9 6 7 1)>
#<AGENT fitness: 48 route: (0 2 4 5 3 9 7 6 8 1)>
#<AGENT fitness: 55 route: (0 2 8 5 9 7 3 4 6 1)>
#<AGENT fitness: 63 route: (0 8 3 5 2 6 7 1 9 4)>
#<AGENT fitness: 65 route: (0 4 2 3 6 1 7 9 5 8)>
```



## 
<img src="graphs/ga_10_8_10_1000/ga_10_8_10_1000.gif" width="900" height="650" border="" align="center" hspace="10" vspace="10">



## Experiment 4 (ACO)
- tau: 5
- alpha: 1
- beta: 10
- rho: 0.5
- Q: 100



## Results 4
- Obtained shortest path: (0 7 5 4 2 1 8 3 9 6)
- Route length: 22
- Time: 0.026 seconds
- Memory: 4,849 kbytes
- Final generation:  
```txt
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
```



## 
<img src="graphs/aco_01/aco_01.gif" width="900" height="650" border="" align="center" hspace="10" vspace="10">




## Experiment 5 (ACO)
- tau: 5
- alpha: 1
- beta: 10
- rho: 0.95
- Q: 10



## Results 5
- Obtained shortest path: (0 7 5 4 2 1 8 3 9 6) 
- Route length: 22
- Time: 0.044 seconds
- Memory: 4,814, kbytes
- Final generation:  
```txt
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
#<ANT position: 0 route: (0 7 5 4 2 1 8 3 9 6)>
#<ANT position: 0 route: (0 7 5 4 2 1 8 3 9 6)>
#<ANT position: 0 route: (0 7 5 3 8 1 2 4 6 9)>
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
#<ANT position: 0 route: (0 7 5 3 8 1 2 4 6 9)>
#<ANT position: 0 route: (0 7 5 3 8 1 2 4 6 9)>
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
#<ANT position: 0 route: (0 7 5 3 8 1 2 4 6 9)>
#<ANT position: 0 route: (0 7 5 3 9 2 4 8 1 6)>
```



## 
<img src="graphs/aco_02/aco_02.gif" width="900" height="650" border="" align="center" hspace="10" vspace="10">



## Discussion & Conclusion
- GA could not obtain correct shortest path. In this experiment I use half of number of agents as a truncation point. This number was too small, so it was difficult that agent which have good fitness survive.
- GA can not converge because GA have cross-over and mutation process. While ACO can converge.
- In GA, for deciding next survived generation, we have to `sort` agents. Therefore compared with ACO, GA spend many memory space.
