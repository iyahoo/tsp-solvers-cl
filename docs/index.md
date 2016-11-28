## Solving TSP with GA and ACO
- m5201146 Tomoya Furukawa
- m5201129 Masato Hashimoto



## Fix point
- I use correct cross over
- Add results with mutation rate is 1%



## Environment
- OS: Max OS X
- CPU: 3.1 GHz Intel Core i7
- RAM: 16 GB 1867 MHz DDR3



## Implementation
- Common Lisp (SBCL)
- **Impremented by us !!!!!!!!**



## Common Parameters (GA and ACO)
- Number of agents(salesman or ants): 10
- Number of graph node: 10
- Distance between nodes: [1,10]
- Number of generation: 100 or 1000



## Parameters of GA
- Representation of gene: (0 1 2 3 4 5 6 7 8 9) 
- Cross over: 1 point cross over
- truncate point: 5 (= Number of agents / 2)  
  Five agents that have good fitness survive when go next generation.
- mutation rate: 10% or 15%  
Selected points are randomly


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
- mutation rate: 15%
- End generation: 100



## Results 1
- Obtained shortest path: (0 6 5 7 8 1 3 9 2 4)
- Route length: 27
- Time: 0.020 seconds
- Memory: 5,405 kbytes
- Final generation:  
```txt
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 2 1 3 9 8 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 2 1 3 9 8 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
```



## 
<img src="graphs/ga_10_5_15_100/ga_10_5_15_100.gif" width="900" height="650" border="" align="center" hspace="10" vspace="10">



## Experiment 2 (GA)
- Cross over point: 8
- mutaito rate: 1%
- End generation: 100
- Change point (from experiment 1):  
  - mutation rate: 15% -> 1%



## Results 2
- Obtained shortest path: (0 7 5 4 8 1 3 9 2 6)
- Route length: 28
- Time: 0.012 seconds
- Memory: 5,045 kbytes
- Final generation:  
```txt
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
#<AGENT fitness: 28 route: (0 7 5 4 8 1 3 9 2 6)>
```



## 
<img src="graphs/ga_10_5_1_100/ga_10_5_1_100.gif" width="900" height="650" border="" align="center" hspace="10" vspace="10">



## Experiment 3 (GA)
- Cross over point: 5
- mutaito rate: 1%
- End generation: 1000
- Change point (from experiment 2):  
  - End generation: 100 -> 1000



## Results 3
- Obtained shortest path: (0 3 8 1 6 9 2 4 5 7)
- Route length: 22
- Time: seconds: 0.150 seconds
- Memory: 49,605 kbytes
- Final generation:  
```txt
#<AGENT fitness: 22 route: (0 3 8 1 6 9 2 4 5 7)>
#<AGENT fitness: 27 route: (0 7 5 6 8 1 3 9 2 4)>
#<AGENT fitness: 27 route: (0 7 5 6 8 1 3 9 2 4)>
#<AGENT fitness: 27 route: (0 7 5 6 8 1 3 9 2 4)>
#<AGENT fitness: 27 route: (0 7 5 6 8 1 3 9 2 4)>
#<AGENT fitness: 27 route: (0 7 5 6 8 1 3 9 2 4)>
#<AGENT fitness: 27 route: (0 7 5 6 8 1 3 9 2 4)>
#<AGENT fitness: 27 route: (0 7 5 6 8 1 3 9 2 4)>
#<AGENT fitness: 27 route: (0 7 5 6 8 1 3 9 2 4)>
#<AGENT fitness: 41 route: (0 7 4 5 8 1 6 9 2 3)>
```



## 
<img src="graphs/ga_10_5_1_1000/ga_10_5_1_1000.gif" width="900" height="650" border="" align="center" hspace="10" vspace="10">



## Experiment 4 (ACO)
- tau: 5
- alpha: 1
- beta: 10
- rho: 0.5
- Q: 100
- End generation: 100



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
- End generation: 100



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



### Comparison:
- Iteration: 100  

|         |  GA     |       ACO | 
|--------:|--------:|----------:|
| Result (shortest)  |     32  |   22       |
| Time (second)   |  0.013     |   0.026      |
| Memory (kbytes) |    5,409    |   4,849       |




|         |  GA     |       ACO | 
|--------:|--------:|----------:|
| Result  |         |   o       |
| Time    |  /      |    /      |
| Memory  |    △     |    o      |



## Discussion & Conclusion
- <strike>GA could not obtain correct shortest path.</strike>
- After modification cross over program, GA could obtain shortest path with 1000 generation
- Improtant difference between GA and ACO,
  - <strike>GA can not converge because GA have cross-over and mutation process.</strike> 
  - <strike>While ACO can converge when parameters are suitable.</strike>
  - GA and AC can converge.
- In GA, for deciding next survived generation, we have to `sort` agents. Therefore compared with ACO, GA spend many memory space.
