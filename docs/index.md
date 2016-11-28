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
- truncate point: 5  
  Five agents that have good fitness survive when go next generation.
- mutation rate: 10% or 15%
    - Selected points are randomly



## Parameters of ACO
- tau: 5
- alpha: 1
- beta 10
- rho: 0.5 or 0.95
- Q: 100



## Experiment 1



## Experiment 2
- ACO 



## Gif
![](../tsp_visualizer/graphs/aco_01/aco_01.gif)
