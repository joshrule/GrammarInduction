README.txt
Simulation 


git hash: d38dc95

Summary: created data using script sparsity.pl and test.pl.
>>  run(1, 99, 1, [10, 100, 1000, 10000, 100000, 1000000, 10000000], 0, 0.1, LPs)

Data: 
      - sw_a_* contains saved alpha parameters (before pruning) for each learning run
      - sw_p_* contains saved p parameters for each learning run (should not change).
      - count_list_prob.csv: contains probability of counting correctly up to each number for each data size 
