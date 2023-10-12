# CLPlearning: Interval Constraint Solving based Binary Classification

This is the implementation of a learning method for binary classification based on interval constraint solving. 
It uses fuzzy logic to map datasets to fuzzy sets with the aim to rescale and redistribute the data points in the fuzzy space.
CLP(BNR) is used for solving constraints. 
The solver finds the coefficients of a weighted sum that classifies the dataset in two classes.
As input the solver requests the dataset, the size of the dataset, the percentage for training and two intervals.
The solved form of the constraint system serves as classifier. 

# Instructions
1. Install SWI-Prolog
2. Run SWI-Prolog
3. Load the main program CLPlearning.pl.
4. Launch a classification problem as follows:
    :- E::real(0.51,1),F::real(0,0.5),clp_learning("moons100.csv",100,0.75,E,F).
5. E, F are the intervals, "moons.csv" is the name of the file with the dataset, 100 is the size and 0.75 is the percentage for training.
6. The file has to use commas as separator, and the class has to be specified in the last column (0 and 1).
7. Examples of datasets can be found in the given folder
