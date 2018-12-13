# CSE505 - Computing with Logic: Final Project

This project is the implementation of the First Order Learner of Default (FOLD) algorithm from the work of Farhad Shakerin, Elmer Salazar, and Gopal Gupta, _A New Algorithm to Automate Inductive Learning of Default Theories_, ICLP, Melbourne, 2017. 

The paper could be found via the link: <a href="https://arxiv.org/abs/1707.02693">Original paper</a>

## Language
SWI-Prolog 7.6.4.

## Testing

To run the code, you have to store the dataset into run.pl with the variables.
* ```B``` : a list of the background knowledge.
    Ex: [bird(X):-penguin(X), bird(a), bird(b), cat(c), penguin(d)]
    
* ```Pos``` : a list of the positive examples.
    Ex: [a, b, jet]
    
* ```Neg``` : a list of the negative examples.
    Ex: [c, d]by
    
* ```Predicates```: A set of predicates that occurs in the background knowledge.
    Ex: [bird, cat, penguin]
    
* ```Goal``` : The final goal that you want the algorithm to learn.
    Ex: fly

Second, import the files run.pl and fold.pl in SWI-Prolog, and run the main function with the command ```run.```
