# CSE505-Final-Project

This project is the implementation of the First Order Learner of Default (FOLD) algorithm from the work of Farhad Shakerin, Elmer Salazar, and Gopal Gupta, _A New Algorithm to Automate Inductive Learning of Default Theories_, ICLP, Melbourne, 2017. 

The paper could be found via the link: <a href="https://arxiv.org/abs/1707.02693">Original paper</a>

## Language
SWI-Prolog 7.6.4.

## Testing

To run the code, you have to store the dataset into run.pl with the variables.
* ```B``` : a list of the background knowledge.
    ex: [bird(X):-penguin(X), bird(a), bird(b), cat(c), penguin(d)]
    
* ```Pos``` : a list of the positive examples.
    ex: [a, b, jet]
    
* ```Neg``` : a list of the negative examples.
    ex: [c, d]
    
* ```Predicates```: A set of predicates that occurs in the background knowledge.
    ex: [bird, cat, penguin]
    
* ```Goal``` : The final goal that you want the algorithm to learn.
    ex: fly
