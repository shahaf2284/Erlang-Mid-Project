# Mid Project BDD
Erlang_Project
This is the mid semester project in Concurrent and Distributed Programming course. In this project we use sequential erlang. The input is a boolean function, and the program has 2 functions:

   * Convert the boolean function to BDD tree
   * Recieve values for the variables of the tree and return a result

In this assignment, you are asked to implement an automatic construction machine of a Binary Decision
Diagram (BDD) to represent a Boolean function. The API should allow the user getting a BDD
representation of a function within a single call to your machine and select the data structure it uses.
BDD is a tree data structure that represents a Boolean function. The search for a Boolean result of an
assignment of a Boolean function is performed in stages, one stage for every Boolean variable, where the
next step of every stage depends on the value of the Boolean variable that’s represented by this stage.
A BDD tree is called reduced if the following two rules have been applied to it:
1. Merge any isomorphic (identical) sub-graphs (bonus)
2. Eliminate any node whose two children are isomorphic
The construction of BDD is based on Shannon expansion theory:

    𝑓(𝑥!, 𝑥",…, 𝑥#) = 𝑥! ∙ 𝑓(1, 𝑥", 𝑥$,…, 𝑥#) + 𝑥++!+ ∙ 𝑓(0, 𝑥", 𝑥$,…, 𝑥#)
