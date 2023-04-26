# TIP

*Tiny Imperative Programming Language*

This project was implemented by University dof Brasília **(UnB)** students from Science Computer Department **(CIC)** and the develpoment was based in [Static Program Analysis’ book](https://users-cs.au.dk/amoeller/spa/spa.pdf), where it is used a Tiny Imperative Programming language **(TIP)**, which is based in a context free grammar, that has a very simple syntax and implements basic ’s constructions like expressions and statements but does not high level features like global variables, objects and others. Moreover, [Principles of Program Analysis’ book](https://link.springer.com/book/10.1007/978-3-662-03811-6) was used as a helper because the first book was not totally dense.

The goal of this projects is explore the concepts for Static Analysis and implement its basic algorithms.

# Implementations

In this study the next implementations  have been done: 

- [x] Parser
- [x] Intraprodecural
    - [x] CFG
    - [x] Classic Algorithms
        - [x] Available Expression
        - [x] Live Variables
        - [x] Reaching Definition
        - [ ] Very Busy Expression
    - [x] Monotone Framework
        - [x] MFP
            - [x] Available Expression
            - [x] Live Variables
            - [x] Reaching Definition
            - [ ] Very Busy Expression
        - [ ] ~~MOP~~
- [ ] Interprocedural
    - [x] CFG
    - [x] Classic Algorithms
        - [ ] Available Expression
        - [ ] Live Variables
        - [ ] Reaching Definition
        - [ ] Very Busy Expression
    - [ ] Monotone Framework
        - [ ] MFP
            - [ ] Available Expression
            - [ ] Live Variables
            - [ ] Reaching Definition
            - [ ] Very Busy Expression
        - [ ] ~~MOP~~
    - [x] Path
        - [x] Valid 
        - [x] Completed
    - [ ] Context Insensitivity
    - [ ] Context Sensityvity
        - [ ] Basic
        - [ ] Call String
        - [ ] Functional Approach

# Version

0.5
