/* module provides heuristic function for 'taquin' puzzle
 * uses:
 * - heuristic/1: for defining heuristic choice
 * provides:
 * - h/2: heuristic function of choice
 */
:- dynamic heuristic/1.

% provides manhattan/2 predicate
% manhattan(S, D):
% calculates manhattan distance D
% from state S to goal state
:- consult('util/manhattan.pl').

% provides hamming/2 predicate
% hamming(S, D):
% calculates hamming distance D
% from state S to goal state
:- consult('util/hamming.pl').

% h/3: alias for calling heuristic functions
% h(S, H, D):
% D is the output of heuristic function H at state S
h(State, manhattan, H) :- manhattan(State, H).
h(State, hamming, H) :- hamming(State, H).
h(State, nilsson, H) :-
    manhattan(State, Manh),
    hamming(State, Ham),
    H is Manh + 3 * Ham.

% h/2 alias for calling heuristic function of choice
h(State, H) :- heuristic(Choice), !, h(State, Choice, H), !.