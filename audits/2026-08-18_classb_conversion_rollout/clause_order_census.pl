/** clause_order_census.pl — the steal-risk census over the class-B worklist.

THE QUESTION (PREREGISTRATION §3). Unit A found that `signature_grade/2`'s bound-probe hazard
is ATOM-SPECIFIC, and located the mechanism: a bound query at atom A skips the cut of any
EARLIER clause whose head binds a DIFFERENT atom B, and if that clause's body would have
succeeded, it steals the answer. That is a property of the class, not of the row. So:

    steal_risk(P, A) = |{ earlier cut-bearing clauses of P whose OUTPUT argument is an
                          atom B, B \= A }|

Zero at A means a bound query at A skips no committing clause that could have answered
differently. The conversion template (fresh-variable heads + unify-after-cut) is DESIGNED to
drive this to zero everywhere; this census records what it is WITNESSED at.

DELIBERATELY AN UPPER BOUND. Whether the skipped clause's BODY would actually have succeeded
is not statically decidable, so a nonzero count is "could steal", not "does steal". The
conservative direction is the right one here: a zero is then a real zero, and a nonzero is a
row to look at rather than a verdict.

A VARIABLE in an earlier clause's output position contributes NOTHING — it unifies with every
atom, so its cut always runs and there is no skip. That asymmetry is the whole reason
`signature_grade/2` is safe at `correction`, and it is what the census must reproduce.

DISCRIMINATION RECORD — naturally arising, no plant, checked in the same run by the Python
driver (audits/.../clause_order_census.py):
    signature_grade/2 @ correction  -> steal_risk 0    (matches: bound == once+== on 5 legs)
    signature_grade/2 @ commentary  -> steal_risk 1    (matches: bound diverges on 5 legs)
A census that cannot reproduce that split is measuring something other than what Unit A
measured, and its zeros license nothing.

REUSES dispatch_head_check:scan_file_clauses/2 — the ordered clause reader that walker already
had. No second reader of the same source (build_discipline Pattern 2).

Output protocol:
  COC_ROW: <file> <name>/<arity> atom=<A> steal_risk=<N> skipped=[<B>,...] nclauses=<M>
  COC_PRED: <file> <name>/<arity> atoms=<K> max_steal_risk=<N>
  COC_SCANNED: <n_specs>
*/

:- module(clause_order_census, [run_clause_order_census/1]).

:- use_module('../../prolog/dispatch_head_check', [scan_file_clauses/2]).
:- use_module(library(lists)).
:- use_module(library(apply)).

run_clause_order_census(SpecFile) :-
    read_specs(SpecFile, Specs),
    length(Specs, N),
    (   N =:= 0
    ->  format("COC_ERROR: 0 specs read from ~w~n", [SpecFile]), fail
    ;   true
    ),
    forall(member(File-Name/Arity, Specs), census_pred(File, Name, Arity)),
    format("COC_SCANNED: ~w~n", [N]).

read_specs(SpecFile, Specs) :-
    setup_call_cleanup(open(SpecFile, read, S), read_lines(S, Specs), close(S)).

read_lines(S, Out) :-
    read_line_to_string(S, L),
    (   L == end_of_file
    ->  Out = []
    ;   (   parse_line(L, Spec)
        ->  Out = [Spec|Rest]
        ;   Out = Rest
        ),
        read_lines(S, Rest)
    ).

parse_line(L, File-Name/Arity) :-
    split_string(L, " \t", " \t", P0),
    exclude(==(""), P0, [FS, PIS]),
    \+ sub_string(FS, 0, 1, _, "%"),
    split_string(PIS, "/", "", [NS, AS]),
    number_string(Arity, AS),
    atom_string(File, FS), atom_string(Name, NS).

% Resolve prolog/ against THIS FILE's own directory, not the cwd — the corpus_loader
% convention (2026-06-04). A cwd-relative path here made the census depend on where it was
% invoked from, which is exactly the class of silent-empty this project fails closed on.
prolog_dir(Dir) :-
    module_property(clause_order_census, file(Self)),
    file_directory_name(Self, AuditDir),
    file_directory_name(AuditDir, AuditsDir),
    file_directory_name(AuditsDir, Repo),
    atomic_list_concat([Repo, '/prolog/'], Dir).

census_pred(File, Name, Arity) :-
    prolog_dir(Dir),
    atomic_list_concat([Dir, File], Path),
    (   exists_file(Path)
    ->  true
    ;   format("COC_ERROR: no such file ~w~n", [Path]), fail
    ),
    scan_file_clauses(Path, All),
    include(is_pred(Name, Arity), All, Mine),
    length(Mine, NCl),
    findall(A-C, (member(_-clause(Args, C), Mine), last(Args, A), atom(A)), AtomClauses),
    findall(A, member(A-_, AtomClauses), Atoms0),
    sort(Atoms0, Atoms),
    length(Atoms, NAtoms),
    findall(R, ( member(A, Atoms), steal_risk(Mine, A, R, _) ), Risks),
    (   Risks == [] -> MaxR = 0 ; max_list(Risks, MaxR) ),
    forall(member(A, Atoms),
           ( steal_risk(Mine, A, R, Skipped),
             format("COC_ROW: ~w ~w/~w atom=~w steal_risk=~w skipped=~w nclauses=~w~n",
                    [File, Name, Arity, A, R, Skipped, NCl]) )),
    atomic_list_concat([Name, /, Arity], PIA),
    format("COC_PRED: ~w ~w atoms=~w max_steal_risk=~w~n", [File, PIA, NAtoms, MaxR]).

is_pred(Name, Arity, PI-_) :- PI == Name/Arity.

%% steal_risk(+OrderedClauses, +Atom, -Count, -SkippedAtoms)
%
%  FIRST VERSION OF THIS PREDICATE WAS WRONG AND THE PREREGISTERED CONTROL CAUGHT IT.
%  It treated a variable-headed clause carrying a cut as an UNCONDITIONAL commit and stopped
%  scanning there, which reported steal_risk 0 for BOTH of signature_grade/2's atoms — while
%  the five-leg measurement says `commentary` diverges. A cut is not reached unless the body
%  reaches it; a guard that fails falls through to the next clause. Recorded rather than
%  silently fixed, because a census whose zeros come from stopping too early is exactly the
%  didn't-look this arc keeps naming, and it produced a clean-looking table.
%
%  CORRECT MODEL. Head unification, not the cut, is what a bound query changes: a clause whose
%  output arg is an atom B \= A is never entered at all, so its cut never runs. A clause whose
%  output arg is a VARIABLE, or is A itself, is entered in both the bound and unbound cases.
%  So the bound and unbound answers can differ exactly when some cut-bearing clause with a
%  different atom sits BEFORE a clause that could still yield A — under the unbound query it
%  may commit first and win; under the bound query it is skipped and the later clause wins.
%
%      steal_risk(P, A) = |{ cut-bearing clauses with head atom B \= A that appear before the
%                            LAST clause of P that can yield A }|
%      "can yield A" = output arg is A, or is a variable.
%
%  The LAST such clause (not the first) is deliberate: it is the upper bound, and this census
%  is an upper bound by design.
steal_risk(Clauses, Atom, Count, Skipped) :-
    findall(I, ( nth1(I, Clauses, _-clause(Args, _)), last(Args, O),
                 ( var(O) ; O == Atom ) ), YieldIdx),
    (   YieldIdx == []
    ->  Count = 0, Skipped = []          % A is not yieldable at all: nothing to steal
    ;   max_list(YieldIdx, LastYield),
        findall(B, ( nth1(J, Clauses, _-clause(Args, true)), J < LastYield,
                     last(Args, B), atom(B), B \== Atom ), Skipped),
        length(Skipped, Count)
    ).
