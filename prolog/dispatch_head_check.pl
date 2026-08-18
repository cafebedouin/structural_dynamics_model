/** dispatch_head_check.pl — definition-site walker for the bound-dispatch shape.

The shape it flags (build_discipline -> Pattern 7, bound-probe-bypasses-clause-order,
seen from the DEFINITION side): a predicate with >= 2 clauses carrying an ATOM in the
head's OUTPUT argument, committing with cuts. A bound call to such a predicate skips
earlier clauses by head unification, so their cuts never run and the call answers
"satisfies this clause body in isolation", not "the engine assigns".

OUTPUT ARGUMENT is taken to be the LAST argument, by engine convention. This is a
declared assumption, not a fact about every predicate: a predicate whose output is not
last escapes this walker (recorded limitation; the Phase-1 hand-table diff is the check
on it). The immune idiom — fresh-variable heads + unify-after-cut, dr_type/3 — has at
most ONE atom-headed clause at the output position (a terminal fact), so ">= 2" is the
discriminator between the hazard shape and the idiom.

Definition-site, deliberately: a call-site regex (bound_selector_check.py) cannot see
callers of an unconverted predicate it has no registry entry for, nor contract-level
bound selectors like isomorphism_engine:cluster_by_signature. This walker is keyed to
where the invariant lives; it stops firing on a predicate once its heads are converted.

READS SOURCE, NEVER LOADS IT: read_term over each file (module load runs initializers
and would make the checker a consumer of the thing it checks). `:- op/3` directives are
honoured as encountered; per-term syntax errors are counted and reported per file
(DHC_READERR), never swallowed — an unreadable term is not a clean term.

Output protocol (consumed by python/dispatch_head_check.py):
  DHC_HIT: <file> <name>/<arity> pos=<P> atom_clauses=<N> total_clauses=<M> cut_clauses=<K> atoms=[..]
  DHC_READERR: <file> <n_syntax_errors>
  DHC_SCANNED: <n_files>

Run: swipl -q -l prolog/dispatch_head_check.pl -g "run_dispatch_head_check, halt" -t "halt(1)"
Scan scope: *.pl in this file's own directory (prolog/), non-recursive — engine modules.
testsets/ (data packs) and tests/ (suites, not engine dispatch) are out of scope by the
non-recursive glob; a second directory can be passed to run_dispatch_head_check/1.
*/

:- module(dispatch_head_check, [run_dispatch_head_check/0, run_dispatch_head_check/1,
                                scan_file/2]).

run_dispatch_head_check :-
    module_property(dispatch_head_check, file(Self)),
    file_directory_name(Self, Dir),
    run_dispatch_head_check(Dir).

run_dispatch_head_check(Dir) :-
    atom_concat(Dir, '/*.pl', Pattern),
    expand_file_name(Pattern, Files0),
    sort(Files0, Files),
    length(Files, NFiles),
    (   NFiles =:= 0
    ->  format("DHC_ERROR: 0 files matched ~w~n", [Pattern]), fail  % Pattern 5: empty scan is broken, not clean
    ;   true
    ),
    forall(member(F, Files), scan_and_report(F)),
    format("DHC_SCANNED: ~w~n", [NFiles]).

scan_and_report(File) :-
    scan_file(File, result(Hits, NErr)),
    file_base_name(File, Base),
    forall(member(H, Hits), report_hit(Base, H)),
    (   NErr > 0
    ->  format("DHC_READERR: ~w ~w~n", [Base, NErr])
    ;   true
    ).

report_hit(Base, hit(Name/Arity, Pos, NAtomClauses, NTotal, NCut, Atoms)) :-
    format("DHC_HIT: ~w ~w/~w pos=~w atom_clauses=~w total_clauses=~w cut_clauses=~w atoms=~w~n",
           [Base, Name, Arity, Pos, NAtomClauses, NTotal, NCut, Atoms]).

%% scan_file(+File, -result(Hits, NSyntaxErrors))
%  Hits = list of hit(Name/Arity, Pos, NAtomClauses, NTotalClauses, NCutClauses, Atoms).
scan_file(File, result(Hits, NErr)) :-
    setup_call_cleanup(
        open(File, read, S),
        read_all_terms(S, Terms, 0, NErr),
        close(S)),
    collect_clauses(Terms, Clauses),          % Clauses = list of Name/Arity-clause(Args, HasCut)
    findall(Hit, shape_hit(Clauses, Hit), Hits).

read_all_terms(S, Terms, EAcc, NErr) :-
    catch(read_term(S, T, [module(dhc_readctx)]), _Err, (T = '$dhc_syntax_error')),
    (   T == end_of_file
    ->  Terms = [], NErr = EAcc
    ;   T == '$dhc_syntax_error'
    ->  EAcc1 is EAcc + 1,
        read_all_terms(S, Terms, EAcc1, NErr)
    ;   honour_op_directive(T),
        Terms = [T|Rest],
        read_all_terms(S, Rest, EAcc, NErr)
    ).

% Make in-file operator declarations visible to subsequent read_term calls,
% in the read context module so nothing leaks into user.
honour_op_directive((:- op(P, T, N))) :- !,
    catch(op(P, T, dhc_readctx:N), _, true).
honour_op_directive(_).

%% collect_clauses(+Terms, -Grouped)
%  Grouped: assoc-free simple list [PI-clause(Args, HasCut), ...] in reading order.
collect_clauses(Terms, Clauses) :-
    findall(PI-clause(Args, HasCut),
            ( member(T, Terms),
              term_clause(T, Head, Body),
              strip_module_head(Head, H),
              compound(H),                      % atoms-as-facts (arity 0) can't carry args
              \+ H = (_,_),
              functor(H, Name, Arity),
              \+ non_predicate_functor(Name),
              H =.. [Name|Args],
              PI = Name/Arity,
              ( body_has_cut(Body) -> HasCut = true ; HasCut = false )
            ),
            Clauses).

term_clause((Head :- Body), Head, Body) :- !.
term_clause((:- _), _, _) :- !, fail.           % directive
term_clause((?- _), _, _) :- !, fail.
term_clause((_ --> _), _, _) :- !, fail.        % DCG — not a dispatch predicate surface
term_clause(Head, Head, true).                  % fact

strip_module_head(_:H0, H) :- !, strip_module_head(H0, H).
strip_module_head(H, H).

non_predicate_functor((:-)).
non_predicate_functor((-->)).

body_has_cut(B) :- \+ \+ sub_term_cut(B).

sub_term_cut(!) :- !.
sub_term_cut(B) :-
    compound(B),
    \+ blocked_cut_scope(B),
    arg(_, B, A),
    sub_term_cut(A), !.

% A cut inside \+/1 or findall-family etc. is still "a cut in the body" for shape
% purposes — do not over-engineer transparency; the criterion needs "commits with
% cuts", and any textual ! in the body satisfies it. No scopes blocked.
blocked_cut_scope(_) :- fail.

%% shape_hit(+Clauses, -Hit)
%  For each predicate: last-arg position; >= 2 clauses with an atom there; and at
%  least one of the predicate's clauses cuts. Number constants are NOT atoms here
%  (atom/1), matching the documented shape; a numeric-constant variant would need
%  its own adjudication before widening.
shape_hit(Clauses, hit(Name/Arity, Arity, NAtom, NTotal, NCut, AtomsSorted)) :-
    setof(PI, C^member(PI-C, Clauses), PIs),
    member(Name/Arity, PIs),
    Arity >= 1,
    findall(cl(Args, HasCut), member(Name/Arity-clause(Args, HasCut), Clauses), Cs),
    length(Cs, NTotal),
    findall(A, ( member(cl(Args, _), Cs), last(Args, A), atom(A) ), AtomList),
    length(AtomList, NAtom),
    NAtom >= 2,
    findall(x, member(cl(_, true), Cs), Xs),
    length(Xs, NCut),
    NCut >= 1,
    sort(AtomList, AtomsSorted).
