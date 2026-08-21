% probe_oq190_edge_admission.pl — the OQ-190 derivation-graph PRUNER.
%
% Governed by the frozen audits/2026-08-17_oq190_blast_radius/PREREGISTRATION.md §(d)
% "Edge-admission pruner". An edge cast-field -> derived-value is ADMITTED only where the cast
% value can CHANGE the derived value, not merely where it is read.
%
% WHY A PRUNER AT ALL. Unbounded transitivity plus weakest-edge grade inheritance degrades
% everything to presence grade, lands the whole repo in SUSPECT-confirmed-grade-unmeasured, and
% produces a headline that is true, unactionable, and not closable in one arc. The pruner makes
% the closure terminate on SUBSTANCE rather than on graph exhaustion.
%
% THE VERDICT LOGIC IS oq35's, PRE-COMMITTED (and its limits are pre-committed too):
%   - non-empty treatment diff                       => edge ADMITTED
%   - 0-diff, source presence > 0, control diff live => edge NOT ADMITTED
%   - 0-diff, source presence == 0                   => "source absent here", NOT a rejection
%   - 0-diff AND the positive control is ALSO 0-diff => PROBE BROKEN, abort; do not conclude
%
% This decides what the GRAPH CONTAINS, not what a dependent's DISPOSITION is. A 0-diff here is a
% SENSITIVITY result and may never move a census row to `-and-survives` (prereg §(h)).

:- module(probe_oq190_edge_admission, [run_oq190_admission/1]).

% ---------------------------------------------------------------------------
% Candidate edges, seeded from RECON §4. Each: id, SourceTemplates, Grade, ObservableName.
% ---------------------------------------------------------------------------

% obs(+Name, -Tuples) — the derived observable, as a sorted corpus-wide tuple set.
obs(dr_type, T) :-
    findall(C-Ty, ( corpus_loader:corpus_constraint(C),
                    ( catch(drl_core:dr_type(C, Ty), _, fail) -> true ; Ty = err ) ), T0),
    msort(T0, T).
obs(coalition_power, T) :-
    findall(C-P, ( corpus_loader:corpus_constraint(C),
                   ( catch(constraint_indexing:resolve_coalition_power(powerless, C, P), _, fail)
                     -> true ; P = err ) ), T0),
    msort(T0, T).
obs(fingerprint_actors, T) :-
    findall(C-A, ( corpus_loader:corpus_constraint(C),
                   ( catch(logical_fingerprint:fingerprint_actors(C, A), _, fail)
                     -> true ; A = err ) ), T0),
    msort(T0, T).
obs(constraint_captured, T) :-
    findall(C, ( corpus_loader:corpus_constraint(C),
                 catch(narrative_ontology:constraint_captured(C), _, fail) ), T0),
    msort(T0, T).
obs(has_mandatrophy, T) :-
    findall(C, ( corpus_loader:corpus_constraint(C),
                 catch(narrative_ontology:has_mandatrophy_declaration(C), _, fail) ), T0),
    msort(T0, T).
obs(q6_cell, T) :-
    findall(C-Cell, ( corpus_loader:corpus_constraint(C),
                      ( catch(stakeholder_seats:q6_crosscheck(C, Cell, _), _, fail)
                        -> true ; Cell = err ) ), T0),
    msort(T0, T).

edge(e1, [narrative_ontology:constraint_beneficiary(_,_)], presence,       dr_type).
edge(e2, [narrative_ontology:constraint_victim(_,_)],       presence,       dr_type).
edge(e3, [narrative_ontology:constraint_victim(_,_)],       cardinality,    coalition_power).
edge(e4, [narrative_ontology:constraint_beneficiary(_,_)],  presence,       constraint_captured).
edge(e7, [narrative_ontology:constraint_beneficiary(_,_),
          narrative_ontology:constraint_victim(_,_)],        cardinality,   fingerprint_actors).
edge(e6, [narrative_ontology:stakeholder_gain_flow(_,_)],   'name-identity', constraint_captured).
edge(e6b,[narrative_ontology:constraint_stakeholder(_,_,_,_,_,_,_)], 'name-identity', constraint_captured).
edge(e6c,[narrative_ontology:stakeholder_gain_flow(_,_)],   'name-identity', dr_type).
edge(e10,[narrative_ontology:founding_problem_status(_,_)], 'name-identity', has_mandatrophy).
edge(e10b,[narrative_ontology:disappearance_verdict(_,_)],  'name-identity', has_mandatrophy).
edge(e11,[narrative_ontology:founding_problem_status(_,_)], 'name-identity', q6_cell).
edge(e12,[narrative_ontology:disappearance_verdict(_,_)],   'name-identity', q6_cell).
% Verdict -> dr_type: the sweep must be able to REJECT as well as admit, and this is the natural
% candidate for a rejection (no verdict atom appears on the classification path).
edge(e13,[narrative_ontology:founding_problem_status(_,_)], 'name-identity', dr_type).
edge(e14,[narrative_ontology:disappearance_verdict(_,_)],   'name-identity', dr_type).

% POSITIVE CONTROL per observable: a source that MUST move it. Without this a 0-diff cannot be
% told from a probe that never dispatched — the whole point of the oq35 verdict logic.
control(dr_type,            [narrative_ontology:constraint_metric(_, extractiveness, _)]).
control(coalition_power,    [narrative_ontology:constraint_victim(_,_)]).
control(fingerprint_actors, [narrative_ontology:constraint_beneficiary(_,_)]).
control(constraint_captured,[narrative_ontology:stakeholder_gain_flow(_,_)]).
control(has_mandatrophy,    [narrative_ontology:founding_problem_status(_,_)]).
control(q6_cell,            [narrative_ontology:founding_problem_status(_,_)]).

presence_count(Templates, N) :-
    findall(1, ( member(M:F, Templates), catch(call(M:F), _, fail) ), L),
    length(L, N).

diff_count(Base, Pert, N) :-
    ord_subtract(Base, Pert, A), ord_subtract(Pert, Base, B),
    length(A, NA), length(B, NB), N is NA + NB.

% OQ-326 (2026-08-21): an EMPTY snapshot is an expected outcome here, and this
% probe already says so in its own verdict logic — `PresN =:= 0` routes to
% Adm='n/a', Why=source_absent_on_this_corpus (below). That is the artifact's own
% declaration that a source may be absent on a given corpus, so the templates
% carry expect_empty rather than the probe throwing on a case it handles.
oq326_expect_empty(T,
    expect_empty(retrofit('2026-08-21',
        "source may be absent on this corpus; the probe declares that case itself at run_edge/5's PresN =:= 0 -> source_absent_on_this_corpus"), T)).

run_edge(Id, Templates, Grade, ObsName, Stream) :-
    obs(ObsName, Base),
    presence_count(Templates, PresN),
    maplist(oq326_expect_empty, Templates, WTemplates),
    ( probe_harness:with_retracted(WTemplates, probe_oq190_edge_admission:obs(ObsName, Pert)) -> true ; Pert = Base ),
    diff_count(Base, Pert, D),
    control(ObsName, CtlT),
    maplist(oq326_expect_empty, CtlT, WCtlT),
    ( probe_harness:with_retracted(WCtlT, probe_oq190_edge_admission:obs(ObsName, Ctl)) -> true ; Ctl = Base ),
    diff_count(Base, Ctl, CD),
    (   D > 0
    ->  Adm = yes, Why = treatment_diff(D)
    ;   PresN =:= 0
    ->  Adm = 'n/a', Why = source_absent_on_this_corpus
    ;   CD =:= 0
    ->  Adm = broken, Why = probe_broken_control_also_zero
    ;   Adm = no, Why = zero_diff_with_live_control(CD)
    ),
    format(Stream, "~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w~n",
           [Id, Templates, ObsName, Grade, Adm, PresN, D, Why]),
    format(user_error, "  ~w  ~w -> ~w  presence=~w treat_diff=~w ctl_diff=~w  => ~w~n",
           [Id, Grade, ObsName, PresN, D, CD, Adm]).

run_oq190_admission(OutFile) :-
    setup_call_cleanup(
        open(OutFile, write, S),
        (   format(S, "edge_id\tsource_templates\tobservable\tgrade\tadmitted\tsource_presence\ttreatment_diff\tdecided_by~n", []),
            forall(edge(Id, T, G, O), run_edge(Id, T, G, O, S))
        ),
        close(S)).
