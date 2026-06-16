% R3 follow-up — presentation-vs-structure probe (operator pre-registered, 2026-06-16).
%
% Pre-registered question (operator): hold the constraint's structural reality fixed
% and vary only the PRESENTATION (authority_grounding) — can cs_pattern be moved
% without changing what actually binds?
%   re-presenting alone moves cs_pattern   -> tracks PRESENTATION -> ORIENTATION (one-seat holds)
%   moves only when binding-structure moves -> tracks STRUCTURE    -> CONTENT     (one-seat falls at P4)
% Operator staked prediction: CONTENT (cs_pattern tracks grounding-structure).
% Operator falsifier: if the per-type false-X checks are authored-vs-authored with no
%   independent substrate, grounding is encoded legitimation -> ORIENTATION.
%
% This probe observes BOTH cs_pattern (the classifier) AND the cs_verdict it carries,
% under (P) vary-presentation/hold-structure and (S) vary-structure/hold-presentation,
% on two grounding patterns: natural_law_constraint and interpretive_accretion.
% Authoring-scratch mode; throwaway process; with_overlay = verified restore; never writes corpus.

:- use_module(library(lists)).
:- use_module(probe_harness).

:- dynamic narrative_ontology:cs_authority_grounding/2.
:- dynamic narrative_ontology:constraint_metric/3.
:- dynamic narrative_ontology:constraint_beneficiary/2.

obs(C, Pattern, Verdict) :-
    ( catch((cs_pattern_detection:cs_pattern(C, Pattern, _), !), _, Pattern=error) -> true ; Pattern=failed ),
    ( catch((cs_pattern_detection:cs_verdict(C, Verdict), !), _, Verdict=err) -> true ; Verdict=none ).

show(Label, C) :-
    obs(C, P, V),
    format('  ~w~t~34|| cs_pattern=~w~t~70|| cs_verdict=~w~n', [Label, P, V]).

run :-
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, 'testsets_flash')),
    corpus_loader:load_all_testsets,
    nl, format('================ R3 PRESENTATION-vs-STRUCTURE PROBE ================~n'),

    % ---- ARM 1: natural_law_constraint (AG=self_enforcing, beneficiary present) ----
    C1 = ai_governance_legitimacy__market_libertarian_reading,
    narrative_ontology:cs_story_uid(C1, U1),
    format('~n## ARM 1  ~w~n', [C1]),
    show('BASELINE', C1),

    format('~n  (P) vary PRESENTATION authority_grounding, hold structure fixed:~n'),
    forall(member(AV, [lineage, extraction, practice, diffuse_epistemic]),
        probe_harness:with_overlay(
            [narrative_ontology:cs_authority_grounding(U1, _)],
            [narrative_ontology:cs_authority_grounding(U1, AV)],
            ( format('   AG:=~w', [AV]), nl, show('      ->', C1) ))),

    format('~n  (S) vary STRUCTURE (remove beneficiary), hold presentation (AG=self_enforcing) fixed:~n'),
    probe_harness:with_overlay(
        [narrative_ontology:constraint_beneficiary(C1, _)],
        [],
        ( format('   beneficiary:=ABSENT'), nl, show('      ->', C1) )),

    % ---- ARM 2: interpretive_accretion, currently carrying NO verdict ----
    ( findall(Cx, ( corpus_loader:corpus_constraint(Cx),
                    once(cs_pattern_detection:cs_pattern(Cx, interpretive_accretion, _)),
                    \+ cs_pattern_detection:cs_verdict(Cx, _) ), L2),
      L2 = [C2|_]
    -> true ; C2 = none ),
    ( C2 == none
    -> format('~n## ARM 2  (no silent interpretive_accretion constraint found)~n')
    ;  narrative_ontology:cs_story_uid(C2, U2),
       format('~n## ARM 2  ~w~n', [C2]),
       show('BASELINE', C2),
       format('~n  (S) vary STRUCTURE suppression_requirement, hold presentation (AG) fixed:~n'),
       forall(member(SV, [0.05, 0.50]),
           probe_harness:with_overlay(
               [narrative_ontology:constraint_metric(C2, suppression_requirement, _)],
               [narrative_ontology:constraint_metric(C2, suppression_requirement, SV)],
               ( format('   suppression_requirement:=~w', [SV]), nl, show('      ->', C2) ))),
       format('~n  (P) vary PRESENTATION authority_grounding, hold structure fixed:~n'),
       probe_harness:with_overlay(
           [narrative_ontology:cs_authority_grounding(U2, _)],
           [narrative_ontology:cs_authority_grounding(U2, self_enforcing)],
           ( format('   AG:=self_enforcing'), nl, show('      ->', C2) ))
    ),

    format('~n-- restore witness (must equal ARM 1 baseline) --~n'),
    show('RESTORED', C1),
    format('~n================ END PROBE ================~n').
