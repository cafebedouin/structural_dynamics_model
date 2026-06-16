% Phase B / P4 / Killer-3 — second-seat test via one-field edits of a REAL
% corpus constraint (authoring-scratch mode; throwaway process; never writes corpus).
%
% 2x2 independence design (each row is the both-direction control in one shot):
%   - vary epsilon (OBSERVER-axis input): dr_type may move (can-fire for observer
%     seat); cs_pattern must stay (won't-over-call: a non-CS input does not move
%     the CS verdict).
%   - vary authority_grounding (CS-axis input): cs_pattern may move (can-fire for
%     CS seat); dr_type must stay (won't-over-call: a non-observer input does not
%     move the observer verdict).
% Disjoint response on disjoint inputs => the two axes range over independent
% content-spaces. Coupled response => one seat under gauge rotation.

:- use_module(library(lists)).
:- use_module(probe_harness).

% Make the two overlaid fields dynamic BEFORE corpus load (run/0 loads corpus),
% so the static-procedure retract refusal does not block the CS-axis arm.
:- dynamic narrative_ontology:cs_authority_grounding/2.
:- dynamic narrative_ontology:constraint_metric/3.

obs_cs(C, Type, Pattern) :-
    constraint_indexing:default_context(DC),
    ( catch(drl_core:dr_type(C, DC, Type),_,Type=error) -> true ; Type = failed ),
    ( catch((cs_pattern_detection:cs_pattern(C, Pattern, _), !),_,Pattern=error) -> true ; Pattern = failed ).

show(Label, C) :-
    obs_cs(C, T, P),
    format('  ~w~t~28|| dr_type=~w~t~46|| cs_pattern=~w~n', [Label, T, P]).

run :-
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, 'testsets_flash')),
    corpus_loader:load_all_testsets,
    C = ai_governance_legitimacy__market_libertarian_reading,
    narrative_ontology:cs_story_uid(C, UID),
    narrative_ontology:cs_kernel_codification(UID, KC0),
    narrative_ontology:cs_authority_grounding(UID, AG0),
    narrative_ontology:constraint_metric(C, extractiveness, E0),
    format('~n=== P4 SEAT-TEST on ~w ===~n', [C]),
    format('baseline authored: eps=~w KC=~w AG=~w~n~n', [E0, KC0, AG0]),
    show('BASELINE', C),

    format('~n-- vary OBSERVER-axis input (extractiveness) --~n'),
    forall(member(EV, [0.02, 0.95]),
        probe_harness:with_overlay(
            [narrative_ontology:constraint_metric(C, extractiveness, _)],
            [narrative_ontology:constraint_metric(C, extractiveness, EV)],
            show_eps(EV, C))),

    format('~n-- vary CS-axis input (authority_grounding) --~n'),
    forall(member(AV, [lineage, extraction, practice]),
        probe_harness:with_overlay(
            [narrative_ontology:cs_authority_grounding(UID, _)],
            [narrative_ontology:cs_authority_grounding(UID, AV)],
            show_ag(AV, C))),

    format('~n-- restore witness (must equal baseline) --~n'),
    show('RESTORED', C),
    format('~n=== END SEAT-TEST ===~n').

show_eps(EV, C) :- format('  eps:=~w', [EV]), nl, show('   ->', C).
show_ag(AV, C)  :- format('  AG:=~w', [AV]),  nl, show('   ->', C).
