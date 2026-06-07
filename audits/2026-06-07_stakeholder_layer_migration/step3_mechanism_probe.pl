/* ===========================================================================
   Step-3 MECHANISM probe (OQ-83) — wiring witness, NOT the experiment.

   PRE-NAMED PREDICTIONS (written before the run):
   Contention demo (phase_a_contention_demo):
     ATOM-KEYED  institutional@(biographical,mobile,national): exactly ONE d
                 = 0.15 (prediction about EXISTING code —
                 power_role_heuristic(institutional, HasB=true, _, 0.15)
                 + mobile 0.00; if it returns something else, the prediction
                 is corrected toward the substrate, never the reverse).
     NAME-KEYED  platform_operator d=0.12 (agenda_setter + mobile 0.00)
                 publisher_consortium d=0.85 (payer + mobile 0.00)
                 independent_creators d=0.90 (payer 0.85 + trapped 0.05)
     IN_CONTENTION exactly [platform_operator-publisher_consortium]
                 (creators are powerless: different atom, no pair).
     TRACE       overlay stakeholder_role_d_payer 0.85->0.40: ONLY payer-role
                 seats move (consortium 0.40, creators 0.45); platform_operator
                 stays 0.12 — locates the split causally in the role-param
                 clause of derive_directionality_for_stakeholder.
     Per-seat types at the two institutional seats differ (corroboration only).
   Control (phase_a_noncontention_control):
     IN_CONTENTION = [] (harness can return "no split").
     standards_body d=0.25 = platform_vendors d=0.25 (identical, same type).
     ATOM-KEYED institutional d = 0.15.
   UNTESTED THIS PASS (deliberate, operator-noted): the +exit_modulation and
   clamp arms of derive_directionality_for_stakeholder beyond trapped(+0.05) —
   both demo institutional seats use mobile (0.00) to isolate role-d; the
   clamp (e.g. payer 0.85 + mod > 1.0) is not exercised here.
   =========================================================================== */

:- [stack].
:- use_module(probe_harness).
:- corpus_loader:ensure_corpus_loaded.
:- consult('testsets/.tmp_phase_a_contention_demo.pl').
:- consult('testsets/.tmp_phase_a_noncontention_control.pl').

m_run :-
    CA = phase_a_contention_demo,
    Ctx = context(agent_power(institutional), time_horizon(biographical),
                  exit_options(mobile), spatial_scope(national)),
    findall(D, constraint_indexing:derive_directionality(CA, Ctx, D), AtomDs),
    format("ATOM-KEYED ~w institutional: all-solutions d = ~q~n", [CA, AtomDs]),
    (   drl_core:dr_type(CA, Ctx, TAtom) -> true ; TAtom = no_type ),
    format("ATOM-KEYED type at institutional ctx: ~q~n", [TAtom]),
    forall(narrative_ontology:constraint_stakeholder(CA, N, R, _, _, _, _),
           ( stakeholder_seats:derive_directionality_for_stakeholder(CA, N, D2),
             stakeholder_seats:chi_for_stakeholder(CA, N, Chi2),
             stakeholder_seats:dr_type_for_stakeholder(CA, N, T2),
             format("NAME-KEYED ~w role=~w d=~q chi=~q type=~q~n",
                    [N, R, D2, Chi2, T2]) )),
    findall(N1-N2, stakeholder_seats:in_contention(CA, N1, N2), Pairs),
    format("IN_CONTENTION: ~q~n", [Pairs]),
    probe_harness:with_overlay(
        [config:param(stakeholder_role_d_payer, _)],
        [config:param(stakeholder_role_d_payer, 0.40)],
        forall(narrative_ontology:constraint_stakeholder(CA, N3, _, _, _, _, _),
               ( stakeholder_seats:derive_directionality_for_stakeholder(CA, N3, D3),
                 format("TRACE payer-param->0.40: ~w d=~q~n", [N3, D3]) ))),
    % post-restore control: original payer d back
    stakeholder_seats:derive_directionality_for_stakeholder(CA, publisher_consortium, DRest),
    format("TRACE restore: publisher_consortium d=~q~n", [DRest]),
    CB = phase_a_noncontention_control,
    findall(P1-P2, stakeholder_seats:in_contention(CB, P1, P2), CPairs),
    format("CONTROL IN_CONTENTION: ~q~n", [CPairs]),
    forall(narrative_ontology:constraint_stakeholder(CB, M, RB, _, _, _, _),
           ( stakeholder_seats:derive_directionality_for_stakeholder(CB, M, DB),
             stakeholder_seats:dr_type_for_stakeholder(CB, M, TB),
             format("CONTROL NAME-KEYED ~w role=~w d=~q type=~q~n", [M, RB, DB, TB]) )),
    findall(DC, constraint_indexing:derive_directionality(CB, Ctx, DC), CBAtomDs),
    format("CONTROL ATOM-KEYED institutional: all-solutions d = ~q~n", [CBAtomDs]).
