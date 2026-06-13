% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__overdetermined_collapse_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Bretton Woods Collapse as Overdetermined Structural Inevitability
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Bretton Woods fixed-exchange-rate system was the postwar architecture
 *   for global trade and finance. Under this reading (the
 *   overdetermined-collapse reading), the system's collapse in 1971–1973 was
 *   not a consequence of policy errors or contingent events, but rather the
 *   inevitable outcome of multiple reinforcing contradictions that no policy
 *   could reconcile. The Triffin Dilemma states that a currency cannot
 *   simultaneously serve as both a store of value (with fixed exchange rate
 *   to gold) and a medium of exchange for growing global commerce (requiring
 *   credit expansion). The US faced a trilemma: maintain the gold peg,
 *   sustain global liquidity, and maintain domestic full employment. The
 *   three goals were incompatible. Gold losses accelerated, dollar confidence
 *   eroded, and the fixed rate became untenable—not because of Nixon's
 *   decision (contingency), but because the underlying math had become
 *   impossible. This reading vindicates structural-contradiction theory and
 *   treats the Triffin Dilemma as a genuine natural law of monetary systems,
 *   not a constructed constraint.
 *
 * KEY AGENTS:
 *   - US Treasury/Federal Reserve: administrator of the peg; faces incompatible policy mandates
 *   - Allied central banks: holders of depreciating reserves; trapped between supporting the system and protecting their economies
 *   - Fixed-rate dependent economies: exporters losing competitiveness due to imported inflation
 *   - Gold speculators and revaluationists: beneficiaries of the visible collapse as peg breaks
 *   - Heterodox economists/Triffin analysts: observers documenting the multiple contradictions converging
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.82).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.71).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Bretton Woods Collapse as Overdetermined Structural Inevitability").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "economic/political").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, '07eb9840-3772-449a-8d6b-ec85361a5558').
narrative_ontology:cs_kernel_codification('07eb9840-3772-449a-8d6b-ec85361a5558', formalized).
narrative_ontology:cs_authority_grounding('07eb9840-3772-449a-8d6b-ec85361a5558', distributed).
narrative_ontology:cs_reading_relation('07eb9840-3772-449a-8d6b-ec85361a5558', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('07eb9840-3772-449a-8d6b-ec85361a5558', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('07eb9840-3772-449a-8d6b-ec85361a5558', foundational, triffin_dilemma_irreconcilable).
narrative_ontology:cs_axiom_status(triffin_dilemma_irreconcilable, holdable).
narrative_ontology:cs_axiom_grounding('07eb9840-3772-449a-8d6b-ec85361a5558', triffin_dilemma_irreconcilable, empirically_contingent).
narrative_ontology:cs_axiom('07eb9840-3772-449a-8d6b-ec85361a5558', foundational, multiple_pathways_same_outcome).
narrative_ontology:cs_axiom_status(multiple_pathways_same_outcome, holdable).
narrative_ontology:cs_axiom_grounding('07eb9840-3772-449a-8d6b-ec85361a5558', multiple_pathways_same_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('07eb9840-3772-449a-8d6b-ec85361a5558', functional_bretton_woods_system).
narrative_ontology:cs_drift_state('07eb9840-3772-449a-8d6b-ec85361a5558', late_bretton_woods_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('07eb9840-3772-449a-8d6b-ec85361a5558', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, us_dollar_hegemony_beneficiaries).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, currency_speculators).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, floating_rate_arbitrageurs).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, ExtMetricName, E),
    domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the system distributes costs asymmetrically: the US exports inflation while collecting seigniorage; dependent economies absorb inflation and lose policy autonomy; speculators profit from the visible collapse. Suppression is moderate-high (0.71) because the system persists despite known dysfunction—central banks continue defending the peg through capital controls, gold hoarding, and coordination, even as they recognize its unsustainability. Theater ratio is low-moderate (0.28) because the coordination function (stable exchange rates) is real, but by the end the constraint's main activity is theater—maintaining the peg through increasingly desperate measures after the underlying function is already dead. Accessibility_collapse is very high (0.89) because once the Triffin Dilemma is understood, no alternative exists that maintains all three policy goals simultaneously; the system is logically trapped. Resistance is low (0.34) because this reading denies agents could have resisted meaningfully—the collapse was not contingent on resistance, but on mathematical inevitability. The measurement series track rising extractiveness (rent asymmetries sharpening), rising suppression requirement (more policy coordination needed to defend a weakening structure), and rising theater ratio (more performative activity as the real function hollows out). Interval t=0 is the early Bretton Woods (1944–1950s, functional coordination); t=35 is 1971–1973 collapse.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (US Treasury) and the constrained-payer seats (allies, dependent economies) should compute different constraint types: the US seat would compute this as Tangled Rope or Rope (coordination with side effects it administers), while the dependent economies compute it as Snare or Tangled Rope (extraction with coordination cover). The overdetermined reading's claim is that BOTH are correct descriptions of a constraint that was mathematically trapped—the disagreement is not resolvable by policy, only by abandoning the system. This is the seat-divergence that the overdetermined reading predicts and the contingent reading denies.
 *
 * DIRECTIONALITY LOGIC:
 *   The US benefits from seigniorage (d near beneficiary end for the US Treasury seat), but faces an impossible policy mandate (not purely beneficiary — d moderates toward 0.4–0.5 for the policy-trapped US). Allied central banks face symmetric costs and benefits: they get coordination benefits and exchange stability, but pay through reserve depreciation and policy autonomy loss (d at ~0.5 symmetric). Fixed-rate dependent economies are pure targets (d at 0.85+) — they lose policy autonomy and absorb exported inflation with no offsetting benefit. Speculators are pure beneficiaries (d near 0.0) — the visible collapse is their profit opportunity. The heterodox-economist observer seat has no directionality (analytical, d = N/A).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII currency instability and beggar-thy-neighbor policy) was LIVE in 1944–1950 and SOLVED by 1960. By 1968–1971, the founding problem was demonstrably dead (global trade had flourished, capital markets had deepened, no risk of return to 1930s-style competitive devaluations) but the system persisted. This is mandatrophy: a system whose mandate has expired but whose institutional machinery continues. The question is whether this mandatrophy itself was structurally inevitable (the overdetermined reading claims yes—breaking the system required catastrophic event because the sunk coordination benefits and institutional path-dependence made incremental reform impossible) or contingent (the contingent reading claims no—different policy choices could have transitioned gracefully). The measurement series show theater_ratio rising (more activity devoted to defending the system against its own dysfunction rather than solving current coordination problems), which is consistent with both mandatrophy and the high accessibility_collapse of this overdetermined reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_inevitability_vs_contingent_agency,
    'Was the Bretton Woods collapse inevitable due to convergent structural contradictions (Triffin Dilemma, incompatible policy mandates, gold scarcity), or contingent on policy decisions and trigger events that alternative leadership could have averted?',
    'Counterfactual analysis: if the US had chosen contractionary policy or allowed the gold price to rise, or if Germany and Japan had not run surpluses, could the peg have held? Comparison with alternative regime-stabilization attempts in similar trapped-parity situations (currency crises that were halted vs. collapsed).',
    'If structural/inevitable, the constraint is a Mountain with vindicated propositions (Triffin Dilemma is real) and high accessibility_collapse (no meaningful alternatives once the contradictions set in). If contingent, the reading shifts toward Tangled Rope (extraction masked by coordination narrative) or Snare (deliberately maintained despite known dysfunction). This is the kernel contest that splits the three readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_inevitability_vs_contingent_agency, conceptual, 'Whether overdetermined structural logic or contingent policy choice drove the collapse.').

omega_variable(
    mountain_vs_false_summit,
    'Is the Triffin Dilemma a genuine mathematical/logical contradiction (mountain), or is it a constructed constraint that benefited the US and was defended because the US benefited from it (false summit)?',
    'Historical analysis of US policy choices: did the US Treasury fight to preserve the peg despite contradictions, or did it engineer the transition to exploit its monopoly on dollar creation? Declassified policy documents show the debate; the answer determines whether the inevitability was mathematical or institutional.',
    'A genuine mountain reading claims the contradictions were irreconcilable; a false-summit reading claims the US deliberately let the peg collapse because floating rates suited its seigniorage interests. The beneficiaries array (US dollar hegemony beneficiaries, speculators) on this mountain story is itself a false-summit flag and requires omegas to document the ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_false_summit, empirical, 'Whether the constraint is natural law or constructed institutional choice.').

omega_variable(
    victim_structure_ambiguity,
    'Who bears the constraint — are the victims the fixed-rate dependent economies (who lost policy autonomy and absorbed imported inflation), or are they everyone locked into the system (US included, forced into impossible policy mandates)?',
    'Measure post-1973 outcomes: did floating rates improve policy autonomy and reduce volatility for previously-pegged economies (supporting the ''fixed-rate dependents as victims'' reading) or did they face new instability (supporting the ''system imposed constraints on all'' reading)?',
    'If the victims are externals (dependent economies), the overdetermined reading stands as a mountain that happened to collapse to the benefit of external parties. If the victims include the US (forced into impossible trade-offs), the reading becomes more symmetric — a constraint that trapped all parties but was ultimately broken by the mathematics of the situation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_structure_ambiguity, empirical, 'Whether the constraint imposed equal or asymmetric victimization.').

omega_variable(
    reading_distinction_empirical_markers,
    'What specific empirical patterns distinguish this overdetermined-collapse reading from the contingent-choice and hybrid-trigger readings?',
    'Test: (1) Did the rate of gold losses accelerate monotonically from 1950 onward (supporting overdetermined), or did they plateau and then spike around specific trigger events (supporting hybrid/contingent)? (2) Did policy-maker statements focus on ''impossible trilemma'' language (overdetermined) or ''preventable if we had chosen differently'' language (contingent)? (3) Did the system exhibit cascading-failure signatures (overdetermined) or event-driven-collapse signatures (contingent/hybrid)?',
    'This omega documents the empirical markers that distinguish this reading''s causal claim from its siblings. The reading_relations in cs_structure depend on whether these patterns show foreclosure (this reading rules out contingency) or coexistence (all three readings remain defensible from different analytical frames).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_distinction_empirical_markers, empirical, 'Empirical signatures distinguishing overdetermined-collapse logic from contingent or hybrid causality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t0, transition_causality__overdetermined_collapse_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(tran_tr_t5, transition_causality__overdetermined_collapse_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(tran_tr_t10, transition_causality__overdetermined_collapse_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(tran_tr_t15, transition_causality__overdetermined_collapse_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(tran_tr_t20, transition_causality__overdetermined_collapse_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(tran_tr_t25, transition_causality__overdetermined_collapse_reading, theater_ratio, 25, 0.25).
narrative_ontology:measurement(tran_tr_t30, transition_causality__overdetermined_collapse_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(tran_tr_t35, transition_causality__overdetermined_collapse_reading, theater_ratio, 35, 0.28).

% Extraction over time
narrative_ontology:measurement(tran_be_t0, transition_causality__overdetermined_collapse_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(tran_be_t5, transition_causality__overdetermined_collapse_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(tran_be_t10, transition_causality__overdetermined_collapse_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(tran_be_t15, transition_causality__overdetermined_collapse_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement(tran_be_t20, transition_causality__overdetermined_collapse_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(tran_be_t25, transition_causality__overdetermined_collapse_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement(tran_be_t30, transition_causality__overdetermined_collapse_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(tran_be_t35, transition_causality__overdetermined_collapse_reading, base_extractiveness, 35, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t0, transition_causality__overdetermined_collapse_reading, suppression_requirement, 0, 0.41).
narrative_ontology:measurement(tran_su_t5, transition_causality__overdetermined_collapse_reading, suppression_requirement, 5, 0.47).
narrative_ontology:measurement(tran_su_t10, transition_causality__overdetermined_collapse_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(tran_su_t15, transition_causality__overdetermined_collapse_reading, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(tran_su_t20, transition_causality__overdetermined_collapse_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(tran_su_t25, transition_causality__overdetermined_collapse_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(tran_su_t30, transition_causality__overdetermined_collapse_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(tran_su_t35, transition_causality__overdetermined_collapse_reading, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(transition_causality__overdetermined_collapse_reading, 0.22).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__hybrid_trigger_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, triffin_dilemma_mountain).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, dollar_seigniorage_system).

% DUAL FORMULATION NOTE:
% This constraint is part of the transition_causality kernel family. The kernel is the Bretton Woods transition event (1971–1973). Three readings decompose this single historical event into three distinct structural claims: (1) overdetermined_collapse_reading (this story) — inevitable due to convergent contradictions; (2) contingent_choice_reading — avoidable via different policy choices; (3) hybrid_trigger_reading — contradictions accumulated but required contingent triggers. Each reading has its own ε (extractiveness), beneficiary/victim structure, and classification. They are NOT different observers of one constraint, but different constraints arising from different causal readings of the same kernel. Network links all three and the upstream Triffin Dilemma mountain constraint they depend on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__overdetermined_collapse_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
