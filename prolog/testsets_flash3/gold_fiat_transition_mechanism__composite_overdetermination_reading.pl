% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__composite_overdetermination_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Gold-Fiat Transition Mechanism: Composite Overdetermination Reading
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story presents the 'composite overdetermination' reading
 *   of the gold-fiat transition mechanism. It argues that the shift from a
 *   gold-backed monetary system to a fiat system was not a singular event or
 *   a simple policy choice, but the result of multiple, independently
 *   evolving structural changes converging. These include advances in
 *   telecommunications enabling rapid capital flows, the inherent instability
 *   of the Bretton Woods system, shifts in labor's bargaining power, and the
 *   maturation of legal tender enforcement. The Nixon Shock of 1971 is viewed
 *   as a symbolic marker rather than the sole causal node. This reading
 *   challenges simpler, monocausal explanations of the transition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.45).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.6).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Gold-Fiat Transition Mechanism: Composite Overdetermination Reading").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, '77bfdb47-3eb2-4a61-ae99-435edcf58b94').
narrative_ontology:cs_kernel_codification('77bfdb47-3eb2-4a61-ae99-435edcf58b94', distributed).
narrative_ontology:cs_authority_grounding('77bfdb47-3eb2-4a61-ae99-435edcf58b94', distributed).
narrative_ontology:cs_reading_relation('77bfdb47-3eb2-4a61-ae99-435edcf58b94', gold_fiat_transition_mechanism__automatic_constraint_reading, influences).
narrative_ontology:cs_reading_relation('77bfdb47-3eb2-4a61-ae99-435edcf58b94', gold_fiat_transition_mechanism__creditor_discipline_reading, influences).
narrative_ontology:cs_axiom('77bfdb47-3eb2-4a61-ae99-435edcf58b94', foundational, monetary_transition_is_multicausal).
narrative_ontology:cs_axiom_status(monetary_transition_is_multicausal, holdable).
narrative_ontology:cs_axiom_grounding('77bfdb47-3eb2-4a61-ae99-435edcf58b94', monetary_transition_is_multicausal, empirically_contingent).
narrative_ontology:cs_axiom('77bfdb47-3eb2-4a61-ae99-435edcf58b94', foundational, nixon_shock_is_symbolic_not_causal).
narrative_ontology:cs_axiom_status(nixon_shock_is_symbolic_not_causal, holdable).
narrative_ontology:cs_axiom_grounding('77bfdb47-3eb2-4a61-ae99-435edcf58b94', nixon_shock_is_symbolic_not_causal, empirically_contingent).
narrative_ontology:cs_reference_frame('77bfdb47-3eb2-4a61-ae99-435edcf58b94', complex_adaptive_system_evolution).
narrative_ontology:cs_drift_state('77bfdb47-3eb2-4a61-ae99-435edcf58b94', contemporary_economic_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('77bfdb47-3eb2-4a61-ae99-435edcf58b94', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, national_governments).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_standard_advocates).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_exchange_rate_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, telecommunications_industry).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_unions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained significant discretionary power over monetary policy, no longer constrained by gold reserves. This allowed for counter-cyclical policy and inflation targeting, but also introduced new political pressures.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks, beneficiary,
    institutional, generational, mobile, national).

% Achieved greater fiscal flexibility, no longer facing the immediate discipline of gold outflows. This enabled increased public spending and deficit financing, but also removed a check on fiscal expansion.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, national_governments, beneficiary,
    institutional, biographical, mobile, national).

% Lost the foundational monetary system they believed in, seeing the transition as a loss of financial discipline and stability. Their arguments for a return to gold are largely marginalized in mainstream policy debates.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_standard_advocates, payer,
    powerless, generational, identity_locked, global).

% The collapse of Bretton Woods pegs, a key component of the gold-fiat transition, removed the framework for their operation. These regimes were replaced by floating exchange rates, leading to new challenges for international trade and finance.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_exchange_rate_regimes, payer,
    institutional, biographical, constrained, global).

% Benefited from the increased demand for infrastructure enabling instant capital flows, a key technological driver of the transition. Their innovations made the old system's constraints on capital mobility obsolete.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, telecommunications_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Experienced a shift in bargaining power dynamics as inflation became a more prominent feature of fiat systems, eroding real wages. The transition removed a structural constraint that had historically supported labor's position.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_unions, payer,
    moderate, biographical, constrained, national).

% Analyze the complex interplay of factors leading to the gold-fiat transition, seeking to understand the long-term consequences and challenge monocausal explanations. Their work informs the 'composite overdetermination' reading.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint, as a composite of multiple changes, coordinated the global financial system's shift from a gold-backed, fixed-exchange-rate regime to a fiat-based, floating-exchange-rate system, adapting to technological and political realities.
% TRANSFER_FUNCTION: Transferred monetary policy discretion from external gold reserves to national central banks, and fiscal flexibility from international creditor discipline to national governments. It also transferred economic power from those who held gold to those who controlled fiat currency issuance.
% ABSENT_VOICES: Those who advocated for a return to a gold standard or a new international monetary system based on fixed parities were largely excluded from the policy decisions that solidified the fiat regime. Their warnings about inflation and instability were not heeded by the dominant policymakers.
% DISAPPEARANCE_RATIONALE: The gold-fiat transition, as a historical event, cannot 'disappear'. Its mechanisms and consequences are now embedded in the global financial system. If the *understanding* of it as a composite overdetermination vanished, it would simply revert to other, simpler causal narratives, but the historical facts would remain.
% FOUNDING_PROBLEM: The Bretton Woods system, based on gold convertibility, faced increasing strain from growing international trade, capital flows, and the Vietnam War's fiscal demands, making the fixed exchange rate and gold peg unsustainable.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and contemporary policymakers widely agree that the specific problems of the Bretton Woods system (e.g., Triffin dilemma, balance of payments crises) are no longer live. The system itself is dead, though its legacy continues to shape monetary policy debates. Corroborated by academic consensus and historical records.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_unchanged).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).
:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while some actors (central banks, national governments) gained significant power and flexibility, the transition itself was a complex adaptation to evolving realities, not a pure rent-seeking maneuver by a single party. Suppression (0.6) reflects the active dismantling of the old system's enforcement mechanisms (e.g., gold convertibility) and the establishment of new ones (e.g., legal tender laws, central bank independence). Theater ratio is low (0.1) as the transition was a genuine, functional shift, not primarily performative. Accessibility collapse (0.7) is high because the structural changes made a return to the old system increasingly impractical. Resistance (0.3) was present from gold standard advocates but ultimately insufficient to prevent the transition.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the distributed causality and varied impacts across different actors and structural levels, contrasting with readings that attribute the transition to a single policy decision or a simple replacement of one constraint with another. The engine's classification will reflect this distributed impact, showing a 'rope' or 'tangled rope' type due to the mix of coordination and diffuse extraction, rather than a 'snare' or 'mountain' that simpler readings might imply.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and national governments are beneficiaries, gaining monetary and fiscal autonomy. The telecommunications industry also benefited from the enabling technology. Gold standard advocates and fixed exchange rate regimes are payers, losing their preferred monetary order. Labor unions experienced a shift in power dynamics that could be seen as a cost. Economic historians act as observers, analyzing the complex causality.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'composite overdetermination' reading prevents mislabeling the transition as a simple 'snare' (pure extraction) by highlighting the genuine coordination function of adapting to new economic realities. It also prevents mislabeling it as a 'mountain' (natural law) by emphasizing the human-driven structural changes. The founding problem (unsustainable Bretton Woods) is 'dead', but the constraint (the fiat system) persists, indicating a successful, albeit complex, adaptation rather than mandatrophy in the sense of a functionless constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_weight_of_factors,
    'What was the precise causal weight of each independent structural change (telecoms, Bretton Woods collapse, labor shifts, legal tender maturation) in driving the gold-fiat transition?',
    'Counterfactual historical analysis and econometric modeling attempting to isolate the impact of each factor, though definitive resolution is likely impossible due to path dependence.',
    'A clearer understanding of causal weights would refine the narrative of who benefited and who paid, potentially shifting the perceived extractiveness or coordination function of specific components of the transition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_weight_of_factors, empirical, 'Quantifying the relative importance of multiple causal factors in a complex historical transition.').

omega_variable(
    kernel_singularity_ambiguity,
    'Is the ''gold-fiat transition mechanism'' a singular, identifiable kernel, or is it a label for a collection of distinct, though related, historical processes?',
    'Conceptual analysis of historical periodization and the coherence of ''transition'' as a unified object of study. If the processes are truly independent, the kernel itself might be a misnomer.',
    'If the kernel is truly non-singular, then any single ''reading'' of it is inherently flawed, and the entire framework of analyzing it as a unified constraint would need re-evaluation. This reading itself challenges the singularity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_singularity_ambiguity, conceptual, 'Whether the ''gold-fiat transition'' is a coherent, singular historical event or a label for multiple distinct processes.').

omega_variable(
    nixon_shock_symbolism_vs_causality,
    'To what extent was the Nixon Shock a purely symbolic marker of an inevitable transition, versus a significant causal accelerant or shaper of the specific path taken?',
    'Detailed historical counterfactuals: what would have happened if Nixon had not acted as he did? Analysis of policy alternatives and their likely outcomes.',
    'If the Nixon Shock had more causal weight, the ''automatic constraint'' or ''creditor discipline'' readings might gain more traction, as it would imply a more singular, policy-driven transition. If purely symbolic, this ''composite overdetermination'' reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nixon_shock_symbolism_vs_causality, empirical, 'The causal role of the Nixon Shock in the gold-fiat transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1960, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(gold_tr_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(gold_tr_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(gold_tr_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1980, 0.1).

% Extraction over time
narrative_ontology:measurement(gold_be_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(gold_be_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement(gold_be_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement(gold_be_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1980, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1960, 0.5).
narrative_ontology:measurement(gold_su_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(gold_su_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(gold_su_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1980, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gold_fiat_transition_mechanism' kernel. It emphasizes the composite, overdetermined nature of the transition, challenging monocausal explanations. Sibling readings include 'automatic_constraint_reading' and 'creditor_discipline_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
