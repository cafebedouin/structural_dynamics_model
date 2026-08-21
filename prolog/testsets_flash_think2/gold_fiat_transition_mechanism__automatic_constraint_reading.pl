% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Discretionary Central Bank Authority Post Gold Standard
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint is the 'automatic_constraint_reading' of the
 *   'gold_fiat_transition_mechanism' kernel. It focuses on the direct
 *   replacement of a physical, automatic constraint (gold reserves limiting
 *   money creation) with a discretionary, institutional one (central bank
 *   authority). The transition, marked by events like the Nixon Shock in
 *   1971, shifted the nature of monetary constraint from an external,
 *   material limit to an internal, policy-driven one. Sibling readings
 *   include the 'creditor_discipline_reading' (focusing on the loss of
 *   creditor veto power) and the 'composite_overdetermination_reading'
 *   (arguing for multiple converging causes rather than a single mechanism).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.82).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.75).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Discretionary Central Bank Authority Post Gold Standard").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '6c90a0bc-8a0b-4d8c-a5d5-b705bc94e25e').
narrative_ontology:cs_kernel_codification('6c90a0bc-8a0b-4d8c-a5d5-b705bc94e25e', formalized).
narrative_ontology:cs_authority_grounding('6c90a0bc-8a0b-4d8c-a5d5-b705bc94e25e', practice).
narrative_ontology:cs_interpretation_layer_present('6c90a0bc-8a0b-4d8c-a5d5-b705bc94e25e').
narrative_ontology:cs_reading_relation('6c90a0bc-8a0b-4d8c-a5d5-b705bc94e25e', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c90a0bc-8a0b-4d8c-a5d5-b705bc94e25e', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('6c90a0bc-8a0b-4d8c-a5d5-b705bc94e25e', foundational, monetary_policy_requires_discretion).
narrative_ontology:cs_axiom_status(monetary_policy_requires_discretion, holdable).
narrative_ontology:cs_axiom_grounding('6c90a0bc-8a0b-4d8c-a5d5-b705bc94e25e', monetary_policy_requires_discretion, instrumental).
narrative_ontology:cs_axiom('6c90a0bc-8a0b-4d8c-a5d5-b705bc94e25e', secondary, automatic_constraints_are_inefficient).
narrative_ontology:cs_axiom_status(automatic_constraints_are_inefficient, holdable).
narrative_ontology:cs_axiom_grounding('6c90a0bc-8a0b-4d8c-a5d5-b705bc94e25e', automatic_constraints_are_inefficient, empirically_contingent).
narrative_ontology:cs_reference_frame('6c90a0bc-8a0b-4d8c-a5d5-b705bc94e25e', discretionary_monetary_management).
narrative_ontology:cs_drift_state('6c90a0bc-8a0b-4d8c-a5d5-b705bc94e25e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6c90a0bc-8a0b-4d8c-a5d5-b705bc94e25e', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained full discretion over money creation and interest rate setting, allowing for flexible responses to economic conditions. They justify this authority as essential for macroeconomic stability and growth, but also benefit from the expanded policy toolkit.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Lost the automatic protection against currency debasement that the gold standard provided. Their fixed-income assets are now subject to the discretionary policies of central banks, which can lead to real wealth erosion through inflation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, generational, constrained, global).

% Gained significant fiscal flexibility, no longer constrained by the need to maintain gold reserves or balance-of-payments discipline. This allows for deficit spending and counter-cyclical policies without immediate external pressure.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_nations, beneficiary,
    institutional, generational, mobile, global).

% Subject to the inflation and deflation cycles driven by central bank policy. While benefiting from economic stability, they bear the indirect costs of monetary policy decisions, often without direct influence or recourse.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, general_public, payer,
    moderate, biographical, constrained, national).

% Argue for a return to a commodity-backed monetary system, believing it provides greater stability and prevents government overspending. Their views are largely outside mainstream policy discourse, making them structurally excluded from decision-making.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_standard_advocates, excluded,
    moderate, generational, identity_locked, global).

% Analyze the long-term economic and political consequences of the transition from gold-backed to fiat currency, providing critical perspectives on its benefits and drawbacks for different societal groups.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To manage the money supply, interest rates, and credit conditions to achieve macroeconomic stability, full employment, and moderate inflation, replacing the rigid and often destabilizing automatic adjustments of the gold standard.
% TRANSFER_FUNCTION: Transfers the power to create and manage money from an automatic, commodity-backed system to a discretionary institutional body. This enables a transfer of wealth from creditors (whose assets are vulnerable to inflation) to debtors (whose real debt burden can decrease), and from the private sector to the state (via seigniorage and fiscal flexibility).
% ABSENT_VOICES: Advocates for a return to the gold standard or other forms of automatic, non-discretionary monetary rules are largely excluded from the policy-making process. They would argue that the current system is prone to political manipulation and inherent instability, leading to wealth transfers and moral hazard.
% DISAPPEARANCE_RATIONALE: If central bank discretionary authority vanished overnight, the global financial system would face immediate collapse. Without a mechanism to manage money supply, set interest rates, or act as a lender of last resort, credit markets would freeze, currencies would become unstable, and economic activity would grind to a halt, necessitating a complete reorganization of monetary and financial governance.
% FOUNDING_PROBLEM: The gold standard imposed rigid constraints on monetary policy, preventing governments from responding flexibly to economic crises (e.g., recessions, financial panics, wars) and often leading to deflationary spirals that exacerbated downturns.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream economists, central bank officials, and most governments corroborate the problem of gold standard rigidity and its inability to provide necessary monetary flexibility. Critics (e.g., gold standard advocates) contest the efficacy and fairness of the fiat alternative, but generally acknowledge the historical limitations of the gold standard in crisis response.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.82, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is 'tangled_rope' because the discretionary central bank authority provides a genuine coordination function (managing money supply for stability) but also involves significant asymmetric extraction. Extractiveness is high (0.82) as the discretion allows for wealth transfers through inflation, impacting creditors and the general public. Suppression (0.75) is high because central bank authority is legally enforced, and alternatives to fiat currency are severely constrained. Theater ratio is low (0.10) as the central bank's actions are genuinely functional, even if their outcomes are contested. Accessibility collapse is high (0.80) because the automatic gold constraint is gone, and the new institutional constraint is pervasive and difficult to exit.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of monetary authorities and debtor nations, the discretionary system is a necessary and beneficial 'rope' for economic management. From the perspective of the creditor class and gold standard advocates, it is a 'snare' or 'tangled_rope' that enables debasement and wealth transfer. The engine's classification as 'tangled_rope' reflects this inherent duality.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities and debtor nations are clear beneficiaries, gaining flexibility and power. The creditor class and the general public are victims, bearing the costs of inflation and losing automatic protection. The 'arbitrage' exit option for monetary authorities reflects their ability to leverage their discretion for policy goals, while 'constrained' for the public and creditors reflects their limited options within the fiat system.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (flexible monetary management for stability) is still live, preventing a 'piton' classification. However, the high extractiveness and suppression indicate that while the coordination function persists, it is intertwined with significant asymmetric transfers, preventing a 'rope' classification. The 'tangled_rope' classification accurately captures this hybrid nature, where the original problem of gold standard rigidity has been 'solved' by replacing it with a system that, while functional, also enables extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automatic_vs_discretionary_efficiency,
    'Is the discretionary central bank authority inherently more efficient or stable than an automatic, commodity-backed system, or does its perceived efficiency mask underlying transfers and moral hazard?',
    'Long-term comparative economic analysis of fiat vs. commodity-backed systems across multiple historical periods and jurisdictions, controlling for other economic variables. This would require a counterfactual history.',
    'If discretionary authority is shown to be less efficient or stable in the long run, the justification for its extractiveness would weaken, potentially reclassifying it closer to a ''snare''. If demonstrably superior, its ''rope'' aspects would be emphasized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(automatic_vs_discretionary_efficiency, empirical, 'The fundamental debate over the efficiency and stability of discretionary vs. automatic monetary systems.').

omega_variable(
    creditor_discipline_impact,
    'To what extent did the gold standard''s ''automatic constraint'' function as a genuine discipline on debtor nations, and how has the removal of this discipline impacted fiscal responsibility and geopolitical power dynamics?',
    'Historical analysis of fiscal policy and balance-of-payments crises under the gold standard versus the fiat regime, focusing on the incentives and constraints faced by debtor and creditor nations. This directly addresses the ''creditor_discipline_reading'' sibling.',
    'If the gold standard''s disciplinary function was significant and beneficial, its removal would be seen as a greater loss for creditors and a greater gain for debtors, amplifying the ''tangled_rope'' aspects. If its discipline was largely ineffective or harmful, the ''rope'' aspects of the fiat system would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_discipline_impact, empirical, 'The role of the gold standard in enforcing fiscal discipline and its implications for the fiat system.').

omega_variable(
    single_mechanism_vs_composite_causation,
    'Was the transition from gold to fiat primarily driven by the direct replacement of an ''automatic constraint'' with ''discretionary authority'', or was it an overdetermined outcome of multiple converging structural changes (e.g., technology, geopolitics, labor power)?',
    'Detailed historical and econometric analysis disentangling the causal weight of various factors leading to the collapse of Bretton Woods and the rise of fiat currency. This directly addresses the ''composite_overdetermination_reading'' sibling.',
    'If the ''automatic constraint'' replacement was the dominant causal factor, this reading''s classification holds. If it was merely one of many, the ''tangled_rope'' classification might be seen as too simplistic, potentially leading to a more nuanced ''composite'' classification that acknowledges multiple interacting constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_mechanism_vs_composite_causation, conceptual, 'The causal mechanism of the gold-fiat transition: single replacement vs. composite overdetermination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 1971, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1971, 0.08).
narrative_ontology:measurement(gold_tr_t1981, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1981, 0.09).
narrative_ontology:measurement(gold_tr_t1991, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(gold_tr_t2001, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2001, 0.11).
narrative_ontology:measurement(gold_tr_t2011, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2011, 0.1).
narrative_ontology:measurement(gold_tr_t2021, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2021, 0.1).

% Extraction over time
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1971, 0.75).
narrative_ontology:measurement(gold_be_t1981, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1981, 0.8).
narrative_ontology:measurement(gold_be_t1991, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1991, 0.83).
narrative_ontology:measurement(gold_be_t2001, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2001, 0.85).
narrative_ontology:measurement(gold_be_t2011, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2011, 0.84).
narrative_ontology:measurement(gold_be_t2021, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2021, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1971, 0.68).
narrative_ontology:measurement(gold_su_t1981, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1981, 0.72).
narrative_ontology:measurement(gold_su_t1991, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1991, 0.75).
narrative_ontology:measurement(gold_su_t2001, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2001, 0.77).
narrative_ontology:measurement(gold_su_t2011, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2011, 0.78).
narrative_ontology:measurement(gold_su_t2021, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2021, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
