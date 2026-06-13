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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Gold-Fiat Transition: Automatic Constraint Reading
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story analyzes the gold-fiat transition from the
 *   perspective that it primarily involved the replacement of an automatic,
 *   physically-backed monetary constraint (gold reserves) with a
 *   discretionary, institutionally-backed one (central bank authority). The
 *   shift is seen as moving from a system where money creation was limited by
 *   a material resource to one where it is limited by policy choices. The
 *   constraint type changed from a de facto Mountain (physical limit) to a
 *   Tangled Rope (institutional discretion with beneficiaries and victims).
 *
 * KEY AGENTS:
 *   - monetary_authorities: Primary beneficiary (institutional/arbitrage) — gained discretion over money supply.
 *   - sovereign_governments: Secondary beneficiary (institutional/arbitrage) — gained fiscal flexibility.
 *   - creditor_class: Primary victim (powerful/constrained) — lost automatic protection against inflation.
 *   - savers: Secondary victim (moderate/constrained) — exposed to inflation risk.
 *   - international_financial_institutions: Agenda setter (institutional/constrained) — managed the transition and new system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.85).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.75).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Gold-Fiat Transition: Automatic Constraint Reading").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '21302b55-e01f-4ac2-b7e3-9c0bf81b2641').
narrative_ontology:cs_kernel_codification('21302b55-e01f-4ac2-b7e3-9c0bf81b2641', formalized).
narrative_ontology:cs_authority_grounding('21302b55-e01f-4ac2-b7e3-9c0bf81b2641', lineage).
narrative_ontology:cs_interpretation_layer_present('21302b55-e01f-4ac2-b7e3-9c0bf81b2641').
narrative_ontology:cs_reading_relation('21302b55-e01f-4ac2-b7e3-9c0bf81b2641', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('21302b55-e01f-4ac2-b7e3-9c0bf81b2641', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('21302b55-e01f-4ac2-b7e3-9c0bf81b2641', foundational, money_creation_requires_automatic_limit).
narrative_ontology:cs_axiom_status(money_creation_requires_automatic_limit, overridden).
narrative_ontology:cs_axiom_grounding('21302b55-e01f-4ac2-b7e3-9c0bf81b2641', money_creation_requires_automatic_limit, conventional).
narrative_ontology:cs_axiom('21302b55-e01f-4ac2-b7e3-9c0bf81b2641', foundational, discretionary_authority_replaces_material_constraint).
narrative_ontology:cs_axiom_status(discretionary_authority_replaces_material_constraint, holdable).
narrative_ontology:cs_axiom_grounding('21302b55-e01f-4ac2-b7e3-9c0bf81b2641', discretionary_authority_replaces_material_constraint, conventional).
narrative_ontology:cs_reference_frame('21302b55-e01f-4ac2-b7e3-9c0bf81b2641', gold_standard_automatic_constraint).
narrative_ontology:cs_drift_state('21302b55-e01f-4ac2-b7e3-9c0bf81b2641', post_nixon_shock_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('21302b55-e01f-4ac2-b7e3-9c0bf81b2641', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_governments).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, savers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained significant discretion over money supply, allowing them to pursue macroeconomic stabilization goals without the direct constraint of gold reserves. This discretion also allows for seigniorage and other forms of implicit taxation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, beneficiary,
    institutional, generational, arbitrage, global).

% Benefited from increased fiscal flexibility, no longer constrained by the need to maintain gold convertibility. This allowed for deficit financing and counter-cyclical policies, but also removed a check on spending.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_governments, beneficiary,
    institutional, generational, arbitrage, national).

% Lost the automatic protection against inflation and currency debasement that the gold standard offered. Their claims are now subject to the discretionary policies of central banks, which can erode the real value of their assets.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, biographical, constrained, global).

% Similar to the creditor class but with less power, individual savers are exposed to inflation risk, as the value of their savings is no longer tied to a physical commodity but to the policy choices of central banks.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, savers, payer,
    moderate, biographical, constrained, national).

% Played a key role in managing the transition (e.g., Bretton Woods) and establishing the new fiat-based international monetary system. They administer the rules and norms of this system, benefiting from their central position.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, international_financial_institutions, agenda_setter,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__automatic_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible monetary system capable of responding to economic shocks and supporting fiscal policy, replacing the rigid, commodity-backed system.
% TRANSFER_FUNCTION: Transfers the power to create money and manage its value from an automatic, commodity-backed system to discretionary central bank authority, implicitly transferring wealth from creditors/savers to debtors/governments through inflation.
% ABSENT_VOICES: Advocates for a return to commodity-backed money or alternative, non-discretionary monetary systems are largely excluded from mainstream policy discourse, their arguments often dismissed as anachronistic or impractical.
% DISAPPEARANCE_RATIONALE: If the discretionary central bank authority vanished overnight, the global financial system would collapse into chaos, as there would be no mechanism for money creation, liquidity provision, or macroeconomic management. A new system would have to be rapidly constructed.
% FOUNDING_PROBLEM: The gold standard imposed rigid constraints on monetary policy, hindering governments' ability to respond to economic crises (e.g., Great Depression) and manage balance of payments, leading to deflationary pressures and unemployment.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and most mainstream economists attest that the problems of the gold standard (e.g., deflationary bias, inability to conduct counter-cyclical policy) are still relevant and that discretionary fiat money is necessary for modern economic management. Critics (e.g., Austrian school economists) contest this, arguing that fiat money introduces new, more severe problems.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).

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
 *   The extractiveness is high (0.85) because the new system allows for significant wealth transfer through inflation, benefiting those with control over money creation. Suppression (0.75) is also high, as the central bank's authority is legally enforced, and alternatives to fiat currency are suppressed. The theater ratio is low (0.1) as the central bank's actions are genuinely functional, albeit with extractive consequences. The transition is modeled as a Tangled Rope because it provides a coordination function (stable currency, fiscal flexibility) but with clear asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   Monetary authorities and sovereign governments perceive the new system as a necessary evolution, providing flexibility and stability. The creditor class and savers, however, experience it as a loss of automatic protection and a source of wealth erosion. The engine's classification will reflect this divergence, with beneficiaries experiencing a Rope-like constraint and victims a Snare-like one.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities and sovereign governments are beneficiaries (d near 0.0) as they gained significant discretion and fiscal space. The creditor class and savers are victims (d near 1.0) as they lost the automatic protection against debasement that the gold standard offered. International financial institutions, while managing the system, also benefit from its stability and their central role.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate shifted from maintaining a fixed gold-to-currency ratio to managing economic stability through discretionary monetary policy. The 'mandatrophy' here is not a decay of function, but a fundamental redefinition of the function itself, from automatic to discretionary. The classification as a Tangled Rope captures this hybrid nature, preventing it from being mislabeled as a pure Snare (ignoring the coordination function) or a pure Rope (ignoring the extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint primarily an automatic physical constraint that was replaced, or is it better understood as a composite overdetermination or a shift in creditor discipline?',
    'Historical analysis of primary sources focusing on the explicit motivations and perceived mechanisms of the transition by contemporary actors, particularly central bankers and policymakers.',
    'If this ''automatic constraint'' reading is primary, the classification as a Tangled Rope (institutional discretion replacing material limit) holds. If the ''composite overdetermination'' reading is primary, the constraint is a network of interacting forces, not a single mechanism. If ''creditor discipline'' is primary, the constraint is a Snare for creditors and a Rope for governments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''gold_fiat_transition_mechanism'' kernel, specifically the ''automatic_constraint_reading''.').

omega_variable(
    discretion_vs_automaticity_ambiguity,
    'To what extent did the gold standard truly represent an ''automatic'' constraint, versus being a managed system with discretionary elements?',
    'Detailed historical and econometric analysis of central bank interventions and policy choices under the gold standard, quantifying the degree of discretion exercised.',
    'If the gold standard was more discretionary than ''automatic'', the shift to fiat money represents a change in the *form* of discretion, not its introduction, potentially lowering the perceived extractiveness of the fiat system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_vs_automaticity_ambiguity, empirical, 'Ambiguity in the ''automaticity'' of the gold standard itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 1944, 1974).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gold_tr_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(gold_tr_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(gold_tr_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(gold_be_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(gold_be_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(gold_be_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(gold_be_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(gold_su_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(gold_su_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(gold_su_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gold_fiat_transition_mechanism' kernel. This reading focuses on the replacement of an automatic physical constraint with discretionary authority. The other readings emphasize creditor discipline and composite overdetermination, respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
