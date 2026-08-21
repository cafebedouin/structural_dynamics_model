% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods: Sovereignty Defense Reading
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint story analyzes the Bretton Woods system from the
 *   'sovereignty_defense' reading, focusing on how the system, while
 *   ostensibly preserving national monetary sovereignty, structurally imposed
 *   external discipline on non-reserve-currency states and granted
 *   'exorbitant privilege' to the United States. The gold anchor, intended as
 *   a stabilizer, became a mechanism for this asymmetric extraction,
 *   eventually acting as a snare for many nations. The claimed type is
 *   'tangled_rope' because it did offer a coordination function (exchange
 *   rate stability) but was deeply asymmetric in its costs and benefits.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.8).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.75).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.8).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods: Sovereignty Defense Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, 'f372f8be-3d6b-47bd-acd9-1b1c75972f49').
narrative_ontology:cs_kernel_codification('f372f8be-3d6b-47bd-acd9-1b1c75972f49', formalized).
narrative_ontology:cs_authority_grounding('f372f8be-3d6b-47bd-acd9-1b1c75972f49', extraction).
narrative_ontology:cs_interpretation_layer_present('f372f8be-3d6b-47bd-acd9-1b1c75972f49').
narrative_ontology:cs_reading_relation('f372f8be-3d6b-47bd-acd9-1b1c75972f49', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('f372f8be-3d6b-47bd-acd9-1b1c75972f49', bretton_woods_treaty_substrate__neoliberal_convertibility, influences).
narrative_ontology:cs_axiom('f372f8be-3d6b-47bd-acd9-1b1c75972f49', foundational, national_monetary_autonomy_paramount).
narrative_ontology:cs_axiom_status(national_monetary_autonomy_paramount, holdable).
narrative_ontology:cs_axiom_grounding('f372f8be-3d6b-47bd-acd9-1b1c75972f49', national_monetary_autonomy_paramount, deontological).
narrative_ontology:cs_axiom('f372f8be-3d6b-47bd-acd9-1b1c75972f49', foundational, reserve_currency_status_grants_asymmetric_power).
narrative_ontology:cs_axiom_status(reserve_currency_status_grants_asymmetric_power, holdable).
narrative_ontology:cs_axiom_grounding('f372f8be-3d6b-47bd-acd9-1b1c75972f49', reserve_currency_status_grants_asymmetric_power, empirically_contingent).
narrative_ontology:cs_reference_frame('f372f8be-3d6b-47bd-acd9-1b1c75972f49', national_monetary_autonomy_under_fixed_exchange).
narrative_ontology:cs_drift_state('f372f8be-3d6b-47bd-acd9-1b1c75972f49', post_nixon_shock_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('f372f8be-3d6b-47bd-acd9-1b1c75972f49', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, developing_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the issuer of the reserve currency (USD), the U.S. benefited from 'exorbitant privilege,' allowing it to run balance of payments deficits without immediate pressure to devalue, effectively exporting inflation and gaining financial flexibility. It also set many of the rules of the system.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, united_states, beneficiary).

% These nations committed to fixed exchange rates against the USD, requiring them to maintain external monetary discipline (e.g., through capital controls or domestic austerity) to defend their pegs. While gaining exchange rate stability, they sacrificed monetary policy autonomy and bore the costs of U.S. deficits.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, beneficiary).

% Often highly dependent on external finance and trade, these nations faced even greater pressure to conform to the Bretton Woods system's discipline. Their limited economic diversification and institutional capacity made the costs of fixed exchange rates and capital controls particularly burdensome, with fewer avenues for independent policy.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, developing_nations, payer,
    powerless, generational, trapped, global).

% The IMF was established to oversee the Bretton Woods system, providing short-term liquidity to countries facing balance of payments difficulties and enforcing the rules of external monetary discipline. It acted as a key enforcer of the system's constraints.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, international_monetary_fund, agenda_setter,
    institutional, generational, constrained, global).

% Initially, capital markets were heavily regulated and constrained by capital controls, limiting their ability to challenge fixed exchange rates. As the system progressed, their growing power and mobility put increasing pressure on the fixed exchange rate regime, eventually contributing to its collapse.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, global_capital_markets, excluded,
    powerful, biographical, constrained, global).

% Scholars who analyze the historical operation and consequences of the Bretton Woods system, identifying its structural biases and the distribution of costs and benefits among participating nations. They provide a critical, retrospective view of the system's true nature.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a stable international monetary system, prevent competitive devaluations, and provide a framework for international financial cooperation, thereby allowing nations to pursue domestic economic goals without external monetary shocks.
% TRANSFER_FUNCTION: Transfers seigniorage benefits and monetary policy flexibility to the United States (as the reserve currency issuer) from other participating nations, who bear the discipline of fixed exchange rates and capital controls.
% ABSENT_VOICES: Advocates for truly independent national monetary policies (unconstrained by fixed exchange rates), proponents of fully free capital mobility, and nations seeking a more equitable international monetary system not centered on a single reserve currency.
% DISAPPEARANCE_RATIONALE: The Bretton Woods system, particularly its fixed exchange rate regime, collapsed in the early 1970s. Its disappearance led to a fundamental reorganization of the international monetary system, ushering in an era of floating exchange rates and increased capital mobility, profoundly altering global finance and national economic policy.
% FOUNDING_PROBLEM: The interwar period was characterized by monetary instability, competitive devaluations, and protectionism, which contributed to economic depression and international conflict. Bretton Woods aimed to prevent a recurrence of these issues by creating a stable, rules-based international monetary order.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians widely corroborate the severity of the interwar problems. However, the specific mechanisms of Bretton Woods, particularly the fixed exchange rate and the role of the USD, are seen by many (including non-U.S. policymakers and scholars) as having outlived their initial problem-solving function, evolving into a system that disproportionately benefited the U.S. while constraining others. The system's collapse itself is strong corroboration of its unsustainability in its original form.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising because the system's design increasingly favored the U.S., allowing it to run deficits that other nations were forced to finance or absorb, leading to a 'dollar overhang.' Suppression was initially high due to strict capital controls and the IMF's enforcement powers, but it began to decline as global capital markets grew and put pressure on the fixed exchange rates. Theater ratio increased as the system became less sustainable but was maintained through political rhetoric and stop-gap measures before its eventual collapse.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. perspective, the system was a successful framework for global stability and its leadership. From the perspective of non-reserve-currency states, it was an increasingly extractive system that limited their policy space and forced them to bear the costs of U.S. economic policy. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The United States is the primary beneficiary, gaining significant seigniorage and monetary policy flexibility. Non-reserve-currency states and especially developing nations are the primary victims, bearing the costs of external discipline and sacrificing monetary autonomy. The IMF acts as an agenda-setter, enforcing the system's rules. Global capital markets, initially excluded, became a powerful force that eventually challenged the system's suppressive elements.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exorbitant_privilege_quantification,
    'What was the precise economic value of the ''exorbitant privilege'' accrued by the United States under Bretton Woods, and how did it evolve over time?',
    'Detailed econometric studies comparing U.S. balance of payments flexibility and seigniorage gains against counterfactual scenarios without reserve currency status.',
    'A higher quantified privilege would strengthen the ''snare'' aspect of the gold anchor for other nations; a lower value would suggest a more balanced, albeit still asymmetric, coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exorbitant_privilege_quantification, empirical, 'Quantifying the asymmetric benefits to the U.S. from its reserve currency status.').

omega_variable(
    gold_anchor_function_ambiguity,
    'Was the gold anchor primarily a stabilizer for global exchange rates or a structural mechanism for transferring wealth and discipline to non-reserve-currency states?',
    'Analysis of historical capital flows, balance of payments adjustments, and national policy responses, particularly during periods of U.S. deficit, to determine the primary burden-sharing mechanism.',
    'If primarily a stabilizer, the constraint leans more towards a ''tangled_rope'' with a genuine coordination function. If primarily a transfer mechanism, it leans more towards a ''snare'' for the victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_anchor_function_ambiguity, conceptual, 'Ambiguity regarding the primary function of the gold-dollar convertibility mechanism.').

omega_variable(
    reading_framing_impact,
    'To what extent does the ''sovereignty_defense'' framing accurately capture the lived experience of non-reserve-currency states, versus other framings like ''embedded liberalism''?',
    'Comparative historical analysis of policy debates and economic outcomes in various non-U.S. countries, assessing whether the ''defense of sovereignty'' was a primary concern or a secondary effect of other policy goals.',
    'If ''sovereignty_defense'' is a dominant lived experience, this reading''s classification is robust. If other framings are more salient, the classification might shift to reflect a different primary coordination problem or extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_impact, conceptual, 'The impact of the chosen reading''s framing on the constraint''s classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(bret_tr_t1958, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(bret_tr_t1965, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(bret_tr_t1970, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(bret_tr_t1973, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1973, 0.4).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.6).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement(bret_be_t1958, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1958, 0.72).
narrative_ontology:measurement(bret_be_t1965, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1965, 0.78).
narrative_ontology:measurement(bret_be_t1970, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1970, 0.8).
narrative_ontology:measurement(bret_be_t1973, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1973, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.7).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(bret_su_t1958, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1958, 0.8).
narrative_ontology:measurement(bret_su_t1965, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1965, 0.82).
narrative_ontology:measurement(bret_su_t1970, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1970, 0.78).
narrative_ontology:measurement(bret_su_t1973, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1973, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, enforcement_mechanism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Bretton Woods treaty substrate kernel. Its structural properties and classification differ significantly from other readings, such as 'keynesian_embedded_liberalism' and 'neoliberal_convertibility', due to differing interpretations of the system's primary function and beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
