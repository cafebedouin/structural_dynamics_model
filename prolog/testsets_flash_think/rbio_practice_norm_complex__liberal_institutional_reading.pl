% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: Rules-Based International Order (RBIO) Practice Norms: Liberal Institutional Reading
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the liberal institutional reading of the
 *   Rules-Based International Order (RBIO) practice norm complex. From this
 *   perspective, RBIO norms are universal, consent-based, and revisable
 *   through legitimate multilateral processes. Enforcement selectivity is
 *   framed as a capacity problem, not a fundamental flaw in legitimacy.
 *   Interventions are justified when authorized by the UN Security Council or
 *   in cases of grave atrocities, and economic conditionality is seen as an
 *   acceptable contractual term. The structural delta identifies intervening
 *   states and their contractors as beneficiaries, and targeted states and
 *   their civilian populations during sanctions as victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.65).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.75).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "Rules-Based International Order (RBIO) Practice Norms: Liberal Institutional Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '3b490397-4615-414b-a6ce-79017d897934').
narrative_ontology:cs_kernel_codification('3b490397-4615-414b-a6ce-79017d897934', formalized).
narrative_ontology:cs_authority_grounding('3b490397-4615-414b-a6ce-79017d897934', lineage).
narrative_ontology:cs_interpretation_layer_present('3b490397-4615-414b-a6ce-79017d897934').
narrative_ontology:cs_reading_relation('3b490397-4615-414b-a6ce-79017d897934', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b490397-4615-414b-a6ce-79017d897934', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('3b490397-4615-414b-a6ce-79017d897934', foundational, multilateralism_is_legitimate).
narrative_ontology:cs_axiom_status(multilateralism_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('3b490397-4615-414b-a6ce-79017d897934', multilateralism_is_legitimate, conventional).
narrative_ontology:cs_axiom('3b490397-4615-414b-a6ce-79017d897934', foundational, humanitarian_intervention_is_justified).
narrative_ontology:cs_axiom_status(humanitarian_intervention_is_justified, holdable).
narrative_ontology:cs_axiom_grounding('3b490397-4615-414b-a6ce-79017d897934', humanitarian_intervention_is_justified, deontological).
narrative_ontology:cs_reference_frame('3b490397-4615-414b-a6ce-79017d897934', post_wwii_liberal_order_founding).
narrative_ontology:cs_drift_state('3b490397-4615-414b-a6ce-79017d897934', contemporary_multipolar_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('3b490397-4615-414b-a6ce-79017d897934', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_contractors).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_in_sanctioned_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over UN Security Council resolutions, which are the primary legitimate mechanism for authorizing interventions and sanctions under this reading. They shape the interpretation and application of RBIO norms.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, unsc_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% States that participate in or lead interventions and enforce sanctions, often gaining geopolitical influence, access to resources, or contracts for reconstruction/security services. They benefit from the framework's legitimization of their actions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, beneficiary,
    powerful, biographical, mobile, global).

% Private military, security, and reconstruction companies that secure contracts during interventions or in sanctioned states. They directly profit from the operationalization of RBIO norms.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_contractors, beneficiary,
    organized, biographical, arbitrage, global).

% States that are subject to sanctions, interventions, or economic conditionality. They bear the direct costs of compliance, loss of sovereignty, and economic disruption, with limited recourse within the system.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states, payer,
    powerless, generational, trapped, global).

% Populations within targeted states who suffer the humanitarian and economic consequences of sanctions and interventions, often without direct agency in the political decisions that led to their situation.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_in_sanctioned_states, payer,
    powerless, generational, trapped, global).

% States that generally adhere to RBIO norms and participate in multilateral institutions but do not actively engage in interventions or impose sanctions. They benefit from the general stability but also bear some diffuse costs of maintaining the system.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, non_intervening_states, observer,
    moderate, biographical, constrained, global).

% Operate within the framework of RBIO, often providing aid in conflict zones or sanctioned states. They witness the impacts of interventions and sanctions firsthand and advocate for humanitarian principles, but have limited power to alter the norms themselves.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_organizations, observer,
    organized, biographical, constrained, global).

% States that fundamentally reject the premise of external intervention in sovereign affairs, viewing RBIO norms as illegitimate infringements on state autonomy. They are often marginalized in multilateral forums where these norms are debated and enforced.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, sovereignty_maximalist_states, excluded,
    powerful, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__liberal_institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for collective action on global security, human rights, and economic stability, aiming to prevent unilateral aggression and address shared challenges through agreed-upon norms and institutions.
% TRANSFER_FUNCTION: Transfers sovereignty and resources from states deemed to violate norms (via sanctions, interventions, or conditionality) to the international system and, in practice, to intervening states and their contractors.
% ABSENT_VOICES: Sovereignty maximalist states and those who view RBIO norms as a tool of hegemonic power are often marginalized or excluded from the processes that define and enforce these norms, despite being subject to them.
% DISAPPEARANCE_RATIONALE: If the RBIO framework and its enforcement vanished overnight, the international system would lose its primary (albeit imperfect) mechanism for collective security and economic governance. This would likely lead to increased unilateralism, great power competition, and instability, as states would revert to self-help and power politics without a shared normative structure.
% FOUNDING_PROBLEM: To prevent a return to great power conflict, establish a rules-based international order, and address transnational challenges (e.g., human rights abuses, economic instability) that individual states cannot solve alone.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, UN officials, and many states (especially those that benefit from the current order) corroborate the ongoing relevance of the founding problems. Critics (e.g., from the hegemonic extraction reading) would argue the founding problem has been re-framed to serve specific interests, but the general need for global order is widely acknowledged.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is 'tangled_rope' because while the liberal institutional reading asserts a genuine coordination function (universal norms, multilateral processes), the structural delta explicitly identifies beneficiaries and victims, and the 'enforcement selectivity' implies asymmetric coercion. Extractiveness (0.65) is substantial due to the costs imposed on targeted states and populations, and suppression (0.75) is high given the coercive nature of sanctions and interventions. Theater ratio (0.25) is moderate, reflecting that while the norms are genuinely applied, the 'capacity problem' framing for selective enforcement may mask political choices. The temporal measurements show a general increase in extractiveness and suppression, reflecting the expanding scope of RBIO application and enforcement over the post-WWII era, with a slight dip towards 2020 as challenges to the order mounted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of intervening states and the UNSC Permanent Members, the RBIO functions as a legitimate framework for global governance, justifying necessary enforcement. From the perspective of targeted states and their populations, the same structure operates as a coercive mechanism that disproportionately extracts from them. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   UNSC Permanent Members, as agenda setters, control the formal mechanisms of enforcement. Intervening states and their contractors are direct beneficiaries, gaining influence and resources. Targeted states and their civilian populations are clear victims, bearing the costs of sanctions and interventions. Non-intervening states and humanitarian organizations act as observers, experiencing diffuse benefits and costs, or advocating within the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rbio_kernel_identity_ambiguity,
    'Is the RBIO practice norm complex a genuine reflection of universal, consent-based norms, or is this ''liberal institutional reading'' one interpretation of a contested kernel?',
    'Comparative analysis of historical application patterns across different geopolitical eras and regions, alongside a critical examination of the power dynamics inherent in norm-setting and enforcement.',
    'If it is confirmed as one reading of a contested kernel, the classification of the RBIO as a whole would require a family of constraints, each reflecting a different structural reality. This would shift the analytical focus from a single, universal RBIO to a set of competing, context-dependent RBIOs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rbio_kernel_identity_ambiguity, conceptual, 'Ambiguity regarding the fundamental nature and universality of RBIO norms.').

omega_variable(
    enforcement_selectivity_mechanism,
    'Is the observed enforcement selectivity of RBIO norms primarily a ''capacity problem'' (as claimed by this reading) or a reflection of political will and power asymmetries among states?',
    'Empirical study comparing enforcement actions against states with varying levels of geopolitical power and strategic importance, controlling for the severity of norm violations. Analysis of resource allocation for enforcement mechanisms versus political motivations for intervention/non-intervention.',
    'If selectivity is primarily due to political will, the ''capacity problem'' framing is theatrical, increasing the constraint''s effective theater_ratio and potentially reclassifying it closer to a Snare for targeted states. If genuinely a capacity issue, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_mechanism, empirical, 'Distinguishing between capacity-driven vs. politically-driven enforcement selectivity.').

omega_variable(
    economic_conditionality_coercion,
    'Is economic conditionality, as applied under RBIO, a legitimate contractual term for development assistance and market access, or does it function as a coercive tool for resource extraction and policy alignment?',
    'Case studies of states subject to economic conditionality, analyzing the long-term economic outcomes, the degree of genuine consent, and the presence of alternative development pathways. Comparison with non-conditional aid and trade agreements.',
    'If conditionality is found to be primarily coercive, the extractiveness metric would be higher, and the ''contractual term'' framing would be seen as cover, pushing the constraint further towards a Snare for recipient states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_conditionality_coercion, conceptual, 'Nature of economic conditionality under RBIO: contract or coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1950, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(rbio_tr_t1960, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(rbio_tr_t1970, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(rbio_tr_t1980, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(rbio_tr_t1990, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(rbio_tr_t2000, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(rbio_tr_t2010, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(rbio_tr_t2020, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2020, 0.25).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1950, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(rbio_be_t1960, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(rbio_be_t1970, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(rbio_be_t1980, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(rbio_be_t1990, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(rbio_be_t2000, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(rbio_be_t2010, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(rbio_be_t2020, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1950, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(rbio_su_t1960, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(rbio_su_t1970, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(rbio_su_t1980, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(rbio_su_t1990, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(rbio_su_t2000, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(rbio_su_t2010, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2010, 0.82).
narrative_ontology:measurement(rbio_su_t2020, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
