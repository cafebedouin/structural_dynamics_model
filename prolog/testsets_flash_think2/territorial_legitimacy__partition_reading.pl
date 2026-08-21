% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via International Partition (UN 181 Reading)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'partition reading' of territorial
 *   legitimacy in the Israeli-Palestinian conflict, grounded in UN Resolution
 *   181 (1948) and subsequent international legal frameworks. It posits the
 *   legitimacy of both Israeli and Palestinian states within recognized
 *   borders, deeming settlements beyond the 1967 lines illegitimate, and
 *   structurally supports a two-state solution. While the principle aims for
 *   coordination, its actual operation is characterized by significant
 *   extraction due to non-compliance, active enforcement of a status quo that
 *   deviates from the ideal, and performative diplomatic efforts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.78).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.85).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via International Partition (UN 181 Reading)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, 'b83fc08b-cf3d-4730-bc3c-5a1e5d060ddf').
narrative_ontology:cs_kernel_codification('b83fc08b-cf3d-4730-bc3c-5a1e5d060ddf', formalized).
narrative_ontology:cs_authority_grounding('b83fc08b-cf3d-4730-bc3c-5a1e5d060ddf', lineage).
narrative_ontology:cs_interpretation_layer_present('b83fc08b-cf3d-4730-bc3c-5a1e5d060ddf').
narrative_ontology:cs_reading_relation('b83fc08b-cf3d-4730-bc3c-5a1e5d060ddf', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b83fc08b-cf3d-4730-bc3c-5a1e5d060ddf', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('b83fc08b-cf3d-4730-bc3c-5a1e5d060ddf', foundational, international_law_supremacy).
narrative_ontology:cs_axiom_status(international_law_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b83fc08b-cf3d-4730-bc3c-5a1e5d060ddf', international_law_supremacy, conventional).
narrative_ontology:cs_axiom('b83fc08b-cf3d-4730-bc3c-5a1e5d060ddf', foundational, two_state_solution_principle).
narrative_ontology:cs_axiom_status(two_state_solution_principle, holdable).
narrative_ontology:cs_axiom_grounding('b83fc08b-cf3d-4730-bc3c-5a1e5d060ddf', two_state_solution_principle, deontological).
narrative_ontology:cs_reference_frame('b83fc08b-cf3d-4730-bc3c-5a1e5d060ddf', two_state_solution_framework).
narrative_ontology:cs_drift_state('b83fc08b-cf3d-4730-bc3c-5a1e5d060ddf', contemporary_conflict_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b83fc08b-cf3d-4730-bc3c-5a1e5d060ddf', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, state_of_palestine).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinians_in_occupied_territories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, international_community).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, state_of_palestine).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_settlers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recognized as a legitimate state by the partition framework, benefiting from international recognition and security. Also acts as an agenda-setter by influencing the implementation and interpretation of the framework, sometimes in ways that deviate from the original partition lines.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, state_of_israel, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, state_of_israel, agenda_setter).

% Recognized as a legitimate state by the partition framework, but its full sovereignty and territorial integrity are often undermined by non-compliance with the framework's principles (e.g., settlements). Bears significant costs due to ongoing occupation and displacement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, state_of_palestine, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, state_of_palestine, payer).

% The primary author and custodian of the international legal framework for partition. Continues to pass resolutions and monitor compliance, but often lacks the enforcement power to ensure full adherence to the partition principles.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, united_nations, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, united_nations, observer).

% Benefits from the theoretical stability and legal clarity offered by the partition framework. Observes and often advocates for its implementation, but its collective action is frequently fragmented and inconsistent, leading to limited enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_community, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, international_community, observer).

% Displaced by the events surrounding the 1948 partition and subsequent conflicts. Their right of return is largely denied by the practical implementation of the partition framework, making them primary victims of its unresolved aspects.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Live under military occupation and settlement expansion, which this reading deems illegitimate. Bear the direct costs of land confiscation, movement restrictions, and lack of sovereignty, despite the partition framework theoretically granting them self-determination.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinians_in_occupied_territories, payer,
    powerless, biographical, identity_locked, local).

% Benefit from land and resources beyond the 1967 lines, which this reading considers illegitimate. While they gain materially, their presence creates a significant political and legal cost for the partition framework, making them a 'payer' in terms of its long-term viability and international legitimacy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_settlers, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, israeli_settlers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__partition_reading, state_of_israel).
narrative_ontology:fixing_cost_class(territorial_legitimacy__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a legal and internationally recognized framework for the division of territory and the creation of two independent states, Israel and Palestine, thereby resolving competing national claims and preventing perpetual conflict.
% TRANSFER_FUNCTION: Transfers legal recognition, territorial rights, and sovereignty to two distinct national entities. In practice, the non-compliance with the framework's principles (e.g., settlement expansion) results in the transfer of land and resources from Palestinians to Israeli control.
% ABSENT_VOICES: Indigenous populations whose claims predate the partition, or those who advocate for a single, binational state, are largely excluded from the dominant discourse that centers on the two-state solution derived from partition. Their perspectives would challenge the foundational premises of the partition framework.
% DISAPPEARANCE_RATIONALE: If the international legal framework for partition vanished overnight, the entire basis for state legitimacy and territorial claims in the region would collapse. This would lead to intensified, unregulated conflict over land and sovereignty, and a complete re-evaluation of international law's role in resolving such disputes.
% FOUNDING_PROBLEM: Competing national claims to the same territory, leading to escalating conflict and instability, requiring an internationally recognized legal framework for division and the establishment of independent states.
% FOUNDING_PROBLEM_CORROBORATION: International legal bodies, the United Nations, and a significant portion of both Israeli and Palestinian civil society (those supporting a two-state solution) corroborate the founding problem and its ongoing relevance, citing persistent conflict and the lack of a final status agreement. Independent historians and political scientists also attest to the historical context and the enduring nature of the problem.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Tangled Rope because it possesses a genuine coordination function (establishing two states) but also exhibits substantial asymmetric extraction (from Palestinians due to occupation and settlements) and requires active enforcement to maintain its contested boundaries. Extractiveness is high (0.78) due to the ongoing appropriation of land and resources beyond the internationally recognized borders. Suppression is very high (0.85) as military and legal mechanisms are actively employed to maintain control and suppress resistance to the status quo. Theater ratio is moderate (0.45) as numerous diplomatic initiatives and UN resolutions have been issued without leading to full implementation, becoming more performative over time. The increasing values for extractiveness, suppression, and theater ratio over the interval reflect the growing gap between the partition's ideal and its contested reality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the partition's proponents (e.g., the UN, many international states), the framework is a legitimate, coordinative solution. However, from the perspective of Palestinians in occupied territories and refugees, the same framework, as implemented, is a source of ongoing extraction and suppression. The engine's computation of per-seat classifications will highlight this divergence, showing a coordinative aspect for some and an extractive one for others.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel is a primary beneficiary, gaining international recognition and security from the framework, though it also bears costs in terms of international pressure for non-compliance. The State of Palestine is a beneficiary in principle (recognized statehood) but a significant payer in practice due to the non-implementation of its territorial rights. Palestinian refugees and those in occupied territories are clear targets/victims, bearing the direct costs of displacement and occupation. The UN and international community are agenda-setters and observers, benefiting from the theoretical stability the framework offers, but struggling with its enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_vs_principle,
    'Is the measured extraction inherent to the partition principle itself, or a consequence of its non-compliance and asymmetric enforcement?',
    'Analysis of counterfactual scenarios where the partition framework was fully and symmetrically implemented, comparing outcomes to the current reality. This would require detailed historical and political modeling.',
    'If extraction is primarily due to non-compliance, the partition principle itself could be re-evaluated as a more coordinative (Rope) constraint, with the current state being a degraded (Piton) or captured (Snare) version of that ideal. If inherent, the principle itself is more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_vs_principle, conceptual, 'Distinguishing between the ideal of partition and its real-world, contested implementation.').

omega_variable(
    two_state_solution_viability,
    'Is the two-state solution, as envisioned by the partition reading, still empirically viable given current demographic, territorial, and political realities?',
    'Empirical assessment by independent geopolitical analysts and demographers on the feasibility of establishing two contiguous, sovereign states, considering settlement expansion, population distribution, and political will.',
    'If deemed unviable, the ''partition_reading'' would face severe challenges to its foundational axioms, potentially leading to a reclassification towards a Piton (atrophied function) or a conceptual shift towards alternative solutions (e.g., one-state, confederation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_state_solution_viability, empirical, 'The empirical possibility of achieving the partition reading''s core outcome.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (e.g., military occupation, legal barriers) or internalized (e.g., learned helplessness, identity fusion with the conflict)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanisms are removed (e.g., if occupation ends but resistance to self-determination remains low), reclassify as partially internalized. This would require a significant shift in the political landscape.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making resolution more complex than merely removing external barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of occupation and conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__partition_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__partition_reading, theater_ratio, 1993, 0.3).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__partition_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy__partition_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__partition_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__partition_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.65).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__partition_reading, base_extractiveness, 1993, 0.7).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__partition_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy__partition_reading, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__partition_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__partition_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__partition_reading, suppression_requirement, 1993, 0.8).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__partition_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy__partition_reading, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__partition_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_legitimacy' kernel. Each reading offers a distinct basis for legitimacy and generates a different constraint story, linked here to form a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
