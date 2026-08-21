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
 *   human_readable: International Legal Partition and State Recognition (UN Res. 181 Reading)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'partition reading' of territorial
 *   legitimacy in the Israeli-Palestinian conflict, grounded in international
 *   legal frameworks like UN Resolution 181 (1947) and subsequent resolutions
 *   affirming the 1967 borders. It posits that both Israeli and Palestinian
 *   states derive legitimacy from international recognition within defined
 *   borders, and that settlements beyond the 1967 lines are illegitimate. The
 *   ideal of this reading is a two-state solution. However, the authored
 *   metrics reflect the reality of its implementation, which has been
 *   characterized by significant extraction and suppression, despite the
 *   claimed 'rope' (coordination) function. The divergence between the
 *   claimed type and the operational metrics is a key measurement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.75).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.85).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "International Legal Partition and State Recognition (UN Res. 181 Reading)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, '3bc1e402-8374-4aed-ae73-52b639f9e5f9').
narrative_ontology:cs_kernel_codification('3bc1e402-8374-4aed-ae73-52b639f9e5f9', formalized).
narrative_ontology:cs_authority_grounding('3bc1e402-8374-4aed-ae73-52b639f9e5f9', lineage).
narrative_ontology:cs_interpretation_layer_present('3bc1e402-8374-4aed-ae73-52b639f9e5f9').
narrative_ontology:cs_reading_relation('3bc1e402-8374-4aed-ae73-52b639f9e5f9', territorial_legitimacy__security_necessity_reading, influences).
narrative_ontology:cs_reading_relation('3bc1e402-8374-4aed-ae73-52b639f9e5f9', territorial_legitimacy__indigenous_continuity_reading, influences).
narrative_ontology:cs_axiom('3bc1e402-8374-4aed-ae73-52b639f9e5f9', foundational, state_sovereignty_via_recognition).
narrative_ontology:cs_axiom_status(state_sovereignty_via_recognition, holdable).
narrative_ontology:cs_axiom_grounding('3bc1e402-8374-4aed-ae73-52b639f9e5f9', state_sovereignty_via_recognition, conventional).
narrative_ontology:cs_axiom('3bc1e402-8374-4aed-ae73-52b639f9e5f9', foundational, territorial_integrity_via_borders).
narrative_ontology:cs_axiom_status(territorial_integrity_via_borders, holdable).
narrative_ontology:cs_axiom_grounding('3bc1e402-8374-4aed-ae73-52b639f9e5f9', territorial_integrity_via_borders, conventional).
narrative_ontology:cs_reference_frame('3bc1e402-8374-4aed-ae73-52b639f9e5f9', post_mandate_international_order).
narrative_ontology:cs_drift_state('3bc1e402-8374-4aed-ae73-52b639f9e5f9', contemporary_conflict_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3bc1e402-8374-4aed-ae73-52b639f9e5f9', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, united_nations).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, international_community).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinians_in_occupied_territories).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_settlers_beyond_1967).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, palestinian_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective of states and international organizations that established and nominally upholds the partition framework, benefiting from the perceived stability and legal order it provides. They set the legal agenda but face challenges in enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_community, agenda_setter,
    institutional, generational, analytical, global).

% Benefited from international recognition of its statehood under the partition plan. While formally committed to the two-state solution, its actions on the ground (e.g., settlement expansion) often contradict the partition's principles, making it an agenda-setter in practice.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, state_of_israel, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, state_of_israel, agenda_setter).

% Recognized as the representative of the Palestinian people and a potential future state under the partition framework, but bears significant costs due to limited sovereignty, ongoing occupation, and the erosion of the partition's territorial basis. Benefits from the principle of statehood recognition.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_authority, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, palestinian_authority, beneficiary).

% Bear the direct costs of the failed implementation of the partition, including military occupation, displacement, and denial of self-determination. Their claims to statehood and defined territory, as envisioned by partition, remain largely unrealized.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinians_in_occupied_territories, payer,
    powerless, immediate, trapped, local).

% Their presence and expansion beyond the 1967 lines are considered illegitimate under the partition framework, directly undermining the territorial basis for a Palestinian state. While they benefit from de facto control, their actions incur significant legitimacy costs to the partition reading itself.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_settlers_beyond_1967, payer,
    moderate, biographical, constrained, local).

% The international body that formally adopted UN Resolution 181, establishing the partition plan. It continues to advocate for a two-state solution based on international law, acting as a key institutional agenda-setter for this framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, united_nations, agenda_setter,
    institutional, generational, analytical, global).

% Monitor and report on human rights violations and the impact of the conflict on civilian populations, often highlighting the gap between the partition's ideals and the realities on the ground. They provide critical analytical perspectives.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, human_rights_organizations, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__partition_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for two independent states (Arab and Jewish) in the former Mandate for Palestine, resolving competing territorial claims through international legal recognition and defined borders, thereby preventing endless conflict.
% TRANSFER_FUNCTION: Transfers territorial sovereignty and political legitimacy within defined borders, aiming to allocate land and resources. In practice, due to enforcement asymmetries and ongoing conflict, it has facilitated the de facto transfer of land and resources to one party at the expense of another.
% ABSENT_VOICES: Those advocating for a single, secular state across the entire territory, or those rejecting the legitimacy of international partition altogether, are largely excluded from the dominant discourse that centers on the two-state solution.
% DISAPPEARANCE_RATIONALE: If the international legal framework for partition and state recognition vanished overnight, the entire basis for a two-state solution would collapse. This would lead to intensified, legally unconstrained conflict over land and sovereignty, and a complete re-evaluation of state legitimacy by all parties, fundamentally reorganizing the political landscape of the region.
% FOUNDING_PROBLEM: To resolve competing national claims to the Mandate for Palestine by proposing a two-state solution and preventing endless conflict following the end of the British Mandate.
% FOUNDING_PROBLEM_CORROBORATION: The international community (UN, many states) attests that the founding problem of resolving competing claims and achieving peace is still live. However, Palestinian groups and some Israeli groups contest the viability or fairness of the partition framework itself, citing its failure to deliver a viable Palestinian state and the ongoing conflict; legislative-hearing testimony and independent analyses from outside the benefiting parties support the view that the framework is no longer fit for purpose or has been fundamentally undermined.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.75) and suppression (0.85) reflect the ongoing reality of military occupation, settlement expansion, and denial of self-determination, which contradict the equitable division envisioned by the partition. The 'rope' claim represents the ideal coordination function of establishing two states, but the metrics show how this ideal has been undermined in practice. Theater ratio (0.45) indicates that while international bodies continue to affirm the two-state solution, effective enforcement of its principles is often lacking, leading to a gap between rhetoric and reality. Resistance is high (0.9) due to the ongoing conflict and contestation of the framework's implementation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the international community and the ideal of international law, the partition framework is a legitimate and necessary coordination mechanism. However, from the perspective of Palestinians in occupied territories, the same framework has been a vehicle for ongoing dispossession and suppression, failing to deliver its promised benefits. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and the international community are structural beneficiaries of the recognition and stability offered by the partition framework. The Palestinian Authority, while a beneficiary of the principle of statehood, is a payer due to the severe limitations on its sovereignty and territorial control. Palestinians in occupied territories are clear payers/victims, bearing the direct costs of the framework's failed implementation. Israeli settlers beyond 1967, while benefiting from de facto expansion, are considered 'payers' in terms of the legitimacy costs they impose on the partition framework itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of the partition framework—to establish two viable states and achieve peace—is still relevant and widely affirmed by the international community. However, the *effectiveness* of the framework in achieving this mandate is highly contested. The high extractiveness and suppression suggest that the mechanism has either atrophied in its coordination function or was always more extractive than claimed, leading to a situation where the founding problem remains 'contested' despite the persistence of the framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_viability_ambiguity,
    'Given the extensive settlement expansion and fragmentation of Palestinian territories, is the two-state solution, as envisioned by the partition framework, still physically and politically viable?',
    'Empirical assessment of contiguous territory, demographic trends, and political will for land swaps and border adjustments. Resolution would require a comprehensive, internationally brokered peace process.',
    'If deemed no longer viable, the partition reading''s core premise would be fundamentally challenged, potentially leading to a reclassification towards a more extractive or inertial type, or a shift towards alternative one-state or confederation solutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partition_viability_ambiguity, empirical, 'The practical viability of the two-state solution under current conditions.').

omega_variable(
    enforcement_asymmetry_ambiguity,
    'Is the international community''s enforcement of the partition principles (e.g., illegitimacy of settlements, right to self-determination) genuinely neutral, or is it subject to political biases and power asymmetries?',
    'Comparative analysis of international enforcement mechanisms in other territorial disputes, and a detailed examination of the political and economic leverage applied (or not applied) to parties violating partition principles.',
    'If significant bias is demonstrated, the ''rope'' claim would be further undermined, pushing the classification towards ''tangled_rope'' or ''snare'' due to the selective application of international law, amplifying effective extraction for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_ambiguity, conceptual, 'The neutrality and effectiveness of international enforcement of partition principles.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is international legal recognition (as per the partition reading) the primary and sufficient source of territorial legitimacy, or do other sources (e.g., indigenous rights, security necessity) hold equal or greater weight?',
    'A conceptual re-evaluation of the hierarchy of international legal principles and historical claims, potentially through international legal arbitration or a shift in global normative consensus.',
    'If other sources of legitimacy are prioritized, the partition reading''s foundational axioms would be challenged, potentially leading to a re-framing of the entire conflict and alternative solutions that do not center on the 1948/1967 partition framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'The relative weight of international legal recognition versus other sources of territorial legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__partition_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy__partition_reading, theater_ratio, 1987, 0.38).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__partition_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy__partition_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(terr_tr_t2023, territorial_legitimacy__partition_reading, theater_ratio, 2023, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__partition_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.65).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy__partition_reading, base_extractiveness, 1987, 0.7).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__partition_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy__partition_reading, base_extractiveness, 2010, 0.74).
narrative_ontology:measurement(terr_be_t2023, territorial_legitimacy__partition_reading, base_extractiveness, 2023, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__partition_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy__partition_reading, suppression_requirement, 1987, 0.8).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__partition_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy__partition_reading, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(terr_su_t2023, territorial_legitimacy__partition_reading, suppression_requirement, 2023, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, israeli_settlement_expansion).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_legitimacy' kernel. It focuses on international legal partition and state recognition as the basis for legitimacy, distinct from security-based or indigenous-continuity claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
