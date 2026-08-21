% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: LDS Marriage Commitment Reversal: Exogenous Override Reading
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story, the 'exogenous override' reading of the
 *   'marriage_commitment_reversal' kernel, describes the period when the LDS
 *   Church publicly suspended the practice of plural marriage due to intense
 *   and sustained federal coercion, without formally renouncing the
 *   underlying doctrinal principle (Section 132 of the Doctrine and
 *   Covenants). The federal government's actions, including legislation,
 *   court cases, and property seizures, are seen as a direct extraction of
 *   institutional autonomy and a forceful imposition of national legal norms.
 *   The high extractiveness and suppression reflect the federal government's
 *   successful assertion of sovereignty over the church's practice, while the
 *   moderate theater ratio indicates the gap between public compliance and
 *   internal doctrinal continuity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.9).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.95).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "LDS Marriage Commitment Reversal: Exogenous Override Reading").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, '7edc822f-a9a4-40e0-b0ff-2323d9c635b9').
narrative_ontology:cs_kernel_codification('7edc822f-a9a4-40e0-b0ff-2323d9c635b9', fixed_text).
narrative_ontology:cs_authority_grounding('7edc822f-a9a4-40e0-b0ff-2323d9c635b9', extraction).
narrative_ontology:cs_interpretation_layer_present('7edc822f-a9a4-40e0-b0ff-2323d9c635b9').
narrative_ontology:cs_reading_relation('7edc822f-a9a4-40e0-b0ff-2323d9c635b9', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('7edc822f-a9a4-40e0-b0ff-2323d9c635b9', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('7edc822f-a9a4-40e0-b0ff-2323d9c635b9', foundational, federal_sovereignty_over_marriage).
narrative_ontology:cs_axiom_status(federal_sovereignty_over_marriage, holdable).
narrative_ontology:cs_axiom_grounding('7edc822f-a9a4-40e0-b0ff-2323d9c635b9', federal_sovereignty_over_marriage, conventional).
narrative_ontology:cs_axiom('7edc822f-a9a4-40e0-b0ff-2323d9c635b9', foundational, divine_mandate_of_plural_marriage_unrenounced).
narrative_ontology:cs_axiom_status(divine_mandate_of_plural_marriage_unrenounced, holdable).
narrative_ontology:cs_axiom_grounding('7edc822f-a9a4-40e0-b0ff-2323d9c635b9', divine_mandate_of_plural_marriage_unrenounced, theological).
narrative_ontology:cs_reference_frame('7edc822f-a9a4-40e0-b0ff-2323d9c635b9', divine_mandate_of_plural_marriage).
narrative_ontology:cs_drift_state('7edc822f-a9a4-40e0-b0ff-2323d9c635b9', federal_anti_polygamy_acts_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('7edc822f-a9a4-40e0-b0ff-2323d9c635b9', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_church_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_members_practicing_plural_marriage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, lds_members_not_practicing_plural_marriage).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_members_not_practicing_plural_marriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the governing body of the LDS Church, they faced direct federal pressure and legal threats, leading to the public suspension of plural marriage. Internally, they maintained the doctrinal principle (Section 132) while outwardly complying with federal law. Their institutional sovereignty was directly targeted and curtailed.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_church_leadership, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_church_leadership, payer).

% Asserted its legal and territorial sovereignty through anti-polygamy legislation, court cases, and property seizures. Benefited from establishing legal uniformity across its territories and consolidating its authority over marriage. Its actions directly coerced the LDS Church into changing its practice.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_government, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, federal_government, beneficiary).

% Were directly targeted by federal laws, facing fines, imprisonment, and social ostracization for continuing the practice. Many were deeply committed to the principle as a divine commandment, making exit from the practice a profound identity crisis or leading to underground persistence.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_members_practicing_plural_marriage, payer,
    powerless, biographical, identity_locked, local).

% Largely condemned plural marriage, providing political support for federal anti-polygamy efforts. Their moral and social norms were aligned with the federal government's actions, reinforcing the pressure on the LDS Church.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, us_public_opinion, observer,
    organized, immediate, mobile, national).

% Benefited from the church's increased social acceptance and integration into mainstream American society following the cessation of plural marriage. However, they also bore the cost of institutional autonomy and the internal tension of a doctrine that remained unrenounced but unpracticed.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_members_not_practicing_plural_marriage, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_members_not_practicing_plural_marriage, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The federal government coordinated its legal and enforcement apparatus to establish and maintain a uniform standard of monogamous marriage across its territories, asserting its sovereignty over religious institutions.
% TRANSFER_FUNCTION: Transferred institutional autonomy and the right to define marriage from the LDS Church to the federal government. It also transferred social legitimacy to the LDS Church by compelling its conformity to national norms, at the cost of its unique practice.
% ABSENT_VOICES: Those deeply committed to the divine mandate of plural marriage, who were forced to abandon their practice or go underground, were effectively silenced in the public discourse surrounding the church's official compliance. Their experiences of coercion and identity-lock were marginalized.
% DISAPPEARANCE_RATIONALE: If the federal anti-polygamy laws and their enforcement vanished overnight, the LDS Church would face immense internal pressure to reconcile its unrenounced doctrine (Section 132) with its current practice. This could lead to significant doctrinal re-evaluation, renewed internal debate, or even a resurgence of plural marriage practice, profoundly reorganizing the church's structure and relationship with the state.
% FOUNDING_PROBLEM: The federal government sought to establish legal and social uniformity across its territories, particularly regarding marriage, which clashed directly with the LDS Church's practice of plural marriage, seen as a challenge to federal authority and societal norms.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal court rulings, congressional acts, and contemporary scholarly analysis from legal historians and religious studies scholars corroborate the federal government's coercive role. The federal government's perspective is self-attested through its actions and legal pronouncements, while independent historians attest to the church's internal struggle and the unrenounced status of the doctrine.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the federal government successfully compelled a fundamental change in religious practice, effectively extracting institutional autonomy from the LDS Church. Suppression is extremely high due to the comprehensive legal and enforcement mechanisms deployed by the federal government, which made continued public practice of plural marriage nearly impossible. Resistance, initially high, was systematically crushed. The theater ratio is moderate, reflecting the church's public compliance (theatrical performance) while internally maintaining the doctrinal principle (the underlying function/belief). The measurement series shows a clear escalation of federal pressure and the corresponding increase in extraction and suppression over the period.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this was an assertion of national sovereignty and the enforcement of moral norms, a necessary coordination function. From the perspective of the LDS Church leadership and practitioners, it was a profound act of coercion and extraction, forcing a change in divinely mandated practice. This reading emphasizes the latter, highlighting the external force rather than internal reinterpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the clear beneficiary and agenda-setter, gaining territorial control and legal uniformity. The LDS Church leadership and members practicing plural marriage are the primary victims and payers, bearing the direct costs of legal enforcement, property loss, and forced abandonment of practice. Other LDS members, not practicing plural marriage, are beneficiaries of increased social acceptance but also pay the cost of institutional autonomy. US public opinion, largely anti-polygamy, acted as an observer reinforcing the federal position.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_status_of_section_132,
    'Has Section 132 been internally reinterpreted or effectively renounced by the LDS Church, despite no formal doctrinal revision?',
    'Analysis of internal church discourse, theological publications, and authoritative statements over time for subtle shifts in interpretation or emphasis, particularly regarding its applicability and future potential.',
    'If reinterpreted or implicitly renounced, the ''endogenous_reinterpretation_reading'' gains strength, and the ''exogenous_override_reading'' becomes less about a pure external override and more about an externally-induced internal shift in understanding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_status_of_section_132, conceptual, 'Whether the unrenounced doctrine has undergone internal reinterpretation.').

omega_variable(
    persistence_of_practice_under_coercion,
    'To what extent did the practice of plural marriage persist underground or in isolated communities despite federal coercion, and what was the true cost of this persistence?',
    'Historical sociological research, archival studies, and oral histories from descendants of practitioners, focusing on the period immediately following the official cessation.',
    'Higher persistence would indicate the limits of federal suppression and the depth of identity-lock for practitioners, potentially increasing the measured ''resistance'' and ''suppression'' metrics for the period, and highlighting the ongoing extraction from those who continued the practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persistence_of_practice_under_coercion, empirical, 'The extent and cost of underground persistence of plural marriage.').

omega_variable(
    kernel_causality_ambiguity,
    'Was the primary cause of the reversal the direct federal coercion (exogenous) or an internal reinterpretation of divine will (endogenous) in response to changing circumstances?',
    'Comparative analysis of the ''exogenous_override_reading'' and ''endogenous_reinterpretation_reading'' through historical evidence, theological arguments, and institutional narratives, weighing the relative influence of external pressure versus internal spiritual guidance.',
    'The classification of the ''marriage_commitment_reversal'' kernel itself depends on which causal account is prioritized. This reading emphasizes the external force, while the ''endogenous_reinterpretation_reading'' would emphasize internal agency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_causality_ambiguity, conceptual, 'Ambiguity regarding the primary causal factor for the reversal of plural marriage practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1862, 1912).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1862, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1862, 0.1).
narrative_ontology:measurement(marr_tr_t1872, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1872, 0.2).
narrative_ontology:measurement(marr_tr_t1882, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1882, 0.3).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1900, 0.45).
narrative_ontology:measurement(marr_tr_t1912, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1912, 0.45).

% Extraction over time
narrative_ontology:measurement(marr_be_t1862, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1862, 0.6).
narrative_ontology:measurement(marr_be_t1872, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1872, 0.7).
narrative_ontology:measurement(marr_be_t1882, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1882, 0.8).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1890, 0.88).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1900, 0.9).
narrative_ontology:measurement(marr_be_t1912, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1912, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1862, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1862, 0.7).
narrative_ontology:measurement(marr_su_t1872, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1872, 0.78).
narrative_ontology:measurement(marr_su_t1882, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1882, 0.85).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1890, 0.92).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1900, 0.95).
narrative_ontology:measurement(marr_su_t1912, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1912, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_reversal' kernel, focusing on the exogenous coercion. It is linked to sibling readings that emphasize internal reinterpretation and the resulting doctrine-practice gap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
