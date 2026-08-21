% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation: Physical Performance Only Reading
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint represents the 'performance_only' reading of the
 *   sacrifice obligation continuity kernel. It asserts that the divine
 *   commandment for sacrifice requires physical performance, which is
 *   currently impossible due to historical circumstances (e.g., destruction
 *   of the Temple). Study of the laws is considered preparation for future
 *   restoration, not a substitute for fulfillment. This reading places
 *   adherents in a position of unfulfillable obligation, generating
 *   significant spiritual burden and guilt, while simultaneously reinforcing
 *   the authority of those who maintain the tradition and anticipate its
 *   restoration.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.85).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.9).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, snare).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation: Physical Performance Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, 'd1cf45f7-b8eb-40ea-913a-bad786b7ad62').
narrative_ontology:cs_kernel_codification('d1cf45f7-b8eb-40ea-913a-bad786b7ad62', fixed_text).
narrative_ontology:cs_authority_grounding('d1cf45f7-b8eb-40ea-913a-bad786b7ad62', lineage).
narrative_ontology:cs_interpretation_layer_present('d1cf45f7-b8eb-40ea-913a-bad786b7ad62').
narrative_ontology:cs_reading_relation('d1cf45f7-b8eb-40ea-913a-bad786b7ad62', sacrifice_obligation_continuity__archival_preservation, coexists_with).
narrative_ontology:cs_reading_relation('d1cf45f7-b8eb-40ea-913a-bad786b7ad62', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('d1cf45f7-b8eb-40ea-913a-bad786b7ad62', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_axiom('d1cf45f7-b8eb-40ea-913a-bad786b7ad62', foundational, physical_performance_is_sine_qua_non).
narrative_ontology:cs_axiom_status(physical_performance_is_sine_qua_non, holdable).
narrative_ontology:cs_axiom_grounding('d1cf45f7-b8eb-40ea-913a-bad786b7ad62', physical_performance_is_sine_qua_non, deontological).
narrative_ontology:cs_axiom('d1cf45f7-b8eb-40ea-913a-bad786b7ad62', foundational, study_is_preparation_not_substitute).
narrative_ontology:cs_axiom_status(study_is_preparation_not_substitute, holdable).
narrative_ontology:cs_axiom_grounding('d1cf45f7-b8eb-40ea-913a-bad786b7ad62', study_is_preparation_not_substitute, conventional).
narrative_ontology:cs_reference_frame('d1cf45f7-b8eb-40ea-913a-bad786b7ad62', pre_temple_destruction_ritual_practice).
narrative_ontology:cs_drift_state('d1cf45f7-b8eb-40ea-913a-bad786b7ad62', post_temple_destruction_diaspora, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('d1cf45f7-b8eb-40ea-913a-bad786b7ad62', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, religious_authorities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, scholarly_community).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, messianic_hopefuls).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, adherents_of_tradition).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, divine_commandment_immutability).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, future_redemption_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the interpretation that sacrifice requires physical performance and that study is merely preparation. They derive authority and purpose from preserving the unfulfilled obligation and guiding adherents in its anticipation. They enforce communal adherence to this reading.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, religious_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the ongoing need for textual study and interpretation of sacrifice laws, which provides a central focus for their academic and religious work. Their status and intellectual capital are tied to the perpetuation of this interpretive framework.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, scholarly_community, beneficiary,
    organized, generational, constrained, global).

% Bear the spiritual burden of an unfulfillable divine commandment. They experience guilt and a sense of incompleteness, with no direct means to fulfill the obligation in their lifetime. Their identity is deeply intertwined with this tradition, making exit unthinkable.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, adherents_of_tradition, payer,
    powerless, biographical, identity_locked, local).

% Find purpose and meaning in the anticipation of a future messianic era when the physical performance of sacrifices will be restored. The current unfulfillable state reinforces their eschatological beliefs and communal solidarity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, messianic_hopefuls, beneficiary,
    moderate, generational, identity_locked, global).

% Analyze the constraint from an external, academic perspective, noting its sociological and psychological impacts without being bound by its normative claims. They observe the dynamics of unfulfillable obligation and its role in identity formation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, secular_observers, observer,
    analytical, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__performance_only, religious_authorities).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To maintain the continuity of a central divine commandment and the readiness for its future restoration, ensuring the tradition's integrity despite the physical impossibility of performance.
% TRANSFER_FUNCTION: Transfers spiritual burden (guilt, unfulfilled duty) from the divine/tradition to the adherents, who bear the cost of non-fulfillment. It transfers authority and intellectual capital to religious and scholarly leaders who interpret and preserve the tradition.
% ABSENT_VOICES: Those who advocate for a reinterpretation where study *is* fulfillment, or for a complete suspension of the obligation, are marginalized. Their arguments are often dismissed as undermining the divine command or the messianic hope, effectively excluding them from the dominant discourse.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the entire religious framework and the identity of its adherents would be fundamentally altered. The spiritual lives, communal practices, and the authority structure of the tradition are deeply tied to this obligation and its anticipation. Its disappearance would necessitate a profound theological and communal reorganization.
% FOUNDING_PROBLEM: To address the theological and communal crisis arising from the destruction of the central site for sacrifice (the Temple), which rendered the physical performance of a core divine commandment impossible.
% FOUNDING_PROBLEM_CORROBORATION: Historical religious texts, rabbinic commentaries, and communal prayers for the restoration of the Temple attest to the founding problem. The ongoing absence of the Temple and the continued emphasis on its future rebuilding corroborate the 'live' status of this problem from outside the immediate benefiting parties (religious authorities).
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the constraint imposes a binding, unfulfillable obligation, leading to persistent spiritual burden and guilt without remedy. Suppression is also very high (0.90) due to the deep-seated nature of the religious tradition, the lack of physical means for performance, and the strong communal pressure to adhere to this interpretation. Accessibility collapse is near total (0.95) as the physical means for performance are absent. Resistance is low (0.15) because rejecting this interpretation often means rejecting one's religious identity. Theater ratio is low (0.10) because this reading explicitly denies that study or other activities are 'fulfillment,' thus minimizing performative substitutes for the core obligation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adherents, the constraint is a source of profound spiritual extraction and guilt, an inescapable burden. From the perspective of religious authorities and scholars, it is a vital mechanism for maintaining the integrity of the tradition, preserving divine law, and fostering communal identity and hope for the future. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious authorities and the scholarly community are beneficiaries, as their roles, authority, and intellectual pursuits are sustained by the ongoing interpretation and preservation of this unfulfilled obligation. Messianic hopefuls also benefit by finding purpose in the anticipation of future restoration. Adherents of the tradition are the primary victims, bearing the spiritual and psychological costs of an unfulfillable divine command. Their identity-locked exit options amplify their target status.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to maintain the divine commandment and prepare for its restoration is still live. However, its *fulfillment* function has atrophied due to the physical impossibility of performance. The persistence of the obligation, despite its unfulfillable nature, indicates a deep-seated structural inertia, where the 'problem' (lack of Temple) is still live, but the 'solution' (sacrifice) is unavailable, leading to ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unfulfillable_obligation_impact,
    'Is the spiritual burden and guilt experienced by adherents a form of extraction, or a necessary spiritual state inherent to the tradition''s eschatology?',
    'Comparative theological analysis of traditions with similar unfulfillable obligations, examining the psychological and communal outcomes, and assessing whether alternative interpretations alleviate or transform this burden.',
    'If primarily extraction, the Snare classification is strongly reinforced. If a necessary spiritual state, the extractiveness might be re-evaluated as an inherent cost of a complex identity_coordination, potentially shifting the classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unfulfillable_obligation_impact, conceptual, 'Ambiguity regarding the nature of spiritual burden: extraction vs. inherent spiritual cost.').

omega_variable(
    study_as_fulfillment_validity,
    'Could the ''study_as_performance'' reading genuinely fulfill the divine commandment without undermining the tradition''s core tenets, or is it a theological compromise?',
    'Internal theological debate and re-evaluation within the tradition, potentially leading to a shift in normative consensus or the emergence of new authoritative interpretations.',
    'If ''study_as_performance'' were widely accepted as valid fulfillment, the extractiveness of this ''performance_only'' reading would collapse, as the obligation would become fulfillable. This would fundamentally alter the constraint''s classification, likely towards a Rope or Scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_as_fulfillment_validity, conceptual, 'Whether study can be a valid substitute for physical performance.').

omega_variable(
    messianic_suspension_legitimacy,
    'Is the ''messianic_suspension'' reading a more compassionate and equally legitimate interpretation of the obligation''s status during the diaspora, or does it diminish the urgency of restoration?',
    'Further theological and philosophical inquiry into the nature of divine command and human obligation in the absence of means, potentially leading to a shift in communal emphasis.',
    'If ''messianic_suspension'' gained wider acceptance, the immediate burden on adherents would be reduced, as the obligation would be seen as temporarily inactive rather than unfulfillable. This would lower the perceived extractiveness and suppression of the ''performance_only'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_suspension_legitimacy, preference, 'Legitimacy of suspending the obligation vs. maintaining its active, unfulfillable status.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (lack of physical means for sacrifice) or internalized (deeply ingrained belief that rejecting the obligation is a rejection of identity)?',
    'Analysis of adherents'' responses to hypothetical scenarios where physical means become available but communal pressure to maintain the ''performance_only'' reading persists. If suppression persists, it indicates internalized components.',
    'If internalized suppression is a significant component, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them even if external barriers were removed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in religious adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.12).
narrative_ontology:measurement(sacr_tr_t400, sacrifice_obligation_continuity__performance_only, theater_ratio, 400, 0.11).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_obligation_continuity__performance_only, theater_ratio, 800, 0.1).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_obligation_continuity__performance_only, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(sacr_tr_t1600, sacrifice_obligation_continuity__performance_only, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__performance_only, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(sacr_be_t400, sacrifice_obligation_continuity__performance_only, base_extractiveness, 400, 0.82).
narrative_ontology:measurement(sacr_be_t800, sacrifice_obligation_continuity__performance_only, base_extractiveness, 800, 0.83).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1200, 0.84).
narrative_ontology:measurement(sacr_be_t1600, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1600, 0.85).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 2000, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(sacr_su_t400, sacrifice_obligation_continuity__performance_only, suppression_requirement, 400, 0.87).
narrative_ontology:measurement(sacr_su_t800, sacrifice_obligation_continuity__performance_only, suppression_requirement, 800, 0.88).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1200, 0.89).
narrative_ontology:measurement(sacr_su_t1600, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1600, 0.9).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 2000, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_obligation_continuity' kernel. Its high extractiveness stems from the unfulfillable nature of the obligation, contrasting with sibling readings that offer alternative modes of fulfillment or suspension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
