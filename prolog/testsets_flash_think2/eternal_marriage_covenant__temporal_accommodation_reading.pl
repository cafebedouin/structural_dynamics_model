% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Eternal Marriage Covenant: Temporal Accommodation Reading
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint describes the 'temporal accommodation' reading of the
 *   eternal marriage covenant within a specific religious tradition.
 *   Following intense federal pressure and legal persecution, the religious
 *   institution issued manifestos suspending the practice of plural marriage.
 *   This reading asserts that the underlying doctrine of plural marriage as
 *   an eternal principle remains valid and unrenounced, but its practice is
 *   temporarily suspended out of obedience to the 'law of the land,' pending
 *   a future restoration when political constraints lift. The constraint thus
 *   coordinates the institution's legal survival with the preservation of its
 *   core theological claims, at the cost of suspending a practice deemed
 *   essential by some adherents.
 *
 * KEY AGENTS:
 *   - religious_institution_leadership: Agenda setter / Beneficiary (institutional / constrained)
 *   - members_prioritizing_legal_compliance: Beneficiary (moderate / mobile)
 *   - members_adhering_to_eternal_principle: Payer (powerless / identity_locked)
 *   - secular_government: Agenda setter (institutional / analytical)
 *   - external_observers: Observer (analytical / analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.45).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.7).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Eternal Marriage Covenant: Temporal Accommodation Reading").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, 'da6be012-f9d0-4a2d-8b10-984a535f8837').
narrative_ontology:cs_kernel_codification('da6be012-f9d0-4a2d-8b10-984a535f8837', fixed_text).
narrative_ontology:cs_authority_grounding('da6be012-f9d0-4a2d-8b10-984a535f8837', lineage).
narrative_ontology:cs_interpretation_layer_present('da6be012-f9d0-4a2d-8b10-984a535f8837').
narrative_ontology:cs_reading_relation('da6be012-f9d0-4a2d-8b10-984a535f8837', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('da6be012-f9d0-4a2d-8b10-984a535f8837', eternal_marriage_covenant__prophetic_override_reading, coexists_with).
narrative_ontology:cs_axiom('da6be012-f9d0-4a2d-8b10-984a535f8837', foundational, eternal_principles_transcend_temporal_law).
narrative_ontology:cs_axiom_status(eternal_principles_transcend_temporal_law, holdable).
narrative_ontology:cs_axiom_grounding('da6be012-f9d0-4a2d-8b10-984a535f8837', eternal_principles_transcend_temporal_law, theological).
narrative_ontology:cs_axiom('da6be012-f9d0-4a2d-8b10-984a535f8837', foundational, obedience_to_law_of_land_is_divine_command).
narrative_ontology:cs_axiom_status(obedience_to_law_of_land_is_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('da6be012-f9d0-4a2d-8b10-984a535f8837', obedience_to_law_of_land_is_divine_command, conventional).
narrative_ontology:cs_reference_frame('da6be012-f9d0-4a2d-8b10-984a535f8837', divine_law_supremacy_with_temporal_prudence).
narrative_ontology:cs_drift_state('da6be012-f9d0-4a2d-8b10-984a535f8837', post_manifesto_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('da6be012-f9d0-4a2d-8b10-984a535f8837', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, religious_institution_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, members_prioritizing_legal_compliance).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, members_adhering_to_eternal_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues manifestos and interpretations that guide the community's practice. Benefits from maintaining the institution's legal standing and avoiding persecution by accommodating secular law, while preserving the doctrine's eternal validity for future restoration. Bears the cost of internal dissent from those who wish to practice the full doctrine.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, religious_institution_leadership, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, religious_institution_leadership, beneficiary).

% Benefits from the stability and legal acceptance of the religious institution. Avoids direct conflict with secular law and social ostracism that would come from non-compliance. May experience internal tension but prioritizes obedience to current leadership and law.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, members_prioritizing_legal_compliance, beneficiary,
    moderate, biographical, mobile, local).

% Bears the cost of suspending a divinely commanded practice they believe is essential for exaltation. Their identity is deeply tied to the eternal principle, making exit from the faith unthinkable, but they are constrained from full obedience by the accommodation. They live in hope of future restoration.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, members_adhering_to_eternal_principle, payer,
    powerless, generational, identity_locked, local).

% Enforces the laws of the land, which led to the initial suspension of the practice. Benefits from the religious institution's compliance, avoiding social unrest and legal challenges. Its power is external to the religious doctrine but directly shapes its practice.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, secular_government, agenda_setter,
    institutional, generational, analytical, national).

% Academics, historians, and other religious communities who analyze the dynamics of religious accommodation, doctrinal shifts, and the interplay between religious and secular authority. They observe the constraint's operation without direct participation or benefit.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious community's relationship with the secular state, allowing the institution to persist and grow without direct legal conflict, while maintaining the theological integrity of its core doctrines in a dormant state.
% TRANSFER_FUNCTION: Transfers the cost of legal non-compliance (persecution, loss of legal status) from the religious institution to individual members, who must suspend a divinely commanded practice. It also transfers the full realization of an 'eternal principle' from the present to an unspecified future.
% ABSENT_VOICES: Those who felt the accommodation was a renunciation and left the faith, or those who secretly continued the practice despite the official suspension. Their dissent is either externalized or driven underground.
% DISAPPEARANCE_RATIONALE: If this accommodation vanished overnight, the religious institution would immediately face renewed legal persecution from the secular government or be forced to formally renounce the eternal principle, fundamentally altering its theological claims and relationship with its members. The community's structure and legal status would be drastically reorganized.
% FOUNDING_PROBLEM: The existential conflict between a divinely commanded practice (plural marriage) and the anti-polygamy laws of the United States, leading to legal persecution, imprisonment of leaders, and the threat of disincorporation for the religious institution.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of legal battles and persecution (e.g., Supreme Court cases like Reynolds v. United States), contemporary theological debates within the faith regarding the nature of eternal covenants, and ongoing academic analysis of religious freedom and state power. These sources, from outside the immediate benefiting parties, corroborate the severity and persistence of the underlying tension.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).
:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (allowing the religious institution to survive and operate legally) but does so with asymmetric extraction. The extraction is borne by members who believe in the eternal principle but are prevented from practicing it, experiencing a deferred hope and a suspension of what they consider a divine command. Base extractiveness is moderate (0.45) because the doctrine itself is not renounced, but the cost of non-practice is significant. Suppression is high (0.7) due to both external legal enforcement and internal institutional pressure to comply with the accommodation. Theater ratio is high (0.6) as significant effort is expended to maintain the validity of the dormant doctrine and the narrative of 'temporal' suspension, rather than full renunciation. The temporal measurements show initial high suppression (federal pressure) gradually normalizing, while extractiveness and theater remain stable or slightly increase as the 'temporary' accommodation becomes a long-term reality.
 *
 * PERSPECTIVAL GAP:
 *   The religious institution's leadership and members prioritizing legal compliance experience this as a necessary and beneficial accommodation, ensuring the faith's survival. For members adhering to the eternal principle, it is a profound sacrifice and a source of ongoing tension, where the 'eternal' is constrained by the 'temporal.' The secular government views it as successful enforcement of law. The engine's per-seat classification will reflect these divergent experiences based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious institution's leadership and members prioritizing legal compliance are beneficiaries (low d) as they gain legal stability and avoid persecution. Members adhering to the eternal principle are targets (high d) as they bear the cost of suspended practice and deferred hope. The secular government acts as an external enforcer, shaping the constraint's operation. External observers are analytical, outside the direct flow of benefit or cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as a pure Snare, acknowledging the genuine coordination problem (institutional survival) that the accommodation solves. However, it also highlights the significant extraction from those whose core beliefs are suspended. The 'temporal' nature of the accommodation, while central to the narrative, is challenged by the sustained extractiveness and high theater ratio, suggesting a potential drift towards a more permanent, extractive arrangement if the 'restoration' never materializes. The 'founding_problem_status' being 'live' indicates the underlying tension persists, even if the immediate crisis was averted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_vs_de_facto_renunciation,
    'Is the ''temporal accommodation'' truly a temporary suspension awaiting future restoration, or has it become a de facto renunciation of the practice?',
    'Longitudinal study of doctrinal evolution, leadership statements, and member expectations over several generations. If the expectation of restoration significantly diminishes or the doctrine is reinterpreted to preclude future practice, it suggests de facto renunciation.',
    'If de facto renunciation, the constraint''s extractiveness might be re-evaluated as lower (as the ''cost'' of deferred hope diminishes), but the theater ratio might remain high as the narrative of ''eternal principle'' is maintained for historical continuity rather than future practice. The classification might shift towards a Piton if the original function atrophies entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_vs_de_facto_renunciation, empirical, 'Ambiguity of the accommodation''s true duration and intent.').

omega_variable(
    internal_vs_external_suppression,
    'What proportion of the measured suppression is due to external secular law versus internal institutional enforcement and social pressure within the religious community?',
    'Comparative analysis with religious communities in jurisdictions with different legal frameworks regarding plural marriage. If internal suppression persists strongly even where external legal pressure is absent, it indicates a higher internalized component.',
    'If internal/institutional suppression is dominant, the constraint''s persistence is more deeply embedded within the community''s social fabric, making it harder to alter even if external conditions change. This would amplify the effective suppression for identity-locked members.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_vs_external_suppression, empirical, 'Structural vs. internalized suppression mechanism for the suspended practice.').

omega_variable(
    obedience_principle_sincerity,
    'Is the ''obedience to the law of the land'' principle a genuine theological tenet, or primarily a strategic justification for institutional survival?',
    'Analysis of the principle''s application in other contexts where religious doctrine conflicts with secular law. Inconsistent application or selective emphasis would suggest a strategic rather than foundational grounding.',
    'If primarily strategic, the constraint''s coordination function is more fragile and less grounded in enduring principle, potentially increasing its effective extractiveness as the ''cover story'' for institutional benefit becomes clearer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(obedience_principle_sincerity, conceptual, 'Theological vs. strategic grounding of the obedience principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 1890, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1890, 0.5).
narrative_ontology:measurement(eter_tr_t1900, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1900, 0.55).
narrative_ontology:measurement(eter_tr_t1910, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1910, 0.58).
narrative_ontology:measurement(eter_tr_t1920, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1920, 0.6).
narrative_ontology:measurement(eter_tr_t1930, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1930, 0.6).
narrative_ontology:measurement(eter_tr_t1940, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1940, 0.6).
narrative_ontology:measurement(eter_tr_t1950, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1950, 0.6).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1890, 0.4).
narrative_ontology:measurement(eter_be_t1900, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1900, 0.42).
narrative_ontology:measurement(eter_be_t1910, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1910, 0.43).
narrative_ontology:measurement(eter_be_t1920, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1920, 0.44).
narrative_ontology:measurement(eter_be_t1930, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1930, 0.44).
narrative_ontology:measurement(eter_be_t1940, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1940, 0.45).
narrative_ontology:measurement(eter_be_t1950, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1950, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1890, 0.8).
narrative_ontology:measurement(eter_su_t1900, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(eter_su_t1910, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1910, 0.7).
narrative_ontology:measurement(eter_su_t1920, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(eter_su_t1930, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1930, 0.68).
narrative_ontology:measurement(eter_su_t1940, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1940, 0.69).
narrative_ontology:measurement(eter_su_t1950, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1950, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
